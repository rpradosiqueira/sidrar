test_that("successful HTTP responses are decoded as UTF-8 text", {
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      fake_http_response(body = '{"nome":"Variação"}')
    },
    .package = "httr"
  )

  text <- sidrar:::.sidra_request(
    "https://apisidra.ibge.gov.br/values/t/1"
  )
  expect_identical(text, '{"nome":"Variação"}')
})

test_that("HTTP failures retain status and API details", {
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      fake_http_response(
        status = 400L,
        body = "Parâmetro de período inválido"
      )
    },
    .package = "httr"
  )

  error <- expect_error(
    sidrar:::.sidra_request(
      "https://apisidra.ibge.gov.br/values/t/1"
    ),
    regexp = "HTTP 400.*Parâmetro de período inválido",
    class = "sidrar_http_error"
  )
  expect_false(inherits(error, "sidrar_limit_error"))
  expect_identical(error$status_code, 400L)
  expect_identical(error$response_body, "Parâmetro de período inválido")
  expect_identical(
    error$url,
    "https://apisidra.ibge.gov.br/values/t/1"
  )
})

test_that("challenge headers produce a structured actionable error", {
  old_options <- options(sidrar.fallback = FALSE)
  on.exit(options(old_options), add = TRUE)
  body <- "<html><head><title>Just a moment...</title></head></html>"
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      fake_http_response(
        status = 403L,
        body = body,
        headers = list(
          "Content-Type" = "text/html; charset=UTF-8",
          "CF-Mitigated" = " Challenge ",
          "CF-Ray" = "test-ray-GRU"
        )
      )
    },
    .package = "httr"
  )

  path <- "/t/7060/n1/1/v/63/p/last/c315/7169/h/n"
  error <- expect_error(
    suppressMessages(get_sidra(api = path)),
    regexp = "browser challenge.*HTTP 403",
    class = "sidrar_challenge_error"
  )
  expect_s3_class(error, "sidrar_http_error")
  expect_s3_class(error, "sidrar_error")
  expect_identical(error$status_code, 403L)
  expect_identical(error$response_body, body)
  expect_identical(
    error$url,
    paste0("https://apisidra.ibge.gov.br/values", path)
  )
  expect_identical(error$cf_ray, "test-ray-GRU")
  expect_match(conditionMessage(error), "Contact IBGE")
  expect_match(conditionMessage(error), "test-ray-GRU", fixed = TRUE)
  expect_false(grepl("<html>", conditionMessage(error), fixed = TRUE))
})

test_that("challenge headers work without a body or Ray ID", {
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      fake_http_response(
        status = 403L,
        body = "",
        headers = list("cf-mitigated" = "challenge")
      )
    },
    .package = "httr"
  )
  error <- expect_error(
    sidrar:::.sidra_request("https://apisidra.ibge.gov.br/values/t/1"),
    class = "sidrar_challenge_error"
  )
  expect_identical(error$response_body, "")
  expect_null(error$cf_ray)
})

test_that("HTML challenge markers are detected before JSON parsing", {
  old_options <- options(sidrar.fallback = FALSE)
  on.exit(options(old_options), add = TRUE)
  bodies <- c(
    paste0(
      " \n<!DOCTYPE html><html><head><title>Just a moment...</title>",
      "</head><body>", strrep("padding ", 100L),
      '<script src="/cdn-cgi/challenge-platform/h/g/orchestrate/chl_page/',
      'v1"></script></body></html>'
    ),
    "<HTML><script>window._cf_chl_opt = {};</script></HTML>"
  )
  response <- NULL
  testthat::local_mocked_bindings(
    RETRY = function(...) response,
    .package = "httr"
  )

  for (status in c(200L, 403L, 503L)) {
    for (body in bodies) {
      response <- fake_http_response(
        status = status,
        body = body,
        headers = list("content-type" = "text/html")
      )
      error <- expect_error(
        suppressMessages(get_sidra(api = "/t/1/n1/1/h/n")),
        class = "sidrar_challenge_error"
      )
      expect_identical(error$status_code, status)
      expect_identical(error$response_body, body)
      expect_null(error$cf_ray)
      expect_false(inherits(error, "sidrar_parse_error"))
    }
  }
})

test_that("generic forbidden responses are not misclassified as challenges", {
  bodies <- c(
    "Access denied",
    "<html><title>Just a moment...</title>Cloudflare</html>",
    '<html><script src="https://challenges.cloudflare.com/turnstile/v0/api.js">',
    '{"message":"_cf_chl_opt and /cdn-cgi/challenge-platform/"}',
    "Example: <html><script>window._cf_chl_opt = {};</script></html>"
  )
  response <- NULL
  testthat::local_mocked_bindings(
    RETRY = function(...) response,
    .package = "httr"
  )
  for (body in bodies) {
    response <- fake_http_response(
      status = 403L,
      body = body,
      headers = list(
        "server" = "cloudflare",
        "cf-ray" = "test-ray-GRU",
        "cf-mitigated" = "other"
      )
    )
    error <- expect_error(
      sidrar:::.sidra_request("https://apisidra.ibge.gov.br/values/t/1"),
      class = "sidrar_http_error"
    )
    expect_false(inherits(error, "sidrar_challenge_error"))
    expect_identical(error$response_body, body)
  }
})

test_that("valid JSON containing challenge-related text remains valid data", {
  text <- paste0(
    '[{"NC":"1","NN":"<html>_cf_chl_opt ',
    '/cdn-cgi/challenge-platform/ Just a moment Cloudflare</html>",',
    '"V":"0.16"}]'
  )
  testthat::local_mocked_bindings(
    RETRY = function(...) fake_http_response(body = text),
    .package = "httr"
  )
  data <- suppressMessages(get_sidra(api = "/t/1/n1/1/h/n"))
  expect_identical(data$V, 0.16)
  expect_identical(data$NC, "1")
  expect_match(data$NN, "_cf_chl_opt", fixed = TRUE)
})

test_that("the real retry loop stops after one forbidden challenge response", {
  old_options <- options(sidrar.retries = 5L)
  on.exit(options(old_options), add = TRUE)
  calls <- 0L
  testthat::local_mocked_bindings(
    request_perform = function(...) {
      calls <<- calls + 1L
      fake_http_response(
        status = 403L,
        body = "<html>Just a moment...</html>",
        headers = list("cf-mitigated" = "challenge")
      )
    },
    backoff_full_jitter = function(...) {
      stop("A forbidden challenge must not trigger retry backoff")
    },
    .package = "httr"
  )
  expect_error(
    sidrar:::.sidra_request("https://apisidra.ibge.gov.br/values/t/1"),
    class = "sidrar_challenge_error"
  )
  expect_identical(calls, 1L)
})

test_that("value-limit responses produce an actionable structured error", {
  body <- paste(
    "Quantidade de valores solicitados: 83550",
    "excedeu o limite: 50000"
  )
  calls <- 0L
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      calls <<- calls + 1L
      fake_http_response(status = 400L, body = body)
    },
    .package = "httr"
  )

  path <- "/t/10261/n6/all/v/4090/p/2022/c11913/all/h/n"
  url <- paste0("https://apisidra.ibge.gov.br/values", path)
  error <- expect_error(
    suppressMessages(get_sidra(api = path)),
    regexp = "83550.*50000.*at least 2 disjoint calls",
    class = "sidrar_limit_error"
  )

  expect_s3_class(error, "sidrar_http_error")
  expect_s3_class(error, "sidrar_error")
  expect_identical(error$status_code, 400L)
  expect_identical(error$requested_values, 83550)
  expect_identical(error$limit_values, 50000)
  expect_identical(error$minimum_batches, 2)
  expect_identical(
    error$suggested_arguments,
    c("period", "geo.filter", "variable", "category")
  )
  expect_identical(error$response_body, body)
  expect_identical(error$url, url)
  expect_identical(calls, 1L)
})

test_that("value-limit detection is strict and uses the complete body", {
  long_body <- paste0(
    strrep("temporary upstream detail ", 25L),
    " QUANTIDADE   DE VALORES SOLICITADOS: 3000000000\n",
    "EXCEDEU O LIMITE: 50000"
  )
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      fake_http_response(status = 400L, body = long_body)
    },
    .package = "httr"
  )

  error <- expect_error(
    sidrar:::.sidra_request(
      "https://apisidra.ibge.gov.br/values/t/1"
    ),
    class = "sidrar_limit_error"
  )
  expect_identical(error$requested_values, 3000000000)
  expect_identical(error$limit_values, 50000)
  expect_identical(error$minimum_batches, 60000)
  expect_identical(error$response_body, long_body)

  testthat::local_mocked_bindings(
    RETRY = function(...) {
      fake_http_response(
        status = 400L,
        body = "A consulta contém 50000 valores"
      )
    },
    .package = "httr"
  )
  generic <- expect_error(
    sidrar:::.sidra_request(
      "https://apisidra.ibge.gov.br/values/t/1"
    ),
    class = "sidrar_http_error"
  )
  expect_false(inherits(generic, "sidrar_limit_error"))

  testthat::local_mocked_bindings(
    RETRY = function(...) {
      fake_http_response(
        status = 500L,
        body = paste(
          "Quantidade de valores solicitados: 83550",
          "excedeu o limite: 50000"
        )
      )
    },
    .package = "httr"
  )
  server_error <- expect_error(
    sidrar:::.sidra_request(
      "https://apisidra.ibge.gov.br/values/t/1"
    ),
    class = "sidrar_http_error"
  )
  expect_false(inherits(server_error, "sidrar_limit_error"))
})

test_that("transport failures have structured fields and conservative classes", {
  url <- "https://apisidra.ibge.gov.br/values/t/1"
  current_error <- NULL
  testthat::local_mocked_bindings(
    RETRY = function(...) stop(current_error),
    .package = "httr"
  )

  cases <- list(
    timeout = list(
      message = "Timeout was reached",
      source_class = "curl_error_operation_timedout",
      expected = c("sidrar_timeout_error", "sidrar_transient_error")
    ),
    tls = list(
      message = "SSL certificate problem",
      source_class = "curl_error_ssl_cacert",
      expected = "sidrar_tls_error"
    ),
    dns = list(
      message = "Could not resolve host: apisidra.ibge.gov.br",
      source_class = "curl_error_couldnt_resolve_host",
      expected = "sidrar_dns_error"
    ),
    connection = list(
      message = "Recv failure: Connection was reset",
      source_class = "curl_error_recv_error",
      expected = c("sidrar_connection_error", "sidrar_transient_error")
    ),
    transient = list(
      message = "Service temporarily unavailable; try again",
      source_class = "curl_error",
      expected = "sidrar_transient_error"
    ),
    unclassified = list(
      message = "libcurl request failed",
      source_class = "curl_error",
      expected = character()
    )
  )

  for (case in cases) {
    current_error <- structure(
      list(message = case$message, call = NULL),
      class = c(case$source_class, "error", "condition")
    )
    error <- expect_error(
      sidrar:::.sidra_request(url),
      class = "sidrar_http_error"
    )
    for (expected_class in case$expected) {
      expect_s3_class(error, expected_class)
    }
    expect_identical(error$status_code, NA_integer_)
    expect_identical(error$response_body, "")
    expect_identical(error$url, url)
  }
})

test_that("empty-response failures use package parse error classes", {
  testthat::local_mocked_bindings(
    RETRY = function(...) fake_http_response(body = ""),
    .package = "httr"
  )
  expect_error(
    sidrar:::.sidra_request(
      "https://apisidra.ibge.gov.br/values/t/1"
    ),
    "empty response",
    class = "sidrar_parse_error"
  )
})

test_that("invalid request options fall back to safe defaults", {
  seen_times <- NULL
  old_options <- options(
    sidrar.timeout = Inf,
    sidrar.retries = 3e9
  )
  on.exit(options(old_options), add = TRUE)

  testthat::local_mocked_bindings(
    RETRY = function(..., times) {
      seen_times <<- times
      fake_http_response()
    },
    .package = "httr"
  )

  sidrar:::.sidra_request(
    "https://apisidra.ibge.gov.br/values/t/1"
  )
  expect_identical(seen_times, 3L)
})
