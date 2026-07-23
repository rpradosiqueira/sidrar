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

  expect_error(
    sidrar:::.sidra_request(
      "https://apisidra.ibge.gov.br/values/t/1"
    ),
    regexp = "HTTP 400.*Parâmetro de período inválido",
    class = "sidrar_http_error"
  )
})

test_that("transport and empty-response failures use package error classes", {
  testthat::local_mocked_bindings(
    RETRY = function(...) stop("connection reset"),
    .package = "httr"
  )
  expect_error(
    sidrar:::.sidra_request(
      "https://apisidra.ibge.gov.br/values/t/1"
    ),
    "connection reset",
    class = "sidrar_http_error"
  )

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
    sidrar.retries = Inf
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
