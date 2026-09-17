fallback_values_json <- function() {
  paste0(
    '[{"NC":"Nível Territorial (Código)","NN":"Nível Territorial",',
    '"MC":"Unidade de Medida (Código)","MN":"Unidade de Medida","V":"Valor"},',
    '{"NC":"1","NN":"Brasil","MC":"2","MN":"%","V":"0.16"}]'
  )
}

test_that("the reported query translates without dropping selections", {
  original <- paste0(
    "https://apisidra.ibge.gov.br/values/t/10061/n3/31/p/2022/v/2667/",
    "c1568/9493,9494,9495,99713/f/a/h/y/d/s"
  )
  result <- sidrar:::.sidra_fallback_url(original)
  parsed <- httr::parse_url(result$url)

  expect_null(result$reason)
  expect_identical(parsed$hostname, "servicodados.ibge.gov.br")
  expect_identical(
    parsed$path, "api/v3/agregados/10061/periodos/2022/variaveis/2667"
  )
  expect_identical(parsed$query, list(
    localidades = "N3[31]",
    classificacao = "1568[9493,9494,9495,99713]",
    view = "flat"
  ))
})

test_that("translation retains list order, identifiers and nested geography", {
  original <- paste0(
    "https://apisidra.ibge.gov.br/values/t/1612/n6/in%20n3%2031,50/",
    "p/2021,2020/v/215,214/c81/2702,2701/c82/all/h/n?formato=json"
  )
  result <- sidrar:::.sidra_fallback_url(original)
  parsed <- httr::parse_url(result$url)

  expect_null(result$reason)
  expect_identical(
    utils::URLdecode(parsed$path),
    "api/v3/agregados/1612/periodos/2021|2020/variaveis/215|214"
  )
  expect_identical(parsed$query, list(
    localidades = "N6[N3[31,50]]",
    classificacao = "81[2702,2701]|82[all]",
    view = "flat"
  ))
  expect_identical(
    httr::parse_url(sidrar:::.sidra_fallback_url(paste0(
      "https://apisidra.ibge.gov.br/values/t/1/n3/01,02/p/last%2012/v/all"
    ))$url)$query$localidades,
    "N3[01,02]"
  )
})

test_that("period and variable selections are required for partial URLs", {
  for (path in c("t/1/n1/all", "t/1/n1/all/p/last", "t/1/n1/all/v/allxp")) {
    result <- sidrar:::.sidra_fallback_url(paste0(
      "https://apisidra.ibge.gov.br/values/", path
    ))
    expect_null(result$url)
    expect_match(result$reason, "period and variable")
  }

  result <- sidrar:::.sidra_fallback_url(
    "https://apisidra.ibge.gov.br/values/t/1/n1/1/p/last%2012/v/all"
  )
  expect_identical(
    httr::parse_url(result$url)$path,
    "api/v3/agregados/1/periodos/-12/variaveis/all"
  )
})

test_that("unsupported semantics never produce a lossy alternative URL", {
  paths <- c(
    "t/1/p/last/v/allxp", "t/x/n1/1/p/last/v/allxp",
    "t/1/n1/1/n3/31/p/last/v/allxp", "t/1/n1/1/p/last/v/allxp/g/1",
    "t/1/n1/1/p/last/v/allxp/u/y", "t/1/n1/1/p/last/v/allxp/u/n",
    "t/1/n1/1/p/last/v/allxp/o/p", "t/1/n1/1/p/last/v/allxp/z/1",
    "t/1/n1/1/p/first/v/allxp", "t/1/n1/1/p/first%205/v/allxp",
    "t/1/n1/1/p/all/v/allxp", "t/1/n1/1/p/2020-2022/v/allxp",
    "t/1/n1/1/p/last%200/v/allxp", "t/1/n1/1/p/2020+2021/v/allxp",
    "t/1/n1/1/p/last/v/allxt", "t/1/n1/1/p/last/v/1+2",
    "t/1/n1/1/p/last/v/all,1", "t/1/n1/1/p/last/v/allxp/c1/allxt",
    "t/1/n1/1/p/last/v/allxp/c1/1+2", "t/1/n1/1/p/last/v/allxp/c1/1-2",
    "t/1/n1/1/p/last/v/allxp/f/c", "t/1/n1/1/p/last/v/allxp/f/n",
    "t/1/n1/1/p/last/v/allxp/f/u", "t/1/n1/1/p/last/v/allxp/d/m",
    "t/1/n1/1/p/last/v/allxp/d/2", "t/1/n1/1/p/last/v/allxp/h/x",
    "t/1/n6/in%20n3%20all/p/last/v/allxp",
    "t/1/n6/in%20n3%2031%20in%20n2%202/p/last/v/allxp",
    "t/1/n1/1/p/2020/p/2021/v/allxp", "t/1/n1/1/t/2/p/last/v/allxp",
    "t/1/n1/1/n1/all/p/last/v/allxp", "t/1/n1/1/p/last/v/allxp/c1/1/c1/2",
    "t/1/n1/1/p/last/v/allxp/h", "t/1/n1/1/p/last/v/allxp/",
    "t/1/n1//p/2020/v/allxp", "t/1/n1/1/v/1/p/2020",
    "t/1/p/2020/n1/1/v/allxp", "t/1/n1/1/p/2020/c1/all/v/1"
  )
  for (path in paths) {
    result <- sidrar:::.sidra_fallback_url(paste0(
      "https://apisidra.ibge.gov.br/values/", path
    ))
    expect_null(result$url, info = path)
    expect_true(nzchar(result$reason), info = path)
  }
})

test_that("translation cannot change protected URL components", {
  urls <- c(
    "http://apisidra.ibge.gov.br/values/t/1/n1/1",
    "https://example.org/values/t/1/n1/1",
    "https://apisidra.ibge.gov.br.example.org/values/t/1/n1/1",
    "https://user@apisidra.ibge.gov.br/values/t/1/n1/1",
    "https://user:secret@apisidra.ibge.gov.br/values/t/1/n1/1",
    "https://@apisidra.ibge.gov.br/values/t/1/n1/1",
    "https://apisidra.ibge.gov.br:443/values/t/1/n1/1",
    "https://apisidra.ibge.gov.br/values/t/1/n1/1#fragment",
    "https://apisidra.ibge.gov.br/values/t/1/n1/1#",
    "https://apisidra.ibge.gov.br/values/t/1/n1/1?formato=xml",
    "https://apisidra.ibge.gov.br/values/t/1/n1/1?formato=json&extra=yes",
    "https://apisidra.ibge.gov.br/values/t/1/n1/1?formato=json&formato=json",
    "https://apisidra.ibge.gov.br/values/t/1/n1/1?other=value",
    "https://apisidra.ibge.gov.br/values/t/1/n1/1%2Fp%2F2020",
    "https://apisidra.ibge.gov.br/values/t/1/n1/1%3Fformato=json",
    "https://apisidra.ibge.gov.br/values/t/1/n1/1%23fragment",
    "https://apisidra.ibge.gov.br/DescritoresTabela/t/1",
    "https://servicodados.ibge.gov.br/api/v3/agregados/1/metadados"
  )
  for (url in urls) {
    result <- sidrar:::.sidra_fallback_url(url)
    expect_null(result$url, info = url)
    expect_true(nzchar(result$reason), info = url)
  }
  for (url in list(NULL, NA_character_, c("a", "b"), 1, "")) {
    expect_null(sidrar:::.sidra_fallback_url(url)$url)
  }
})

test_that("a successful primary response retains its actual header flag", {
  seen <- character()
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      seen <<- c(seen, url)
      values_without_header_json()
    },
    .package = "sidrar"
  )
  url <- "https://apisidra.ibge.gov.br/values/t/1/n1/1/h/n"
  result <- sidrar:::.sidra_values_request(url)
  expect_identical(result, list(
    text = values_without_header_json(), url = url, response_header = FALSE
  ))
  expect_identical(seen, url)
})

test_that("only a challenge activates one explicit alternative request", {
  seen <- character()
  url <- "https://apisidra.ibge.gov.br/values/t/1/n1/1/p/last/v/allxp/h/n"
  primary <- structure(
    list(message = "Cloudflare challenge", call = NULL, url = url),
    class = c("sidrar_challenge_error", "sidrar_http_error", "error", "condition")
  )
  testthat::local_mocked_bindings(
    .sidra_request = function(request_url) {
      seen <<- c(seen, request_url)
      if (identical(request_url, url)) stop(primary)
      fallback_values_json()
    },
    .package = "sidrar"
  )
  expect_message(
    result <- sidrar:::.sidra_values_request(url),
    "Cloudflare challenge; using IBGE's official aggregate API"
  )
  expect_identical(seen, c(url, result$url))
  expect_identical(result$text, fallback_values_json())
  expect_true(result$response_header)
  expect_match(result$url, "^https://servicodados[.]ibge[.]gov[.]br/")
})

test_that("other HTTP and parse errors do not trigger fallback", {
  seen <- 0L
  error <- NULL
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      seen <<- seen + 1L
      stop(error)
    },
    .package = "sidrar"
  )
  for (class in c("sidrar_http_error", "sidrar_limit_error", "sidrar_parse_error")) {
    error <- structure(
      list(message = "original failure", call = NULL, status_code = 403L),
      class = c(class, "error", "condition")
    )
    result <- tryCatch(
      sidrar:::.sidra_values_request(
        "https://apisidra.ibge.gov.br/values/t/1/n1/1/p/last/v/allxp"
      ), error = identity
    )
    expect_identical(result, error)
  }
  expect_identical(seen, 3L)
})

test_that("disabled and ineligible fallback preserve the primary challenge", {
  seen <- 0L
  primary <- structure(
    list(message = "challenge", call = NULL, status_code = 403L, cf_ray = "ray"),
    class = c("sidrar_challenge_error", "sidrar_http_error", "error", "condition")
  )
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      seen <<- seen + 1L
      stop(primary)
    },
    .package = "sidrar"
  )
  old <- options(sidrar.fallback = FALSE)
  on.exit(options(old), add = TRUE)
  expect_identical(
    tryCatch(sidrar:::.sidra_values_request(
      "https://apisidra.ibge.gov.br/values/t/1/n1/1/p/last/v/allxp"
    ), error = identity),
    primary
  )

  options(sidrar.fallback = TRUE)
  for (url in c(
    "https://apisidra.ibge.gov.br/values/t/1/n1/1/p/last/v/allxp/d/2",
    "https://apisidra.ibge.gov.br/DescritoresTabela/t/1"
  )) {
    result <- tryCatch(sidrar:::.sidra_values_request(url), error = identity)
    expect_s3_class(result, "sidrar_challenge_error")
    expect_identical(result$cf_ray, primary$cf_ray)
    expect_identical(result$status_code, primary$status_code)
    expect_true(nzchar(result$fallback_reason))
    expect_match(conditionMessage(result), "Alternative API unavailable")
  }
  expect_identical(seen, 3L)
})

test_that("failed alternatives retain their error and the primary challenge", {
  url <- "https://apisidra.ibge.gov.br/values/t/1/n1/1/p/last/v/allxp"
  seen <- character()
  primary <- structure(
    list(message = "original challenge", call = NULL, cf_ray = "original-ray"),
    class = c("sidrar_challenge_error", "sidrar_http_error", "error", "condition")
  )
  alternative <- NULL
  testthat::local_mocked_bindings(
    .sidra_request = function(request_url) {
      seen <<- c(seen, request_url)
      if (identical(request_url, url)) stop(primary)
      if (inherits(alternative, "condition")) stop(alternative)
      alternative
    },
    .package = "sidrar"
  )
  for (class in c("sidrar_http_error", "sidrar_challenge_error")) {
    alternative <- structure(
      list(message = "alternative failed", call = NULL, status_code = 503L),
      class = c(class, "error", "condition")
    )
    result <- suppressMessages(tryCatch(
      sidrar:::.sidra_values_request(url), error = identity
    ))
    expect_s3_class(result, class)
    expect_identical(result$status_code, alternative$status_code)
    expect_identical(result$primary_error, primary)
    expect_match(conditionMessage(result), "alternative API failed after")
  }
  for (alternative in c(
    "not JSON", "[]", '{"V":"1"}', '[{"V":"Valor"},{"V":"1"}]',
    '[{"NC":"1","NN":"Brasil","MC":"2","MN":"%","V":"0.16"}]'
  )) {
    result <- suppressMessages(tryCatch(
      sidrar:::.sidra_values_request(url), error = identity
    ))
    expect_s3_class(result, "sidrar_parse_error")
    expect_identical(result$primary_error, primary)
  }
  expect_length(seen, 14L)
  expect_identical(seen[seq.int(1L, 14L, by = 2L)], rep(url, 7L))
})
