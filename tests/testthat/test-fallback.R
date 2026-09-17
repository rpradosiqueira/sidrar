fallback_values_json <- function() {
  paste0(
    '[{"NC":"Nível Territorial (Código)","NN":"Nível Territorial",',
    '"MC":"Unidade de Medida (Código)","MN":"Unidade de Medida","V":"Valor",',
    '"D1C":"Brasil (Código)","D1N":"Brasil",',
    '"D2C":"Mês (Código)","D2N":"Mês",',
    '"D3C":"Variável (Código)","D3N":"Variável"},',
    '{"NC":"1","NN":"Brasil","MC":"2","MN":"%","V":"0.16",',
    '"D1C":"1","D1N":"Brasil","D2C":"202601","D2N":"janeiro 2026",',
    '"D3C":"63","D3N":"Variação mensal"}]'
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

test_that("all, first and interval periods retain their complete selections", {
  selectors <- c(
    "all", "first", "first%205", "2020-2022", "2018,2020-2022,2024",
    "last", "last%2012"
  )
  expected <- c(
    "all", "first", "first 5", "2020-2022", "2018|2020-2022|2024",
    "-1", "-12"
  )
  for (i in seq_along(selectors)) {
    result <- sidrar:::.sidra_fallback_url(paste0(
      "https://apisidra.ibge.gov.br/values/t/1/n1/all/v/63/p/",
      selectors[[i]]
    ))
    expect_null(result$reason, info = selectors[[i]])
    expect_identical(
      utils::URLdecode(httr::parse_url(result$url)$path),
      paste0("api/v3/agregados/1/periodos/", expected[[i]], "/variaveis/63"),
      info = selectors[[i]]
    )
    expect_identical(result$dimensions, c("n", "v", "p"))
  }
})

test_that("multiple geographic levels and dimension order are retained", {
  cases <- list(
    list(
      path = "t/1/n1/all/n2/all/n3/all/v/63/p/all",
      dimensions = c("n", "v", "p"),
      locality = "N1[all]|N2[all]|N3[all]",
      classification = NULL
    ),
    list(
      path = paste0(
        "t/1/p/all/n6/in%20n3%2031,50/n1/all/v/63/",
        "c82/all/n3/01,02/c81/2702,2701"
      ),
      dimensions = c("p", "n", "v", "c82", "c81"),
      locality = "N6[N3[31,50]]|N1[all]|N3[01,02]",
      classification = "82[all]|81[2702,2701]"
    ),
    list(
      path = "t/1/c81/2702,2701/v/63/p/all/n3/31/n1/all",
      dimensions = c("c81", "v", "p", "n"),
      locality = "N3[31]|N1[all]",
      classification = "81[2702,2701]"
    )
  )
  for (case in cases) {
    result <- sidrar:::.sidra_fallback_url(paste0(
      "https://apisidra.ibge.gov.br/values/", case$path
    ))
    expect_null(result$reason, info = case$path)
    expect_identical(result$dimensions, case$dimensions, info = case$path)
    parsed <- httr::parse_url(result$url)
    expect_identical(parsed$query$localidades, case$locality, info = case$path)
    expect_identical(
      parsed$query$classificacao, case$classification, info = case$path
    )
    expect_identical(parsed$query$view, "flat")
  }
})

test_that("global and variable-specific precision are retained for formatting", {
  cases <- list(
    list(
      selection = "s",
      expected = list(mode = "default", digits = NULL, variable = NULL)
    ),
    list(
      selection = "2",
      expected = list(mode = "global", digits = 2L, variable = NULL)
    ),
    list(
      selection = "v4099%201",
      expected = list(mode = "variable", digits = 1L, variable = "4099")
    )
  )
  for (case in cases) {
    result <- sidrar:::.sidra_fallback_url(paste0(
      "https://apisidra.ibge.gov.br/values/t/6468/n3/all/v/4099/p/all/d/",
      case$selection
    ))
    expect_null(result$reason, info = case$selection)
    expect_identical(result$precision, case$expected, info = case$selection)
    expect_identical(
      httr::parse_url(result$url)$path,
      "api/v3/agregados/6468/periodos/all/variaveis/4099"
    )
  }
})

test_that("all ten reported PNAD queries preserve their complete selections", {
  paths <- c(
    "/t/6469/n1/all/n2/all/n3/all/v/5935/p/all",
    "/t/6472/n1/all/n2/all/n3/all/v/5933/p/all",
    "/t/6468/n1/all/n2/all/n3/all/v/4099/p/all/d/v4099%201",
    "/t/6461/n1/all/n2/all/n3/all/v/4096/p/all/d/v4096%201",
    "/t/4099/n1/all/n2/all/n3/all/v/4118/p/all/d/v4118%201",
    "/t/8529/n1/all/n2/all/n3/all/v/12466/p/all/d/v12466%201",
    "/t/6385/n1/all/n2/all/n3/all/v/4108/p/all/c12043/31656/d/v4108%201",
    paste0(
      "/t/1616/n1/all/n2/all/n3/all/v/4110/p/all/",
      "c1965/31829,101227/d/v4110%201"
    ),
    paste0(
      "/t/5440/n1/all/n2/all/n3/all/v/5932,5934/p/all/",
      "c11913/31722,31723,31725,31726,31727,96165,96170,96171"
    ),
    paste0(
      "/t/4097/n1/all/n2/all/n3/all/v/4108/p/all/",
      "c11913/31722,31723,31725,31726,31727,96170,96171/d/v4108%201"
    )
  )
  tables <- c(
    "6469", "6472", "6468", "6461", "4099", "8529", "6385", "1616",
    "5440", "4097"
  )
  variables <- c(
    "5935", "5933", "4099", "4096", "4118", "12466", "4108", "4110",
    "5932|5934", "4108"
  )
  classifications <- list(
    NULL, NULL, NULL, NULL, NULL, NULL,
    "12043[31656]", "1965[31829,101227]",
    "11913[31722,31723,31725,31726,31727,96165,96170,96171]",
    "11913[31722,31723,31725,31726,31727,96170,96171]"
  )
  class_keys <- list(
    character(), character(), character(), character(), character(),
    character(), "c12043", "c1965", "c11913", "c11913"
  )
  precision_variables <- c(
    NA_character_, NA_character_, "4099", "4096", "4118", "12466",
    "4108", "4110", NA_character_, "4108"
  )
  for (i in seq_along(paths)) {
    result <- sidrar:::.sidra_fallback_url(paste0(
      "https://apisidra.ibge.gov.br/values", paths[[i]]
    ))
    expect_null(result$reason, info = paths[[i]])
    parsed <- httr::parse_url(result$url)
    expect_identical(parsed$hostname, "servicodados.ibge.gov.br")
    expect_identical(
      utils::URLdecode(parsed$path),
      paste0(
        "api/v3/agregados/", tables[[i]], "/periodos/all/variaveis/",
        variables[[i]]
      ),
      info = paths[[i]]
    )
    expect_identical(parsed$query$localidades, "N1[all]|N2[all]|N3[all]")
    expect_identical(
      parsed$query$classificacao, classifications[[i]], info = paths[[i]]
    )
    expect_identical(parsed$query$view, "flat")
    expect_identical(result$classes, class_keys[[i]], info = paths[[i]])
    expect_identical(
      result$dimensions, c("n", "v", "p", class_keys[[i]]), info = paths[[i]]
    )
    expected_precision <- if (is.na(precision_variables[[i]])) {
      list(mode = "default", digits = NULL, variable = NULL)
    } else {
      list(mode = "variable", digits = 1L, variable = precision_variables[[i]])
    }
    expect_identical(result$precision, expected_precision, info = paths[[i]])
  }
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
    "t/1/n1/1/p/last/v/allxp/g/1",
    "t/1/n1/1/p/last/v/allxp/u/y", "t/1/n1/1/p/last/v/allxp/u/n",
    "t/1/n1/1/p/last/v/allxp/o/p", "t/1/n1/1/p/last/v/allxp/z/1",
    "t/1/n1/1/p/first%200/v/allxp", "t/1/n1/1/p/2020-/v/allxp",
    "t/1/n1/1/p/2020--2022/v/allxp", "t/1/n1/1/p/all,2020/v/allxp",
    "t/1/n1/1/p/last%200/v/allxp", "t/1/n1/1/p/2020+2021/v/allxp",
    "t/1/n1/1/p/last/v/allxt", "t/1/n1/1/p/last/v/1+2",
    "t/1/n1/1/p/last/v/all,1", "t/1/n1/1/p/last/v/allxp/c1/allxt",
    "t/1/n1/1/p/last/v/allxp/c1/1+2", "t/1/n1/1/p/last/v/allxp/c1/1-2",
    "t/1/n1/1/p/last/v/allxp/f/c", "t/1/n1/1/p/last/v/allxp/f/n",
    "t/1/n1/1/p/last/v/allxp/f/u", "t/1/n1/1/p/last/v/allxp/d/m",
    "t/1/n1/1/p/last/v/allxp/d/1.5", "t/1/n1/1/p/last/v/allxp/h/x",
    "t/1/n6/in%20n3%20all/p/last/v/allxp",
    "t/1/n6/in%20n3%2031%20in%20n2%202/p/last/v/allxp",
    "t/1/n1/1/p/2020/p/2021/v/allxp", "t/1/n1/1/t/2/p/last/v/allxp",
    "t/1/n1/1/n1/all/p/last/v/allxp", "t/1/n1/1/p/last/v/allxp/c1/1/c1/2",
    "t/1/n1/1/p/last/v/allxp/h", "t/1/n1/1/p/last/v/allxp/",
    "t/1/n1//p/2020/v/allxp"
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
    "https://apisidra.ibge.gov.br/values/t/1/n1/1/p/last/v/allxp/g/1",
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
