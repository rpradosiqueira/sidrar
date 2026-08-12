run_live_tests <- function() {
  identical(tolower(Sys.getenv("SIDRAR_RUN_LIVE_TESTS")), "true")
}

live_api_pause <- function() {
  seconds <- suppressWarnings(as.numeric(Sys.getenv(
    "SIDRAR_LIVE_PAUSE_SECONDS",
    unset = "1"
  )))
  if (!is.finite(seconds) || seconds < 0) {
    seconds <- 1
  }
  Sys.sleep(seconds)
}

test_that("the live SIDRA values endpoint returns a current observation", {
  skip_on_cran()
  skip_if_not(run_live_tests(), "Set SIDRAR_RUN_LIVE_TESTS=true")
  live_api_pause()

  result <- get_sidra(
    api = "/t/7060/n1/all/v/63/p/last/c315/7169/h/n"
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
  expect_true("V" %in% names(result))
})

test_that("a structured live query preserves the legacy schema", {
  skip_on_cran()
  skip_if_not(run_live_tests(), "Set SIDRAR_RUN_LIVE_TESTS=true")
  live_api_pause()

  result <- get_sidra(
    x = 7060,
    variable = 63,
    period = "last",
    geo = "City",
    geo.filter = list(State = 50),
    classific = "c315",
    category = list(7169)
  )

  message(
    "SIDRA schema fingerprint: ",
    paste(names(result), collapse = " | ")
  )

  expect_identical(
    names(result),
    c(
      "Nível Territorial (Código)",
      "Nível Territorial",
      "Unidade de Medida (Código)",
      "Unidade de Medida",
      "Valor",
      "Município (Código)",
      "Município",
      "Mês (Código)",
      "Mês",
      "Variável (Código)",
      "Variável",
      "Geral, grupo, subgrupo, item e subitem (Código)",
      "Geral, grupo, subgrupo, item e subitem"
    )
  )
  expect_type(result$Valor, "double")
  expect_true(all(vapply(result[-5L], is.character, logical(1))))
})

test_that("the live values endpoint accepts a complete URL", {
  skip_on_cran()
  skip_if_not(run_live_tests(), "Set SIDRAR_RUN_LIVE_TESTS=true")
  live_api_pause()

  url <- paste0(
    "https://apisidra.ibge.gov.br/values/",
    "t/7060/n1/all/v/63/p/last/c315/7169/h/n"
  )
  result <- suppressMessages(get_sidra(api = url))

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
  expect_true("V" %in% names(result))
})

test_that("live special values can be preserved", {
  skip_on_cran()
  skip_if_not(run_live_tests(), "Set SIDRAR_RUN_LIVE_TESTS=true")
  live_api_pause()

  result <- suppressMessages(get_sidra(
    api = "/t/1849/n3/all/v/811/p/2018/c12762/all",
    value_type = "both"
  ))

  expect_true(all(c("Valor", "Valor_raw") %in% names(result)))
  expect_true(any(result$Valor_raw == "X"))
  expect_true(all(is.na(result$Valor[result$Valor_raw == "X"])))
})

test_that("the live descriptor and catalog endpoints respond", {
  skip_on_cran()
  skip_if_not(run_live_tests(), "Set SIDRAR_RUN_LIVE_TESTS=true")
  live_api_pause()

  info <- info_sidra(7060)
  live_api_pause()
  matches <- search_sidra("IPCA")

  expect_identical(
    names(info),
    c("table", "period", "variable", "classific_category", "geo")
  )
  expect_match(info$period, " a ", fixed = TRUE)
  expect_type(info$classific_category, "list")
  expect_match(names(info$classific_category)[[1L]], "^c[0-9]+ ")
  expect_gt(length(matches), 0L)
  expect_true(all(nzchar(names(matches))))
})
