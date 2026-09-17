fallback_values_fixture <- function() {
  jsonlite::toJSON(list(
    list(
      NC = "Nível Territorial (Código)", NN = "Nível Territorial",
      MC = "Unidade de Medida (Código)", MN = "Unidade de Medida",
      V = "Valor", D1C = "UF (Código)", D1N = "UF",
      D2C = "Ano (Código)", D2N = "Ano",
      D3C = "Variável (Código)", D3N = "Variável"
    ),
    list(
      NC = "3", NN = "Unidade da Federação", MC = "45", MN = "Pessoas",
      V = "X", D1C = "0031", D1N = "Minas Gerais",
      D2C = "2022", D2N = "2022", D3C = "2667", D3N = "Pessoas"
    ),
    list(
      NC = "3", NN = "Unidade da Federação", MC = "45", MN = "Pessoas",
      V = "12.50", D1C = "0031", D1N = "Minas Gerais",
      D2C = "2023", D2N = "2023", D3C = "2667", D3N = "Pessoas"
    )
  ), auto_unbox = TRUE)
}

test_that("get_sidra keeps header and special-value contracts on fallback", {
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      if (startsWith(url, "https://apisidra.ibge.gov.br/")) {
        sidrar:::.sidrar_abort(
          "Browser challenge", c("sidrar_challenge_error", "sidrar_http_error"),
          status_code = 403L, response_body = "challenge", url = url,
          cf_ray = "test-ray"
        )
      }
      fallback_values_fixture()
    },
    .package = "sidrar"
  )

  for (header in c(TRUE, FALSE)) {
    for (type in c("numeric", "character", "both")) {
      result <- suppressMessages(get_sidra(
        10061, variable = 2667, period = "2022", geo = "State",
        geo.filter = list(State = 31), classific = "c1568",
        category = list(c(9493, 9494, 9495, 99713)),
        header = header, value_type = type
      ))
      expect_s3_class(result, "data.frame")
      expect_identical(nrow(result), 2L)
      expect_identical(rownames(result), c("1", "2"))
      value <- if (header) "Valor" else "V"
      code <- if (header) "UF (Código)" else "D1C"
      expect_identical(result[[code]], c("0031", "0031"))
      if (type == "character") {
        expect_identical(result[[value]], c("X", "12.50"))
      } else {
        expect_identical(result[[value]], c(NA_real_, 12.5))
        if (type == "both") {
          expect_identical(result[[paste0(value, "_raw")]], c("X", "12.50"))
        }
      }
    }
  }

  result <- suppressMessages(get_sidra(
    api = "/t/10061/n3/31/p/2022/v/2667/c1568/9493/h/n",
    value_type = "both"
  ))
  expect_identical(result$V_raw, c("X", "12.50"))
  expect_identical(result$D1C, c("0031", "0031"))
})

test_that("mixed primary and fallback batches retain actual provenance", {
  calls <- character()
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      calls <<- c(calls, url)
      if (startsWith(url, "https://apisidra.ibge.gov.br/") &&
          grepl("/p/2023/", url, fixed = TRUE)) {
        sidrar:::.sidrar_abort(
          "Browser challenge", c("sidrar_challenge_error", "sidrar_http_error"),
          status_code = 403L, response_body = "challenge", url = url
        )
      }
      fallback_values_fixture()
    },
    .package = "sidrar"
  )
  queries <- lapply(c("2022", "2023"), function(period) {
    sidra_query(10061, variable = 2667, period = period, geo = "State",
                geo.filter = list(State = 31), classific = "c1568",
                category = list(9493), value_type = "both")
  })

  result <- suppressMessages(sidra_collect(queries, provenance = TRUE))
  provenance <- sidra_provenance(result)
  expect_identical(nrow(result), 4L)
  expect_identical(result$Valor_raw, rep(c("X", "12.50"), 2))
  expect_identical(provenance$urls, calls[c(1L, 3L)])
  expect_identical(provenance$requested_urls,
                   vapply(queries, `[[`, character(1), "url"))
  expect_identical(provenance$batch_count, 2L)
})

test_that("API header records may be consumed without renaming columns", {
  result <- sidrar:::.parse_sidra_values(
    values_with_header_json(), header = FALSE, value_type = "both",
    response_header = TRUE
  )
  expect_identical(names(result), c("NC", "NN", "V", "V_raw"))
  expect_identical(nrow(result), 1L)
  expect_identical(result$V, 0.16)
  expect_identical(result$V_raw, "0.16")
  expect_error(sidrar:::.parse_sidra_values(
    "[]", header = FALSE, response_header = TRUE
  ), "no header record", class = "sidrar_parse_error")
})
