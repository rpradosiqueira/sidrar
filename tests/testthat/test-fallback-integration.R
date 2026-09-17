fallback_values_fixture <- function(
  periods = c("2022", "2023"), categories = c("9493", "9493"),
  classification = TRUE
) {
  records <- list(
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
      D2C = periods[[1L]], D2N = periods[[1L]],
      D3C = "2667", D3N = "Pessoas"
    ),
    list(
      NC = "3", NN = "Unidade da Federação", MC = "45", MN = "Pessoas",
      V = "12.50", D1C = "0031", D1N = "Minas Gerais",
      D2C = periods[[2L]], D2N = periods[[2L]],
      D3C = "2667", D3N = "Pessoas"
    )
  )
  if (classification) {
    records[[1L]]$D4C <- "Categoria (Código)"
    records[[1L]]$D4N <- "Categoria"
    for (i in seq_along(categories)) {
      records[[i + 1L]]$D4C <- categories[[i]]
      records[[i + 1L]]$D4N <- paste("Categoria", categories[[i]])
    }
  }
  jsonlite::toJSON(records, auto_unbox = TRUE)
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
        10061, variable = 2667, period = c("2022", "2023"), geo = "State",
        geo.filter = list(State = 31), classific = "c1568",
        category = list(9493),
        header = header, value_type = type
      ))
      expect_s3_class(result, "data.frame")
      expect_identical(nrow(result), 2L)
      expect_identical(rownames(result), c("1", "2"))
      value <- if (header) "Valor" else "V"
      code <- if (header) "UF (Código)" else "D1C"
      expect_identical(result[[code]], c("0031", "0031"))
      period <- if (header) "Ano (Código)" else "D2C"
      category <- if (header) "Categoria (Código)" else "D4C"
      expect_identical(result[[period]], c("2022", "2023"))
      expect_identical(result[[category]], c("9493", "9493"))
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
    api = "/t/10061/n3/31/p/2022,2023/v/2667/c1568/9493/h/n",
    value_type = "both"
  ))
  expect_identical(result$V_raw, c("X", "12.50"))
  expect_identical(result$D1C, c("0031", "0031"))
  expect_identical(result$D2C, c("2022", "2023"))
  expect_identical(result$D4C, c("9493", "9493"))
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
      period <- if (grepl("/(p|periodos)/2023/", url)) "2023" else "2022"
      fallback_values_fixture(
        periods = rep(period, 2L), categories = c("9493", "9494")
      )
    },
    .package = "sidrar"
  )
  queries <- lapply(c("2022", "2023"), function(period) {
    sidra_query(10061, variable = 2667, period = period, geo = "State",
                geo.filter = list(State = 31), classific = "c1568",
                category = list(c(9493, 9494)), value_type = "both")
  })

  result <- suppressMessages(sidra_collect(queries, provenance = TRUE))
  provenance <- sidra_provenance(result)
  expect_identical(nrow(result), 4L)
  expect_identical(result$Valor_raw, rep(c("X", "12.50"), 2))
  expect_identical(result[["Ano (Código)"]], rep(c("2022", "2023"), each = 2))
  expect_identical(result[["Categoria (Código)"]], rep(c("9493", "9494"), 2))
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

test_that("reordered URL dimensions work with all public value representations", {
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      if (startsWith(url, "https://apisidra.ibge.gov.br/")) {
        sidrar:::.sidrar_abort(
          "Browser challenge", c("sidrar_challenge_error", "sidrar_http_error"),
          status_code = 403L, url = url, cf_ray = "integration-ray"
        )
      }
      fallback_values_fixture(classification = FALSE)
    },
    .package = "sidrar"
  )
  base <- "/t/10061/n3/31/v/2667/p/all/d/v2667%202"
  for (header in c("y", "n")) {
    for (type in c("numeric", "character", "both")) {
      result <- suppressMessages(get_sidra(
        api = paste0(base, "/h/", header), value_type = type
      ))
      expect_identical(nrow(result), 2L)
      if (header == "y") {
        expect_identical(names(result)[8:11], c(
          "Variável (Código)", "Variável", "Ano (Código)", "Ano"
        ))
        expect_identical(result[["UF (Código)"]], rep("0031", 2))
      } else {
        expect_identical(result$D2C, rep("2667", 2))
        expect_identical(result$D3C, c("2022", "2023"))
        expect_identical(result$D1C, rep("0031", 2))
      }
      value <- if (header == "y") "Valor" else "V"
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

  query <- sidra_query(api = base, value_type = "both")
  result <- suppressMessages(sidra_collect(query, provenance = TRUE))
  provenance <- sidra_provenance(result)
  expect_identical(provenance$requested_urls, query$url)
  expect_match(provenance$urls, "servicodados[.]ibge[.]gov[.]br")
  expect_identical(result$Valor_raw, c("X", "12.50"))

  error <- suppressMessages(tryCatch(
    get_sidra(api = sub("%202$", "%201", base)), error = identity
  ))
  expect_s3_class(error, "sidrar_fallback_precision_error")
  expect_s3_class(error, "sidrar_parse_error")
  expect_s3_class(error$primary_error, "sidrar_challenge_error")
  expect_identical(error$primary_error$cf_ray, "integration-ray")
})
