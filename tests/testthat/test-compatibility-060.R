# Synthetic compatibility fixtures, not observations downloaded from IBGE.
# Survey-shaped schemas exercise contracts independently of live availability.
compat060_cases <- function() {
  list(
    monthly_prices = list(
      period_label = "Mês", periods = c("202001", "202002"),
      period_names = c("janeiro 2020", "fevereiro 2020"),
      classifications = c(c315 = "Grupo, item e subitem"),
      measure = "Variação mensal", unit = "%"
    ),
    quarterly_labour = list(
      period_label = "Trimestre", periods = c("202001", "202002"),
      period_names = c("1º trimestre 2020", "2º trimestre 2020"),
      classifications = c(c11913 = "Condição na força de trabalho"),
      measure = "Pessoas ocupadas", unit = "Mil pessoas"
    ),
    annual_agriculture = list(
      period_label = "Ano", periods = c("2019", "2020"),
      period_names = c("2019", "2020"),
      classifications = character(),
      measure = "Área colhida", unit = "Hectares"
    ),
    census_two_classifications = list(
      period_label = "Ano", periods = c("2010", "2022"),
      period_names = c("2010", "2022"),
      classifications = c(c1 = "Sexo", c2 = "Cor ou raça"),
      measure = "População residente", unit = "Pessoas"
    )
  )
}

compat060_fixture <- function(case) {
  result <- data.frame(
    NC = c("Nível Territorial (Código)", rep("003", 6L)),
    NN = c("Nível Territorial", rep("Unidade da Federação", 6L)),
    MC = c("Unidade de Medida (Código)", rep("0007", 6L)),
    MN = c("Unidade de Medida", rep(case$unit, 6L)),
    V = c("Valor", "-", "X", "..", "...", "0.00", "-12.50"),
    D1C = c("UF (Código)", rep(c("0031", "0050", "0012"), 2L)),
    D1N = c("UF", rep(c("Minas Gerais", "Mato Grosso do Sul", "Acre"), 2L)),
    D2C = c(paste0(case$period_label, " (Código)"), rep(case$periods, each = 3L)),
    D2N = c(case$period_label, rep(case$period_names, each = 3L)),
    D3C = c("Variável (Código)", rep(c("00101", "00102", "00101"), 2L)),
    D3N = c("Variável", rep(c(case$measure, "Índice complementar", case$measure), 2L)),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(case$classifications)) {
    # Reused category IDs across classifications are distinct dimensions.
    result[[paste0("D", 3L + i, "C")]] <- c(
      paste0(case$classifications[[i]], " (Código)"),
      rep(c("0001", "0002", "0001"), 2L)
    )
    result[[paste0("D", 3L + i, "N")]] <- c(
      case$classifications[[i]],
      rep(c(paste0("Não especificado ", i), paste0("Descrição São Luís ", i),
            paste0("Não especificado ", i)), 2L)
    )
  }
  result
}

compat060_url <- function(case, dimensions, header) {
  selections <- c(
    n = "n3/31,50,12", p = paste0("p/", paste(case$periods, collapse = ",")),
    v = "v/101,102",
    stats::setNames(paste0(names(case$classifications), "/1,2"),
                    names(case$classifications))
  )
  paste0("/t/999999/", paste(selections[dimensions], collapse = "/"),
         "/h/", if (header) "y" else "n")
}

compat060_permute <- function(fixture, case, dimensions) {
  canonical <- c("n", "p", "v", names(case$classifications))
  indices <- match(dimensions, canonical)
  dimension_columns <- as.vector(rbind(
    paste0("D", indices, "C"), paste0("D", indices, "N")
  ))
  result <- fixture[c("NC", "NN", "MC", "MN", "V", dimension_columns)]
  names(result) <- names(fixture)
  result
}

compat060_expected <- function(fixture, header, value_type) {
  result <- fixture[-1L, , drop = FALSE]
  if (header) names(result) <- unname(unlist(fixture[1L, ]))
  rownames(result) <- NULL
  value <- if (header) "Valor" else "V"
  raw <- result[[value]]
  if (value_type != "character") result[[value]] <- c(rep(NA_real_, 4L), 0, -12.5)
  if (value_type == "both") result[[paste0(value, "_raw")]] <- raw
  result
}

compat060_sort_by_key <- function(result, header) {
  keys <- if (header) {
    grep(" \\(Código\\)$", names(result), value = TRUE)
  } else {
    c("NC", grep("^D[0-9]+C$", names(result), value = TRUE))
  }
  keys <- setdiff(keys, "Unidade de Medida (Código)")
  result <- result[do.call(order, unname(result[keys])), , drop = FALSE]
  rownames(result) <- NULL
  result
}

test_that("survey-shaped schemas preserve values and identifiers through both routes", {
  old <- options(sidrar.fallback = TRUE)
  on.exit(options(old), add = TRUE)
  response_text <- NULL
  blocked <- FALSE
  calls <- character()
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      calls <<- c(calls, url)
      if (blocked && startsWith(url, "https://apisidra.ibge.gov.br/")) {
        sidrar:::.sidrar_abort(
          "Synthetic browser challenge",
          c("sidrar_challenge_error", "sidrar_http_error"),
          status_code = 403L, url = url
        )
      }
      response_text
    },
    .package = "sidrar"
  )

  for (case in compat060_cases()) {
    fixture <- compat060_fixture(case)
    dimensions <- c("v", names(case$classifications), "n", "p")
    permuted <- compat060_permute(fixture, case, dimensions)
    for (header in c(TRUE, FALSE)) {
      url <- compat060_url(case, dimensions, header)
      for (value_type in c("numeric", "character", "both")) {
        expected <- compat060_expected(permuted, header, value_type)
        blocked <- FALSE
        primary <- if (header) permuted else permuted[-1L, , drop = FALSE]
        response_text <- as.character(jsonlite::toJSON(primary))
        calls <- character()
        direct <- suppressMessages(get_sidra(api = url, value_type = value_type))
        expect_identical(direct, expected)
        expect_length(calls, 1L)

        blocked <- TRUE
        # The alternative returns the same observations in a different order.
        permutation <- c(1L, 7L, 3L, 6L, 2L, 5L, 4L)
        response_text <- as.character(jsonlite::toJSON(fixture[permutation, ]))
        calls <- character()
        fallback <- suppressMessages(get_sidra(api = url, value_type = value_type))
        expect_identical(
          compat060_sort_by_key(fallback, header),
          compat060_sort_by_key(direct, header)
        )
        expect_length(calls, 2L)
        expect_match(calls[[2L]], "^https://servicodados[.]ibge[.]gov[.]br/")
        expect_false(identical(fallback, direct))
        # Character codes retain padding even when selections use unpadded IDs.
        location <- if (header) "UF (Código)" else
          paste0("D", match("n", dimensions), "C")
        expect_setequal(fallback[[location]], c("0031", "0050", "0012"))
        code_columns <- if (header) {
          grep(" \\(Código\\)$", names(fallback), value = TRUE)
        } else c("NC", "MC", grep("^D[0-9]+C$", names(fallback), value = TRUE))
        expect_true(all(vapply(fallback[code_columns], is.character, logical(1))))
        expect_identical(nrow(fallback), 6L)
      }
    }
  }
})

test_that("multi-classification values remain aligned when URL dimensions move", {
  old <- options(sidrar.fallback = TRUE)
  on.exit(options(old), add = TRUE)
  case <- compat060_cases()$census_two_classifications
  fixture <- compat060_fixture(case)
  response_text <- as.character(jsonlite::toJSON(fixture))
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      if (startsWith(url, "https://apisidra.ibge.gov.br/")) {
        sidrar:::.sidrar_abort("Synthetic challenge", "sidrar_challenge_error")
      }
      response_text
    },
    .package = "sidrar"
  )
  permutations <- list(
    c("n", "p", "v", "c1", "c2"),
    c("c1", "v", "c2", "p", "n"),
    c("p", "c1", "n", "c2", "v")
  )
  for (dimensions in permutations) {
    for (header in c(TRUE, FALSE)) {
      result <- suppressMessages(get_sidra(
        api = compat060_url(case, dimensions, header), value_type = "both"
      ))
      expect_identical(result, compat060_expected(
        compat060_permute(fixture, case, dimensions), header, "both"
      ))
    }
  }
})

test_that("territorial levels distinguish observations sharing locality codes", {
  old <- options(sidrar.fallback = TRUE)
  on.exit(options(old), add = TRUE)
  case <- compat060_cases()$annual_agriculture
  fixture <- compat060_fixture(case)[1:3, ]
  fixture$NC[-1L] <- c("1", "2")
  fixture$NN[-1L] <- c("Brasil", "Grande Região")
  fixture$D1C[-1L] <- "0001"
  fixture$D1N[-1L] <- c("Brasil", "Norte")
  fixture$D2C[-1L] <- "2020"
  fixture$D2N[-1L] <- "2020"
  fixture$D3C[-1L] <- "00101"
  fixture$D3N[-1L] <- "Área colhida"
  response_text <- as.character(jsonlite::toJSON(fixture))
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      if (startsWith(url, "https://apisidra.ibge.gov.br/")) {
        sidrar:::.sidrar_abort("Synthetic challenge", "sidrar_challenge_error")
      }
      response_text
    },
    .package = "sidrar"
  )
  result <- suppressMessages(get_sidra(
    api = "/t/999999/n1/1/n2/1/p/2020/v/101/h/n", value_type = "character"
  ))
  expect_identical(result$NC, c("1", "2"))
  expect_identical(result$D1C, c("0001", "0001"))
  expect_identical(result$D1N, c("Brasil", "Norte"))
  expect_identical(result$V, c("-", "X"))
})

test_that("large textual identifiers are never rounded during filter matching", {
  old <- options(sidrar.fallback = TRUE)
  on.exit(options(old), add = TRUE)
  case <- compat060_cases()$census_two_classifications
  fixture <- compat060_fixture(case)[1:3, ]
  fixture$D1C[-1L] <- c("09007199254740993", "09007199254740994")
  fixture$D2C[-1L] <- "2022"
  fixture$D2N[-1L] <- "2022"
  fixture$D3C[-1L] <- "00101"
  fixture$D3N[-1L] <- "População residente"
  response_text <- as.character(jsonlite::toJSON(fixture))
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      if (startsWith(url, "https://apisidra.ibge.gov.br/")) {
        sidrar:::.sidrar_abort("Synthetic challenge", "sidrar_challenge_error")
      }
      response_text
    },
    .package = "sidrar"
  )
  url <- paste0(
    "/t/999999/n3/9007199254740993,9007199254740994/",
    "p/2022/v/101/c1/1,2/c2/1,2/h/n"
  )
  result <- suppressMessages(get_sidra(api = url, value_type = "character"))
  expect_identical(result$D1C, c("09007199254740993", "09007199254740994"))
  expect_identical(nrow(result), 2L)
})
