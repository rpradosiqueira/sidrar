test_that("sidra_catalog preserves research groups and table codes", {
  payload <- list(
    list(
      id = "B",
      nome = "Second research",
      ignored = "future field",
      agregados = list(
        list(id = 20, nome = "Table twenty", extra = TRUE),
        list(id = "03")
      )
    ),
    list(id = "A", nome = "First research", agregados = list())
  )
  calls <- 0L
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      calls <<- calls + 1L
      expect_identical(
        url,
        "https://servicodados.ibge.gov.br/api/v3/agregados"
      )
      jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
    },
    .package = "sidrar"
  )

  result <- sidra_catalog()
  refreshed <- sidra_catalog(refresh = TRUE)

  expect_identical(calls, 2L)
  expect_identical(result, refreshed)
  expect_identical(
    names(result),
    c("research_id", "research_name", "table_id", "table_name")
  )
  expect_identical(result$research_id, c("A", "B", "B"))
  expect_identical(result$table_id, c(NA_character_, "3", "20"))
  expect_identical(result$table_name, c(NA_character_, NA_character_, "Table twenty"))
  expect_type(result$table_id, "character")
})

test_that("sidra_metadata normalizes all documented components", {
  metadata_payload <- list(
    id = 1419,
    nome = "IPCA table",
    URL = "https://sidra.ibge.gov.br/tabela/1419",
    pesquisa = "IPCA",
    assunto = "Prices",
    periodicidade = list(
      frequencia = "monthly",
      inicio = 201201,
      fim = 201912,
      future = "ignored"
    ),
    nivelTerritorial = list(
      Administrativo = c("N06", "N1"),
      Especial = list(),
      IBGE = "7"
    ),
    variaveis = list(
      list(
        id = 63,
        nome = "Monthly change",
        unidade = "%",
        sumarizacao = list("periodo", "nivelTerritorial")
      ),
      list(
        id = "69",
        nome = "Year-to-date change",
        sumarizacao = list()
      )
    ),
    classificacoes = list(
      list(
        id = 315,
        nome = "Products",
        sumarizacao = list(status = TRUE, excecao = list(63, 69)),
        categorias = list(
          list(id = 900, nome = "Overall", unidade = NULL, nivel = -1),
          list(id = "100", nome = "Food", nivel = 0)
        )
      ),
      list(id = 999, categorias = list())
    ),
    extra = list(raw = "must not leak")
  )
  period_payload <- list(
    list(
      id = "201202",
      literals = c("February 2012", "February of 2012"),
      modificacao = "17/07/2013"
    ),
    list(id = 201201, literals = "January 2012")
  )
  seen <- character()
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      seen <<- c(seen, url)
      if (grepl("/metadados$", url)) {
        return(jsonlite::toJSON(
          metadata_payload,
          auto_unbox = TRUE,
          null = "null"
        ))
      }
      if (grepl("/periodos$", url)) {
        return(jsonlite::toJSON(
          period_payload,
          auto_unbox = TRUE,
          null = "null"
        ))
      }
      stop("unexpected endpoint")
    },
    .package = "sidrar"
  )

  result <- sidra_metadata(1419, refresh = TRUE)

  expect_identical(
    seen,
    c(
      paste0(
        "https://servicodados.ibge.gov.br/api/v3/agregados/",
        "1419/metadados"
      ),
      paste0(
        "https://servicodados.ibge.gov.br/api/v3/agregados/",
        "1419/periodos"
      )
    )
  )
  expect_identical(
    names(result),
    c(
      "table", "periods", "variables", "classifications", "categories",
      "geographies"
    )
  )
  expect_true(all(vapply(result, is.data.frame, logical(1))))
  expect_identical(result$table$table_id, "1419")
  expect_identical(result$table$period_start, "201201")
  expect_identical(result$periods$period_id, c("201201", "201202"))
  expect_identical(
    result$periods$alternative_names[[2L]],
    "February of 2012"
  )
  expect_identical(result$variables$variable_id, c("63", "69"))
  expect_true(is.na(result$variables$unit[[2L]]))
  expect_identical(
    result$variables$summarized_by[[1L]],
    c("periodo", "nivelTerritorial")
  )
  expect_identical(result$variables$summarized_by[[2L]], character())
  expect_identical(
    result$classifications$classification_id,
    c("315", "999")
  )
  expect_identical(result$classifications$summarizable, c(TRUE, NA))
  expect_identical(
    result$classifications$summarization_exceptions[[1L]],
    c("63", "69")
  )
  expect_identical(
    result$classifications$summarization_exceptions[[2L]],
    character()
  )
  expect_identical(result$categories$category_id, c("900", "100"))
  expect_identical(result$categories$level, c(-1L, 0L))
  expect_identical(result$categories$category_order, c(1L, 2L))
  expect_identical(
    result$geographies$level_id,
    c("N1", "N6", "N7")
  )
  expect_false("extra" %in% names(result$table))
})

test_that("period and location endpoints return stable empty schemas", {
  seen <- character()
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      seen <<- c(seen, url)
      "[]"
    },
    .package = "sidrar"
  )

  periods <- sidra_periods("001")
  locations <- sidra_locations("001", "N06", refresh = TRUE)

  expect_identical(
    names(periods),
    c(
      "table_id", "period_id", "period_name", "alternative_names", "modified"
    )
  )
  expect_identical(
    names(locations),
    c("table_id", "level_id", "level_name", "location_id", "location_name")
  )
  expect_identical(nrow(periods), 0L)
  expect_identical(nrow(locations), 0L)
  expect_type(periods$period_id, "character")
  expect_type(periods$alternative_names, "list")
  expect_type(locations$location_id, "character")
  expect_identical(
    seen,
    c(
      paste0(
        "https://servicodados.ibge.gov.br/api/v3/agregados/1/periodos"
      ),
      paste0(
        "https://servicodados.ibge.gov.br/api/v3/agregados/1/",
        "localidades/N6"
      )
    )
  )
})

test_that("sidra_locations normalizes level and incomplete records", {
  payload <- list(
    list(
      id = 5300108,
      nome = "Brasilia",
      nivel = list(id = "n6", nome = "Municipality"),
      future = 1
    ),
    list(id = "1200401")
  )
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      expect_match(url, "/localidades/N6$", fixed = FALSE)
      jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
    },
    .package = "sidrar"
  )

  result <- sidra_locations(1419, " n6 ")

  expect_identical(result$location_id, c("1200401", "5300108"))
  expect_identical(result$level_id, c("N6", "N6"))
  expect_true(is.na(result$location_name[[1L]]))
  expect_true(is.na(result$level_name[[1L]]))
})

test_that("discovery functions tolerate incomplete metadata payloads", {
  seen <- character()
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      seen <<- c(seen, url)
      if (grepl("/metadados$", url)) "{}" else "[]"
    },
    .package = "sidrar"
  )

  metadata <- sidra_metadata("7060")
  catalog <- sidra_catalog()

  expect_identical(metadata$table$table_id, "7060")
  expect_true(is.na(metadata$table$table_name))
  expect_identical(nrow(metadata$periods), 0L)
  expect_identical(nrow(metadata$variables), 0L)
  expect_identical(nrow(metadata$classifications), 0L)
  expect_identical(nrow(metadata$categories), 0L)
  expect_identical(nrow(metadata$geographies), 0L)
  expect_identical(nrow(catalog), 0L)
  expect_length(seen, 3L)
})

test_that("discovery arguments are validated before requests", {
  testthat::local_mocked_bindings(
    .sidra_request = function(url) stop("request should not be made"),
    .package = "sidrar"
  )

  expect_error(sidra_catalog(refresh = NA), "TRUE or FALSE")
  expect_error(sidra_metadata("table"), "numeric SIDRA table code")
  expect_error(sidra_periods(c(1, 2)), "exactly one SIDRA table")
  expect_error(sidra_locations(1, "municipality"), "code such as")
  expect_error(sidra_locations(1, c("N1", "N6")), "exactly one")
  expect_error(sidra_locations(1, "N6", refresh = 1), "TRUE or FALSE")
})

test_that("metadata always uses the independently refreshed period cache", {
  directory <- tempfile("sidrar-discovery-cache-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  period_calls <- 0L
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      if (grepl("/metadados$", url)) {
        return('{"id":"1","nome":"Table"}')
      }
      if (grepl("/periodos$", url)) {
        period_calls <<- period_calls + 1L
        return(paste0(
          '[{"id":"p', period_calls,
          '","literals":["Period ', period_calls, '"]}]'
        ))
      }
      stop("unexpected endpoint")
    },
    .package = "sidrar"
  )

  first <- sidra_metadata(1, cache = TRUE, cache_dir = directory)
  refreshed <- sidra_periods(
    1, cache = TRUE, refresh = TRUE, cache_dir = directory
  )
  second <- sidra_metadata(1, cache = TRUE, cache_dir = directory)

  expect_identical(first$periods$period_id, "p1")
  expect_identical(refreshed$period_id, "p2")
  expect_identical(second$periods$period_id, "p2")
  expect_identical(period_calls, 2L)
})

test_that("table and geographic level codes use one canonical spelling", {
  seen <- character()
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      seen <<- c(seen, url)
      if (grepl("/metadados$", url)) {
        return('{"id":"07060","nome":"Table"}')
      }
      "[]"
    },
    .package = "sidrar"
  )

  metadata <- sidra_metadata("07060")
  locations <- sidra_locations("07060", "N06")

  expect_identical(metadata$table$table_id, "7060")
  expect_true(all(vapply(metadata, function(x) {
    nrow(x) == 0L || all(x$table_id == "7060")
  }, logical(1))))
  expect_identical(
    seen,
    c(
      paste0(
        "https://servicodados.ibge.gov.br/api/v3/agregados/",
        "7060/metadados"
      ),
      paste0(
        "https://servicodados.ibge.gov.br/api/v3/agregados/",
        "7060/periodos"
      ),
      paste0(
        "https://servicodados.ibge.gov.br/api/v3/agregados/",
        "7060/localidades/N6"
      )
    )
  )
})
