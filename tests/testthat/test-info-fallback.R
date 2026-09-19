info_fallback_challenge <- function() {
  structure(
    list(message = "Cloudflare challenge", call = NULL, status_code = 403L),
    class = c("sidrar_challenge_error", "sidrar_http_error", "error", "condition")
  )
}

info_fallback_metadata <- function() {
  list(
    id = 1419,
    nome = "IPCA - Monthly change",
    variaveis = list(
      list(id = 63, nome = "Monthly change", unidade = "%"),
      list(id = "2265", nome = "Twelve-month change", unidade = "%")
    ),
    classificacoes = list(list(
      id = 315, nome = "Products",
      categorias = list(
        list(id = 7169, nome = "Overall"),
        list(id = "7170", nome = "Food")
      )
    )),
    nivelTerritorial = list(
      Administrativo = c("N1", "N6"), Especial = "N7", IBGE = list()
    )
  )
}

info_fallback_periods <- function() {
  list(
    list(id = 201201, literals = list("January 2012")),
    list(id = "201203", literals = list("March 2012"))
  )
}

info_fallback_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
}

test_that("info_sidra preserves descriptor data without an alternative request", {
  old <- options(sidrar.fallback = TRUE)
  on.exit(options(old), add = TRUE)
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) descriptor_fixture(),
    .sidra_request = function(...) stop("must not request an alternative"),
    .package = "sidrar"
  )
  result <- info_sidra(1419)
  expect_identical(result, sidrar:::.descriptor_to_legacy_info(descriptor_fixture()))
  expect_null(attr(result, "sidrar_metadata"))
  expect_match(result$geo$desc[[1L]], "(1)", fixed = TRUE)
  expect_match(result$variable$desc[[2L]], "dezembro 2012", fixed = TRUE)
})

test_that("info_sidra falls back to fresh official metadata and periods", {
  old <- options(sidrar.fallback = TRUE)
  on.exit(options(old), add = TRUE)
  seen <- character()
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      seen <<- c(seen, url)
      if (grepl("DescritoresTabela", url, fixed = TRUE)) {
        stop(info_fallback_challenge())
      }
      if (grepl("/metadados$", url)) {
        return(info_fallback_json(info_fallback_metadata()))
      }
      if (grepl("/periodos$", url)) {
        return(info_fallback_json(info_fallback_periods()))
      }
      stop("unexpected endpoint")
    },
    .package = "sidrar"
  )
  expect_message(result <- info_sidra(1419), "counts.*unavailable")
  expect_identical(
    seen,
    c(
      "https://apisidra.ibge.gov.br/DescritoresTabela/t/1419",
      "https://servicodados.ibge.gov.br/api/v3/agregados/1419/metadados",
      "https://servicodados.ibge.gov.br/api/v3/agregados/1419/periodos"
    )
  )
  expect_identical(names(result), c("table", "period", "variable", "classific_category", "geo"))
  expect_identical(result$table, "Tabela 1419: IPCA - Monthly change")
  expect_identical(result$period, "201201, 201203")
  expect_identical(names(result$variable), c("cod", "desc"))
  expect_identical(result$variable$cod, c("63", "2265"))
  expect_identical(result$variable$desc, c("Monthly change (%)", "Twelve-month change (%)"))
  expect_identical(names(result$classific_category), "c315 = Products (2)")
  expect_identical(result$classific_category[[1L]]$cod, c("7169", "7170"))
  expect_identical(names(result$geo), c("cod", "desc"))
  expect_identical(result$geo$cod, c("Brazil", "City", "MetroRegion"))
  expect_true(all(grepl("quantidade de unidades indispon", result$geo$desc)))
  provenance <- attr(result, "sidrar_metadata")
  expect_identical(provenance$source, "aggregate-v3")
  expect_identical(unname(provenance$urls), seen[-1L])
  expect_identical(provenance$unavailable, c(
    "descriptor_geographic_names", "active_geographic_unit_counts",
    "variable_period_availability_exceptions"
  ))
  expect_identical(provenance$variables_without_units, character())
  expect_identical(suppressMessages(info_sidra(1419)), result)
  expect_length(seen, 6L)
})

test_that("wb bypasses requests and fallback even when enabled", {
  old <- options(sidrar.fallback = TRUE)
  on.exit(options(old), add = TRUE)
  opened <- NULL
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(...) stop("must not request metadata"),
    .sidra_request = function(...) stop("must not request values"),
    .open_sidra_descriptor = function(url) opened <<- url,
    .package = "sidrar"
  )
  expect_invisible(result <- info_sidra(1419, wb = TRUE))
  expect_identical(result, opened)
  expect_identical(result, "https://apisidra.ibge.gov.br/desctabapi.aspx?c=1419")
})

test_that("info_sidra never translates ordinary descriptor failures", {
  old <- options(sidrar.fallback = TRUE)
  on.exit(options(old), add = TRUE)
  failure <- simpleError("ordinary failure")
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(...) stop(failure),
    .sidra_request = function(...) stop("must not request an alternative"),
    .package = "sidrar"
  )
  for (type in c("sidrar_http_error", "sidrar_connection_error", "sidrar_parse_error")) {
    class(failure) <- c(type, "error", "condition")
    expect_identical(tryCatch(info_sidra(1419), error = identity), failure)
  }
})

test_that("fallback controls preserve the primary challenge unmodified", {
  old <- options(sidrar.fallback = TRUE)
  on.exit(options(old), add = TRUE)
  primary <- info_fallback_challenge()
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(...) stop(primary),
    .sidra_request = function(...) stop("must not request an alternative"),
    .package = "sidrar"
  )
  for (setting in list(FALSE, NA, "TRUE", 1, c(TRUE, FALSE))) {
    options(sidrar.fallback = setting)
    expect_identical(tryCatch(info_sidra(1419), error = identity), primary)
  }
})

test_that("missing units and empty dimensions are transparent", {
  payload <- info_fallback_metadata()
  payload$variaveis[[1L]]$unidade <- NULL
  payload$variaveis[[2L]]$unidade <- ""
  payload$classificacoes <- list()
  payload$nivelTerritorial <- list()
  urls <- c(metadata = "https://example.test/metadados", periods = "https://example.test/periodos")
  result <- sidrar:::.info_metadata_to_legacy(payload, list(), "1419", urls)
  expect_identical(result$period, "")
  expect_identical(result$variable$desc, c("Monthly change", "Twelve-month change"))
  expect_null(result$classific_category)
  expect_identical(result$geo, data.frame(cod = character(), desc = character()))
  expect_identical(attr(result, "sidrar_metadata")$variables_without_units, c("63", "2265"))
  expect_true("variable_units" %in% attr(result, "sidrar_metadata")$unavailable)

  payload$variaveis <- list()
  payload$classificacoes <- list(list(id = 1, nome = "Empty", categorias = list()))
  result <- sidrar:::.info_metadata_to_legacy(payload, list(), "1419", urls)
  expect_identical(result$variable, data.frame(cod = character(), desc = character()))
  expect_identical(result$classific_category[[1L]], result$variable)
  expect_identical(names(result$classific_category), "c1 = Empty (0)")
})

test_that("text identifiers preserve leading zeroes in legacy metadata", {
  payload <- info_fallback_metadata()
  payload$id <- "01419"
  payload$variaveis[[1L]]$id <- "00063"
  payload$classificacoes[[1L]]$id <- "00315"
  payload$classificacoes[[1L]]$categorias[[1L]]$id <- "007169"
  payload$nivelTerritorial <- list(Admin = list("N01", "N06"), Other = "N999")
  periods <- info_fallback_periods()
  periods[[1L]]$id <- "0201201"
  result <- sidrar:::.info_metadata_to_legacy(
    payload, periods, "1419", c(metadata = "meta", periods = "periods")
  )
  expect_type(result$variable$cod, "character")
  expect_identical(result$variable$cod, c("00063", "2265"))
  expect_identical(names(result$classific_category), "c00315 = Products (2)")
  expect_identical(result$classific_category[[1L]]$cod, c("007169", "7170"))
  expect_identical(result$period, "0201201, 201203")
  expect_identical(result$geo$cod, c("Brazil", "City", "n999"))
  expect_match(result$geo$desc[[3L]], "N999", fixed = TRUE)
})

test_that("unsafe numeric identifiers are rejected without rounding", {
  for (id in list(2^53, 2^53 + 2, 1e20, Inf, -Inf, NaN, -1, 1.5, TRUE)) {
    expect_false(sidrar:::.info_valid_id(id))
    payload <- info_fallback_metadata()
    payload$variaveis[[1L]]$id <- id
    expect_error(
      sidrar:::.info_metadata_to_legacy(
        payload, info_fallback_periods(), "1419",
        c(metadata = "meta", periods = "periods")
      ),
      class = "sidrar_parse_error"
    )
  }
  records <- list(
    list(id = 2^53 - 1),
    list(id = "009007199254740993"),
    list(id = 1e12)
  )
  expect_identical(
    sidrar:::.info_metadata_records(records, "test", "url", names_required = FALSE),
    c("9007199254740991", "009007199254740993", "1000000000000")
  )
})

test_that("invalid alternative schemas fail with the primary error preserved", {
  old <- options(sidrar.fallback = TRUE)
  on.exit(options(old), add = TRUE)
  primary <- info_fallback_challenge()
  payload <- info_fallback_metadata()
  periods <- info_fallback_periods()
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(...) stop(primary),
    .sidra_request = function(url) {
      info_fallback_json(if (grepl("/metadados$", url)) payload else periods)
    },
    .package = "sidrar"
  )
  mutations <- list(
    function(x) { x$id <- 7060; x },
    function(x) { x$id <- TRUE; x },
    function(x) { x$id <- NULL; x },
    function(x) { x$nome <- c("one", "two"); x },
    function(x) { x$variaveis <- NULL; x },
    function(x) { x$variaveis <- "bad"; x },
    function(x) { x$variaveis[[1L]]$nome <- NULL; x },
    function(x) { x$variaveis[[1L]]$unidade <- list("bad"); x },
    function(x) { x$variaveis[[2L]]$id <- "063"; x },
    function(x) { x$classificacoes <- NULL; x },
    function(x) { x$classificacoes <- list(x$classificacoes[[1L]], x$classificacoes[[1L]]); x },
    function(x) { x$classificacoes[[1L]]$categorias <- NULL; x },
    function(x) { x$classificacoes[[1L]]$categorias[[2L]]$id <- "07169"; x },
    function(x) { x$classificacoes[[1L]]$categorias[[1L]]$nome <- ""; x },
    function(x) { x$nivelTerritorial <- "N1"; x },
    function(x) { x$nivelTerritorial <- list(Admin = "invalid"); x },
    function(x) { x$nivelTerritorial <- list(Admin = list(list("N1"))); x },
    function(x) { x$nivelTerritorial <- list(Admin = list(id = "N1")); x },
    function(x) list(x)
  )
  for (index in seq_along(mutations)) {
    payload <- mutations[[index]](info_fallback_metadata())
    error <- tryCatch(suppressMessages(info_sidra(1419)), error = identity)
    expect_true(inherits(error, "sidrar_parse_error"), info = paste("mutation", index))
    expect_identical(error$primary_error, primary)
    if (inherits(error, "error")) {
      expect_match(conditionMessage(error), "after a SIDRA Cloudflare challenge", fixed = TRUE)
    }
  }
})

test_that("duplicated metadata fields are rejected before interpretation", {
  payload <- info_fallback_metadata()
  urls <- c(metadata = "meta", periods = "periods")
  expect_error(
    sidrar:::.info_metadata_to_legacy(
      c(payload, list(id = 1419)), info_fallback_periods(), "1419", urls
    ),
    class = "sidrar_parse_error"
  )
  payload$variaveis[[1L]] <- c(payload$variaveis[[1L]], list(nome = "duplicate"))
  expect_error(
    sidrar:::.info_metadata_to_legacy(payload, info_fallback_periods(), "1419", urls),
    class = "sidrar_parse_error"
  )
})

test_that("invalid period records do not imply a usable period catalog", {
  urls <- c(metadata = "meta", periods = "periods")
  for (periods in list(
    NULL, "2020", list(id = 2020), list(list(id = NA)),
    list(list(id = list(2020))), list(list(id = 2020), list(id = "02020"))
  )) {
    expect_error(
      sidrar:::.info_metadata_to_legacy(info_fallback_metadata(), periods, "1419", urls),
      class = "sidrar_parse_error"
    )
  }
})

test_that("alternative transport and JSON failures retain both errors", {
  old <- options(sidrar.fallback = TRUE)
  on.exit(options(old), add = TRUE)
  primary <- info_fallback_challenge()
  failure <- structure(
    list(message = "service unavailable", call = NULL, status_code = 503L),
    class = c("sidrar_http_error", "error", "condition")
  )
  request <- function(url) stop(failure)
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(...) stop(primary),
    .sidra_request = function(url) request(url),
    .package = "sidrar"
  )
  error <- tryCatch(suppressMessages(info_sidra(1419)), error = identity)
  expect_s3_class(error, "sidrar_http_error")
  expect_identical(error$status_code, 503L)
  expect_identical(error$primary_error, primary)

  request <- function(url) {
    if (grepl("/metadados$", url)) return(info_fallback_json(info_fallback_metadata()))
    stop(failure)
  }
  error <- tryCatch(suppressMessages(info_sidra(1419)), error = identity)
  expect_identical(error$primary_error, primary)
  expect_identical(error$status_code, 503L)

  request <- function(url) "not JSON"
  error <- tryCatch(suppressMessages(info_sidra(1419)), error = identity)
  expect_s3_class(error, "sidrar_parse_error")
  expect_identical(error$primary_error, primary)
})

test_that("invalid or mismatched primary descriptor identity is not translated", {
  payload <- descriptor_fixture()
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(...) payload,
    .sidra_request = function(...) stop("must not use an alternative"),
    .package = "sidrar"
  )
  payload$Id <- 7060
  expect_error(info_sidra(1419), class = "sidrar_parse_error")
  payload$Id <- list(1419)
  expect_error(info_sidra(1419), class = "sidrar_parse_error")
  payload <- descriptor_fixture()
  payload$Nome <- ""
  expect_error(info_sidra(1419), class = "sidrar_parse_error")
})
