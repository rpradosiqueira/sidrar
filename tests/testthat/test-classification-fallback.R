classification_challenge <- function() {
  structure(
    list(
      message = "SIDRA browser challenge", call = NULL,
      status_code = 403L, url = "https://apisidra.ibge.gov.br/DescritoresTabela/t/1419"
    ),
    class = c("sidrar_challenge_error", "sidrar_http_error", "error", "condition")
  )
}

test_that("classification discovery keeps the working descriptor path", {
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) descriptor_fixture(),
    .sidra_request = function(...) stop("alternative must not be requested"),
    .package = "sidrar"
  )

  expect_identical(
    sidrar:::.resolve_classification_path("1419", "all", "all"),
    "/c315/all"
  )
})

test_that("classification discovery uses v3 only after a descriptor challenge", {
  primary <- classification_challenge()
  seen <- character()
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) stop(primary),
    .sidra_request = function(url) {
      seen <<- c(seen, url)
      '{"id":1419,"classificacoes":[{"id":20},{"id":"3"}]}'
    },
    .package = "sidrar"
  )

  expect_message(
    query <- sidra_query(1419),
    "official aggregate metadata"
  )
  expect_match(query$url, "/c20/all/c3/all/", fixed = TRUE)
  expect_identical(query$parameters$classific, "all")
  expect_identical(
    seen,
    "https://servicodados.ibge.gov.br/api/v3/agregados/1419/metadados"
  )
  path <- suppressMessages(
    sidrar:::.resolve_classification_path("1419", NULL, "all")
  )
  expect_identical(path, "/c20/all/c3/all")
})

test_that("classification fallback preserves defaults and explicit categories", {
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) stop(classification_challenge()),
    .sidra_request = function(url) {
      '{"id":1419,"classificacoes":[{"id":20},{"id":3}]}'
    },
    .package = "sidrar"
  )

  expect_message(
    path <- sidrar:::.resolve_classification_path(
      "1419", "all", list(123)
    ),
    "Considering all categories"
  )
  expect_identical(path, "/c20/all/c3/all")

  testthat::local_mocked_bindings(
    .fetch_descriptor = function(...) stop("descriptor must not be requested"),
    .sidra_request = function(...) stop("alternative must not be requested"),
    .package = "sidrar"
  )
  expect_identical(
    sidrar:::.resolve_classification_path(
      "1419", c("c20", "c3"), list(c("001", "002"))
    ),
    "/c20/001,002/c3/all"
  )
})

test_that("tables without classifications omit the classification path", {
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) stop(classification_challenge()),
    .sidra_request = function(url) '{"id":1419,"classificacoes":[]}',
    .package = "sidrar"
  )

  query <- suppressMessages(sidra_query(1419))
  expect_match(query$url, "/v/allxp/f/a/", fixed = TRUE)
  expect_identical(
    suppressMessages(sidrar:::.query_classifications("1419")),
    character()
  )
})

test_that("classification fallback respects disabling and unrelated errors", {
  primary <- classification_challenge()
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) stop(primary),
    .sidra_request = function(...) stop("alternative must not be requested"),
    .package = "sidrar"
  )
  old <- options(sidrar.fallback = FALSE)
  on.exit(options(old), add = TRUE)
  expect_identical(
    tryCatch(sidra_query(1419), error = identity), primary
  )

  options(sidrar.fallback = TRUE)
  classes <- c("sidrar_http_error", "sidrar_parse_error", "sidrar_timeout_error")
  for (error_class in classes) {
    other <- structure(
      list(message = "No challenge", call = NULL, status_code = 403L),
      class = c(error_class, "error", "condition")
    )
    testthat::local_mocked_bindings(
      .fetch_descriptor = function(table) stop(other),
      .package = "sidrar"
    )
    expect_identical(
      tryCatch(sidra_query(1419), error = identity), other
    )
  }
})

test_that("classification discovery preserves text IDs and canonicalizes tables", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) stop(classification_challenge()),
    .sidra_request = function(url) {
      seen <<- url
      '{"id":"01419","classificacoes":[{"id":"020"}]}'
    },
    .package = "sidrar"
  )

  result <- suppressMessages(sidrar:::.query_classifications("01419"))
  expect_identical(result, "c020")
  expect_match(seen, "/1419/metadados$", fixed = FALSE)
})

test_that("classification fallback errors retain the original challenge", {
  primary <- classification_challenge()
  calls <- 0L
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) stop(primary),
    .sidra_request = function(url) {
      calls <<- calls + 1L
      sidrar:::.sidrar_abort(
        "Alternative challenge", "sidrar_challenge_error", status_code = 403L
      )
    },
    .package = "sidrar"
  )

  error <- suppressMessages(tryCatch(sidra_query(1419), error = identity))
  expect_s3_class(error, "sidrar_challenge_error")
  expect_identical(error$primary_error, primary)
  expect_match(conditionMessage(error), "alternative classification metadata failed")
  expect_identical(calls, 1L)
})

test_that("classification fallback rejects incomplete or inconsistent metadata", {
  primary <- classification_challenge()
  payloads <- c(
    "not json", "[]", "{}", '{"id":999,"classificacoes":[]}',
    '{"id":1419}', '{"id":1419,"classificacoes":null}',
    '{"id":1419,"classificacoes":[1,{"id":20}]}',
    '{"id":1419,"classificacoes":[{}]}',
    '{"id":1419,"classificacoes":[{"id":"c20"}]}',
    '{"id":1419,"classificacoes":[{"id":20},{"id":"020"}]}',
    '{"id":[1419,999],"classificacoes":[]}',
    '{"identifier":1419,"classificacoes":[{"id":20}]}',
    '{"id":1419,"classificacoes":[{"identifier":20}]}'
  )
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) stop(primary),
    .sidra_request = function(url) payload,
    .package = "sidrar"
  )

  for (payload in payloads) {
    error <- suppressMessages(tryCatch(sidra_query(1419), error = identity))
    expect_s3_class(error, "sidrar_parse_error")
    expect_identical(error$primary_error, primary)
  }
})

test_that("classification fallback does not persist metadata across calls", {
  descriptors <- 0L
  alternatives <- 0L
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) {
      descriptors <<- descriptors + 1L
      stop(classification_challenge())
    },
    .sidra_request = function(url) {
      alternatives <<- alternatives + 1L
      paste0('{"id":1419,"classificacoes":[{"id":', alternatives, '}]}')
    },
    .package = "sidrar"
  )

  first <- suppressMessages(sidra_query(1419))
  second <- suppressMessages(sidra_query(1419))
  expect_match(first$url, "/c1/all/", fixed = TRUE)
  expect_match(second$url, "/c2/all/", fixed = TRUE)
  expect_identical(descriptors, 2L)
  expect_identical(alternatives, 2L)
})

test_that("get_sidra can construct defaults after a descriptor challenge", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) stop(classification_challenge()),
    .sidra_request = function(url) '{"id":1419,"classificacoes":[{"id":315}]}',
    .sidra_values_request = function(url) {
      seen <<- url
      list(text = values_with_header_json(), url = url, response_header = TRUE)
    },
    .package = "sidrar"
  )

  values <- suppressMessages(get_sidra(1419))
  expect_match(seen, "/c315/all/", fixed = TRUE)
  expect_identical(values$Valor, 0.16)
})

test_that("info_sidra keeps its legacy descriptor contract", {
  primary <- classification_challenge()
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) stop(primary),
    .sidra_request = function(...) stop("alternative must not be requested"),
    .package = "sidrar"
  )

  expect_identical(tryCatch(info_sidra(1419), error = identity), primary)
})
