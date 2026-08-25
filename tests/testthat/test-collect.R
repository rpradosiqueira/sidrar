test_that("explicit periods are split into ordered disjoint queries", {
  query <- sidra_query(
    1612,
    variable = 214,
    period = as.character(2018:2022),
    geo = "Brazil",
    classific = "c81",
    category = list(2702)
  )
  batches <- sidra_split(query, by = "period", size = 2)

  expect_s3_class(batches, "sidra_batch")
  expect_length(batches$queries, 3L)
  expect_match(batches$queries[[1L]]$url, "/p/2018,2019/", fixed = TRUE)
  expect_match(batches$queries[[2L]]$url, "/p/2020,2021/", fixed = TRUE)
  expect_match(batches$queries[[3L]]$url, "/p/2022/", fixed = TRUE)
})

test_that("category sums remain indivisible members when split", {
  query <- sidra_query(
    1612,
    variable = 214,
    period = "2021",
    geo = "Brazil",
    classific = "c81",
    category = list(c("2692", "2694 2695", "2702"))
  )
  batches <- sidra_split(query, by = "category", size = 2)

  expect_length(batches$queries, 2L)
  expect_match(
    batches$queries[[1L]]$url,
    "/c81/2692,2694%202695/",
    fixed = TRUE
  )
  expect_match(batches$queries[[2L]]$url, "/c81/2702/", fixed = TRUE)
})

test_that("split rejects special selectors, duplicates, and api paths", {
  special <- sidra_query(
    1612,
    period = "all",
    geo = "Brazil",
    classific = "c81",
    category = list(2702)
  )
  expect_error(sidra_split(special, "period", 2), "explicit values")

  relative <- sidra_query(
    1612,
    period = c(last = 12),
    geo = "Brazil",
    classific = "c81",
    category = list(2702)
  )
  expect_error(sidra_split(relative, "period", 2), "explicit values")

  ranged <- sidra_query(
    1612,
    period = "2018-2022",
    geo = "Brazil",
    classific = "c81",
    category = list(2702)
  )
  expect_error(sidra_split(ranged, "period", 2), "explicit values")

  duplicated <- sidra_query(
    1612,
    variable = c(214, 214),
    period = "2021",
    geo = "Brazil",
    classific = "c81",
    category = list(2702)
  )
  expect_error(sidra_split(duplicated, "variable", 1), "duplicates")

  path <- sidra_query(api = "/t/1612/n1/1/v/214/p/2021/h/n")
  expect_error(sidra_split(path, "period", 1), "cannot be split safely")
  expect_error(sidra_split(duplicated, "variable", 3e9), "positive integer")
  expect_error(
    sidra_split(duplicated, "variable", 1, index = 3e9),
    "positive integer"
  )
})

test_that("one effective geo filter and classification indexes can be selected", {
  geo_query <- sidra_query(
    1612,
    variable = 214,
    period = "2021",
    geo = "City",
    geo.filter = list(State = c(50, 51, 52)),
    classific = "c81",
    category = list(2702)
  )
  geo_batches <- sidra_split(
    geo_query, by = "geo.filter", size = 2
  )
  expect_length(geo_batches$queries, 2L)
  expect_match(
    geo_batches$queries[[1L]]$url,
    "n6/in%20n3%2050,51",
    fixed = TRUE
  )

  category_query <- sidra_query(
    822,
    variable = 183,
    period = "2006",
    geo = "Brazil",
    classific = c("c220", "c226"),
    category = list(1, c(4857, 4858, 4859))
  )
  category_batches <- sidra_split(
    category_query, by = "category", size = 2, index = 2
  )
  expect_length(category_batches$queries, 2L)
  expect_match(
    category_batches$queries[[1L]]$url,
    "/c220/1/c226/4857,4858/",
    fixed = TRUE
  )
})

test_that("geographic splitting rejects overlapping or ignored filters", {
  overlapping <- sidra_query(
    1612,
    variable = 214,
    period = "2021",
    geo = c("State", "City"),
    geo.filter = list(Region = 3, State = c(50, 51)),
    classific = "c81",
    category = list(2702)
  )
  expect_error(
    sidra_split(overlapping, "geo.filter", 1, index = 2),
    "overlapping batches"
  )

  ignored <- suppressMessages(sidra_query(
    1612,
    variable = 214,
    period = "2021",
    geo = "Brazil",
    geo.filter = list(c(1, 2)),
    classific = "c81",
    category = list(2702)
  ))
  expect_error(sidra_split(ignored, "geo.filter", 1), "non-Brazil")

  ignored_category <- suppressMessages(sidra_query(
    1612,
    variable = 214,
    period = "2021",
    geo = "Brazil",
    classific = "all",
    category = list(c(2702, 2703))
  ))
  expect_error(
    sidra_split(ignored_category, "category", 1),
    "explicit values"
  )
})

test_that("splitting reuses classifications resolved in the original URL", {
  descriptor_calls <- 0L
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(table) {
      descriptor_calls <<- descriptor_calls + 1L
      list(Classificacoes = list(list(Id = if (descriptor_calls == 1L) 81 else 99)))
    },
    .package = "sidrar"
  )

  query <- sidra_query(
    1612,
    variable = 214,
    period = c("2020", "2021"),
    geo = "Brazil"
  )
  batches <- sidra_split(query, "period", 1)

  expect_identical(descriptor_calls, 1L)
  expect_true(all(vapply(
    batches$queries,
    function(x) grepl("/c81/all", x$url, fixed = TRUE),
    logical(1)
  )))
})

test_that("collect combines batches sequentially and attaches provenance", {
  queries <- list(
    sidra_query(api = "/t/1/n1/1/v/1/p/2020/h/y"),
    sidra_query(api = "/t/1/n1/1/v/1/p/2021/h/y")
  )
  calls <- character()
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      calls <<- c(calls, url)
      value <- if (grepl("2020", url, fixed = TRUE)) "1" else "2"
      paste0('[{"D1C":"Ano (Código)","V":"Valor"},',
             '{"D1C":"', if (value == "1") "2020" else "2021",
             '","V":"', value, '"}]')
    },
    .package = "sidrar"
  )

  result <- sidra_collect(queries, provenance = TRUE)

  expect_identical(result$Valor, c(1, 2))
  expect_identical(result[["Ano (Código)"]], c("2020", "2021"))
  expect_identical(calls, vapply(queries, `[[`, character(1), "url"))
  provenance <- sidra_provenance(result)
  expect_identical(provenance$batch_count, 2L)
  expect_identical(provenance$urls, calls)
})

test_that("collect rejects schema drift immediately with diagnostics", {
  queries <- list(
    sidra_query(api = "/t/1/n1/1/v/1/p/2020/h/y"),
    sidra_query(api = "/t/1/n1/1/v/1/p/2021/h/y"),
    sidra_query(api = "/t/1/n1/1/v/1/p/2022/h/y")
  )
  call <- 0L
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      call <<- call + 1L
      header <- if (call == 1L) "Ano" else "Período"
      paste0('[{"D1C":"', header, '","V":"Valor"},',
             '{"D1C":"2020","V":"1"}]')
    },
    .package = "sidrar"
  )

  error <- expect_error(
    sidra_collect(queries),
    "schema different",
    class = "sidrar_batch_schema_error"
  )
  expect_identical(error$batch_index, 2L)
  expect_identical(error$batch_count, 3L)
  expect_identical(error$batch_url, queries[[2L]]$url)
  expect_identical(
    unname(error$expected_classes),
    unname(error$received_classes)
  )
  expect_identical(call, 2L)
})

test_that("collect preserves the original error class and batch context", {
  query <- sidra_query(api = "/t/1/n1/1/v/1/p/2020/h/n")
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      sidrar:::.sidrar_abort(
        "upstream timeout",
        c("sidrar_timeout_error", "sidrar_http_error"),
        url = url
      )
    },
    .package = "sidrar"
  )

  error <- expect_error(
    sidra_collect(query),
    "batch 1 of 1",
    class = "sidrar_timeout_error"
  )
  expect_identical(error$batch_index, 1L)
  expect_identical(error$batch_count, 1L)
})
