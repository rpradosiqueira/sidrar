split_url_period_fixture <- function(codes = c("202001", "202003", "202004"),
                                     table = "1") {
  data.frame(
    table_id = rep(table, length(codes)), period_id = codes,
    period_name = if (length(codes)) paste("Period", codes) else character(),
    stringsAsFactors = FALSE
  )
}

split_url_selection <- function(batch, parameter) {
  unlist(lapply(batch$queries, function(query) {
    pairs <- sidrar:::.sidra_path_pairs(query$url)
    strsplit(pairs$selection[pairs$parameter == parameter], ",", fixed = TRUE)[[1L]]
  }), use.names = FALSE)
}

test_that("explicit URL selections split offline without rewriting other tokens", {
  testthat::local_mocked_bindings(
    .sidra_request = function(...) stop("unexpected network"),
    .package = "sidrar"
  )
  url <- paste0(
    "https://apisidra.ibge.gov.br/values/t/1/v/0001,0002/p/202001,202003,202004",
    "/n1/all/n3/all/c81/0007/c2/0008/h/n/f/a/d/v0001%202?formato=json"
  )
  query <- sidra_query(api = url, value_type = "both")
  batch <- sidrar:::.sidra_split_url(query, "period", size = 2)
  expect_s3_class(batch, "sidra_batch")
  expect_length(batch$queries, 2L)
  expect_identical(batch$queries[[1L]]$url,
                   sub("202001,202003,202004", "202001,202003", url, fixed = TRUE))
  expect_identical(batch$queries[[2L]]$url,
                   sub("202001,202003,202004", "202004", url, fixed = TRUE))
  expect_false(batch$queries[[1L]]$header)
  expect_identical(batch$queries[[1L]]$parameters$value_type, "both")
  expect_identical(batch$source, query)
  expect_null(batch$resolution$periods)
  expect_null(batch$resolution$resolved_at)
  variables <- sidrar:::.sidra_split_url(query, "variable", size = 1)
  expect_identical(split_url_selection(variables, "v"), c("0001", "0002"))
  expect_identical(variables$queries[[1L]]$url,
                   sub("v/0001,0002", "v/0001", url, fixed = TRUE))
})

test_that("URL categories use path order and geographic identifiers remain text", {
  testthat::local_mocked_bindings(
    .sidra_request = function(...) stop("unexpected network"),
    .package = "sidrar"
  )
  query <- sidra_query(api = paste0(
    "/t/1/p/202001/v/1/n6/0001,0010,0012/c9/1/c2/001,004,005/d/s"
  ))
  categories <- sidrar:::.sidra_split_url(query, "category", size = 2, index = 2)
  expect_identical(split_url_selection(categories, "c2"), c("001", "004", "005"))
  expect_identical(categories$resolution$parameter, "c2")
  expect_match(categories$queries[[1L]]$url, "/c9/1/c2/001,004/", fixed = TRUE)
  locations <- sidrar:::.sidra_split_url(query, "geo.filter", size = 2)
  expect_identical(split_url_selection(locations, "n6"), c("0001", "0010", "0012"))
  expect_match(locations$queries[[1L]]$url, "/n6/0001,0010/", fixed = TRUE)
  expect_match(locations$queries[[2L]]$url, "/n6/0012/", fixed = TRUE)
})

test_that("all periods freeze one fresh official inventory without filling calendar gaps", {
  calls <- list()
  snapshot <- split_url_period_fixture(c("202004", "202001", "202003"))
  testthat::local_mocked_bindings(
    sidra_periods = function(table, refresh, cache) {
      calls[[length(calls) + 1L]] <<- list(table, refresh, cache)
      snapshot
    },
    .sidra_request = function(...) stop("values must not be downloaded"),
    .package = "sidrar"
  )
  query <- sidra_query(api = "/t/1/n1/1/v/1/p/all/h/n")
  batch <- sidrar:::.sidra_split_url(query, "period", 2)
  expect_length(calls, 1L)
  expect_identical(calls[[1L]], list("1", TRUE, FALSE))
  expect_identical(batch$resolution$source_selection, "all")
  expect_identical(batch$resolution$periods, snapshot)
  expect_s3_class(batch$resolution$resolved_at, "POSIXct")
  expect_identical(split_url_selection(batch, "p"), c("202001", "202003", "202004"))
  expect_length(batch$queries, 2L)
  expect_false(any(grepl("202002", vapply(batch$queries, `[[`, "", "url"))))
})

test_that("relative period selectors use inventory codes and not guessed dates", {
  testthat::local_mocked_bindings(
    sidra_periods = function(...) split_url_period_fixture(c("0009", "0002", "0004")),
    .package = "sidrar"
  )
  cases <- list(
    first = "0002", last = "0009", `first%202` = c("0002", "0004"),
    `last%202` = c("0004", "0009"), `first%2099` = c("0002", "0004", "0009"),
    `last%2099` = c("0002", "0004", "0009")
  )
  for (selector in names(cases)) {
    query <- sidra_query(api = paste0("/t/1/n1/1/v/1/p/", selector))
    batch <- sidrar:::.sidra_split_url(query, "period", 1)
    expect_identical(split_url_selection(batch, "p"), cases[[selector]],
                     info = selector)
  }
  query <- sidra_query(1, period = c(first = 2), variable = 1, classific = "c1")
  batch <- sidrar:::.sidra_split_url(query, "period", 1)
  expect_identical(split_url_selection(batch, "p"), c("0002", "0004"))
})

test_that("period ranges expand only observed codes and preserve token ordering", {
  testthat::local_mocked_bindings(
    sidra_periods = function(...) split_url_period_fixture(as.character(c(2018, 2020, 2021, 2024))),
    .package = "sidrar"
  )
  query <- sidra_query(api = "/t/1/n1/1/v/1/p/2024,2018-2021/h/n")
  batch <- sidrar:::.sidra_split_url(query, "period", 2)
  expect_identical(split_url_selection(batch, "p"), c("2024", "2018", "2020", "2021"))
  expect_match(batch$queries[[1L]]$url, "/p/2024,2018/", fixed = TRUE)
  for (selection in c("2017-2021", "2018-2022", "2018-2021,2019")) {
    query <- sidra_query(api = paste0("/t/1/n1/1/v/1/p/", selection))
    expect_error(sidrar:::.sidra_split_url(query, "period", 1),
                 "must exist in the period inventory")
  }
  query <- sidra_query(api = "/t/1/n1/1/v/1/p/2021-2018")
  expect_error(sidrar:::.sidra_split_url(query, "period", 1), "increasing endpoints")
  for (selection in c("2018-2021,2020", "2018-2021,2020-2024", "2024,2024")) {
    query <- sidra_query(api = paste0("/t/1/n1/1/v/1/p/", selection))
    expect_error(sidrar:::.sidra_split_url(query, "period", 1), "duplicates")
  }
})

test_that("malformed period metadata cannot silently narrow a batch plan", {
  invalid <- list(
    NULL, list(), data.frame(), split_url_period_fixture(character()),
    split_url_period_fixture(c("2020", NA_character_)),
    split_url_period_fixture(c("2020", "")),
    split_url_period_fixture(c("2020", "not-a-code")),
    split_url_period_fixture(c("2020", "2020")),
    split_url_period_fixture(c("02020", "2020")),
    split_url_period_fixture(table = "2"),
    data.frame(table_id = 1, period_id = 2020),
    data.frame(table_id = NA_character_, period_id = "2020"),
    data.frame(table_id = c("1", "2"), period_id = c("2020", "2021")),
    data.frame(table_id = "1", period_id = "2020", period_id = "2021", check.names = FALSE)
  )
  returned <- NULL
  testthat::local_mocked_bindings(
    sidra_periods = function(...) returned,
    .package = "sidrar"
  )
  query <- sidra_query(api = "/t/1/n1/1/v/1/p/all")
  for (value in invalid) {
    returned <- value
    expect_error(sidrar:::.sidra_split_url(query, "period", 2),
                 class = "sidrar_split_metadata_error")
  }
  returned <- split_url_period_fixture(c("2020", "202001"))
  expect_length(sidrar:::.sidra_split_url(query, "period", 1)$queries, 2L)
  for (selector in c("first", "last%202", "2020-202001")) {
    query <- sidra_query(api = paste0("/t/1/n1/1/v/1/p/", selector))
    expect_error(sidrar:::.sidra_split_url(query, "period", 1), "mixed-width")
  }
})

test_that("metadata transport errors retain their original class", {
  testthat::local_mocked_bindings(
    sidra_periods = function(...) sidrar:::.sidrar_abort(
      "inventory unavailable", "sidrar_http_error", status_code = 503L
    ),
    .package = "sidrar"
  )
  query <- sidra_query(api = "/t/1/n1/1/v/1/p/all")
  error <- expect_error(sidrar:::.sidra_split_url(query, "period", 1),
                        class = "sidrar_http_error")
  expect_identical(error$status_code, 503L)
})

test_that("unsafe geographic URL partitions and implicit selections are rejected", {
  testthat::local_mocked_bindings(
    .sidra_request = function(...) stop("unexpected network"),
    .package = "sidrar"
  )
  for (geography in c("n1/all", "n1/1", "n2/1,2/n3/11,12", "g/1")) {
    query <- sidra_query(api = paste0("/t/1/", geography, "/v/1/p/2020"))
    expect_error(sidrar:::.sidra_split_url(query, "geo.filter", 1),
                 "non-Brazil|overlapping")
  }
  for (geography in c("n3/all", "n6/in%20n3%2050,51", "n3/11,011")) {
    query <- sidra_query(api = paste0("/t/1/", geography, "/v/1/p/2020"))
    expect_error(sidrar:::.sidra_split_url(query, "geo.filter", 1),
                 "explicit numeric codes|duplicates")
  }
  for (by in c("variable", "category")) {
    key <- if (by == "variable") "v" else "c1"
    for (selection in c("all", "allxp", "allxt", "1%202", "1-2", "1,1", "1,01")) {
      query <- sidra_query(api = paste0("/t/1/n1/1/p/2020/", key, "/", selection))
      expect_error(sidrar:::.sidra_split_url(query, by, 1),
                   "explicit numeric codes|duplicates")
    }
  }
})

test_that("unsupported URL parameters are rejected instead of dropped", {
  testthat::local_mocked_bindings(
    .sidra_request = function(...) stop("unexpected network"),
    .package = "sidrar"
  )
  paths <- c(
    "/t/1/n1/1/v/1/p/2020/p/2021",
    "/t/1/n1/1/v/1/p/2020/w/unknown",
    "/t/1/n1/1/v/1/p/2020/n01/1",
    "/t/1/n1/1/v/1/p/2020/c1/2/c01/3",
    "/t/1/n1/1/v/1/p/2020?formato=json&extra=1",
    "/t/1/n1/1/v/1/p/2020/",
    "/t/1/n1/1/v/1/p//2020"
  )
  for (path in paths) {
    expect_error(sidrar:::.sidra_split_url(sidra_query(api = path), "period", 1),
                 "split safely|incomplete parameter")
  }
  for (authority in c("user@apisidra.ibge.gov.br", "apisidra.ibge.gov.br:443")) {
    query <- sidra_query(api = paste0("https://", authority, "/values/t/1/n1/1/p/2020"))
    expect_error(sidrar:::.sidra_split_url(query, "period", 1), "split safely")
  }
})

test_that("missing URL selections and invalid split arguments fail before metadata", {
  testthat::local_mocked_bindings(
    sidra_periods = function(...) stop("unexpected metadata"),
    .package = "sidrar"
  )
  for (by in c("period", "variable", "category")) {
    query <- sidra_query(api = "/t/1/n1/1")
    expect_error(sidrar:::.sidra_split_url(query, by, 1), "no explicit URL element")
  }
  query <- sidra_query(api = "/t/1/n3/11/p/all/v/1/c1/2")
  for (by in c("period", "variable", "category", "geo.filter")) {
    expect_error(sidrar:::.sidra_split_url(query, by, 1, index = 2),
                 "no explicit URL element")
  }
  for (size in list(0, -1, NA_real_, Inf, 1.5, 3e9, "1")) {
    expect_error(sidrar:::.sidra_split_url(query, "period", size), "positive integer")
  }
  expect_error(sidrar:::.sidra_split_url(query, "period", 1, 0), "positive integer")
  for (selection in c("last%200", "first%20-1", "last%201.5", "all,2020", "2020,", ",2020")) {
    query <- sidra_query(api = paste0("/t/1/n1/1/v/1/p/", selection))
    expect_error(sidrar:::.sidra_split_url(query, "period", 1), "must use explicit codes")
  }
})

test_that("raw period inventories never discard malformed records", {
  returned <- "[]"
  testthat::local_mocked_bindings(
    .sidra_request = function(...) returned,
    .package = "sidrar"
  )
  invalid <- c(
    'null', '{}', '"2020"', '42',
    '[{"id":"2020"},null]', '[{"id":"2020"},42]',
    '[{"id":"2020"},"2021"]', '[{"id":"2020"},{}]',
    '[{"id":"2020"},{"nome":"missing id"}]',
    '[{"id":"2020"},{"id":null}]',
    '[{"id":"2020"},{"id":["2021","2022"]}]',
    '[{"id":"2020"},{"id":{"nested":"2021"}}]',
    '[{"id":"2020"},{"id":true}]',
    '[{"id":"2020"},{"id":2021.5}]',
    '[{"id":"2020"},{"id":-2021}]',
    '[{"id":"2020"},{"id":9007199254740992}]',
    '[{"id":"2020"},{"id":""}]',
    '[{"id":"2020"},{"id":" 2021"}]',
    '[{"id":"2020"},{"id":"2021\\n"}]',
    '[{"id":"2020"},{"id":"2020"}]',
    '[{"id":"2020"},{"id":"02020"}]',
    '[{"id":"2020","id":"2021"}]'
  )
  for (payload in invalid) {
    returned <- payload
    error <- expect_error(sidra_periods(1), class = "sidrar_parse_error")
    expect_match(conditionMessage(error), "period inventory is invalid", fixed = TRUE)
  }
  returned <- '[{"id":"2020"},null]'
  query <- sidra_query(api = "/t/1/n1/1/v/1/p/all")
  expect_error(sidrar:::.sidra_split_url(query, "period", 1),
               "period inventory is invalid", class = "sidrar_parse_error")
})

test_that("valid period inventories retain compatible schemas and exact IDs", {
  returned <- "[]"
  testthat::local_mocked_bindings(
    .sidra_request = function(...) returned,
    .package = "sidrar"
  )
  expect_identical(nrow(sidra_periods(1)), 0L)
  returned <- '{"id":"p1","literals":["Period 1"],"future":{"anything":42}}'
  expect_identical(sidra_periods(1)$period_id, "p1")
  returned <- paste0(
    '[{"id":"0001","literals":["First","Alternate"],"future":true},',
    '{"id":9007199254740991,"nome":"Large exact integer"},',
    '{"id":"9007199254740993"}]'
  )
  result <- sidra_periods(1)
  expect_identical(result$period_id, c("0001", "9007199254740991", "9007199254740993"))
  expect_identical(result$alternative_names[[1L]], "Alternate")
  expect_identical(result$period_name[[2L]], "Large exact integer")
  expect_true(is.na(result$period_name[[3L]]))
  query <- sidra_query(api = "/t/1/n1/1/v/1/p/all")
  expect_identical(split_url_selection(sidrar:::.sidra_split_url(query, "period", 1), "p"),
                   result$period_id)
})
