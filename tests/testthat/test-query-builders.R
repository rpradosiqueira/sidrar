test_that("the existing public argument order is preserved", {
  existing <- c(
    "x", "variable", "period", "geo", "geo.filter", "classific",
    "category", "header", "format", "digits", "api"
  )

  expect_identical(names(formals(get_sidra))[seq_along(existing)], existing)
  expect_identical(tail(names(formals(get_sidra)), 1L), "value_type")
})

test_that("geographic paths preserve requested order and filter association", {
  expect_identical(
    sidrar:::.build_geo_path("City", 5002704),
    "n6/5002704"
  )
  expect_identical(
    sidrar:::.build_geo_path(
      c("State", "City"),
      list(Region = 3, State = 50)
    ),
    "n3/in%20n2%203/n6/in%20n3%2050"
  )
  expect_identical(
    sidrar:::.build_geo_path(
      c("State", "City"),
      list(Region = 3)
    ),
    "n3/in%20n2%203/n6/all"
  )
  expect_identical(
    sidrar:::.build_geo_path("City", list(c(5002704, 5003702))),
    "n6/5002704,5003702"
  )
})

test_that("geographic validation handles vectors and invalid filters", {
  expect_error(
    sidrar:::.build_geo_path(character(), NULL),
    "non-empty character"
  )
  expect_error(
    sidrar:::.build_geo_path("State", list(City = 5002704)),
    "geo.filter"
  )
  expect_error(
    sidrar:::.build_geo_path("Unknown", NULL),
    "misspecified"
  )
})

test_that("classification categories are composed without being overwritten", {
  expect_identical(
    sidrar:::.build_classification_path(
      c("c1", "c2", "c3"),
      list(10, 20)
    ),
    "/c1/10/c2/20/c3/all"
  )
  expect_identical(
    sidrar:::.build_classification_path(c(1, 2), list()),
    "/c1/all/c2/all"
  )
  expect_error(
    sidrar:::.build_classification_path("c1", c("one", "two")),
    "type 'list'"
  )
})

test_that("the default classification selection uses the JSON descriptor", {
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(x) descriptor_fixture(),
    .package = "sidrar"
  )

  query <- sidrar:::.build_sidra_query(
    x = 1419,
    variable = 63,
    period = "last",
    geo = "Brazil",
    geo_filter = NULL,
    classific = "all",
    category = "all",
    header = TRUE,
    format = 4,
    digits = "default"
  )

  expect_match(query$url, "/c315/all", fixed = TRUE)
  expect_true(query$header)
})

test_that("period, variable, header, format, and digits are deterministic", {
  expect_identical(
    sidrar:::.build_period_path(c(last = 12)),
    "last%2012"
  )
  expect_identical(
    sidrar:::.build_period_path(c("2012", "2014-2016")),
    "2012,2014-2016"
  )
  expect_identical(
    sidrar:::.build_variable_path(c(63, 69)),
    "63,69"
  )
  expect_identical(sidrar:::.build_header_path(FALSE), "n")
  expect_error(sidrar:::.build_header_path(NA), "TRUE or FALSE")
  expect_identical(sidrar:::.build_format_path(NULL), "/f/a")
  expect_identical(sidrar:::.build_digits_path(NULL), "/d/s")

  expect_warning(
    format_path <- sidrar:::.build_format_path(99),
    "default specification"
  )
  expect_identical(format_path, "/f/a")

  expect_warning(
    digits_path <- sidrar:::.build_digits_path("invalid"),
    "default specification"
  )
  expect_identical(digits_path, "/d/s")
})

test_that("relative paths and full official URLs are accepted", {
  expected <- paste0(
    "https://apisidra.ibge.gov.br/values/",
    "t/7060/n1/all/v/63/p/last/h/n"
  )

  expect_identical(
    sidrar:::.normalize_api_url(
      "/t/7060/n1/all/v/63/p/last/h/n"
    ),
    expected
  )
  expect_identical(
    sidrar:::.normalize_api_url(
      "t/7060/n1/all/v/63/p/last/h/n"
    ),
    expected
  )
  expect_identical(sidrar:::.normalize_api_url(expected), expected)
  expect_identical(
    sidrar:::.normalize_api_url(
      "/values/t/7060/n1/all/v/63/p/last%2012/h/n"
    ),
    sub("/p/last/", "/p/last%2012/", expected, fixed = TRUE)
  )
  expect_false(sidrar:::.api_has_header(expected))
  expect_true(
    sidrar:::.api_has_header(
      sub("/h/n", "", expected, fixed = TRUE)
    )
  )

  expect_error(
    sidrar:::.normalize_api_url(
      "https://example.com/values/t/7060/n1/all"
    ),
    "official HTTPS"
  )
  expect_error(
    sidrar:::.normalize_api_url(
      paste0(expected, "?formato=xml")
    ),
    "Only JSON"
  )
  expect_error(
    sidrar:::.normalize_api_url(
      sub("https://", "http://", expected, fixed = TRUE)
    ),
    "official HTTPS"
  )
})
