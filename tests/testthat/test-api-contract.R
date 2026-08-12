test_that("the public API and defaults remain backward compatible", {
  expect_identical(
    sort(getNamespaceExports("sidrar")),
    sort(c("get_sidra", "info_sidra", "search_sidra"))
  )

  expect_identical(
    names(formals(get_sidra)),
    c(
      "x", "variable", "period", "geo", "geo.filter", "classific",
      "category", "header", "format", "digits", "api", "value_type"
    )
  )
  expect_identical(names(formals(info_sidra)), c("x", "wb"))
  expect_identical(names(formals(search_sidra)), "x")

  get_defaults <- formals(get_sidra)
  expect_identical(get_defaults$variable, "allxp")
  expect_identical(get_defaults$period, "last")
  expect_identical(get_defaults$geo, "Brazil")
  expect_identical(get_defaults$header, TRUE)
  expect_identical(get_defaults$format, 4)
  expect_identical(get_defaults$digits, "default")
  expect_null(get_defaults$api)
  expect_identical(
    get_defaults$value_type,
    quote(c("numeric", "character", "both"))
  )
  expect_identical(formals(info_sidra)$wb, FALSE)
})

test_that("public validation fails before any network request", {
  expect_error(get_sidra(), "'x' is required")
  expect_error(info_sidra("not-a-table"), "numeric SIDRA table code")
  expect_error(search_sidra(NA_character_), "non-empty character vector")
})
