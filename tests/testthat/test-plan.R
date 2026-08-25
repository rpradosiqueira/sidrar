test_that("sidra_plan calculates explicit dimension cardinalities offline", {
  testthat::local_mocked_bindings(
    .sidra_request = function(...) {
      fail("sidra_plan() must not download values")
    },
    .package = "sidrar"
  )

  query <- sidrar:::sidra_query(
    1612,
    variable = c(214, 215),
    period = c("2020", "2021"),
    geo = "n6",
    geo.filter = list(c(5002704, 5003702)),
    classific = "c81",
    category = list(c(2702, 2703))
  )
  plan <- sidrar:::sidra_plan(query, limit = 10)

  expect_s3_class(plan, "sidra_plan")
  expect_named(
    plan,
    c(
      "table", "url", "dimensions", "total_estimated", "limit",
      "exceeds_limit", "risk"
    )
  )
  expect_identical(plan$table, "1612")
  expect_identical(plan$url, query$url)
  expect_s3_class(plan$dimensions, "data.frame")
  expect_named(
    plan$dimensions,
    c("dimension", "parameter", "selection", "cardinality", "known", "reason")
  )
  expect_identical(
    plan$dimensions$dimension,
    c("territory", "period", "variable", "classification:c81")
  )
  expect_equal(plan$dimensions$cardinality, c(2, 2, 2, 2))
  expect_true(all(plan$dimensions$known))
  expect_equal(plan$total_estimated, 16)
  expect_identical(plan$limit, 10)
  expect_true(plan$exceeds_limit)
  expect_identical(plan$risk, "exceeds_limit")

  at_limit <- sidrar:::sidra_plan(query, limit = 16)
  expect_false(at_limit$exceeds_limit)
  expect_identical(at_limit$risk, "within_limit")
})

test_that("sidra_plan leaves special selections unknown", {
  query <- sidrar:::sidra_query(
    1612,
    variable = "allxp",
    period = c(last = 12),
    geo = "n3",
    classific = "c81",
    category = list("all")
  )
  plan <- sidrar:::sidra_plan(query, limit = 12345)

  expect_true(is.na(plan$total_estimated))
  expect_true(any(!plan$dimensions$known))
  expect_true(all(
    c("period", "variable", "classification:c81") %in%
      plan$dimensions$dimension[!plan$dimensions$known]
  ))
  expect_true(is.na(plan$exceeds_limit))
  expect_identical(plan$risk, "unknown")

  no_limit <- sidrar:::sidra_plan(query)
  expect_true(is.na(no_limit$limit))
  expect_true(is.na(no_limit$exceeds_limit))
  expect_identical(no_limit$risk, "not_assessed")

  for (special_period in list("all", "first", c(first = 5))) {
    special_query <- sidrar:::sidra_query(
      1612,
      variable = 214,
      period = special_period,
      geo = "n1",
      classific = character()
    )
    expect_true(is.na(sidrar:::sidra_plan(special_query)$total_estimated))
  }

  mixed <- sidra_query(
    1612,
    variable = c("allxp", "214"),
    period = "2021",
    geo = "n1",
    classific = character()
  )
  mixed_plan <- sidra_plan(mixed)
  expect_true(is.na(mixed_plan$total_estimated))
  expect_identical(
    mixed_plan$dimensions$reason[mixed_plan$dimensions$dimension == "variable"],
    "mixed_special_selection"
  )
})

test_that("sidra_plan treats territorial levels as one union dimension", {
  query <- sidrar:::sidra_query(
    1612,
    variable = 214,
    period = "2021",
    geo = c("n1", "n3"),
    geo.filter = list(1, c(50, 51)),
    classific = character()
  )
  plan <- sidrar:::sidra_plan(query)

  territory <- plan$dimensions[plan$dimensions$dimension == "territory", ]
  expect_identical(territory$parameter, "n1+n3")
  expect_equal(territory$cardinality, 3)
  expect_equal(plan$total_estimated, 3)

  filtered <- sidrar:::sidra_query(
    1612,
    variable = 214,
    period = "2021",
    geo = "n6",
    geo.filter = list(n3 = 50),
    classific = character()
  )
  filtered_plan <- sidrar:::sidra_plan(filtered, limit = 100)
  filtered_territory <- filtered_plan$dimensions[
    filtered_plan$dimensions$dimension == "territory",
  ]
  expect_true(is.na(filtered_territory$cardinality))
  expect_identical(
    filtered_territory$reason,
    "territorial_filter_requires_metadata"
  )
  expect_true(is.na(filtered_plan$total_estimated))

  repeated <- sidra_plan(
    "/t/1612/n3/50/n3/51/p/2021/v/214/h/n"
  )
  repeated_territory <- repeated$dimensions[
    repeated$dimensions$dimension == "territory",
  ]
  expect_true(is.na(repeated_territory$cardinality))
  expect_identical(
    repeated_territory$reason,
    "overlapping_territorial_levels"
  )
})

test_that("sidra_plan recognizes G queries and relative API paths", {
  query <- sidrar:::sidra_query(
    1612,
    variable = 214,
    period = "2021",
    classific = character(),
    geo_view = 44
  )
  plan <- sidrar:::sidra_plan(query, limit = 999)
  territory <- plan$dimensions[plan$dimensions$dimension == "territory", ]

  expect_identical(territory$parameter, "g")
  expect_identical(territory$selection, "44")
  expect_identical(
    territory$reason,
    "territorial_view_requires_metadata"
  )
  expect_identical(plan$risk, "unknown")

  direct <- sidrar:::sidra_plan(
    "/t/1612/n1/1/p/2021/v/214/c81/2702/h/n",
    limit = 1
  )
  expect_equal(direct$total_estimated, 1)
  expect_identical(direct$risk, "within_limit")
})

test_that("sidra_plan validates its input and optional limit", {
  query <- sidrar:::sidra_query(
    1612,
    variable = 214,
    period = "2021",
    geo = "n1",
    classific = character()
  )

  expect_error(sidrar:::sidra_plan(query, limit = 0), "positive finite")
  expect_error(sidrar:::sidra_plan(query, limit = Inf), "positive finite")
  expect_error(sidrar:::sidra_plan(list(url = query$url)), "must be")
  expect_error(
    sidrar:::sidra_plan(
      "/t/1612/n1/1/g/44/p/2021/v/214"
    ),
    "cannot combine"
  )
  expect_error(
    sidrar:::sidra_plan(
      "/t/1612/g/44/u/y/p/2021/v/214"
    ),
    "extinct units"
  )
  expect_error(
    sidrar:::sidra_plan(
      "/t/1612/n1/1/p/2021/v/214/c81/1/c81/2"
    ),
    "duplicate classifications"
  )
})
