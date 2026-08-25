test_that("responses with and without a header keep all data rows", {
  with_header <- sidrar:::.parse_sidra_values(
    values_with_header_json(),
    header = TRUE
  )
  without_header <- sidrar:::.parse_sidra_values(
    values_without_header_json(),
    header = FALSE
  )

  expect_s3_class(with_header, "data.frame")
  expect_identical(nrow(with_header), 1L)
  expect_identical(names(with_header), c(
    "Nível Territorial (Código)", "Nível Territorial", "Valor"
  ))
  expect_identical(row.names(with_header), "1")
  expect_equal(with_header$Valor, 0.16)

  expect_identical(nrow(without_header), 1L)
  expect_identical(names(without_header), c("NC", "NN", "V"))
  expect_equal(without_header$V, 0.16)
})

test_that("SIDRA special values can be numeric, character, or both", {
  text <- paste0(
    '[{"V":"-"},{"V":"0"},{"V":"X"},',
    '{"V":".."},{"V":"..."},{"V":"A"}]'
  )

  numeric <- sidrar:::.parse_sidra_values(text, FALSE, "numeric")
  character <- sidrar:::.parse_sidra_values(text, FALSE, "character")
  both <- sidrar:::.parse_sidra_values(text, FALSE, "both")

  expect_true(is.na(numeric$V[[1L]]))
  expect_equal(numeric$V[[2L]], 0)
  expect_true(all(is.na(numeric$V[3:6])))
  expect_identical(
    character$V,
    c("-", "0", "X", "..", "...", "A")
  )
  expect_identical(both$V_raw, character$V)
  expect_equal(both$V, numeric$V)
})

test_that("malformed and empty responses fail with parse errors", {
  expect_error(
    sidrar:::.parse_sidra_values("", FALSE),
    class = "sidrar_parse_error"
  )
  expect_error(
    sidrar:::.parse_sidra_values("<html>error</html>", FALSE),
    class = "sidrar_parse_error"
  )
  expect_error(
    sidrar:::.parse_sidra_values("[]", TRUE),
    "no header record",
    class = "sidrar_parse_error"
  )
  expect_error(
    sidrar:::.parse_sidra_values('{"V":"1"}', FALSE),
    "unexpected values structure",
    class = "sidrar_parse_error"
  )
})

test_that("header edge cases and raw-column collisions are deterministic", {
  expect_error(
    sidrar:::.parse_sidra_values('[{"V":""},{"V":"1"}]', TRUE),
    "invalid header record",
    class = "sidrar_parse_error"
  )
  expect_error(
    sidrar:::.parse_sidra_values('[{"V":null},{"V":"1"}]', TRUE),
    "invalid header record",
    class = "sidrar_parse_error"
  )

  header_only <- sidrar:::.parse_sidra_values(
    '[{"V":"Valor"}]',
    TRUE
  )
  expect_identical(names(header_only), "Valor")
  expect_identical(nrow(header_only), 0L)

  collision <- sidrar:::.parse_sidra_values(
    '[{"Valor":"1.5","Valor_raw":"existing"}]',
    FALSE,
    "both"
  )
  expect_identical(names(collision), c("Valor", "Valor_raw", "Valor_raw.1"))
  expect_identical(collision$Valor, 1.5)
  expect_identical(collision$Valor_raw, "existing")
  expect_identical(collision$Valor_raw.1, "1.5")
})

test_that("get_sidra accepts full URLs and honors h/n", {
  seen_url <- NULL
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      seen_url <<- url
      values_without_header_json()
    },
    .package = "sidrar"
  )

  url <- paste0(
    "https://apisidra.ibge.gov.br/values/",
    "t/7060/n1/all/v/63/p/last/h/n"
  )
  expect_message(
    result <- get_sidra(api = url, value_type = "character"),
    "query-construction arguments are ignored.*value_type.*still applies"
  )

  expect_identical(seen_url, url)
  expect_identical(nrow(result), 1L)
  expect_identical(result$V, "0.16")
})

test_that("blank query strings are rejected before making a request", {
  testthat::local_mocked_bindings(
    .sidra_request = function(...) {
      stop("a network request should not be made", call. = FALSE)
    },
    .package = "sidrar"
  )

  expect_error(
    get_sidra(1, period = "  "),
    "'period'.*whitespace-only"
  )
  expect_error(
    get_sidra(1, variable = c(63, "\t")),
    "'variable'.*whitespace-only"
  )
  expect_error(
    get_sidra(1, geo = "City", geo.filter = list(State = " ")),
    "'geo.filter'.*whitespace-only"
  )
  expect_error(
    get_sidra(1, classific = " "),
    "'classific'.*whitespace-only"
  )
  expect_error(
    get_sidra(1, classific = 315, category = list(" ")),
    "'category'.*whitespace-only"
  )
})

test_that("documented vector queries work in modern R", {
  seen_url <- NULL
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      seen_url <<- url
      values_without_header_json()
    },
    .package = "sidrar"
  )

  result <- get_sidra(
    x = 1378,
    variable = 93,
    geo = c("State", "City"),
    geo.filter = list(Region = 3, Region = 3),
    classific = c("c1", "c2"),
    category = list(1),
    header = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_match(
    seen_url,
    "n3/in%20n2%203/n6/in%20n2%203",
    fixed = TRUE
  )
  expect_match(seen_url, "/c1/1/c2/all", fixed = TRUE)
})

test_that("get_sidra forwards territorial views and extinct-unit selections", {
  seen <- character()
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      seen <<- c(seen, url)
      values_without_header_json()
    },
    .package = "sidrar"
  )

  get_sidra(
    1612,
    variable = 214,
    period = "2021",
    classific = character(),
    header = FALSE,
    geo_view = 44
  )
  get_sidra(
    1612,
    variable = 214,
    period = "2021",
    geo = "n3",
    classific = character(),
    header = FALSE,
    include_extinct = TRUE
  )

  expect_match(seen[[1L]], "/g/44/", fixed = TRUE)
  expect_match(seen[[2L]], "/n3/all/u/y/", fixed = TRUE)
})
