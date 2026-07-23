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
    result <- get_sidra(api = url),
    "other arguments are ignored"
  )

  expect_identical(seen_url, url)
  expect_identical(nrow(result), 1L)
  expect_equal(result$V, 0.16)
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
