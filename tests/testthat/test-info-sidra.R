test_that("JSON descriptors retain the legacy info_sidra structure", {
  info <- sidrar:::.descriptor_to_legacy_info(descriptor_fixture())

  expect_identical(
    names(info),
    c("table", "period", "variable", "classific_category", "geo")
  )
  expect_match(info$table, "^Tabela 1419:")
  expect_identical(info$period, "janeiro 2012 a fevereiro 2012")
  expect_identical(info$variable$cod, c("63", "2265"))
  expect_match(info$variable$desc[[2L]], "12 meses", fixed = TRUE)
  expect_match(
    info$variable$desc[[2L]],
    "dezembro 2012 a dezembro 2019",
    fixed = TRUE
  )
  expect_identical(
    names(info$classific_category),
    "c315 = Geral, grupo, subgrupo, item e subitem (2)"
  )
  expect_identical(
    info$geo$cod,
    c("Brazil", "City", "MetroRegion")
  )
})

test_that("period codes are a fallback for older descriptor shapes", {
  descriptor <- descriptor_fixture()
  descriptor$PeriodoDisponibilidade <- NULL

  info <- sidrar:::.descriptor_to_legacy_info(descriptor)
  expect_identical(info$period, "201201, 201202")
})

test_that("info_sidra uses the JSON descriptor and validates wb", {
  testthat::local_mocked_bindings(
    .fetch_descriptor = function(x) descriptor_fixture(),
    .package = "sidrar"
  )

  info <- info_sidra(1419)
  expect_identical(info$variable$cod, c("63", "2265"))
  expect_error(info_sidra(1419, wb = NA), "TRUE or FALSE")
  expect_error(info_sidra(1419, wb = c(TRUE, FALSE)), "TRUE or FALSE")
})

test_that("wb uses the portable browser helper without prompting", {
  opened <- NULL
  testthat::local_mocked_bindings(
    .open_sidra_descriptor = function(url) {
      opened <<- url
      invisible(TRUE)
    },
    .package = "sidrar"
  )

  result <- info_sidra(1419, wb = TRUE)
  expect_identical(
    opened,
    "https://apisidra.ibge.gov.br/desctabapi.aspx?c=1419"
  )
  expect_identical(result, opened)
})
