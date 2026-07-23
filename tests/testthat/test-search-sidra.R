test_that("search_sidra uses the official aggregate catalog", {
  testthat::local_mocked_bindings(
    .fetch_aggregate_catalog = catalog_fixture,
    .package = "sidrar"
  )

  expect_identical(
    search_sidra("ipca"),
    c(
      "7060" = "IPCA - Variação mensal e acumulada",
      "1737" = "IPCA - Série histórica"
    )
  )
  expect_identical(
    search_sidra(c("contas", "nacionais")),
    c("5932" = "Sistema de Contas Nacionais")
  )
  expect_identical(search_sidra("inexistente"), character())
})

test_that("search_sidra is accent and case insensitive", {
  testthat::local_mocked_bindings(
    .fetch_aggregate_catalog = catalog_fixture,
    .package = "sidrar"
  )

  expect_identical(
    search_sidra("SERIE HISTORICA"),
    c("1737" = "IPCA - Série histórica")
  )
  expect_identical(
    search_sidra("VARIACAO"),
    c("7060" = "IPCA - Variação mensal e acumulada")
  )
  expect_identical(
    search_sidra("SE\u0301RIE HISTO\u0301RICA"),
    c("1737" = "IPCA - Série histórica")
  )
  expect_identical(
    search_sidra(c("ipca", "acumulada")),
    c("7060" = "IPCA - Variação mensal e acumulada")
  )
  expect_error(search_sidra(character()), "non-empty character")
  expect_error(search_sidra(""), "empty search terms")
})
