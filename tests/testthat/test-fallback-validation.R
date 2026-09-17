selection_fixture <- function() {
  data.frame(
    NC = c("Level code", "1", "3", "3", "3"),
    NN = c("Level", "Brazil", "State", "State", "State"),
    MC = c("Unit code", rep("2", 4)), MN = c("Unit", rep("%", 4)),
    V = c("Valor", "1.0", "2.0", "...", "4.0"),
    D1C = c("Place code", "1", "0031", "0031", "0012"),
    D1N = c("Place", "Brazil", "Minas Gerais", "Minas Gerais", "Acre"),
    D2C = c("Year code", "2021", "2022", "2021", "2022"),
    D2N = c("Year", "2021", "2022", "2021", "2022"),
    D3C = c("Variable code", "63", "63", "64", "64"),
    D3N = c("Variable", "A", "A", "B", "B"),
    D4C = c("Category code", "9", "9", "10", "10"),
    D4N = c("Category", "C", "C", "D", "D"), stringsAsFactors = FALSE
  )
}

selection_alternative <- function(
  period = "2021,2022", variable = "63,64", geography = "n1/all/n3/31,12",
  category = "9,10"
) {
  sidrar:::.sidra_fallback_url(paste0(
    "https://apisidra.ibge.gov.br/values/t/1/", geography,
    "/p/", utils::URLencode(period, reserved = TRUE),
    "/v/", variable, "/c17/", category
  ))
}

test_that("selection checks preserve sparse tables and textual identifiers", {
  parsed <- selection_fixture()
  before <- parsed
  alternative <- selection_alternative()
  expect_no_warning(sidrar:::.sidra_fallback_validate_selection(parsed, alternative))
  expect_identical(parsed, before)
  # Four observations need not fill the geography x period x variable x category
  # product. Leading zeros compare canonically but remain in the returned data.
  expect_identical(parsed$D1C[-1L], c("1", "0031", "0031", "0012"))
})

test_that("out-of-selection periods variables levels and categories fail", {
  cases <- list(
    list(column = "D2C", row = 2L, value = "209999", dimension = "period"),
    list(column = "D3C", row = 2L, value = "999999", dimension = "variable"),
    list(column = "NC", row = 2L, value = "6", dimension = "territorial level"),
    list(column = "D1C", row = 3L, value = "99", dimension = "n3"),
    list(column = "D4C", row = 2L, value = "88", dimension = "c17")
  )
  for (case in cases) {
    parsed <- selection_fixture()
    parsed[[case$column]][[case$row]] <- case$value
    error <- tryCatch(sidrar:::.sidra_fallback_validate_selection(
      parsed, selection_alternative()
    ), error = identity)
    expect_s3_class(error, "sidrar_response_mismatch_error")
    expect_s3_class(error, "sidrar_parse_error")
    expect_identical(error$dimension, case$dimension)
    expect_identical(error$received, case$value)
    expect_match(error$url, "^https://servicodados[.]ibge[.]gov[.]br/")
  }
})

test_that("period intervals and comma lists constrain returned periods", {
  for (selection in c("2020-2023", "2021,2022-2023", "02020-02023")) {
    expect_no_warning(sidrar:::.sidra_fallback_validate_selection(
      selection_fixture(), selection_alternative(period = selection)
    ))
  }
  for (selection in c("2023-2024", "2018,2023-2024", "2022-2020")) {
    expect_error(sidrar:::.sidra_fallback_validate_selection(
      selection_fixture(), selection_alternative(period = selection)
    ), class = "sidrar_response_mismatch_error")
  }
  # Numeric-string comparisons are independent of floating point precision.
  parsed <- selection_fixture()
  parsed$D2C[-1L] <- rep(c("9007199254740992", "9007199254740993"), 2)
  expect_no_warning(sidrar:::.sidra_fallback_validate_selection(
    parsed, selection_alternative(period = "9007199254740992-9007199254740993")
  ))
  expect_error(sidrar:::.sidra_fallback_validate_selection(
    parsed, selection_alternative(period = "9007199254740991-9007199254740992")
  ), class = "sidrar_response_mismatch_error")
})

test_that("relative periods never permit more periods than requested", {
  for (selection in c("first 2", "last 2", "all", "first 3")) {
    expect_no_warning(sidrar:::.sidra_fallback_validate_selection(
      selection_fixture(), selection_alternative(period = selection)
    ))
  }
  for (selection in c("first", "last", "first 1", "last 1")) {
    expect_error(sidrar:::.sidra_fallback_validate_selection(
      selection_fixture(), selection_alternative(period = selection)
    ), class = "sidrar_response_mismatch_error")
  }
})

test_that("duplicated observation keys fail even when values or labels differ", {
  for (variation in c("same", "value", "label", "zero", "unit")) {
    parsed <- selection_fixture()
    extra <- parsed[3L, , drop = FALSE]
    if (variation == "value") extra$V <- "99.0"
    if (variation == "label") extra$D1N <- "Other label"
    if (variation == "zero") extra$D1C <- "31"
    if (variation == "unit") extra$MC <- "99"
    parsed <- rbind(parsed, extra)
    error <- tryCatch(sidrar:::.sidra_fallback_validate_selection(
      parsed, selection_alternative()
    ), error = identity)
    expect_s3_class(error, "sidrar_duplicate_error")
    expect_s3_class(error, "sidrar_parse_error")
    expect_identical(error$duplicate_rows, 5L)
  }
  parsed <- selection_fixture()
  extra <- parsed[2L, , drop = FALSE]
  extra$NC <- "2"
  parsed <- rbind(parsed, extra)
  expect_no_warning(sidrar:::.sidra_fallback_validate_selection(
    parsed, selection_alternative(geography = "n1/all/n2/all/n3/31,12")
  ))
})

test_that("missing explicit members warn without fabricating observations", {
  parsed <- selection_fixture()
  alternative <- selection_alternative(
    period = "2021,2022,2023", variable = "63,64,65",
    geography = "n1/1/n3/31,12,50", category = "9,10,11"
  )
  warnings <- list()
  withCallingHandlers(
    sidrar:::.sidra_fallback_validate_selection(parsed, alternative),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  expect_s3_class(warnings[[1L]], "sidrar_incomplete_warning")
  expect_identical(warnings[[1L]]$missing,
                   list(variable = "65", period = "2023", n3 = "50", c17 = "11"))
  expect_match(conditionMessage(warnings[[1L]]), "unavailable data or an incomplete")
  expect_identical(nrow(parsed), 5L)
  warning <- NULL
  withCallingHandlers(
    sidrar:::.sidra_fallback_validate_selection(
      parsed, selection_alternative(period = "2018,2020-2022,2024")
    ), warning = function(w) {
      warning <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(warning, "sidrar_incomplete_warning")
  expect_identical(warning$missing, list(period = c("2018", "2024")))
})

test_that("all and contextual filters do not infer absent or parent membership", {
  alternative <- selection_alternative(
    period = "all", variable = "allxp", geography = "n1/all/n3/in%20n2%201",
    category = "all"
  )
  expect_no_warning(sidrar:::.sidra_fallback_validate_selection(
    selection_fixture(), alternative
  ))
  empty <- selection_fixture()[1L, , drop = FALSE]
  expect_no_warning(sidrar:::.sidra_fallback_validate_selection(empty, alternative))
  expect_warning(sidrar:::.sidra_fallback_validate_selection(
    empty, selection_alternative()
  ), class = "sidrar_incomplete_warning")
})

test_that("selection failures preserve the primary challenge through public calls", {
  parsed <- selection_fixture()
  parsed$D3C[[2L]] <- "999999"
  testthat::local_mocked_bindings(
    .sidra_request = function(url) {
      if (startsWith(url, "https://apisidra.ibge.gov.br/")) {
        sidrar:::.sidrar_abort(
          "Challenge", c("sidrar_challenge_error", "sidrar_http_error"),
          status_code = 403L, cf_ray = "selection-ray", url = url
        )
      }
      as.character(jsonlite::toJSON(parsed, dataframe = "rows"))
    }, .package = "sidrar"
  )
  query <- sidra_query(api = "/t/1/n1/all/n3/31,12/v/63,64/p/2021,2022/c17/9,10")
  error <- suppressMessages(tryCatch(sidra_collect(query), error = identity))
  expect_s3_class(error, "sidrar_response_mismatch_error")
  expect_s3_class(error$primary_error, "sidrar_challenge_error")
  expect_identical(error$primary_error$cf_ray, "selection-ray")
  expect_identical(error$batch_index, 1L)
})
