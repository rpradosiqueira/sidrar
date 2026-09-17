fallback_format_fixture <- function() {
  data.frame(
    NC = c("Territorial level (Code)", "3", "3", "3"),
    NN = c("Territorial level", "State", "State", "State"),
    MC = c("Unit (Code)", "1", "1", "1"),
    MN = c("Unit", "People", "People", "People"),
    V = c("Valor", "1.2", "...", "0.123"),
    D1C = c("State (Code)", "0031", "0012", "0031"),
    D1N = c("State", "Minas Gerais", "Acre", "Minas Gerais"),
    D2C = c("Year (Code)", "2022", "2021", "2022"),
    D2N = c("Year", "2022", "2021", "2022"),
    D3C = c("Variable (Code)", "4099", "4099", "5935"),
    D3N = c("Variable", "First", "First", "Second"),
    D4C = c("Category (Code)", "009", "001", "009"),
    D4N = c("Category", "Nine", "One", "Nine"),
    D5C = c("Implicit category (Code)", "1", "1", "1"),
    D5N = c("Implicit category", "All", "All", "All"),
    stringsAsFactors = FALSE
  )
}

fallback_format_alternative <- function(
  dimensions = c("n", "p", "v", "c315"), digits = "s", classes = "c315"
) {
  list(
    dimensions = dimensions, classes = classes, format = "a",
    precision = sidrar:::.sidra_fallback_precision(digits)
  )
}

test_that("fallback precision supports only verified unambiguous syntax", {
  for (value in c("s", " S ")) {
    expect_identical(
      sidrar:::.sidra_fallback_precision(value),
      list(mode = "default", digits = NULL, variable = NULL)
    )
  }
  for (digits in 0:9) {
    expect_identical(
      sidrar:::.sidra_fallback_precision(as.character(digits)),
      list(mode = "global", digits = digits, variable = NULL)
    )
  }
  expect_identical(
    sidrar:::.sidra_fallback_precision("v4099 1"),
    list(mode = "variable", digits = 1L, variable = "4099")
  )
  expect_identical(
    sidrar:::.sidra_fallback_precision("V004099 0"),
    list(mode = "variable", digits = 0L, variable = "004099")
  )
  invalid <- list(
    NULL, NA_character_, character(), c("1", "2"), 1, "", "m", "max", "default",
    "10", "-1", "01", "1.0", "v4099", "v4099 10", "v4099  1",
    "v4099\t1", "v4099,5935 1", "v4099 1 v5935 2", "v4099 1 v4099 2"
  )
  for (value in invalid) {
    expect_null(sidrar:::.sidra_fallback_precision(value))
  }
})

test_that("canonical default fallback returns the exact original text", {
  text <- as.character(jsonlite::toJSON(fallback_format_fixture()))
  parsed <- jsonlite::fromJSON(text)
  alternative <- fallback_format_alternative()
  expect_identical(sidrar:::.sidra_fallback_format(text, parsed, alternative), text)
})

test_that("variable precision preserves official strings and other variables", {
  parsed <- fallback_format_fixture()
  text <- as.character(jsonlite::toJSON(parsed))
  alternative <- fallback_format_alternative(digits = "v4099 1")
  expect_identical(sidrar:::.sidra_fallback_format(text, parsed, alternative), text)

  # An unrelated variable keeps its representation and is not rounded.
  parsed$V[[4L]] <- "unrelated raw value"
  text <- as.character(jsonlite::toJSON(parsed))
  expect_identical(sidrar:::.sidra_fallback_format(text, parsed, alternative), text)

  alternative$precision <- sidrar:::.sidra_fallback_precision("v9999 9")
  expect_identical(sidrar:::.sidra_fallback_format(text, parsed, alternative), text)
})

test_that("global precision permits matching strings and SIDRA special symbols", {
  parsed <- fallback_format_fixture()
  alternative <- fallback_format_alternative(digits = "2")
  for (symbol in c("-", "..", "...", "X")) {
    parsed$V[-1L] <- c("-1.20", symbol, "+0.00")
    text <- as.character(jsonlite::toJSON(parsed))
    expect_identical(sidrar:::.sidra_fallback_format(text, parsed, alternative), text)
  }
  alternative$precision <- sidrar:::.sidra_fallback_precision("0")
  parsed$V[-1L] <- c("0", "X", "-123")
  text <- as.character(jsonlite::toJSON(parsed))
  expect_identical(sidrar:::.sidra_fallback_format(text, parsed, alternative), text)
})

test_that("fallback does not round, pad, or guess ambiguous decimal values", {
  parsed <- fallback_format_fixture()
  alternative <- fallback_format_alternative(digits = "v4099 1")
  for (value in c("1", "1.20", "1.23", "1e1", "1,2", "1.", ".1", " 1.2", "x", "?", "")) {
    parsed$V[[2L]] <- value
    text <- as.character(jsonlite::toJSON(parsed))
    error <- tryCatch(
      sidrar:::.sidra_fallback_format(text, parsed, alternative), error = identity
    )
    expect_s3_class(error, "sidrar_fallback_precision_error")
    expect_s3_class(error, "sidrar_parse_error")
    expect_identical(error$requested_digits, 1L)
    expect_identical(error$variable, "4099")
  }
})

test_that("variable-specific precision rejects ambiguous variable identity", {
  alternative <- fallback_format_alternative(digits = "v4099 1")
  for (code in c(NA, "", "4099/5935", "unknown")) {
    parsed <- fallback_format_fixture()
    parsed$D3C[[2L]] <- code
    expect_error(
      sidrar:::.sidra_fallback_format("text", parsed, alternative),
      "observation codes", class = "sidrar_parse_error"
    )
  }
})

test_that("fallback remaps dimensions and labels without changing row order", {
  parsed <- fallback_format_fixture()
  text <- as.character(jsonlite::toJSON(parsed))
  alternative <- fallback_format_alternative(
    dimensions = c("n", "v", "p", "c315"), digits = "v4099 1"
  )
  result <- jsonlite::fromJSON(sidrar:::.sidra_fallback_format(text, parsed, alternative))
  expect_identical(names(result), names(parsed))
  expect_identical(result$D1C, parsed$D1C)
  expect_identical(result$D2C, parsed$D3C)
  expect_identical(result$D2N, parsed$D3N)
  expect_identical(result$D3C, parsed$D2C)
  expect_identical(result$D3N, parsed$D2N)
  expect_identical(result$D4C, parsed$D4C)
  expect_identical(result$D5C, parsed$D5C)
  expect_identical(result$V, parsed$V)
  expect_identical(result$NC, parsed$NC)
})

test_that("explicit classifications move while implicit dimensions stay appended", {
  parsed <- fallback_format_fixture()
  alternative <- fallback_format_alternative(dimensions = c("c315", "p", "n", "v"))
  result <- jsonlite::fromJSON(sidrar:::.sidra_fallback_format("text", parsed, alternative))
  expect_identical(result$D1C, parsed$D4C)
  expect_identical(result$D2C, parsed$D2C)
  expect_identical(result$D3C, parsed$D1C)
  expect_identical(result$D4C, parsed$D3C)
  expect_identical(result$D5C, parsed$D5C)
})

test_that("multiple explicit classification positions are mapped independently", {
  parsed <- fallback_format_fixture()
  alternative <- fallback_format_alternative(
    dimensions = c("c315", "n", "c999", "v", "p"),
    classes = c("c315", "c999")
  )
  text <- sidrar:::.sidra_fallback_format("text", parsed, alternative)
  result <- jsonlite::fromJSON(text)
  expect_identical(result$D1C, parsed$D4C)
  expect_identical(result$D2C, parsed$D1C)
  expect_identical(result$D3C, parsed$D5C)
  expect_identical(result$D4C, parsed$D3C)
  expect_identical(result$D5C, parsed$D2C)
})

test_that("header-only responses retain headers without requiring observations", {
  parsed <- fallback_format_fixture()[1L, ]
  alternative <- fallback_format_alternative(
    dimensions = c("n", "v", "p", "c315"), digits = "v4099 1"
  )
  text <- sidrar:::.sidra_fallback_format("text", parsed, alternative)
  result <- sidrar:::.parse_sidra_values(text, value_type = "character")
  expect_identical(nrow(result), 0L)
  expect_identical(names(result)[8L], "Variable (Code)")
})

test_that("fallback validates complete contiguous dimension code/name pairs", {
  alternative <- fallback_format_alternative(dimensions = c("n", "v", "p", "c315"))
  original <- fallback_format_fixture()
  examples <- list(
    original[!names(original) %in% "D2N"],
    original[!names(original) %in% c("D2C", "D2N")],
    original[!startsWith(names(original), "D")],
    original[!names(original) %in% c("D3C", "D3N", "D4C", "D4N", "D5C", "D5N")]
  )
  leading_zero <- original
  names(leading_zero)[names(leading_zero) == "D1C"] <- "D01C"
  duplicated <- original
  names(duplicated)[names(duplicated) == "D2C"] <- "D1C"
  nested <- original
  nested$D2C <- as.list(nested$D2C)
  examples <- c(examples, list(leading_zero, duplicated))
  for (parsed in examples) {
    expect_error(
      sidrar:::.sidra_fallback_format("text", parsed, alternative),
      "inconsistent dimension", class = "sidrar_parse_error"
    )
  }
  expect_error(
    sidrar:::.sidra_fallback_format("text", nested, alternative),
    "non-textual", class = "sidrar_parse_error"
  )
})

test_that("canonical passthrough validates every required dimension", {
  parsed <- fallback_format_fixture()
  alternative <- fallback_format_alternative()
  for (column in names(parsed)) {
    broken <- parsed[!names(parsed) %in% column]
    expect_error(
      sidrar:::.sidra_fallback_format("unchanged", broken, alternative),
      class = "sidrar_parse_error"
    )
  }
  # The three basic dimensions do not satisfy an explicitly requested class.
  basic <- parsed[!names(parsed) %in% c("D4C", "D4N", "D5C", "D5N")]
  expect_error(
    sidrar:::.sidra_fallback_format("unchanged", basic, alternative),
    "inconsistent dimension", class = "sidrar_parse_error"
  )
  alternative <- fallback_format_alternative(
    dimensions = c("n", "p", "v"), classes = character()
  )
  expect_identical(
    sidrar:::.sidra_fallback_format("unchanged", basic, alternative),
    "unchanged"
  )
})

test_that("canonical schemas reject unknown and ambiguous field names", {
  parsed <- fallback_format_fixture()
  alternative <- fallback_format_alternative()
  for (column in c("extra", "metadata", "D0C", "D01C", "D6X", "D6C")) {
    broken <- parsed
    broken[[column]] <- "extra"
    expect_error(
      sidrar:::.sidra_fallback_format("unchanged", broken, alternative),
      class = "sidrar_parse_error"
    )
  }
  for (column in c("", NA_character_, "NC", "D1C")) {
    broken <- parsed
    names(broken)[[2L]] <- column
    expect_error(
      sidrar:::.sidra_fallback_format("unchanged", broken, alternative),
      class = "sidrar_parse_error"
    )
  }
})

test_that("all flat response fields must be textual, not nested or coerced", {
  parsed <- fallback_format_fixture()
  alternative <- fallback_format_alternative()
  for (column in names(parsed)) {
    for (value in list(rep(1L, 4L), rep(TRUE, 4L), as.list(parsed[[column]]))) {
      broken <- parsed
      broken[[column]] <- value
      expect_error(
        sidrar:::.sidra_fallback_format("unchanged", broken, alternative),
        "non-textual", class = "sidrar_parse_error"
      )
    }
  }
})

test_that("headers are complete nonblank text without language assumptions", {
  parsed <- fallback_format_fixture()
  alternative <- fallback_format_alternative()
  for (column in names(parsed)) {
    for (value in c(NA_character_, "", " ", "\t\r\n")) {
      broken <- parsed
      broken[[column]][[1L]] <- value
      expect_error(
        sidrar:::.sidra_fallback_format("unchanged", broken, alternative),
        "invalid values header", class = "sidrar_parse_error"
      )
    }
  }
  parsed$V[[1L]] <- "Value"
  expect_identical(
    sidrar:::.sidra_fallback_format("unchanged", parsed, alternative),
    "unchanged"
  )
  for (broken in list(parsed[FALSE, ], list(), NULL, as.matrix(parsed))) {
    expect_error(
      sidrar:::.sidra_fallback_format("unchanged", broken, alternative),
      "invalid values header", class = "sidrar_parse_error"
    )
  }
})

test_that("observation codes require text digits and retain leading zeros", {
  parsed <- fallback_format_fixture()
  alternative <- fallback_format_alternative()
  columns <- c("NC", "MC", paste0("D", 1:5, "C"))
  for (column in columns) {
    for (value in c(NA_character_, "", " 1", "1 ", "1.0", "1e2", "-1", "+1", "1,2", "...")) {
      broken <- parsed
      broken[[column]][[2L]] <- value
      expect_error(
        sidrar:::.sidra_fallback_format("unchanged", broken, alternative),
        "invalid observation codes", class = "sidrar_parse_error"
      )
    }
  }
  parsed$NC[-1L] <- "003"
  parsed$MC[-1L] <- "0001"
  original <- parsed
  text <- as.character(jsonlite::toJSON(parsed))
  expect_identical(sidrar:::.sidra_fallback_format(text, parsed, alternative), text)
  expect_identical(parsed, original)
  order <- sidrar:::.sidra_fallback_schema(parsed, alternative)
  expect_identical(order$count, 5L)
  expect_identical(order$columns, names(parsed)[startsWith(names(parsed), "D")])
})

test_that("missing labels and value strings cannot masquerade as valid data", {
  parsed <- fallback_format_fixture()
  alternative <- fallback_format_alternative()
  for (column in c("NN", "MN", "V", paste0("D", 1:5, "N"))) {
    broken <- parsed
    broken[[column]][[2L]] <- NA_character_
    expect_error(
      sidrar:::.sidra_fallback_format("unchanged", broken, alternative),
      "missing observation", class = "sidrar_parse_error"
    )
  }
  # Future nonnumeric value representations remain available to character mode.
  parsed$V[-1L] <- c("-", "...", "future value marker")
  expect_identical(
    sidrar:::.sidra_fallback_format("unchanged", parsed, alternative),
    "unchanged"
  )
})

test_that("complete header-only and sparse responses do not require a grid", {
  alternative <- fallback_format_alternative()
  for (rows in list(1L, c(1L, 3L), c(1L, 2L, 4L))) {
    parsed <- fallback_format_fixture()[rows, ]
    text <- as.character(jsonlite::toJSON(parsed))
    expect_identical(
      sidrar:::.sidra_fallback_format(text, parsed, alternative), text
    )
  }
  broken <- fallback_format_fixture()[1L, ]
  broken$D4N <- NULL
  expect_error(
    sidrar:::.sidra_fallback_format("unchanged", broken, alternative),
    "inconsistent dimension", class = "sidrar_parse_error"
  )
})

test_that("fallback refuses incomplete or ambiguous dimension mappings", {
  parsed <- fallback_format_fixture()
  for (dimensions in list(c("n", "v"), c("n", "v", "v", "c315"), c("n", "v", "p", "c999"))) {
    alternative <- fallback_format_alternative(dimensions = dimensions)
    expect_error(
      sidrar:::.sidra_fallback_format("text", parsed, alternative),
      "inconsistent dimension", class = "sidrar_parse_error"
    )
  }
})

test_that("remapped headers work with both public header modes", {
  parsed <- fallback_format_fixture()
  alternative <- fallback_format_alternative(dimensions = c("n", "v", "p", "c315"))
  text <- sidrar:::.sidra_fallback_format("text", parsed, alternative)
  named <- sidrar:::.parse_sidra_values(text, header = TRUE, value_type = "both")
  coded <- sidrar:::.parse_sidra_values(
    text, header = FALSE, value_type = "both", response_header = TRUE
  )
  expect_identical(named[["Variable (Code)"]], parsed$D3C[-1L])
  expect_identical(coded$D2C, parsed$D3C[-1L])
  expect_identical(coded$D1C, c("0031", "0012", "0031"))
  expect_identical(named$Valor_raw, parsed$V[-1L])
  expect_identical(coded$V_raw, parsed$V[-1L])
})
