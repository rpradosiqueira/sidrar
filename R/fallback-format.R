.sidra_fallback_precision <- function(digits) {
  if (!is.character(digits) || length(digits) != 1L || is.na(digits)) {
    return(NULL)
  }
  digits <- tolower(trimws(digits))
  if (identical(digits, "s")) {
    return(list(mode = "default", digits = NULL, variable = NULL))
  }
  if (grepl("^[0-9]$", digits)) {
    return(list(mode = "global", digits = as.integer(digits), variable = NULL))
  }

  # Only one variable-specific precision has a verified equivalent syntax.
  match <- regexec("^v([0-9]+) ([0-9])$", digits)
  parts <- regmatches(digits, match)[[1L]]
  if (length(parts) != 3L) {
    return(NULL)
  }
  list(mode = "variable", digits = as.integer(parts[[3L]]), variable = parts[[2L]])
}

.sidra_fallback_schema <- function(parsed, alternative) {
  invalid <- function(detail = "inconsistent dimension columns") {
    .sidrar_abort(
      paste("IBGE's alternative API returned", detail),
      "sidrar_parse_error"
    )
  }
  if (!is.data.frame(parsed) || nrow(parsed) < 1L) {
    invalid("an invalid values header")
  }
  columns <- names(parsed)
  if (anyNA(columns) || any(!nzchar(columns)) || anyDuplicated(columns)) {
    invalid()
  }
  required <- c("NC", "NN", "MC", "MN", "V")
  if (!all(required %in% columns)) {
    invalid("incomplete flat values columns")
  }
  dimension_columns <- columns[startsWith(columns, "D")]
  count <- length(dimension_columns) / 2L
  if (count < 3L || count != as.integer(count)) {
    invalid()
  }
  expected <- as.vector(rbind(
    paste0("D", seq_len(count), "C"),
    paste0("D", seq_len(count), "N")
  ))
  if (!setequal(dimension_columns, expected)) {
    invalid()
  }
  # The flat response has no defined extension fields. Refuse new fields until
  # their role is known, rather than silently preserving incompatible metadata
  # or treating it as an observation dimension during remapping.
  if (!setequal(columns, c(required, expected))) {
    invalid("unsupported flat values columns")
  }
  if (any(!vapply(parsed, is.character, logical(1)))) {
    invalid("non-textual flat values columns")
  }
  header <- vapply(parsed, `[[`, character(1), 1L)
  if (anyNA(header) || any(!nzchar(trimws(header)))) {
    invalid("an invalid values header")
  }

  classes <- alternative$classes
  dimensions <- alternative$dimensions
  canonical <- c("n", "p", "v", classes)
  if (!is.character(classes) || anyNA(classes) ||
        any(!grepl("^c[0-9]+$", classes)) ||
        anyDuplicated(canonical) || !is.character(dimensions) ||
        anyNA(dimensions) || anyDuplicated(dimensions) ||
        !setequal(dimensions, canonical) || count < length(canonical)) {
    invalid()
  }
  rows <- seq_len(nrow(parsed))[-1L]
  code_columns <- c("NC", "MC", paste0("D", seq_len(count), "C"))
  for (column in code_columns) {
    codes <- parsed[[column]][rows]
    if (anyNA(codes) || any(!grepl("^[0-9]+$", codes))) {
      invalid(paste0("invalid observation codes in '", column, "'"))
    }
  }
  label_columns <- c("NN", "MN", paste0("D", seq_len(count), "N"))
  if (any(vapply(parsed[label_columns], function(x) anyNA(x[rows]), logical(1)))) {
    invalid("missing observation labels")
  }
  if (anyNA(parsed$V[rows])) {
    invalid("missing observation value strings")
  }
  order <- match(dimensions, canonical)
  # v3 appends implicit classifications after those explicitly requested.
  if (count > length(canonical)) {
    order <- c(order, seq.int(length(canonical) + 1L, count))
  }
  list(
    columns = as.vector(rbind(
      paste0("D", order, "C"), paste0("D", order, "N")
    )),
    names = expected,
    remaining = columns[!startsWith(columns, "D")],
    count = as.integer(count)
  )
}

.sidra_fallback_check_precision <- function(parsed, precision) {
  if (identical(precision$mode, "default")) {
    return(invisible(NULL))
  }
  invalid <- function(detail) {
    .sidrar_abort(
      paste0(
        "IBGE's alternative API cannot preserve the requested decimal ",
        "precision without changing its published values: ", detail,
        ". Use default precision to retain the official representation."
      ),
      c("sidrar_fallback_precision_error", "sidrar_parse_error"),
      requested_digits = precision$digits,
      variable = precision$variable
    )
  }
  rows <- seq_len(nrow(parsed))[-1L]
  if (identical(precision$mode, "variable")) {
    variables <- parsed$D3C[rows]
    if (!is.character(variables) || anyNA(variables) ||
          any(!grepl("^[0-9]+$", variables))) {
      invalid("variable codes are missing or ambiguous")
    }
    selected <- .discovery_canonical_digits(variables) ==
      .discovery_canonical_digits(precision$variable)
    rows <- rows[selected]
  }
  values <- parsed$V[rows]
  if (!is.character(values) || anyNA(values)) {
    invalid("value strings are missing or ambiguous")
  }
  # These are SIDRA's documented non-numeric values, not numbers to round.
  values <- values[!values %in% c("-", "..", "...", "X")]
  if (any(!grepl("^[+-]?[0-9]+(\\.[0-9]+)?$", values))) {
    invalid("a value is neither a plain decimal nor a SIDRA special symbol")
  }
  decimals <- ifelse(
    grepl(".", values, fixed = TRUE),
    nchar(sub("^[^.]*\\.", "", values)),
    0L
  )
  if (any(decimals != precision$digits)) {
    invalid("the response uses a different number of decimal places")
  }
  invisible(NULL)
}

.sidra_fallback_format <- function(text, parsed, alternative) {
  # Validate even the canonical/default passthrough path: a well-formed JSON
  # response can still omit a requested dimension or contain nested fields.
  order <- .sidra_fallback_schema(parsed, alternative)
  canonical <- c("n", "p", "v", alternative$classes)
  reorder <- !identical(alternative$dimensions, canonical)
  if (!reorder && identical(alternative$precision$mode, "default")) {
    return(text)
  }
  .sidra_fallback_check_precision(parsed, alternative$precision)
  if (!reorder) {
    return(text)
  }

  # Move paired code/name fields and their header labels together. Keep the
  # API's row order: its geographic/hierarchical ordering cannot be inferred.
  result <- parsed[c(order$remaining, order$columns)]
  names(result) <- c(order$remaining, order$names)
  as.character(jsonlite::toJSON(result, dataframe = "rows", na = "null"))
}
