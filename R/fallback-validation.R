.sidra_fallback_validate_selection <- function(parsed, alternative) {
  # Validate the canonical v3 dimensions, before their output permutation.
  rows <- seq_len(nrow(parsed))[-1L]
  data <- parsed[rows, , drop = FALSE]
  selections <- alternative$selections
  canonical <- .discovery_canonical_digits
  code_columns <- grep("^D[0-9]+C$", names(data), value = TRUE)

  mismatch <- function(dimension, requested, received) {
    .sidrar_abort(
      paste0(
        "IBGE's alternative API returned ", dimension,
        " codes outside the requested selection"
      ),
      c("sidrar_response_mismatch_error", "sidrar_parse_error"),
      dimension = dimension, requested = requested,
      received = unique(received), url = alternative$url
    )
  }
  # The territorial level is part of the key: Brazil and a region may share
  # the same locality code. Units and labels are not independent dimensions.
  key <- lapply(data[c("NC", code_columns)], canonical)
  key <- as.data.frame(key, stringsAsFactors = FALSE)
  if (anyDuplicated(key)) {
    .sidrar_abort(
      "IBGE's alternative API returned duplicate observation keys",
      c("sidrar_duplicate_error", "sidrar_parse_error"),
      duplicate_rows = which(duplicated(key)), url = alternative$url
    )
  }

  missing <- list()
  explicit <- function(selection) {
    grepl("^[0-9]+(,[0-9]+)*$", selection)
  }
  check <- function(received, selection, dimension) {
    if (!explicit(selection)) return(invisible(NULL))
    requested <- strsplit(selection, ",", fixed = TRUE)[[1L]]
    allowed <- canonical(requested)
    observed <- canonical(received)
    outside <- !observed %in% allowed
    if (any(outside)) mismatch(dimension, requested, received[outside])
    absent <- !allowed %in% observed
    if (any(absent)) missing[[dimension]] <<- unique(requested[absent])
    invisible(NULL)
  }

  check(data$D3C, selections[["v"]], "variable")
  period <- tolower(selections[["p"]])
  if (explicit(period)) {
    check(data$D2C, period, "period")
  } else if (grepl("^[0-9]+(-[0-9]+)?(,[0-9]+(-[0-9]+)?)*$", period)) {
    tokens <- strsplit(period, ",", fixed = TRUE)[[1L]]
    observed <- canonical(data$D2C)
    # Compare digit strings without coercing administrative codes to doubles.
    less_equal <- function(a, b) {
      nchar(a) < nchar(b) | (nchar(a) == nchar(b) & a <= b)
    }
    allowed <- rep(FALSE, length(observed))
    for (token in tokens) {
      limits <- canonical(strsplit(token, "-", fixed = TRUE)[[1L]])
      if (length(limits) == 1L) {
        allowed <- allowed | observed == limits
      } else {
        allowed <- allowed |
          (less_equal(limits[[1L]], observed) & less_equal(observed, limits[[2L]]))
      }
    }
    if (any(!allowed)) mismatch("period", period, data$D2C[!allowed])
    singletons <- tokens[!grepl("-", tokens, fixed = TRUE)]
    absent <- !canonical(singletons) %in% observed
    if (any(absent)) missing$period <- unique(singletons[absent])
  } else if (grepl("^(first|last)( [1-9][0-9]*)?$", period)) {
    count <- if (grepl(" ", period, fixed = TRUE)) {
      suppressWarnings(as.double(sub("^[^ ]+ ", "", period)))
    } else 1
    if (length(unique(canonical(data$D2C))) > count) {
      mismatch("period", period, data$D2C)
    }
  }

  geo_keys <- names(selections)[grepl("^n[0-9]+$", names(selections))]
  requested_levels <- canonical(substring(geo_keys, 2L))
  levels <- canonical(data$NC)
  if (any(!levels %in% requested_levels)) {
    mismatch("territorial level", requested_levels, data$NC[!levels %in% requested_levels])
  }
  for (i in seq_along(geo_keys)) {
    selected <- levels == requested_levels[[i]]
    check(data$D1C[selected], selections[[geo_keys[[i]]]], geo_keys[[i]])
    # Contextual selections (e.g. municipalities within a state) require an
    # official correspondence; do not infer parent membership from a prefix.
  }
  for (i in seq_along(alternative$classes)) {
    classification <- alternative$classes[[i]]
    check(data[[paste0("D", i + 3L, "C")]],
          selections[[classification]], classification)
  }

  # Absence is not necessarily truncation. Warn for missing explicit members,
  # but never require a Cartesian product or add rows/zeros to sparse tables.
  if (length(missing)) {
    descriptions <- vapply(names(missing), function(dimension) {
      codes <- missing[[dimension]]
      paste0(dimension, " [", paste(utils::head(codes, 5L), collapse = ", "),
             if (length(codes) > 5L) ", ..." else "", "]")
    }, character(1))
    warning(structure(
      list(
        message = paste0(
          "IBGE's alternative API did not return some explicitly requested ",
          "members: ", paste(descriptions, collapse = "; "),
          ". This may reflect unavailable data or an incomplete response; ",
          "no missing observations were filled."
        ),
        call = NULL, missing = missing, url = alternative$url
      ),
      class = c("sidrar_incomplete_warning", "warning", "condition")
    ))
  }
  invisible(NULL)
}
