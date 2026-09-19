.sidra_split_code_list <- function(selection, argument) {
  if (!is.character(selection) || length(selection) != 1L ||
      is.na(selection) || !grepl("^[0-9]+(,[0-9]+)*$", selection)) {
    stop(
      sprintf("'%s' must use explicit numeric codes before it can be split", argument),
      call. = FALSE
    )
  }
  codes <- strsplit(selection, ",", fixed = TRUE)[[1L]]
  if (anyDuplicated(.discovery_canonical_digits(codes))) {
    stop(sprintf("'%s' cannot contain duplicates when split", argument),
         call. = FALSE)
  }
  codes
}

.sidra_split_period_snapshot <- function(table) {
  periods <- sidra_periods(table, refresh = TRUE, cache = FALSE)
  invalid <- function() {
    .sidrar_abort(
      "Cannot split periods: the official period inventory is empty or invalid",
      c("sidrar_split_metadata_error", "sidrar_parse_error"), table = table
    )
  }
  if (!is.data.frame(periods) || anyDuplicated(names(periods)) ||
      !all(c("table_id", "period_id") %in% names(periods)) ||
      nrow(periods) == 0L ||
      !is.character(periods$table_id) || !is.character(periods$period_id) ||
      anyNA(periods$table_id) || anyNA(periods$period_id) ||
      any(!grepl("^[0-9]+$", periods$table_id)) ||
      any(!grepl("^[0-9]+$", periods$period_id))) {
    invalid()
  }
  if (any(.discovery_canonical_digits(periods$table_id) !=
          .discovery_canonical_digits(table)) ||
      anyDuplicated(.discovery_canonical_digits(periods$period_id))) {
    invalid()
  }
  periods
}

.sidra_split_period_selection <- function(selection, table) {
  if (grepl("^[0-9]+(,[0-9]+)*$", selection)) {
    return(list(
      selection = .sidra_split_code_list(selection, "period"),
      periods = NULL
    ))
  }
  lowered <- tolower(selection)
  relative <- grepl("^(first|last)( [1-9][0-9]*)?$", lowered)
  ranged <- grepl("^[0-9]+(-[0-9]+)?(,[0-9]+(-[0-9]+)?)*$", lowered)
  if (!identical(lowered, "all") && !relative && !ranged) {
    stop(
      "'period' must use explicit codes, ranges, 'all', 'first', or 'last' to split",
      call. = FALSE
    )
  }

  periods <- .sidra_split_period_snapshot(table)
  codes <- periods$period_id
  canonical <- .discovery_canonical_digits(codes)
  # Never coerce identifiers to doubles or infer unlisted calendar periods.
  order_index <- order(nchar(canonical), canonical, method = "radix")
  codes <- codes[order_index]
  canonical <- canonical[order_index]
  if (!identical(lowered, "all") && length(unique(nchar(codes))) != 1L) {
    stop(
      "Cannot resolve relative periods or ranges from mixed-width period codes",
      call. = FALSE
    )
  }
  if (identical(lowered, "all")) {
    selected <- codes
  } else if (relative) {
    count <- if (grepl(" ", lowered, fixed = TRUE)) {
      suppressWarnings(as.double(sub("^[^ ]+ ", "", lowered)))
    } else 1
    if (!is.finite(count) || count > .Machine$integer.max) {
      stop("The relative period count must be a positive integer", call. = FALSE)
    }
    count <- min(as.integer(count), length(codes))
    selected <- if (startsWith(lowered, "first")) {
      utils::head(codes, count)
    } else {
      utils::tail(codes, count)
    }
  } else {
    tokens <- strsplit(selection, ",", fixed = TRUE)[[1L]]
    selected <- unlist(lapply(tokens, function(token) {
      limits <- strsplit(token, "-", fixed = TRUE)[[1L]]
      positions <- match(.discovery_canonical_digits(limits), canonical)
      if (anyNA(positions)) {
        stop(
          "Every explicit period and range endpoint must exist in the period inventory",
          call. = FALSE
        )
      }
      if (length(positions) == 1L) return(codes[positions])
      if (positions[[1L]] > positions[[2L]]) {
        stop("Period ranges must have increasing endpoints", call. = FALSE)
      }
      codes[seq.int(positions[[1L]], positions[[2L]])]
    }), use.names = FALSE)
  }
  if (anyDuplicated(.discovery_canonical_digits(selected))) {
    stop("'period' cannot contain duplicates or overlapping ranges when split",
         call. = FALSE)
  }
  list(selection = selected, periods = periods)
}

.sidra_split_url_parts <- function(url) {
  url <- .normalize_api_url(url)
  parsed <- httr::parse_url(url)
  if (!is.null(parsed$username) || !is.null(parsed$password) ||
      !is.null(parsed$port) || !is.null(parsed$fragment) ||
      !is.null(parsed$params) || grepl("[[:cntrl:]\\\\]", url)) {
    stop("Only plain official SIDRA values URLs can be split safely", call. = FALSE)
  }
  # Retain the URL literally except for the one selection being partitioned.
  # Rebuilding every pair would reorder dimensions or recode precision tokens.
  match <- regexec("^(https://[^/]+/)([^?]*)(\\?.*)?$", url,
                   ignore.case = TRUE, perl = TRUE)
  parts <- regmatches(url, match)[[1L]]
  if (length(parts) != 4L || !grepl("^values/t/", parts[[3L]],
                                    ignore.case = TRUE) ||
      grepl("//|/$", parts[[3L]])) {
    stop("Only complete SIDRA values paths can be split safely", call. = FALSE)
  }
  if (nzchar(parts[[4L]]) &&
      !grepl("^\\?formato=json$", parts[[4L]], ignore.case = TRUE)) {
    stop("Custom URL query parameters cannot be split safely", call. = FALSE)
  }
  pairs <- .sidra_path_pairs(url)
  keys <- pairs$parameter
  allowed <- keys %in% c("t", "p", "v", "f", "h", "d", "u", "g") |
    grepl("^[nc][0-9]+$", keys)
  # Numeric aliases for the same dimension cannot establish disjointness.
  canonical_keys <- ifelse(
    grepl("^[nc][0-9]+$", keys),
    paste0(substring(keys, 1L, 1L),
           .discovery_canonical_digits(substring(keys, 2L))),
    keys
  )
  if (any(!allowed) || anyDuplicated(canonical_keys) ||
      any(!nzchar(pairs$selection)) ||
      !grepl("^[0-9]+$", pairs$selection[pairs$parameter == "t"])) {
    stop("Unsupported, empty, or duplicate URL parameters cannot be split safely",
         call. = FALSE)
  }
  list(prefix = parts[[2L]], tokens = strsplit(parts[[3L]], "/", fixed = TRUE)[[1L]],
       suffix = parts[[4L]], pairs = pairs)
}

.sidra_split_url <- function(query, by, size, index = 1L) {
  if (!inherits(query, "sidra_query") || !is.list(query)) {
    stop("'query' must be a sidra_query object", call. = FALSE)
  }
  by <- match.arg(by, c("period", "variable", "geo.filter", "category"))
  size <- .sidra_validate_split_size(size)
  index <- .sidra_validate_split_index(index)
  parts <- .sidra_split_url_parts(query$url)
  pairs <- parts$pairs
  keys <- pairs$parameter
  positions <- switch(
    by,
    period = which(keys == "p"),
    variable = which(keys == "v"),
    category = which(grepl("^c[0-9]+$", keys)),
    `geo.filter` = which(grepl("^n[0-9]+$", keys))
  )
  if (identical(by, "geo.filter") &&
      ("g" %in% keys || length(positions) != 1L ||
       .discovery_canonical_digits(substring(keys[positions], 2L)) == "1")) {
    stop(
      "'geo.filter' can be split only for one non-Brazil level; multiple levels can produce overlapping batches",
      call. = FALSE
    )
  }
  if (length(positions) == 0L || index > length(positions)) {
    stop(sprintf("'%s' has no explicit URL element %s", by, index), call. = FALSE)
  }
  position <- positions[[index]]
  original_selection <- pairs$selection[[position]]
  resolved <- if (identical(by, "period")) {
    .sidra_split_period_selection(original_selection, pairs$selection[keys == "t"])
  } else {
    list(selection = .sidra_split_code_list(original_selection, by), periods = NULL)
  }
  selection <- resolved$selection
  value_type <- query$parameters$value_type
  if (is.null(value_type)) value_type <- "numeric"
  groups <- split(seq_along(selection), ceiling(seq_along(selection) / size))
  queries <- lapply(groups, function(positions) {
    tokens <- parts$tokens
    # Token 1 is 'values'; every parameter consumes two further tokens.
    tokens[[position * 2L + 1L]] <- paste(selection[positions], collapse = ",")
    url <- paste0(parts$prefix, paste(tokens, collapse = "/"), parts$suffix)
    sidra_query(api = url, value_type = value_type)
  })
  structure(list(
    queries = unname(queries), by = by, size = size, index = index, source = query,
    resolution = list(
      parameter = keys[[position]], source_selection = original_selection,
      selection = selection, periods = resolved$periods,
      resolved_at = if (is.null(resolved$periods)) NULL else {
        as.POSIXct(Sys.time(), tz = "UTC")
      }
    )
  ), class = c("sidra_batch", "list"))
}
