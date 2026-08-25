.sidra_path_pairs <- function(url) {
  path <- .scalar_text(httr::parse_url(url)$path)
  tokens <- strsplit(sub("^/+", "", path), "/", fixed = TRUE)[[1L]]
  table_index <- which(tolower(tokens) == "t")

  if (length(table_index) == 0L) {
    stop(
      "SIDRA query URL does not contain a table parameter ('t/')",
      call. = FALSE
    )
  }

  tokens <- tokens[seq.int(table_index[[1L]], length(tokens))]
  if (length(tokens) %% 2L != 0L) {
    stop("SIDRA query URL contains an incomplete parameter", call. = FALSE)
  }

  parameters <- tolower(utils::URLdecode(tokens[c(TRUE, FALSE)]))
  if (sum(parameters == "t") != 1L) {
    stop(
      "SIDRA query URL must contain exactly one table parameter ('t/')",
      call. = FALSE
    )
  }

  data.frame(
    parameter = parameters,
    selection = utils::URLdecode(tokens[c(FALSE, TRUE)]),
    stringsAsFactors = FALSE
  )
}

.selection_cardinality <- function(
  selection,
  special = character(),
  ranges_unknown = FALSE
) {
  value <- trimws(tolower(selection))
  if (!nzchar(value)) {
    return(list(cardinality = NA_real_, reason = "empty_selection"))
  }

  elements <- strsplit(value, ",", fixed = TRUE)[[1L]]
  elements <- trimws(elements)
  if (any(!nzchar(elements))) {
    return(list(cardinality = NA_real_, reason = "empty_selection"))
  }
  special_elements <- elements %in% special |
    grepl("^(first|last)(\\s|$)", elements)
  if (any(special_elements)) {
    reason <- if (length(elements) == 1L) {
      "special_selection"
    } else {
      "mixed_special_selection"
    }
    return(list(cardinality = NA_real_, reason = reason))
  }
  if (ranges_unknown && any(grepl("-", elements, fixed = TRUE))) {
    return(list(cardinality = NA_real_, reason = "range_requires_metadata"))
  }
  if (anyDuplicated(elements)) {
    return(list(cardinality = NA_real_, reason = "duplicate_selection"))
  }

  list(cardinality = as.double(length(elements)), reason = "")
}

.dimension_row <- function(
  dimension,
  parameter,
  selection,
  cardinality,
  reason = ""
) {
  data.frame(
    dimension = dimension,
    parameter = parameter,
    selection = selection,
    cardinality = as.double(cardinality),
    known = !is.na(cardinality),
    reason = reason,
    stringsAsFactors = FALSE
  )
}

.territory_dimension <- function(pairs) {
  n_index <- grepl("^n[0-9]+$", pairs$parameter)
  g_index <- pairs$parameter == "g"

  if (any(n_index) && any(g_index)) {
    stop(
      "SIDRA query cannot combine territorial views ('g') with levels ('nNN')",
      call. = FALSE
    )
  }

  if (any(g_index)) {
    selections <- pairs$selection[g_index]
    return(.dimension_row(
      "territory",
      "g",
      paste(selections, collapse = ","),
      NA_real_,
      "territorial_view_requires_metadata"
    ))
  }

  if (!any(n_index)) {
    return(.dimension_row(
      "territory",
      "",
      "",
      NA_real_,
      "missing_territory"
    ))
  }

  territory <- pairs[n_index, , drop = FALSE]
  if (anyDuplicated(territory$parameter)) {
    return(.dimension_row(
      "territory",
      paste(territory$parameter, collapse = "+"),
      paste0(
        territory$parameter,
        "/",
        territory$selection,
        collapse = ";"
      ),
      NA_real_,
      "overlapping_territorial_levels"
    ))
  }
  counts <- lapply(
    territory$selection,
    function(selection) {
      value <- trimws(tolower(selection))
      if (identical(value, "all")) {
        return(list(
          cardinality = NA_real_,
          reason = "special_selection"
        ))
      }
      if (grepl("^in\\s+n[0-9]+\\s+", value)) {
        return(list(
          cardinality = NA_real_,
          reason = "territorial_filter_requires_metadata"
        ))
      }
      .selection_cardinality(value)
    }
  )
  cardinalities <- vapply(counts, `[[`, numeric(1), "cardinality")
  reasons <- vapply(counts, `[[`, character(1), "reason")
  known <- !anyNA(cardinalities)

  .dimension_row(
    "territory",
    paste(territory$parameter, collapse = "+"),
    paste0(
      territory$parameter,
      "/",
      territory$selection,
      collapse = ";"
    ),
    if (known) sum(cardinalities) else NA_real_,
    if (known) "" else paste(unique(reasons[nzchar(reasons)]), collapse = ";")
  )
}

.single_dimension <- function(
  pairs,
  parameter,
  dimension,
  default,
  special,
  ranges_unknown = FALSE
) {
  index <- which(pairs$parameter == parameter)
  if (length(index) > 1L) {
    stop(
      sprintf("SIDRA query contains more than one '%s' parameter", parameter),
      call. = FALSE
    )
  }

  selection <- if (length(index) == 0L) default else pairs$selection[index]
  count <- .selection_cardinality(
    selection,
    special = special,
    ranges_unknown = ranges_unknown
  )
  .dimension_row(
    dimension,
    parameter,
    selection,
    count$cardinality,
    count$reason
  )
}

.classification_dimensions <- function(pairs) {
  index <- grepl("^c[0-9]+$", pairs$parameter)
  if (!any(index)) {
    return(NULL)
  }

  classifications <- pairs[index, , drop = FALSE]
  rows <- lapply(seq_len(nrow(classifications)), function(i) {
    parameter <- classifications$parameter[[i]]
    selection <- classifications$selection[[i]]
    count <- .selection_cardinality(
      selection,
      special = c("all", "allxt")
    )
    .dimension_row(
      paste0("classification:", parameter),
      parameter,
      selection,
      count$cardinality,
      count$reason
    )
  })

  do.call(rbind, rows)
}

.validate_plan_limit <- function(limit) {
  if (is.null(limit)) {
    return(NA_real_)
  }
  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) ||
        !is.finite(limit) || limit <= 0) {
    stop("'limit' must be NULL or one positive finite number", call. = FALSE)
  }

  as.double(limit)
}

#' Estimate the size of a SIDRA query
#'
#' Creates an offline plan from a [sidra_query()] object or official SIDRA
#' values URL. Exact cardinalities are calculated only when they follow from
#' explicit selections in the URL. Special selections such as `all`, `allxp`,
#' `first`, and `last`, territorial views and containment filters remain
#' unknown until metadata is available.
#'
#' @param query A `sidra_query` object or an official SIDRA values URL.
#' @param limit Optional positive numeric limit used only to classify risk.
#'   No SIDRA limit is assumed by the package.
#'
#' @return A list of class `sidra_plan` containing `table`, `url`, a stable
#'   `dimensions` data frame, `total_estimated`, `limit`, `exceeds_limit`, and
#'   `risk`. The originating query is retained internally for use by
#'   [sidra_collect()]. `total_estimated` is `NA` whenever any cardinality is
#'   unknown.
#' @seealso [sidra_query()]
#' @examples
#' query <- sidra_query(
#'   1612,
#'   variable = c(214, 215),
#'   period = c("2020", "2021"),
#'   geo = "n1",
#'   classific = "c81",
#'   category = list(c(2702, 2703))
#' )
#' sidra_plan(query, limit = 20)
#' @export
sidra_plan <- function(query, limit = NULL) {
  if (inherits(query, "sidra_query")) {
    if (!is.list(query) || !is.character(query$url) ||
          length(query$url) != 1L || is.na(query$url)) {
      stop("'query' is not a valid sidra_query object", call. = FALSE)
    }
    url <- .normalize_api_url(query$url)
    query_object <- query
  } else if (is.character(query) && length(query) == 1L && !is.na(query)) {
    url <- .normalize_api_url(query)
    query_object <- sidra_query(api = url)
  } else {
    stop(
      "'query' must be a sidra_query object or official SIDRA URL",
      call. = FALSE
    )
  }

  limit <- .validate_plan_limit(limit)
  pairs <- .sidra_path_pairs(url)
  table_index <- which(pairs$parameter == "t")
  table <- if (length(table_index) == 1L) {
    pairs$selection[[table_index]]
  } else {
    stop("SIDRA query must contain exactly one table parameter", call. = FALSE)
  }

  dimensions <- rbind(
    .territory_dimension(pairs),
    .single_dimension(
      pairs, "p", "period", "last", c("all", "first", "last"), TRUE
    ),
    .single_dimension(
      pairs, "v", "variable", "allxp", c("all", "allxp")
    )
  )
  classifications <- .classification_dimensions(pairs)
  if (!is.null(classifications)) {
    dimensions <- rbind(dimensions, classifications)
  }
  rownames(dimensions) <- NULL

  total <- if (all(dimensions$known)) {
    prod(dimensions$cardinality)
  } else {
    NA_real_
  }
  if (!is.finite(total)) {
    total <- NA_real_
  }

  if (is.na(limit)) {
    exceeds_limit <- NA
    risk <- "not_assessed"
  } else if (is.na(total)) {
    exceeds_limit <- NA
    risk <- "unknown"
  } else if (total > limit) {
    exceeds_limit <- TRUE
    risk <- "exceeds_limit"
  } else {
    exceeds_limit <- FALSE
    risk <- "within_limit"
  }

  structure(
    list(
      table = table,
      url = url,
      dimensions = dimensions,
      total_estimated = total,
      limit = limit,
      exceeds_limit = exceeds_limit,
      risk = risk
    ),
    class = c("sidra_plan", "list"),
    query = query_object
  )
}

#' @export
print.sidra_plan <- function(x, ...) {
  estimated <- if (is.na(x$total_estimated)) {
    "unknown"
  } else {
    format(x$total_estimated, scientific = FALSE, trim = TRUE)
  }
  cat(
    "<sidra_plan>\n",
    "Table: ", x$table, "\n",
    "Estimated values: ", estimated, "\n",
    "Risk: ", x$risk, "\n",
    sep = ""
  )
  invisible(x)
}
