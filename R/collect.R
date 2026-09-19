.sidra_query_arguments <- function(parameters) {
  if (!is.list(parameters) || is.null(parameters$table)) {
    stop("Only structured SIDRA queries can be split", call. = FALSE)
  }

  list(
    x = parameters$table,
    variable = parameters$variable,
    period = parameters$period,
    geo = parameters$geo,
    `geo.filter` = parameters$geo_filter,
    classific = parameters$classific,
    category = parameters$category,
    header = parameters$header,
    format = parameters$format,
    digits = parameters$digits,
    value_type = parameters$value_type,
    geo_view = parameters$geo_view,
    include_extinct = parameters$include_extinct
  )
}

.sidra_validate_split_size <- function(size) {
  if (!is.numeric(size) || length(size) != 1L || is.na(size) ||
      !is.finite(size) || size < 1 || size != floor(size) ||
      size > .Machine$integer.max) {
    stop("'size' must be one positive integer", call. = FALSE)
  }
  as.integer(size)
}

.sidra_validate_split_index <- function(index) {
  if (!is.numeric(index) || length(index) != 1L || is.na(index) ||
      !is.finite(index) || index < 1 || index != floor(index) ||
      index > .Machine$integer.max) {
    stop("'index' must be one positive integer", call. = FALSE)
  }
  as.integer(index)
}

.sidra_explicit_selection <- function(selection, argument) {
  if (is.null(selection) || length(selection) == 0L || anyNA(selection)) {
    stop(
      sprintf("'%s' must contain explicit non-missing values", argument),
      call. = FALSE
    )
  }

  values <- as.character(selection)
  lowered <- tolower(trimws(values))
  special <- c("all", "allxp", "allxt", "first", "last")
  selection_names <- names(selection)
  named_period <- identical(argument, "period") &&
    !is.null(selection_names) && any(nzchar(selection_names))
  if (any(!nzchar(lowered)) ||
      named_period ||
      any(lowered %in% special) ||
      any(grepl("^(first|last)($|\\s)", lowered)) ||
      any(grepl(",", values, fixed = TRUE)) ||
      (identical(argument, "period") &&
        any(grepl("-", values, fixed = TRUE)))) {
    stop(
      sprintf("'%s' must use explicit values before it can be split", argument),
      call. = FALSE
    )
  }
  if (anyDuplicated(values)) {
    stop(
      sprintf("'%s' cannot contain duplicates when split", argument),
      call. = FALSE
    )
  }
  selection
}

.sidra_effective_classifications <- function(url) {
  pairs <- .sidra_path_pairs(url)
  selected <- grepl("^c[0-9]+$", pairs$parameter)
  if (!any(selected)) {
    return(list(classific = character(), category = list()))
  }

  list(
    classific = pairs$parameter[selected],
    category = lapply(
      pairs$selection[selected],
      function(value) strsplit(value, ",", fixed = TRUE)[[1L]]
    )
  )
}

.sidra_validate_geo_split <- function(parameters, index) {
  if (!is.null(parameters$geo_view)) {
    stop("'geo.filter' is unavailable for territorial-view queries", call. = FALSE)
  }

  geo <- .normalize_geo_names(parameters$geo)
  if (length(geo) != 1L || identical(geo, "Brazil")) {
    stop(
      paste(
        "'geo.filter' can be split only for one non-Brazil geographic",
        "level; multiple levels can produce overlapping batches"
      ),
      call. = FALSE
    )
  }
  if (index != 1L) {
    stop("'geo.filter' has only one effective element", call. = FALSE)
  }
  invisible(TRUE)
}

.sidra_nested_selection <- function(selection, index, argument) {
  if (!is.list(selection)) {
    if (index != 1L) {
      stop(sprintf("'%s' has no element %s", argument, index), call. = FALSE)
    }
    return(.sidra_explicit_selection(selection, argument))
  }
  if (index > length(selection)) {
    stop(sprintf("'%s' has no element %s", argument, index), call. = FALSE)
  }
  .sidra_explicit_selection(selection[[index]], argument)
}

.sidra_replace_nested_selection <- function(selection, index, value) {
  if (is.list(selection)) {
    selection[[index]] <- value
    return(selection)
  }
  value
}

#' Split a SIDRA query into disjoint batches
#'
#' Divides one explicit query dimension without downloading values. The
#' resulting batches can be executed with [sidra_collect()]. Period selectors
#' `all`, `first`, `last`, and ranges are resolved through [sidra_periods()]
#' into a fixed inventory before splitting. Other special selections must be
#' expanded explicitly. No values are downloaded during splitting.
#'
#' Geographic filters can be split only when the query requests one non-Brazil
#' geographic level. Queries with multiple territorial levels are rejected
#' because unchanged levels would overlap across batches. Classifications
#' already resolved in the original URL are reused without another metadata
#' request.
#'
#' @param query A [sidra_query()] object or relative/complete SIDRA values URL.
#'   URL parameter order and unsplit selections are preserved. URL geographic
#'   splitting requires direct explicit codes at one non-Brazil level.
#' @param by One of `"period"`, `"variable"`, `"geo.filter"`, or
#'   `"category"`.
#' @param size Maximum number of selected members in each batch.
#' @param index For list-valued `geo.filter` and `category`, the element to
#'   split. The default is the first element.
#'
#' @return An object of class `sidra_batch` containing disjoint `queries` in
#'   their original order.
#' @seealso [sidra_query()], [sidra_plan()], [sidra_collect()]
#' @examples
#' query <- sidra_query(
#'   1612,
#'   variable = 214,
#'   period = as.character(2018:2022),
#'   geo = "Brazil",
#'   classific = "c81",
#'   category = list(2702)
#' )
#' batches <- sidra_split(query, by = "period", size = 2)
#' length(batches$queries)
#' @export
sidra_split <- function(
  query,
  by = c("period", "variable", "geo.filter", "category"),
  size,
  index = 1L
) {
  if (is.character(query) && length(query) == 1L && !is.na(query)) {
    query <- sidra_query(api = query)
  }
  if (!inherits(query, "sidra_query") || !is.list(query)) {
    stop("'query' must be a sidra_query object", call. = FALSE)
  }

  by <- match.arg(by)
  size <- .sidra_validate_split_size(size)
  index <- .sidra_validate_split_index(index)
  parameters <- query$parameters
  if (is.null(parameters$table) ||
      (identical(by, "period") &&
       (any(grepl("all|first|last|-|,", as.character(parameters$period))) ||
        !is.null(names(parameters$period))))) {
    return(.sidra_split_url(query, by, size, index))
  }
  effective_classifications <- .sidra_effective_classifications(query$url)
  parameters$classific <- effective_classifications$classific
  parameters$category <- effective_classifications$category

  if (identical(by, "geo.filter")) {
    .sidra_validate_geo_split(parameters, index)
  }

  selection <- switch(
    by,
    period = .sidra_explicit_selection(parameters$period, "period"),
    variable = .sidra_explicit_selection(parameters$variable, "variable"),
    `geo.filter` = .sidra_nested_selection(
      parameters$geo_filter, index, "geo.filter"
    ),
    category = .sidra_nested_selection(
      parameters$category, index, "category"
    )
  )
  groups <- split(
    seq_along(selection),
    ceiling(seq_along(selection) / size)
  )

  queries <- lapply(groups, function(positions) {
    selected <- selection[positions]
    updated <- parameters
    if (identical(by, "period")) {
      updated$period <- selected
    } else if (identical(by, "variable")) {
      updated$variable <- selected
    } else if (identical(by, "geo.filter")) {
      updated$geo_filter <- .sidra_replace_nested_selection(
        updated$geo_filter, index, selected
      )
    } else {
      updated$category <- .sidra_replace_nested_selection(
        updated$category, index, selected
      )
    }

    do.call(sidra_query, .sidra_query_arguments(updated))
  })

  urls <- vapply(queries, `[[`, character(1), "url")
  if (anyDuplicated(urls)) {
    stop(
      "The requested split did not produce distinct SIDRA queries",
      call. = FALSE
    )
  }

  structure(
    list(
      queries = unname(queries),
      by = by,
      size = size,
      index = index,
      source = query
    ),
    class = c("sidra_batch", "list")
  )
}

#' @export
print.sidra_batch <- function(x, ...) {
  cat(
    "<sidra_batch>\n",
    "Queries: ", length(x$queries), "\n",
    "Split by: ", x$by, "\n",
    "Batch size: ", x$size, "\n",
    sep = ""
  )
  invisible(x)
}

.sidra_collect_queries <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    return(list(sidra_query(api = x)))
  }
  if (inherits(x, "sidra_query")) {
    return(list(x))
  }
  if (inherits(x, "sidra_batch")) {
    queries <- x$queries
  } else if (inherits(x, "sidra_plan")) {
    query <- attr(x, "query", exact = TRUE)
    if (inherits(query, "sidra_query")) {
      return(list(query))
    }
    return(list(sidra_query(api = x$url)))
  } else if (is.list(x) && length(x) > 0L) {
    queries <- x
  } else {
    stop(
      "'x' must be a sidra_query, sidra_plan, sidra_batch, or query list",
      call. = FALSE
    )
  }

  if (length(queries) == 0L ||
      !all(vapply(queries, inherits, logical(1), "sidra_query"))) {
    stop("Every batch element must be a sidra_query object", call. = FALSE)
  }
  unname(queries)
}

.sidra_result_signature <- function(data) {
  list(
    names = names(data),
    classes = vapply(
      data,
      function(column) paste(class(column), collapse = "/"),
      character(1)
    )
  )
}

.sidra_batch_error <- function(error, index, total, url) {
  error$message <- sprintf(
    "SIDRA batch %s of %s failed: %s",
    index,
    total,
    conditionMessage(error)
  )
  error$batch_index <- as.integer(index)
  error$batch_count <- as.integer(total)
  error$batch_url <- url
  stop(error)
}

#' Collect one or more planned SIDRA queries
#'
#' Executes queries sequentially, checks that every batch has the same column
#' names and types, and combines the rows without sorting or deduplicating
#' them. Parallel requests are intentionally not used.
#'
#' @param x A [sidra_query()], [sidra_plan()], [sidra_split()] result, or a
#'   non-empty list of `sidra_query` objects; also accepts a SIDRA values URL.
#' @param value_type Optional value representation overriding the preference
#'   stored in each query: `"numeric"`, `"character"`, or `"both"`.
#' @param provenance Logical. Attach URLs, access time, package version, and
#'   batch count as a `sidrar_provenance` attribute. URLs record the endpoint
#'   that supplied each batch; `requested_urls` is also included when a
#'   Cloudflare challenge required the official aggregate API fallback.
#' @param batch_size Optional positive integer. Split each input query by
#'   period into batches of at most this many periods before downloading.
#'   This is not a bound on the number of values: a single period can still
#'   exceed the service limit. The default `NULL` preserves existing batches.
#' @param checkpoint Optional path to a dedicated local directory. Successful
#'   batches are saved with checksums and a frozen query inventory. No values
#'   are saved to disk by default. Only use checkpoints you trust.
#' @param resume Logical. Reuse verified completed batches in `checkpoint`
#'   (default `TRUE`). An existing checkpoint must match the original queries,
#'   value representation, batch size, and package version. `FALSE` requires
#'   a new directory; existing files are never silently cleared.
#'
#' @details Checkpoints are separate from the metadata cache. They preserve
#'   successful responses, including their original access times; they are not
#'   refreshed automatically and can span upstream revisions. Use a new
#'   directory for a fresh collection. Relative periods are resolved once and
#'   reused on resume, even if the current catalog has changed. Corruption,
#'   incompatible schemas, mismatched settings, and concurrent writers fail
#'   explicitly. An interrupted process may leave a `.sidrar-lock` directory;
#'   remove that lock only after confirming that no collector is still running.
#'   Results are still combined in memory; checkpoints are not an out-of-core
#'   database. Row order is not a key: join different extracts by identifiers.
#'   Warnings from completed batches are saved and signaled again on resume,
#'   so reuse does not hide incomplete-coverage diagnostics.
#'
#' @return A base [data.frame()]. When `provenance = TRUE`, its
#'   `sidrar_provenance` attribute can be read with [sidra_provenance()].
#' @seealso [sidra_query()], [sidra_split()], [sidra_provenance()]
#' @examples
#' \dontrun{
#' query <- sidra_query(
#'   7060,
#'   variable = 63,
#'   period = as.character(202401:202406),
#'   geo = "Brazil",
#'   classific = "c315",
#'   category = list(7169)
#' )
#' sidra_collect(sidra_split(query, "period", size = 3))
#' }
#' @export
sidra_collect <- function(
  x, value_type = NULL, provenance = FALSE, batch_size = NULL,
  checkpoint = NULL, resume = TRUE
) {
  if (!is.null(value_type)) {
    value_type <- match.arg(value_type, c("numeric", "character", "both"))
  }
  if (!is.logical(provenance) || length(provenance) != 1L ||
      is.na(provenance)) {
    stop("'provenance' must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.null(batch_size)) {
    batch_size <- .sidra_validate_split_size(batch_size)
  }
  .sidra_checkpoint_validate(checkpoint, resume)
  original <- .sidra_collect_queries(x)
  identity <- .sidra_collection_identity(original, value_type, batch_size)
  state <- .sidra_checkpoint_open(checkpoint, identity, resume)
  on.exit(.sidra_checkpoint_unlock(state), add = TRUE)
  if (!is.null(state$manifest)) {
    queries <- state$manifest$queries
  } else {
    queries <- if (is.null(batch_size) && is.null(checkpoint)) original else unlist(
      lapply(original, function(query) {
        sidra_split(query, by = "period", size = if (is.null(batch_size)) {
          .Machine$integer.max
        } else batch_size)$queries
      }), recursive = FALSE
    )
    state <- .sidra_checkpoint_initialize(state, identity, queries)
  }
  results <- vector("list", length(queries))
  signatures <- vector("list", length(queries))
  value_types <- character(length(queries))
  actual_urls <- character(length(queries))
  accessed_at <- as.POSIXct(rep(NA_real_, length(queries)),
                           origin = "1970-01-01", tz = "UTC")
  reused <- logical(length(queries))

  for (index in seq_along(queries)) {
    query <- queries[[index]]
    query_value_type <- query$parameters$value_type
    if (is.null(query_value_type)) {
      query_value_type <- "numeric"
    }
    current_value_type <- if (is.null(value_type)) {
      match.arg(query_value_type, c("numeric", "character", "both"))
    } else {
      value_type
    }
    value_types[[index]] <- current_value_type

    saved <- .sidra_checkpoint_read(state, index)
    reused[[index]] <- !is.null(saved)
    batch_warnings <- list()
    batch <- tryCatch(
      {
        if (!is.null(saved)) {
          for (condition in saved$warnings) warning(condition)
          saved
        } else withCallingHandlers({
          .validate_sidra_url_semantics(query$url)
          response <- .sidra_values_request(query$url)
          data <- .parse_sidra_values(
            response$text, query$header, current_value_type,
            response_header = response$response_header
          )
          list(
            data = data,
            url = response$url,
            accessed_at = as.POSIXct(Sys.time(), tz = "UTC"),
            warnings = batch_warnings
          )
        }, warning = function(w) {
          batch_warnings[[length(batch_warnings) + 1L]] <<- w
        })
      },
      error = function(e) {
        e$checkpoint <- state$directory
        e$completed_batches <- index - 1L
        .sidra_batch_error(e, index, length(queries), query$url)
      }
    )
    results[[index]] <- batch$data
    actual_urls[[index]] <- batch$url
    accessed_at[[index]] <- batch$accessed_at
    signatures[[index]] <- .sidra_result_signature(results[[index]])
    if (index > 1L && !identical(signatures[[index]], signatures[[1L]])) {
      .sidrar_abort(
        sprintf(
          "SIDRA batch %s returned a schema different from batch 1",
          index
        ),
        "sidrar_batch_schema_error",
        batch_index = as.integer(index),
        batch_count = length(queries),
        batch_url = query$url,
        expected_names = signatures[[1L]]$names,
        received_names = signatures[[index]]$names,
        expected_classes = signatures[[1L]]$classes,
        received_classes = signatures[[index]]$classes
      )
    }
    if (!reused[[index]]) {
      state <- .sidra_checkpoint_write(state, index, batch)
    }
  }

  result <- do.call(rbind, results)
  rownames(result) <- NULL
  if (provenance) {
    package_version <- tryCatch(
      as.character(utils::packageVersion("sidrar")),
      error = function(e) "development"
    )
    attr(result, "sidrar_provenance") <- list(
      accessed_at = as.POSIXct(Sys.time(), tz = "UTC"),
      package_version = package_version,
      batch_count = length(queries),
      urls = actual_urls,
      value_type = unique(value_types)
    )
    requested_urls <- vapply(queries, `[[`, character(1), "url")
    if (!identical(actual_urls, requested_urls)) {
      attr(result, "sidrar_provenance")$requested_urls <- requested_urls
    }
    if (!is.null(checkpoint)) {
      attr(result, "sidrar_provenance")$batch_accessed_at <- accessed_at
      attr(result, "sidrar_provenance")$resumed <- reused
    }
  }
  result
}

#' Extract SIDRA result provenance
#'
#' @param x An object returned by [sidra_collect()] with
#'   `provenance = TRUE`.
#'
#' @return The provenance list, with `accessed_at`, `package_version`,
#'   `batch_count`, `urls`, and `value_type`; or `NULL` when none is attached.
#'   When a fallback was used, `urls` records the actual endpoints and
#'   `requested_urls` records the original queries.
#'   Checkpointed collections also include `batch_accessed_at` (original
#'   download times) and `resumed` (logical flags for reused batches).
#' @seealso [sidra_collect()]
#' @export
sidra_provenance <- function(x) {
  attr(x, "sidrar_provenance", exact = TRUE)
}
