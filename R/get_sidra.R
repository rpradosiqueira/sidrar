#' Get a SIDRA table
#'
#' Retrieves aggregate data from the Brazilian Institute of Geography and
#' Statistics (IBGE) SIDRA API.
#'
#' @param x A numeric SIDRA table code. It may be omitted when `api` is used.
#' @param variable A vector of variable codes. Defaults to `"allxp"`, which
#'   selects all variables except automatically generated percentages.
#' @param period A character vector of period codes, `"all"`, or a single
#'   named value such as `c(last = 12)` or `c(first = 5)`. Defaults to
#'   `"last"`, the latest available period.
#' @param geo A character vector containing supported geographic levels.
#'   Aliases and their `nNN` codes are case-insensitive. Defaults to
#'   `"Brazil"`.
#' @param geo.filter A list of geographic filters. Each element corresponds
#'   positionally to an element of `geo`; names may identify a higher
#'   geographic level, such as `list(State = 50)` for cities in a state.
#' @param classific A vector of classification codes. Defaults to `"all"`.
#' @param category `"all"` or a list containing categories for each
#'   classification.
#' @param header Logical. Should the first API record be used as the returned
#'   column names?
#' @param format An integer from 1 to 4 controlling the returned descriptor
#'   fields. See Details.
#' @param digits `"default"`, `"max"`, or an integer from 0 to 9.
#' @param api A relative SIDRA API path or a complete URL under
#'   `https://apisidra.ibge.gov.br/values`. When supplied, the other query
#'   arguments are ignored.
#' @param value_type How the value column is returned: `"numeric"` preserves
#'   the historical numeric interface, `"character"` preserves SIDRA symbols,
#'   and `"both"` keeps the numeric column and appends a `_raw` column.
#' @param geo_view Optional numeric SIDRA territorial-view code, using the
#'   API's `G` parameter instead of an `N` level. It cannot be combined with
#'   explicitly supplied `geo` or `geo.filter` values.
#' @param include_extinct Logical. Include extinct territorial units in `geo`
#'   queries through the API's `/u/y` parameter. It cannot be combined with
#'   `geo_view`.
#'
#' @details
#' Supported values of `geo` are `"Brazil"`, `"Region"`, `"State"`,
#' `"IntermediaryRegion"`, `"ImmediateRegion"`, `"MesoRegion"`,
#' `"MicroRegion"`, `"MetroRegion"`, `"MetroRegionDiv"`, `"IRD"`,
#' `"UrbAglo"`, `"PopArrang"`, `"City"`, `"District"`,
#' `"subdistrict"`, and `"Neighborhood"`.
#' Their corresponding `nNN` codes and all aliases are accepted without regard
#' to letter case.
#'
#' `format = 1` returns codes, `format = 2` returns names, `format = 3`
#' returns codes and names for geographic units plus names for other
#' descriptors, and `format = 4` returns codes and names for all descriptors.
#'
#' Requests use HTTPS, UTF-8 decoding, a timeout, and limited retries for
#' transient failures. Set `options(sidrar.timeout = 120)` or
#' `options(sidrar.retries = 4)` to override their defaults. Responses are
#' requested live and are not cached by the package.
#' HTTP 429 and 503 responses honor a valid `Retry-After` delay (seconds or
#' HTTP date). Set `options(sidrar.retry_after_max = 120)` to change the maximum
#' server-requested delay accepted for another attempt (default: 60 seconds).
#' The option must be one finite positive number; invalid settings use 60.
#' If the requested delay exceeds that limit,
#' a `sidrar_retry_after_error` (also a `sidrar_http_error`) carries
#' `retry_after` and `retry_after_max`, rather than retrying before the server
#' permits it. This limit does not change the per-attempt timeout or the total
#' number of attempts.
#'
#' HTTP conditions inherit from `sidrar_http_error` and carry `status_code`,
#' `response_body`, and `url`. Transport failures may additionally inherit
#' from `sidrar_timeout_error`, `sidrar_tls_error`, `sidrar_dns_error`,
#' `sidrar_connection_error`, or `sidrar_transient_error` when the underlying
#' failure can be identified conservatively.
#'
#' Cloudflare browser challenges raise `sidrar_challenge_error`, which
#' inherits from `sidrar_http_error` and also carries `cf_ray` when available.
#' For compatible values queries, the package retries through IBGE's official
#' aggregate API v3 with `view=flat`. This fallback supports multiple geographic
#' levels, explicit periods and ranges, `all`, `first`, and `last` selections,
#' standard variable/category selections, and the default descriptor format.
#' Dimension columns follow the original URL, including when variable precedes
#' period; observation order remains that returned by the alternative service.
#' Explicit decimal precision is accepted only when numeric values already have
#' the requested decimal places. Otherwise `sidrar_fallback_precision_error`
#' (also a `sidrar_parse_error`) is raised: the alternative cannot reconstruct
#' unavailable precision, and values are not rounded or padded. The default
#' precision preserves values as received; maximum precision is unsupported.
#' Unsupported selections retain the
#' original challenge error with a `fallback_reason` field. Set
#' `options(sidrar.fallback = FALSE)` to disable this alternative route.
#' If the alternative request fails, its error carries `primary_error` with
#' the original challenge. Availability still depends on IBGE; increasing
#' retries does not solve a browser challenge.
#' Automatic classification discovery (`classific = "all"`) can use official
#' aggregate metadata if SIDRA's table descriptor returns a browser challenge.
#'
#' Alternative responses are checked for complete dimension fields, textual
#' identifiers, duplicate observation keys, and membership in explicit filters.
#' Violations raise `sidrar_parse_error` subclasses and retain `primary_error`.
#' Missing explicitly selected members produce `sidrar_incomplete_warning`,
#' whose `missing` field identifies them, without adding or removing rows.
#' This warning does not prove truncation: sparse tables can legitimately omit
#' observations. No Cartesian product is required. Full coverage of `all`,
#' the exact membership of first/latest selections, and containing-level
#' geographic filters require catalog or territorial metadata comparisons and
#' are not inferred by these local checks.
#'
#' When SIDRA rejects a query for exceeding its per-request value limit,
#' `get_sidra()` raises a `sidrar_limit_error`, which also inherits from
#' `sidrar_http_error`. The condition records `requested_values`,
#' `limit_values`, and `minimum_batches`. Use [sidra_split()] to split a URL or
#' structured query across disjoint calls, or [sidra_collect()] with
#' `batch_size` for opt-in period batching and `checkpoint` for resumable
#' downloads. `get_sidra()` itself does not split requests or save values.
#'
#' The SIDRA API uses special value symbols. With the default
#' `value_type = "numeric"`, non-numeric symbols such as `"-"`, `"X"`,
#' `".."`, and `"..."` become `NA`, as in earlier versions. Use
#' `value_type = "character"` or `"both"` when those distinctions matter.
#'
#' Geographic identifiers returned by SIDRA should be kept as character
#' strings. In particular, `"Neighborhood"` (`n102`) identifiers belong to
#' SIDRA's territorial level and are not census tract identifiers; do not join
#' them directly without an official correspondence.
#'
#' @return A base `data.frame`.
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso [info_sidra()], [search_sidra()], [sidra_query()], and
#'   [sidra_collect()]
#' @examples
#' \dontrun{
#' get_sidra(
#'   x = 7060,
#'   variable = 63,
#'   period = c(last = 12),
#'   geo = "City",
#'   geo.filter = list(State = 50),
#'   classific = "c315",
#'   category = list(7169)
#' )
#'
#' get_sidra(
#'   api = "/t/7060/n1/all/v/63/p/last/c315/7169/h/n"
#' )
#' }
#' @keywords sidra IBGE
#' @export
get_sidra <- function(
  x,
  variable = "allxp",
  period = "last",
  geo = "Brazil",
  geo.filter = NULL, # nolint: object_name_linter. Legacy public argument.
  classific = "all",
  category = "all",
  header = TRUE,
  format = 4,
  digits = "default",
  api = NULL,
  value_type = c("numeric", "character", "both"),
  geo_view = NULL,
  include_extinct = FALSE
) {
  value_type <- match.arg(value_type)

  if (is.null(api)) {
    if (missing(x)) {
      stop("'x' is required when 'api' is not supplied", call. = FALSE)
    }

    query_arguments <- list(
      variable = variable,
      period = period,
      geo = geo,
      `geo.filter` = geo.filter,
      geo_view = geo_view,
      classific = classific,
      category = category
    )
    for (argument in names(query_arguments)) {
      .reject_blank_strings(query_arguments[[argument]], argument)
    }

    selected_geo <- geo
    if (!is.null(geo_view) && missing(geo)) {
      selected_geo <- NULL
    }

    request <- .build_sidra_query(
      x = x,
      variable = variable,
      period = period,
      geo = selected_geo,
      geo_filter = geo.filter,
      classific = classific,
      category = category,
      header = header,
      format = format,
      digits = digits,
      geo_view = geo_view,
      include_extinct = include_extinct
    )
  } else {
    message(
      "When 'api' is provided, query-construction arguments are ignored; ",
      "'value_type' still applies."
    )
    url <- .normalize_api_url(api)
    request <- list(url = url, header = .api_has_header(url))
  }

  response <- .sidra_values_request(request$url)
  .parse_sidra_values(
    response$text, request$header, value_type,
    response_header = response$response_header
  )
}

.parse_sidra_values <- function(
  text,
  header = TRUE,
  value_type = c("numeric", "character", "both"),
  response_header = header
) {
  value_type <- match.arg(value_type)
  parsed <- .sidra_parse_json(
    text,
    simplify = TRUE,
    context = "values response"
  )

  if (is.data.frame(parsed)) {
    result <- parsed
  } else if (is.list(parsed) && length(parsed) == 0L) {
    result <- data.frame()
  } else {
    .sidrar_abort(
      "SIDRA API returned an unexpected values structure",
      "sidrar_parse_error"
    )
  }

  if (isTRUE(response_header)) {
    if (nrow(result) == 0L) {
      .sidrar_abort(
        "SIDRA API returned no header record",
        "sidrar_parse_error"
      )
    }

    column_names <- as.character(
      unlist(result[1L, , drop = FALSE], use.names = FALSE)
    )
    if (anyNA(column_names) || any(!nzchar(column_names))) {
      .sidrar_abort(
        "SIDRA API returned an invalid header record",
        "sidrar_parse_error"
      )
    }

    if (isTRUE(header)) {
      names(result) <- column_names
    }
    result <- result[-1L, , drop = FALSE]
    row.names(result) <- NULL
  }

  value_columns <- which(names(result) %in% c("V", "Valor"))
  raw_columns <- list()

  for (index in value_columns) {
    raw <- as.character(result[[index]])
    value_name <- names(result)[[index]]

    if (identical(value_type, "character")) {
      result[[index]] <- raw
    } else {
      result[[index]] <- suppressWarnings(as.numeric(raw))

      if (identical(value_type, "both")) {
        raw_columns[[paste0(value_name, "_raw")]] <- raw
      }
    }
  }

  if (length(raw_columns) > 0L) {
    for (raw_name in names(raw_columns)) {
      unique_name <- make.unique(c(names(result), raw_name))
      unique_name <- utils::tail(unique_name, 1L)
      result[[unique_name]] <- raw_columns[[raw_name]]
    }
  }

  result
}
