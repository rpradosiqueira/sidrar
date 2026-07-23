#' Get a SIDRA table
#'
#' Retrieves aggregate data from the Brazilian Institute of Geography and
#' Statistics (IBGE) SIDRA API.
#'
#' @param x A numeric SIDRA table code. It may be omitted when `api` is used.
#' @param variable A vector of variable codes. Defaults to `"allxp"`, which
#'   selects all variables except automatically generated percentages.
#' @param period A character vector of period codes, `"all"`, or a single
#'   named value such as `c(last = 12)` or `c(first = 5)`.
#' @param geo A character vector containing supported geographic levels.
#'   Defaults to `"Brazil"`.
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
#'
#' @details
#' Supported values of `geo` are `"Brazil"`, `"Region"`, `"State"`,
#' `"IntermediaryRegion"`, `"ImmediateRegion"`, `"MesoRegion"`,
#' `"MicroRegion"`, `"MetroRegion"`, `"MetroRegionDiv"`, `"IRD"`,
#' `"UrbAglo"`, `"PopArrang"`, `"City"`, `"District"`,
#' `"subdistrict"`, and `"Neighborhood"`.
#'
#' `format = 1` returns codes, `format = 2` returns names, `format = 3`
#' returns codes and names for geographic units plus names for other
#' descriptors, and `format = 4` returns codes and names for all descriptors.
#'
#' Requests use HTTPS, UTF-8 decoding, a timeout, and limited retries for
#' transient failures. Set `options(sidrar.timeout = 120)` or
#' `options(sidrar.retries = 4)` to override their defaults.
#'
#' The SIDRA API uses special value symbols. With the default
#' `value_type = "numeric"`, non-numeric symbols such as `"-"`, `"X"`,
#' `".."`, and `"..."` become `NA`, as in earlier versions. Use
#' `value_type = "character"` or `"both"` when those distinctions matter.
#'
#' @return A base `data.frame`.
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso [info_sidra()] and [search_sidra()]
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
  value_type = c("numeric", "character", "both")
) {
  value_type <- match.arg(value_type)

  if (is.null(api)) {
    if (missing(x)) {
      stop("'x' is required when 'api' is not supplied", call. = FALSE)
    }

    request <- .build_sidra_query(
      x = x,
      variable = variable,
      period = period,
      geo = geo,
      geo_filter = geo.filter,
      classific = classific,
      category = category,
      header = header,
      format = format,
      digits = digits
    )
  } else {
    message("All other arguments are ignored when 'api' is provided.")
    url <- .normalize_api_url(api)
    request <- list(url = url, header = .api_has_header(url))
  }

  text <- .sidra_request(request$url)
  .parse_sidra_values(text, request$header, value_type)
}

.parse_sidra_values <- function(
  text,
  header = TRUE,
  value_type = c("numeric", "character", "both")
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

  if (isTRUE(header)) {
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

    names(result) <- column_names
    result <- result[-1L, , drop = FALSE]
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
