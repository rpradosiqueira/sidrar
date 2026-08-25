.geo_dictionary <- function() {
  data.frame(
    code = c(
      "n1", "n2", "n3", "n24", "n25", "n8", "n9", "n7",
      "n13", "n14", "n15", "n23", "n6", "n10", "n11", "n102"
    ),
    description = c(
      "Brazil", "Region", "State", "IntermediaryRegion",
      "ImmediateRegion", "MesoRegion", "MicroRegion", "MetroRegion",
      "MetroRegionDiv", "IRD", "UrbAglo", "PopArrang", "City",
      "District", "subdistrict", "Neighborhood"
    ),
    rank = seq_len(16L),
    stringsAsFactors = FALSE
  )
}

.normalize_geo_names <- function(x, argument = "geo") {
  dictionary <- .geo_dictionary()
  values <- trimws(as.character(x))
  aliases <- c(dictionary$description, dictionary$code)
  canonical <- c(dictionary$description, dictionary$description)
  index <- match(tolower(values), tolower(aliases))

  if (anyNA(index)) {
    stop(
      sprintf("Some element in '%s' is misspecified", argument),
      call. = FALSE
    )
  }

  unname(canonical[index])
}

.is_all <- function(x) {
  is.atomic(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    identical(tolower(as.character(x)), "all")
}

.collapse_codes <- function(x, argument) {
  x <- unlist(x, use.names = FALSE)
  if (length(x) == 0L) {
    return("all")
  }
  if (anyNA(x)) {
    stop(sprintf("'%s' cannot contain missing values", argument), call. = FALSE)
  }

  x <- as.character(x)
  if (any(!nzchar(x))) {
    stop(sprintf("'%s' cannot contain empty values", argument), call. = FALSE)
  }
  .reject_url_delimiters(x, argument)

  gsub(" ", "%20", paste(x, collapse = ","), fixed = TRUE)
}

.build_geo_path <- function(geo, geo_filter = NULL) {
  dictionary <- .geo_dictionary()

  if (is.null(geo)) {
    geo <- "Brazil"
  }
  if (!is.character(geo) || length(geo) == 0L || anyNA(geo)) {
    stop("'geo' must be a non-empty character vector", call. = FALSE)
  }

  geo <- .normalize_geo_names(geo)

  if (length(geo) == 1L && identical(geo, "Brazil")) {
    if (!is.null(geo_filter)) {
      message(
        "No filter is necessary in 'geo.filter' argument once ",
        "'geo' is set to 'Brazil' (default)"
      )
    }
    return("n1/1")
  }

  if (is.null(geo_filter)) {
    filters <- vector("list", length(geo))
  } else if (is.list(geo_filter)) {
    filters <- geo_filter
  } else {
    filters <- as.list(geo_filter)
  }

  if (length(filters) > length(geo)) {
    stop(
      "The geo.filter argument must have the same or less length than 'geo'",
      call. = FALSE
    )
  }

  filter_names <- names(filters)
  if (is.null(filter_names)) {
    filter_names <- rep("", length(filters))
  }

  missing_filters <- length(geo) - length(filters)
  if (missing_filters > 0L) {
    filters <- c(filters, rep(list("all"), missing_filters))
    filter_names <- c(filter_names, rep("", missing_filters))
  }

  for (i in seq_along(geo)) {
    if (is.null(filters[[i]]) || length(filters[[i]]) == 0L) {
      filters[[i]] <- "all"
    }
    if (!nzchar(filter_names[[i]])) {
      filter_names[[i]] <- geo[[i]]
    }
  }

  filter_names <- .normalize_geo_names(filter_names, "geo.filter")

  geo_index <- match(geo, dictionary$description)
  filter_index <- match(filter_names, dictionary$description)
  if (any(dictionary$rank[filter_index] > dictionary$rank[geo_index])) {
    stop("Some element in 'geo.filter' is misspecified", call. = FALSE)
  }

  paths <- character(length(geo))
  for (i in seq_along(geo)) {
    values <- .collapse_codes(filters[[i]], "geo.filter")
    geo_code <- dictionary$code[[geo_index[[i]]]]
    filter_code <- dictionary$code[[filter_index[[i]]]]

    if (identical(geo_code, filter_code)) {
      paths[[i]] <- paste0(geo_code, "/", values)
    } else {
      paths[[i]] <- paste0(
        geo_code, "/in%20", filter_code, "%20", values
      )
    }
  }

  paste(paths, collapse = "/")
}

.build_geo_view_path <- function(geo_view) {
  if (length(geo_view) != 1L || is.na(geo_view) || !is.atomic(geo_view)) {
    stop("'geo_view' must identify exactly one territorial view", call. = FALSE)
  }

  value <- trimws(tolower(as.character(geo_view)))
  value <- sub("^g", "", value)
  if (!nzchar(value) || !grepl("^[0-9]+$", value)) {
    stop(
      "'geo_view' must be a numeric SIDRA territorial view code",
      call. = FALSE
    )
  }

  paste0("g/", value)
}

.build_extinct_path <- function(include_extinct) {
  if (!is.logical(include_extinct) || length(include_extinct) != 1L ||
        is.na(include_extinct)) {
    stop("'include_extinct' must be either TRUE or FALSE", call. = FALSE)
  }

  if (include_extinct) "/u/y" else ""
}

.build_territory_path <- function(
  geo,
  geo_filter = NULL,
  geo_view = NULL,
  include_extinct = FALSE
) {
  extinct_path <- .build_extinct_path(include_extinct)

  if (!is.null(geo_view)) {
    if (!is.null(geo) || !is.null(geo_filter)) {
      stop(
        "'geo_view' is mutually exclusive with 'geo' and 'geo.filter'",
        call. = FALSE
      )
    }
    if (include_extinct) {
      stop(
        "'include_extinct' is only available for 'geo' (N) selections",
        call. = FALSE
      )
    }

    return(.build_geo_view_path(geo_view))
  }

  paste0(.build_geo_path(geo, geo_filter), extinct_path)
}

.normalize_classifications <- function(classific) {
  if ((!is.character(classific) && !is.numeric(classific)) ||
        length(classific) == 0L || anyNA(classific)) {
    stop("'classific' must be a non-empty vector", call. = FALSE)
  }

  classific <- tolower(as.character(classific))
  valid <- grepl("^c?[0-9]+$", classific)
  if (any(!valid)) {
    stop("Some element in 'classific' is misspecified", call. = FALSE)
  }

  classific <- ifelse(
    startsWith(classific, "c"),
    classific,
    paste0("c", classific)
  )
  if (anyDuplicated(classific)) {
    stop("'classific' cannot contain duplicates", call. = FALSE)
  }
  classific
}

.build_classification_path <- function(classific, category = "all") {
  if (length(classific) == 0L) {
    return("")
  }

  classific <- .normalize_classifications(classific)

  if (is.null(category) || .is_all(category)) {
    categories <- rep(list("all"), length(classific))
  } else {
    if (!is.list(category)) {
      stop(
        "If not 'all', 'category' must be an object of type 'list'",
        call. = FALSE
      )
    }
    if (length(category) > length(classific)) {
      stop(
        paste(
          "The length of 'category' must be equal or less than",
          "'classific' argument"
        ),
        call. = FALSE
      )
    }

    categories <- category
    if (length(categories) < length(classific)) {
      categories <- c(
        categories,
        rep(list("all"), length(classific) - length(categories))
      )
    }
  }

  selections <- vapply(
    categories,
    .collapse_codes,
    character(1),
    argument = "category"
  )

  paste0("/", paste0(classific, "/", selections, collapse = "/"))
}

.descriptor_classifications <- function(descriptor) {
  classifications <- descriptor$Classificacoes
  if (is.null(classifications) || length(classifications) == 0L) {
    return(character())
  }

  vapply(
    classifications,
    function(x) paste0("c", .scalar_text(x$Id)),
    character(1)
  )
}

.resolve_classification_path <- function(table, classific, category) {
  if (is.null(classific) || .is_all(classific)) {
    if (!is.null(category) && !.is_all(category)) {
      message(
        "Considering all categories once 'classific' was set to ",
        "'all' (default)"
      )
    }

    descriptor <- .fetch_descriptor(table)
    classific <- .descriptor_classifications(descriptor)
    return(.build_classification_path(classific, "all"))
  }

  .build_classification_path(classific, category)
}

.build_period_path <- function(period) {
  if (length(period) == 0L || anyNA(period)) {
    stop("'period' must not be empty or contain missing values", call. = FALSE)
  }

  period_names <- names(period)
  has_names <- !is.null(period_names) && any(nzchar(period_names))

  if (has_names) {
    if (length(period) != 1L) {
      stop(
        paste(
          "Only one element is possible when a named vector",
          "('last' or 'first') is present"
        ),
        call. = FALSE
      )
    }
    if (!period_names[[1L]] %in% c("last", "first")) {
      stop(
        "The element's 'name' attribute must be 'last' or 'first'",
        call. = FALSE
      )
    }

    value <- as.character(period[[1L]])
    .reject_url_delimiters(value, "period")
    return(paste0(period_names[[1L]], "%20", value))
  }

  if (!is.character(period)) {
    stop(
      "The 'period' argument must be an object of type character",
      call. = FALSE
    )
  }

  .reject_url_delimiters(period, "period")

  paste(period, collapse = ",")
}

.build_variable_path <- function(variable) {
  if ((!is.character(variable) && !is.numeric(variable)) ||
        length(variable) == 0L || anyNA(variable)) {
    stop("'variable' must be a non-empty vector", call. = FALSE)
  }

  variable <- as.character(variable)
  .reject_url_delimiters(variable, "variable")
  paste(variable, collapse = ",")
}

.build_header_path <- function(header) {
  if (!is.logical(header) || length(header) != 1L || is.na(header)) {
    stop("'header' must be either TRUE or FALSE", call. = FALSE)
  }

  if (header) "y" else "n"
}

.build_format_path <- function(format) {
  format_map <- c("1" = "c", "2" = "n", "3" = "u", "4" = "a")
  value <- if (is.null(format)) {
    "4"
  } else if (length(format) == 1L && !is.na(format)) {
    as.character(format)
  } else {
    ""
  }

  if (!value %in% names(format_map)) {
    warning(
      "The format argument is misspecified. Considering default specification.",
      call. = FALSE
    )
    value <- "4"
  }

  paste0("/f/", unname(format_map[[value]]))
}

.build_digits_path <- function(digits) {
  value <- if (is.null(digits)) {
    "default"
  } else if (length(digits) == 1L && !is.na(digits)) {
    as.character(digits)
  } else {
    ""
  }

  if (identical(value, "default")) {
    return("/d/s")
  }
  if (identical(value, "max")) {
    return("/d/m")
  }
  if (value %in% as.character(0:9)) {
    return(paste0("/d/", value))
  }

  warning(
    "The digits argument is misspecified. Considering default specification.",
    call. = FALSE
  )
  "/d/s"
}

.build_sidra_query <- function(
  x,
  variable,
  period,
  geo,
  geo_filter,
  classific,
  category,
  header,
  format,
  digits,
  geo_view = NULL,
  include_extinct = FALSE
) {
  table <- .validate_table(x)
  geo_path <- .build_territory_path(
    geo = geo,
    geo_filter = geo_filter,
    geo_view = geo_view,
    include_extinct = include_extinct
  )
  period_path <- .build_period_path(period)
  variable_path <- .build_variable_path(variable)
  classification_path <- .resolve_classification_path(
    table, classific, category
  )
  header_path <- .build_header_path(header)

  url <- paste0(
    .sidra_values_base,
    "/t/", table,
    "/", geo_path,
    "/p/", period_path,
    "/v/", variable_path,
    classification_path,
    .build_format_path(format),
    "/h/", header_path,
    .build_digits_path(digits)
  )
  .validate_sidra_url_semantics(url)

  list(
    url = url,
    header = identical(header_path, "y"),
    parameters = list(
      table = table,
      variable = variable,
      period = period,
      geo = geo,
      geo_filter = geo_filter,
      geo_view = geo_view,
      include_extinct = include_extinct,
      classific = classific,
      category = category,
      header = header,
      format = format,
      digits = digits
    )
  )
}

#' Build a SIDRA query without downloading values
#'
#' Constructs and validates a SIDRA values URL. This is useful for inspecting
#' a request or passing it to [sidra_plan()] before any values are downloaded.
#'
#' @param x A numeric SIDRA table code. It may be omitted when `api` is used.
#' @param variable A vector of variable codes. The special selections `"all"`
#'   and `"allxp"` are also accepted.
#' @param period A character vector of period codes, `"all"`, or a single
#'   named value such as `c(last = 12)` or `c(first = 5)`.
#' @param geo A character vector with geographic aliases or `nNN` level codes.
#'   Aliases and codes are case-insensitive.
#' @param geo.filter A list of geographic filters corresponding to `geo`.
#'   Names may be aliases or `nNN` codes and are case-insensitive.
#' @param classific A vector of classification codes.
#' @param category `"all"` or a list of categories for each classification.
#' @param header Logical. Should the API include its header record?
#' @param format An integer from 1 to 4 controlling descriptor fields.
#' @param digits `"default"`, `"max"`, or an integer from 0 to 9.
#' @param api A relative SIDRA API path or complete official values URL. When
#'   supplied, the other URL-building arguments are ignored.
#' @param value_type Preferred value representation for a later collection:
#'   `"numeric"`, `"character"`, or `"both"`. It does not change the URL.
#' @param geo_view Optional numeric SIDRA territorial-view code. Territorial
#'   views (`G`) cannot be combined with `geo` or `geo.filter` (`N`).
#' @param include_extinct Logical. Add `/u/y` to an `N` query so extinct
#'   territorial units may be returned. It cannot be used with `geo_view`.
#'
#' @return A list of class `sidra_query` with stable `url`, `header`, and
#'   `parameters` components. No values are downloaded.
#' @seealso [get_sidra()] and [sidra_plan()]
#' @examples
#' query <- sidra_query(
#'   1612,
#'   variable = 214,
#'   period = "2021",
#'   geo = "n1",
#'   classific = "c81",
#'   category = list(2702)
#' )
#' query
#' @export
sidra_query <- function(
  x,
  variable = "allxp",
  period = "last",
  geo = "Brazil",
  geo.filter = NULL, # nolint: object_name_linter. Matches get_sidra().
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
      classific = classific,
      category = category,
      geo_view = geo_view
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
    request$parameters$value_type <- value_type
  } else {
    url <- .normalize_api_url(api)
    request <- list(
      url = url,
      header = .api_has_header(url),
      parameters = list(api = url, value_type = value_type)
    )
  }

  structure(request, class = c("sidra_query", "list"))
}

#' @export
print.sidra_query <- function(x, ...) {
  cat(
    "<sidra_query>\n",
    "URL: ", x$url, "\n",
    "Header: ", if (isTRUE(x$header)) "yes" else "no", "\n",
    sep = ""
  )
  invisible(x)
}

.normalize_api_url <- function(api) {
  if (!is.character(api) || length(api) != 1L || is.na(api)) {
    stop(
      "The 'api' argument must be a character vector of length 1",
      call. = FALSE
    )
  }

  api <- trimws(api)
  if (!nzchar(api)) {
    stop("The 'api' argument must not be empty", call. = FALSE)
  }
  if (grepl("%(2f|3f|23|5c)", api, ignore.case = TRUE, perl = TRUE)) {
    stop("'api' cannot contain encoded URL delimiters", call. = FALSE)
  }

  if (grepl("^https?://", api, ignore.case = TRUE)) {
    parsed <- httr::parse_url(api)
    path <- sub("^/+", "", .scalar_text(parsed$path))

    if (!identical(tolower(.scalar_text(parsed$scheme)), "https") ||
      !identical(
        tolower(.scalar_text(parsed$hostname)),
        "apisidra.ibge.gov.br"
      ) ||
      !grepl("^values/t/", path, ignore.case = TRUE)) {
      stop(
        "'api' must use the official HTTPS SIDRA values endpoint",
        call. = FALSE
      )
    }
    if (!is.null(parsed$query$formato) &&
          !identical(tolower(parsed$query$formato), "json")) {
      stop("Only JSON SIDRA responses are supported", call. = FALSE)
    }
    if (!is.null(parsed$fragment) && nzchar(parsed$fragment)) {
      stop("'api' must not contain a URL fragment", call. = FALSE)
    }

    .validate_sidra_url_semantics(api)
    return(api)
  }

  api <- sub("^/+", "", api)
  api <- sub("^values/+", "", api, ignore.case = TRUE)
  if (!grepl("^t/", api, ignore.case = TRUE)) {
    stop("'api' must start with a SIDRA table parameter ('t/')", call. = FALSE)
  }

  url <- paste0(.sidra_values_base, "/", api)
  parsed <- httr::parse_url(url)
  if (!is.null(parsed$query$formato) &&
        !identical(tolower(parsed$query$formato), "json")) {
    stop("Only JSON SIDRA responses are supported", call. = FALSE)
  }

  .validate_sidra_url_semantics(url)
  url
}

.api_has_header <- function(url) {
  path <- .scalar_text(httr::parse_url(url)$path)
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  header_index <- which(tolower(parts) == "h")

  if (length(header_index) == 0L) {
    return(TRUE)
  }

  index <- utils::tail(header_index, 1L)
  if (index >= length(parts)) {
    return(TRUE)
  }

  !identical(tolower(parts[[index + 1L]]), "n")
}

.validate_sidra_url_semantics <- function(url) {
  pairs <- .sidra_path_pairs(url)
  has_n <- any(grepl("^n[0-9]+$", pairs$parameter))
  has_g <- any(pairs$parameter == "g")
  has_extinct <- any(
    pairs$parameter == "u" & tolower(trimws(pairs$selection)) == "y"
  )
  classifications <- pairs$parameter[
    grepl("^c[0-9]+$", pairs$parameter)
  ]

  if (has_n && has_g) {
    stop(
      "SIDRA query cannot combine territorial views ('g') with levels ('nNN')",
      call. = FALSE
    )
  }
  if (has_g && has_extinct) {
    stop(
      "SIDRA query cannot combine territorial views ('g') with extinct units ('u/y')",
      call. = FALSE
    )
  }
  if (anyDuplicated(classifications)) {
    stop("SIDRA query cannot contain duplicate classifications", call. = FALSE)
  }

  invisible(url)
}
