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

  invalid <- geo[!geo %in% dictionary$description]
  if (length(invalid) > 0L) {
    stop(
      paste0(
        "Some element in 'geo' argument is misspecified: ",
        paste(invalid, collapse = " & ")
      ),
      call. = FALSE
    )
  }

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

  invalid_filters <- filter_names[
    !filter_names %in% dictionary$description
  ]
  if (length(invalid_filters) > 0L) {
    stop("Some element in 'geo.filter' is misspecified", call. = FALSE)
  }

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

  ifelse(startsWith(classific, "c"), classific, paste0("c", classific))
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

    return(paste0(period_names[[1L]], "%20", as.character(period[[1L]])))
  }

  if (!is.character(period)) {
    stop(
      "The 'period' argument must be an object of type character",
      call. = FALSE
    )
  }

  paste(period, collapse = ",")
}

.build_variable_path <- function(variable) {
  if ((!is.character(variable) && !is.numeric(variable)) ||
        length(variable) == 0L || anyNA(variable)) {
    stop("'variable' must be a non-empty vector", call. = FALSE)
  }

  paste(as.character(variable), collapse = ",")
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
  digits
) {
  table <- .validate_table(x)
  geo_path <- .build_geo_path(geo, geo_filter)
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

  list(url = url, header = identical(header_path, "y"))
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
