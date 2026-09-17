.sidra_fallback_url <- function(url) {
  unavailable <- function(reason) list(url = NULL, reason = reason)
  if (!is.character(url) || length(url) != 1L || is.na(url) ||
        grepl("[[:cntrl:]]", url) || grepl("\\", url, fixed = TRUE)) {
    return(unavailable("the original URL is not a valid SIDRA values URL"))
  }

  parsed <- tryCatch(httr::parse_url(url), error = function(e) NULL)
  if (is.null(parsed) ||
        !identical(tolower(.scalar_text(parsed$scheme)), "https") ||
        !identical(
          tolower(.scalar_text(parsed$hostname)), "apisidra.ibge.gov.br"
        ) ||
        !is.null(parsed$username) || !is.null(parsed$password) ||
        !is.null(parsed$port) || !is.null(parsed$fragment) ||
        !is.null(parsed$params)) {
    return(unavailable(
      "the alternative requires the official HTTPS values URL without credentials, ports, or fragments"
    ))
  }

  path <- .scalar_text(parsed$path)
  if (!grepl("^values/t/", path, ignore.case = TRUE) ||
        grepl("//|/$", path) ||
        grepl("%(2f|3f|23|5c)", path, ignore.case = TRUE)) {
    return(unavailable("only complete SIDRA values paths can be translated"))
  }
  query <- parsed$query
  if (length(query) > 0L &&
        !(length(query) == 1L && identical(names(query), "formato") &&
          identical(tolower(query[[1L]]), "json"))) {
    return(unavailable("custom query parameters cannot be translated safely"))
  }

  pairs <- tryCatch(.sidra_path_pairs(url), error = function(e) NULL)
  if (is.null(pairs) || anyDuplicated(pairs$parameter)) {
    return(unavailable("incomplete or duplicated parameters cannot be translated"))
  }
  keys <- pairs$parameter
  if (!all(c("p", "v") %in% keys)) {
    return(unavailable("explicit period and variable parameters are required"))
  }
  values <- pairs$selection
  names(values) <- keys
  allowed <- keys %in% c("t", "p", "v", "f", "d", "h") |
    grepl("^[nc][0-9]+$", keys)
  if (any(!allowed)) {
    return(unavailable(paste0(
      "unsupported parameter(s): ", paste(keys[!allowed], collapse = ", ")
    )))
  }
  dimensions <- keys[grepl("^[nc][0-9]+$", keys) | keys %in% c("p", "v")]
  dimensions <- unique(sub("^n[0-9]+$", "n", dimensions))
  value <- function(key, default) {
    if (key %in% keys) values[[key]] else default
  }
  code_list <- function(x) grepl("^[0-9]+(,[0-9]+)*$", x)
  if (!grepl("^[0-9]+$", value("t", ""))) {
    return(unavailable("the table code must be numeric"))
  }
  format <- tolower(value("f", "a"))
  digits <- tolower(value("d", "s"))
  precision <- .sidra_fallback_precision(digits)
  if (!identical(format, "a") ||
        is.null(precision) || !tolower(value("h", "y")) %in% c("y", "n")) {
    return(unavailable(
      "unsupported format, decimal precision, or header option"
    ))
  }

  geo_key <- keys[grepl("^n[0-9]+$", keys)]
  if (length(geo_key) == 0L) {
    return(unavailable("at least one explicit territorial level is required"))
  }
  locality <- character(length(geo_key))
  for (i in seq_along(geo_key)) {
    geo <- tolower(values[[geo_key[[i]]]])
    if (code_list(geo) || identical(geo, "all")) {
      locality[[i]] <- paste0(toupper(geo_key[[i]]), "[", geo, "]")
    } else {
      match <- regexec("^in (n[0-9]+) ([0-9]+(?:,[0-9]+)*)$", geo, perl = TRUE)
      parts <- regmatches(geo, match)[[1L]]
      if (length(parts) != 3L) {
        return(unavailable("the territorial selection cannot be translated safely"))
      }
      locality[[i]] <- paste0(
        toupper(geo_key[[i]]), "[", toupper(parts[[2L]]), "[", parts[[3L]], "]]"
      )
    }
  }

  period <- tolower(value("p", "last"))
  if (grepl("^[0-9]+(-[0-9]+)?(,[0-9]+(-[0-9]+)?)*$", period)) {
    period <- gsub(",", "|", period, fixed = TRUE)
  } else if (identical(period, "last")) {
    period <- "-1"
  } else if (grepl("^last [1-9][0-9]*$", period)) {
    period <- paste0("-", sub("^last ", "", period))
  } else if (!(period %in% c("all", "first") ||
               grepl("^first [1-9][0-9]*$", period))) {
    return(unavailable("the period selection cannot be translated safely"))
  }
  # Native all/first selectors remain intact: never replace a complete series
  # with a fixed number of recent observations.

  variable <- tolower(value("v", "allxp"))
  if (!code_list(variable) && !variable %in% c("all", "allxp")) {
    return(unavailable("the variable selection cannot be translated safely"))
  }
  variable <- gsub(",", "|", variable, fixed = TRUE)

  class_keys <- keys[grepl("^c[0-9]+$", keys)]
  classes <- character(length(class_keys))
  for (i in seq_along(class_keys)) {
    selection <- tolower(values[[class_keys[[i]]]])
    if (!code_list(selection) && !identical(selection, "all")) {
      return(unavailable("the category selection cannot be translated safely"))
    }
    classes[[i]] <- paste0(
      substring(class_keys[[i]], 2L), "[", selection, "]"
    )
  }

  query <- list(localidades = paste(locality, collapse = "|"))
  if (length(classes) > 0L) {
    query$classificacao <- paste(classes, collapse = "|")
  }
  query$view <- "flat"
  alternative <- paste0(
    .sidra_catalog_url, "/", values[["t"]],
    "/periodos/", utils::URLencode(period, reserved = TRUE),
    "/variaveis/", utils::URLencode(variable, reserved = TRUE)
  )
  list(
    url = httr::modify_url(alternative, query = query), reason = NULL,
    dimensions = dimensions, classes = class_keys, format = format,
    precision = precision, selections = values
  )
}

.sidra_values_request <- function(url) {
  primary <- tryCatch(
    .sidra_request(url),
    sidrar_challenge_error = function(e) e
  )
  if (!inherits(primary, "sidrar_challenge_error")) {
    return(list(
      text = primary, url = url, response_header = .api_has_header(url)
    ))
  }
  if (!isTRUE(getOption("sidrar.fallback", TRUE))) {
    stop(primary)
  }

  alternative <- .sidra_fallback_url(url)
  if (is.null(alternative$url)) {
    primary$fallback_reason <- alternative$reason
    primary$message <- paste0(
      conditionMessage(primary), " Alternative API unavailable: ",
      alternative$reason, "."
    )
    stop(primary)
  }

  message(
    "SIDRA returned a Cloudflare challenge; using IBGE's official ",
    "aggregate API for this request."
  )
  text <- tryCatch({
    result <- .sidra_request(alternative$url)
    parsed <- .sidra_parse_json(
      result, simplify = TRUE, context = "alternative values response"
    )
    required <- c("NC", "NN", "MC", "MN", "V")
    if (!is.data.frame(parsed) || nrow(parsed) == 0L ||
          !all(required %in% names(parsed)) ||
          !identical(parsed$V[[1L]], "Valor")) {
      .sidrar_abort(
        "IBGE's alternative API returned an unexpected flat values header",
        "sidrar_parse_error"
      )
    }
    .parse_sidra_values(result, header = TRUE, value_type = "character")
    formatted <- .sidra_fallback_format(result, parsed, alternative)
    .sidra_fallback_validate_selection(parsed, alternative)
    formatted
  }, error = function(e) {
    e$primary_error <- primary
    e$message <- paste0(
      "IBGE's alternative API failed after a SIDRA Cloudflare challenge: ",
      conditionMessage(e)
    )
    stop(e)
  })
  list(text = text, url = alternative$url, response_header = TRUE)
}
