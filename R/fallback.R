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
  ranks <- ifelse(
    grepl("^n", dimensions), 1L,
    ifelse(dimensions == "p", 2L, ifelse(dimensions == "v", 3L, 4L))
  )
  if (any(diff(ranks) < 0L)) {
    return(unavailable(
      "dimension order must be territory, period, variable, then classifications"
    ))
  }
  value <- function(key, default) {
    if (key %in% keys) values[[key]] else default
  }
  code_list <- function(x) grepl("^[0-9]+(,[0-9]+)*$", x)
  if (!grepl("^[0-9]+$", value("t", ""))) {
    return(unavailable("the table code must be numeric"))
  }
  if (!identical(tolower(value("f", "a")), "a") ||
        !identical(tolower(value("d", "s")), "s") ||
        !tolower(value("h", "y")) %in% c("y", "n")) {
    return(unavailable(
      "custom format, decimal precision, or header options cannot be translated safely"
    ))
  }

  geo_key <- keys[grepl("^n[0-9]+$", keys)]
  if (length(geo_key) != 1L) {
    return(unavailable("exactly one explicit territorial level is required"))
  }
  geo <- tolower(values[[geo_key]])
  if (code_list(geo) || identical(geo, "all")) {
    locality <- paste0(toupper(geo_key), "[", geo, "]")
  } else {
    match <- regexec("^in (n[0-9]+) ([0-9]+(?:,[0-9]+)*)$", geo, perl = TRUE)
    parts <- regmatches(geo, match)[[1L]]
    if (length(parts) != 3L) {
      return(unavailable("the territorial selection cannot be translated safely"))
    }
    locality <- paste0(
      toupper(geo_key), "[", toupper(parts[[2L]]), "[", parts[[3L]], "]]"
    )
  }

  period <- tolower(value("p", "last"))
  if (code_list(period)) {
    period <- gsub(",", "|", period, fixed = TRUE)
  } else if (identical(period, "last")) {
    period <- "-1"
  } else if (grepl("^last [1-9][0-9]*$", period)) {
    period <- paste0("-", sub("^last ", "", period))
  } else {
    return(unavailable("only explicit periods and last-period selections are supported"))
  }

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

  query <- list(localidades = locality)
  if (length(classes) > 0L) {
    query$classificacao <- paste(classes, collapse = "|")
  }
  query$view <- "flat"
  alternative <- paste0(
    .sidra_catalog_url, "/", values[["t"]],
    "/periodos/", utils::URLencode(period, reserved = TRUE),
    "/variaveis/", utils::URLencode(variable, reserved = TRUE)
  )
  list(url = httr::modify_url(alternative, query = query), reason = NULL)
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
    result
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
