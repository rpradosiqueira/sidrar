.sidra_values_base <- "https://apisidra.ibge.gov.br/values"
.sidra_descriptor_base <- "https://apisidra.ibge.gov.br/DescritoresTabela/t"
.sidra_descriptor_html_base <- "https://apisidra.ibge.gov.br/desctabapi.aspx?c="
.sidra_catalog_url <- "https://servicodados.ibge.gov.br/api/v3/agregados"

.sidrar_abort <- function(message, class = "sidrar_error", ...) {
  condition <- structure(
    c(list(message = message, call = NULL), list(...)),
    class = unique(c(class, "sidrar_error", "error", "condition"))
  )
  stop(condition)
}

.sidra_value_limit <- function(status, detail) {
  if (length(status) != 1L || is.na(status) || status != 400L ||
        !is.character(detail) || length(detail) != 1L || is.na(detail)) {
    return(NULL)
  }

  pattern <- paste0(
    "quantidade\\s+de\\s+valores\\s+solicitados\\s*:\\s*",
    "([0-9]+)\\s+excedeu\\s+o\\s+limite\\s*:\\s*([0-9]+)\\b"
  )
  match <- regexec(
    pattern,
    enc2utf8(detail),
    ignore.case = TRUE,
    perl = TRUE
  )
  values <- regmatches(enc2utf8(detail), match)[[1L]]

  if (length(values) != 3L) {
    return(NULL)
  }

  requested <- suppressWarnings(as.numeric(values[[2L]]))
  limit <- suppressWarnings(as.numeric(values[[3L]]))
  if (!is.finite(requested) || !is.finite(limit) || limit <= 0 ||
        requested <= limit) {
    return(NULL)
  }

  list(
    requested_values = requested,
    limit_values = limit,
    minimum_batches = ceiling(requested / limit)
  )
}

.scalar_text <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) {
    return(default)
  }

  as.character(x[[1L]])
}

.validate_table <- function(x) {
  if (length(x) != 1L || is.na(x) || !is.atomic(x)) {
    stop("'x' must identify exactly one SIDRA table", call. = FALSE)
  }

  x <- trimws(as.character(x))
  if (!nzchar(x) || !grepl("^[0-9]+$", x)) {
    stop("'x' must be a numeric SIDRA table code", call. = FALSE)
  }

  x
}

.sidrar_user_agent <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("sidrar")),
    error = function(e) "development"
  )

  paste0(
    "sidrar/", version,
    " (https://github.com/rpradosiqueira/sidrar)"
  )
}

.sidra_transport_classes <- function(error) {
  signature <- tolower(paste(
    c(class(error), conditionMessage(error)),
    collapse = " "
  ))
  matches <- function(pattern) {
    grepl(pattern, signature, perl = TRUE)
  }

  classes <- character()
  is_timeout <- matches(
    "operation_timedout|timeout|timed\\s*out"
  )
  is_tls <- matches(
    paste0(
      "ssl|tls|schannel|certificate|certproblem|",
      "peer_failed_verification"
    )
  )
  is_dns <- matches(
    paste0(
      "couldnt_resolve_host|could not resolve host|failed to resolve|",
      "name or service not known|no such host|dns|getaddrinfo|",
      "temporary failure in name resolution"
    )
  )
  is_connection <- matches(
    paste0(
      "couldnt_connect|failed to connect|could not connect|",
      "connection (reset|refused|aborted|closed)|recv failure|",
      "send failure|empty reply|got nothing|network is unreachable"
    )
  )
  is_transient <- matches(
    paste0(
      "temporar(?:y|ily)|try again|operation_timedout|timeout|",
      "timed\\s*out|connection (reset|aborted)|recv failure|",
      "send failure"
    )
  )

  if (is_timeout) {
    classes <- c(classes, "sidrar_timeout_error")
  }
  if (is_tls) {
    classes <- c(classes, "sidrar_tls_error")
  }
  if (is_dns) {
    classes <- c(classes, "sidrar_dns_error")
  }
  if (is_connection) {
    classes <- c(classes, "sidrar_connection_error")
  }
  if (is_transient) {
    classes <- c(classes, "sidrar_transient_error")
  }

  unique(classes)
}

.contains_blank_string <- function(x) {
  if (is.character(x)) {
    return(any(!is.na(x) & !nzchar(trimws(x))))
  }
  if (is.list(x)) {
    return(any(vapply(x, .contains_blank_string, logical(1))))
  }

  FALSE
}

.reject_blank_strings <- function(x, argument) {
  if (.contains_blank_string(x)) {
    stop(
      sprintf(
        "'%s' cannot contain empty or whitespace-only values",
        argument
      ),
      call. = FALSE
    )
  }

  invisible(x)
}

.contains_url_delimiter <- function(x) {
  if (is.character(x)) {
    return(any(
      grepl("[/?#%]", x) |
        grepl("\\", x, fixed = TRUE) |
        grepl("[[:cntrl:]]", x)
    ))
  }
  if (is.list(x)) {
    return(any(vapply(x, .contains_url_delimiter, logical(1))))
  }

  FALSE
}

.reject_url_delimiters <- function(x, argument) {
  if (.contains_url_delimiter(x)) {
    stop(
      sprintf("'%s' cannot contain reserved URL delimiters", argument),
      call. = FALSE
    )
  }

  invisible(x)
}

.sidra_request <- function(url) {
  timeout <- getOption("sidrar.timeout", 60)
  retries <- getOption("sidrar.retries", 3L)

  if (length(timeout) != 1L || is.na(timeout) ||
        !is.numeric(timeout) || !is.finite(timeout) || timeout <= 0) {
    timeout <- 60
  }
  if (length(retries) != 1L || is.na(retries) ||
        !is.numeric(retries) || !is.finite(retries) || retries < 1 ||
        retries != floor(retries) || retries > .Machine$integer.max) {
    retries <- 3L
  }

  response <- tryCatch(
    httr::RETRY(
      "GET",
      url,
      httr::accept_json(),
      httr::user_agent(.sidrar_user_agent()),
      httr::timeout(timeout),
      times = as.integer(retries),
      pause_base = 0.5,
      pause_min = 0.5,
      pause_cap = 4,
      quiet = TRUE,
      terminate_on = setdiff(400:499, c(408, 425, 429))
    ),
    error = function(e) {
      .sidrar_abort(
        paste0("SIDRA request failed: ", conditionMessage(e)),
        c(.sidra_transport_classes(e), "sidrar_http_error"),
        status_code = NA_integer_,
        response_body = "",
        url = url
      )
    }
  )

  body <- tryCatch(
    httr::content(response, as = "text", encoding = "UTF-8"),
    error = function(e) ""
  )
  status <- httr::status_code(response)

  if (httr::http_error(response)) {
    detail <- trimws(gsub("[\r\n]+", " ", body))
    if (!nzchar(detail)) {
      detail <- httr::http_status(response)$message
    }
    limit <- .sidra_value_limit(status, detail)
    display_detail <- detail
    if (nchar(display_detail) > 500L) {
      display_detail <- paste0(substr(display_detail, 1L, 497L), "...")
    }

    if (!is.null(limit)) {
      .sidrar_abort(
        paste0(
          sprintf(
            paste0(
              "SIDRA API request failed (HTTP %s): requested %s values, ",
              "exceeding the limit of %s. "
            ),
            status,
            format(limit$requested_values, scientific = FALSE, trim = TRUE),
            format(limit$limit_values, scientific = FALSE, trim = TRUE)
          ),
          "Split 'period', 'geo.filter', 'variable', or 'category' ",
          "into at least ", limit$minimum_batches,
          " disjoint calls and combine the returned rows. API response: ",
          display_detail
        ),
        c("sidrar_limit_error", "sidrar_http_error"),
        status_code = status,
        requested_values = limit$requested_values,
        limit_values = limit$limit_values,
        minimum_batches = limit$minimum_batches,
        suggested_arguments = c(
          "period", "geo.filter", "variable", "category"
        ),
        response_body = body,
        url = url
      )
    }

    .sidrar_abort(
      sprintf(
        "SIDRA API request failed (HTTP %s): %s",
        status,
        display_detail
      ),
      "sidrar_http_error",
      status_code = status,
      response_body = body,
      url = url
    )
  }

  if (!nzchar(body)) {
    .sidrar_abort(
      "SIDRA API returned an empty response",
      "sidrar_parse_error"
    )
  }

  body
}

.sidra_parse_json <- function(text, simplify = FALSE, context = "response") {
  if (!is.character(text) || length(text) != 1L || is.na(text) ||
        !nzchar(trimws(text))) {
    .sidrar_abort(
      paste0("SIDRA API returned an empty ", context),
      "sidrar_parse_error"
    )
  }

  tryCatch(
    jsonlite::fromJSON(
      text,
      simplifyVector = simplify,
      simplifyDataFrame = simplify,
      simplifyMatrix = simplify
    ),
    error = function(e) {
      preview <- trimws(gsub("[\r\n]+", " ", text))
      if (nchar(preview) > 200L) {
        preview <- paste0(substr(preview, 1L, 197L), "...")
      }

      .sidrar_abort(
        paste0(
          "SIDRA API returned an invalid ", context, ": ",
          preview
        ),
        "sidrar_parse_error"
      )
    }
  )
}

.fetch_descriptor <- function(x) {
  table <- .validate_table(x)
  text <- .sidra_request(paste0(.sidra_descriptor_base, "/", table))
  .sidra_parse_json(text, simplify = FALSE, context = "table descriptor")
}

.fetch_aggregate_catalog <- function() {
  text <- .sidra_request(.sidra_catalog_url)
  .sidra_parse_json(text, simplify = FALSE, context = "aggregate catalog")
}

.open_sidra_descriptor <- function(url) {
  utils::browseURL(url)
}
