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

.sidra_response_header <- function(response, name) {
  headers <- httr::headers(response)
  index <- match(tolower(name), tolower(names(headers)))
  if (is.na(index)) {
    return("")
  }
  trimws(.scalar_text(headers[[index]]))
}

.sidra_is_challenge <- function(response, body) {
  mitigation <- .sidra_response_header(response, "cf-mitigated")
  if (identical(tolower(mitigation), "challenge")) {
    return(TRUE)
  }

  html_root <- grepl(
    "^\\s*(?:<!doctype\\s+html(?:\\s|>)|<html(?:\\s|>))",
    body,
    ignore.case = TRUE,
    perl = TRUE
  )
  if (!html_root) {
    return(FALSE)
  }

  explicit_marker <- grepl(
    "/cdn-cgi/challenge-platform/|\\b_cf_chl_opt\\b",
    body,
    perl = TRUE
  )
  if (explicit_marker) {
    return(TRUE)
  }

  # Some challenge pages expose only their title and CSP/script host. Neither
  # a generic Cloudflare error page nor a Turnstile script alone is sufficient.
  challenge_title <- grepl(
    paste0(
      "<title(?:\\s[^>]*)?>\\s*",
      "(?:just\\s+a\\s+moment|checking\\s+your\\s+browser)",
      "[.\u2026\\s]*</title\\s*>"
    ),
    body,
    ignore.case = TRUE,
    perl = TRUE
  )
  challenge_host <- grepl(
    paste0(
      "(?:https?:)?//challenges\\.cloudflare\\.com",
      "(?=[/?#\\s\\\"'<>;]|$)"
    ),
    body,
    ignore.case = TRUE,
    perl = TRUE
  )

  challenge_title && challenge_host
}

.sidra_retry_now <- function() {
  Sys.time()
}

.sidra_retry_header <- function(response, name) {
  headers <- httr::headers(response)
  index <- which(tolower(names(headers)) == tolower(name))
  if (length(index) != 1L) {
    return("")
  }
  value <- headers[[index]]
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return("")
  }
  trimws(value)
}

.sidra_retry_date <- function(value) {
  # Restrict the parser to HTTP-date syntax: parse_http_date() also accepts
  # trailing text. Its implementation sets the C locale while parsing.
  weekday <- "(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)"
  month <- "(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
  clock <- "[0-9]{2}:[0-9]{2}:[0-9]{2}"
  formats <- c(
    paste0("^", weekday, ", [0-9]{2} ", month, " [0-9]{4} ", clock, " GMT$"),
    paste0(
      "^(?:Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday), ",
      "[0-9]{2}-", month, "-[0-9]{2} ", clock, " GMT$"
    ),
    paste0("^", weekday, " ", month, " [ 0-9][0-9] ", clock, " [0-9]{4}$")
  )
  if (!any(vapply(formats, grepl, logical(1), x = value, perl = TRUE))) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(httr::parse_http_date(value)))
}

.sidra_retry_after <- function(response, now = .sidra_retry_now()) {
  value <- .sidra_retry_header(response, "retry-after")
  if (!nzchar(value)) {
    return(NA_real_)
  }
  if (grepl("^[0-9]+$", value)) {
    # Overflow remains Inf and is handled as an excessive wait, never as an
    # invalid header that would permit another request after a short backoff.
    return(suppressWarnings(as.numeric(value)))
  }

  date <- .sidra_retry_date(value)
  if (!is.finite(date)) {
    return(NA_real_)
  }
  delay <- date - as.numeric(now)
  server_date <- .sidra_retry_date(.sidra_retry_header(response, "date"))
  if (is.finite(server_date)) {
    # A local clock ahead of the server must not cause an early retry.
    delay <- max(delay, date - server_date)
  }
  max(0, delay)
}

.sidra_retry_pause <- function(attempt) {
  max(0.5, stats::runif(1L, max = min(4, 0.5 * 2^attempt)))
}

.sidra_retry_sleep <- function(seconds) {
  Sys.sleep(seconds)
}

.sidra_request <- function(url) {
  timeout <- getOption("sidrar.timeout", 60)
  retries <- getOption("sidrar.retries", 3L)
  retry_after_max <- getOption("sidrar.retry_after_max", 60)

  if (length(timeout) != 1L || is.na(timeout) ||
        !is.numeric(timeout) || !is.finite(timeout) || timeout <= 0) {
    timeout <- 60
  }
  if (length(retries) != 1L || is.na(retries) ||
        !is.numeric(retries) || !is.finite(retries) || retries < 1 ||
        retries != floor(retries) || retries > .Machine$integer.max) {
    retries <- 3L
  }
  if (!is.numeric(retry_after_max) || length(retry_after_max) != 1L ||
        is.na(retry_after_max) || !is.finite(retry_after_max) ||
        retry_after_max <= 0) {
    retry_after_max <- 60
  }
  retry_after_max <- as.double(retry_after_max)

  retry_after <- NA_real_
  for (attempt in seq_len(as.integer(retries))) {
    # httr 1.4.x only honors Retry-After when quiet = FALSE, and only for 429.
    # Single attempts let us remain silent and safely handle both HTTP-date
    # and delay-seconds without changing httr internals or TLS configuration.
    response <- tryCatch(
      httr::RETRY(
        "GET",
        url,
        httr::accept_json(),
        httr::user_agent(.sidrar_user_agent()),
        httr::timeout(timeout),
        times = 1L,
        quiet = TRUE
      ),
      error = identity
    )
    if (inherits(response, "error")) {
      if (attempt >= retries) {
        .sidrar_abort(
          paste0("SIDRA request failed: ", conditionMessage(response)),
          c(.sidra_transport_classes(response), "sidrar_http_error"),
          status_code = NA_integer_,
          response_body = "",
          url = url
        )
      }
      .sidra_retry_sleep(.sidra_retry_pause(attempt))
      next
    }

    body <- tryCatch(
      httr::content(response, as = "text", encoding = "UTF-8"),
      error = function(e) ""
    )
    status <- httr::status_code(response)
    retry_after <- .sidra_retry_after(response)
    if (.sidra_is_challenge(response, body) ||
          !httr::http_error(response) ||
          status %in% setdiff(400:499, c(408, 425, 429)) ||
          attempt >= retries) {
      break
    }

    # Respect the configured upper bound for a server-requested delay.
    # Abort instead of capping the sleep, which would retry too early.
    if (!is.na(retry_after) && retry_after > retry_after_max) {
      .sidrar_abort(
        sprintf(
          paste0(
            "SIDRA API request failed (HTTP %s): Retry-After requests ",
            "%s seconds, exceeding the %s-second automatic wait limit ",
            "(option 'sidrar.retry_after_max'). ",
            "No further request was sent; try again after that interval."
          ),
          status,
          format(retry_after, scientific = FALSE, trim = TRUE),
          format(retry_after_max, scientific = FALSE, trim = TRUE)
        ),
        c("sidrar_retry_after_error", "sidrar_http_error"),
        status_code = status,
        response_body = body,
        url = url,
        retry_after = retry_after,
        retry_after_max = retry_after_max,
        retry_after_header = .sidra_retry_header(response, "retry-after"),
        attempts = attempt
      )
    }
    delay <- .sidra_retry_pause(attempt)
    if (!is.na(retry_after)) {
      delay <- max(delay, retry_after)
    }
    .sidra_retry_sleep(delay)
  }

  if (.sidra_is_challenge(response, body)) {
    cf_ray <- .sidra_response_header(response, "cf-ray")
    if (!nzchar(cf_ray)) {
      cf_ray <- NULL
    }
    .sidrar_abort(
      paste0(
        sprintf(
          "SIDRA API returned a Cloudflare browser challenge (HTTP %s). ",
          status
        ),
        "sidrar cannot complete this interactive check. ",
        "Contact IBGE with the request URL",
        if (is.null(cf_ray)) "." else paste0(" and Ray ID: ", cf_ray, ".")
      ),
      c("sidrar_challenge_error", "sidrar_http_error"),
      status_code = status,
      response_body = body,
      url = url,
      cf_ray = cf_ray
    )
  }

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
      url = url,
      retry_after = if (is.na(retry_after)) NULL else retry_after,
      retry_after_header = .sidra_retry_header(response, "retry-after"),
      attempts = attempt
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
