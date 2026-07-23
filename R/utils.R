.sidra_values_base <- "https://apisidra.ibge.gov.br/values"
.sidra_descriptor_base <- "https://apisidra.ibge.gov.br/DescritoresTabela/t"
.sidra_descriptor_html_base <- "https://apisidra.ibge.gov.br/desctabapi.aspx?c="
.sidra_catalog_url <- "https://servicodados.ibge.gov.br/api/v3/agregados"

.sidrar_abort <- function(message, class = "sidrar_error") {
  condition <- structure(
    list(message = message, call = NULL),
    class = unique(c(class, "sidrar_error", "error", "condition"))
  )
  stop(condition)
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

.sidra_request <- function(url) {
  timeout <- getOption("sidrar.timeout", 60)
  retries <- getOption("sidrar.retries", 3L)

  if (length(timeout) != 1L || is.na(timeout) ||
        !is.numeric(timeout) || !is.finite(timeout) || timeout <= 0) {
    timeout <- 60
  }
  if (length(retries) != 1L || is.na(retries) ||
        !is.numeric(retries) || !is.finite(retries) || retries < 1) {
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
        "sidrar_http_error"
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
    if (nchar(detail) > 500L) {
      detail <- paste0(substr(detail, 1L, 497L), "...")
    }

    .sidrar_abort(
      sprintf("SIDRA API request failed (HTTP %s): %s", status, detail),
      "sidrar_http_error"
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
