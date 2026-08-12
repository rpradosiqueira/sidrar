#' Search SIDRA tables
#'
#' Searches table titles in the official IBGE aggregate catalog.
#'
#' @param x A non-empty character vector containing the search terms.
#'
#' @details Matching is case- and accent-insensitive. When `x` contains
#'   multiple terms, every term must occur in the title, but the terms need
#'   not be adjacent. The catalog is requested live and is not cached by the
#'   package. The timeout and retry options described in [get_sidra()] also
#'   apply.
#'
#' @return A named character vector with matching SIDRA table titles. Names
#'   are the table codes. Returns `character(0)` when there are no matches.
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso [get_sidra()] and [info_sidra()]
#' @examples
#' \dontrun{
#' search_sidra("contas nacionais")
#' search_sidra("IPCA")
#' }
#' @keywords sidra IBGE
#' @export
search_sidra <- function(x) {
  if (!is.character(x) || length(x) == 0L || anyNA(x)) {
    stop("'x' must be a non-empty character vector", call. = FALSE)
  }

  x <- trimws(x)
  if (any(!nzchar(x))) {
    stop("'x' must not contain empty search terms", call. = FALSE)
  }

  catalog <- .fetch_aggregate_catalog()
  matches <- unlist(
    lapply(catalog, function(group) {
      aggregates <- group$agregados
      if (is.null(aggregates) || length(aggregates) == 0L) {
        return(list())
      }
      lapply(
        aggregates,
        function(aggregate) {
          list(
            id = .scalar_text(aggregate$id),
            title = .scalar_text(aggregate$nome)
          )
        }
      )
    }),
    recursive = FALSE
  )

  ids <- vapply(matches, function(match) match$id, character(1))
  titles <- vapply(matches, function(match) match$title, character(1))
  query <- .normalize_search_text(x)
  normalized_titles <- .normalize_search_text(titles)
  selected <- Reduce(
    `&`,
    lapply(
      query,
      function(term) grepl(term, normalized_titles, fixed = TRUE)
    )
  )
  if (!any(selected)) {
    return(character())
  }

  result <- titles[selected]
  names(result) <- ids[selected]
  result[!duplicated(names(result))]
}

.normalize_search_text <- function(x) {
  accents <- paste0(
    "\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5",
    "\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5",
    "\u00c8\u00c9\u00ca\u00cb",
    "\u00e8\u00e9\u00ea\u00eb",
    "\u00cc\u00cd\u00ce\u00cf",
    "\u00ec\u00ed\u00ee\u00ef",
    "\u00d2\u00d3\u00d4\u00d5\u00d6",
    "\u00f2\u00f3\u00f4\u00f5\u00f6",
    "\u00d9\u00da\u00db\u00dc",
    "\u00f9\u00fa\u00fb\u00fc",
    "\u00c7\u00e7\u00d1\u00f1",
    "\u00dd\u0178\u00fd\u00ff"
  )
  ascii <- paste0(
    "AAAAAA", "aaaaaa",
    "EEEE", "eeee",
    "IIII", "iiii",
    "OOOOO", "ooooo",
    "UUUU", "uuuu",
    "CcNn", "YYyy"
  )
  normalized <- chartr(accents, ascii, enc2utf8(x))
  normalized <- gsub("[\u0300-\u036f]", "", normalized, perl = TRUE)
  tolower(normalized)
}
