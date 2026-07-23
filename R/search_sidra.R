#' Search SIDRA tables
#'
#' Searches table titles in the official IBGE aggregate catalog.
#'
#' @param x A non-empty character vector containing the search terms.
#'
#' @return A named character vector with matching SIDRA table titles. Names
#'   are the table codes.
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
  normalized <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  normalized[is.na(normalized)] <- x[is.na(normalized)]
  tolower(normalized)
}
