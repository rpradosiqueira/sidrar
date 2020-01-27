#' Search SIDRA's tables with determined term(s)
#'
#' It returns all SIDRA's tables with determined term
#'
#' @param x A character vector with the term(s)/word(s) to search.
#' @return A character vector with the tables' names.
#' @author Renato Prado Siqueira \email{<rpradosiqueira@@gmail.com>}
#' @seealso \code{\link{get_sidra}}
#' @examples
#' \dontrun{
#' search_sidra("contas nacionais")
#' }
#'
#' @keywords sidra IBGE
#' @importFrom magrittr %>%
#' @export

search_sidra <- function(x) {

  x <- gsub(" ", "%20", x)

  a <- xml2::read_html(paste0("https://sidra.ibge.gov.br/Busca?q=", paste0(x, collapse = "%20")))

  s <- a %>%
    rvest::html_nodes(".busca-link-tabela") %>%
    rvest::html_text()

  return(s)
}
