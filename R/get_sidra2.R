#' Get SIDRA's table
#'
#' This function allows the user put directly the parameters of the query in the 
#' api format.
#'
#' @usage get_sidra2(x)
#' @param x A character with the IBGE's SIDRA API parameters.
#' @return The function returns a data frame printed by parameters input.
#' @author Renato Prado Siqueira \email{<rpradosiqueira@@gmail.com>}
#' @seealso \code{\link{get_sidra}}
#' @examples
#' \dontrun{
#' ## Requesting table 1860 with the default parameters from the SIDRA's online acess
#' get_sidra2("/t/1860/n1/all/v/140/p/last%201/c2/6794/c1/6795/c12021/106827/d/v140%200")
#'
#' ## Firm count from table 987 (Cadastro Central de Empresas) to states.
#' get_sidra2("/t/987/n3/all/v/2585/p/last%201/c12762/117897/c319/104029")
#'
#' @keywords sidra IBGE
#' @export

get_sidra2 <- function(x) {
  
  if (!is.character(x)) stop("x must be a character")
  
  if (length(x) != 1) stop("Only one element is allowed")
  
  path=RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values", x),
                     ssl.verifyhost=FALSE,
                     ssl.verifypeer=FALSE)
  
  test1 <- try(rjson::fromJSON(path), silent=TRUE)
  
  
  if (strsplit(path, " ")[[1]][2] == "P") {
    
    stop("The 'period' argument is misspecified.")
    
  } else if (strsplit(path, " ")[[1]][1] == "Tabela" &
             strsplit(path, " ")[[1]][3] == "Tabela"){
    
    ntable = strsplit(path, " ")[[1]][2]
    ntable = substr(ntable, 1, nchar(ntable)-1)
    
    stop("This table does not exists.")
    
  } else if (strsplit(path, " ")[[1]][2] == "V") {
    
    stop(sprintf("The table %s does not contain the %s variable", x, variable))
    
  } else if ('try-error' %in% class(test1)) {
    
    stop(path)
    
  } else {
    
    path = rjson::fromJSON(path)
    path = as.data.frame(do.call("rbind", path))
    
    colnames(path) = unlist(path[1, ])
    path = path[-1, ]
    
    
    id = which(colnames(path) == "V" | colnames(path) == "Valor")
    
    path[ ,id] = suppressWarnings(ifelse(unlist(path[ ,id]) != "..", as.numeric(unlist(path[ ,id])), NA))
    
  }
  
  return(path)
  
}