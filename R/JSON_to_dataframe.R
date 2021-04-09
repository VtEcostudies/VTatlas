#' Collate content data from API request
#'
#' \code{JSON_to_dataframe} extracts data from an API request and reformats
#' into a dataframe
#'
#' @param x is the content from an API query
#'
#' @return \code{data.frame} where each observation is a row
#' @export
#'
JSON_to_dataframe <- function(x){
  # remove variables that are lists
  fields <- names(x)
  fields <- fields[which(sapply(x,class) != "list")]
  x.new <- x[names(x) %in% fields]
  data <- data.frame(x.new)
  return(data)
}
