#' Create query to grab data from API request
#'
#' \code{get_data} is a wrapper for httr::GET that connects to Vermont Atlas of
#' Life's API
#'
#' @param query \code{list} of query terms \code{list('order:Odonata','etc')}
#' @param type default = occurrences
#' @param fields defaults to 'all'
#' @param quiet defaults to \code{FALSE}, if \code{TRUE} prints brief description to console
#' @param pageSize defaults to NULL which returns all records
#' @param spatialWKT well-known text for a \code{POLYGON} or \code{MULTIPOLYGON}, defaults to null
#' @return \code{data frame} of results from query
#' @seealso \code{dataFields} for description of fields and which fields can be queried
#' @export
queryAPI <- function(query,
                     type = 'occurrences',
                     fields = "all",
                     quiet = FALSE,
                     pageSize = NULL,
                     spatialWKT = NULL){

  # Put a check here to make sure that the query fields are in
  # the dataFields dataset

  # base API to start search
  VAL_base <- "https://biocache-ws.vtatlasoflife.org/"

  occ_search <- "occurrences/search?"

  # collapse the query list
  Query <- paste0(query, collapse = "&")

  q <- paste0("q=",Query)

  queryURL <- paste0(VAL_base,occ_search,q)

  if(!is.null(spatialWKT)){
  queryURL <- paste0(queryURL,"&wkt=",spatialWKT)
  }
  # quick query to get the pageSize and report inital stats
  intialQuery <- suppressWarnings(jsonlite::fromJSON(readLines(queryURL)))

  # if pageSize is null get the total number of records
  ps <- ifelse(is.null(pageSize),intialQuery$totalRecords, pageSize)

  if(!quiet){cat(intialQuery$totalRecords,'records found. Gathering',ps,'records now\n')}

  queryURL <- paste0(queryURL,"&pageSize=", ps)

  datalist <- suppressWarnings(jsonlite::fromJSON(readLines(queryURL), flatten = TRUE))

  return(datalist$occurrences)
}
