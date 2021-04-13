#' Collate content data from API request
#'
#' \code{JSON_to_dataframe} extracts data from an API request and reformats
#' into a dataframe
#'
#' @param x is the content from an API query
#'
#' @param fields which data fields should be returned? \code{all} returns all
#' fields that are not \code{class("list")}, while \code{minimal} returns only
#' a subset of fields to run analyses - most metadata for the observation are
#' removed. However, enough information is retained to relate the obs. back to
#' the appropriate metadata.
#'
#'
#' @param type which records to report
#'
#' @param quiet defaults to \code{TRUE}, if \code{FALSE} prints the number of
#' records and number of pages to console
#'
#' @return \code{data.frame} where each observation is a row
#' @export
#'
JSON_to_dataframe <- function(x, fields = "minimal", type = "occurrence", quiet = TRUE){

  if(!quiet){cat('There are',x$totalRecords,'records stored in',x$pageSize,'pages\n')}

  if (!(fields %in% c("all","minimal"))){
    stop("fields must be either all or mimimal", call. = FALSE)
  }

  # get the occurrence portion of the list
  if(type == "occurrence"){x<-x$occurrences}

  # remove variables that are lists
  data <- lapply(x,FUN = function(xx, fields = fields){
  data_fields <- names(xx)
  data_fields <- data_fields[which(sapply(xx,class) != "list")]

  if(fields == "minimal"){
    data_fields <- c("uuid","occurrenceID","eventDate","scientificName",
                     "country","kingdom","phylum","classs","order","family",
                     "genuis","species","decimalLatitude","decimalLongitude",
                     "coordinateUncertaintyInMeters","year","month","basisOfRecord",
                     "latLong")
  }

  x.new <- x[names(xx) %in% data_fields]
  return(data.frame(x.new))
  }) # end of lapply

  return(do.call(rbind, data))
}
