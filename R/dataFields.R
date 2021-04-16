#' Fields in the Vermont Atlas of Life Solr database.
#'
#' A dataset containing the field names and attributes of the
#' solr database as of April 2021.
#'
#' @return  A data frame with 237 rows and 14 variables:
#' \describe{
#'   \item{name}{Variable name}
#'   \item{dataType}{How are data stored}
#'   \item{indexed}{is the data indexed in the db}
#'   \item{stored}{are the data stored or computed}
#'   \item{multivalue}{does the field return a multivalue field}
#'   \item{docvalue}{??}
#'   \item{downloadName}{Name needed to get data from API}
#'   \item{description}{simple description of the field}
#'   \item{dwcTerm}{Darwin Core name}
#'   \item{classs}{??}
#'   \item{jsonName}{??}
#'   \item{downloadDescription}{??}
#'   \item{i18nValues}{??}
#'   \item{info}{description and/or link to more information}
#' }
"dataFields"
