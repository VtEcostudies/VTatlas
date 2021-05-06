#' Generate heatmap for the number of observations within polygons
#'
#'
#' @param query \code{list} of query terms \code{list('order:Odonata','etc')}
#' @param polygons shapefile of \code{POLYGON} or \code{MULTIPOLYGON}, can be
#' either an \code{sf} object or \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame}
#' @param seq_by an integer used to 'group' the number of observations. Defaults to 10
#' @param cols should be a vector of colors used for the color ramp
#' @param plot_results boolean. When \code{TRUE} a simple plot is returned.
#' @return polygon with NumObs and when \code{plot_results=TRUE} a map of the number of observations
#' @seealso \code{queryAPI} for description of fields and which fields can be queried
#' @examples
#' \dontrun{
#' SurveyBlocks <- sf::st_read("VT_survey_block_WGS.shp", quiet = TRUE)
#' z <- map_observations(query = list("order:Odonata"),
#'                           polygons = SurveyBlocks,
#'                           seq_by = 10,
#'                           cols = c("gray","blue","firebrick"))}
#' @export
map_observations <- function(query,
                             polygons,
                             seq_by = NULL,
                             cols = NULL,
                             plot_results = FALSE){
  # internal check that polygons the correct format #
  if(!(class(polygons)[1] %in% c("sf","SpatialPolygonsDataFrame","SpatialPolygons"))){
    stop("polygons needs to be either a POLYGON, MULTIPOLYGON, SpatialPolygons or SpatialPolygonsDataFrame")}

  # convert to SF object if needed
  if(class(polygons)[1] %in% c("SpatialPolygonsDataFrame","SpatialPolygons")){
    polygons <- sf::st_as_sf(polygons)}

  # make sure projection is set
  if(is.null(sf::st_crs(polygons)$input)){
    stop('crs is needed for polygons')}

  # project into WGS 84 since the observations are WGS
  if(sf::st_crs(polygons)$input != "WGS 84"){
    polygons <- sf::st_transform(polygons, 4326)
  }
  # get the observations using the queryAPI function
  # for now only get the fields needed to make the map
  fields_to_get <- c("latitude","longitude","taxon_name")
  obs <- queryAPI(query = query, type = 'occurrences', fields = fields_to_get)

  # convert the query results to an sf object #
  # remove the na location values
  obs_wgs <- sf::st_as_sf(obs[!is.na(obs$decimalLongitude),],
                          coords = c('decimalLongitude','decimalLatitude'),
                          crs = 4326)

  # Number of observations for each polygon
  Num_obs <- sf::st_intersects(polygons,obs_wgs)

  # add number of observations to the polygon file
  NumSurveys <- lengths(Num_obs)

  polygons$NumSurveys <- NumSurveys

  if(plot_results==TRUE){
  # set a default for seq_by
  seqVal <- ifelse(is.null(seq_by), 10, seq_by)

  MaxObs <- max(NumSurveys, na.rm = TRUE)+seqVal

  intervals <- seq(0,MaxObs,by = seqVal)

  if(is.null(cols)){
  col_ramp <- colorRampPalette(c("gray88","blue","firebrick4"))(length(intervals))
  }else{
  col_ramp <- colorRampPalette(cols)(length(intervals))
  }

  par(mar = c(0,0,0,0))
  plot(sf::st_geometry(polygons),
       col = col_ramp[findInterval(NumSurveys,intervals)],
       border = "white")
  #empty raster
  r <- raster::raster(ncol = length(intervals), nrow = 1)
  r[] <- intervals

  raster::plot(r, legend.only = TRUE,
               horiz = TRUE,
               breaks = intervals,
               col = col_ramp,
               smallplot = c(0.6,0.9,0.1,0.12),
               axis.args = list(at=intervals[seq(1,length(intervals),,5)],
                                labels = intervals[seq(1,length(intervals),,5)],
                                las = 1,
                                cex.axis = 0.8,
                                mgp = c(1,0.3,0)),
               legend.args = list(text = "Number of Observations"))

  return(polygons)}else{
  return(polygons)}
}
