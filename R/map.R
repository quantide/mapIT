#' Easier choropleth maps
#' 
#' @export
#' 
#' @title Easier choropleth maps
#' 
#' @name map
#' 
#' @param area The geographical area for which the map should be drawn (currently only 'Italy')
#' @param ... Arguments for the called function
#' 
#' @return NULL
#' 
#' @author Nicola Sturaro

map <- function(..., area = "Italy") {
  if(tolower(area) %in% c("it", "italy", "italia")) {
    mapIT(...)
  } else {
    stop("area actually support only 'Italy'.")
  }
}