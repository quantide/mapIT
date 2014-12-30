#' Easier choropleth maps
#' 
#' @export
#' 
#' @title Easier choropleth maps
#' 
#' @name map
#' 
#' @param region The region for which the map should be drawn (currently only 'Italy')
#' @param ... Arguments for the calle dfunction
#' 
#' @return NULL
#' 
#' @author Nicola Sturaro

map <- function(region = "Italy", ...) {
  if(tolower(region) %in% c("it", "italy", "italia")) {
    mapIT(...)
  } else {
    stop("region actually support only 'Italy'.")
  }
}