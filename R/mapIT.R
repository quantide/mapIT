############################################################
### mapIT() function
# Authors: Davide Massidda and Nicola Sturaro
# Date: Mar 4, 2014
# Version: 0.1
# Requires: ggplot2
# License: GPLv3

# The function draws a map of Italy
############################################################

mapIT <- function(
  values, id = NULL, data = NULL, detail = "regions", dataSource = "istat", guide.label = deparse(substitute(values)),
  graphPar = list(
    low = "#f0f0f0", high = "#005096", title = NULL,
    theme = theme_minimal(),
    themeOption = list(
      title = element_text(size = 18), axis.ticks = element_blank(),
      axis.text.x = element_blank(), axis.text.y = element_blank()
    ),
    borderCol = "black", show_guide = TRUE,
    legendTitle = guide.label
  ) 
) {
    
  ### Check inputs
  if(class(dataSource) == "character") {
    if (dataSource != "istat") {
      stop("dataSource must be 'istat' or a shape data")
    }
  }
  if(class(dataSource) == "data.frame") {
    if(sum(!(names(dataSource) %in% c("long", "lat", "order", "hole", "piece", "group", "id", "region")))) {
      stop("dataSource must contains the following columns: long, lat, order, hole, piece, group, id, region")
    }
    if(detail != "regions") {
      warning("shapefile provided. detail is currently ignored")
    }
  }
  if(detail != "regions") {
    warning("detail is currently ignored")
  }
  if(is.null(id)) {
    warning("id not provided. values assigned by order")
  }
  
  ### Remove all non alphanumeric characters from region names and transform to lower case
  onlyChar <- function(string) {
    tolower(gsub(" ", "", gsub("[^[:alnum:]]", " ", string)))
  }
  
  ### If data exists then search values and id as data columns
  if(!is.null(data)) {
    values = data[, values]
    if(!is.null(id)) {id = data[, id]}
  }
  
  ### If id is null then assign numbers from 0
  if(is.null(id)) {id = 0:(length(values)-1)}
  
  ### If guide.label contains $, keep the second part
  if(grepl("\\$", guide.label))
    guide.label <- unlist(strsplit(guide.label, "\\$"))[2]
  
  ### If dataSource is a dataframe use it
  if(class(dataSource) == "data.frame") {shapedata = dataSource}
  
  ### Match region ID or names
  if(is.numeric(id)) {
    pos <- match(shapedata$id, as.character(id))
    ck <- match(as.character(id), shapedata$id)
  } else { 
    pos <- match(onlyChar(shapedata$region), onlyChar(id))
    ck <- match(onlyChar(id), onlyChar(shapedata$region))
  }

  ### Check if some region ID or name is not matched
  if(sum(is.na(pos)) > 0) {
    warning(paste("Some ID not recognized:", paste(id[is.na(ck)], collapse = ", ")))
  }
  
  ### Add values to shape data
  shapedata[, guide.label] <- values[pos]
  
  ### Remove unused variables
  rm(values, id, pos)
  
  ### Plot building
  gp <- ggplot(shapedata, aes_string(x = "long", y = "lat"))
  bg <- graphPar$theme
  th <- do.call(theme, graphPar$themeOption)
  col <- scale_fill_continuous(aes_string(guide.label), low = graphPar$low, high = graphPar$high)
  #col <- scale_fill_discrete(breaks=as.character(1:20))
  map <- geom_map(aes_string(map_id = "region", fill = guide.label), map = shapedata, col = graphPar$borderCol, show_guide = graphPar$show_guide)
  lab <- labs(x = "", y = "", title=graphPar$title, colour=guide.label)
  leg <- guides(fill = guide_legend(title = graphPar$legendTitle))
  out <- gp + bg + col + map + th + lab # + leg
  return(out)
}


