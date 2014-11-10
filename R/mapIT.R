############################################################
### mapIT() function
# See map/mapIT.Rd for further information
############################################################

mapIT <- function(
  values, id, data, detail = "regions", dataSource = "istat", sub = NULL, discrete = NULL, guide.label = NULL, title = NULL,
  graphPar = list(
    low = "#f0f0f0", high = "#005096", palette = "BuGn", colours = NULL, theme = theme_minimal(),
    themeOption = list(
      title = element_text(size = 18), axis.ticks = element_blank(),
      axis.text.x = element_blank(), axis.text.y = element_blank()
    ),
    borderCol = "black", show_guide = TRUE
  )
) {

  ### Check inputs
  if(missing(id)) {
    warning("id not provided. values assigned by order")
  }  
  if(detail != "regions") {
    warning("the argument 'detail' is currently ignored")
    detail <- "regions"
  }
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
  if(! all(sort(names(graphPar)) %in% sort(names(eval(formals(sys.function(sys.parent()))$graphPar))))) {
    warning("additional arguments to graphPar ignored")
  }
  if(!is.null(graphPar$themeOption)) {
    if(! all(sort(names(graphPar$themeOption)) %in% sort(names(eval(formals(sys.function(sys.parent()))$graphPar$themeOption))))) {
      warning("additional arguments to themeOption ignored")
    }
  }
  
  ### Remove all non alphanumeric characters from region names and transform to lower case
  onlyChar <- function(string) {
    tolower(gsub(" ", "", gsub("[^[:alnum:]]", " ", string)))
  }
  
  ### Update lists elements (and keep default when argument is not passed)
  listDefTO = eval(formals(sys.function(sys.parent()))$graphPar$themeOption)
  if(!is.null(graphPar$themeOption)) {listDefTO[sort(names(listDefTO[sort(names(listDefTO)) %in% sort(names(graphPar$themeOption))]))] = graphPar$themeOption[sort(names(graphPar$themeOption))]}
    
  listDef = eval(formals(sys.function(sys.parent()))$graphPar)
  listDef[sort(names(listDef[names(listDef) %in% names(graphPar)]))] = graphPar[sort(names(graphPar))]
  
  graphPar = listDef
  graphPar$themeOption = listDefTO
  
  ### If the label for the legend is unspecified
  if(is.null(guide.label)) guide.label <- deparse(substitute(values))
  
  ### If data exists then search values and id as data columns
  if(!missing(data)) {
    values <- data[,deparse(substitute(values))]
    if(!missing(id)) id <- data[,deparse(substitute(id))]
  }
  
  ### If id is missing then assign numbers from 0
  if(missing(id)) delayedAssign("id", 0:(length(values)-1))
  
  ### Transform values to factor
  if(is.numeric(values)) {
    discrete <- FALSE
  } else {
    discrete <- TRUE
    values <- as.factor(values)
  }
  
  ### If the data argument is unspecified
  if(missing(data)) {
    assign("data", data.frame(values,id))
  }
  
  ### If guide.label contains $, keep the second part
  if(grepl("\\$", guide.label)) {
    guide.label <- unlist(strsplit(guide.label, "\\$"))[2]
  }
  
  ### If dataSource is a dataframe use it
  if(class(dataSource) == "data.frame") shapedata = dataSource
  
  ### If dataSource is a string load data
  if(class(dataSource) == "character" & dataSource == "istat" & detail == "regions") {
    shapedata_istat_regioni <- NULL # avoid note in R CMD check
    data("shapefile_istat_regioni", envir = environment())
    shapedata = shapedata_istat_regioni
  }
  
  ### Match region ID or names
  if(is.numeric(id)) {
    pos <- match(shapedata$id, as.character(id))
    ck <- match(as.character(id), shapedata$id)
  } else { 
    pos <- match(onlyChar(shapedata$region), onlyChar(id))
    ck <- match(onlyChar(id), onlyChar(shapedata$region))
  }
  
  ### Select 'sub' regions
  ### TODO
   if (!is.null(sub)) {
     if(is.numeric(id)) {
       posSub <- match(shapedata$id, as.character(sub))
       ckSub <- match(as.character(sub), shapedata$id)
       shapedata <- shapedata[shapedata$id %in% as.character(sub), ]
       values <- values[id %in% as.character(sub)]
     } else { 
       posSub <- match(onlyChar(shapedata$region), onlyChar(sub))
       ckSub <- match(onlyChar(sub), onlyChar(shapedata$region))
       shapedata <- shapedata[onlyChar(shapedata$region) %in% onlyChar(sub), ]
       values <- values[id %in% onlyChar(sub)]
     }
     pos <- posSub[which(!is.na(posSub))]
   }

  ### Check if some region ID or name is not matched
  if(sum(is.na(pos)) > 0) {
    warning(paste("Some ID not recognized:", paste(id[is.na(ck)], collapse = ", ")))
  }
  
  ### Add values to shape data
  shapedata[, "values"] <- values[pos]
  
  ### Plot building
  gp <- ggplot(shapedata, aes_string(x = "long", y = "lat"))
  bg <- graphPar$theme
  th <- do.call(theme, graphPar$themeOption)
  map <- geom_map(aes_string(map_id = "region", fill = "values"), map = shapedata, col = graphPar$borderCol, show_guide = graphPar$show_guide)
  lab <- labs(x = "", y = "", title=title)
  out <- gp + bg + th + map + lab
  if(discrete == TRUE) {
    if(is.null(graphPar$colours)) {
      scf <- scale_fill_brewer(labels=levels(as.factor(values)), palette = graphPar$palette, name=guide.label)
    } else {
      scf <- scale_fill_manual(values = graphPar$colours, name=guide.label)
    }
  } else {
      scf <- scale_fill_continuous(low = graphPar$low, high = graphPar$high, name=guide.label)
  }
  
  return(out+scf)
}
