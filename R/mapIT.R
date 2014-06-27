############################################################
### mapIT() function
# See map/mapIT.Rd for further information
############################################################

mapIT <- function(
  values, id = NULL, data = NULL, detail = "regions", dataSource = "istat", sub = NULL,
  discrete = ifelse(is.numeric(values), FALSE, TRUE),
  guide.label = deparse(substitute(values)),
  graphPar = list(
    low = "#f0f0f0", high = "#005096", palette = "BuGn", colours = NULL,
    title = NULL,
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
  if(is.null(id)) {
    warning("id not provided. values assigned by order")
  }  
  if(detail != "regions") {
    warning("detail is currently ignored")
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
  if(! is.null(sub)) {
    warning("detail is currently ignored")
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

  
  ### If data exists then search values and id as data columns
  if(!is.null(data)) {
    values = data[, values]
    if(!is.null(id)) {id = data[, id]}
  }
  
  ### Transform values to factor
  if(discrete == TRUE) {values = as.factor(values)}
  
  
  ### If id is null then assign numbers from 0
  if(is.null(id)) {id = 0:(length(values)-1)}
  
  
  ### If guide.label contains $, keep the second part
  if(grepl("\\$", guide.label))
    guide.label <- unlist(strsplit(guide.label, "\\$"))[2]
  
  
  ### If guide.label contain non alphanumeric characters, remove them
  guide.label <- onlyChar(guide.label)
    
  
  ### If dataSource is a dataframe use it
  if(class(dataSource) == "data.frame") {shapedata = dataSource}
  
  
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
  
#  if(sum(is.na(posSub)) > 0) {
#    warning(paste("Some 'sub' not recognized:", paste(id[is.na(ckSub)], collapse = ", ")))
#  }


  ### Check if some region ID or name is not matched
  if(sum(is.na(pos)) > 0) {
    warning(paste("Some ID not recognized:", paste(id[is.na(ck)], collapse = ", ")))
  }

  
  ### Add values to shape data
  shapedata[, guide.label] <- values[pos]
  

  ### Plot building
  gp <- ggplot(shapedata, aes_string(x = "long", y = "lat"))
  bg <- graphPar$theme
  th <- do.call(theme, graphPar$themeOption)
  sfc <- scale_fill_continuous(graphPar$legendTitle, low = graphPar$low, high = graphPar$high)
  sfb <- scale_fill_brewer(graphPar$legendTitle, labels=levels(as.factor(values)), palette = graphPar$palette)
  sfm <- scale_fill_manual(graphPar$legendTitle, values = graphPar$colours)
  map <- geom_map(aes_string(map_id = "region", fill = guide.label), map = shapedata, col = graphPar$borderCol, show_guide = graphPar$show_guide)
  lab <- labs(x = "", y = "", title=graphPar$title)
  
  out <- gp + bg + th + map + lab
  
  if(discrete == TRUE) {
    if(is.null(graphPar$colours)) {
      out <- out + sfb
    } else {
      out <- out + sfm 
    }
  } else {
    out <- out + sfc
  }
  
  return(out)
}
