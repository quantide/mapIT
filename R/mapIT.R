#' Creazione di mappe (coroplete) delle regioni italiane in maniera semplice.
#' 
#' 
#' @export
#' @import ggplot2
#' 
#' @title Choropleth Maps of Italy
#' 
#' @name mapIT
#' 
#' @param values Vettore numerico contenente i valori da associare a ciascuna regione. Può essere indicato un oggetto di tipo vettore oppure una stringa indicante la colonna del data.frame specificato in \code{data}.
#' @param id Vettore contenente i nomi delle aree geografiche a cui associare i valori di \code{values}. Spazi e altri caratteri non alfabetici vengono ignorati. Questo significa che si può scrivere indifferentemente: 'Trentino-Alto Adige', 'Trentino Alto Adige' o 'TrentinoAltoAdige'. E' indifferente usare lettere maiuscole o minuscole. Per le localit con denominazione bilingue, viene riconosciuta la sola dicitura in italiano. Può essere indicato un oggetto di tipo vettore oppure una stringa indicante la colonna del data.frame specificato in \code{data}. Per ulteriori informazioni, si vedano i dettagli.
#' @param data Specifica il data.frame in cui sono contenuti \code{values} e \code{id}. Può essere omesso nel caso in cui \code{values} e \code{id} siano due vettori.
#' @param detail Dettaglio con cui disegnare la mappa. Aggiunto per future estensioni, attualmente l'unico valore ammesso è quello di default, '\code{regions}'. Valori diversi vengono ignorati.
#' @param dataSource Fonte dei dati. Se uguale a '\code{istat}' (default), vengono usati i dati forniti dall'Istat e contenuti nel package. Altrimenti un data.frame con la stessa struttura dei dati Istat può essere utilizzato. 
#' @param sub Vettore contenente i nomi delle aree geografiche da mostrare nella mappa. Consente di produrre una mappa di alcune aree e non di tutta l'Italia.
#' @param show_missing Se TRUE (default) mostra anche le aree geografiche per cui non ci sono dati. Se FALSE, invece, le aree geografiche per cui non ci sono dati non vengono mostrate.
#' @param show_na Correntemente non implementato. Per sviluppi futuri.
#' @param discrete Considera valori numerici come fattori.
#' @param graphPar \describe{
#'   Lista contenente i seguenti parametri grafici: 
#'   \item{\code{guide.label}}{Una stringa contenente un'etichetta per i dati. Se mancante, il nome dell'oggetto contenente i valori \code{values} viene utilizzato.}
#'   \item{\code{title}}{Titolo del grafico. Se nullo (default), nessun titolo sarà visualizzato.}
#'   \item{\code{low}}{solo se \code{values} assume valori continui, colore da assegnare alle aree geografiche con valore di \code{values} più basso. (default: #F0F0F0)}
#'   \item{\code{high}}{solo se \code{values} assume valori continui, colore da assegnare alle aree geografiche con valore di \code{values} più alto. (default: #005096)}                                                                                                                                              
#'   \item{\code{na_color}}{solo se \code{values} assume valori continui, colore da assegnare alle aree geografiche il cui valore è mancante. (default: #333333)}                                                                                                                                              
#'   \item{\code{palette}}{solo se \code{values} assume valori discreti, tipo di palette RColorBrewer da utilizzare. Viene ignorato se \code{colours} non è nullo. Per visualizzare tutte le palette disponibili, è possibile utilizzare il comando \code{RColorBrewer::display.brewer.all()}  (default: 'BuGn')}
#'   \item{\code{colours}}{solo se \code{values} assume valori discreti, colore da associare a ciascun valore di 'values'. Se nullo (default), viene ignorato e utilizzata una palette.}
#'   \item{\code{theme}}{tema ggplot2 da utilizzare. (default: \code{\link{theme_minimal}()})}
#'   \item{\code{themeOption}}{lista contenente le seguenti impostazioni relative al tema: \code{title}, \code{axis.ticks}, \code{axis.text.x}, \code{axis.text.y}. Per informazioni sul significato si veda \code{\link{theme}}.}
#'   \item{\code{borderCol}}{colore per i confini delle diverse aree. (default: '\code{black}')}
#'   \item{\code{show_grid}}{Se TRUE (default), mostra la griglia di sfondo. Se FALSE, la griglia di sfondo non viene visualizzata. Questo parametro viene ignorato se un \code{theme} viene specificato. }
#'   \item{\code{show_guide}}{correntemente ignorato. Per future estensioni.}
#' }
#'
#' @details Se \code{id} è mancante, i valori contenuti in \code{values} sono associati alle regioni nel seguente ordine: Piemonte, Valle d'Aosta, Lombardia, Trentino-Alto Adige, Veneto, Friuli-Venezia Giulia, Liguria, Emilia-Romagna, Toscana, Umbria, Marche, Lazio, Abruzzo, Molise, Campania, Puglia, Basilicata, Calabria, Sicilia, Sardegna.
#' 
#' @return A ggplot class object
#' 
#' @seealso About the ggplot function to map data: \code{\link[ggplot2:geom_map]{geom_map}}. About data (shapefile): \code{\link{shapefile_istat_regioni}}.
#' 
#' @author Nicola Sturaro
#' 
#' @examples 
#'   ### utilizza dati continui
#'   values = runif(20, min = 0, max = 1000)       
#'   
#'   # funzione minimale per creare la mappa
#'   mapIT(values = values)
#'   
#'   # crea id regionali
#'   # è indifferente usare maiuscole e minuscole
#'   # i caratteri quali trattini e apostrofi possono essere ignorati
#'   id = c('Piemonte', 'VALLE DAOSTA', 'lOMBARDiA', 'Trentino Alto Adige', 'VENETO', 'FRiULi - VENEZiA GiULiA', 'LiGURiA', 'EMiLiAROMAGNA', 'TOSCANA', 'UMBRiA', 'MARCHE', 'LAZiO', 'ABRUZZO', 'MOLiSE', 'CAMPANiA', 'PUGLiA', 'BASiLiCATA', 'CALABRiA', 'SiCiLiA', 'SARDEGNA')
#'   
#'   # crea la mappa
#'   mapIT(values = values, id = id, graphPar = list(guide.label = "Valori"))
#'   
#'   # modifica i colori
#'   mapIT(values = values, id = id, graphPar = list(guide.label = "Valori", low = "#00ff00", high = "#ff0000"))
#'   
#'   ### utilizza dati categoriali
#'   values = c(rep("Nord-Ovest", 4), rep("Nord-Est", 4), rep("Centro", 4), rep("Sud", 6), rep("Isole", 2))
#'   id = c('Piemonte', 'VALLE DAOSTA', 'lOMBARDiA', 'LiGURiA', 'Trentino Alto Adige', 'VENETO', 'FRiULi - VENEZiA GiULiA', 'EMiLiAROMAGNA', 'TOSCANA', 'UMBRiA', 'MARCHE', 'LAZiO', 'ABRUZZO', 'MOLiSE', 'CAMPANiA', 'PUGLiA', 'BASiLiCATA', 'CALABRiA', 'SiCiLiA', 'SARDEGNA')
#'   
#'   # funzione minimale per creare la mappa
#'   mapIT(values = values, id = id, graphPar = list(guide.label = "Valori"))
#'   
#    # aggiungi i colori alle mappe
#'   mapIT(values = values, id = id, graphPar = list(guide.label = "Aree geografiche", colours = c("red", "darkblue", "green", "yellow", "purple")))
#'   
#'   # utilizzando i set di colori predefiniti, si ottiene un risultato migliore
#'   mapIT(values = values, id = id, graphPar = list(guide.label = "Aree geografiche", palette = "Dark2"))
#'   
#'   # modificando i dati affinché le macroaree siano un fattore ordinato, si ottiene una legenda e una colorazione più coerente
#'   values = factor(values, levels = c("Nord-Ovest", "Nord-Est", "Centro", "Sud", "Isole"), ordered = TRUE)
#'   mapIT(values = values, id = id, graphPar = list(guide.label = "Aree geografiche"))


mapIT <- function(
  values, id, data, detail = "regions", dataSource = "istat", sub = NULL, show_missing = TRUE, show_na = TRUE,
  discrete = NULL, 
  graphPar = list(
    guide.label = NULL, title = NULL,
    low = "#f0f0f0", high = "#005096", na_color = "#333333", palette = "BuGn", colours = NULL,
    theme = theme_minimal(),
    themeOption = list(
      title = element_text(size = 18), axis.ticks = element_blank(),
      axis.text.x = element_blank(), axis.text.y = element_blank()
    ),
    borderCol = "black", show_grid = TRUE, show_guide = TRUE
  ),
  ...
) {

  ### If the label for the legend is not specified
  ### (the label assignment to guide.label must be done before the manipulation of 'values')
  if(is.null(graphPar$guide.label)) {
    graphPar$guide.label <- deparse(substitute(values))
    graphPar$guide.label <- strsplit(graphPar$guide.label,"\\$")[[1]]
    if(length(graphPar$guide.label) == 2)
      graphPar$guide.label <- graphPar$guide.label[2]
  }
  
  ### Check inputed data
  ### If data exists then search values as data columns
  if(!missing(data)) values <- data[,deparse(substitute(values))]
  ### If missing exists...
  if(!missing(id)) {
    if(!missing(data)) {
      ### ...if data exists then search id as data columns
      id <- data[,deparse(substitute(id))]
    }
  } else {
    ### ...if data is missing then assign numbers from 0
    id <- 0:(length(values)-1)
    warning("id not provided. Values assigned by order")
  }
  if(is.factor(id)) id <- as.character(id)
  
  ### Transform values to factor
  if(is.numeric(values)) {
    discrete <- FALSE
  } else {
    discrete <- TRUE
    values <- as.factor(values)
  }
  
  if(detail != "regions") {
    warning("the argument 'detail' is currently ignored")
    detail <- "regions"
  }
  if(class(dataSource) == "character") {
    if (dataSource != "istat") {
      stop("dataSource must be 'istat' or a data frame")
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
  if(!show_missing %in% c(TRUE, FALSE)) {
    stop("show_missing must be TRUE or FALSE")
  }
  if(!show_na %in% c(TRUE)) {
    stop("show_na ignored")
  }
  if(! all(sort(names(graphPar)) %in% sort(names(eval(formals(mapIT)$graphPar))))) {
    warning("additional arguments to graphPar ignored")
  }
  if(!is.null(graphPar$themeOption)) {
    if(! all(sort(names(graphPar$themeOption)) %in% sort(names(eval(formals(mapIT)$graphPar$themeOption))))) {
      warning("additional arguments to themeOption ignored")
    }
  }
  
  ### Remove all non alphanumeric characters from region names and transform to lower case
  onlyChar <- function(string) {
    tolower(gsub(" ", "", gsub("[^[:alnum:]]", " ", string)))
  }

  ### Update lists elements (and keep default when argument is not passed)
  listDefTO = eval(formals(mapIT)$graphPar$themeOption)
  if(!is.null(graphPar$themeOption)) {listDefTO[sort(names(listDefTO[sort(names(listDefTO)) %in% sort(names(graphPar$themeOption))]))] = graphPar$themeOption[sort(names(graphPar$themeOption))]}
    
  listDef = eval(formals(mapIT)$graphPar)
  listDef[sort(names(listDef[names(listDef) %in% names(graphPar)]))] = graphPar[sort(names(graphPar))]
  
  graphPar = listDef
  graphPar$themeOption = listDefTO
  
  rm(listDef, listDefTO)
  
  ### If show_grid is FALSE then hide grid
  if(graphPar$show_grid == FALSE & identical(graphPar$theme, theme_minimal())) {
    graphPar$theme$panel.grid.major <- element_blank()
    graphPar$theme$panel.grid.minor <- element_blank()
  }
  
  ### If dataSource is a dataframe use it
  if(class(dataSource) == "data.frame") shapedata = dataSource
  
  ### If dataSource is a string load data
  if(class(dataSource) == "character") {
    if(dataSource == "istat") {
      if(detail == "regions") {
        shapedata <- shapedata_istat_regioni # require LazyData in DESCRIPTION
      }
    }
  }
  
  ### Match region ID or names
  id_df <- unique(shapedata_istat_regioni[, c("id", "region")])
  
  if(is.numeric(id)) {
    id_input <- id
    id <- id_df$region[!is.na(match(onlyChar(id_df$id), onlyChar(id)))]
  }
  
  match_all <- match(onlyChar(id_df$region), onlyChar(id)) # NA if not all region are provided
  match_missing <- match(onlyChar(id), onlyChar(id_df$region)) # NA if some region is not recognized
  
  pos <- match(onlyChar(shapedata$region), onlyChar(id))

  ### Check if some region name is not matched
  if(sum(is.na(match(onlyChar(id), onlyChar(id_df$region)))) > 0) {
    warning(paste("Some id not recognized:", paste(id[is.na(match_missing)], collapse = ", ")))
  }
  
  ### Select 'sub' regions
  if(show_missing == FALSE) {
    sub_fromData <- id_df$region[!is.na(match(onlyChar(id_df$region), onlyChar(id)))]
    if(is.null(sub)) {
      sub <- sub_fromData
    } else {
      sub <- sub[onlyChar(sub) %in% onlyChar(sub_fromData)]
    }
  }
  
  if (!is.null(sub)) {
    # Match sub and region
    sub_match_all <- match(onlyChar(shapedata$region), onlyChar(sub))
    sub_match_missing <- match(onlyChar(sub), onlyChar(shapedata$region))
    # Remove shapedata not in sub
    shapedata <- shapedata[onlyChar(shapedata$region) %in% onlyChar(sub), ]
    # Remove values not in sub
    values <- values[onlyChar(id) %in% onlyChar(sub)]
    # Remove pos not in sub
    pos <- sub_match_all[which(!is.na(sub_match_all))]
    # Check if some region sub is not matched
    if(sum(is.na(sub_match_missing)) > 0) {
      warning(paste("Some sub not recognized:", paste(id[is.na(sub_match_missing)], collapse = ", ")))
    }
  }

  ### Add values to shape data
  shapedata[, "values"] <- values[pos]
  
  
  ### Plot building
  gp <- ggplot(shapedata, aes_string(x = "long", y = "lat"))
  bg <- graphPar$theme
  th <- do.call(theme, graphPar$themeOption)
  map <- geom_map(aes_string(map_id = "region", fill = "values"), map = shapedata, col = graphPar$borderCol, show_guide = graphPar$show_guide)
  lab <- labs(x = "", y = "", title=graphPar$title)
  out <- gp + bg + th + map + lab
  if(discrete == TRUE) {
    if(is.null(graphPar$colours)) {
      scf <- scale_fill_brewer(labels=levels(as.factor(values)), palette = graphPar$palette, name=graphPar$guide.label)
    } else {
      scf <- scale_fill_manual(values = graphPar$colours, name=graphPar$guide.label)
    }
  } else {
      scf <- scale_fill_continuous(low = graphPar$low, high = graphPar$high, na.value = graphPar$na_color, name=graphPar$guide.label)
  }
  
  return(out+scf)
}
