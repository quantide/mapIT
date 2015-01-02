#' Creazione di mappe (coroplete) delle regioni italiane in maniera semplice.
#' 
#' @export
#' @import ggplot2
#' 
#' @title Choropleth Maps of Italy
#' 
#' @name mapIT
#' 
#' @param values Vettore numerico contenente i valori da associare a ciascuna regione. Puo' essere indicato un oggetto di tipo vettore oppure una stringa indicante la colonna del data.frame specificato in \code{data}.
#' @param id Vettore contenente i nomi delle regioni a cui associare i valori di \code{values}. Spazi e altri caratteri non alfabetici vengono ignorati. Questo significa che si puo' scrivere indifferentemente: 'Trentino-Alto Adige', 'Trentino Alto Adige' o 'TrentinoAltoAdige'. E' indifferente usare lettere maiuscole o minuscole. Per le regioni con denominazione bilingue, viene riconosciuta la sola dicitura in italiano. Puo' essere indicato un oggetto di tipo vettore oppure una stringa indicante la colonna del data.frame specificato in \code{data}. Se \code{id} e' mancante, i valori contenuti in \code{values} sono associati alle regioni nel seguente ordine: Piemonte, Valle d'Aosta, Lombardia, Trentino-Alto Adige, Veneto, Friuli-Venezia Giulia, Liguria, Emilia-Romagna, Toscana, Umbria, Marche, Lazio, Abruzzo, Molise, Campania, Puglia, Basilicata, Calabria, Sicilia, Sardegna.
#' @param data Specifica il data.frame in cui sono contenuti \code{values} e \code{id}. Puo' essere omesso nel caso in cui \code{values} e \code{id} siano due vettori.
#' @param detail Dettaglio con cui disegnare la mappa. Aggiunto per future estensioni, attualmente l'unico valore ammesso e' quello di default, '\code{regions}'. Valori diversi vengono ignorati.
#' @param dataSource Fonte dei dati. Se uguale ad '\code{istat}' (default), vengono usati i dati forniti dall'iSTAT e contenuti nel package. Altrimenti un data.frame con la stessa struttura dei dati iSTAT puo' essere utilizzato. 
#' @param sub Consente di produrre una mappa di alcune aree selezionate e non di tutta l'Italia.
#' @param discrete Considera valori numerici come fattori.
#' @param graphPar \describe{
#'   Lista contenente i seguenti parametri grafici: 
#'   
#'   \item{\code{guide.label}}{Una stringa contenente un'etichetta per i dati. Se mancante, il nome dell'oggetto contenente i valori \code{values} viene utilizzato.}
#'   \item{\code{title}}{Titolo del grafico. Se nullo (default), nessun titolo sara' visualizzato.}
#'   \item{\code{low}}{solo se \code{values} assume valori continui, colore da assegnare alle regioni con valore di \code{values} piu' basso. (default: #F0F0F0)}
#'   \item{\code{high}}{solo se \code{values} assume valori continui, colore da assegnare alle regioni con valore di \code{values} piu' alto. (default: #005096)}                                                                                                                                              
#'   \item{\code{palette}}{solo se \code{values} assume valori discreti, tipo di palette RColorBrewer da utilizzare. Viene ignorato se \code{colours} non e' nullo. Per visualizzare tutte le palette disponibili, e' possibile utilizzare il comando \code{RColorBrewer::display.brewer.all()}  (default: 'BuGn')}
#'   \item{\code{colours}}{solo se \code{values} assume valori discreti, colore da associare a ciascun valore di 'values'. Se nullo (default), viene ignorato e utilizzata una palette.}
#'   \item{\code{theme}}{tema ggplot2 da utilizzare. (default: \code{\link{theme_minimal}()})}
#'   \item{\code{themeOption}}{lista contenente le seguenti impostazioni relative al tema: \code{title}, \code{axis.ticks}, \code{axis.text.x}, \code{axis.text.y}. Per informazioni sul significato si veda \code{\link{theme}}.}
#'   \item{\code{borderCol}}{colore per i confini delle diverse aree. (default: '\code{black}')}
#'   \item{\code{show_guide}}{correntemente ignorato. Per future estensioni.}
#' }
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
#'   # crea una mappa per la sola area centro-settentrionale:
#'   mapIT(values,sub=0:12)
#'   
#'   # crea id regionali
#'   # e' indifferente usare maiuscole e minuscole
#'   # i caratteri quali trattini e apostrofi posso essere ignorati
#'   id = c('Piemonte', 'VALLE DAOSTA', 'lOMBARDiA', 'Trentino Alto Adige', 'VENETO', 'FRiULi - VENEZiA GiULiA', 'LiGURiA', 'EMiLiAROMAGNA', 'TOSCANA', 'UMBRiA', 'MARCHE', 'LAZiO', 'ABRUZZO', 'MOLiSE', 'CAMPANiA', 'PUGLiA', 'BASiLiCATA', 'CALABRiA', 'SiCiLiA', 'SARDEGNA')
#'   
#'   # crea la mappa
#'   mapIT(values = values, id = id, guide.label = "Valori")
#'   
#'   # modifica i colori
#'   mapIT(values = values, id = id, guide.label = "Valori", graphPar = list(low = "#00ff00", high = "#ff0000"))
#'   
#'   ### utilizza dati categoriali
#'   values = c(rep("Nord-Ovest", 4), rep("Nord-Est", 4), rep("Centro", 4), rep("Sud", 6), rep("Isole", 2))
#'   id = c('Piemonte', 'VALLE DAOSTA', 'lOMBARDiA', 'LiGURiA', 'Trentino Alto Adige', 'VENETO', 'FRiULi - VENEZiA GiULiA', 'EMiLiAROMAGNA', 'TOSCANA', 'UMBRiA', 'MARCHE', 'LAZiO', 'ABRUZZO', 'MOLiSE', 'CAMPANiA', 'PUGLiA', 'BASiLiCATA', 'CALABRiA', 'SiCiLiA', 'SARDEGNA')
#'   
#'   # funzione minimale per creare la mappa
#'   mapIT(values = values, id = id, guide.label = "Valori")
#'   
#    # aggiungi i colori alle mappe
#'   mapIT(values = values, id = id, guide.label = "Aree geografiche", graphPar = list(colours = c("red", "darkblue", "green", "yellow", "purple")))
#'   
#'   # utilizzando i set di colori predefiniti, si ottiene un risultato migliore
#'   mapIT(values = values, id = id, guide.label = "Aree geografiche", graphPar = list(palette = "Dark2"))
#'   
#'   # modificando i dati affinche' le macroaree siano un fattore ordinato, si ottiene una legenda e una colorazione piu' coerente
#'   values = factor(values, levels = c("Nord-Ovest", "Nord-Est", "Centro", "Sud", "Isole"), ordered = TRUE)
#'   mapIT(values = values, id = id, guide.label = "Aree geografiche")


mapIT <- function(
  values, id, data, detail = "regions", dataSource = "istat", sub = NULL, 
  discrete = NULL, 
  graphPar = list(
    guide.label = NULL, title = NULL,
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
  
  ### If the label for the legend is not specified
  if(is.null(graphPar$guide.label)) graphPar$guide.label <- deparse(substitute(values))
  
  ### If data exists then search values and id as data columns
  if(!missing(data)) {
    values <- data[,deparse(substitute(values))]
    if(!missing(id)) id <- data[,deparse(substitute(id))]
  }
  
  ### If id is missing then assign numbers from 0
  if(missing(id)) {id <- 0:(length(values)-1)}
  
  ### Transform values to factor
  if(is.numeric(values)) {
    discrete <- FALSE
  } else {
    discrete <- TRUE
    values <- as.factor(values)
  }
  
  ### If data argument is not specified
  if(missing(data)) {
    assign("data", data.frame(values,id))
  }
  
  ### If guide.label contains $, keep the second part
  if(grepl("\\$", graphPar$guide.label)) {
    graphPar$guide.label <- unlist(strsplit(graphPar$guide.label, "\\$"))[2]
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
  if(is.numeric(id)) {
    pos <- match(shapedata$id, as.character(id))
    ck <- match(as.character(id), shapedata$id)
  } else { 
    pos <- match(onlyChar(shapedata$region), onlyChar(id))
    ck <- match(onlyChar(id), onlyChar(shapedata$region))
  }
  
  ### Select 'sub' regions
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
  lab <- labs(x = "", y = "", title=graphPar$title)
  out <- gp + bg + th + map + lab
  if(discrete == TRUE) {
    if(is.null(graphPar$colours)) {
      scf <- scale_fill_brewer(labels=levels(as.factor(values)), palette = graphPar$palette, name=graphPar$guide.label)
    } else {
      scf <- scale_fill_manual(values = graphPar$colours, name=graphPar$guide.label)
    }
  } else {
      scf <- scale_fill_continuous(low = graphPar$low, high = graphPar$high, name=graphPar$guide.label)
  }
  
  return(out+scf)
}
