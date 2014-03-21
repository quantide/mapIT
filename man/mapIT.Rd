\name{mapIT}
\alias{mapIT}

\title{Choropleth Maps of Italy}

\description{
Creazione di mappe (coroplete) delle regioni italiane in maniera semplice.
}

\usage{
mapIT(values, id = NULL, data = NULL, detail = "regions", dataSource = "istat", sub = NULL, guide.label = deparse(substitute(values)), graphPar = list(low = "#f0f0f0", high = "#005096", title = NULL, theme = theme_minimal(), themeOption = list(title = element_text(size = 18), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()), borderCol = "black", show_guide = TRUE, legendTitle = guide.label))
}

\arguments{
  \item{values}{Vettore numerico contenente i valori da associare a ciascuna regione. Può essere indicato un oggetto di tipo vettore oppure una stringa indicante la colonna del data.frame specificato in \code{data}.}
  \item{id}{
Vettore contenente i nomi delle regioni a cui associare i valori di \code{values}. Spazi e altri caratteri non alfabetici vengono ignorati. Questo significa che si può scrivere indifferentemente: 'Trentino-Alto Adige', 'Trentino Alto Adige' o 'TrentinoAltoAdige'. E' indifferente usare lettere maiuscole o minuscole. Per le regioni con denominazione bilingue, viene riconosciuta la sola dicitura in italiano. Può essere indicato un oggetto di tipo vettore oppure una stringa indicante la colonna del data.frame specificato in \code{data}. Se \code{id} è mancante, i valori contenuti in \code{values} sono associati alle regioni nel seguente ordine: Piemonte, Valle d'Aosta, Lombardia, Trentino-Alto Adige, Veneto, Friuli-Venezia Giulia, Liguria, Emilia-Romagna, Toscana, Umbria, Marche, Lazio, Abruzzo, Molise, Campania, Puglia, Basilicata, Calabria, Sicilia, Sardegna.
}
  \item{data}{
Specifica il data.frame in cui sono contenuti \code{values} e \code{id}. Può essere omesso nel caso in cui \code{values} e \code{id} siano due vettori.
}
  \item{detail}{
Dettaglio con cui disegnare la mappa. Aggiunto per future estensioni, attualmente l'unico valore ammesso è quello di default, '\code{regions}'. Valori diversi vengono ignorati.
}
  \item{dataSource}{
Fonte dei dati. Se uguale ad '\code{istat}' (default), vengono usati i dati forniti dall'ISTAT e contenuti nel package. Altrimenti un data.frame con la stessa struttura dei dati ISTAT può essere utilizzato. 
}
  \item{sub}{
 Correntemente ignorato. Per future estensioni.
}
  \item{guide.label}{
Una stringa contenente un'etichetta per i dati. Se mancante, il nome dell'oggetto contenente i valori \code{values} viene utilizzato.
}
  \item{graphPar}{
  
    \describe{
    Lista contenente i seguenti parametri grafici:
    
    \item{\code{low}}{solo se \code{values} assume valori continui, colore da assegnare alle regioni con valore di \code{values} più basso. (default: #F0F0F0)}

    \item{\code{high}}{solo se \code{values} assume valori continui, colore da assegnare alle regioni con valore di \code{values} più alto. (default: #005096)}
    
    \item{\code{palette}}{solo se \code{values} assume valori discreti, tipo di palette RColorBrewer da utilizzare. Viene ignorato se \code{colours} non è nullo. Per visualizzare tutte le palette disponibili, è possibile utilizzare il comando \code{RColorBrewer::display.brewer.all()}  (default: 'BuGn')}

    \item{\code{colours}}{solo se \code{values} assume valori discreti, colore da associare a ciascun valore di 'values'. Se nullo (default), viene ignorato e utilizzata una palette.}
    
    \item{\code{title}}{titolo del grafico. Se nullo (default), nessun titolo sarà visualizzato.}

    \item{\code{theme}}{tema ggplot2 da utilizzare. (default: \code{\link{theme_minimal}()})}
    
    \item{\code{themeOption}}{
      lista contenente le seguenti impostazioni relative al tema: \code{title}, \code{axis.ticks}, \code{axis.text.x}, \code{axis.text.y}. Per informazioni sul significato si veda \code{\link{theme}}.
    }
      
    \item{\code{borderCol}}{colore per i confini delle diverse aree. (default: '\code{black}')}
    
    \item{\code{show_guide}}{correntemente ignorato. Per future estensioni.}
    
    \item{\code{legendTitle}}{titolo della legenda. (default: \code{guide.label})}
    }
  }
}

\author{
Nicola Sturaro Sommacal [aut, cre], Davide Massidda [aut], Lorenzo Di Blasio [ctb].
}

\seealso{
\code{\link[ggplot2:geom_map]{geom_map}} \code{\link{shapefile_istat_regioni}}
}

\examples{
### utilizza dati continui
values = runif(20, min = 0, max = 1000)       

# funzione minimale per creare la mappa
mapIT(values = values)

# crea id regionali
# è indifferente usare maiuscole e minuscole
# i caratteri quali trattini e apostrofi posso essere ignorati
id = c('Piemonte', 'VALLE DAOSTA', 'lOMBARDIA', 'Trentino Alto Adige', 'VENETO', 'FRIULI - VENEZIA GIULIA', 'LIGURIA', 'EMILIAROMAGNA', 'TOSCANA', 'UMBRIA', 'MARCHE', 'LAZIO', 'ABRUZZO', 'MOLISE', 'CAMPANIA', 'PUGLIA', 'BASILICATA', 'CALABRIA', 'SICILIA', 'SARDEGNA')

# crea la mappa
mapIT(values=values, id=id, guide.label = "Valori")

# modifica i colori
mapIT(values=values, id=id, guide.label = "Valori", graphPar = list(low = "#00ff00", high = "#ff0000"))

### utilizza dati categoriali
values = c(rep("Nord-Ovest", 4), rep("Nord-Est", 4), rep("Centro", 4), rep("Sud", 6), rep("Isole", 2))
id = c('Piemonte', 'VALLE DAOSTA', 'lOMBARDIA', 'LIGURIA', 'Trentino Alto Adige', 'VENETO', 'FRIULI - VENEZIA GIULIA', 'EMILIAROMAGNA', 'TOSCANA', 'UMBRIA', 'MARCHE', 'LAZIO', 'ABRUZZO', 'MOLISE', 'CAMPANIA', 'PUGLIA', 'BASILICATA', 'CALABRIA', 'SICILIA', 'SARDEGNA')

# funzione minimale per creare la mappa
mapIT(values=values, id=id, guide.label = "Valori")

# aggiungi i colori alle mappe
mapIT(values=values, id=id, guide.label = "Aree geografiche", graphPar = list(colours = c("red", "darkblue", "green", "yellow", "purple")))

# utilizzando i set di colori predefiniti, si ottiene un risultato migliore
mapIT(values=values, id=id, guide.label = "Aree geografiche", graphPar = list(palette = "Dark2"))

# modificando i dati affinché le macroaree siano un fattore ordinato, si ottiene una legenda e una colorazione più coerente
values = factor(values, levels = c("Nord-Ovest", "Nord-Est", "Centro", "Sud", "Isole"), ordered=TRUE)
mapIT(values=values, id=id, guide.label = "Aree geografiche")

}

