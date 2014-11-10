mapIT
=====

Easy plot Italian maps with R and ggplot package


## Why this file is not written in English?
This package is addressed to users who want draw a choropleth map of Italy. Because users who want draw a map of Italy are usually Italian-speaking, this README and help files will be provided in Italian. Code comments and variable names are in English to maintain R standards and to facilitate code sharing among developers.

## Come installare il package
Il package non è ancora disponibile sul CRAN ma può essere installato direttamente da GitHub. 

Per l'installazione è necessario che sia installato il package `devtools`, disponibile sul CRAN.
```
install.packages("devtools")
```

Una volta installato `devtools` si può installare mapIT:
```
library(devtools)
install_github("nicolasturaro/mapIT")
```


## Perché questo package?
Il package contiene una funzione `mapIT()` utile per creare mappe cloropete, ossia mappe tematiche in cui le diverse aree assumono una colorazione differente a seconda dell'intensità del fenomeno che si sta osservando. Attualmente le uniche aree disponibili sono le regioni.

In maniera immediata, è possibile creare una mappa usando la funzione `mapIT()` con un solo argomento: il vettore contenente i valori da associare a ciascuna regione.

## Miglioramenti previsti
 - caricamento di altri shapefile per creare mappe con dettaglio maggiore.
