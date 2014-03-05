library(ggplot2)
load("data/shapefile_istat_regioni.rda")


values = runif(20,min=0,max=1000)          # crea dati sintetici
values = c(rep(1,10), rep(2,10))
id = 0:19                                  # crea i codici regionali

id = c('Piemonte', 'VALLE D\'AOSTA', 'lOMBARDIA', 'TRENTINO-ALTO ADIGE', 'VENETO', 'FRIULI-VENEZIA GIULIA', 'LIGURIA', 'EMILIA-ROMAGNA', 'TOSCANA', 'UMBRIA', 'MARCHE', 'LAZIO', 'ABRUZZO', 'MOLISE', 'CAMPANIA', 'PUGLIA', 'BASILICATA', 'CALABRIA', 'SICILIA', 'SARDEGNA')

df = data.frame(values3=values, id3=id)

source('~/Dropbox/quantide/dev/mapIT/R/mapIT.R')
mapIT(values=values, id=id, guide.label="Valori") # crea la mappa



shape=shapedata
shape=shape[shape$region == "VENETO", ]
mapIT(values=values, id=id, dataSource=shape, guide.label="Valori") # crea la mappa
names(shape)


