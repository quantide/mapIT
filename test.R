library(ggplot2)
load("data/shapefile_istat_regioni.rda")
load("~/Desktop/regionsIT.rda")
shapedata_istat_regioni = regionsIT

shapedata_istat_regioni$id = as.numeric(shapedata_istat_regioni$id)

str(shapedata_istat_regioni)
save(shapedata_istat_regioni, file="data/shapefile_istat_regioni.rda")


values = c(1:20)
values = runif(20,min=0,max=1000)          # crea dati sintetici
id = 0:19                                  # crea i codici regionali

id = c('Piemonte', 'VALLE D\'AOSTA', 'lOMBARDIA', 'TRENTINO-ALTO ADIGE', 'VENETO', 'FRIULI-VENEZIA GIULIA', 'LIGURIA', 'EMILIA-ROMAGNA', 'TOSCANA', 'UMBRIA', 'MARCHE', 'LAZIO', 'ABRUZZO', 'MOLISE', 'CAMPANIA', 'PUGLIA', 'BASILICATA', 'CALABRIA', 'SICILIA', 'SARDEGNA')

df = data.frame(values3=values, id3=id)

source('~/Dropbox/git/dev/mapIT/R/mapIT.R')

library(mapIT)






library(mapIT)

values = runif(20,min=0,max=1000)          # crea dati sintetici
id = c('Piemonte', 'VALLE D\'AOSTA', 'lOMBARDIA', 'TRENTINO-ALTO ADIGE', 'VENETO', 'FRIULI-VENEZIA GIULIA', 'LIGURIA', 'EMILIA-ROMAGNA', 'TOSCANA', 'UMBRIA', 'MARCHE', 'LAZIO', 'ABRUZZO', 'MOLISE', 'CAMPANIA', 'PUGLIA', 'BASILICATA', 'CALABRIA', 'SICILIA', 'SARDEGNA')


mapIT(values=values, id=id, guide.label="Aree geografiche")
mapIT(values=values, id=id, guide.label="Aree geografiche", graphPar = list(colours = c("red", "darkblue", "green", "yellow", "purple")))

values = c(rep("Nord-Ovest", 4), rep("Nord-Est", 4), rep("Centro", 4), rep("Sud", 6), rep("Isole", 2))
id = c('Piemonte', 'VALLE DAOSTA', 'lOMBARDIA', 'LIGURIA', 'Trentino Alto Adige', 'VENETO', 'FRIULI - VENEZIA GIULIA', 'EMILIAROMAGNA', 'TOSCANA', 'UMBRIA', 'MARCHE', 'LAZIO', 'ABRUZZO', 'MOLISE', 'CAMPANIA', 'PUGLIA', 'BASILICATA', 'CALABRIA', 'SICILIA', 'SARDEGNA')
values = factor(values, levels = c("Nord-Ovest", "Nord-Est", "Centro", "Sud", "Isole"), ordered=TRUE)
mapIT(values=values, id=id, guide.label="Areegeografiche", graphPar = list(title = "title", palette = "Dark2"))
mapIT(values=runif(20,min=0,max=1000), id=id, guide.label="Valori", graphPar = list(low = "#000000", high = "#ff0000"))
mapIT(values=runif(20,min=0,max=1000), guide.label="Valori") # crea la mappa


shape=shapedata
shape=shape[shape$region == "PIEMONTE", ]
mapIT(values=45, id="PIEMONTE", dataSource=shape, guide.label="Valori") # crea la mappa
names(shape)


