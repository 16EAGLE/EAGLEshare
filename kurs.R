library(raster)

germany <- getData("GADM",country="DEU",level=2)
plot(germany)

prec <- getData("worldclim",var="prec",res=.5,lon=10,lat=51)
plot(prec)

prec_ger1 <- crop(prec,germany)
spplot(prec_ger1)

prec_ger2 <- mask(prec_ger1,germany)
spplot(prec_ger2)

prec_avg <- cellStats(prec_ger2,stat = "mean")
