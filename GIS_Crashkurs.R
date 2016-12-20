#https://github.com/Markuless64/Programming
#GIS Crashcourse in R
#author: Marcus Groll
#date: 20.12.2016
# you´ll need the rapideye scene & the catchment.shp of Christophers course
install.packages("dismo")
library(dismo)
install.packages("rgdal")
library(rgdal)
install.packages("maps")
library(maps)
install.packages("mapdata")
library(mapdata)
install.packages("raster")
library(raster)
#hallo zusammen

#don´t forget the working directory
setwd()
getwd()

#Creating very easy & quick a map
#package dismo
#package maps
#package mapdata
x11()
map("worldHires", "Germany", col = "red", fill=F)
map("worldHires", "Austria",col = "blue",add = T, fill=T)
map("worldHires", "France",col = "Green",add = T, fill=F)
map("worldHires", "Belgium",col = "black",add = T, fill=T)
map("worldHires", "Luxembourg",col = "grey",add = T, fill=T)

savePlot(filename = "Europe", type = c("jpg"))

#you can also generate some Google Earth maps very easy
#package dismo
mymap <- gmap("Germany")  # choose whatever country
plot(mymap)

#zoom in map
mymap <- gmap("Germany", exp = 0.4)
plot(mymap)

#zoom out map
mymap <- gmap("Germany", exp = 3)
plot(mymap)

# generate satellite picture
mymap <- gmap("Germany", type = "satellite")
plot(mymap)

#load and plot raster
#package raster
#package rgdal
allbands  <- brick("F:/Uni/EAGLE/MB1/01_Data/02_Raster/RapidEye_Ammersee/re20110906_ortho_ls_prj_grto0509_sub.tif")
plotRGB(allbands, 3,2,1, stretch="lin")

band1 <- ("F:/Uni/EAGLE/MB1/01_Data/02_Raster/Landsat-5_Ammersee/LT51930272003195MTI01/LT51930272003195MTI01_B1.TIF")
band2 <- ("F:/Uni/EAGLE/MB1/01_Data/02_Raster/Landsat-5_Ammersee/LT51930272003195MTI01/LT51930272003195MTI01_B2.TIF")
band3 <- ("F:/Uni/EAGLE/MB1/01_Data/02_Raster/Landsat-5_Ammersee/LT51930272003195MTI01/LT51930272003195MTI01_B3.TIF")
band4 <- ("F:/Uni/EAGLE/MB1/01_Data/02_Raster/Landsat-5_Ammersee/LT51930272003195MTI01/LT51930272003195MTI01_B4.TIF")
band5 <- ("F:/Uni/EAGLE/MB1/01_Data/02_Raster/Landsat-5_Ammersee/LT51930272003195MTI01/LT51930272003195MTI01_B5.TIF")
band6 <- ("F:/Uni/EAGLE/MB1/01_Data/02_Raster/Landsat-5_Ammersee/LT51930272003195MTI01/LT51930272003195MTI01_B6.TIF")
band7 <- ("F:/Uni/EAGLE/MB1/01_Data/02_Raster/Landsat-5_Ammersee/LT51930272003195MTI01/LT51930272003195MTI01_B7.TIF")

gesamt_pic <- stack(band1,band2,band3,band4,band5,band6,band7)
plot(gesamt_pic)

b <- shapefile("F:/Uni/All.shp")
a <- readOGR("F:/Uni","All")

summary(a)
summary(b)

#drawing polygon
#only possible in the lower right side
#package raster
new_poly <- drawPoly(show = T, col = "blue")
summary(new_poly)
proj4string(new_poly) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
summary(new_poly)

plot(new_poly)

spp <-SpatialPolygonsDataFrame(new_poly,data=as.data.frame("new_poly"))
writeOGR(spp,"Shape", "new_data",driver="ESRI Shapefile")

View(spp)

new_poly1 <- drawPoly(show = T, col = "orange")
proj4string(new_poly1) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

#clip function
#package raster
c <- crop(allbands, new_poly)
d <- crop(allbands, new_poly1)

plotRGB(c, 3,2,1, stretch="lin")
plotRGB(d, 3,2,1, stretch="lin")
writeRaster(c, datatype = "FLT4S", filename = "newdata.tif", format = "GTiff", overwrite = T)

new <- brick("F:/Uni/EAGLE/Program/Program_1/newdata.tif")
plotRGB(new, 3,2,1, stretch = "lin")

#mask function
catchment <- shapefile("F:/Uni/EAGLE/MB1/01_Data/01_Vector/catchments.shp")
plot(catchment)
plotRGB(allbands, 3,2,1, stretch="lin")
e <- mask(allbands,catchment)
plotRGB(e, 3,2,1, stretch="lin")

#merge function
f <- merge(c, d)
plotRGB (f, 3,2,1, stretch="lin")

#buffer function
#package raster
g <- buffer(new_poly, width=299990)
plot(g, col="blue")
plot(new_poly, col = "red", add = T)