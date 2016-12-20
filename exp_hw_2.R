#Needed packages (do not change)
packages_required <- c("raster","RStoolbox","rgdal","randomForest","e1071")

#Define output handling
log_sign <- "[LOG]: "
out <-  function(input){
  print(paste(log_sign,input))
}

#Define error handling for missing packages
loadp <- function() {
  out("Loading packages...")
  n_packages <- length(packages_required)
  for(p in 1:n_packages){
    p_lib <- try(library(packages_required[p],character.only = TRUE))
    p_lib_status <- FALSE
    if(class(p_lib) == "try-error"){
      out(paste0("Could not find the package '",packages_required[p],"'."))
      p_lib_status <- TRUE
    }
    if(p_lib_status == TRUE){
      out(paste0("Installing package '",packages_required[p],"'..."))
      install.packages(packages_required[p])
      library(packages_required[p],character.only = TRUE)
    }
  }
}

loadp()

dir <- "D:\\jsw\\05_MB1_GIS\\data\\wd\\"
setwd(dir)

aoi <- readOGR(c(paste0(dir,"catchments.shp")))
r1 <- raster(c(paste0(dir,"MULTI_TOAref.TIF")))

r1c <- crop(r1,aoi)
r1m <- mask(r1c,aoi)

train <- readOGR(c(paste0(dir,"class_training.shp")))
train_repro <- spTransform(train, crs(raster))

scl <- superClass(r1m, trainData = train_repro,responseCol = "id")


legend(c("Health","Defense"))








#indi <- spectralIndices(r_set, blue = "LT51930272003195MTI01_B1", green = "LT51930272003195MTI01_B2", red = "LT51930272003195MTI01_B3", nir = "LT51930272003195MTI01_B4", indices = c("NDVI","MSAVI"))
#cl <- unsuperClass(r_set,nSamples = 1000,nClasses = 5,nStarts = 25)
#x <- stack(b1,b2,b3,b4,b5,b6,b7)
#writeRaster(x,filename = "stack.tif", format="GTiff",overwrite=FALSE)




#r_set <- stack(r1,r2)
#ndvi <- overlay(r2[],r1[])

#Clustering
#ndvi_df <- as.data.frame(ndvi)
#km <- kmeans(ndvi_df,10)

#ndvi_raster <- raster(ndvi)
#ndvi_raster[] <- km$cluster

#Looping
#x <- 1
#j <- 0
#while(j < 1)
#{
#  j <- j+0.01
#  print(j)
#  Sys.sleep(x)
#}