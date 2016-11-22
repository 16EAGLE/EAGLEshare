library(raster)

#Simulate bands and create NDVI
r1v <- runif(10000,min=0,max = 1)
r1 <- raster(nrows=100,ncols=100,vals=r1v)
r2v <- runif(10000,min=0,max = 1)
r2 <- raster(nrows=100,ncols=100,vals=r2v)
r3v <- runif(10000,min=0,max = 1)
r3 <- raster(nrows=100,ncols=100,vals=r3v)
r4v <- runif(10000,min=0,max = 1)
r4 <- raster(nrows=100,ncols=100,vals=r4v)

r_set <- stack(r1,r2,r3,r4)
ndvi <- (r_set[[4]]-r_set[[3]])/(r_set[[4]]+r_set[[3]])


#Clustering
ndvi_df <- as.data.frame(ndvi)
km <- kmeans(ndvi_df,10)

ndvi_raster <- raster(ndvi)
ndvi_raster[] <- km$cluster


#Looping
x <- 1
j <- 0
while(j < 1)
{
  j <- j+0.01
  print(j)
  Sys.sleep(x)
}