#' Species distribution models (SDM) using buffalo data from GBIF
#' ============================================
#' [March 2016](www.animove.org, www.ecosens.org)  
#' B. Reineking, M. Wegmann  
#' Occurrence data source: [gbif](http://www.gbif.org)  
#' Environmental data source: [worldclim](http://www.worldclim.org)  
#' Algorithms: GAM, RF, MaxEnt
#' 
#' Overview
#' ===============================
#' - Set working directory and load packages
#' - Import data
#' - Data preprocessing
#'  * Presence records for which environmental data is available
#'  * Collinearity
#' - Modelling
#' - Model evaluation
#'  * Model performance on test data
#'  * Variable importance
#'  * Response functions
#' - Model predictions
#' 

#' Set working directory and load packages
#' ==========================================
#' Set the working directory. You have to use your own path.
# setwd("/set/path/to/your/script/")

Drive <- "F:/"
work_path <- "/EAGLE/Biodiv_model/01_data/"
setwd(str_c(Drive,work_path))

rasterOptions(progress="text")

#' Package rms provides function val.prob for evaluation of model performance; 
#' load before raster to avoid name conflicts with mask (alternatively: use raster::mask, 
#' i.e. explicitly specify that you want to use function mask from namespace raster)
#' 
#' We assume that the file "varImpBiomod.R" is in the working directory
source("varImpBiomod.R")

source("C:/Users/Luigi/Documents/EAGLE/Pixel/pixel/MAIN_FUNC.R")
#source("C:/Users/Louis/Documents/Uni/EAGLE/pixel/MAIN_FUNC.R")
loadp(c("raster","rgdal","ggplot2","maptools","dismo","ellipse","sp","randomForest",
        "mgcv","stringr","RStoolbox","caret","randomForest","rJava","rms","biomod2","gdalUtils"))


#' Import data: Environment and species occurrences
#' ====================================================
#' Read vector layer with study area
#' ----------------------------------
#' Notes:
#' - './' refers to the current working directory, i.e. we 
#' are specifying as the first argument of readOGR, the dsn, the subdirectory "GIS" within the current working directory
#' - make sure there is no trailing '/' in the value of the dsn argument, i.e. do **not** use "./GIS/"
#' - when importing shapefiles, drop the suffix from the layer argument, i.e. do **not** use "africa_dissolved.shp"
#' 

#study_area <- getData('GADM', country='Fra', level=1)

 study_area <- readOGR("Bay_DEM/Bay_ADM.shp")
 study_area_ex <- extent(study_area)

germany_shp <- readOGR("Germ_ad/Germ_ad.shp")
germany_ex <- extent(germany_shp)


#' Download and read bioclim variables
#' -----------------------------------
#' The function getData will load the data from the working directory, if available. If they are not available,
#' the function will attempt to download the data from the internet.
#' For a definition of the variables, see [http://www.worldclim.org/bioclim](http://www.worldclim.org/bioclim)
#' 
#' Variable | Description
#' -------- | -----------
#' BIO1 | Annual Mean Temperature
#' BIO2 | Mean Diurnal Range (Mean of monthly (max temp - min temp))
#' BIO3 | Isothermality (BIO2/BIO7) (* 100)
#' BIO4 | Temperature Seasonality (standard deviation *100)
#' BIO5 | Max Temperature of Warmest Month
#' BIO6 | Min Temperature of Coldest Month
#' BIO7 | Temperature Annual Range (BIO5-BIO6)
#' BIO8 | Mean Temperature of Wettest Quarter
#' BIO9 | Mean Temperature of Driest Quarter
#' BIO10 | Mean Temperature of Warmest Quarter
#' BIO11 | Mean Temperature of Coldest Quarter
#' BIO12 | Annual Precipitation
#' BIO13 | Precipitation of Wettest Month
#' BIO14 | Precipitation of Driest Month
#' BIO15 | Precipitation Seasonality (Coefficient of Variation)
#' BIO16 | Precipitation of Wettest Quarter
#' BIO17 | Precipitation of Driest Quarter
#' BIO18 | Precipitation of Warmest Quarter
#' BIO19 | Precipitation of Coldest Quarter
#' 

############ Landsat_timescan###################################################################

LS_timescan_oldpro <- brick("Landsat/timescan/TimeScan_EAGLE_AOI_UTM_WGS84.tif")
LS_timescan <- projectRaster(LS_timescan,crs="+proj=longlat +datum=WGS84",method = 'ngb')  # Projection correction
LS_timescan <- projectRaster(LS_timescan,crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",method = 'ngb')  # Projection correction


#study_area <- readOGR("Bay_DEM/Bay_ADM.shp")
study_area_ex <- extent(LS_timescan)


bio <- raster::getData("worldclim", var = "bio", res = 10)

#' Plot the first raster layer, i.e. annual mean temperature
plot(raster(bio, 1))
# Add the outline of the study area
plot(germany_shp, add=TRUE)

#' Crop to study area extent (with a 5 degree buffer in each direction)
biocrop <- crop(bio, extent(germany_shp) + 5)

#' Plot the first raster layer of the cropped climate data
plot(raster(biocrop, 1))
plot(germany_shp, add=TRUE)

# species_germ <- readOGR("./FagusL/FagusL_germ.mif", layer = "FagusL_germ")

#####################################################################
#' Read occurrence points. If they exist locally, use the local file.
#' If they do not exist, download from [gbif](http://www.gbif.org) 
if (file.exists("./FagusL/FagusL.mif")) {
  species <- readOGR("./FagusL/FagusL.mif", layer = "FagusL")
} else {
  # Download species location data from gbif
  # species <- gbif("Syncerus", "caffer", ext = extent(bio), sp = TRUE, removeZeros = TRUE)
  species0 <- gbif("Fagus", species="sylvatica L.", ext = study_area_ex, sp = TRUE, removeZeros = TRUE)
  species <- subset(as.data.frame(species0) ,select=c("lat","lon"))
  species <- na.omit(species)
  species <- SpatialPointsDataFrame(species[c("lon","lat")],species)
  
  coordinates(species) <- c("lon", "lat")  # set spatial coordinates
  
  
  # Add projection information
  proj4string(species) <- CRS("+proj=longlat +datum=WGS84")
#  proj4string(species) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  species <- SpatialPointsDataFrame(species)
  # Save species records in mif-format (preserves full column names)
  writeOGR(species, "FagusL", "FagusL", driver="MapInfo File", dataset_options="FORMAT=MIF",overwrite=TRUE)
  }

species_crop <- crop(species_germ, extent(study_area_ex))

plot(LS_timescan[[1]])
plot(species_crop, add = TRUE)

#' Data preprocessing
#' ==============================

#' Select species records for which environmental information is available
#' -------------------------------
#' 

species <- species_crop[complete.cases(extract(LS_timescan, species_crop)), ]

#' Collinearity
#' -----------------------------
#' ### Visual inspection of collinearity ###
cm <- cor(getValues(LS_timescan), use = "complete.obs")
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

#' ### Select an uncorrelated subset of environmental variables ###
env <- brick(LS_timescan[[3]], LS_timescan[[8]], LS_timescan[[13]], LS_timescan[[18]], LS_timescan[[23]], LS_timescan[[28]])
varnames <- c("NDBI","mNDVI","NDVI", "ND57", "ND42", "ND32")
names(env) <- varnames


#' Sampling of (pseudo-)absence points
#' ====================================================
#' The function randomPoints in package dismo allows to 
#' randomly select a certain number of random points,
#' and to adjust the probability of selecting a cell
#' according to its size, which is relevant in lat-lon-grids,
#' where cells are of differing size

#' Selecting 2000 random background points, excluding cells where
#' the species is present
set.seed(2) #  for reproducible records
background <- randomPoints(env, 2000, species)
#' Select only one presence record in each cell of the environmental layer
presence <- gridSample(species, env, n = 1)

#' 
#' Now we combine the presence and background points, adding a 
#' column "species" that contains the information about presence (1)
#' and background (0)
fulldata <- SpatialPointsDataFrame(rbind(presence, background),
                data = data.frame("species" = rep(c(1,0), 
                                  c(nrow(presence), nrow(background)))),
                                   match.ID = FALSE,
                                   proj4string = CRS(projection(env)))
#' Add information of environmental conditions at point locations
fulldata@data <- cbind(fulldata@data, extract(env, fulldata))


#' 
# Split data set into a training and test data set
set.seed(2)
fold <- kfold(fulldata, k = 5)
traindata <- fulldata[fold != 1, ]
testdata <- fulldata[fold == 1, ]

#' We can now use a range of statistical methods to estimate the
#' probability of species occurrence.
#' Unfortunately, there are often subtle differences in how the models
#' are specified and in which data formats are useable



## Generalized Linear Model

## Generalized additive models
gammodel <- gam(species ~ s(NDBI) + s(NDVI) + s(ND57) + s(ND42) + s(ND32),
         family="binomial", data=traindata)
summary(gammodel)

plot(gammodel)

# Now we should do model selection: bio14 does not contribute to the fit

# Evaluate model on test data
# a) Predict to test data
gamtest <- predict(gammodel, newdata = testdata, type = "response")
# b) Calculate performance indices
val.prob(gamtest, testdata[["species"]])

### Calculate variable importance
varImpBiomod <- function(model, varnames, data, n=10) {
  # This calculation of variable importance is based on
  # the Biomod-Tutorial by Wilfried Thuiller
  varImp <- numeric(length(varnames))
  names(varImp) <- varnames
  base.pred <- predict(model, type="response", newdata=data)
  for (var in varnames) {
    varimp.test <- data
    tmp <- numeric(n)
    for (i in 1:n) {
      varimp.test[[var]] <- sample(varimp.test[[var]])
      tmp.pred <- predict(model, type="response", newdata=varimp.test)
      tmp[i] <- cor(base.pred, tmp.pred)
    }	
    varImp[var] <- mean(tmp)
  }
  1-varImp
}

# Variable importance
gamimp <- varImpBiomod(gammodel, varnames,
             traindata)
barplot(100 * gamimp/sum(gamimp), ylab = "Variable importance (%)")

# Response functions
plot(gammodel, pages = 1)

# png("gammodel_resp.png", 800, 800)
# plot(gammodel, pages = 1)
# dev.off()

# Prediction map
gammap <- predict(env, gammodel, type = "response")

plot(gammap)


## Random forest

# randomForest requires the dependent variable to be a factor
# if we want to do classification
rftraindata <- as(traindata, "data.frame")
rftraindata$species <- factor(rftraindata$species)

# TODO: check proper settings of random forest algorithm
rfmodel <- randomForest(species ~ bio1 + bio7 + bio10 + bio14 + bio15, data = rftraindata)

# Evaluate model on test data
# a) Predict to test data
rftest <- predict(rfmodel, newdata = testdata, type = "prob")[,2]
# b) Calculate performance indices
val.prob(rftest, testdata[["species"]])

# Variable importance
rfImp <- importance(rfmodel)
varImpPlot(rfmodel)

# Response functions
par(mfrow=c(3,2))
for (i in seq_along(varnames)) {
  partialPlot(rfmodel, rftraindata, varnames[i], xlab = varnames[i], main="")  
}

# Prediction map
rfmap <- predict(env, rfmodel, type = "prob", index = 2)
par(mfrow=c(1, 1))
plot(rfmap)

## Maxent (Maximum Entropy)
# The following code assumes that the column with the species information
# is in the first position
maxentmodel <- maxent(traindata@data[, -1], traindata[["species"]], 
                args = c("nothreshold", 
                         "nohinge"))

# Model evaluation on test data
maxenttest <- predict(maxentmodel, testdata)
val.prob(maxenttest, testdata[["species"]])

# Alternatively, we can use the evaluate function
maxente <- evaluate(p = maxenttest[testdata[["species"]] == 1],
              a = maxenttest[testdata[["species"]] == 0])

# Show variable importance
plot(maxentmodel)

# Plot response functions
response(maxentmodel)

# Prediction map
maxentmap <- predict(maxentmodel, env)
plot(maxentmap)

# Plot predictions of several methods, using the same
# colour scheme
par(mfrow = c(3, 1), mar = c(3, 3, 1, 1))
brks <- seq(0, 1, by = 0.1)
arg <- list(at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2))
col <- rev(terrain.colors(length(brks) - 1))
# col <- rev(heat.colors(length(brks) - 1)) # different colors # col <- rev(rainbow(length(brks) - 1))
plot(gammap, breaks = brks, col = col, axis.args = arg, main="gammap")
plot(study_area, add=T)
plot(rfmap, breaks = brks, col = col, axis.args = arg, main="rfmap")
plot(study_area, add=T)
plot(maxentmap, breaks = brks, col = col, axis.args = arg, main="maxentmap")
plot(study_area, add=T)


####################### Biomod2 ########################################################

##### import /formate data to fit for Biomod2

expl_var <- as.data.frame(fulldata[,(2:7)])

full_BiomodData <- BIOMOD_FormatingData(resp.var = as.numeric(fulldata$species),
                                     expl.var = expl_var[,(1:6)],
                                     resp.xy = as.matrix(fulldata@coords),
                                     resp.name = "FagusL")

# c("GLM","GBM","GAM","CTA","ANN","SRE","FDA","MARS","RF","MAXENT.Phillips","MAXENT.Tsuruoka")

myBiomodOption <-BIOMOD_ModelingOptions()

myBiomodModelOut <- BIOMOD_Modeling( full_BiomodData,
                                     models = c("GLM","GBM","CTA","ANN","SRE","FDA","MARS","RF","MAXENT.Phillips"),
                                     models.options = myBiomodOption,
                                     NbRunEval=2,
                                     DataSplit=70,
                                     models.eval.meth = c("TSS","KAPPA","ROC"),
                                     SaveObj = TRUE,
                                     do.full.models = TRUE)

##### load individual models build during the BIOMOD_Modeling step.

myBiomod_Maxent <- BIOMOD_LoadModels(myBiomodModelOut, models="MAXENT.Phillips")

##### BIOMOD_EnsembleModeling  # Create and evaluate an ensemble set of models and predictions

myBiomodEM <-  BIOMOD_EnsembleModeling( modeling.output = myBiomodModelOut,
                                        chosen.models = "all",
                                        em.by = "all",eval.metric = c("TSS"),
                                        eval.metric.quality.threshold = c(0.7),
                                        models.eval.meth = c("TSS","ROC"),
                                        prob.mean = TRUE,
                                        prob.cv = FALSE,
                                        prob.ci = FALSE,
                                        prob.ci.alpha = 0.05,
                                        prob.median = FALSE,
                                        committee.averaging = FALSE,
                                        prob.mean.weight = TRUE,
                                        prob.mean.weight.decay = "proportional" )
get_evaluations(myBiomodEM)



###### test training using biomod2

train_biomod <- BIOMOD_cv(full_BiomodData, k = 5, repetition = 5, do.full.models = TRUE,
                        stratified.cv = FALSE, stratify = "both", balance = "pres")


###### BIOMOD_Projection "For  all  the  models  currently  implemented, biomod2 is  able  to  project  potential  distributions  of
#      species in other areas, other resolutions or other time scales.

#pro_var <- brick(fulldata[[2]],fulldata[[3]],fulldata[[4]],fulldata[[5]],fulldata[[6]],fulldata[[7]])

pro_var <- stack(env)
pro_var_repro <- projectRaster(pro_var,crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",method = 'ngb')  # Projection correction
pro_var_repro <- stack(pro_var)

gdalwarp(pro_var,dstfile="pro_var_warp",t_srs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",output_Raster = TRUE,
         tr(c(0.000262,0.000262)), r ("bilinear"))

myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                        new.env = pro_var,
                                        proj.name = "current",
                                        selected.models ="all",
                                        binary.meth ='TSS',
                                        compress = FALSE,
                                        build.clamping.mask = FALSE)

##### BIOMOD_EnsembleForcasting  #

myBiomodEMProjection <- BIOMOD_EnsembleForecasting( projection.output = myBiomodProjection,
                            EM.output = myBiomodEM)

###### BIOMOD_RangeSize ???

###### BIOMOD_tuning ??? Function to tune biomod single models parameters

###### calculate.stat ?? Calculate evaluation metrics based on a misclassification table


###### response.plot2 # Function for for plotting predicted responses from species distribution models in 2 or 3 dimensions

# response.plot2( c("GLM","GBM","CTA","ANN","SRE","FDA","MARS","RF"),
#                 Data = get_formal_data(myBiomodModelOut, expl_var[,(1:6)]),
#                 do.bivariate = FALSE,
#                 fixed.var.metric = "mean",
#                 save.file="tiff",
#                 name="BiomodResponse_curve",
#                 ImageSize=600,
#                 plot=TRUE)

##### Save projectons as GTiff  ##############################################
## get models projections raster stack
xx <- get_predictions(myBiomodProjection)
## cretae an output directory
dir.create("tiff_proj", showWarnings = F)
## resave all layers of our raster stack as 'tiff'
writeRaster(xx,
            format = 'GTiff',
            filename = "tiff_proj/proj_current",
            bylayer=T,
            suffix='names' )
############################################################################### 
