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

Drive <- "G:/"
work_path <- "/EAGLE/Biodiv_model/01_data/"
setwd(str_c(Drive,work_path))

rasterOptions(progress="text")


#source("C:/Users/Luigi/Documents/EAGLE/Pixel/pixel/MAIN_FUNC.R")
source("C:/Users/Louis/Documents/Uni/EAGLE/pixel/MAIN_FUNC.R")
loadp(c("raster","rgdal","ggplot2","maptools","dismo","ellipse","sp","randomForest",
        "mgcv","stringr","RStoolbox","caret","randomForest","rms","biomod2","gdalUtils"))


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


############ Landsat_timescan###################################################################

#LS_timescan_oldpro <- brick("Landsat/timescan/TimeScan_EAGLE_AOI_UTM_WGS84.tif")
#LS_timescan <- projectRaster(LS_timescan_oldpro,crs="+proj=longlat +datum=WGS84",method = 'ngb')  # Projection correction
#LS_timescan <- projectRaster(LS_timescan,crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",method = 'ngb')  # Projection correction
#writeRaster(LS_timescan,"Landsat/timescan/LS_timescan_repro", format="GTiff",overwrite=TRUE)
LS_timescan <- brick("Landsat/timescan/LS_timescan_repro.tif")
#writeRaster(Scene_TOA_NDVI, filename=str_c("LS_Ammersee_NDVI"), format="GTiff",overwrite=TRUE, bylayer=T)          


#study_area <- readOGR("Bay_DEM/Bay_ADM.shp")
study_area_ex <- extent(LS_timescan)


Birds <- read.csv2("birds/UebungsdatenVoegel261Einhektarplots.csv",dec = ".")

ZaunK <- Birds[, -c(4:49)]
ZaunK <- ZaunK[, -5]
ZaunK$Rasterkurz <- NULL 
ZaunK$Zaunkoenig[which(ZaunK$Zaunkoenig==0)] <- NA
ZaunK <- ZaunK[complete.cases(ZaunK),]
ZaunK$Zaunkoenig <- NULL 
#ZaunK <- raster(ZaunK)


ZaunK <- SpatialPoints(coords = ZaunK)
proj4string(ZaunK) <- CRS("+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +datum=potsdam +units=m +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7")
ZaunK <- spTransform(ZaunK, CRS("+proj=longlat +datum=WGS84")) # transform x y --> lat long
ZaunK_df <- as.data.frame(ZaunK)
ZaunK_spdf <- SpatialPointsDataFrame(ZaunK_df[c("x_gps","y_gps")],ZaunK_df)
proj4string(ZaunK_spdf) <- CRS("+proj=longlat +datum=WGS84")

writeOGR(ZaunK_spdf, "results/ZaunK_prepro", "ZaunK_spdf", driver="ESRI Shapefile", dataset_options="FORMAT=shp",overwrite=TRUE)

######################
######################

species_crop <- crop(ZaunK, extent(study_area_ex))

plot(LS_timescan[[1]])
plot(ZaunK_q, add = TRUE)
species <- ZaunK
#' Data preprocessing
#' ==============================

#' Select species records for which environmental information is available
#' -------------------------------
#' 

#species <- species_crop[complete.cases(extract(LS_timescan, species)), ]

#' Collinearity
#' -----------------------------
#' ### Visual inspection of collinearity ###
cm <- cor(getValues(LS_timescan), use = "complete.obs")
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

#' ### Select an uncorrelated subset of environmental variables ###
# env <- brick(LS_timescan[[3]], LS_timescan[[8]], LS_timescan[[13]], LS_timescan[[18]], LS_timescan[[23]], LS_timescan[[28]])
# varnames <- c("NDBI","mNDVI","NDVI", "ND57", "ND42", "ND32")
# names(env) <- varnames


i <- 3

while (i < nlayers(LS_timescan)){
  if(i == 3){
   env <- LS_timescan[[i]] 
  }else{
    env <- stack(env, LS_timescan[[i]])

  }
    i <- i+5
}

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
                                     resp.name = "ZaunK")

# c("GLM","GBM","GAM","CTA","ANN","SRE","FDA","MARS","RF","MAXENT.Phillips","MAXENT.Tsuruoka")

## Defining Model Options using default options
myBiomodOption <-BIOMOD_ModelingOptions()
##

## Creating DataSplitTable
DataSplitTable <- BIOMOD_cv(full_BiomodData, k=5, rep=5, do.full.models=F)
DataSplitTable.y <- BIOMOD_cv(full_BiomodData,stratified.cv=T, stratify="y", k=2)
colnames(DataSplitTable.y)[1:2] <- c("RUN11","RUN12")
DataSplitTable <- cbind(DataSplitTable,DataSplitTable.y)
head(DataSplitTable)
##

myBiomodModelOut_cv <- BIOMOD_Modeling( full_BiomodData,
                                     models = c("GLM","GBM","CTA","ANN","SRE","FDA","MARS","RF"),
                                     models.options = myBiomodOption,
                                     DataSplitTable = DataSplitTable,
                                     VarImport=2,
                                     models.eval.meth = c("TSS","KAPPA","ROC"),
                                     SaveObj = TRUE,
                                     modeling.id="cv_rep5",
                                     do.full.models = TRUE)

## get cv evaluations
eval <- get_evaluations(myBiomodModelOut_cv,as.data.frame=T)
eval$strat <- NA
eval$strat[grepl("13",eval$Model.name)] <- "Full"
eval$strat[!(grepl("11",eval$Model.name)|
               grepl("12",eval$Model.name)|
               grepl("13",eval$Model.name))] <- "Random"
eval$strat[grepl("11",eval$Model.name)|grepl("12",eval$Model.name)] <- "Strat"
boxplot(eval$Testing.data~ eval$strat, ylab="ROC AUC")

# myBiomodModelOut <- BIOMOD_Modeling( full_BiomodData,
#                                      models = c("GLM","GBM","CTA","ANN","SRE","FDA","MARS","RF"),
#                                      models.options = myBiomodOption,
#                                      NbRunEval=2,
#                                      DataSplit=70,
#                                      models.eval.meth = c("TSS","KAPPA","ROC"),
#                                      SaveObj = TRUE,
#                                      do.full.models = TRUE)
##
##### load individual models build during the BIOMOD_Modeling step.

#myBiomod_Maxent <- BIOMOD_LoadModels(myBiomodModelOut, models="MAXENT.Phillips")
##
##### evaluate # This function will evaluate biomod2 modelling output for given metrics (e.g 'TSS', 'ROC'...) for a given dataset
#evaluate(model, data, stat, as.array=FALSE)
##### integrated evaluation?

myBiomodModelOut@models.evaluation


##### BIOMOD_EnsembleModeling  # Create and evaluate an ensemble set of models and predictions

myBiomodEM_cv <-  BIOMOD_EnsembleModeling( modeling.output = myBiomodModelOut_cv,
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

train_biomod_cv <- BIOMOD_cv(full_BiomodData, k = 5, repetition = 5, do.full.models = TRUE,
                        stratified.cv = FALSE, stratify = "both", balance = "pres")


###### BIOMOD_Projection "For  all  the  models  currently  implemented, biomod2 is  able  to  project  potential  distributions  of
#      species in other areas, other resolutions or other time scales.

#pro_var <- brick(fulldata[[2]],fulldata[[3]],fulldata[[4]],fulldata[[5]],fulldata[[6]],fulldata[[7]])

pro_var <- stack(env)
# pro_var_repro <- projectRaster(pro_var,crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",method = 'ngb')  # Projection correction
# pro_var_repro <- stack(pro_var)
# 
# gdalwarp(pro_var,dstfile="pro_var_warp",t_srs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",output_Raster = TRUE,
#          tr(c(0.000262,0.000262)), r ("bilinear"))

myBiomodProjection_cv <- BIOMOD_Projection(modeling.output = myBiomodModelOut_cv,
                                        new.env = pro_var,
                                        proj.name = "current",
                                        selected.models ="all",
                                        binary.meth ='TSS',
                                        compress = FALSE,
                                        build.clamping.mask = FALSE)

##### BIOMOD_EnsembleForcasting  #

myBiomodEMProjection_cv <- BIOMOD_EnsembleForecasting( projection.output = myBiomodProjection_cv,
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
xx <- get_predictions(myBiomodProjection_cv)
## cretae an output directory
dir.create("tiff_proj_cv", showWarnings = F)
## resave all layers of our raster stack as 'tiff'
writeRaster(xx,
            format = 'GTiff',
            filename = "tiff_proj_cv/proj_current",
            bylayer=T,
            suffix='names',
            overwrite=TRUE)
## save EM
xx_EM <- get_predictions(myBiomodEMProjection)
writeRaster(xx_EM,
            format = 'GTiff',
            filename = "tiff_proj_cv/proj_current",
            bylayer=T,
            suffix='names',
            overwrite=TRUE)
############################################################################### 
