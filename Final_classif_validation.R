library(raster)
library(rgdal)
library(RStoolbox)
library(sp)
library(randomForest)
library(kernlab)
#setting the working directory
workingdir<-("H:\\Eagle_subjects\\Sem1\\geostatistics\\classification_prac")
setwd(workingdir)
#import the viector training data
vector<-readOGR("H:\\Eagle_subjects\\Sem1\\geostatistics\\classification_prac","Pra_image_training")
#import the raster data
raster_stack<-brick("H:\\Eagle_subjects\\Sem1\\geostatistics\\classification_prac\\raster_stack.tif")
#directly applying supervise classification with random forest
train_sc<-superClass(img=raster_stack,model="rf",trainData = vector,responseCol = "Classname")
train_sc
plot(train_sc$map)
#assisging the breaks
brk<-c(0.99,1.99,2.99,3.99,4.99)
#terrain.colors function is used to give colors to the five segments of the ndvi raster
col=terrain.colors(4)
#plotting ndvi raster with main name as "NDVI",with breake values and the above color combination
plot(train_sc$map, col=col, breaks=brk, main="Supervised classification(RF)",legend = FALSE)
#changing the position and name of the five segments
legend("topright", 
       legend = c("Built_up", "Forest", "Meadow", "Water"), 
       fill = col, ncol = 1,
       cex = 0.45)
# generating the random points
uniqueClasses<-unique(vector$Classname)
uniqueClasses
# if we have four classes than length of unique classes will be 4
length(uniqueClasses)
set.seed(25)
# declaring the data type of i
i<-as.integer()
for(i in 1:length(uniqueClasses))
{
  #taking any points from particular class
  class_data<-subset(vector,Classname==uniqueClasses[i])
  #randomly selecting points from any class
  classpts<-spsample(class_data,type="random",n=10)
  # generating new column class and inserting points into it
  classpts$class<-rep(uniqueClasses[i],length(classpts))
  #for first class, all points should be added into xy and for other classes,xy and classpts should be combined
  if(i==1)
  {
    xy<-classpts
  }
  else
  {
    xy<-rbind(xy,classpts)
  }
  
}
#to view the table of xy with class column
table(xy$class)
#extracting the values of points from raster(rastoe stack values of xy points are extracted)
train_vals<-extract(raster_stack,y=xy,cellnumbers=TRUE)
#Putting these values into data frame
train_vals<-data.frame(response=xy$class,train_vals)
head(train_vals)
#for duplicate vaues
any(duplicated(train_vals$cells))
#not working yet [dup<-duplicated(train_vals) ],train_vals<-train_vals[!dup,"Classname"]
#generating confusuion matrix without validation sets
model_rf<-randomForest(response~.,data=train_vals,na.action = na.omit,confusion=TRUE)
model_rf
#importing validated vector file
validation<-readOGR("H:\\Eagle_subjects\\Sem1\\geostatistics\\classification_prac","Pra_image_validation")
#performing validation and generating confusion matrix
validate_sc<-superClass(img=raster_stack,model="rf",trainData = vector,responseCol = "Classname",valData = validation)

validate_sc$validation$performance
#applying two different models rf and svm
models<-c("rf","svmRadial")

#ensemble them together
ensemble<-lapply(models,function(mod){
  set.seed(5)
  sc<-superClass(img=raster_stack,model=mod,trainData = vector,responseCol = "Classname")
  return(sc$map)
})
prediction_stack<-stack(ensemble)
names(prediction_stack) <-models
#comparing the models using raster entropy function
modelEntropy<-rasterEntropy(prediction_stack)
plot(prediction_stack)
plot(modelEntropy)
#expect_error(n=10)
