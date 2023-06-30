rm(list=ls())
pstrttime<-Sys.time()
old_dir<-("C:\\Users\\damodara\\Dropbox (Your team)\\Bharath Bhushan\\EduServ\\ML_RCodes")
setwd(old_dir)
getwd()
source('bh_loadLib.R')
bh_loadLib()
#### ++++++++++ Options+++++++++++++++
FullImageClassification=TRUE
TestingSampClassification=FALSE
crop_img = FALSE
image_write=TRUE
ndata<-numeric(0)
### ++++++++++++++ Read the Data or Image +++++++++++++++++++++++++

# read orginal Image
# give appropriate path name, and file name
OrgImgPathname<-"D:/PostDocWork/Denmark/Datas/ISPRS/"
OrgImgfilenames<-"ortho_top_mosaic_09cm_area7.tif"

# read AP Image (Insert path of AP Image)
# generate the Attribute profiles using AP Tool and Load here
# give appropriate path name, and file name
APAreaPathname<-'D:/tmp/'
APAreafilenames<-"area7_ap.tif"

# read GT Image
# give the path name and file name of the ground truth
GTPathname<-'D:/PostDocWork/Denmark/Datas/ISPRS/'
GTfilenames<-"GTS_top_mosaic_09cm_area7.tif"



## Ground Truth read
setwd(GTPathname)
GTImage<-readGDAL(GTfilenames[1])
GTImage <-GTImage@data
nsz<-dim(GTImage)
Sindex<-1:(nsz[1])

# label coding from the RGB Image
setwd(old_dir)
source('bh_ISPRS_labelcoding.R')
GTImage<-bh_ISPRS_labelcoding(GTImage)
L <-unique(GTImage)

## Read the original image
setwd(OrgImgPathname)
fname<-OrgImgfilenames[1]
ff<-substr(fname,1,nchar(fname))#-4)
OrgImg<-readGDAL(ff)
img_size <-OrgImg@grid@cells.dim
#OrgImg<-readGDAL(OrgImgfilenames[1]) # for JP2 format
OrgImg<-OrgImg@data
zero_index<-which(OrgImg[1]==0)

#### NDVI Computation ######
# NDVIImg<-(OrgImg[[4]]-OrgImg[[3]])/(OrgImg[[4]]+OrgImg[[3]]) # for tiff or geotiff
NDVIImg<-(OrgImg[1]-OrgImg[2])/(OrgImg[1]+OrgImg[2]) # for JP2 

#  reading the original image using raster for writing classification map
setwd(OrgImgPathname)
oorrimg<-stack(OrgImgfilenames)
org_dim<-dim(oorrimg)


#AreaAP read  
setwd(APAreaPathname)
fname<-APAreafilenames
ff<-substr(fname,1,nchar(fname))#-4)
AreaAPImg<-readGDAL(ff)
sz<-dim(AreaAPImg)
AreaAPImg<-AreaAPImg@data



##### combine all the inputs in single variable#########\
# if you want to do classification only with original image, then use only OrgImg in 
# cbind. If you want to include Attribute profiles, you can remove the OrgImg. Remember
# attribute profiles already includes original image bands.
ndata<-cbind(OrgImg)#,NDVIImg,AreaAPImg)
ndata<-as.matrix(ndata)
nr=nrow(ndata);nc=ncol(ndata)
orglabel<-GTImage
rm(OrgImg, NDVIImg, AreaAPImg)
# Normalize the data
setwd(old_dir)
source('bh_normalize.R')
ndata<-bh_normalize(ndata)
ndata[is.na(ndata)]<-0
dim(ndata)


source('bh_randomsubsetsamples.R')
source('bh_confusionmat.R')
# samples per class for training
NPer= 500  


 
classnames<-list("Imp_surf","Building","Low_Veg","Tree","Car") # ISPRS_7
#classnames<-list("Building", "Hed&Bush","Grass","Road&Parklot","Tree","Wall&Carport")


# Training and testing sample partition
# sss<-bh_randomsubsetsamples(orglabel,"Percentage",NPer[outer])
sss<-bh_randomsubsetsamples(orglabel,"No_of_samples",NPer)
TrainTestSampleIndex<-sss
testlocation_index=sss$test_index;
TrainData<-ndata[sss$tr_index,];train_label<-orglabel[sss$tr_index]
s<- dim(TrainData)
cat("\n dim of TrainData.",s, "\n\n")
testlabel<-orglabel[sss$test_index]

##
if (FullImageClassification==TRUE)
{
  TestData<-ndata
}else{
  TestData<-ndata[-sss$tr_index,];
}
rm(ndata)    
# Decision Tree
source('bh_decisiontreeclassification.R')
option="train"
DTmodel<-bh_decisiontreeclassification(TrainData,train_label,option)
DTmodel$Nclass<-length(unique(train_label))
DTPrediction<-bh_decisiontreeclassification(DTmodel,TestData,option="test")
# confusion matrix
DTAcc<-bh_confusionmat(testlabel,DTPrediction$classifiedlabel[testlocation_index],classnames,option="CI")
print(DTAcc)

    
if (image_write==TRUE)
  {
  setwd(old_dir)
  source('bh_ISPRS_labelcoding.R')
  predicted_img<-bh_ISPRS_labelcoding(DTPrediction$classifiedlabel)
  predicted_img<-reshape(predicted_img, img_size[1], img_size[2],3)
  p_img<-zeros(img_size[2], img_size[1],3)
   p_img[,,1]<-t(predicted_img[,,1])
   p_img[,,2]<-t(predicted_img[,,2])
   p_img[,,3]<-t(predicted_img[,,3])
  
   
  source('bh_rasterclassificationMap.R')
  option<-3
  rc<-bh_rasterclassificationMap(p_img,oorrimg,option)
  savepname<-OrgImgPathname
  setwd(savepname)
  # writeTIFF(p_img, fn)
  #fn<-OrgImgfilenames[1]
  fn<-'DT_Classification_Map.tif'
  rc<-writeRaster(rc, filename=fn, format="GTiff", datatype='INT1U', overwrite=TRUE)
  setwd(old_dir)
  rm(rc,p_img, predicted_img)
  
#       setwd(old_dir)
#       source('bh_ENVIWrite.R')
#       source('bh_EnviHeaderWrite.R')
#       setwd(savepname)
#       fname<-paste("ISPRS_7_DT_CLassifiedImage_", sep="")
#       Img<-reshape(as.matrix(DTPrediction$classifiedlabel), img_size[1],img_size[2])
#       Img<-t(Img)
#       bh_ENVIWrite(Img,fname)
#       bh_EnviHeaderWrite(Img,fname)
  # rm(Img)
  }
setwd(old_dir)
rm(DTPrediction, predicted_img)
    
## Random Forest
    
setwd(old_dir)
source('bh_randomforest.R')
option="train"
str<-Sys.time()
RFmodel<-bh_randomforest(TrainData,train_label,option)
RFmodel$Nclass<-length(unique(train_label))
print(Sys.time()-str)
    
 
str<-Sys.time()
RFPrediction<-bh_randomforest(RFmodel,TestData,option="test")
print(Sys.time()-str)
#confusion matrix
source('bh_confusionmat.R')
RFAcc<-bh_confusionmat(testlabel,RFPrediction$classifiedlabel[testlocation_index],classnames,option="CI")
print(RFAcc)

if (image_write==TRUE)
{
  setwd(old_dir)
  source('bh_ISPRS_labelcoding.R')
  predicted_img<-bh_ISPRS_labelcoding(RFPrediction$classifiedlabel)
  predicted_img<-reshape(predicted_img, img_size[1], img_size[2],3)
  p_img<-zeros(img_size[2], img_size[1],3)
  p_img[,,1]<-t(predicted_img[,,1])
  p_img[,,2]<-t(predicted_img[,,2])
  p_img[,,3]<-t(predicted_img[,,3])
  
  
  source('bh_rasterclassificationMap.R')
  option<-3
  rc<-bh_rasterclassificationMap(p_img,oorrimg,option)
  savepname<-OrgImgPathname
  setwd(savepname)
  # writeTIFF(p_img, fn)
  #fn<-OrgImgfilenames[1]
  fn<-'RF_Classification_Map.tif'
  rc<-writeRaster(rc, filename=fn, format="GTiff", datatype='INT1U', overwrite=TRUE)
  setwd(old_dir)
  rm(predicted_img, p_img, rc)
  
  #       setwd(old_dir)
  #       source('bh_ENVIWrite.R')
  #       source('bh_EnviHeaderWrite.R')
  #       setwd(savepname)
  #       fname<-paste("ISPRS_7_DT_CLassifiedImage_", sep="")
  #       Img<-reshape(as.matrix(DTPrediction$classifiedlabel), img_size[1],img_size[2])
  #       Img<-t(Img)
  #       bh_ENVIWrite(Img,fname)
  #       bh_EnviHeaderWrite(Img,fname)
}
rm(RFPrediction)
# PerTurbo Classification
# Pertubro CV
# fold<-5
# source('bh_perturbocrossvalidation.R')
# str<-Sys.time()
# perparams<-bh_perturbocrossvalidation(train_label,TrainData,fold)
# print(Sys.time()-str)
# #training
# source('bh_perturboclassification.R')
# option<-"train"
# PW<-bh_perturboclassification(TrainData,train_label,option,perparams)
# #testing
# option<-"test"
# PerTurboPrediction<-bh_perturboclassification(TestData,PW,option)
# 
# nclass=length(unique(testlabel))
# # confusion matrix
# source('bh_confusionmat.R')
# PAcc<-bh_confusionmat(testlabel,PerTurboPrediction$classifiedlabel[testlocation_index],classnames,option="CI")
# 
# if (image_write==TRUE)
# {
#   setwd(old_dir)
#   source('bh_ISPRS_labelcoding.R')
#   predicted_img<-bh_ISPRS_labelcoding(PerTurboPrediction$classifiedlabel)
#   predicted_img<-reshape(predicted_img, img_size[1], img_size[2],3)
#   p_img<-zeros(img_size[2], img_size[1],3)
#   p_img[,,1]<-t(predicted_img[,,1])
#   p_img[,,2]<-t(predicted_img[,,2])
#   p_img[,,3]<-t(predicted_img[,,3])
#   
#   
#   source('bh_rasterclassificationMap.R')
#   option<-3
#   rc<-bh_rasterclassificationMap(p_img,oorrimg,option)
#   savepname<-OrgImgPathname
#   setwd(savepname)
#   # writeTIFF(p_img, fn)
#   #fn<-OrgImgfilenames[1]
#   fn<-'PerTurbo_Classification_Map1.tif'
#   rc<-writeRaster(rc, filename=fn, format="GTiff", datatype='INT1U', overwrite=TRUE)
#   setwd(old_dir)
#   rm(predicted_img, p_img, rc)
#   ## to write image in Envi Format
#   #       setwd(old_dir)
#   #       source('bh_ENVIWrite.R')
#   #       source('bh_EnviHeaderWrite.R')
#   #       setwd(savepname)
#   #       fname<-paste("ISPRS_7_DT_CLassifiedImage_", sep="")
#   #       Img<-reshape(as.matrix(DTPrediction$classifiedlabel), img_size[1],img_size[2])
#   #       Img<-t(Img)
#   #       bh_ENVIWrite(Img,fname)
#   #       bh_EnviHeaderWrite(Img,fname)
# }    
    
    ## SVM
# SVM parameter optimization
library(e1071)
kertype<-2;fold<-3;
source('bh_ksvmcrossvalidation.R')
parameter<-bh_ksvmcrossvalidation(TrainData,train_label,kertype,fold)
# SVM Training
option<-"train"
source('bh_svmclassification.R')
kertype<-2;
model<-bh_svmclassification(TrainData,train_label,option,kertype,parameter)
# SVM Classification
option<-"test"
source('bh_svmclassification.R')
SVMPrediction<-bh_svmclassification(model,TestData,option)
# SVM Acc
source('bh_confusionmat.R')
SVMAcc<-bh_confusionmat(testlabel,SVMPrediction$classifiedlabel,classnames,option="CI")
print(SVMAcc)
if (image_write==TRUE)
{
  setwd(old_dir)
  source('bh_ISPRS_labelcoding.R')
  predicted_img<-bh_ISPRS_labelcoding(SVMPrediction$classifiedlabel)
  predicted_img<-reshape(predicted_img, img_size[1], img_size[2],3)
  p_img<-zeros(img_size[2], img_size[1],3)
  p_img[,,1]<-t(predicted_img[,,1])
  p_img[,,2]<-t(predicted_img[,,2])
  p_img[,,3]<-t(predicted_img[,,3])
  
  
  source('bh_rasterclassificationMap.R')
  option<-3
  rc<-bh_rasterclassificationMap(p_img,oorrimg,option)
  savepname<-OrgImgPathname
  setwd(savepname)
  # writeTIFF(p_img, fn)
  #fn<-OrgImgfilenames[1]
  fn<-'SVM_Classification_Map1.tif'
  rc<-writeRaster(rc, filename=fn, format="GTiff", datatype='INT1U', overwrite=TRUE)
  setwd(old_dir)
  rm(predicted_img, p_img, rc, SVMPrediction)
  
  #       setwd(old_dir)
  #       source('bh_ENVIWrite.R')
  #       source('bh_EnviHeaderWrite.R')
  #       setwd(savepname)
  #       fname<-paste("ISPRS_7_DT_CLassifiedImage_", sep="")
  #       Img<-reshape(as.matrix(DTPrediction$classifiedlabel), img_size[1],img_size[2])
  #       Img<-t(Img)
  #       bh_ENVIWrite(Img,fname)
  #       bh_EnviHeaderWrite(Img,fname)
}    
    
    
## Large Scale SVM using random Fourier Features
gamma = model$model$gamma
source('bh_rff.R')
M=100 # no of RFF features
rff_coef = bh_rff(TrainData,M=M)
RFF_TrainData<-cos(cbind(TrainData, ones(nrow(TrainData),1))%*%rff_coef)*((2/M)^0.5)
RFF_TestData<-cos(cbind(TestData, ones(nrow(TestData),1))%*%rff_coef)*((2/M)^0.5)
rm(TrainData, TestData)
option<-"train"
source('bh_svmclassification.R')
kertype<-0;
print("Linear SVM Training")
rffmodel<-bh_svmclassification(RFF_TrainData,train_label,option,kertype,parameter)
print("SVM Training completed")
print("====================================================")
##### Linear SVM Prediction (Classification)
option<-"test"
source('bh_svmclassification.R')
RFFPrediction<-bh_svmclassification(rffmodel,RFF_TestData,option)

# Computation of Confusion Matrix for SVM Classifier
source('bh_confusionmat.R')
RFFAcc<-bh_confusionmat(testlabel,RFFPrediction$classifiedlabel,classnames,option="CI")
print(RFFAcc)
print(paste("Random Fourier Features accuracy = ",RFFAcc$OA, sep=""))

if (image_write==TRUE)
{
  setwd(old_dir)
  source('bh_ISPRS_labelcoding.R')
  predicted_img<-bh_ISPRS_labelcoding(RFFPrediction$classifiedlabel)
  predicted_img<-reshape(predicted_img, img_size[1], img_size[2],3)
  p_img<-zeros(img_size[2], img_size[1],3)
  p_img[,,1]<-t(predicted_img[,,1])
  p_img[,,2]<-t(predicted_img[,,2])
  p_img[,,3]<-t(predicted_img[,,3])
  
  
  source('bh_rasterclassificationMap.R')
  option<-3
  rc<-bh_rasterclassificationMap(p_img,oorrimg,option)
  savepname<-OrgImgPathname
  setwd(savepname)
  # writeTIFF(p_img, fn)
  #fn<-OrgImgfilenames[1]
  fn<-'RFF_Classification_Map.tif'
  rc<-writeRaster(rc, filename=fn, format="GTiff", datatype='INT1U', overwrite=TRUE)
  setwd(old_dir)
  rm(predicted_img, p_img, rc)
  #       setwd(old_dir)
  #       source('bh_ENVIWrite.R')
  #       source('bh_EnviHeaderWrite.R')
  #       setwd(savepname)
  #       fname<-paste("ISPRS_7_DT_CLassifiedImage_", sep="")
  #       Img<-reshape(as.matrix(DTPrediction$classifiedlabel), img_size[1],img_size[2])
  #       Img<-t(Img)
  #       bh_ENVIWrite(Img,fname)
  #       bh_EnviHeaderWrite(Img,fname)
}         
    
     
  
  
   setwd(old_dir)
  