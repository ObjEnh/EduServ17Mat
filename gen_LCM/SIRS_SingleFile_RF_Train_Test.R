rm(list=ls())
pstrttime<-Sys.time()
# change to directory where you have downloaded the codes
old_dir<-("D:/PostDocWork/Denmark/EduservCodes")
setwd(old_dir)
getwd()
source('bh_loadLib.R')
bh_loadLib()
#### ++++++++++ Options+++++++++++++++
FullImageClassification=TRUE
TestingSampClassification=FALSE
crop_img = FALSE

# change to the path name, where AP and Original image is located
APAreaPathname<-'C:/Users/damodara/Dropbox/EduServ/swf_detection/data/'
OrgImgPathname<-"C:/Users/damodara/Dropbox/EduServ/swf_detection/data/"
GTPathname<-OrgImgPathname

# APAreafilenames<-list.files(path=APAreaPathname, pattern= "tif")
# OrgImg_tif_filenames<-list.files(path=OrgImgPathname, pattern= "tif")
# OrgImgfilenames<-list.files(path=OrgImgPathname, pattern= "hdr")
# # GTfilenames<-list.files(path=GTPathname, pattern= "tif")

# change the filename to the name you have given to AP image
APfilenames<-'ndvi_ap.tif'
OrgImg_tif_filenames<-'SWF_570d_ext_v1.tif'
OrgImgfilenames<-'SWF_570d_ext_v1.tif'
GTfilenames<-'SWF_SAMPLE_ext_v1.tif'
Data<-numeric(0)
# OrgImgData<-numeric(0);OrgImgBackData<-numeric(0)
# NDVIImgData<-numeric(0);NDVIBackData<-numeric(0)
# AreaAPData<-numeric(0);AreaAPBackData<-numeric(0)
# STDAPData<-numeric(0);STDAPBackData<-numeric(0)
# MOIAPData<-numeric(0);MOIAPBackData<-numeric(0)

Nfiles<-length(OrgImgfilenames)

## Ground Truth read
setwd(GTPathname)
GTImage<-readGDAL(GTfilenames[1])
GTImage <-GTImage@data
nsz<-dim(GTImage)
Sindex<-1:(nsz[1])
L <-unique(GTImage)

## Read the original image
setwd(OrgImgPathname)
 fname<-OrgImgfilenames[1]
 ff<-substr(fname,1,nchar(fname))#-4)
 OrgImg<-readGDAL(ff)
 img_size<-OrgImg@grid@cells.dim
#OrgImg<-readGDAL(OrgImgfilenames[1]) # for JP2 format

OrgImg<-OrgImg@data
zero_index<-which(OrgImg[1]==0)

# 
setwd(OrgImgPathname)
oorrimg<-stack(OrgImg_tif_filenames)

# Raster based GT index
GTRIndex<-which(GTImage==3)
GBGIndex<-which(GTImage==0)

GTBackIndex<-Sindex[-c(GTRIndex, zero_index)]

set.seed(0)
# extract subset of background samples for training

GTRB<-GTBackIndex[sample(length(GTBackIndex))[1:50000]] # tile based

FGImgData<-OrgImg[GTRIndex,]
GTRIndex<-GTRIndex[which(FGImgData[1]>0)]
FGImgData<-OrgImg[GTRIndex,]
BGData<-OrgImg[GTRB,]

#

## Outlier removal from background
setwd(old_dir)
source('bh_outlierremoval.R')
nonoutlier_index<-bh_outlierremoval(FGImgData, BGData, probs =0.05)
GTB_index<-GTRB[nonoutlier_index]
OrgImgBackData<- BGData[nonoutlier_index,]

# NDVI
# NDVIImg<-(OrgImg[[4]]-OrgImg[[3]])/(OrgImg[[4]]+OrgImg[[3]]) # for tiff or geotiff
NDVIImg<-(OrgImg[1]-OrgImg[2])/(OrgImg[1]+OrgImg[2]) # for JP2 
NDVIImgData<-NDVIImg[GTRIndex,]
NDVIBackData<-NDVIImg[GTB_index,]

#AreaAP read envi file
setwd(APAreaPathname)
fname<-APfilenames[1]
ff<-substr(fname,1,nchar(fname))
AreaAPImg<-readGDAL(ff)
sz<-dim(AreaAPImg)
AreaAPImg<-AreaAPImg@data

#NDVIDAP
for (i in 2:sz[2])
 {
   AreaAPImg[,i]<-(AreaAPImg[,1]-AreaAPImg[,i])*AreaAPImg[,1]
 }
##

AreaAPData<-AreaAPImg[GTRIndex,]
AreaAPBackData<-AreaAPImg[GTB_index,]
#STDAP
# setwd(STDPathname)
# fname<-STDfilenames[3]
# ff<-substr(fname,1,nchar(fname)-4)
# STDAPImg<-readGDAL(ff)
# sz<-dim(STDAPImg)
# STDAPImg<-STDAPImg@data
# STDAPData<-STDAPImg[GTRIndex,]
# STDAPBackData<-STDAPImg[GTB_index,]
# #MOIAP
# setwd(MOIPathname)
# fname<-MOIfilenames[1]
# ff<-substr(fname,1,nchar(fname)-4)
# MOIAPImg<-readGDAL(ff)
# sz<-dim(MOIAPImg)
# MOIAPImg<-MOIAPImg@data
# MOIAPData<-MOIAPImg[GTRIndex,]
# MOIAPBackData<-MOIAPImg[GTB_index,]
#rm(MOIAPImg, AreaAPImg, STDAPImg, OrgImg, GTImage)
gc()

################
Data<-cbind(FGImgData,NDVIImgData,AreaAPData)#,,STDAPData, MOIAPData)
DataBackGround<- cbind(OrgImgBackData,NDVIBackData,AreaAPBackData)#,STDAPBackData,MOIAPBackData)
colnames(Data)[colnames(Data)=="NDVIImgData"] <- "NDVI"
colnames(DataBackGround)[colnames(DataBackGround)=="NDVIBackData"] <- "NDVI"
#
setwd(old_dir)
source('bh_naInfRemoval.R')
Data<-bh_naInfRemoval(Data)
DataBackGround<-bh_naInfRemoval(DataBackGround)
#
nn<-which(is.nan(Data))
if (length(nn)>1)
{
  Data[nn]<-0
}
#
nn<-which(is.na(DataBackGround))
if (length(nn)>1)
{
  DataBackGround[nn]<-0
}

nFGTD<-nrow(Data)
nBGTD<-nrow(DataBackGround)
FGlabel<-repmat(2,nFGTD,1) # ForeGroundBush
BGlabel<-repmat(1, nBGTD,1)  # backGroundBush
#############################
# No of training data for the model
NFGTR<-ceil(0.1*nFGTD)
NBGTR<-ceil(0.4*nBGTD)
set.seed(0)
FGTRIndex<-sample(nFGTD)[1:NFGTR]
FGTestIndex<-seq(1:nFGTD)[-FGTRIndex]

set.seed(0)  
BGTRIndex<-sample(nBGTD)[1:NBGTR]
BGTestIndex<-seq(1:NBGTR)[-BGTRIndex]
# Data extraction
FGTrainData<-Data[FGTRIndex,];FGTRLabel<-FGlabel[FGTRIndex]
FGTestData<-Data[-FGTRIndex,];FGTestLabel<-FGlabel[-FGTRIndex]

BGTrainData<-DataBackGround[BGTRIndex,];BGTRLabel<-BGlabel[BGTRIndex]
BGTestData<-DataBackGround[-BGTRIndex,];BGTestLabel<-BGlabel[-BGTRIndex]

# Concat Train Data from FG and BG
TrainData<-rbind(FGTrainData, BGTrainData)
trainlabel<-c(FGTRLabel, BGTRLabel)

TestData<-rbind(FGTestData, BGTestData)
testlabel<-c(FGTestLabel, BGTestLabel)

#######################
####### Image data###########
ImageData<-cbind(OrgImg, NDVIImg,AreaAPImg)#,STDAPImg)
#############################
rm(list= ls()[!(ls() %in% c('TrainData','TestData','trainlabel',
    'testlabel','old_dir', 'ImageData','img_size','crop_img','oorrimg'))])
gc()
Nclass<-length(unique(trainlabel))
## Random Forest
setwd(old_dir)
source('bh_randomforest.R')
option="train"
str<-Sys.time()
RFmodel<-bh_randomforest(TrainData,trainlabel,option)
RFmodel$Nclass<-Nclass
print(Sys.time()-str)

#### ranger RF #########
# require(ranger)
# str<-Sys.time()
# label<-as.factor(trainlabel)
# colnames(TrainData)<-paste0("feature", 1:ncol(TrainData))
# f<-as.formula(paste("label~",paste0("feature",1:ncol(TrainData), collapse = "+")))
# TrainData<-data.frame(TrainData)
# 
# rangerrfmodel<-ranger(f,data=TrainData, num.threads=30)
# print(Sys.time()-str)

# setwd(old_dir)
# source('bh_naInfRemoval.R')
# TestData<-bh_naInfRemoval(TestData)
# str<-Sys.time() 
# colnames(TestData)<-paste0("feature", 1:ncol(TestData))
# pred.iris <- predict(rangerrfmodel, dat = TestData, num.threads=30)
# print(Sys.time()-str)

##########################################

# pn<-'/share/home/damodara/SIRSWoody/Codes/TrainingModels/AvgImgAP'
# setwd(pn)
# save(RFmodel, file='RFTrainingmodel_arles_img2_AvgImgAP')

setwd(old_dir)

### seq. Implementation for testing
str=Sys.time()
RFPrediction<-bh_randomforest(RFmodel,TestData,option="test")
print(Sys.time()-str)

### parallel implementation
# Ndata<-nrow(TestData)
# dataindex<-1:Ndata
# partition<-100000
# Npart<-ceil(Ndata/partition)
# extindex<-mod(Npart*partition, Ndata)
# dataindex<-c(dataindex, zeros(extindex,1))
# splitindex<-split(as.data.frame(dataindex), rep(1:Npart, each = partition))
# 
# source('bh_loadparallel_Lib.R')
# cl<-bh_loadparallel_Lib()
# str=Sys.time()
# RFPer<-foreach(testpar=1:length(splitindex),.packages = c('rpart','pracma')) %dopar%
# {
#   tindex<-as.vector(splitindex[[testpar]])
#   TData<-TestData[tindex[[1]],]
#   RFPrediction<-bh_randomforest(RFmodel,TData,option="test")
#   return(RFPrediction)
# }
# stopCluster(cl)
# print(Sys.time()-str)
# RFPrediction<-bh_list2matix(RFPer)

#########################

#confusion matrix
source('bh_confusionmat.R')
RAcc<-bh_confusionmat(testlabel,RFPrediction$classifiedlabel,option="CI")

loop<-1
RF<-list()
RF$OA<-RAcc$OA
RF$kappa[loop]<-RAcc$kappa

RF$conf[[loop]]<-RAcc$conf
RF$UA[[loop]]<-RAcc$UA
RF$PA[[loop]]<-RAcc$PA
RF$F1[[loop]]<-RAcc$F1
print(RF)
# pname<-'/share/home/damodara/SIRSWoody/DATA/ARLES/ClassificationResults/AccuracyAssesment/AvgImgAP/'
# fname=paste(pname,"ARLES_Img2_AllFeat_AvgImgAP",80,"_train",".xlsx", sep="")
# option<-4
# setwd(old_dir)
# source('bh_confwriteexcel_cluster.R')
# bh_confwriteexcel_cluster(RF, fname,option,loop)


crop_img = FALSE
#### Subset Image Classification
if (crop_img == TRUE)
{
st_row<-9453;end_row<-11489;st_col<-3227;end_col<-8150
orgdim<-dim(oorrimg)
setwd(old_dir)
source('bh_imagecropindex.R')
pos_index<-bh_imagecropindex(st_row,end_row,st_col,end_col,orgdim)
ImgData<-ImageData[as.matrix(pos_index$pos_index),]
nrow<-pos_index$crop_dim[1]
ncol<-pos_index$crop_dim[2]
}
# RF classification for whole image
str=Sys.time()
RFPrediction<-bh_randomforest(RFmodel,ImageData,option="test")
print(Sys.time()-str)

#
if (crop_img==FALSE)
{
predicted_img<-reshape(as.matrix(RFPrediction$classifiedlabel),img_size[1],img_size[2])
predicted_img<-t(predicted_img)
} else
{
  predicted_img<-reshape(as.matrix(RFPrediction$classifiedlabel),nrow, ncol)
} 
  
  
 
#   setwd(old_dir)
#   source('bh_ENVIWrite.R')
#   source('bh_EnviHeaderWrite.R')
#   savepname<-'/share/home/damodara/SIRSWoody/DATA/ATC/results/DAP_MaxTree'
#   setwd(savepname)
#   fn<-OrgImgfilenames[1]
#   fname<-substr(fn,1,nchar(fn)-4)
#   #Img<-as.integer(Img)
#   bh_ENVIWrite(predicted_img,fname)
#   bh_EnviHeaderWrite(predicted_img,fname)
#   setwd(old_dir)
# save(RFPrediction, file = 'ClassifiedImg')


setwd(old_dir)
source('bh_rasterclassificationMap.R')
rc<-bh_rasterclassificationMap(predicted_img,oorrimg)
# change the pathname to save the outputfile
savepname<-'C:/Users/damodara/Dropbox/EduServ/swf_detection/data/'

setwd(savepname)
#fn<-OrgImgfilenames[1]
fn<-'AP_woody_detection_RF1.tif'
rc<-writeRaster(rc, filename=fn, format="GTiff", datatype='INT1U', overwrite=TRUE)
setwd(old_dir)