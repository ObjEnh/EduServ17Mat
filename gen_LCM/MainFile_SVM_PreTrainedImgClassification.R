rm(list=ls())
pstrttime<-Sys.time()
old_dir<-setwd("/share/home/damodara/DenmarkWork/RCodes")
getwd()
source('bh_loadLib.R')
bh_loadLib()
#### ++++++++++ Options+++++++++++++++
FullImageClassification=TRUE
TestingSampClassification=FALSE

### ++++++++++++++ Read the Data or Image +++++++++++++++++++++++++
ndata<-numeric(0)
## Read the data  
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_1/ISPRS_Area1_Image_Mat.mat"
ndata<-readMat(fname);nam<-names(ndata)
ndata<-ndata[[nam[1]]]
sz<-dim(ndata)
ndata<-reshape(ndata,sz[1]*sz[2],sz[3])
gc()
# ## Read the NDVI File in as image
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_1/ISPRS_Area1_NDVI.mat"
NDVIData<-readMat(fname);nam<-names(NDVIData)
NDVIData<-NDVIData[[nam[1]]]
sz<-dim(NDVIData)
NDVIData<-reshape(NDVIData,sz[1]*sz[2],1)
# Append NDVI data to the AP of NDVI data
ndata<-cbind(NDVIData,ndata)
rm(NDVIData)
gc()
##+++++++++++++++++ Read the DSM File in as image
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_1/ISPRS_Area1_DSM_Mat.mat"
DSMData<-readMat(fname);nam<-names(DSMData)
DSMData<-DSMData[[nam[1]]]
sz<-dim(DSMData)
DSMData<-reshape(DSMData,sz[1]*sz[2],1)
# Append DSM data to the AP of DSM data
ndata<-cbind(DSMData,ndata)
rm(DSMData)
gc()
######+++++++++++++++++++###########
#######################+++++++++++++++++++++++++++  AP---------------------
# ## Read the data from the envi file
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_1/AP/ISPRS_Area7_NDVIAP_All"
AP<-read.ENVI(fname, headerfile = paste(fname,".hdr",sep=""))
sz<-dim(AP)
sz
AP<-reshape(AP,sz[1]*sz[2],sz[3])
ndata<-cbind(AP,ndata)
rm(AP)
gc()
# ####++++++++
############ for RCD
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_1/ISPRS_Area7_GT_Class5.mat"
ClassImage<-readMat(fname)
nam<-names(ClassImage)
ClassImage<-ClassImage[[nam[1]]]
SampleLocationIndex<-which(ClassImage>0)
nlabel<-ClassImage[SampleLocationIndex]
##

##########################
if (is.null(ncol(ndata)))
{
  ndata=as.matrix(ndata)
}
nr=nrow(ndata);nc=ncol(ndata)
orglabel<-nlabel

source('bh_normalize.R')
str<-Sys.time()
ndata<-bh_normalize(ndata)
print(Sys.time()-str)
ndata[is.na(ndata)]<-0
dim(ndata)
gc()
 

 
source('bh_confwriteexcel_cluster.R')

fn="/share/home/damodara/DenmarkWork/RCodes/Results/ISPRS_1/ISPRS_A7_OrgImgDSMNDVINDVIAP_SVM_Fold2_Model"
sss<-load(fn)

MeanAccuracy<-list();StdAccuracy<-list()


savepathname<-"/share/home/damodara/DenmarkWork/RCodes/Results/ISPRS_1/OrgImg+DSM+NDVI+NDVIAP/SVM"
for (outer in 1:length(NPer))
{
  Acc<-numeric(0)
  SVMAcc<-numeric(0)
  SVMCrossVald_Time<-numeric(0)
  #
  SVM<-list();SVMDV<-list();
  SVMClassifiedlabel<-list();SVMTrainingParameter<-list();
  SVM$OA<-numeric(0);
  for (loop in 1)#:10)  # loop for number of folds
  {

    Nclass<-length(unique(orglabel))
    gc()
    if (FullImageClassification==TRUE)
    {
      TestData<-ndata
    }else{
      TestData<-ndata[testlocation_index,];
    }
    
    gc()
    
    ## SVM ++++++++++++++++++++++++++++++++++++++++++++++++
    
    library(e1071)
    kertype<-2;fold<-5;
    # trained model parameters
    
    
    # SVM Classification
    #     # Testing Data Parition
    #     source('bh_datapartitiontolist.R')
    #     Lndata<-bh_datapartitiontolist(TestData,20000)
    #     rm(TestData)
    #     option<-"test"
    #     # Load parallel libraries
    #     source('bh_loadparallel_Lib.R')
    #     cl<-bh_loadparallel_Lib()
    #     SVMPer<-foreach(testpar=1:length(Lndata), .packages = c('e1071','pracma')) %dopar%
    #     {
    #       TData<-Lndata[[testpar]]
    #       SVMPrediction<-bh_svmclassification(model,TData,option)
    #       return(SVMPrediction)
    #     }
    #     stopCluster(cl)
    #     #
    #     source('bh_list2matix.R')
    #     SVMPrediction<-bh_list2matix(SVMPer)
    #     rm(SVMPer)
    ## seq testing
    option<-"test"
    str<-Sys.time()
    SVMPrediction<-bh_svmclassification(model,TestData,option)
    print(Sys.time()-str)
    rm(TestData)
    gc()
    # Classification Results
    # SVMDV[[loop]]<-SVMPrediction$Probvalues
    SVMClassifiedlabel[[loop]]<-SVMPrediction$classifiedlabel
    SVMTrainingParameter[[loop]]<-model
    gc()
    # SVM Acc
    source('bh_confusionmat.R')
    SAcc<-bh_confusionmat(orglabel,SVMPrediction$classifiedlabel[SampleLocationIndex],option="CI")
    SVMAcc[loop]=SAcc$OA[1]
    SVM$OA<-rbind(SVM$OA, SAcc$OA) 
    SVM$conf[[loop]]<-SAcc$conf
    SVM$UA[[loop]]<-SAcc$UA
    SVM$PA[[loop]]<-SAcc$PA
    SVM$F1[[loop]]<-SAcc$F1
    
    # Image write in ENVI form
    source('bh_ENVIWrite.R')
    source('bh_EnviHeaderWrite.R')
    setwd(savepathname)
    fname<-paste("ISPRS_Area1_SVM_CLassifiedImage_BasedTrainModel_Area7","fold2", sep="")
    Img<-reshape(as.matrix(SVMPrediction$classifiedlabel), sz[1],sz[2])
    bh_ENVIWrite(Img,fname)
    bh_EnviHeaderWrite(Img,fname)
    setwd(old_dir)
    rm(SVMPrediction)
    rm(Img)
    gc()
    #
    pname= paste(savepathname, "/",sep="")
    trp<-as.character(NPer[outer]);testp<-as.character(1-NPer[outer])
    # SVM
    fname=paste(pname,"ISPRS_Area1_OrgImgNDVIDSMDSMAPSVM_BasedTrainModel_Area7fold2",".xlsx", sep="")
    option<-2
    setwd(old_dir)
    bh_confwriteexcel_cluster(SVM, fname,option,loop)
    gc()
  }
  
  
  ######## ++++++ Decison values and labels write
  setwd(savepathname)
  # Model parameter
  save(SVMTrainingParameter,
       file=paste("ISPRS_Area1_OrgImgNDVIDSMDSMAPSVM_BasedTrainModel_Area7fold", as.character(2), sep=""))
  # Decision values
  #save(SVMDV, file=paste("ISPRS_SVM_DV_OrgImg_DSM_NDVI_DSM_OrgImg_DSM_NDVI_OrgImg_DSM_NDVIAP_OrgImgAP_", as.character(NPer[outer]), sep=""))
  # Classifiedlabels
  save(SVMClassifiedlabel, 
       file=paste("ISPRS_Area1_SVM_OrgImgNDVIDSMDSMAPClassifiedlabel_BasedTrainModel_Area7_","fold2", sep=""))
  setwd(old_dir)
  rm(SVMClassifiedlabel) #SVMDV
  gc()
  
  
  ########### ++++++++++++++++++++++++++++++++++++++++++++#################
 
  setwd(savepathname)
  save(SVM,file=paste("OrgImgNDVIDSMNDVIAP_SVM_Results_BasedTrainModel_Area7_","fold2",sep=""))
  setwd(old_dir)
  MeanAccuracy$SVM[[outer]]<-colMeans(SVM$OA)
  StdAccuracy$SVM[[outer]]<-sqrt(colVars(SVM$OA))
}

setwd(savepathname)
TrainingPercent<-NPer;
save(MeanAccuracy, StdAccuracy,TrainingPercent,file=paste("OrgImg_NDVI_DSM_DSMAP_SVMClassificationAcc_", as.character(NPer[outer]), sep=""))

FinalRuntime<-as.numeric(Sys.time()-pstrttime)
save(FinalRuntime, file="OrgImg_NDVI_DSM_NDVIAP_SVM_Runtime")
