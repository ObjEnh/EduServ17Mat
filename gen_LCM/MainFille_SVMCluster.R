rm(list=ls())
old_dir<-setwd("/share/home/damodara/DenmarkWork/RCodes")
getwd()
source('bh_loadLib.R')
bh_loadLib()
#### ++++++++++ Options+++++++++++++++
FullImageClassification=TRUE
TestingSampClassification=FALSE

### ++++++++++++++ Read the Data or Image +++++++++++++++++++++++++

## Read the training samples file (CSV Files)
#fname<-"/share/home/damodara/DenmarkWork/Data/Sebastien/train_all_6.csv"
#data<-read.csv(fname)
#featurenames=names(data)
#newdata<-data.matrix(data)
#label=newdata[,length(featurenames)]
#ndata<-newdata[,c(4,5)]


## Read the data  
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_7/ISPRS7_OriignalImage3D.mat"
ndata<-readMat(fname);nam<-names(ndata)
ndata<-ndata[[nam[1]]]
sz<-dim(ndata)
ndata<-reshape(ndata,sz[1]*sz[2],sz[3])
## Read the NDVI File in as image
# fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_7/ISPRS7_NDVI.mat"
# NDVIData<-readMat(fname);nam<-names(NDVIData)
# NDVIData<-NDVIData[[nam[1]]]
# NDVIData<-reshape(NDVIData,sz[1]*sz[2],1)
# # Append NDVI data to the AP of NDVI data
# ndata<-cbind(NDVIData,ndata)
# rm(NDVIData)
####
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_7/ISPRS_7_GT_SampleLocation.mat"
SampleLocationIndex<-readMat(fname)
nam<-names(SampleLocationIndex)
SampleLocationIndex<-SampleLocationIndex[[nam[1]]]
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_7/ISPRS_7_GT_Label_Classwise.mat"
label<-readMat(fname)
nam<-names(label)
label<-label[[nam[1]]]
##
cat('Sucess')
sc<-sort(SampleLocationIndex, decreasing = FALSE, index.return=TRUE)
nlabel<-label[sc$ix]
#
nr=nrow(ndata);nc=ncol(ndata)
orglabel<-nlabel

source('bh_normalize.R')
str<-Sys.time()
ndata<-bh_normalize(ndata)
print(Sys.time()-str)
ndata[is.na(ndata)]<-0
dim(ndata)
# Random subset the samples
# nsamples=length(label);
# randperum=sample(nsamples);
# ntrain=round(nsamples*0.8)
# subsetindex=randperum[1:ntrain];
# newdata<-ndata[randperum,]

source('bh_randomsubsetsamples.R')

NPer= 500 #seq(0.2,0.9, by=0.1)
MeanAccuracy<-list();StdAccuracy<-list()

# Loading pre-generated testing and training location indexes
fname<-"/share/home/damodara/DenmarkWork/RCodes/Results/ISPRS_7/AP/NDVI_AP/ISPRS7_NDVIAP_Results_500/ISPRS_7_NDVIAP_TRTestSampleLocationIndex_500"
sss<-load(fname)
for (outer in 1:length(NPer))
{
  Acc<-numeric(0)
  SVMAcc<-numeric(0)
  SVMCrossVald_Time<-numeric(0)
  #
  SVM<-list();SVMDV<-list();
  SVMClassifiedlabel<-list();SVMTrainingParameter<-list();
  SVM$OA<-numeric(0);
  for (loop in 1:10)  # loop for number of folds
  {
    # sss<-bh_randomsubsetsamples(orglabel,"Percentage",NPer[outer])
    #sss<-bh_randomsubsetsamples(orglabel,"No_of_samples",500)
    #testlocation_index=sss$test_index;
    #TrainData<-ndata[sss$tr_index,];train_label<-orglabel[sss$tr_index]
    #s<- dim(TrainData)
    #cat("\n dim of TrainData.",s, "\n\n")
    #testlabel<-orglabel[sss$test_index]
    ##### with existing generated samples
    testlocation_index<-TrainTestSampleIndex[[loop]]$test_index;
    train_index<-TrainTestSampleIndex[[loop]]$tr_index;
    TrainData<-ndata[train_index,];train_label<-orglabel[train_index]
    s<- dim(TrainData)
    cat("\n dim of TrainData.",s, "\n\n")
    testlabel<-orglabel[testlocation_index]
    if (FullImageClassification==TRUE)
    {
      TestData<-ndata
    }else{
      TestData<-ndata[testlocation_index,];
    }
    
    
    
    ## SVM ++++++++++++++++++++++++++++++++++++++++++++++++
    
    library(e1071)
    kertype<-2;fold<-5;
    source('bh_ksvmcrossvalidation.R')
    str<-Sys.time()
    parameter<-bh_ksvmcrossvalidation(TrainData,train_label,kertype,fold)
    SVMCrossVald_T<-print(Sys.time()-str)
    SVMCrossVald_Time[loop]<-as.numeric(SVMCrossVald_T)
    # SVM Training
    option<-"train"
    source('bh_svmclassification.R')
    kertype<-2;
    model<-bh_svmclassification(TrainData,train_label,option,kertype,parameter)
    # SVM Classification
    # Testing Data Parition
    source('bh_datapartitiontolist.R')
    Lndata<-bh_datapartitiontolist(TestData,20000)
    rm(TestData)
    option<-"test"
    # Load parallel libraries
    source('bh_loadparallel_Lib.R')
    cl<-bh_loadparallel_Lib()
    SVMPer<-foreach(testpar=1:length(Lndata), .packages = c('e1071','pracma')) %dopar%
    {
      TData<-Lndata[[testpar]]
      SVMPrediction<-bh_svmclassification(model,TData,option)
      return(SVMPrediction)
    }
    stopCluster(cl)
    #
    source('bh_list2matix.R')
    SVMPrediction<-bh_list2matix(SVMPer)
    rm(SVMPer)
    # Classification Results
    SVMDV[[loop]]<-SVMPrediction$Probvalues
    SVMClassifiedlabel[[loop]]<-SVMPrediction$classifiedlabel
    SVMTrainingParameter[[loop]]<-model
    
    # SVM Acc
    source('bh_confusionmat.R')
    SAcc<-bh_confusionmat(testlabel,SVMPrediction$classifiedlabel[testlocation_index],option="CI")
    SVMAcc[loop]=SAcc$OA[1]
    SVM$OA<-rbind(SVM$OA, SAcc$OA) 
    SVM$conf[[loop]]<-SAcc$conf
    SVM$UA[[loop]]<-SAcc$UA
    SVM$PA[[loop]]<-SAcc$PA
    
    # Image write in ENVI form
    source('bh_ENVIWrite.R')
    source('bh_EnviHeaderWrite.R')
    setwd("/share/home/damodara/DenmarkWork/RCodes/Results/ISPRS_7/Original Features/OriginalImage_500samples/SVM_PerTurbo")
    fname<-paste("ISPRS_7_SVM_CLassifiedImage_",as.character(NPer),"fold_", as.character(loop), sep="")
    Img<-reshape(as.matrix(SVMPrediction$classifiedlabel), sz[1],sz[2])
    bh_ENVIWrite(Img,fname)
    bh_EnviHeaderWrite(Img,fname)
    setwd(old_dir)
    rm(SVMPrediction)
    rm(Img)
  }
  
  
  ######## ++++++ Decison values and labels write
  setwd("/share/home/damodara/DenmarkWork/RCodes/Results/ISPRS_7/Original Features/OriginalImage_500samples/SVM_PerTurbo")
  # Decision values
  save(SVMDV, file=paste("ISPRS_SVM_DV_OrgImg_", as.character(NPer[outer]), sep=""))
  # Classifiedlabels
  save(SVMClassifiedlabel, SVMCrossVald_Time,
       file=paste("ISPRS_SVM_OrgImg_Classifiedlabel_", as.character(NPer[outer]), sep=""))
  setwd(old_dir)
  rm(SVMDV,SVMClassifiedlabel)
  
  ## Accuracy write in Excel
  
  source('bh_confwriteexcel_cluster.R')
  trp<-as.character(NPer[outer]);testp<-as.character(1-NPer[outer])
  # SVM
  fname=paste("ISPRS7_OrgImg_SVM_tr",trp,"_test",testp,".xlsx", sep="")
  option<-2
  setwd(old_dir)
  bh_confwriteexcel_cluster(SVM, fname,option)
  ########### ++++++++++++++++++++++++++++++++++++++++++++#################
  MeanAccuracy$SVM[[outer]]<-colMeans(SVM$OA)
  StdAccuracy$SVM[[outer]]<-sqrt(colVars(SVM$OA))
  setwd("/share/home/damodara/DenmarkWork/RCodes/Results/ISPRS_7/Original Features/OriginalImage_500samples/SVM_PerTurbo")
  save(SVM,file=paste("OrgImg_SVM_Results_Percent_",as.character(NPer[outer]),sep=""))
  
  setwd(old_dir)
  
}

setwd("/share/home/damodara/DenmarkWork/RCodes/Results/ISPRS_7/Original Features/OriginalImage_500samples/SVM_PerTurbo")
TrainingPercent<-NPer;
save(MeanAccuracy, StdAccuracy,TrainingPercent,file=paste("OrgImg_SVMClassificationAcc_", as.character(NPer[outer]), sep=""))
