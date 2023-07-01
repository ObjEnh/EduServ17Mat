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

## Read the data from mat format
# fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_7/AP/ISPRS7_NDVI_AP_All.mat"
# data<-readMat(fname)
# nam<-names(data)
# data<-data[[nam[1]]]
# sz<-dim(data)
# ndata<-reshape(data,sz[1]*sz[2],sz[3])
# rm(data)
## Read the data from the envi file
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_7/AP/NDVIALL_demo"
data<-read.ENVI(fname, headerfile = paste(fname,".hdr",sep=""))
sz<-dim(data)
sz
ndata<-reshape(data,sz[1]*sz[2],sz[3])
rm(data)
## Read the NDVI File in as image
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_7/ISPRS7_NDVI.mat"
NDVIData<-readMat(fname);nam<-names(NDVIData)
NDVIData<-NDVIData[[nam[1]]]
NDVIData<-reshape(NDVIData,sz[1]*sz[2])
# Append NDVI data to the AP of NDVI data
ndata<-cbind(NDVIData,ndata)
rm(NDVIData)
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
sc<-sort(SampleLocationIndex, decreasing = FALSE, index.return=TRUE)
nlabel<-label[sc$ix]
#
nr=nrow(ndata);nc=ncol(ndata)
orglabel<-nlabel

source('bh_normalize_par.R')
ndata<-bh_normalize_par(ndata)
dim(ndata)
# Random subset the samples
# nsamples=length(label);
# randperum=sample(nsamples);
# ntrain=round(nsamples*0.8)
# subsetindex=randperum[1:ntrain];
# newdata<-ndata[randperum,]

source('bh_randomsubsetsamples.R')

NPer= 2000 #seq(0.2,0.9, by=0.1)
MeanAccuracy<-list();StdAccuracy<-list()

for (outer in 1:length(NPer))
{
  Acc<-numeric(0)
  SVMAcc<-numeric(0)
  DTAcc<-numeric(0)
  RFAcc<-numeric(0)
  #
  SVM<-list();SVMDV<-list();
  SVMClassifiedlabel<-list();SVMTrainingParameter<-list();
 SVM$OA<-numeric(0);
  for (loop in 1)#:10)  # loop for number of folds
  {
    # sss<-bh_randomsubsetsamples(orglabel,"Percentage",NPer[outer])
    sss<-bh_randomsubsetsamples(orglabel,"No_of_samples",2000)
    testlocation_index=sss$test_index;
    TrainData<-ndata[sss$tr_index,];train_label<-orglabel[sss$tr_index]
    s<- dim(TrainData)
    cat("\n dim of TrainData.",s, "\n\n")
    testlabel<-orglabel[sss$test_index]
    if (FullImageClassification==TRUE)
    {
      TestData<-ndata
    }else{
      TestData<-ndata[-sss$tr_index,];
    }
    
     # Testing Data Parition
    source('bh_datapartitiontolist.R')
    Lndata<-bh_datapartitiontolist(TestData,20000)
    rm(TestData)
    
    ## SVM ++++++++++++++++++++++++++++++++++++++++++++++++
    
    library(e1071)
    kertype<-2;fold<-5;
    source('bh_ksvmcrossvalidation.R')
    str<-Sys.time()
    parameter<-bh_ksvmcrossvalidation(TrainData,train_label,kertype,fold)
    SVMCrossVald_Time<-print(Sys.time()-str)
    SVMCrossVald_Time<-as.numeric(SVMCrossVald_Time)
    # SVM Training
    option<-"train"
    source('bh_svmclassification.R')
    kertype<-2;
    model<-bh_svmclassification(TrainData,train_label,option,kertype,parameter)
    # SVM Classification
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
    SVMPrediction<-bh_list2matix(SVMPer)
    rm(SVMPer)
    # Classification Results
    SVMDV[[loop]]<-SVMPrediction$Probvalues
    SVMClassifiedlabel[[loop]]<-SVMPrediction$classifiedlabel
    SVMTrainingParameter[[loop]]<-model
    
    # SVM Acc
    source('bh_confusionmat.R')
    SAcc<-bh_confusionmat(testlabel,SVMPrediction$classifiedlabel[sss$test_index],option="CI")
    SVMAcc[loop]=SAcc$OA[1]
    SVM$OA<-rbind(SVM$OA, SAcc$OA) 
    SVM$conf[[loop]]<-SAcc$conf
    SVM$UA[[loop]]<-SAcc$UA
    SVM$PA[[loop]]<-SAcc$PA
   
  }
  ## Accuracy write in Excel
  
  source('bh_confwriteexcel_cluster.R')
  # PerTurbo
  option<-2
  setwd(old_dir)
  bh_confwriteexcel_cluster(SVM, fname,option)
  ########### ++++++++++++++++++++++++++++++++++++++++++++#################
  MeanAccuracy$SVM[[outer]]<-colMeans(SVM$OA)
  StdAccuracy$SVM[[outer]]<-sqrt(colVars(SVM$OA))
  setwd("/share/home/damodara/DenmarkWork/RCodes/Results/ISPRS_7/AP/NDVI_AP")
  save(SVM,file=paste("NDVI_AP_SVM_Results_Percent_",as.character(NPer[outer]),sep=""))
  ######## ++++++ Decison values and labels write
  # Decision values
   save(SVMDV, file=paste("ISPRS_SVM_DV_NDVIAP_", as.character(NPer[outer]), sep=""))
  # Classifiedlabels
  save(SVMClassifiedlabel,
       file=paste("ISPRS_SVM_NDVI_AP_Classifiedlabel_", as.character(NPer[outer]), sep=""))
  setwd(old_dir)
  
}

setwd("/share/home/damodara/DenmarkWork/RCodes/Results/ISPRS_7/AP/NDVI_AP")
TrainingPercent<-NPer;
save(MeanAccuracy, StdAccuracy,TrainingPercent,file=paste("NDVI_AP_SVMClassificationAcc_", as.character(NPer[outer]), sep=""))