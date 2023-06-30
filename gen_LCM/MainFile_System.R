# main file for the classification experiments
rm(list=ls())
old_dir<-setwd("./")
source('bh_loadLib.R')
bh_loadLib()
getwd()
file_save = FALSE
## Read the training samples file, choose the data from datas folder '.csv' file
data<-read.csv(file.choose())
featurenames=names(data)
newdata<-data.matrix(data)
label=newdata[,length(featurenames)]
ndata<-newdata[,c(4,5)]

## Read the data from mat format
# data<-readMat(file.choose())
#  label=data$Data[,ncol(data$Data)]
#  ndata<-data$Data[,-ncol(data$Data)]
# newdata<-data.matrix(data)
# featurenames=names(data)
#
 nclass<-max(max(label));Nsamples<-0
 for (i in 1:nclass)
 {
   Nsamples[i]<-sum(label==i)
 }
nr=nrow(ndata);nc=ncol(ndata)


orglabel<-label

# normalize the input data using zero mean and unit variance 
source('bh_normalize.R')
strt<-Sys.time()
ndata<-bh_normalize(ndata)
print(Sys.time()-strt)


# function for randomly selecting the data from the training ground trurg
source('bh_randomsubsetsamples.R')
source('bh_confusionmat.R')
#NPer= 0.6 #seq(0.2,0.9, by=0.1)



  Acc<-numeric(0)
  SVMAcc<-numeric(0)
  DTAcc<-numeric(0)
  RFAcc<-numeric(0)
  #
  PerTurbo<-list();SVM<-list();DT<-list();RF<-list();RFF<-list()
  PerTurboDV<-list();SVMDV<-list();DTDV<-list();RFDV<-list()
  PerTurboClassifiedlabel<-list();SVMClassifiedlabel<-list();DTClassifiedlabel<-list()
  RFClassifiedlabel<-list();
  PerTurboTrainingParameter<-list();SVMTrainingParameter<-list();DTTrainingParameter<-list()
  RFTrainingParameter<-list()
  PerTurbo$OA<-numeric(0);SVM$OA<-numeric(0);DT$OA<-numeric(0);RF$OA<-numeric(0)
  RFF$OA<-numeric(0)
  

#sss<-bh_randomsubsetsamples(orglabel,"Percentage",NPer[outer])
  # randomly choose 100 samples per class as training and remaining as testing 
  # samples for the accessing the performance of the classifier
sss<-bh_randomsubsetsamples(orglabel,"No_of_samples",100)
# Training data and the labels
TrainData<-ndata[sss$tr_index,];train_label<-orglabel[sss$tr_index];
# Testing data and the labels
TestData<-ndata[-sss$tr_index,];testlabel<-orglabel[-sss$tr_index]

#=============== Decision Tree Classifier============================
## Decision Tree Training
source('bh_decisiontreeclassification.R')
option="train"
DTmodel<-bh_decisiontreeclassification(TrainData,train_label,option)

## Decision Tree Testing or prediction 
DTPrediction<-bh_decisiontreeclassification(DTmodel,TestData,option="test")
# Classification Results
DTDV<-DTPrediction$Probvalues
DTClassifiedlabel<-DTPrediction$classifiedlabel
DTTrainingParameter<-DTmodel

# Decision Tree confusion matrix
DAcc<-bh_confusionmat(testlabel,DTPrediction$classifiedlabel,option="CI")

DTAcc<-DAcc$OA[1]
DT$OA<-rbind(DT$OA, DAcc$OA) 
DT$conf<-DAcc$conf
DT$UA<-DAcc$UA
DT$PA<-DAcc$PA
print(paste("Decision Tree accuracy = ",DTAcc, sep=""))
#=============== Random Forest Classifier============================
## Random Forest
source('bh_randomforest.R')
option="train"
RFmodel<-bh_randomforest(TrainData,train_label,option)
## Random Forest Testing
RFPrediction<-bh_randomforest(RFmodel,TestData,option="test")
# Classification results
RFDV<-RFPrediction$Probvalues
RFClassifiedlabel<-RFPrediction$classifiedlabel
RFTrainingParameter<-RFmodel
#confusion matrix
RAcc<-bh_confusionmat(testlabel,RFPrediction$classifiedlabel,option="CI")
RFAcc<-RAcc$OA[1]
RF$OA<-rbind(RF$OA, RAcc$OA) 
RF$conf<-RAcc$conf
RF$UA<-RAcc$UA
RF$PA<-RAcc$PA
print(paste("Random Forest accuracy = ",RFAcc, sep=""))

#=============== PerTurbo Classifier============================

## PerTurbo Classifier

# Pertubro hyper-parameter tuning through cross validation, for 
# the details please see the codes inside the function
fold<-5
source('bh_perturbocrossvalidation.R')
str<-Sys.time()
perparams<-bh_perturbocrossvalidation(train_label,TrainData,fold)
print(Sys.time()-str)
#training
source('bh_perturboclassification.R')
option<-"train"
PW<-bh_perturboclassification(TrainData,train_label,option,perparams)
#testing
option<-"test"
PerTurboPrediction<-bh_perturboclassification(TestData,PW,option)
# Classification Results
PerTurboDV<-PerTurboPrediction$Probvalues
PerTurboClassifiedlabel<-PerTurboPrediction$classifiedlabel
PerTurboTrainingParameter<-perparams


nclass=length(unique(testlabel))
# confusion matrix
source('bh_confusionmat.R')
PAcc<-bh_confusionmat(testlabel,PerTurboPrediction$classifiedlabel,option="CI")
PerTurbo$OA<-rbind(PerTurbo$OA, PAcc$OA)
PerTurbo$conf<-PAcc$conf
PerTurbo$UA<-PAcc$UA
PerTurbo$PA<-PAcc$PA

print(paste("PerTurbo accuracy = ",PAcc$OA[1], sep=""))

#=============== SVM RBF Classifier============================
## SVM Classifier

# SVM hyper-parameter tuning using five fold cross validation
library(e1071)
kertype<-2;fold<-5;
source('bh_ksvmcrossvalidation.R')
print("SVM hyper-parameter cross validation")
parameter<-bh_ksvmcrossvalidation(TrainData,train_label,kertype,fold)
print("SVM hyper-parameter cross validation completed")
print("===========================================")
######## SVM Training
option<-"train"
source('bh_svmclassification.R')
kertype<-2;
print("SVM Training")
model<-bh_svmclassification(TrainData,train_label,option,kertype,parameter)
print("SVM Training completed")
print("====================================================")
##### SVM Prediction (Classification)
option<-"test"
source('bh_svmclassification.R')
SVMPrediction<-bh_svmclassification(model,TestData,option)
# Classification Results
SVMDV<-SVMPrediction$Probvalues
SVMClassifiedlabel<-SVMPrediction$classifiedlabel
SVMTrainingParameter<-model

# Computation of Confusion Matrix for SVM Classifier
source('bh_confusionmat.R')
SAcc<-bh_confusionmat(testlabel,SVMPrediction$classifiedlabel,option="CI")
SVMAcc=SAcc$OA[1]
SVM$OA<-rbind(SVM$OA, SAcc$OA) 
SVM$conf<-SAcc$conf
SVM$UA<-SAcc$UA
SVM$PA<-SAcc$PA
print(paste("SVM accuracy = ",SVMAcc, sep=""))

#=============== Large scale SVM Classifier with Linear kernel============================
## Large Scale SVM using random Fourier Features
gamma = SVMTrainingParameter$model$gamma
source('bh_rff.R')
M=100 # no of RFF features
rff_coef = bh_rff(TrainData,M=M)
RFF_TrainData<-cos(cbind(TrainData, ones(nrow(TrainData),1))%*%rff_coef)*((2/M)^0.5)
RFF_TestData<-cos(cbind(TestData, ones(nrow(TestData),1))%*%rff_coef)*((2/M)^0.5)
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
# Classification Results
RFFDV<-RFFPrediction$Probvalues
RFFClassifiedlabel<-RFFPrediction$classifiedlabel
RFFTrainingParameter<-rffmodel

# Computation of Confusion Matrix for SVM Classifier
source('bh_confusionmat.R')
RfAcc<-bh_confusionmat(testlabel,RFFPrediction$classifiedlabel,option="CI")
RFFAcc=RfAcc$OA[1]
RFF$OA<-RFFAcc
RFF$conf<-RfAcc$conf
RFF$UA<-RfAcc$UA
RFF$PA<-RfAcc$PA
print(paste("Random Fourier Features accuracy = ",RFFAcc, sep=""))
RFF
## Accuracy write in Excel
if (file_save==TRUE)
  {
    setwd("D:/PostDocWork/Denmark/Codes")
    source('bh_confwriteexcel.R')

      # SVM
    fname=paste("ISPRS7_SVM_tr",trp,"_test",testp,".xlsx", sep="")
    #fname="ISPRS7_SVM_10tr_90test.xlsx"
    option<-2
    setwd(old_dir)
    bh_confwriteexcel(SVM, fname,option) 
    # Decision Tree
    fname=paste("ISPRS7_DT_tr",trp,"_test",testp,".xlsx", sep="")
    #fname="ISPRS7_DT_10tr_90test.xlsx"
    option<-3
    setwd(old_dir)
    bh_confwriteexcel(DT, fname,option) 
    # Random Forest
    fname=paste("ISPRS7_RF_tr",trp,"_test",testp,".xlsx", sep="")
    #fname="ISPRS7_RF_10tr_90test.xlsx"
    option<-4
    setwd(old_dir)
    bh_confwriteexcel(RF, fname,option) 
      ########### ++++++++++++++++++++++++++++++++++++++++++++#################
    
    
    setwd("D:/PostDocWork/Denmark/Codes/Results")
    save(PerTurbo,file=paste("PerTurbo_Results_Percent_",as.character(NPer),sep=""))
    save(SVM,file=paste("SVM_Results_Percent_",as.character(NPer),sep=""))
    save(DT,file=paste("DT_Results_Percent_",as.character(NPer),sep=""))
    setwd("D:/PostDocWork/Denmark/Codes")
    
    ######## ++++++ Decison values and labels write
    setwd("D:/PostDocWork/Denmark/Codes/Results")
    # Decision values
    save(PerTurboDV, SVMDV, DTDV, RFDV, file=paste("ISPRS_Classifiers_DecisionValue_", as.character(NPer[outer]), sep=""))
    # Classifiedlabels
    save(PerTurboClassifiedlabel, SVMClassifiedlabel, DTClassifiedlabel, RFClassifiedlabel, 
         file=paste("ISPRS_Classifiers_Classifiedlabel_", as.character(NPer[outer]), sep=""))
    
    setwd("D:/PostDocWork/Denmark/Codes/Results")
    TrainingPercent<-NPer;
    save(MeanAccuracy, StdAccuracy,TrainingPercent,file="MeanAccuarcy_Measures_ofclassifier")

}


 