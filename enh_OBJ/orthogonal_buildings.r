#name: orthogonal_buildings.r
#author: J. Höhle
#date of generation: 19.2.2019
#extraction of one building
#buildings of RCD30
#b13 (example building #13)

setwd("C:/Users/Bruger/Dropbox/EduServ17/R_programs")
library("EBImage")
display = function(...) if (interactive()) EBImage::display(...)

## start of program
LCM_enh_b=readImage("./data/LCM_cart_enh_buildings.jpg") #input of enhanced billede (raster)
display(LCM_enh_b, method="raster")
LCM_enh_b_t=thresh(LCM_enh_b, 2,2,0.01) #thresholding -> white oulines 
display(LCM_enh_b_t, method="raster")
LCM_label<-bwlabel(LCM_enh_b_t) #labeling of all buildings
display(LCM_label,method="raster")
#
colorMode(LCM_label)=Grayscale
cat('number of buildings=', max(LCM_label),'\n')
coor<-computeFeatures.moment(LCM_label) #geometric features (moment)
coor
write.table(coor,"./data/coor.csv") 
shap<-computeFeatures.shape(LCM_label) #geometric features (shape)
shap
write.table(shap,"./data/shap.csv")

##extract pixels of b13
bnr=13 # nr of building (to be changed)
N1<-230400
N1
x_dat<-rep(0,N1)
y_dat<-rep(0,N1)
idx<-rep(0,N1)

#search area
xl<-as.integer(coor[bnr,1]-shap[bnr,6]-10)
xr<-as.integer(coor[bnr,1]+shap[bnr,6]+10)
yu<-as.integer(coor[bnr,2]-shap[bnr,6]-10)
yl<-as.integer(coor[bnr,2]+shap[bnr,6]+10)

##matrix to table
k=1
i<-0
for(x in xl:xr){
  for(y in yu:yl) {
    if(imageData(LCM_label[x,y])==bnr){
      i<-i+1
      idx[i]<-i
      x_dat[i]<-x 
      y_dat[i]<-y}
  }
} #end of loop
#

print(i)
length(x_dat)
N<-i
N
idx<-idx[1:N]
x_dat<-x_dat[1:N]
y_dat<-y_dat[1:N]
idxy<-cbind(idx, x_dat, y_dat)
idxy[1:10,]
length(idxy[,1])
fname=paste("./data/idxy_LCM_RCD30_b_",bnr,".csv", sep="")
fname
write.table(idxy, fname,  sep = " ") #output of point cluster for one building (b13)
#save(shap[bnr,6],file="b")
##end of program

