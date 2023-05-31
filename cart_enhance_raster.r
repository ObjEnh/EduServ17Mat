##program for cartographic enhancement using images
##name of program: cart_enhance_raster.r
##author:JH
##dates of creation/modification: 2.4.14, modified 7.8.16 and 9.3.18
#user has to modify the path for the working directory (wd) and input/output

##set working directory
setwd("C:/Users/Joa/Dropbox/EduServ17/R_programs")
#install.packages("EBImage")
library("EBImage")

#function
display = function(...) if (interactive()) EBImage::display(...)

#input
LCM_building=readImage("./data/PCL_b_new.tiff") 
display(LCM_building,method="raster")
##end preparation and input

############################################
##Generation of refined class 'LCM_building'
############################################

##smooth objects (buildings) by dilation and erosion
kern=makeBrush(5,shape='diamond')
LCMdilate=dilate(LCM_building,kern) #dilation
LCMdilaterode=erode(LCMdilate,kern) #erosion
display(LCMdilaterode, method="raster")
LCMdilaterode_gray=channel(LCMdilaterode,'gray')
#print(LCMdilaterode_gray)

display(LCMdilaterode_gray, method="raster")
LCMdilaterode_t=thresh(LCMdilaterode_gray, 2,2,0.01) #generation of outlines
display(LCMdilaterode_t, method="raster")

LCMdilaterode_t_f=fillHull(LCMdilaterode_t) #fill outlines with pixels
display(LCMdilaterode_t_f,method="raster")

LCMdilaterode_t_f_lab=bwlabel(LCMdilaterode_t_f) #labeling of objects
cat('Number of buildings=',max(LCMdilaterode_t_f_lab),'\n')
display(LCMdilaterode_t_f_lab)
#print(LCMdilaterode_t_f_lab)

sh=computeFeatures.shape(LCMdilaterode_t_f_lab) #compute geometric features (area,...)
sh
n_rem=c(10,12,14,18)
LCM_cart_enh_buildings=rmObjects(LCMdilaterode_t_f_lab,n_rem) #remove buildings >=339 pixels
reenumerate(LCM_cart_enh_buildings)
display(LCM_cart_enh_buildings, method="raster")

sh2=computeFeatures.shape(LCM_cart_enh_buildings) #compute geometric features after new labeling
sh2
##output of result
#f='./results/LCM_cart_enh_buildings.jpg'
#writeImage(LCM_cart_enh_buildings,f, quality=100)
## end of program


