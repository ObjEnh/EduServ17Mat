#name of the program: outline_of_building_new.r
#Detecting the line segments of the outline of a building
#derives the sequence of point clusters (PC) 
#and approximate corners of the closed polygon
#input is the result of the Hough transform 
#output is a file with approximate corners (vertices)
#with solution when no common points exist
#building b13
#authors:JH & MH
#version: 22.2.19

## start of program
setwd("C:/Users/Joa/Dropbox/EduServ17/R_programs")

##functions

#function circ
circ=function(x0,y0,r,phi_arc){
  xy=matrix(ncol=1, nrow=2)
  xy[1]<-x0+r*cos(phi_arc)
  xy[2]<-y0+r*sin(phi_arc)
  return(xy)
} #end of function 'circ'
#

#function test_gaps_onedirection (MH)
test_gaps_onedirection <- function(hist) { 
  
  gap_candidates <- which(hist$counts==0)
  
  if (length(gap_candidates)>0) {
    nTotal <- sum(hist$counts)
    
    gaps <- t(sapply(gap_candidates, function(i) {
      lower <- sum(hist$counts[seq_len(i)])
      upper <- nTotal - lower
      isLower <- lower >= upper
      return( c(i,isLower, max(lower,upper)/nTotal))
    }))
    
    bestGap <- gaps[which.max(gaps[,3]),]
    return(data.frame(cut=hist$mids[bestGap[1]],
                      direction=ifelse(bestGap[2],"lower","upper"),
                      proportion=bestGap[3],
                      nGaps=length(gap_candidates)))
  } else {
    return(NULL)
  }
} #end of function 'test gaps'

#function reduce_pointset (MH)
reduce_pointset <- function(P) {
  
  xgaps <- test_gaps_onedirection(hist(P$x,plot=FALSE)) #modified
  ygaps <- test_gaps_onedirection(hist(P$y,plot=FALSE)) #modified
  
  if (!is.null(xgaps) | !is.null(ygaps)) {
    
    if (!is.null(xgaps) & !is.null(ygaps)) {
      if (xgaps$nGaps > ygaps$nGaps) { ygaps <- NULL} else { xgaps <- NULL}
    }  
    if (is.null(xgaps)) {
      P <- if (ygaps$direction == "lower") subset(P, P$y <= ygaps$cut) else subset(P, P$y > ygaps$cut)
    }
    if (is.null(ygaps)) {
      P <- if (xgaps$direction == "lower") subset(P, P$x <= xgaps$cut) else subset(P, P$x > xgaps$cut)
    }
  }
  return(P)
} # end of function "reduce_pointset"

##transformation of the parameters in B to other units (degrees, pixels)
trans_H_res<-function(B1,theta_step1,ro_step1,ro11,k1){
  theta_angle<<-(B1[i,2]-1)*theta_step1
  ro_pixel<<-(B1[i,3]-1)*ro_step1+ro11
  n<<-B1[i,4]/k1 # coefficient for converting to length of line in pixels
  H_para<-matrix(ncol=1,nrow=6)
  H_para<-c(B1[i,2], B1[i,3],B1[i,4],theta_angle,ro_pixel,n)
  return(H_para)
} #end transformation 
#########################################################################################

##input of hough trans result
obj_nr=13 #to be changed for other buildings
fname6=paste("./data/parameter_space_b_",obj_nr,".txt", sep="")
#fname6
B<-read.table(fname6,row.names=NULL) #result of Hough transform
options(digits = 8)
names(B)=c("lnr","theta_index","ro_index","N")
head(B)

##input of point cloud for one object 
fname=paste("./data/idxy_LCM_RCD30_b_",obj_nr,".csv",sep="")
fname
pc3<-read.table(fname, header=TRUE)
names(pc3)[2:3]<-c("col","row")
head(pc3)

##set up of graphics for checking the intermediate results
##input of centre coordinates
fname=paste("./data/coor",obj_nr,".csv",sep="")
fname
coor<-read.table(fname, header=TRUE)
xc=coor[obj_nr,1]
yc=coor[obj_nr,2]

##plot of point cloud of building together with origin (0,0) in small scale (overview)
x=0
y=0
plot(x,-y, pch=3, cex=2, col="red", asp=1, xlim=c(1,250), ylim=c(-360,-1)) #small scale
#plot(x,-y, pch=3, cex=3, col="green", asp=1, xlim=c(200,250), ylim=c(-357,-300), xlab="x") #large scale
points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.5, col="red")
#plot of centre of building
points(xc, -yc, pch=3, asp=1, cex=2.0, col="green")
# end of plot

##plot of reference line (if building has long form)
omega=180/pi
##Transformation of B parameters into angles and pixels
#selected parameters in Hough transform
theta_step=5
ro_step=5
ro_1=0

##calculation of scale factor k
obj=obj_nr
lnr=1 #first (longest) line in Hough transform (reference)
fname=paste("./data/b_",obj,"_",lnr,".txt", sep="")
#fname
P <- read.table(fname, col.names=c("idx","x","y")) #point cloud of first line
head(P)
nrow1=nrow(P)
nrow1
points(P[,2],-P[,3], pch=20, asp=1, cex=0.5, col="blue") #point cloud 

##
x_max=max(P[,2])
x_min=min(P[,2])
y_max=max(P[,3])
y_min=min(P[,3])
d_line=sqrt((x_max-x_min)^2+(y_max-y_min)^2) #length of line [pixel]
d_line #distance of line 1
B[1,4]
k=B[1,4]/d_line #scale factor to convert into number of pixel
k
#end of calculation of the scale factor 'k'
#
head(B)
B2<-subset(B, B[,4]>=24) #minimum length of side assumed with 10 pixel (2.4m)
n1<-length(B2[,1])
n1
B3=matrix(nrow=n1,ncol=7)
B3[,1:4]<-0
head(B3)

##completion of table B3
#loop
for(i in 1:n1){
  H_para<-trans_H_res(B,theta_step,ro_step,ro_1,k)
  theta_index<-H_para[1]
  ro_index<-H_para[2]
  N<-H_para[3]
  theta_angle<-H_para[4]
  ro_pixel<-H_para[5]
  n<-H_para[6]
  B3[i,]=c(i,theta_index, ro_index, N, theta_angle,ro_pixel,round(n)) 
} #end loop

##Generation of data frame
B4<-data.frame(B3)
names(B4)=c("lnr", "theta_index", "ro_index", "n", "theta_angle","ro_pixel","n_pixel")
head(B4)
nrow(B4)

##plot of first line of Hough trans results 
alph=B4$theta_angle[1] - 90
alph
alph_arc=alph/omega
a=tan(alph_arc)
B4$theta_angle[1]
theta_arc<-B4$theta_angle[1]/omega
B4$ro_pixel[1]
b=B4$ro_pixel[1]/sin(theta_arc)
coef=c(-b,-a)
abline(coef, col="brown", lty=1, lwd=2, asp=1)

#plot of corected point cluster for line 1 
P_red <- reduce_pointset(P)
head(P_red)
points(P_red[,2],-P_red[,3], pch='.', asp=3, cex=3, col="green")
# end of plot line 1 (reference)
## end of all input
#############################################################################################

##Analysing the hough matrix B4
#search for parallel and orthogonal lines based on longest (first) line derived by Hough transform
#joint search for parallel and orthogonal lines with theta_angle and theta_angle -90 degrees, length of line > 10 pixels

k13=nrow(B4)
theta_ref<-B4$theta_angle[1]
theta_ref
alph
#
theta_ref_arc<-theta_ref/omega
ro_ref<-cos(theta_ref_arc)*xc+sin(theta_ref_arc)*yc
ro_ref=abs(ro_ref)
ro_ref

#radius_max=shap[26,6]
i=0
B5_2<-matrix(nrow=k13,ncol=7)
B5_2[,1:7]<-0
k1=1
while (i<k13){ 
  i=i+1
  if (B4$theta_angle[i]==theta_ref && B4$n_pixel[i]>10 ||
      B4$theta_angle[i]==alph && B4$n_pixel[i]>10) {
      B5_2[k1,]<-c(B4$lnr[i],B4$theta_index[i], B4$ro_index[i], B4$n[i], B4$theta_angle[i],B4$ro_pixel[i],B4$n_pixel[i])
      k1=k1+1
  }
} #end loop

###############################################################################################

##generalizing, reduction of matrix, conversion to data frame
B5_3<-subset(B5_2,B5_2[,7]>15) #lines of lenght > 15 pixels (=6 m)
B5_4<-data.frame(B5_3)
names(B5_4)=c("lnr", "theta_index", "ro_index", "n", "theta_angle","ro_pixel","n_pixel")
B5_4 #matrix of outlines of building/polygon
#

##check plot of all detected point clusters belonging to the building
#plot of all straight lines using the parameters of the Hough matrix
lnr_det<-B5_4$lnr
lnr_det
##loop
for (n in lnr_det){ 
  alph=B4$theta_angle[n] - 90
  alph
  alph_arc=alph/omega
  a=tan(alph_arc)
  B4$theta_angle[n]
  theta_arc<-B4$theta_angle[n]/omega
  B4$ro_pixel[n]
  b=B4$ro_pixel[n]/sin(theta_arc)
  coef=c(-b,-a)
  abline(coef, col="blue", lty=1, lwd=2, asp=1)
  
  ##plot of point clusters
  fname=paste("C:/Users/Joa/R_programming/LandCoverMap/R/hough_trans/results/b_",obj,"_",n,".txt", sep="")
  fname
  P <- read.table(fname, col.names=c("idx","x","y")) #point cloud
  P_red <- reduce_pointset(P)
  points(P_red[,2],-P_red[,3], pch=20, asp=3, cex=1.0, col="green")
} #end of for loop
# end of plot lines
## end of all input


## output final table of parameters
#obj_nr
f3<-paste("./data/unsorted_lines_b",obj_nr,".txt",sep="")
f3
B5_4
write.table(B5_4,f3)

##Storage in a list (all_lines)
PC_nr<-B5_4$lnr
PC_nr
n_PC<-length(B5_4$lnr)
n_PC
x=1:n_PC
x
f4<-paste("./data/b_",obj_nr,"_PC_nr.txt",sep="")
f4
write.table(PC_nr,f4)
#

all_lines<-list("PC","PC","PC","PC","PC","PC") #solution for six PCs
all_lines

##solution for x PCs
for (i in x){
  all_lines[i]<-paste("P",i,sep="")
}
all_lines

##loop for reading all point clouds
#PC_nr
#obj_nr
k=1
for (i in PC_nr){ 
  lnr=i
  cat("lnr=", lnr,"\n")
  fname=paste("C:/Users/Joa/R_programming/LandCoverMap/R/hough_trans/results/b_",obj_nr,"_",lnr,".txt", sep="")
  fname
  P0 <- read.table(fname, col.names=c("idx","x","y"))
  head(P0)
  nrow=nrow(P0)
  nrow
  P0_red <- reduce_pointset(P0) #correction for gaps using histogram analysis
  nrow=length(P0_red$idx)
  all_lines[[k]]=P0_red
  k=k+1
  points(P0_red[,2],-P0_red[,3], pch='.', asp=1, cex=2, col="red")
} #end loop
#

## convert 'all_lines' (matrix) to 'all_PC' (list)
all_PC<-all_lines 
names_PC=list("PCN", "PCN", "PCN", "PCN", "PCN", "PCN")

##loop
k=1
for (i in PC_nr){
na_PC<-paste("PC_",PC_nr[k],sep="")
name_PC<-as.name(na_PC)
names_PC[[k]]<-name_PC
k=k+1
} #end of loop i
names(all_PC)<-names_PC
names(all_PC)
all_PC

##check by plot of identified point clouds P_nr
x=length(PC_nr)
x
y=1:x
y
for (i in y) { 
  points(all_PC[[i]]$x,-all_PC[[i]]$y, pch='.', asp=1, cex=2, col="cyan")
} #end loop
# end of checkplot

##Output of files of individual PCs
for (i in y) { #loop
  fname8<-paste("./data/all_PC$PC_nr",PC_nr[i],".txt",sep="")
  write.table(all_PC[[i]], fname8)
} #end loop out put of list PC_all

##sequence of lines
#set up of matrix b13 with corner point number, angle, and coordinates
b13_angle=matrix(nrow=x,ncol=4)
b13_angle[,]<-0
b13_angle_df<-data.frame(b13_angle)
names(b13_angle_df)=c("nr_corner","alpha","x_com","y_com")
b13_angle_df
############################################################################

##part B

##functions 

#function common points in two lines
common_pts<-function(P_1,P_2){
  nrow_1=length(P_1$idx)
  nrow_2=length(P_2$idx)
  JP[,]<-0
  cp<-matrix(0,nrow=nrow_2,ncol=3)
  #loop
  i=0
  m=1
  while(i<nrow_1){
    i=i+1
    j=0
    while(j<nrow_2){
      j=j+1
      if(P_2$x[j]==P_1$x[i] && P_2$y[j]==P_1$y[i]){
        cp[m,]<-as.numeric(P_1[i,]) #c(P_1$idx[i],P_1$x[i],P_1$y[i]) 
        m=m+1
      } #end if
    } #end loop j
  } #end loop i
  JP<-cp
  return(JP)
} #end of function 'common_pts'
#

##calculation of midpoint of PointCloud
mean_line<-function(ln_num){
  i<-ln_num
  x=mean(all_PC[[i]]$x)
  y=mean(all_PC[[i]]$y)
  points(x,-y,pch=20,asp=1,cex=3,col="blue")
  b13_angle_df[ln_num,1]=names(all_PC)[i]
  b13_angle_df[ln_num,3]=x
  b13_angle_df[ln_num,4]=y
  angle_center_corner<-det_of_angle(x, y)
  b13_angle_df[ln_num,2]<-angle_center_corner
  b13_angle_df
} #end of function 

##function for determination of corners from common points between two lines
av_com_pts<-function(JP_red){ 
  av_xy_jp<-apply(JP_red,2,mean)
  av_xy_jp
  corner_xy<-av_xy_jp[2:3]
  return(corner_xy)
} #end of function 
#

#function for 'name of corners'
corner_name=function(nPC1,nPC2){ 
  name_of_corner<-paste("P",nPC1,"_","P",nPC2,sep="")
  return(name_of_corner)
} #end of function 'corner_name'
#

#function for determination of the angle
det_of_angle<-function(corner_x,corner_y){ 
  alph=(atan2(-(corner_y-yc),(corner_x-xc)))*omega # -dy/dx because alpha in math coor system
  if(alph<0){alph3=alph+360} else {alph3=alph}
  angle_center_corner<-alph3
  return(angle_center_corner)
} #end of function 'det_of_angle'
#

combi<-function(x2,B5){
  l1=length(x2)
  C<-matrix(nrow=l1,ncol=2)
  C[,]<-0
  k1=1
  #loop
  for (i in x2){
    first<-x2[1]-1
    if (B5[i,2]!=B5[first,2]){
      C[k1,1]<-B5[first,1]
      C[k1,2]<-B5[i,1] 
      k1=k1+1
    }  #end if
  } #end loop
  C2<<-C
} #end of function combi
## end of all functions

##set up of matrix JP for common points between two lines
JP<-matrix(nrow=19,ncol=4)

##array of combinations for point clusters (C3_all)
n3<-length(PC_nr)-1
C3_all<-array(dim=c(17,3,n3)) #array for 18 point clouds
C3_all[,,]<-0
# end of initialize C3_all
#############################################################

##continue
B5_4
x1=length(B5_4[,1]) #number of point clusters (same as length(PC_nr))
x1
x22=rep(0,x1)
C2=matrix(nrow=x1,ncol=2)
C2[,]<-0
#loop for C3_all
m=1 
while (m<x1){
  m=m+1
  x22<-m:x1
  combi(x22,B5_4)
  C3<-subset(C2,C2[,1]>0)
  fs<-nrow(C3)
  C3_all[1:fs,1:2,m-1]<-C3
} #end while-loop
C3_all
dim(C3_all)
################################################################################

##Test for common points

#loop B1
#list for point cloud combinations (C3_all_red)
C3_all_red=C3_all
n_x3<-1:dim(C3_all_red)[3]
n_x3

## initialize C3_all_red
C3_all_red<- list("L1","L2","L3","L4","L5","L6","L7","L8","L9","L10","L11","L12","L13","L14","L15","L16","L17","L18") #with 18 combinations of point clouds (L)

for (k in n_x3) {
  M1<-C3_all[,,k]
  nozero <- apply(M1, 1, function(x) return(all(x[1] != 0))) 
  M1new<-M1[nozero, ] #reduced matrix
  n_L<-length(PC_nr)-1
  L_nr<-PC_nr[1:n_L]
  x<-L_nr
  k1=1
  na_line<-rep(0,n_L)
  #loop to produce individual names
  for (n in x){
    na<-paste("L_",n, sep="")
    na_line[k1]<-na
    k1=k1+1
  } #end of loop to produce individual names

# list for all L (C3_all_red)
names(C3_all_red)=na_line
C3_all_red[[k]]<-get("M1new") #assign of reduced matrix "M1new" to lists "C3_all_red"
} #end of loop B1

#
C3_all_red #list of all point cluster combinations 
#

##set up of matrix b13 with corner point number, angle, and coordinates (b13_angle_df)
b13_angle=matrix(nrow=18,ncol=4)
b13_angle[,]<-0
b13_angle_df<-data.frame(b13_angle)
names(b13_angle_df)=c("nr_center","alpha","x_centre","y_centre")
b13_angle_df
m=0
## sequence of point clouds and approximate coordinates of corners

## loop (B2)
  for (k in n_x3 ){ #loop for angle 
    C3_all_red[[k]]
    x3<-length(C3_all_red[[k]])/3 # because number of columns=3
    x33<-1:x3
    ##start of combinations
    #loop C
    for (i in x33) {  
     namePC1<-C3_all[i,1,k]
     namePC2<-C3_all[i,2,k]
     na_PC_A<-paste("PC_",namePC1,sep="")
     na_PC_B<-paste("PC_",namePC2,sep="")
     PC_A<-all_PC[[na_PC_A]]
     PC_B<-all_PC[[na_PC_B]]
     JP[,]<-0
     
     JP<-common_pts(PC_A,PC_B) #common points between 2 point clouds
     JP
     if (JP[1,1]==0){
       cat("No common points at PC combination: PC1=",C3_all[i,1,k],"PC2=",C3_all[i,2,k],"\n")
       next
     } else {
       JP_red=subset(JP,JP[,2]>0)
       n_JP<-nrow(JP_red)
     } #end of if
    corner_xy<-av_com_pts(JP_red)
    corner_x<-corner_xy[1]
    corner_y<-corner_xy[2]
    name_of_corner<-corner_name(namePC1,namePC2)
    #
    m=m+1
    b13_angle_df$nr_corner[m]<-name_of_corner #first point in matrix b13
    b13_angle_df$x_com[m]<-corner_x
    b13_angle_df$y_com[m]<-corner_y
    angle_center_corner<-det_of_angle(corner_x, corner_y)
    b13_angle_df$alpha[m]<-angle_center_corner #angle_center_corner
    } #end of loop i for combinations L_x (loop C) 
} #end of loop k for angle (B2 loop) 

b13_angle_df
m=x1 #number of corners
b13_angle_df<-b13_angle_df[1:m,]
b13_angle_df
b13_angle_df$alpha<-as.numeric(b13_angle_df$alpha)

##plot of common points between 2 lines
y<-1:length(b13_angle_df$nr_corner)
y
for (i in y) {
  points(b13_angle_df$x_com,-b13_angle_df$y_com, pch=20, asp=1, cex=2, col="red")  
} #end plot of common points
#

##calculation of angles from coordinates of common points-average to determine
#the sequence of lines
cas="Cpts"
cas
str(cas)
b13_angle_df_seq<-b13_angle_df[order(b13_angle_df[,2], decreasing = FALSE),]
b13_angle_df_seq
################################################################################

m2=length(b13_angle_df_seq$nr_corner)
m2

## if-else (condition: same number of points)
if (m2==n_PC){ 
y=1:m2
y
nr<-matrix(ncol=1,nrow=m2)
for (i in y){
  nr[i,1]<-b13_angle_df_seq$nr_corner[i]
}
nr #vector of point assignments (characters)
str(nr)
b13_angle_df_seq$nr_corner
b13_angle_df_seq

##Output data of file with coordinates of the approximate corners in the right sequence 
fname7=paste("./data/b",obj,"_angle_df_seq.txt",sep="")
write.table(b13_angle_df_seq,fname7)
#
fname8=paste("./data/nr_b",obj,".txt", sep="")
fname8
write.table(nr,fname8)
#

##plot of outlines of building together with point cloud
#b13_outlines<-read.table(fname7, header=TRUE)
b13_outlines<-b13_angle_df_seq
b13_outlines #all outlines of building
points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.3, col="red") # original pointcloud for building

##plot of separated and corrected point clusters 
y<-1:length(all_PC)
y
for (i in y) {
  points(all_PC[[i]]$x,-all_PC[[i]]$y, pch='.', asp=1, cex=2, col="blue")  
}

} else { 
#if common points do not all exist->find midpoints of lines and
#determine sequence of lines by the angle of line's midpoints
  cas="Mpts"
  m3<-length(b13_angle_df$nr_corner)
  m3 #number of averaged common points
  n_PC #number of PCs forming the outline of the building
  names(b13_angle_df)=c("nr_center","alpha","x_centre","y_centre")
  b13_angle_df
  b13_angle_df$alpha<-0
  b13_angle_df$x_centre<-0  
  b13_angle_df$y_centre<-0
  b13_angle_df<-b13_angle_df[,1:4]
  x<-length(PC_nr)
  x
  y<-1:x
  y
  for(i2 in y){ 
    lnum=i2
    cat("nr=",i2)
    b13_angle_df<-mean_line(lnum)
  }
  #
  b13_angle_df
  b13_angle_df_2<-b13_angle_df
  b13_angle_df_2
  b13_angle_df_2_seq<-b13_angle_df_2[order(b13_angle_df_2$alpha, decreasing = FALSE),]
  b13_angle_df_2_seq
  #
  stop("missing corners") #manual solution for missing corners?
  #if necessary --> modify b13_angle_df_2_seq
  
  ##Output data of file with coordinates of the midpoints in the right sequence 
  fname7=paste("./data/b",obj,"_angle_df_seq.txt",sep="")
  fname7
  write.table(b13_angle_df_2_seq,fname7)
  #
  nr2<-b13_angle_df_2_seq$nr_center
  nr2 #number of the point cloud in the right sequence 
  fname8=paste("./data/nr2_b",obj,".txt", sep="")
  fname8
  write.table(nr2,fname8)
  #
  
  ##plot of outlines of building together with point cloud
  #b13_outlines<-read.table(fname7, header=TRUE)
  b13_outlines<-b13_angle_df_2_seq
  b13_outlines #all outlines of building
  points(pc3$col, -pc3$row, pch=20, asp=1, cex=0.3, col="red") # original pointcloud for building
  
  ##plot of separated and corrected point clusters 
  y<-1:length(all_PC)
  y
  for (i in y) {
    points(all_PC[[i]]$x,-all_PC[[i]]$y, pch='.', asp=1, cex=1.0, col="blue")  
  }
} #end of if-else condition

##output of case
#"Cpts"...Common points, "Mpts"...Midpoints
fname9=paste("./data/b_",obj_nr,"_case.txt", sep="")
fname9
cas
write.table(cas,fname9,row.names = FALSE, col.names = FALSE)
##end of program
#######################################################################################


