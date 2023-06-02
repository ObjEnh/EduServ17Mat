## name of program: Hough transform for line detection.r
## separation of Point clusters representing line segments of the building outline
## author: J. Höhle
## date of generation: 10.3.2019

setwd("C:/Users/Joa/Dropbox/EduServ17/R_programs")
library("EBImage")

##input 
obj=13
f1=paste("./data/idxy_LCM_RCD30_b_",obj,".csv",sep="")
#f1
pc2<-read.table(f1, header=TRUE)
names(pc2)[2]<-"col"
names(pc2)[3]<-"row" 
length(pc2$row)
nrow<-length(pc2$col)
coor<-read.table("./data/coor.csv")
max(pc2$col)
max(pc2$row)
min(pc2$col)
min(pc2$row)

##plot of origin, center, and point cluster of extracted building
x=0
y=0
plot(x,-y, pch=3, cex=2, col="black", asp=1, xlim=c(1,480), ylim=c(-480, -1))
x=coor[obj,1] 
y=coor[obj,2]
x
y
points(x,-y, pch=3, cex=2, col="green", asp=1)
points(pc2$col,-pc2$row, pch=".", cex=1.6, col="blue", asp=1)
##end of plot b13
#############################################################

##Preparation for Hough transformation
theta_step<-5 #step in angle 
ro_step<-5 #step in distance
X_min=min(pc2$col)
Y_min=min(pc2$row)
X_max=max(pc2$col)
Y_max=max(pc2$row)

##calculation of Dis_min; Dis_max
Dis_min<-sqrt(X_min*X_min+Y_min*Y_min)
Dis_min<-as.integer(Dis_min)
Dis_min
Dis_max<-sqrt(X_max*X_max+Y_max*Y_max)
Dis_max<-as.integer(Dis_max)
Dis_max
theta<-(seq(0,355, by=theta_step))
#theta
n_theta<-length(theta)
n_theta
ro<-(seq(0,Dis_max,by=ro_step))
n_ro<-length(ro)
n_ro
omega=180/pi
#

i<-1
while (i< n_theta)
{
  i<-i+1
  theta_rad<-theta/omega
}

H<-array(dim=c(n_theta, n_ro)) # init Hough matrix 
####################################################################################

## Extracting seperated point clusters (P)

i<-0
while (i<n_theta){
  i<-i+1
  H[,]<-0
}
#end H null-stellung

##H display of parameter space
ratio<-(n_ro-1)/(ro[n_ro]-ro[1])
ratio
X<-pc2$col
Y<-pc2$row
X[1]
Y[1]
ro2<-rep(0,n_ro)
ro_index<-rep(0,n_ro)
n_X<-length(pc2$col)
n_X
P<-matrix(nrow=n_X, ncol=3)
head(P)

##loop for alle Punkte
lnr=1 #specify line nr
P[,]<-0
k1<-0
k3=1
while (k1<n_X){
  k1<-k1+1
  i<-0
  while (i < n_theta){
    i<-i+1
    ro2[i]<-cos(theta_rad[i])*X[k1]+sin(theta_rad[i])*Y[k1]
    ro_index[i]<-round(ratio*(ro2[i]-ro[1]+1))
    #ro_index[i]<-abs(ro_index[i]) #absolute ro_index
    if (ro_index[i] >=1 && ro_index[i] <= n_ro ){
      #if (ro_index[i] <= n_ro ){
      k2<-ro_index[i]
      H[i,k2]<-H[i,k2]+1
    } #end ro index
  } #end loop i
} #end loop k1
n_P=k3-1
cat("n_P=",n_P,"lnr=",lnr,"\n")

##Generation of pointcloud
antal<-n_theta*n_ro
pointcloud<-matrix(nrow=antal,ncol=3)
i<-0
while (i<n_theta){
  i<-i+1
  j<-0
  while (j<n_ro){
    j<-j+1
    #    cat("i=",i,"j=",j, "\n")
    n1<-n_ro*(i-1)+j
    pointcloud[n1,1]<-i
    pointcloud[n1,2]<-j
    pointcloud[n1,3]<-H[i,j]
  } #end loop j
} #end loop i
#end generation of pointcloud

A<-pointcloud
B<-A[order(A[,3],decreasing = TRUE),]
head(B)

## storage of B
f1=paste("./data/parameter_space_b_",obj,".txt",sep="")
f1
write.table(B, f1)

##plotting of H with intentsities
H2<-H
H3<-H2[,]/B[1,3]
display(H3) #display of the parameter space 

##end of Hough trans 


###############################################################
##loop for checkplot of extracted point clusters
################################################################
lnr=1
while (lnr<=90){ #loop for 90 point clouds
  i<-0
  while (i<n_theta){
    i<-i+1
    H[,]<-0
  }
  #end H null-stellung
  #H
  ratio<-(n_ro-1)/(ro[n_ro]-ro[1])
  ratio
  X<-pc2$col
  Y<-pc2$row
  X[1]
  Y[1]
  ro2<-rep(0,n_ro)
  ro_index<-rep(0,n_ro)
  n_X<-length(pc2$col)
  n_X
  P<-matrix(nrow=n_X, ncol=3)
  P[,]<-0
  head(P)
  
  ##loop for all point clusters
  k1<-0
  k3=1
  while (k1<n_X){
    k1<-k1+1
    i<-0
    while (i < n_theta){
      i<-i+1
      ro2[i]<-cos(theta_rad[i])*X[k1]+sin(theta_rad[i])*Y[k1]
      ro_index[i]<-round(ratio*(ro2[i]-ro[1]+1))
      if (ro_index[i] >=1 && ro_index[i] <= n_ro ){
        k2<-ro_index[i]
        H[i,k2]<-H[i,k2]+1
        if (i==B[lnr,1] && k2==B[lnr,2]){ 
          P[k3,1]<-k3
          P[k3,2]<-X[k1]
          P[k3,3]<-Y[k1]
          k3<-k3+1
        } #end if parameter 
      }#end ro index
    } #end loop i
  } #end loop k1
  n_P=k3-1
  #cat("lnr=",lnr,"n_P=",n_P,"\n")
  f<-paste("./data/b_",obj,"_",lnr,".txt",sep="")
  write.table(P[1:n_P,],file=f, sep="   ")
  points(P[,2],-P[,3], pch=".", asp=1, cex=0.6, col="red") #switch to 'Plots' to see plot
  #end plot of lines
  lnr=lnr+1
} #end loop while
## end of b13




