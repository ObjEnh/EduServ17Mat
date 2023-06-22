##calculation of final corner coordinates (theta,ro - approach)
##theta calculated by weighted average
##ro-values by least-squares adjustment
##name of program: adj_of_corner_coordinates.r
##author: JH
##date of version: 10.3.2019

setwd("C:/Users/Joa/Dropbox/EduServ17/R_programs")

#input 
bnr<-13 #label of building (to be changed for other buildings)
#input weighted mean
fname10="./data/theta_av.txt"
theta_av<-as.numeric(read.table(fname10))
cat("theta angle (weighted average) [degrees]","\n")
theta_av
theta<-theta_av

#
phi_deg<-c(theta-90,theta,theta-90,theta,theta-90,theta) #sequence of lines (5,1,25,89,71,8)
phi<-(phi_deg/180)*pi
m<-length(phi_deg)
m #number of lines in object
A<-matrix(nrow=2*m, ncol=m) #matrix for building with 6 corners
A[,]<-0
k1<-sin(phi[1])-cos(phi[1])*tan(phi[2])
k2<-cos(phi[2])*tan(phi[1])-sin(phi[2])
A[1,1]<-1/cos(phi[1])-tan(phi[1])/k1
A[2,1]<-1/k1
A[1,2]<-tan(phi[1])/k2
A[2,2]<-(-1/k2)
#
k1<-sin(phi[2])-cos(phi[2])*tan(phi[3])
k2<-cos(phi[3])*tan(phi[2])-sin(phi[3])
A[3,2]<-1/cos(phi[2])-tan(phi[2])/k1
A[4,2]<-1/k1
A[3,3]<-tan(phi[2])/k2
A[4,3]<-(-1/k2)
#
k1<-sin(phi[3])-cos(phi[3])*tan(phi[4])
k2<-cos(phi[4])*tan(phi[3])-sin(phi[4])
A[5,3]<-1/cos(phi[3])-tan(phi[3])/k1
A[6,3]<-1/k1
A[5,4]<-tan(phi[3])/k2 
A[6,4]<-(-1/k2)
#
k1<-sin(phi[4])-cos(phi[4])*tan(phi[5])
k2<-cos(phi[5])*tan(phi[4])-sin(phi[5])
A[7,4]<-1/cos(phi[4])-tan(phi[4])/k1
A[8,4]<-1/k1
A[7,5]<-tan(phi[4])/k2
A[8,5]<-(-1/k2)
#
k1<-sin(phi[5])-cos(phi[5])*tan(phi[6])
k2<-cos(phi[6])*tan(phi[5])-sin(phi[6])
A[9,5]<-1/cos(phi[5])-tan(phi[5])/k1
A[10,5]<-1/k1
A[9,6]<-tan(phi[5])/k2
A[10,6]<-(-1/k2)
#
k1<-sin(phi[6])-cos(phi[6])*tan(phi[1])
k2<-cos(phi[1])*tan(phi[6])-sin(phi[1])
A[11,6]<-1/cos(phi[6])-tan(phi[6])/k1
A[12,6]<-1/k1
A[11,1]<-tan(phi[6])/k2
A[12,1]<-(-1/k2)
#
A #design matrix

##adjustment of observations (ro)

#input of approximate coordinates 
fname12="./data/b13_coord_appr.txt"
b01<-read.table(fname12,col.names="xy")

b01 #approximate corner coordinates
b=rep(0,m)
b<-b01$xy

##adjustment of ro-values
At<-t(A)
AtA<-At%*%A
Atb<-At%*%b
x<-rep(0,m)
AtAinv<-solve(At%*%A)
x<-AtAinv%*%Atb

x #adjusted ro-values 
x1<-x

##residuals
r=A%*%x1-b
r #residuals [pixels]
max(r) #maximal residual [pixels]
rt<-t(r)
sigma<-sqrt((rt%*%r)/m) 
sigma #standard deviation of residuals [pixel]

##calculation of adjusted coordinates
p<-A%*%x1
p #adjusted coordinates x,y,...


# output of adjusted coordinates
fname13=paste("./results/b",bnr,"_coord_adj.txt",sep="")
#fname13
write.table(p,fname13)


##checks by plotting

##plot of adjusted corner coordinates
#seperate in x und y
#x
Points_x<-rep(0,7)
Points_x[7]<-p[1,1] #repeat first point (x-coordinate)
k=1
i=1
while(i<=11){
  Points_x[k]<-p[i,1]
  k=k+1
  i=i+2
} #end loop
#Points_x

#y
Points_y<-rep(0,7)
Points_y[7]<-p[2,1] #repeat first point (y-coordinated)
k=1
i=2
while(i<=12){
  Points_y[k]<-p[i,1]
  k=k+1
  i=i+2
} #end loop
#Points_y

#final coordinates of corners
b13_xy2<-cbind(Points_x,-Points_y)
dimnames(b13_xy2)[[2]]<-list("x","-y")
b13_xy2 
#

##plot in large scale
b13_center<-read.table("C:/Users/Joa/Dropbox/EduServ16/R_programming/data/b13_center.txt")
xc<-b13_center$x[1]
yc<-b13_center$x[2]
plot(xc,-yc, pch=3, cex=2, col="red", asp=1, xlim=c(200,240), ylim=c(-370,-300), xlab="col", ylab="row",main="b13")
#

##plot with line segments
k1=m
#loop
i=0
while(i<k1){
  i=i+1
  x<-Points_x[i]
  y<-Points_y[i]
  points(x,-y, pch=20, cex=2.0, col="green", asp=1)
  lines(b13_xy2,  col="red", asp=1, type="l", lwd=2, lty=1)
}
# end of plot of final corner points 

# output of final coordinates (plot-version)
fname14=paste("./results/b",bnr,"_coord_adj_plot.txt",sep="")
write.table(b13_xy2,fname14)
#
#end of adj_of_corner_coordinates.r


 







