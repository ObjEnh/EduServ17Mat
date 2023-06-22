##adjustment of a line with parameters phi (angle) and intercept (a)
#residuals are orthogonal to line
#Name of program: adj_of_line.r
#author: JH
#date of creation/modification: 22.2.2019
#example b13

setwd("C:/Users/Joa/Dropbox/EduServ17/R_programs")
PC_nr<-read.table("./data/b13_PC_nr.txt",header=TRUE) #
PC_nr
names(PC_nr)="lnr"
PC_nr$lnr
bnr=14 #number of building 
all_PC<-list("PC", "PC", "PC", "PC", "PC", "PC") 

##plot point clouds of building and origo (small scale) 
x<-0
y<-0
plot(x, -y, pch=3, col='red', asp=1, cex=1.5, xlim=c(1,480), ylim=c(-480,-1), main="b13")

##Input of center coordinates
b13_center<-read.table("./data/b13_center.txt")
xc<-b13_center$x[1]
yc<-b13_center$x[2]
points(xc,-yc, pch=3, cex=2, col="green", asp=1, xlim=c(200,240), ylim=c(-370,-300), xlab="col", ylab="row",main="b13")
##

##input of individual point clusters (PCs)
#cat("sequence of PCs:")
#PC_nr$lnr 
x=length(PC_nr$lnr)
y=1:x

#loop input of list PC_all
for (i in y){
  fname8<-paste("./data/all_PC$PC_",PC_nr$lnr[i],".txt",sep="") 
  fname8
  all_PC[[i]]<-read.table(fname8)
} #end of input of list PC_all (loop "i")
#

## plot PCs
# loop for each point cluster
for (i in y) { 
  ##plot of separated and corrected point clusters 
  points(all_PC[[i]]$x,-all_PC[[i]]$y, pch='.', asp=1, cex=2.5, col="blue")
} # end of input PCs


## input of B (corrected)
fname9<-"./data/unsorted_lines_b13.txt"
B6<-read.table(fname9, header=T)
B6<-subset(B6,select=c(lnr,theta_angle,ro_pixel,n_pixel)) #modified, correct?
B6[,5:6]<-0
names(B6)<-c("PC_nr","theta_ang","ro_pixel","n", "theta_adj", "ro_adj")
B6
#

##derivation of line parameters (with residuals orthogonal to line)
omega<-180/pi

#loop i
for (i in y){ 
  k4=nrow(all_PC[[i]])
  k4
  all_PC[[i]][1:k4,]
  #loop j
  j=0
    while(j<k4){
    j=j+1
    x<-all_PC[[i]]$x[j]
    y<-all_PC[[i]]$y[j]
    points(x,-y, pch=16, cex=0.4, col="brown", asp=1)
    }
  x_dat<-all_PC[[i]]$x
  y_dat<-all_PC[[i]]$y
  xs<-sum(x_dat)/k4
  xs
  ys<-sum(y_dat)/k4
  ys
  x_dat_v<-x_dat-xs
  x_dat_v
  y_dat_v<-y_dat-ys
  y_dat_v
  N<-(t(x_dat_v)%*%x_dat_v-t(y_dat_v)%*%y_dat_v)
  phi_2<-(2*t(x_dat_v)%*%y_dat_v)/N
  phi_2
  phi_2_deg<-omega*atan(phi_2) #two solutions are possible for phi_2 (phi_2 or phi_2 +180)
  phi_2_deg
  phi_deg<-0.5*(phi_2_deg) #calculation of angle phi (slope angle)
  phi_arc<-phi_deg/omega
  
  # solution 1
  if (phi_deg < B6[i,2]+25 && phi_deg > B6[i,2]-25) { 
  phi_deg_b<-phi_deg+90
  B6[i,5]=phi_deg_b-90 # solution 1 
  phi_arc_b<-phi_deg_b/omega
  b1<-tan(phi_arc_b)
  b1
  a1<-ys-xs*tan(phi_arc_b)
  a1
  B6[i,6]<-a1*sin(phi_arc)
  abline(-a1,-b1, col="green") #plot of lines orthogonal to main line
  } #end of if  
  else {
    b<-tan(phi_arc)
    a<-ys-xs*tan(phi_arc)
    B6[i,6]<-a*cos(phi_arc) 
    B6[i,5]=phi_deg+90 #solution 2
    abline(-a,-b, col="brown") #plot of lines parallel to main line
  } #end of loop j
} #end of loop i (generation of line parameters)

##output of results (B6)
fname9<-paste("./data/param_adj_b",bnr,".txt",sep="") 
write.table(B6,fname9)
cat("Approximate and adjusted line parameters:","\n")
#end of program "adj_of_line.r


