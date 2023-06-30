bh_rasterclassificationMap<-function(Img, OrgImage, option)
{
  # Img <-classified Image
  # OrgImg <- OrgImg raster object to extract the attributes for classified image
  
  if (missing(option))
     { 
      sz<-dim(Img)
      I<-matrix(0, sz[1]*sz[2],3)
      
    #   zindx<-which(Img==0)
    #   bindx<-which(Img==1)
    #   findx<-which(Img==2)
    #   
    #   CM<-rbind(c(0,0,0), c(255,255,255), c(0,255,0))
      
      aindx<-which(Img==0)
      bindx<-which(Img==1)
      cindx<-which(Img==2)
      dindx<-which(Img==3)
      eindx<-which(Img==4)
      findx<-which(Img==5)
    
      
      CM<-rbind(c(0,0,0), c(255,255,255), c(0,0,255), c(0,255,255), c(0,255,0), c(255,255,0))
      for (i in 1:3)
      {
    #     I[zindx,i]<-CM[1,i]
    #     I[bindx,i]<-CM[2,i]
    #     I[findx,i]<-CM[3,i]
        
        I[aindx,i]<-CM[1,i]
        I[bindx,i]<-CM[2,i]
        I[cindx,i]<-CM[3,i]
        I[dindx,i]<-CM[4,i]
        I[eindx,i]<-CM[5,i]
        I[findx,i]<-CM[6,i]
    
    
      }
      
      
      
      Img3D<-reshape(I, sz[1],sz[2],3)
      r1<-raster(Img3D[,,1])
      r2<-raster(Img3D[,,2])
      r3<-raster(Img3D[,,3])
  }
    else
    { 
      
      r1<-raster(Img[,,1])
      r2<-raster(Img[,,2])
      r3<-raster(Img[,,3])
    }
  
  
  
  rf<-stack(r1,r2,r3)
  
  extent(rf)<-extent(OrgImage)
  projection(rf)<-projection(OrgImage)
  
  return(rf)
  
}