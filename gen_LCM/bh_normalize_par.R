bh_normalize_par<-function(Data)
{
  ##
  mu=colMeans(Data);sigma=colSds(Data);
  sz<-dim(Data)
  #NData<-matrix(0,nrow(Data),ncol(Data))
 # NData<-(Data-repmat(mu, nrow(Data),1))/(repmat(sigma,nrow(Data),1))
  #   
    if (sz[1]<=5000)
    {
      NData<-(Data-repmat(mu, nrow(Data),1))/(repmat(sigma,nrow(Data),1))
    } else
    {
      source('bh_loadparallel_Lib.R')
      cl<-bh_loadparallel_Lib()
      #
      partition=5000;
      Nsamples=nrow(Data);
      Npart=ceil(Nsamples/partition);
      
      NData<-foreach (k =1:Npart, .combine=rbind, .packages = 'pracma')%doPar%
      {
        st=((k-1)*partition)+1;
        last=k*partition;
        if (k==Npart)
        {
          last<-Nsamples
           
        }
       
        (Data[st:last,]-repmat(mu, nrow(Data[st:last,]),1))/(repmat(sigma,nrow(Data[st:last,]),1))
        
      
      
    }
    
      stopCluster(cl)
      
    } 
  
  #   ## [0-1] scaling
  #   NData<-Data-repmat(col.min(Data), nrow(Data),1)/repmat()
  return(NData)
}

