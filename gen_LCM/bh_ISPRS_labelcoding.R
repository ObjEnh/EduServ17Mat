bh_ISPRS_labelcoding<-function(Img)
{ 
  # ISPRS class color label
  c = matrix(c(255,255,255, 0,0,255, 0,255,255, 0,255,0, 255,255,0, 255,0,0),
             nrow=3, ncol=6)
  c= t(c)
  nclass = nrow(c)
  Img = as.matrix(Img)
  size= dim(Img)
  if (size[2]==3)
  {
    labels = zeros(size[1],1)
    for (i in 1:nclass)
    {
      index = Img[,1] == c[i,1] & Img[,2] == c[i,2] & Img[,3] == c[i,3]
      if (sum(index>0))
         labels[index]=i
      rm(index)
    }
    #labels = reshape(labels, size[1], size[2])
  }
  else if (size[2]==1)
  {
    labels = zeros(size[1],3)
    for (i in 1:nclass)
    {
      index = Img==i
      labels[index,] = c[i,]
    }
    # labels = reshape(lables, size[1], size[2],3)
  }
  return(labels) 
}


    
    
    
#     
#   nclass = length(unique(train_label))
#   Nsamples = length(train_label)
#   codelabel = -1*matrix(1, Nsamples, nclass)
#   
#   for (i in 1:nclass)
#   {
#     index = which(train_label==i)
#     codelabel[index,i]=1
#   }
#   return(codelabel)
# }