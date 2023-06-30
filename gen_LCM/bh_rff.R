bh_rff<-function(data, M, gamma)
  # gamma to sample RFF coefficients
  # M number of RFF features
{
  D = ncol(data)
  nsamples =nrow(data)
  if (missing(gamma))
  {
    if (nsamples>10000)
    {
      rp<- sample(nsamples)
      sdata <- data[rp[1:1000],]
    }
    else
      sdata <- data
    dis <- dist(as.matrix(sdata))
    sigma <-mean(dis) 
  }
  else 
  {
    sigma = sqrt(2*gamma)
  }
  # sample random fourier coefficients
  rf_c<- rnorm(M*D, mean=0, sd=sigma)
  rff_g<-reshape(as.matrix(rf_c), D, M)
  # sample the bias from uniform distribution
  rf_u<- runif(M, min=0, max=2*pi)
  rff_coef<-rbind(rff_g, rf_u)
  
}