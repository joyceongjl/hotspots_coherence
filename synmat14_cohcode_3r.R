#code for synmat14 function using only coh.sig.fft code for 2 rows
#only returns the rows with data instead of huge matrix
synmat14<-function(indata, istart, times=NULL, tsrange=c(0,Inf), nsurrogs=1000,  
                 scale.min=2, scale.max.input=NULL, sigma=1.05, f0=1, rescale01=FALSE)
{
  nlocs<-nrow(indata)
  ntimes<-ncol(indata)
  timescales<-wt(indata[1,], times, scale.min, scale.max.input, sigma, f0)$timescales
    synmat14<-matrix(NA,nlocs,nlocs) #compute the matrix
    for (i in istart:(istart+2))
    {
      for (j in 1:(i-1))
      {
        synmat14[i,j]<-cohtestfast(indata[i,], indata[j,], nsurrogs, min.scale=scale.min, 
                                   max.scale=scale.max.input, sigma, f0, tsranges=tsrange)$pvals
      }
  }
    return(synmat14[istart:(istart+2),])
} 