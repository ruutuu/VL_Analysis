
# Function to extract results:
## Function 1
#Extract estimates from the copas object.
#Make a function: Input= Copas object; Output data frame

m_extract<-function(m_adj,catname){
  
  
  #0) Orignal estiamte: 
  est0<-exp(m_adj$TE.random)/(1+exp(m_adj$TE.random))
  #2. Lower and upper bound
  lb0<-exp(m_adj$lower.random)/(1+exp(m_adj$lower.random))
  ub0<-exp(m_adj$upper.random)/(1+exp(m_adj$upper.random))
  
  #1. Estimate
  est<-exp(m_adj$TE.adjust)/(1+exp(m_adj$TE.adjust))
  #2. Lower and upper bound
  lb<-exp(m_adj$lower.adjust)/(1+exp(m_adj$lower.adjust))
  ub<-exp(m_adj$upper.adjust)/(1+exp(m_adj$upper.adjust))
  
  #3. I2 value
  i2<-m_adj$x$I2
  
  #4. Lower and upper bound for I2
  i2lb<-m_adj$x$lower.I2
  i2ub<-m_adj$x$upper.I2
  
  #5. Calculation of prediction interval
  #se<-m_adj$seTE.adjust
  
  #pilb<-est-t*sqrt(i2^2+se^2)
  
  #Total no of studies
  n<-length(unique(m_adj$x$data$pub_id))
  
  arms<-length(m_adj$x$data$pub_id)
  #Total study arms
  
  #Total patients
  tot<-sum(m_adj$x$data$sa_num_tx,na.rm = TRUE )
  
  #Total cured
  tot_cured<-sum(m_adj$x$data$n_icured,na.rm = TRUE )
  
  #Total relapsed
  tot_rel<-sum(m_adj$x$data$frel180 ,na.rm = TRUE )
  
  
  op<-as.data.frame(t(as.data.frame(c(est0, lb0, ub0, est, lb, ub, i2, i2lb, i2ub, n, arms, tot, tot_cured, tot_rel))))
  colnames(op)<-c("est0","lb0","ub0","est","lb","ub","i2","i2lb","i2ub","n","arms","tot", "cured","relapse")
  rownames(op)<-NULL
  
  
  op <- sapply(op,as.numeric)
  op <- sapply(op, function(x) round(x,4))
  
  op<-as.data.frame(t(op))
  op$name<-catname
  
  return(op)
}



## Function 2


#Extract estimates from the copas object.
#Make a function: Input= Copas object; Output data frame

m_extract0<-function(m_unadj,catname){
  
  
  #0) Orignal estiamte: 
  est0<-exp(m_unadj$TE.random)/(1+exp(m_unadj$TE.random))
  #2. Lower and upper bound
  lb0<-exp(m_unadj$lower.random)/(1+exp(m_unadj$lower.random))
  ub0<-exp(m_unadj$upper.random)/(1+exp(m_unadj$upper.random))
  
  #1. Estimate
  #est<-exp(m_unadj$TE.adjust)/(1+exp(m_unadj$TE.adjust))
  #2. Lower and upper bound
  #lb<-exp(m_unadj$lower.adjust)/(1+exp(m_unadj$lower.adjust))
  #ub<-exp(m_unadj$upper.adjust)/(1+exp(m_unadj$upper.adjust))
  
  #3. I2 value
  i2<-m_unadj$I2
  
  #4. Lower and upper bound for I2
  i2lb<-m_unadj$lower.I2
  i2ub<-m_unadj$upper.I2
  
  #5. Calculation of prediction interval
  #se<-m_unadj$seTE.adjust
  
  #pilb<-est-t*sqrt(i2^2+se^2)
  
  #Total no of studies
  n<-length(unique(m_unadj$data$pub_id))
  
  arms<-length(m_unadj$data$pub_id)
  #Total study arms
  
  #Total patients
  tot<-sum(m_unadj$data$sa_num_tx,na.rm = TRUE )
  
  #Total cured
  tot_cured<-sum(m_unadj$data$n_icured,na.rm = TRUE )
  
  #Total relapsed
  tot_rel<-sum(m_unadj$data$frel180 ,na.rm = TRUE )
  
  est<-99
  lb<-99
  ub<-99
  op<-as.data.frame(t(as.data.frame(c(est0, lb0, ub0, est, lb, ub, i2, i2lb, i2ub, n, arms, tot, tot_cured, tot_rel))))
  colnames(op)<-c("est0","lb0","ub0","est","lb","ub","i2","i2lb","i2ub","n","arms","tot", "cured","relapse")
  rownames(op)<-NULL
  
  
  op <- sapply(op,as.numeric)
  op <- sapply(op, function(x) round(x,4))
  
  op<-as.data.frame(t(op))
  op$name<-catname
  
  return(op)
}