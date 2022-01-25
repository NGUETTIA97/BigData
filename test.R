data1=function(data){
  
  
  
  data$Rev_mois[is.na(data$Rev_mois)==TRUE] <- median(data$Rev_mois,na.rm=T)
  data$P_charge[is.na(data$P_charge)==TRUE] <- median(data$P_charge,na.rm=T)
  
  
  
  return(data)
}