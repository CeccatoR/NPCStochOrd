#Aim:
#- Combine parital p-values

#Input:
#- pv: array of partial p-values
#- fcomb: combining function to be used. "F"=Fisher, "L"=Liptak, "T"=Tippet (see Pesarin and Salmaso, 2010)

#Output:
#- pc: combined p-values


comb<-function(pv,fcomb){
  if(fcomb=="F"){
    #cat('Combining function: Fisher \n')
    pc<-apply(pv,1,function(x) -2*log(prod(x,na.rm=TRUE)))
  }
  if(fcomb=="L"){
    #cat('Combining function: Liptak \n')
    pc<-apply(pv,1,function(x) sum(qnorm(1-x),na.rm=TRUE))
  }
  if(fcomb=="T"){
    #cat('Combining function: Tippet \n')
    pc<-apply(pv,1,function(x) max((1-x),na.rm=TRUE)) 
  }
  
  return(pc)
  
}