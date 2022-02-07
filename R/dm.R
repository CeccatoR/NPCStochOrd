#Aim:
#- Compute difference in mean test statistic for stochastic ordering problems

#Input:
#- x: numeric vector containing the outcome variable
#- label: vector containing the group labels

#Output:
#- T_dm: test statistic


dm<-function(x,label){    
  
  T_dm=mean(x[label==1])-mean(x[label==2])
  
  T_dm
}
