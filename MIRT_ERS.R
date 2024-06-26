

# Author: Jung Yeon (Ellie) Park 
# Multidimensional IRT Approach for ERS (Compensatory rule) 
updateparams.comp<-function(response, theta.current, beta.current, Q.current, K=0.2) 
{
  require(boot)
  exp.response<-inv.logit(sum(theta.current*Q.current)-beta.current)
  #exp.response <- ( exp(2*( sum(theta.current*Q.current)-beta.current ))+1 )/( exp(2*( sum(theta.current*Q.current)-beta.current ))-1 )-1/(sum(theta.current*Q.current)-beta.current)
  theta.update <- theta.current+Q.current*K*(response-exp.response)
  beta.update <- beta.current#-K*(response-exp.response)
  return(list(theta.update,beta.update,exp.response))
}



y.data1<-NULL
beta.history1<-NULL
theta.history1<-NULL
y.history1<-NULL
true.theta1<-NULL
for (jjj in 1:length(qj))  
{
  Q.matrix<-q[[jjj]]
  
  for (iii in 1:length(cj))    
  {
    #setwd(paste("C:/Users/~",as.integer(qj[jjj]),"-r",cj[iii],sep=""))
    #load("true.theta.RData") #true.theta
    #load("data.RData") #data.seq
    
    theta.estimate.ini<-matrix(0,N.student,ncol(Q.matrix))  
    beta.estimate.ini<-beta.item #rep(0,N.item)
    theta.estimate.current<-theta.estimate.ini
    beta.estimate.current<-beta.estimate.ini
    y.estimate.current<-NULL
    
    # update the parameter estimates along the seq of item-student combination
    theta.history<-array(NA, dim=c(N.student, ncol(Q.matrix), N.answer+1))
    beta.history<-matrix(NA, nrow=N.item, ncol=max(table(data.seq$item))+1)     
    y.history<-matrix(NA, nrow=N.student, ncol=N.answer+1)
    y.data<-matrix(NA, nrow=N.student, ncol=N.answer+1)
    
    theta.history[,,1]<-theta.estimate.ini
    beta.history[,1]<-beta.estimate.ini
    k.seq<-seq(0.4,0.2,length.out = N.session*item.session)
    
    for( i in 1:nrow(data.seq)) 
    {
      curr.student<-data.seq$student[i] 
      curr.item<-data.seq$item[i]
      curr.item.time<-data.seq$individual.item.time[i]
      curr.response<-(2*data.seq$response[i]-1)*(1-curr.item.time) 
      curr.sessio.no<-data.seq$session[i]
      curr.item.no<-data.seq$item.no[i] 
      curr.Q<-Q.matrix[curr.item,]
      curr.k.seq<-item.session*(curr.sessio.no-1)+curr.item.no 
      
      updated.params<-updateparams.comp(curr.response, theta.estimate.current[curr.student,],                 
                                           beta.estimate.current[curr.item], curr.Q, K=0.2) 
      
      print(c(i,curr.Q, curr.student,curr.item, curr.response, theta.estimate.current[curr.student,],
              beta.estimate.current[curr.item], updated.params[[1]],updated.params[[2]])) 
      
      theta.estimate.current[curr.student,]<-updated.params[[1]]
      beta.estimate.current[curr.item]<-updated.params[[2]]
      y.estimate.current[curr.student]<-updated.params[[3]] 
      
      theta.history[curr.student,,min(which(is.na(theta.history[curr.student,1,])))]<-updated.params[[1]]   
      beta.history[curr.item,min(which(is.na(beta.history[curr.item,])))]<-updated.params[[2]]
      y.history[curr.student,min(which(is.na(y.history[curr.student,])))]<-updated.params[[3]]
      y.data[curr.student,min(which(is.na(y.history[curr.student,])))]<-curr.response
    }
    y.data1[[(jjj-1)*length(cj)+iii]]<-y.data
    y.history1[[(jjj-1)*length(cj)+iii]]<-y.history
    beta.history1[[(jjj-1)*length(cj)+iii]]<-beta.history
    theta.history1[[(jjj-1)*length(cj)+iii]]<-theta.history  
    true.theta1[[(jjj-1)*length(cj)+iii]]<-true.theta 
  }
}


final.sim.result<-list(y.history1=y.history1, y.data1=y.data1, beta.history1=beta.history1,
                       theta.history1=theta.history1, true.theta1=true.theta1) 



