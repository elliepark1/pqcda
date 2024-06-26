

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





