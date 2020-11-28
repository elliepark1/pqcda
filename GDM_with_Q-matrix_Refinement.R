library(CDM)

# we use an example data (fraction subtraction)
data(data.fraction2, package="CDM")
data <- data.fraction2$data


# an original q-matrix defined by human experts 
q1 <- data.fraction2$q.matrix2


# check dimensions of the data and q-matrix 
dim(data)
dim(q1)


# fit general diagnostic model (GDM) to the data using the original q-matrix   
out.GDM <- gdm(data, theta.k=c(0,1), Qmatrix=q1, skillspace="loglinear", irtmodel="2PL",maxiter=5000)
summary(out.GDM) 


# The original q-matrix are refined by 3 methods (de la Torre, 2008; Chiu, 2013) 

# 1) De la Torre Dina  
out.DelaTorre.DINA <- CDM::din(data, q.matr=q1, rule="DINA")
qm.DelaTorre.DINA <- CDM::din.validate.qmatrix(out.DelaTorre.DINA, IDI_diff=.02, print=F)$q.matrix.prop


# 2) De la Torre DINO #
out.DelaTorre.DINO <- CDM::din(data, q.matr=q1, rule="DINO")
qm.DelaTorre.DINO <- CDM::din.validate.qmatrix(out.DelaTorre.DINO, IDI_diff=.02, print=F)$q.matrix.prop


# 3) Q-matrix validation: Chiu method 
library(NPCD) 

# to be able to use Chiu method, data with missing values needs to be imputed   
library(mice)
# imputed_Data <- mice(data, m=1, maxit = 50, method = 'pmm', seed = 500)
# summary(imputed_Data)
# completedData <- complete(imputed_Data,1) 

qm.CHIU <- NPCD::Qrefine(data, q1, gate="AND", max.ite=50)
class(qm.CHIU)
print(qm.CHIU)



## fit general diagnostic model (GDM) to the data using the refined q-matrices  

# GDM With refined Q-matrix by De la Torre DINA-method 
out.DelaTorre.DINA.GDM <- gdm(data, theta.k=c(0,1), Qmatrix=qm.DelaTorre.DINA, skillspace="loglinear", irtmodel="1PL", maxiter=5000)


# GDM with refined Q-matrix by de la Torre DINO-method 
out.DelaTorre.DINO.GDM <- gdm(data, theta.k=c(0,1), Qmatrix=qm.DelaTorre.DINO, skillspace="loglinear", irtmodel="1PL", maxiter=5000)


# GDM with refined Q-matrix by Chiu-method 
out.CHIU.GDM <- gdm(data, theta.k=c(0,1), Qmatrix=qm.CHIU$modified.Q, skillspace="loglinear",irtmodel="1PL", maxiter=5000)


## skill distribution table comparision among the original and 3 refined q-matrices  
out.GDM$mean.trait 
out.DelaTorre.DINA.GDM$mean.trait
out.DelaTorre.DINO.GDM$mean.trait
out.CHIU.GDM$mean.trait


out.EAP.Original <- round(out.GDM$person[,c(3,5,7,9,11)])
out.EAP.qm.DelaTorre.DINA <- round(out.DelaTorre.DINA.GDM$person[,c(3,5,7,9,11)])
out.EAP.qm.DelaTorre.DINO <- round(out.DelaTorre.DINO.GDM$person[,c(3,5,7,9,11)])
out.EAP.qm.CHIU <- round(out.CHIU.GDM$person[,c(3,5,7,9,11)])




