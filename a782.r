
r=read.table('m-gs1n10.txt',header=TRUE)[-1][-1][-1]
c=apply(r,2,diff)#makes it stationary

library('MTS')
arm=VAR(c,p=1,include.mean=TRUE)
#rearrange to get second structural eqn
arm2=VAR(cbind(c[,2],c[,1]),p=1,include.mean=TRUE)

#arm
## $Sigma
##            [,1]       [,2]
## [1,] 0.16137482 0.08352195
## [2,] 0.08352195 0.07002164

## $Phi
##             [,1]      [,2]
## [1,]  0.20350694 0.3150519
## [2,] -0.03637767 0.3510741

## $Ph0
## [1] -0.002792771  0.000101311

## > LD(arm$Sigma)
##           [,1] [,2]
## [1,] 1.0000000    0
## [2,] 0.5175649    1
## >

#arm2
## $Sigma
##            [,1]       [,2]
## [1,] 0.07002164 0.08352195
## [2,] 0.08352195 0.16137482

## $Phi
##           [,1]        [,2]
## [1,] 0.3510741 -0.03637767
## [2,] 0.3150519  0.20350694

## $Ph0
## [1]  0.000101311 -0.002792771

## > LD(arm2$Sigma)
##          [,1] [,2]
## [1,] 1.000000    0
## [2,] 1.192802    1
## >


#LDLT decomposition
LD=function(A){
U=chol(A)
temp=diag(U)
L=t(U/temp)
D=diag(temp^2)
#return(list(L,D))
return(L)
}
#should_be_A <- L%*%D%*%t(L)

mam=VMA(c,q=1,include.mean=TRUE)

## $Sigma
##            [,1]       [,2]
## [1,] 0.15080036 0.07785418
## [2,] 0.07785418 0.06644951

## $Theta
##            [,1]       [,2]
## [1,] -0.3593162 -0.2934983
## [2,]  0.0144947 -0.4570243

## $mu
## [1] -0.002593286  0.001122851
