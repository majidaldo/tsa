library(fGarch)

set.seed(1234)
n=200
eps=rnorm(n,mean=0,sd=1)
A=vector(mode='numeric',length=n)
A[1]=1.0
A[2]=2.0



sigsqt=function(Atminus2){return(1+2*Atminus2^2)}
At=function(sigt,epst){return(sigt*epst)}
#Atf=function(t){A[t]=At(sigsqt(A[t-2])^.5,eps[t-2])}
#why can't i write to the global var??
#A=sapply(3:n,Atf)
for(t in 3:n){A[t]=At(sigsqt(A[t-2])^.5,eps[t])}

gf10=garchFit(formula=~garch(1,0),data=A)
gf20=garchFit(formula=~garch(2,0),data=A)
gf21=garchFit(formula=~garch(2,1),data=A)
