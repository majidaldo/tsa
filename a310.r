aa=read.table('w-Aaa.txt')
ba=read.table('w-Baa.txt')

ymd2date=function(y,m,d){return(  as.Date(paste(y,m,d,sep='-'))  ) }
library('xts')
aa=xts(aa$V4,ymd2date(aa$V1,aa$V2,aa$V3))
ba=xts(ba$V4,ymd2date(ba$V1,ba$V2,ba$V3))

library(psych)
#describe(aa)
library(moments)
library(forecast)
library(fUnitRoots)

maa=apply.monthly(aa,mean)
mba=apply.monthly(ba,mean)

pdq=c(10,1,0)
aafit=Arima(maa,order=pdq)
bafit=Arima(mba,order=pdq)

plotfit = function(fit){
    plot(fit$x)
    lines(xts(fitted(fit),as.Date(index(fit$x))),col='blue')
}

dmaa=diff(maa)[-1]
dmba=diff(mba)[-1]

#lf=lm(maa~mba)
#lfr=Arima(lf$residuals,order=c(0,0,10))
