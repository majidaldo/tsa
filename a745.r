
source('financetools.r')
if(FALSE==exists('idx')){
    idx=get.stock.price('GE', start.date=c(1,1,1926)
                             , stop.date=c(12,31,2008)
        ,freq='m')
}
r=(idx[-1]/idx[-length(idx)])

k=3
n=length(r)
yr=r[(k+1):n]
rtm1=r[3:(n-1)];
rtm2=r[2:(n-2)]
rtm3=r[1:(n-3)]
xr=cbind(rtm1,rtm2,rtm3)

library('nnet')
nnr=nnet(xr,yr,size=2,maxit=1000,skip=FALSE
    ,linout=TRUE) #why constant pred. if false??
rha=predict(nnr,xr)

library('Metrics')
#> mse(yr,rha)
#[1] 0.004272931

xr=cbind(xr,(ifelse(yr>1,1,0)))
xr=cbind(rtm1,ifelse(rtm1>0,1,0)
    ,rtm2,ifelse(rtm2>0,1,0)
    ,rtm3,ifelse(rtm3>0,1,0))
nnr=nnet(xr,yr,size=5,maxit=1000,skip=FALSE
    ,linout=TRUE)
rhb=predict(nnr,xr)

## > mse(yr,rhb)
## [1] 0.004049289
