un=read.table('m-unrate.txt',header=TRUE
    ,colClasses=c("character","character","character","numeric")
    )

ymd2date=function(y,m,d){return(  as.Date(paste(y,m,d,sep='-'))  ) }
##why doesn't this work?? mapply(ymd2date,c("2000"),c("11"),c("11"))
library('xts')
dp=1
ti=ymd2date(un$Year,un$Mon,un$Day)
ii=seq(1,length(ti),by=dp) #every seven days
ti=ti[ii]
un=un[ii,]
fti=seq(as.Date("2009-4-1"),as.Date("2009-7-01"),by="month")
un=xts(un$Rate,ti)

library('forecast')

fitarima = function(p,d,q){
return (Arima(un,order=c(p,d,q))) #captial A Arima in forecast pkg
}

plotfit = function(unfit){
    plot(unfit$x)
    lines(xts(fitted(unfit),ti),col='blue')
}


#plot(forecast(unfit,h=
