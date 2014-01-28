#install.packages('Quandl')
#library(Quandl)
#ats=Quandl('AMX')

symbols=c('AXP','CAT','SBUX')
ns=length(symbols)
R100=vector("list",ns)


#todo put in time series
for(si in 1:ns){#why should i have to type 1?
    data=read.csv(paste(symbols[[si]],'.csv',sep=''))
    sr=100*(data$Close-data$Open)/data$Open #simple return
    R100[[si]]=sr
}
names(R100)=symbols

#install.packages('moments')
library('moments')

compute=c(mean,sd,skewness,function(x){kurtosis(x)-3},min,max)
computenames=c("mean",'sd','skew','ex kurt','min','max')
nc=length(computenames)

p1=function(){

    for(ats in symbols){
        writeLines(ats)
        for(ic in 1:nc){
            writeLines(  paste(
                computenames[[ic]]
                ,'   '
                ,as.character(compute[[ic]](R100[[ats]])))
                       )
        }
    }
}

