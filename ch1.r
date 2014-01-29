

symbols1=c('AXP','CAT','SBUX','SP')
ns1=length(symbols1)
R100=vector("list",ns1)
r100=vector("list",ns1)

library(xts)
# return calculated as:
## > tts=xts(d$Close,as.Date(d$Date))
## > head((diff(tts)[-1]/as.vector(tts[-length(tts)])))
##                   [,1]
## 1999-01-05 -0.01911330
## 1999-01-06  0.04077943
## 1999-01-07  0.01206331
## 1999-01-08  0.03041861
## 1999-01-11 -0.01674995
## 1999-01-12 -0.03651765
#where tts is a xts ts
#from book
##  date      axp       cat      sbux
## 19990104 -0.009756 0.029891 -0.040089
## 19990105 -0.019089 -0.002639 -0.034803
## 19990106 0.043063 0.026455 -0.008413
## 19990107 0.012063 0.009021 0.003636
## 19990108 0.030393 0.042146 0.021739
## 19990111 -0.016773 0.039216 0.002364
## 19990112 -0.036471 -0.044811 0.003538
## 19990113 -0.03602 -0.050617 -0.008813
## 19990114 -0.027232 -0.027308 0.000593

for(si in 1:ns1){#why should i have to type 1?
    data=read.csv(paste(symbols1[[si]] ,'.csv',sep=''))
    closings=xts(data$Close ,as.Date(data$Date))
    sr=100*(diff(closings)[-1] #simple return
        /as.vector(closings[-length(closings)]))
    R100[[si]]=sr
    r100[[si]]=100*log(1+sr/100)
}
names(R100)=symbols1
names(r100)=symbols1

#install.packages('moments')
library('moments')

compute=c(mean,sd,skewness,function(x){kurtosis(x)-3},min,max)
computenames=c("mean",'sd','skew','ex kurt','min','max')
nc=length(computenames)

p1=function(){

    for(ats in symbols1[-4]){
        writeLines(ats)
        for(ic in 1:nc){
            writeLines(  paste(
                computenames[[ic]]
                ,'   '
                ,as.character(round(compute[[ic]](R100[[ats]]),3))
                ,'   '
                ,as.character(round(compute[[ic]](r100[[ats]]),3))
                )
                       )
        }
        if(t.test(r100[[ats]])$p.value>.05)
            {writeLines('accept=> H0: mu=0')}
        else{writeLines('reject=> Ha: mu!=0')}
    }
}

data2=read.table('m-gm3dx7508.txt',header=TRUE)


p2=function(){

    for(ats in names(data2)){
        writeLines(ats)
        for(ic in 1:nc){
            writeLines(  paste(
                computenames[[ic]]
                ,'   '
                ,as.character(round(compute[[ic]](data2[[ats]]),3))
                )
                       )
        }
        if(t.test(data2[[ats]])$p.value>.05)
            {writeLines('accept=> H0: mu=0')}
        else{writeLines('reject=> Ha: mu!=0')}
    }
}


#sp=xts(data2$sp,strptime(data2$date,format='%Y%m%d'))
sp=r100[['SP']]/100
p3=function(){

#add log returns for each year
    for(ay in 1975:2013){
        writeLines(paste(as.character(ay),'avg annual return   '
            ,round(mean(sp[as.character(ay)]),3)#asdate woudl be better
                                        ))
    }

writeLines(paste(
    'factor from 1975 to 2013 '
    ,round(exp(sum(sp['1975::2013'])),3)

    ))

}




