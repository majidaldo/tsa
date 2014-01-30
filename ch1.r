

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
    closings=xts(data$Adj.Close ,as.Date(data$Date))
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
#pearson kurtosis
nc=length(computenames)





p1=function(){

    for(ats in symbols1[-4]){
        writeLines(paste(as.character(ats),'return     log return'))
 #writeLines(paste((ats),'return     log return'))
        #^^this line is writing a file!!! why?!?!
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

## AXP return     log return
## mean     0.066     0.035
## sd     2.492     2.486
## skew     0.384     0.011
## ex kurt     8.397     7.904
## min     -17.593     -19.349
## max     20.628     18.754
## accept=> H0: mu=0
## CAT return     log return
## mean     0.07     0.046
## sd     2.192     2.191
## skew     0.098     -0.107
## ex kurt     4.2     4.287
## min     -14.522     -15.691
## max     14.717     13.73
## accept=> H0: mu=0
## SBUX return     log return
## mean     0.098     0.067
## sd     2.5     2.504
## skew     0.086     -0.397
## ex kurt     9.103     12.658
## min     -28.185     -33.108
## max     18.444     16.927
## accept=> H0: mu=0






data2=read.table('m-gm3dx7508.txt',header=TRUE)

p2=function(){

    for(ats in names(data2[-1]) ){#take out date col
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


## > p2()
## gm
## mean     0.006
## sd     0.093
## skew     -0.385
## ex kurt     2.073
## min     -0.389
## max     0.277
## accept=> H0: mu=0
## vw
## mean     0.01
## sd     0.045
## skew     -0.745
## ex kurt     2.694
## min     -0.225
## max     0.142
## reject=> Ha: mu!=0
## ew
## mean     0.013
## sd     0.056
## skew     -0.301
## ex kurt     4.37
## min     -0.272
## max     0.299
## reject=> Ha: mu!=0
## sp
## mean     0.007
## sd     0.044
## skew     -0.573
## ex kurt     2.295
## min     -0.218
## max     0.132
## reject=> Ha: mu!=0


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

#P1.3
## > p3()
## 1975 avg annual return    0.023
## 1976 avg annual return    0.015
## 1977 avg annual return    -0.01
## 1978 avg annual return    0.001
## 1979 avg annual return    0.01
## 1980 avg annual return    0.019
## 1981 avg annual return    -0.009
## 1982 avg annual return    0.011
## 1983 avg annual return    0.013
## 1984 avg annual return    0.001
## 1985 avg annual return    0.019
## 1986 avg annual return    0.011
## 1987 avg annual return    0.002
## 1988 avg annual return    0.01
## 1989 avg annual return    0.02
## 1990 avg annual return    -0.006
## 1991 avg annual return    0.019
## 1992 avg annual return    0.004
## 1993 avg annual return    0.006
## 1994 avg annual return    -0.001
## 1995 avg annual return    0.024
## 1996 avg annual return    0.015
## 1997 avg annual return    0.023
## 1998 avg annual return    0.02
## 1999 avg annual return    0.015
## 2000 avg annual return    -0.009
## 2001 avg annual return    -0.012
## 2002 avg annual return    -0.022
## 2003 avg annual return    0.02
## 2004 avg annual return    0.007
## 2005 avg annual return    0.002
## 2006 avg annual return    0.011
## 2007 avg annual return    0.003
## 2008 avg annual return    -0.04
## 2009 avg annual return    0.018
## 2010 avg annual return    0.01
## 2011 avg annual return    0
## 2012 avg annual return    0.01
## 2013 avg annual return    0.022
## factor from 1975 to 2013  26.96


axp=r100[['AXP']]
axp4=axp['1999::2013']

p4=function(){


    if(  agostino.test(axp4)$p.value>.05)
        {writeLines('accept=> H0: skw=0')}
    else{writeLines('reject=> Ha: skw!=0')}
    writeLines(paste('skewness=',skewness(axp4)))

        if(  anscombe.test(axp4)$p.value>.05)
        {writeLines('accept=> H0: kurt=3')}
    else{writeLines('reject=> Ha: kurt!=3')}
    writeLines(paste('kurtosis=',kurtosis(axp4)))


}
#P1.4
# >p4()
## accept=> H0: skw=0
## skewness= 0.0110578105333932
## reject=> Ha: kurt!=3
## kurtosis= 10.9042985653787


## had to use adj closing bc look what happened to axp
## Date,Open,High,Low,Close,Volume,Adj Close
## 2000-05-11,47.75,49.50,47.50,49.50,3806000,36.14
## 2000-05-10,148.50,150.38,142.63,143.38,10161200,34.90
