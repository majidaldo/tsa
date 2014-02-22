library('xts')
ibm=read.csv('ibmw.csv',sep=',')
ibm=xts(ibm[['Adj.Close']],as.Date(ibm[['Date']]))

ibm2=diff(ibm)[-1]

p4=function(){

for(h in 1:5){

    writeLines(paste(
        h
        ,round(Box.test(ibm2,type="Ljung-Box",lag=h)$p.value,3)

               ))

}

}
