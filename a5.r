#For data from Jan 1, 2008, and Dec 31, 2013, compute the correlations between each pair of the following:
#ˆ DJI,ˆ GSPC,ˆ IXIC, and VIX.
#Now do the same thing for the log returns of the series.
#Comment on your results.

source('financetools.r')

idxns=c("^GSPC","^IXIC","VIX")
for(i in idxns){
    idx=get.stock.price(i, start.date=c(1,1,2008)
                          , stop.date=c(12,31,2013))
    if(idxns[1]==i){ idxs=data.frame(idx) ;colnames(idxs)[1]=i  }
    else{idxs[i]=idx}
}

lr=function(idx){log(idx[-1]/idx[-length(idx)])}
lidxs=apply(idxs,2,lr)
