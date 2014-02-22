
ibmdata=read.csv("ibmd.csv")
closings=xts(ibmdata$Adj.Close ,as.Date(ibmdata$Date))
sr=(abs(diff(closings))[-1] #simple return
        /as.vector(closings[-length(closings)]))

#acf(sr,100)
