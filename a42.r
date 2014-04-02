library(RQuantLib)

ulp=14.4;today="2014-02-25";

opts=read.csv('vixopts.csv')

iv=function(type
    ,strike
    ,month,day="14" #expiration. num month
    ){

Tminust =as.numeric(difftime(
    paste("2014",month,day,sep='-'),today))/365.24

if(type=='c'){cpn="Calls";cpt="";  type='call'}
else         {cpn="Puts" ;cpt=".1";type='put'}
bid=paste("Bid",cpt,sep="")
ask=paste("Ask",cpt,sep="")

row=(opts[opts$Month==month.abb[match(month,c(1:12))]
          & opts$Day==day
          & opts[[cpn]]==strike,])
if(nrow(row)!=1){return()}

v=EuropeanOptionImpliedVolatility(
    type=type, value=(row[[bid]]+row[[ask]])/2
    ,underlying=ulp, strike=strike, dividendYield=0,
    riskFreeRate=0.0003, maturity=Tminust, volatility=0.123456
    #,timeSteps=150, gridPoints=151
    )

return(v$impliedVol)

}

strikenotinmoney=function(type,month,n=5,day=14){

if(type=='c'){strikes=opts$Calls>ulp;}
else         {strikes=opts$Puts<ulp;}


o=(#overkill b/c the strike prices are the same for each month
    opts[opts$Month==month.abb[match(month,c(1:12))]
         & opts$Day==day
         & strikes
      ,       ]$Calls
    )
if(type=='p'){o=sort(o,decreasing=TRUE)}
o=o[c(1:n)];o=sort(o)

return(o)
}

#ivdfg=function(months=c(3:8)){
#months=month.abb[months]

#for(am in months){
#psapply(str
plotv=function(type){#c or p
strike=strikenotinmoney(type,3)
volati=sapply(strike,function(x) iv(type,x,3))
plot(strike,volati)

}

plotv3d=function(){
mos=c(3:8)
ivs=matrix(nrow=10,ncol=8-3+1)
strikep=strikenotinmoney('p',3)
strikec=strikenotinmoney('c',3)
strike=c(strikep,strikec)
for(amo in mos){
    ivs[,amo-2]=c(
            sapply(strikep,function(x) iv('p',x,amo))
           ,sapply(strikec,function(x) iv('c',x,amo))
           )
}
#return(ivs)
persp(ivs,x=strike,y=mos#month.abb[mos]
      ,xlab="strike price",ylab="month",zlab="implied volatility"
      ,ticktype='detailed'
      ,phi=30,theta=-30)
return(c(strike,mos))
}
