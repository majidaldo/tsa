## Consider the monthly simple returns of General Electric (GE) stock from
## January 1926 to December 2008 with 996 observations. You may download
## the data from CRSP or use the ﬁle m-ge2608.txt on the Web. Convert
## the returns into log returns in percentages. Build a TGARCH model with
## GED innovations for the series using at − 1 as the threshold variable with zero
## threshold, where at − 1 is the shock at time t − 1. Write down the ﬁtted model.
## Is the leverage effect signiﬁcant at the 5% level?

source('financetools.r')
if(FALSE==exists('idx')){
    idx=get.stock.price('GE', start.date=c(1,1,1962)
                             , stop.date=c(12,31,2013)
        ,freq='m')
}
lr=log(idx[-1]/idx[-length(idx)])
lr=100*lr

## > auto.arima(lr)
## Series: lr
## ARIMA(2,0,1) with non-zero mean

## Coefficients:
##          ar1     ar2      ma1  intercept
##       0.6054  0.0295  -0.7065     0.0394
## s.e.  0.0781  0.0144   0.0775     0.0131

## sigma^2 estimated as 3.486:  log likelihood=-26745.26
## AIC=53500.52   AICc=53500.52   BIC=53537.92
## >
library('fGarch')
gf=garchFit(formula=~arma(2,1)+aparch(1,1),data=lr
    ,delta=1,include.delta=TRUE,cond.dist='sged',trace=FALSE)


## Call:
##  garchFit(formula = ~arma(2, 1) + aparch(1, 1), data = lr, delta = 1,
##     cond.dist = "sged", include.delta = TRUE, trace = FALSE)

## Mean and Variance Equation:
##  data ~ arma(2, 1) + aparch(1, 1)
## <environment: 0x000000001b98d280>
##  [data = lr]

## Conditional Distribution:
##  sged

## Coefficient(s):
##        mu        ar1        ar2        ma1      omega     alpha1     gamma1
##  1.578588  -0.893281  -0.019391   0.864405   0.993651   0.114073   0.413889
##     beta1      delta       skew      shape
##  0.820451   1.303581   1.038097   1.628980

## Std. Errors:
##  based on Hessian

## Error Analysis:
##         Estimate  Std. Error  t value Pr(>|t|)
## mu       1.57859     0.48845    3.232 0.001230 **
## ar1     -0.89328     0.09677   -9.231  < 2e-16 ***
## ar2     -0.01939     0.04407   -0.440 0.659935
## ma1      0.86440     0.08608   10.041  < 2e-16 ***
## omega    0.99365     0.42467    2.340 0.019292 *
## alpha1   0.11407     0.03138    3.635 0.000278 ***
## gamma1   0.41389     0.21191    1.953 0.050803 .
## beta1    0.82045     0.04639   17.685  < 2e-16 ***
## delta    1.30358     0.58462    2.230 0.025761 *
## skew     1.03810     0.06261   16.582  < 2e-16 ***
## shape    1.62898     0.16679    9.767  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Log Likelihood:
##  -2045.996    normalized:  -3.284103
