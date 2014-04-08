## 4.1. Consider the daily simple returns of Johnson & Johnson stock from January
## 1998 to December 2008. The data are in the ﬁle d-jnj9808.txt or can be
## obtained from CRSP. Convert the returns into log returns in percentage. (a)
## Build a GJR model for the log return series. Write down the ﬁtted model. Is
## the leverage effect signiﬁcant at the 1% level? (b) Build a general threshold
## volatility model for the log return series. (c) Compare the two TGARCH
## models.

source('financetools.r')
if(FALSE==exists('idx')){
    idx=get.stock.price('JNJ', start.date=c(1,1,1998)
                             , stop.date=c(12,31,2013))
}
lr=log(idx[-1]/idx[-length(idx)])
#d-jnj9808.txt
## date             rtn
## 19980102      -0.012334
## 19980105      -0.000961
## 19980106      -0.015385
## 19980107       0.009766
## 19980108       0.011605
## 19980109      -0.011472
## 19980112       0.020309
## 19980113       0.006635
## > head(lr)
## [1] -0.0008956561 -0.0158052511  0.0099638505  0.0112033437 -0.0112033437
## [6]  0.0200765164
lr=100*lr

## > auto.arima(lr)
## Series: lr
## ARIMA(2,0,2) with non-zero mean

## Coefficients:
##          ar1     ar2      ma1      ma2  intercept
##       0.5428  0.0880  -0.5332  -0.1598     0.0469
## s.e.  0.1714  0.1585   0.1703   0.1605     0.0120

## sigma^2 estimated as 2.313:  log likelihood=-20408.24
## AIC=40828.48   AICc=40828.49   BIC=40872.37
## >
library('fGarch')

#GJR=GARCH delta=2, and 0<gamma<1
gf=garchFit(formula=~arma(2,2)+aparch(1,1),data=lr
    ,delta=2,include.delta=FALSE,trace=FALSE)#,gamma=1)#,cond.dist="sstd")

## > gf

## Title:
##  GARCH Modelling

## Call:
##  garchFit(formula = ~arma(2, 2) + aparch(1, 1), data = lr, delta = 2,
##     include.delta = FALSE, trace = FALSE)

## Mean and Variance Equation:
##  data ~ arma(2, 2) + aparch(1, 1)
## <environment: 0x000000000d78a650>
##  [data = lr]

## Conditional Distribution:
##  norm

## Coefficient(s):
##        mu        ar1        ar2        ma1        ma2      omega     alpha1
##  0.015386   0.664291  -0.003623  -0.642413  -0.053699   0.015671   0.070017
##    gamma1      beta1
##  0.191651   0.924320

## Std. Errors:
##  based on Hessian

## Error Analysis:
##         Estimate  Std. Error  t value Pr(>|t|)
## mu      0.015386    0.005192    2.963  0.00304 **
## ar1     0.664291    0.152881    4.345 1.39e-05 ***
## ar2    -0.003623    0.147359   -0.025  0.98039
## ma1    -0.642413    0.152548   -4.211 2.54e-05 ***
## ma2    -0.053699    0.147656   -0.364  0.71610
## omega   0.015671    0.002663    5.886 3.96e-09 ***
## alpha1  0.070017    0.005207   13.446  < 2e-16 ***
## gamma1  0.191651    0.024864    7.708 1.29e-14 ***
## beta1   0.924320    0.005261  175.692  < 2e-16 ***

#leragae sig at 1% level

gf=garchFit(formula=~arma(2,2)+aparch(1,1),data=lr
    ,delta=2,include.delta=TRUE,trace=FALSE)#,gamma=1)#,cond.dist="sstd
## > gf

## Title:
##  GARCH Modelling

## Call:
##  garchFit(formula = ~arma(2, 2) + aparch(1, 1), data = lr, delta = 2,
##     include.delta = TRUE, trace = FALSE)

## Mean and Variance Equation:
##  data ~ arma(2, 2) + aparch(1, 1)
## <environment: 0x000000000d8844d0>
##  [data = lr]

## Conditional Distribution:
##  norm

## Coefficient(s):
##        mu        ar1        ar2        ma1        ma2      omega     alpha1
##  0.015850   0.663260  -0.020499  -0.641493  -0.037727   0.016657   0.076940
##    gamma1      beta1      delta
##  0.242704   0.927038   1.496718

## Std. Errors:
##  based on Hessian

## Error Analysis:
##         Estimate  Std. Error  t value Pr(>|t|)
## mu      0.015850    0.005327    2.976  0.00292 **
## ar1     0.663260    0.143597    4.619 3.86e-06 ***
## ar2    -0.020499    0.133009   -0.154  0.87752
## ma1    -0.641493    0.143631   -4.466 7.96e-06 ***
## ma2    -0.037727    0.133545   -0.283  0.77756
## omega   0.016657    0.002709    6.149 7.80e-10 ***
## alpha1  0.076940    0.005303   14.509  < 2e-16 ***
## gamma1  0.242704    0.032003    7.584 3.35e-14 ***
## beta1   0.927038    0.005027  184.426  < 2e-16 ***
## delta   1.496718    0.085836   17.437  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Log Likelihood:
##  -19161.24    normalized:  -1.725927

## Description:
##  Mon Apr 07 21:28:09 2014 by user: Majid

>
