library(vars)
library(urca)
url <- "http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/m-gs1n3-5304.txt"
if (exists('x')==FALSE){x <- read.table(url)}
y <- data.frame(x$V1,x$V2)#cbind(x$V1,x$V2)
colnames(y)=c("yr1","yr3")
ps <- VARselect(y)
### use BIC (also called SC)
#turns out BIC has lowest order
p <- as.numeric(ps$selection[3])
fit <- VAR(y, p=p)

#impulse response
ir=irf(fit,n.ahead=6)
## > ir

## Impulse response coefficients
## $X1yr
##           X1yr      X3yr
## [1,] 0.4070973 0.3174225
## [2,] 0.5857936 0.4533965
## [3,] 0.5640063 0.4311556
## [4,] 0.5044067 0.3877779
## [5,] 0.4779343 0.3757229
## [6,] 0.4761506 0.3822046
## [7,] 0.4768558 0.3882583

## $X3yr
##            X1yr      X3yr
## [1,] 0.00000000 0.1262354
## [2,] 0.04509795 0.1773616
## [3,] 0.05173065 0.1615947
## [4,] 0.03484460 0.1356486
## [5,] 0.02497717 0.1229463
## [6,] 0.02521160 0.1189881
## [7,] 0.02743177 0.1156653


## Lower Band, CI= 0.95
## $X1yr
##           X1yr      X3yr
## [1,] 0.3524456 0.2727459
## [2,] 0.5039685 0.3855259
## [3,] 0.4755000 0.3604142
## [4,] 0.4091028 0.3073464
## [5,] 0.3752596 0.2959579
## [6,] 0.3677373 0.2995592
## [7,] 0.3619747 0.2967925

## $X3yr
##              X1yr       X3yr
## [1,]  0.000000000 0.11607065
## [2,]  0.009453597 0.14457455
## [3,]  0.002776498 0.11200205
## [4,] -0.023777215 0.08085778
## [5,] -0.030383524 0.06810881
## [6,] -0.033100698 0.06316071
## [7,] -0.036137810 0.05404639


## Upper Band, CI= 0.95
## $X1yr
##           X1yr      X3yr
## [1,] 0.4491376 0.3520448
## [2,] 0.6483498 0.4976399
## [3,] 0.6294712 0.4773100
## [4,] 0.5749873 0.4469200
## [5,] 0.5390529 0.4356226
## [6,] 0.5237704 0.4353500
## [7,] 0.5308759 0.4382000

## $X3yr
##            X1yr      X3yr
## [1,] 0.00000000 0.1356816
## [2,] 0.08148430 0.2038171
## [3,] 0.11220476 0.2132068
## [4,] 0.10486435 0.1945833
## [5,] 0.08738291 0.1763178
## [6,] 0.08612937 0.1680385
## [7,] 0.08677296 0.1680766

## >

fcast=predict(fit,n.ahead=12)

cotst=ca.jo(y,K=p)
## \> summary(cotst)

# Johansen-Procedure #
######################

## Test type: maximal eigenvalue statistic (lambda max) , with linear trend

## Eigenvalues (lambda):
## [1] 0.043535810 0.006062325

## Values of teststatistic and critical values of test:

##           test 10pct  5pct  1pct
## r <= 1 |  3.70  6.50  8.18 11.65
## r = 0  | 27.11 12.91 14.90 19.19

## Eigenvectors, normalised to first column:
## (These are the cointegration relations)

##           yr1.l3    yr3.l3
## yr1.l3  1.000000  1.000000
## yr3.l3 -1.008773 -2.331354

## Weights W:
## (This is the loading matrix)

##            yr1.l3      yr3.l3
## yr1.d -0.02757524 0.008403043
## yr3.d  0.03131554 0.006973878

#test stat shows r!=0. does it equal 1? yes.


library('tsDyn')
vecm=lineVar(y,p,r=1,include='const',model='VECM')#beta is est.
#> summary(vecm)
#############
###Model VECM
#############
## Full sample size: 612 	End sample size: 608
## Number of variables: 2 	Number of estimated slope parameters 16
## AIC -3598.319 	BIC -3523.346 	SSR 169.9209
## Cointegrating vector (estimated by OLS):
##    yr1        yr3
## r1   1 -0.9512159


##              ECT                 Intercept
## Equation yr1 -0.0432(0.0309)     -0.0065(0.0169)
## Equation yr3 0.0148(0.0259)      0.0004(0.0141)
##              yr1 -1              yr3 -1
## Equation yr1 0.1823(0.1088).     0.3594(0.1293)**
## Equation yr3 -0.0311(0.0911)     0.4783(0.1083)***
##              yr1 -2              yr3 -2
## Equation yr1 -0.0906(0.1098)     -0.2010(0.1306)
## Equation yr3 0.0307(0.0919)      -0.3144(0.1094)**
##              yr1 -3              yr3 -3
## Equation yr1 -0.0899(0.1081)     0.1521(0.1301)
## Equation yr3 -0.1942(0.0905)*    0.2683(0.1089)*

vecmfcst=predict(vecm,n.ahead=12)
