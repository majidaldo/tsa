## Consider the monthly log stock returns, in percentages and including divi-
## dends, of Merck & Company, Johnson & Johnson, General Electric, General
## Motors, Ford Motor Company, and value-weighted index from January 1960
## to December 2008;

## (a) Compute the sample mean, covariance matrix, and correlation matrix of
##the data.

r=read.table('m-mrk2vw.txt',header=TRUE)[-1] #throw away date

## > colMeans(r)
##   MRK          JNJ           GE           GM            F
##  9.665028e-01 1.169972e+00 7.209367e-01 1.243458e-01 4.489213e-01
##           VW
## 7.245890e-01

## > cov(r)
##          MRK       JNJ       GE        GM        F       VW
## MRK 49.57914 23.867654 17.92527 10.228013 12.69143 14.29833
## JNJ 23.86765 39.590745 17.42600  8.818861 11.85487 14.24747
## GE  17.92527 17.426005 41.37391 23.551507 25.22338 20.48991
## GM  10.22801  8.818861 23.55151 72.819340 54.19326 20.07358
## F   12.69143 11.854873 25.22338 54.193257 85.29819 21.85352
## VW  14.29833 14.247467 20.48991 20.073579 21.85352 19.93892
## >


## > cor(r)
##           MRK       JNJ        GE        GM         F        VW
## MRK 1.0000000 0.5387204 0.3957792 0.1702231 0.1951601 0.4547629
## JNJ 0.5387204 1.0000000 0.4305640 0.1642451 0.2040000 0.5070957
## GE  0.3957792 0.4305640 1.0000000 0.4290739 0.4245903 0.7133883
## GM  0.1702231 0.1642451 0.4290739 1.0000000 0.6876255 0.5268058
## F   0.1951601 0.2040000 0.4245903 0.6876255 1.0000000 0.5299083
## VW  0.4547629 0.5070957 0.7133883 0.5268058 0.5299083 1.0000000
## >

library('portes')

##> Hosking(as.matrix(r),lags=c(1,2,3,4,5,6),order=0) #white noise test order=0
##  Lags Statistic  df      p-value
##     1  87.41509  36 3.640487e-06
##     2 130.61678  72 2.921285e-05
##     3 181.35580 108 1.267206e-05
##     4 223.64278 144 2.363755e-05
##     5 249.34864 180 4.777563e-04
##     6 285.07134 216 1.127163e-03
## >
#null is rejected at 5% significance at all lags
