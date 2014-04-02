#library(RQuantLib)
#Tminust <- as.numeric(difftime("2014-02-25","2014-02-24"))/365.24
## AmericanOptionImpliedVolatility(type=’call’, value=4.5,
## underlying=183.69, strike=185, dividendYield=0,
## riskFreeRate=0.0003, maturity=Tminust, volatility=0.14,
## timeSteps=150, gridPoints=151)

opts=read.csv('vixopts.csv')
