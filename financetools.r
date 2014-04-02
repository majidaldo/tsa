# R programs to use for Math Finance class
# Spring 2005 at American University
# John Nolan   jpnolan@american.edu
# modified JEG Fall 2011
#
#   For S&P 500, use "^GSPC"
##########################################################################################
get.stock.price <- function(symbol, start.date=c(1,1,2007), stop.date=c(12,31,2007),
#                           full.table=FALSE, print.info=TRUE) {
                            freq="d",full.table=FALSE, print.info=TRUE) {
# gets stock price data from yahoo.com for specifed symbol
# default is to return ADJUSTED closing price from 1/1/2007 to 12/31/2007

if( print.info ) cat(symbol," ")
url <- paste("http://ichart.finance.yahoo.com/table.csv?a=",
          start.date[1]-1,"&b=",start.date[2],"&c=",start.date[3],
          "&d=",stop.date[1]-1,"&e=",stop.date[2],"&f=",stop.date[3],
#         "&s=",symbol,sep="")
         "&g=",freq,"&s=",symbol,sep="")
                   # for some reason, need to subtract 1 from start and stop month???
x <- read.csv(url) # in S-Plus, use read.table(url,sep=","), but note that it treats
                   # the dates as row labels, not a separate column

# data has most recent days first, going back to start date
n <- length(x$Date); date <- as.character(x$Date[c(1,n)])
if (print.info) cat("has", n,"values from",date[2],"to",date[1],"\n")
if (full.table) { # return full table
 x$Date <- rev(x$Date)
 x$Open <- rev(x$Open)
 x$High <- rev(x$High)
 x$Low  <- rev(x$Low)
 x$Close <- rev(x$Close)
 x$Volume <- rev(x$Volume)
 x$Adj.Close <- rev(x$Adj.Close)
 return(x)
} else return(rev(x$Adj.Close)) # return just the closing prices
}
##########################################################################################
get.portfolio.returns = function( symbols, start.date=c(1,1,2007),
        stop.date = c(12,31,2007) ){
# get a table (data.frame) of values for a list of stocks in the stated time period
n = length(symbols)
for (i in 1:n) {
  t1 = get.stock.price( symbols[i], start.date=start.date, stop.date=stop.date,full.table=T)
  #  need to merge columns, possibly with mismatching dates
  a = data.frame(t1$Date,t1$Adj.Close)
  names(a) = c("Date",symbols[i])
  if (i == 1) {b=a}
  else {b = merge(b,a,sort=FALSE)}
  }
#  leave off the date column
nn = dim(b)[1]
cat("    ",nn,"dates with values for all stocks,",nn-1,"returns calculated\n")
b = b[,2:ncol(b)]
bb = data.frame(apply(b,2,"log.ratio"))
names(bb) = symbols
return(bb)
}
##########################################################################################
log.ratio <- function(x) { return(diff(log(x))) }
##########################################################################################
trimmed.hist <- function(x,low=-Inf,high=+Inf,nclass=20,...) {

y <- x[ (x > low) & (x < high) ]
hist(y,nclass=nclass,...)
cat("original data had",length(x),"values, trimmed data has",length(y),"values\n")
invisible(y)
}
##########################################################################################


