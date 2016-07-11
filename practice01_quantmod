# PART I
rm(list=ls())
library("quantmod")

getSymbols("MSFT",src="yahoo")
barChart(MSFT)
chartSeries(MSFT["2014-05::2016-05"],theme="black")

ma_20<-runMean(MSFT[,4],n=20)
addTA(ma_20,on=1,col="blue")
ma_60<-runMean(MSFT[,4],n=60)
addTA(ma_60,on=1,col="red")

p<-Lag(ifelse(ma_20<ma_60, 1,0))
return<-ROC(MSFT[,4])*p
return<-return['2007-05-31/2014-05-31']
return<-exp(cumsum(return))
plot(return)


# PART II
rm(list=ls())
library("quantmod")
getSymbols("MSFT",src="yahoo")
chartSeries(MSFT["2007-05::2014-05"],theme="black")

ma_20<-runMean(MSFT[,4],n=20)
addTA(ma_20,on=1,col="blue")
ma_60<-runMean(MSFT[,4],n=60)
addTA(ma_60,on=1,col="red")

allDateNumber = length(ma_20)
return = as.vector( c(1:allDateNumber) )
p = as.vector( c(1:allDateNumber) )
for (dateidx in 1:allDateNumber)
{
  if( dateidx < 60 )
  {
    return[dateidx] = 0
  }
  else
  {
    prePrice = as.numeric(MSFT[dateidx-1, 4])
    nowPrice = as.numeric(MSFT[dateidx, 4])
    return[dateidx] = (prePrice - nowPrice) / prePrice
    if( ma_20[dateidx, 1] < ma_60[dateidx, 1] )
    {
      p[dateidx] = -1
    }
    else if( ma_20[dateidx, 1] > ma_60[dateidx, 1] )
    {
      p[dateidx] = 1
    }
    else
    {
      p[dateidx] = 0
    }
    return[dateidx] = return[dateidx] * p[dateidx]
  }
}

portfolio = cumsum(return)
plot(portfolio)
