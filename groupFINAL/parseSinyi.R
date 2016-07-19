library(XML)
library(bitops)
library(RCurl)
library(RJSONIO)
library(RSelenium)

rm(list=ls())

# RSelenium
checkForServer()
startServer()
remDr <- remoteDriver(browserName="chrome")
remDr$open(silent=TRUE)

orgURL <- "http://tradeinfo.sinyi.com.tw/itemList.html?a1="
# Sinyi URL format: http://tradeinfo.sinyi.com.tw/itemList.html?a1=106&s2=10408_10507&p=2

areaIndex <- c(100:116) #Sinyi's indeces for TPE areas
startMonth <- as.character(10301) #Search start YYYMM 
endMonth <- as.character(10507) #Search end YYYMM

allData <- data.frame()
for (i in 1:length(areaIndex))
{
  siteURL <- paste0(orgURL, areaIndex[i], '&s2=', startMonth, '_', endMonth)
  remDr$navigate(siteURL)
  #get result numbers
  elem <- remDr$findElement(using = "xpath", "//div[2]/div[1]/div[1]/span")
  elemtxt <- elem$getElementAttribute("outerHTML")[[1]] 
  elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)
  itemN <- xpathSApply(elemxml, "//span", xmlValue)
  pageN <- trunc(as.numeric(itemN)/15)+1 #get page numbers
  
  # retrieve and merge tables on result pages
  for (j in 1:pageN)
  {
    siteURL <- paste0(orgURL, areaIndex[i], '&s2=', startMonth, '_', endMonth, '&p=', j)
    remDr$navigate(siteURL)
    doc <- htmlParse(remDr$getPageSource()[[1]], encoding = "UTF-8")
    doc <- readHTMLTable(doc)
    tempData <- doc$newTable
    allData <- rbind(allData, tempData)
  }
}

names(allData)[1] <- "年月"
names(allData)[2] <- "地址"
names(allData)[3] <- "類型"
names(allData)[4] <- "總價"
names(allData)[5] <- "每坪單價"
names(allData)[6] <- "建坪"
names(allData)[7] <- "地坪"
names(allData)[8] <- "屋齡"
names(allData)[9] <- "樓層"
names(allData)[10] <- "格局"

#write.csv(allData, "Sinyi10301_10507.csv")
