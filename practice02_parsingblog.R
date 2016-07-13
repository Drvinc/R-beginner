rm(list=ls())
library(XML)
library(bitops)
library(RCurl)
library(httr)

orgURL <- 'http://skygene.blogspot.tw/'

startPage <- as.numeric(as.Date('2013/12/01',format='%Y/%m/%d'))
endPage <- as.numeric(as.Date(cut(Sys.Date(),"month")))
alldata <- data.frame()
for(i in startPage:endPage)
{
  if(as.Date(cut(as.Date(i,origin="1970-01-01"),"month"))>as.Date(cut(as.Date(i-1,origin="1970-01-01"),"month")))
  {
    blogURL <- paste(orgURL, gsub("-","_",as.character(as.Date(i,origin="1970-01-01"))),'_archive.html',sep='')
    urlExists <- url.exists(blogURL)
    if(urlExists)
    {
      html <- getURL(blogURL, ssl.verifypeer = FALSE)
      xml <- htmlParse(html, encoding ='utf-8')
      title <- xpathSApply(xml, "//h3[@class='post-title entry-title']/a//text()", xmlValue)
      author <- xpathSApply(xml, "//span[@class='fn']", xmlValue)
      path <- xpathSApply(xml, "//h3[@class='post-title entry-title']/a//@href")
      date.month <- format(as.Date(i,origin="1970-01-01"),format="%Y-%m")
      
      html <- getURL(path, ssl.verifypeer = FALSE)
      xml <- htmlParse(html, encoding ='utf-8')
      date <- xpathSApply(xml, "//h2[@class='date-header']/span", xmlValue)
      #response <- xpathSApply(xml, "//span[@id='u_0_2']", xmlValue)
      tempdata <- data.frame(title, author, path, date, date.month)
      
      alldata <- rbind(alldata, tempdata)
    }
  }
}

allDate <- levels(alldata$date.month)

res = hist(as.numeric(alldata$date.month), nclass=length(allDate), axes=F, labels=T) 
axis(1, at=1:length(allDate), labels=allDate)
axis(2, at=1:max(res$counts), labels=1:max(res$counts))
