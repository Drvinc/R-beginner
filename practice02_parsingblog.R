rm(list=ls())
library(XML)
library(bitops)
library(RCurl)
library(httr)

orgURL <- 'http://skygene.blogspot.tw/'

startPage <- as.numeric(as.Date('2005/12/01',format='%Y/%m/%d'))
endPage <- as.numeric(as.Date('2006/12/01',format='%Y/%m/%d'))
#endPage <- as.numeric(as.Date(cut(Sys.Date(),"month")))
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
      if(length(xpathSApply(xml, "//h3[@class='post-title entry-title']/a//text()", xmlValue))!=0)
      {
      title <- xpathSApply(xml, "//h3[@class='post-title entry-title']/a//text()", xmlValue)
      author <- xpathSApply(xml, "//span[@class='fn']", xmlValue)
      path <- xpathSApply(xml, "//h3[@class='post-title entry-title']/a//@href")
      date.month <- format(as.Date(i,origin="1970-01-01"),format="%Y-%m")
      
      html <- getURL(path, ssl.verifypeer = FALSE)
      xml <- htmlParse(html, encoding ='utf-8')
      date <- xpathSApply(xml, "//h2[@class='date-header']/span", xmlValue)
      #response <- xpathSApply(xml, "//span[@id='u_0_2']", xmlValue)
      response <- ifelse(length(xpathSApply(xml, "//span[@id='u_0_2']", xmlValue))==0,as.character("0"),xpathSApply(xml, "//span[@id='u_0_2']", xmlValue))
      tempdata <- data.frame(title, author, path, date, date.month, response)
      
      alldata <- rbind(alldata, tempdata)
      }
    }
  }
}

allDate <- levels(alldata$date.month)

res = hist(as.numeric(alldata$date.month), nclass=length(allDate), axes=F, labels=T) 
axis(1, at=1:length(allDate), labels=allDate)
axis(2, at=1:max(res$counts), labels=1:max(res$counts))
