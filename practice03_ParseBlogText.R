rm(list=ls(all=TRUE))
library(XML)
library(bitops)
library(RCurl)
library(NLP)
library(httr)

Sys.setlocale("LC_ALL", "cht")

alldata = read.csv('alldata.csv')
for( i in 1:length(alldata$X))
{
  blogURL <- as.character(alldata$path[i])
  urlExists = url.exists(blogURL)
  
  if(urlExists)
  {
    html = getURL(blogURL, ssl.verifypeer = FALSE, encoding='UTF-8')
    xml = htmlParse(html, encoding='UTF-8')
    text = as.character(xpathSApply(xml, "//*[@id='Blog1']/div[1]/div/div/div/div[1]/div[2]/p", xmlValue))
    name <- paste('./allText/c', i, '.txt', sep='')
    write(text, name)
  }
}
