rm(list=ls())

library("geosphere")

Addre124 <- read.csv("Address124.csv")
# delete empty rows
Addre <- Addre124[!(Addre124$Response_X == ''),] 
# delete columns
Addre <- Addre[,-(2:3)]
# clean up x,y
Addre$Response_X <- as.numeric(sub(";.*", "", Addre$Response_X))
Addre$Response_Y <- as.numeric(sub(";.*", "", Addre$Response_Y))
Addre$Id <- as.numeric(as.character(Addre$Id))

# only TPE city
MetroAddr <- read.csv("tpeMetro.csv")
Metro <- MetroAddr[grep("^台",MetroAddr$address),]

# calculate distance to the nearest MRT station
Addre$distMRT = NA
for (i in 1:nrow(Addre)){
  x <- c(Addre$Response_X[i],Addre$Response_Y[i])
  for (j in 1:nrow(Metro)){
    y <- c(Metro$lon[j],Metro$lat[j])
    if (j==1){dist = distHaversine(x,y)}
    dist <- ifelse(distHaversine(x,y)<dist, distHaversine(x,y), dist)
  }
  Addre$distMRT[i] <- dist
}

# Merge to AllData
AllData <- read.csv("SinyiClean.csv")
colnames(AllData)[1] <- "Id"
FINAL <- merge(AllData,Addre,by="Id")

# Stats
target = FINAL[FINAL$distMRT<4000,]
target = target[target$MetPr<200,]
hist(target$distMRT, breaks = 50)
plot(target$distMRT,target$MetPr,pch=20,cex=0.5)
target$dist15 <- ifelse(target$distMRT<1500,TRUE,FALSE)
boxplot(MetPr~dist15,target)
t.test(MetPr~dist15,target, paired=FALSE)