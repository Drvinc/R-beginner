rm(list=ls())

allData <- read.csv("SinyiClean.csv")

# delete extreme values
target = allData[allData$TotPr<12000,]
target = target[target$MetPr<250,]
target = target[target$Age<80,]

# TotPr to MetPr scatterplot
plot(target$MetPr, target$TotPr, pch=20, cex=0.5, xlab="每坪價格(萬)", ylab="總價(萬)")
# MetPr to Age
plot(target$Age, target$MetPr, pch=20, cex=0.5)
# MetPr to Floor
boxplot(MetPr~Floor, data=target)
# MetPr to TotFl
boxplot(MetPr~TotFloor, data=target)
# MetPr to Car
boxplot(MetPr~Car, data=target)
# MetPr to Area
boxplot(MetPr~Area, data=target)
# distribution by Age
hist(target$Age, breaks=80)
# distribution by Floor
hist(target$Floor)
# MetPr to isTaoFang
boxplot(MetPr~isTaoFang, data=target)

# Simple Bar Plot 
counts <- table(allData$Area)
barplot(counts, main="行政區交易量", xlab="行政區")

# Grouped Bar Plot
counts <- table(allData$Area, allData$YyySe)
barplot(counts, main="逐季交易量",
        xlab="季", legend = rownames(counts), beside=TRUE)
