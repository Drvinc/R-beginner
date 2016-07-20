rm(list = ls())

allData <- read.csv("SinyiClean.csv")
AreaList <- c("松山區","信義區","大安區","中山區","中正區","大同區","萬華區","文山區","南港區","內湖區","士林區","北投區")
AreaIndex <- data.frame(Index=1:12, Area=AreaList)

# calculate mean MetPr
All12 = allData[allData$TotPr<12000,] # cut extreme values
meanPrice <- data.frame("Area"=c(1:12),"MeanMetPr"=0)
for (i in 1:nrow(meanPrice)){
  target <- All12[(All12$Area == i),]
  meanPrice$MeanMetPr[i] <- mean(target$MetPr,na.rm=TRUE)
}

# Correlation: MeanMetPr to avgConsume
aIncome <- read.csv("aIncome.csv")
FINAL <- merge(aIncome, meanPrice, by="Area")

col_title <- c("行政區","年","所得收入總計","非消費支出","消費支出","可支配所得","最終消費支出","儲蓄","所得總額","平均每坪價格")


# OVERALL plot complicated
  # Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
  # Customize upper panel
my_cols <- c(1:12)
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[FINAL$Area])
}
  # Create the plots
pairs(FINAL[,c(10,3:9)], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

# OVERALL plot simple
pairs(FINAL[,c(10,3:9)],pch=20, cex=2, col = my_cols[FINAL$Area], lower.panel = NULL)


# Another OVERALL plot
library(psych)
pairs.panels(FINAL[,c(10,3:9)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


# 3 highest correlated

  # plot MeanMetPr to DominIncome
x_lab=col_title[grep("DominIncome", colnames(FINAL))]
y_lab=col_title[grep("MeanMetPr", colnames(FINAL))]
DF <- data.frame(VAR1=FINAL$DominIncome, VAR2=FINAL$MeanMetPr)
with(DF, plot(VAR1, VAR2, xlab=x_lab, ylab=y_lab))
abline(fit <- lm(VAR2 ~ VAR1, data=DF), col='red')
legend("topleft", bty="n", legend=paste("r square = ", format(summary(fit)$adj.r.squared, digits=4)))

  # plot MeanMetPr to Saving
x_lab=col_title[grep("Saving", colnames(FINAL))]
y_lab=col_title[grep("MeanMetPr", colnames(FINAL))]
DF <- data.frame(VAR1=FINAL$Saving, VAR2=FINAL$MeanMetPr)
with(DF, plot(VAR1, VAR2, xlab=x_lab, ylab=y_lab))
abline(fit <- lm(VAR2 ~ VAR1, data=DF), col='red')
legend("topleft", bty="n", legend=paste("r square = ", format(summary(fit)$adj.r.squared, digits=4)))

  # plot MeanMetPr to endIncome
x_lab=col_title[grep("EndIncome", colnames(FINAL))]
y_lab=col_title[grep("MeanMetPr", colnames(FINAL))]
DF <- data.frame(VAR1=FINAL$EndIncome, VAR2=FINAL$MeanMetPr)
with(DF, plot(VAR1, VAR2, xlab=x_lab, ylab=y_lab))
abline(fit <- lm(VAR2 ~ VAR1, data=DF), col='red')
legend("topleft", bty="n", legend=paste("r square = ", format(summary(fit)$adj.r.squared, digits=4)))
