rm(list = ls())

allData <- read.csv("SinyiClean.csv")
AreaList <- c("松山區","信義區","大安區","中山區","中正區","大同區","萬華區","文山區","南港區","內湖區","士林區","北投區")
AreaIndex <- data.frame(Index=1:12, Area=AreaList)

# calculate mean MetPr
All12 = allData[allData$TotPr<12000,] # cut extreme values
All12 = All12[All12$MetPr<250,] # cut extreme values
meanPrice <- data.frame("Area"=c(1:12),"MeanMetPr"=0)
for (i in 1:nrow(meanPrice)){
  target <- All12[(All12$Area == i),]
  meanPrice$MeanMetPr[i] <- mean(target$MetPr,na.rm=TRUE)
}

# Correlation: MeanMetPr to avgConsume
aIncome <- read.csv("aIncome.csv")
FINAL <- merge(aIncome, meanPrice, by="Area")

#write.csv(FINAL,"SinyiDist.csv")

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

# correlation by ggplot2
library(ggplot2)
library(ggrepel)
library(grid)

DF <- data.frame(VAR1=FINAL$Saving/10000, VAR2=FINAL$MeanMetPr, Area=FINAL$Area)
rownames(DF) <- AreaList[DF$Area]
reg <- lm(VAR2~VAR1,data=DF)
coeff <- coefficients(reg)
eq = paste0("y = ", round(coeff[2],2), "*x ", ifelse(coeff[1]<0, "- ", "+ "), abs(round(coeff[1],2)))
r_sq = paste0("R^2 = ", round(summary(reg)$adj.r.squared, 2))
p <- ggplot(DF,aes(VAR1, VAR2, label = rownames(DF)))
p <- p + geom_point() + geom_smooth(method=lm, fill='lightblue', color='darkblue', size=1)
p <- p + geom_label_repel(size=3,aes(label=rownames(DF)))
p <- p + labs(x = "儲蓄(萬)", y = "平均每坪價格(萬)", title = "房價對儲蓄") 
annot <- grobTree(textGrob(eq, x=0.05, y=0.9, hjust=0, gp=gpar(col="black", fontsize=13, fontface="bold")),
                  textGrob(r_sq, x=0.05, y=0.85, hjust=0 , gp=gpar(col="black", fontsize=13, fontface="bold")))
p <- p+ annotation_custom(annot)
p # MrtPr to Saving

DF <- data.frame(VAR1=FINAL$DominIncome/10000, VAR2=FINAL$MeanMetPr, Area=FINAL$Area)
rownames(DF) <- AreaList[DF$Area]
reg <- lm(VAR2~VAR1,data=DF)
coeff <- coefficients(reg)
eq = paste0("y = ", round(coeff[2],2), "*x ", ifelse(coeff[1]<0, "- ", "+ "), abs(round(coeff[1],2)))
r_sq = paste0("R^2 = ", round(summary(reg)$adj.r.squared, 2))
p <- ggplot(DF,aes(VAR1, VAR2, label = rownames(DF)))
p <- p + geom_point() + geom_smooth(method=lm, fill='lightblue', color='darkblue', size=1)
p <- p + geom_label_repel(size=3,aes(label=rownames(DF)))
p <- p + labs(x = "可支配所得(萬)", y = "平均每坪價格(萬)", title = "房價對可支配所得") 
annot <- grobTree(textGrob(eq, x=0.05, y=0.9, hjust=0, gp=gpar(col="black", fontsize=13, fontface="bold")),
                  textGrob(r_sq, x=0.05, y=0.85, hjust=0 , gp=gpar(col="black", fontsize=13, fontface="bold")))
p <- p+ annotation_custom(annot)
p # MrtPr to DominIncome

DF <- data.frame(VAR1=FINAL$EndIncome/10000, VAR2=FINAL$MeanMetPr, Area=FINAL$Area)
rownames(DF) <- AreaList[DF$Area]
reg <- lm(VAR2~VAR1,data=DF)
coeff <- coefficients(reg)
eq = paste0("y = ", round(coeff[2],2), "*x ", ifelse(coeff[1]<0, "- ", "+ "), abs(round(coeff[1],2)))
r_sq = paste0("R^2 = ", round(summary(reg)$adj.r.squared, 2))
p <- ggplot(DF,aes(VAR1, VAR2, label = rownames(DF)))
p <- p + geom_point() + geom_smooth(method=lm, fill='lightblue', color='darkblue', size=1)
p <- p + geom_label_repel(size=3,aes(label=rownames(DF)))
p <- p + labs(x = "所得總額(萬)", y = "平均每坪價格(萬)", title = "房價對所得總額") 
annot <- grobTree(textGrob(eq, x=0.05, y=0.9, hjust=0, gp=gpar(col="black", fontsize=13, fontface="bold")),
                  textGrob(r_sq, x=0.05, y=0.85, hjust=0 , gp=gpar(col="black", fontsize=13, fontface="bold")))
p <- p+ annotation_custom(annot)
p # MrtPr to EndIncome
