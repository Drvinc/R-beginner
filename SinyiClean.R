rm(list = ls())
rawraw <- read.csv("sinyi10301_10507.csv")

# rename columns to eng
names(rawraw) = c("Id","YrMn","Addre","Type","TotPr","MetPr","ConstMet","LandMet","Age","Floor","Layout")
#delete NA rows
SinyiClean <- rawraw[!(is.na(rawraw$Addre)),] 
# YrMn => YyyMm
SinyiClean$YrMn <- gsub("年", "", SinyiClean$YrMn)
SinyiClean$YrMn <- gsub("月", "", SinyiClean$YrMn)
names(SinyiClean)[names(SinyiClean)=="YrMn"] <- "YyyMm"
# Add column Year
SinyiClean$Year <- substr(SinyiClean$YyyMm, start=1, stop=3)
# Add column Mnth
Mnth <- substr(SinyiClean$YyyMm, start=4, stop=5)
SinyiClean$Month <- Mnth
# Add column Season and YyySe 
Season <- round(as.numeric(Mnth)/4)+1
SinyiClean$Season <- Season
SinyiClean$YyySe <- paste0(SinyiClean$Year, SinyiClean$Season)
# Clean up Addre
SinyiClean$Addre <- gsub("檢視位置", "", SinyiClean$Addre)
# Add column Car
SinyiClean$Car <- grepl("車位",SinyiClean$Type)
# Add column TotFloor
TotFloor <- sub(".*/", "", SinyiClean$Floor)
SinyiClean$TotFloor <- gsub("[^0-9]","",TotFloor)
# Clean up Floor
Floor <- sub(" /.*", "", SinyiClean$Floor)
SinyiClean$Floor <- Floor
# Clean up TotPr
totalPrice <- gsub("([0-9]+).*$","\\1",SinyiClean$TotPr)
SinyiClean$TotPr <- as.numeric(totalPrice)
# Clean up MetPr
metPrice <- unlist(regmatches(SinyiClean$MetPr,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",SinyiClean$MetPr)))
SinyiClean$MetPr <- as.numeric(metPrice)
# Clean up ConstMet
ConstMet <- sub("坪.*","",SinyiClean$ConstMet)
SinyiClean$ConstMet <- as.numeric(ConstMet)
# Clean up LandMet
LandMet <- unlist(regmatches(SinyiClean$LandMet,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",SinyiClean$LandMet)))
SinyiClean$LandMet <- as.numeric(LandMet)
# Clean up Age
Age <- gsub("--","0",SinyiClean$Age)
Age <- unlist(regmatches(Age,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",Age)))
SinyiClean$Age <- gsub("0",NA,Age)
# Add Fang_, Ting_, Wei_, Shi_
Layoutcut <- sub(" .*","",SinyiClean$Layout)
Layout <- strsplit(as.character(Layoutcut),"/")
SinyiClean$Fang_ <- as.numeric(gsub("[^0-9]","",(sapply(Layout, "[[", 1))))
SinyiClean$Ting_ <- as.numeric(gsub("[^0-9]","",(sapply(Layout, "[[", 2))))
SinyiClean$Wei_ <- as.numeric(gsub("[^0-9]","",(sapply(Layout, "[[", 3))))
SinyiClean$Shi_ <- c(0)
for (i in 1:nrow(SinyiClean))
  {
    if (length(Layout[[i]])>3)
    {
      SinyiClean$Shi_[i] <- as.numeric(gsub("[^0-9]","",(sapply(Layout[i], "[[", 4))))
    }
  }
# delete Layout
SinyiClean <- SinyiClean[,-which(names(SinyiClean) %in% c("Layout"))]
# Add Area
AreaList <- c("松山區","信義區","大安區","中山區","中正區","大同區","萬華區","文山區","南港區","內湖區","士林區","北投區")
AreaIndex <- data.frame(Index=1:12, Area=AreaList)
Area <- substr(SinyiClean$Addre, start=1, stop=3)
for (i in 1:length(Area)){
  SinyiClean$Area[i] <- which(AreaIndex$Area == Area[i])
}

# Add isTaoFang
SinyiClean$isTaoFang <- grepl("套房", SinyiClean$Type)
# Clean up Type
SinyiClean$Type <- gsub(" .*","",SinyiClean$Type)

SinyiClean <- SinyiClean[,-which(names(SinyiClean) %in% c("Id"))]
# write csv with address and type
write.csv(SinyiClean, "SinyiCleanCh.csv")

# delete Addre and Type
SinyiClean_ <- SinyiClean[,-which(names(SinyiClean) %in% c("Addre","Type"))]
# write csv, without chinese columns
write.csv(SinyiClean_, "SinyiClean.csv")
