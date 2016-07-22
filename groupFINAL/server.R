library(shiny)
library(ggmap)
library(mapproj)
library(ggplot2)

AllData <- read.csv("SinyiDist.csv")
target <- AllData[AllData$TotPr<12000,]
target <- target[target$MetPr<250,]
target$PrLevel <- ifelse(target$MetPr<20, "1",
                         ifelse(target$MetPr<40, "2",
                                ifelse(target$MetPr<80, "3",
                                       ifelse(target$MetPr<120, "4", "5"))))

AreaList <- c("Song-Shan","Sin-Yi","Da-An","Chung-Shan","Chung-Cheng","Da-Tung","Wan-Hua","Wen-Shan","Nan-Gang","Nei-Hu","Shi-Lin","Bei-Tou")

shinyServer(function(input, output, session) {
  selAll <- observeEvent(input$checkAll, {
    updateCheckboxGroupInput(session,"Area", selected=as.character(c(1:12)))
  })
  delALL <- observeEvent(input$checkNone, {
    updateCheckboxGroupInput(session,"Area", selected=as.character(c()))
  })
  selAll1 <- observeEvent(input$checkAll1, {
    updateCheckboxGroupInput(session,"PrLevel", selected=as.character(c(1:5)))
  })
  delALL1 <- observeEvent(input$checkNone1, {
    updateCheckboxGroupInput(session,"PrLevel", selected=as.character(c()))
  })
  #TAB1
  output$TPEitems <- renderPlot({
    A_select <- as.vector(input$Area)
    Pr_select <- as.vector(input$PrLevel)
    tarr <- subset(target, Area %in% A_select)
    tarr <- subset(tarr, PrLevel %in% Pr_select)
    
    
    # plot items on TPE city
    x <- tarr$MetPr
    tarr$diff <- with(tarr, sqrt(MetPr/250))
    p <- ggmap(get_map(location=c(lon=121.545,lat=25.0545), zoom=12, maptype = "roadmap"))
    p <- p + geom_point(data=tarr,aes(Response_X,Response_Y,colour=diff,size=diff))
    p <- p + scale_color_gradient2(low="black",mid="blue",high = "red",midpoint = 0.5)
    p <- p + scale_size(range=c(1,2.5))
    p
  })
  output$MetPrArea <- renderPlot({
    A_select <- as.vector(input$Area)
    tarr <- subset(target, Area %in% A_select)
    df = subset(tarr, select=c("Area","MetPr"))
    bymedian <- with(df, reorder(Area, MetPr, median)) #sort by median
    A_select <- as.vector(levels(bymedian))
    A_names <- vector(mode="character",length=length(A_select))
    for (i in 1:length(A_select))
    {A_names[i] <- as.character(AreaList[as.numeric(A_select[i])])}
    box <- ggplot(data = df,aes(x=bymedian , y=MetPr,fill = bymedian)) + coord_cartesian(ylim = c(10, 200))
    box <- box + geom_boxplot(outlier.size = 1,outlier.colour = "gray",position = "dodge",colour = "#666666",width = 0.5)
    box <- box + labs(x = "Area", y = "Per Ping price (10K)", title = "Price Comparison by Area")
    box
  })
  output$AvgCompare <- renderPlot({
    A_select <- as.vector(input$Area)
    meanPrice <- data.frame("Area"=c(1:13),"MeanMetPr"=0)
    for (i in 1:12){
      tarr <- target[(target$Area == i),]
      meanPrice$MeanMetPr[i] <- mean(tarr$MetPr,na.rm=TRUE)
    }
    meanPrice$MeanMetPr[13]=mean(target$MetPr,na.rm=TRUE)
    
    x = A_select
    df <- meanPrice[meanPrice$Area %in% c(x,13),]
    A_names <- vector(mode="character",length=length(A_select))
    for (i in 1:length(A_select))
    {A_names[i] <- as.character(AreaList[as.numeric(A_select[i])])}
    A_names <- c(A_names, "TPE") 
    #barplot(height=df$MeanMetPr, horiz=TRUE, names.arg = A_names)
    df$Area = factor(df$Area,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13))
    c <- ggplot(df, aes(x = df$Area , y = df$MeanMetPr,fill = Area))
    c <- c + geom_bar(stat="identity",width = 0.5)
    c <- c +labs(x = "Area", y = "Avg per Ping price (10K)")
    c 
  })
  
  #TAB2
  output$MetPrHist <- renderPlot({
    A_select <- input$Area1
    tarr1 <- target[target$Area==A_select,]
    if(A_select == 0){tarr1 <- target}
    df = subset(tarr1, select=c("Area","MetPr","YyySe"))
    
    Y1 <- as.numeric(input$Year[1])
    Y2 <- as.numeric(input$Year[2])
    SeN <- ((Y2-Y1)/0.25)+1
    YSe <- vector(mode="character",length = SeN+1)
    int1 <- as.integer(Y1)
    dec1 <- Y1-as.integer(Y1)
    for (i in 1:SeN){
      YSe[i] <- paste(int1+as.integer(dec1+(i-1)*0.25),(dec1+(i-1)*0.25-as.integer(dec1+(i-1)*0.25))/0.25+1,sep="")
    }
    
    df = subset(tarr1, YyySe %in% YSe)
    bins <- input$bins
    hist(df$MetPr, breaks=bins, col="gray",xlab = "Per Ping price (10K)", ylab = "Deal number (counts)", main=paste(ifelse(A_select==0,"Taipei ",AreaList[as.numeric(A_select)]),"'s Distribution of per Ping Price: ",YSe[1]," to ",YSe[SeN],sep=""))
  })
  output$MetPrTrend <- renderPlot({
    A_select <- input$Area1
    tarr2 <- target[target$Area==A_select,]
    if (A_select==0){tarr2 <- target}
    
    Y1 <- as.numeric(input$Year[1])
    Y2 <- as.numeric(input$Year[2])
    SeN <- ((Y2-Y1)/0.25)+1
    YSe <- vector(mode="character",length = SeN+1)
    int1 <- as.integer(Y1)
    dec1 <- Y1-as.integer(Y1)
    for (i in 1:SeN){
      YSe[i] <- paste(int1+as.integer(dec1+(i-1)*0.25),(dec1+(i-1)*0.25-as.integer(dec1+(i-1)*0.25))/0.25+1,sep="")
    }
    tarr2 = subset(tarr2, YyySe %in% YSe)
    
    YSeRange <- levels(as.factor(tarr2$YyySe))
    meanPr2 <- data.frame("YyySe"=YSeRange,"MeanMetPr"=0)
    for (i in 1:length(YSeRange)){
      tarrM <- tarr2[(tarr2$YyySe == YSeRange[i]),]
      meanPr2$MeanMetPr[i] <- mean(tarrM$MetPr,na.rm=TRUE)
      meanPr2$Year[i] <- substr(meanPr2$YyySe[i], start=1, stop=3)
      meanPr2$Season[i] <- substr(meanPr2$YyySe[i], start=4, stop=4)
    }
    
    p <- ggplot(meanPr2, aes(x = YyySe, y = MeanMetPr , color = Season, group=1)) + geom_point(size = 3.8) + geom_line(size = 0.8) 
    p <- p +labs(x = "Season", y = "Avg per Ping price (10K)", title=paste(ifelse(A_select==0,"Taipei",AreaList[as.numeric(A_select)]),"'s Trend: ",YSe[1]," to ",YSe[SeN],sep=""))
    p
  })
  output$DistMRT <- renderPlot({
    A_select <- input$Area1
    tarr3 <- target[target$Area==A_select,]
    if (A_select==0){tarr3 <- target}
    
    NearCrit <- input$Distnce
    tarr3$DistNear <- ifelse(tarr3$distMRT<NearCrit,TRUE,FALSE)
    box <- ggplot(data=tarr3 ,aes(x=DistNear , y=MetPr,fill = DistNear)) 
    box <- box + geom_boxplot(outlier.size = 2, outlier.shape = 1,outlier.colour = "gray",position = "dodge",colour = "#666666",width = 0.5)
    box <- box + labs(x = "Near MRT or Not", y = "Per Ping price (10K)", title = paste0(ifelse(A_select==0,"Taipei",AreaList[as.numeric(A_select)])," Near MRT vs. Far: 1033 to 1053"))
    box
  })
  
  #TAB3
  output$preImage <- renderImage({
    filename <- normalizePath(file.path('./images',
                                        paste('image', input$StatType, '.png', sep='')))
    list(src = filename,
         alt = paste("Image number", input$n))
    
  }, deleteFile = FALSE)
})