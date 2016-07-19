
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

AllData <- read.csv("SinyiClean.csv")
target <- AllData[AllData$TotPr<12000,]
target <- target[target$MetPr<250,]
target <- target[target$ConstMet<160,]
target <- target[target$LandMet<50,]

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$TotPrPlot <- renderPlot({
    x <- target$TotPr
    bins <- seq(min(x), max(x), length.out = input$bins)
    # generate bins based on input$bins from ui.R
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main="台北市房價分布(總價)" , xlab="總價(萬)", ylab="件數")
  })
  
  output$MetPrPlot <- renderPlot({
    x <- target$MetPr
    bins <- seq(min(x), max(x), length.out = input$bins)
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main="台北市房價分布(每坪)" , xlab="每坪價格(萬)", ylab="件數")
  })
  
  output$ConsMetPlot <- renderPlot({
    x <- target$ConstMet
    bins <- seq(min(x), max(x), length.out = input$bins)
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main="建坪分布" , xlab="坪數(坪)", ylab="件數")
  })
  
  output$LandMetPlot <- renderPlot({
    x <- target$LandMet
    bins <- seq(min(x), max(x), length.out = input$bins)
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main="地坪分布" , xlab="坪數(坪)", ylab="件數")
  })
  
  output$MetTotPlot <- renderPlot({
  target = AllData[AllData$TotPr<12000,]
  target = target[target$MetPr<250,]
  plot(target$MetPr, target$TotPr, pch=20, cex=0.3, main="總價對每坪價格", xlab="每坪價格(萬)", ylab="總價(萬)")
  })
  
})