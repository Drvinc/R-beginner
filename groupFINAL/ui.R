
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("台北市房價分析"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "區間數:",
                  min = 5,
                  max = 100,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("TotPrPlot"),
      plotOutput("MetPrPlot"),
      plotOutput("ConsMetPlot"),
      plotOutput("LandMetPlot"),
      plotOutput("MetTotPlot")
    )
  )
))
