library(shiny)

shinyUI(navbarPage("台北市房價分析",
                   tabPanel("房價地圖",
                            sidebarLayout(
                              sidebarPanel(
                                actionLink("select_none","清空"),
                                checkboxGroupInput("Area", 
                                                   label = h2("行政區"), 
                                                   choices = list("1.松山區"=1,"2.信義區"=2,"3.大安區"=3,"4.中山區"=4,"5.中正區"=5,"6.大同區"=6,"7.萬華區"=7,"8.文山區"=8,"9.南港區"=9,"10.內湖區"=10,"11.士林區"=11,"12.北投區"=12),
                                                   selected = c(3,7))
                              ),
                              mainPanel(
                                plotOutput("TPEitems", height="900px"),
                                plotOutput("MetPrArea", height="400px"),
                                plotOutput("AvgCompare", height="400px")
                              )
                            )
                   ),
                   tabPanel("房價統計",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("Area1", label = "行政區", 
                                            choices = list("台北市"=0,"松山區"=1,"信義區"=2,"大安區"=3,"中山區"=4,"中正區"=5,"大同區"=6,"萬華區"=7,"文山區"=8,"南港區"=9,"內湖區"=10,"士林區"=11,"北投區"=12),
                                            selected = 1),
                                sliderInput("Year", "時間範圍(季)",
                                            103.5, 105.5, c(103.5,105.5), step = 0.25),
                                sliderInput("bins",
                                            "價格分布圖區間數:",
                                            min = 5,
                                            max = 100,
                                            value = 30,step=5),
                                sliderInput("Distnce",
                                            "近捷運站標準--小於___公尺:",
                                            min = 400,
                                            max = 2000,
                                            value = 1200, step=100)
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput("MetPrHist"),
                                plotOutput("MetPrTrend"),
                                plotOutput("DistMRT")
                              )
                            )
                   ),
                   tabPanel("其他統計",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("StatType", label = "統計項目", 
                                            choices = list("地區平均房價對收支--1"=1,"地區平均房價對收支--2"=2,"地區平均房價對收支--回歸1"=3,"地區平均房價對收支--回歸2"=4,"地區平均房價對收支--回歸3"=5,"格局圓餅圖"=6),
                                            selected = 1)
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput("preImage")
                              )
                            )
                   ),
                   tabPanel("關於"
                            
                            
                   ) 
                   
))