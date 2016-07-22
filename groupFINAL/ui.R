library(shiny)

shinyUI(navbarPage("台北市房價分析",
                   tabPanel("房價地圖",
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput("Area", 
                                                   label = h2("行政區"), 
                                                   choices = list("1.松山區"=1,"2.信義區"=2,"3.大安區"=3,"4.中山區"=4,"5.中正區"=5,"6.大同區"=6,"7.萬華區"=7,"8.文山區"=8,"9.南港區"=9,"10.內湖區"=10,"11.士林區"=11,"12.北投區"=12),
                                                   selected = c(3,7)),
                                actionButton("checkAll", label = "全選"),
                                actionButton("checkNone", label = "清空"),
                                checkboxGroupInput("PrLevel", 
                                                   label = h2("價位(每坪)"), 
                                                   choices = list("20萬以下"=1,"20萬~40萬"=2,"40萬~80萬"=3,"80萬~120萬"=4,"120萬以上"=5),
                                                   selected = c(1,5)),
                                actionButton("checkAll1", label = "全選"),
                                actionButton("checkNone1", label = "清空")
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
                   tabPanel("關於",
                            "台大系統資訊班課程作品: 使用R語言進行資料分析【270期】(暑期密集班) Jul. 11-22, 2016",br(),
                            a("https://www.csie.ntu.edu.tw/train/?page=course_info.html&courseid=1505",href="https://www.csie.ntu.edu.tw/train/?page=course_info.html&courseid=1505",target="_blank"),
                            h3("Project 原始碼:"),
                            a("https://github.com/Drvinc/R-beginner/tree/master/groupFINAL",href="https://github.com/Drvinc/R-beginner/tree/master/groupFINAL",target="_blank"),
                            h3("資料來源:"),
                            "1. 信義房屋成交行情",br(),
                            a("http://tradeinfo.sinyi.com.tw",href="http://tradeinfo.sinyi.com.tw",target="_blank"),br(),
                            "2. Data.Taipei -- 家庭收支訪問調查",br(),
                            a("http://data.taipei/opendata/datalist/datasetMeta?oid=52e5fce0-c7a3-49a9-a37b-f7d3e014f525",href="http://data.taipei/opendata/datalist/datasetMeta?oid=52e5fce0-c7a3-49a9-a37b-f7d3e014f525",target="_blank"),br(),
                            "3. 地址轉座標: 地理資訊圖資雲服務平台",br(),
                            a("http://tgos.nat.gov.tw/tgos/Web/Address/TGOS_Address.aspx",href="http://tgos.nat.gov.tw/tgos/Web/Address/TGOS_Address.aspx",target="_blank"),br(),
                            "4. 台北捷運站點座標:",br(),
                            a("https://github.com/repeat/taipei-metro-stations",href="https://github.com/repeat/taipei-metro-stations",target="_blank")
                   ) 
                   
))