#部署的package
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(readxl)
library(shinycssloaders) #動態等待圖式
library(rvest) # 爬蟲相關套件
library(httr) # 爬蟲相關套件
library(stringr) # 處理字串的套件
library(wordcloud2)  
library(jiebaR)
library(treemapify) #树状图
library(showtext) #文字編碼
require(reshape2)
require(lattice)
library(skimr)   #进阶 summary skim(iris)
library(plotly)

###### 异常侦测包
library(anomalize) #tidy anomaly detectiom
library(tidyverse) #tidyverse packages like dplyr, ggplot, tidyr
library(coindeskr) #bitcoin price extraction from coindesk

showtext_auto()  #文字字動編碼

#引入需要的涵式
#upload=source("uploadcsv.R",verbose = FALSE)
#source("server.R")

# Define UI for application that draws a histogram
############################   標題設計  ##########################

Header <- dashboardHeader(title = "Lottery Dashboard" ,
                          dropdownMenu(type = "messages",
                                       messageItem(
                                           from = "Sales Dept",
                                           message = "Sales are steady this month."
                                       ),
                                       messageItem(
                                           from = "New User",
                                           message = "How do I register?",
                                           icon = icon("question"),
                                           time = "13:45"
                                       ),
                                       messageItem(
                                           from = "Support",
                                           message = "The new server is ready.",
                                           icon = icon("life-ring"),
                                           time = "2014-12-01"
                                       )
                          ),
                          # This is a drop-down menu for checking notifications.
                          # This should alert users of alerts that have not been merged to a case in the last 15 days.
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                           text = "5 new users today",
                                           icon("users")
                                       ),
                                       notificationItem(
                                           text = "12 items delivered",
                                           icon("truck"),
                                           status = "success"
                                       ),
                                       notificationItem(
                                           text = "Server load at 86%",
                                           icon = icon("exclamation-triangle"),
                                           status = "warning"
                                       )
                          ),
                          # This is a drop-down menu for checking tasks.
                          # This drop-down menu will eventually offer suggestions based off of ML Algorithms.
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 90, color = "green",
                                                "Documentation"
                                       ),
                                       taskItem(value = 17, color = "aqua",
                                                "Project X"
                                       ),
                                       taskItem(value = 75, color = "yellow",
                                                "Server deployment"
                                       ),
                                       taskItem(value = 80, color = "red",
                                                "Overall project"
                                       )
                          )
)


#########################  側邊設計  Sidebar content########################

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("奖金组机率表", tabName = "reports", icon = icon("list-alt")),
        menuItem("统计图表检验", tabName = "dashboard", icon = icon("bar-chart-o")),
        menuItem("网路声量文字云", tabName = "wordcloud", icon = icon("th")),
        menuItem("风险异常侦测", tabName = "abnormal", icon = icon("dashboard")),
        menuItem("大陆各地区游玩人数", tabName = "showmap", icon = icon("fas fa-map-marked"))
    ) 
)

###############################主內文 Body content###############################

body <- dashboardBody(
    #大主軸
    tabItems(
        # 第一页的内容
        tabItem(tabName = "reports",
                fluidRow(
                    # 自定义输出面板
                    navbarPage(
                        title = '',
                        tabPanel(title = actionButton("sscb",'时时彩奖金组'), titlePanel(''), htmlOutput("ssc") ),
                        tabPanel(title = actionButton("pk10b",'北京PK拾奖金组'), titlePanel(''), htmlOutput("pk10") ),
                        tabPanel(title = actionButton("c115b",'十一选五奖金组'), titlePanel(''), htmlOutput("c115") ),
                        tabPanel(title = actionButton("d3b",'3D奖金组'), titlePanel(''), htmlOutput("d3") ),
                        tabPanel(title = actionButton("fast3b",'江苏快三奖金组'), titlePanel(''), htmlOutput("fast3") ),
                        tabPanel(title = actionButton("happy8b",'北京快樂8奖金组'), titlePanel(''), htmlOutput("happy8") )
                    )    
                )
        ),
        
        #第二分頁
        tabItem(tabName = "dashboard",  
                #第一個框架讀取數據
                fluidRow(
                    #upload_css
                    box(title = "Upload the data", status = "primary", 
                        solidHeader = TRUE,
                        collapsible = TRUE ,
                        width=12 ,helpText("Default max. file size is 5 MB."),
                        ################## upload CSV 檔案
                        sidebarLayout(
                            sidebarPanel(
                                fileInput("file1", "  Choose CSV File ", multiple = TRUE,
                                          accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv"
                                          )
                                ),
                                actionButton("do_it", "Click Here Send Data" , icon("paper-plane")),
                                # Horizontal line ----
                                tags$hr(),
                                # Input: Checkbox if file has header ----
                                checkboxInput("header", "Header", TRUE)
                                
                            ),
                            
                            # output
                            mainPanel(
                                DTOutput(outputId = "table")
                            )
                        )
                        ############################  upload 結束
                        ,br()
                        ,actionButton("do", "  Click Here Show Sample Data" , icon("paper-plane"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        ,actionButton("delete", "Clear Data" , icon("calendar") ,style = "color: white; background-color: red")
                        )
                ),
                
                
                #第二組框架各圖式 plotlyOutput
                fluidRow(
                    
                    #piechart   分裂圖表splitLayout(cellWidths = c("19%", "19%"), plotlyOutput("pie1"))
                    box(title = "Piechart", status = "info", solidHeader = TRUE,
                        collapsible = TRUE,
                        withSpinner(plotlyOutput("piechart")) ,width = 12, height = 480),
                    
                    #histogram 5
                    box(title = "Histogram 5", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        withSpinner(plotOutput("histogram5", height = 250)) ,width = 6, height = 350),
                    #box plot
                    box(title = "Boxplot", status = "warning", solidHeader = TRUE,
                        collapsible = TRUE,
                        withSpinner(plotlyOutput("boxplot", height = 250)) ,width = 6, height = 350),
                    
                    #histogram
                    box(title = "Histogram", status = "success", solidHeader = TRUE,
                        collapsible = TRUE,
                        withSpinner(plotOutput("histogram1", height = 280)) ,width = 6, height = 350),
                    #Cumulative Histogram 
                    box(title = "Cumulative Histogram", status = "info", solidHeader = TRUE,
                        collapsible = TRUE,
                        withSpinner(plotlyOutput("scatter", height = 270)) ,width = 6, height = 350),
                    
                    #summary 
                    box(title = "Summary", status = "warning", solidHeader = TRUE,
                        collapsible = TRUE,
                        withSpinner(verbatimTextOutput("summaryDset")) ,width = 6, height = 300),
                    #T-test
                    box(title = "T-test u", status = "danger", solidHeader = TRUE,
                        collapsible = TRUE,
                        withSpinner(verbatimTextOutput("ttest")) ,width = 6, height = 300)
                    
                )
        ),
        
        # 第三页的内容
        tabItem(tabName = "wordcloud",
                
                titlePanel(h1("大家赢博奕论坛",style={'color = "blue";'})),
                fluidRow(
                    
                    #文字雲
                    box(title = "Word clouds", status = "success", solidHeader = TRUE,
                        collapsible = TRUE,
                        withSpinner(wordcloud2Output('cloud', width = "100%")) ,width = 12),
                    
                    #长条图
                    box(title = "Histogram word", status = "warning", solidHeader = TRUE,
                        collapsible = TRUE,
                        withSpinner(plotOutput('histword1')) ,width = 6 ),
                    
                    #树状图
                    box(title = "Tree map", status = "danger", solidHeader = TRUE,
                        collapsible = TRUE,
                        withSpinner(plotOutput('treemap')) ,width = 6 )
                    #顏色選擇
                    ###primary Blue ,success Green ,info Blue ,warning Orange ,danger Red
                )
        ),
        
        
        # 第四页的内容
        tabItem(tabName = "abnormal",
                #范例按钮
                fluidRow(

                    navbarPage(
                        title = '',
                        tabPanel(title = '异常下注侦测', titlePanel(''), 
                                 
                                 #异常侦测图1
                                 box(title = "Time Series Decomposition with Anomalies", status = "warning", solidHeader = TRUE,
                                     collapsible = TRUE,
                                     withSpinner(plotOutput('timeseries')) ,width = 12 ),
                                 #异常侦测图2
                                 box(title = "Anomaly Detection", status = "success", solidHeader = TRUE,
                                     collapsible = TRUE,
                                     withSpinner(plotOutput('anomalydetection')) ,width = 12 )
                                 ),
                        tabPanel(title = '异常出入款侦测', titlePanel('')),
                        tabPanel(title = '异常连线数侦测', titlePanel(''))
                        
                    ),
                    
                    
                    
                )    
        ),
        
        # 第五页的内容
        tabItem(tabName = "showmap",
                fluidRow(
                    # 自定义输出面板
                    box(title = "World Map", status = "info", solidHeader = TRUE,
                        collapsible = TRUE,
                        withSpinner(plotlyOutput("map" , height = 600)) ,width = 12 , height = 700),
                    
                )
        )
        
    ) #tabItems底框
    
)#body底框

##########################    合併三大部分      #####################

dashboardPage(Header,sidebar,body , title = "Torin Dashboard")


