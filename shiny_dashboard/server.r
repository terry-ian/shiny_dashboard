#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(htmlwidgets)
library(webshot)

# Define server logic required to draw a histogram
function(input, output) {
    
    #讀取html
    htmlssc <- eventReactive(input$sscb, {includeHTML("./gameview/ssc.html")} , ignoreNULL = FALSE)
    htmlpk10 <- eventReactive(input$pk10b, {includeHTML("./gameview/pk10.html")} , ignoreNULL = FALSE)
    htmlc115 <- eventReactive(input$c115b, {includeHTML("./gameview/c115.html")}, ignoreNULL = FALSE)
    htmld3 <- eventReactive(input$d3b, {includeHTML("./gameview/3d.html")}, ignoreNULL = FALSE)
    htmlfast3 <- eventReactive(input$fast3b, {includeHTML("./gameview/fast3.html")}, ignoreNULL = FALSE)
    htmlhappy8 <- eventReactive(input$happy8b, {includeHTML("./gameview/happy8.html")}, ignoreNULL = FALSE)
    
    #網頁
    output$ssc<-renderUI(htmlssc() )
    output$pk10<-renderUI(htmlpk10() )
    output$c115<-renderUI(htmlc115() )
    output$d3<-renderUI(htmld3() )
    output$fast3<-renderUI(htmlfast3() )
    output$happy8<-renderUI(htmlhappy8() )

    # Read data
    csvfile <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath,header=TRUE , fileEncoding = "UTF-8")
        return(df)
    })
    
    values <- reactiveValues(inDir = NULL)
    observeEvent(input$do_it, {values$inDir <- csvfile()})
    observeEvent(input$do, {values$inDir <- read.csv("./sampledata/test123.csv", header=TRUE, fileEncoding = "UTF-8")})
    observeEvent(input$delete, {values$inDir <- NULL })
    DATA <- eventReactive(values$inDir, {values$inDir})
    
    # set uploaded file
    output$table <- renderDT({
        inFile <- DATA()
    })
    
    # 圖表呈現涵式
    output$scatter <- renderPlotly({
        data1 <- DATA()
        datavv=as.vector(t(data1))
        p <- plot_ly(x = ~datavv,
            type = "histogram",
            cumulative = list(enabled=TRUE) ) %>% layout(title = "累积次数图" ,xaxis=list(title='Number'))
    })
    output$summaryDset <- renderPrint({
        data2 <- DATA()
        summary(data2)
    })
    output$boxplot <- renderPlotly({
        data3 <- DATA()
        databox=as.data.frame(data3)
        p <- plot_ly(type = 'box') %>%
            add_boxplot(y = databox$x1, name = "Only X1", boxpoints = 'outliers',
                        marker = list(color = 'rgb(7,40,89)'),
                        line = list(color = 'rgb(7,40,89)'),) %>%
            add_boxplot(y = databox$x2, name = "Only X2", boxpoints = 'outliers',
                        marker = list(color = 'rgb(9,56,125)'),
                        line = list(color = 'rgb(9,56,125)')) %>%
            add_boxplot(y = databox$x3, name = "Only X3", boxpoints = 'outliers', 
                        marker = list(color = 'rgb(8,81,156)'),
                        line = list(color = 'rgb(8,81,156)')) %>%
            add_boxplot(y = databox$x4, name = "Only X4", boxpoints = 'outliers',
                        marker = list(color = 'rgb(107,174,214)'),
                        line = list(color = 'rgb(107,174,214)')) %>%
            add_boxplot(y = databox$x5, name = "Only X5", boxpoints = 'outliers',
                        marker = list(color = 'rgb(17, 157, 255)'),
                        line = list(color = 'rgb(17, 157, 255)')) %>%
            layout(title = "Box Plot Outliers")
    })
    output$histogram1 <- renderPlot({
        data4 <- DATA()
        data4=as.matrix(data4)
        barplot(table(data4), main = "Count All ball for 0 to 9 Number", xlab = "Ball Number", 
                ylab = "Frequency", col = rainbow(11))
    })
    output$ttest <- renderPrint({
        ballvalue <- DATA()
        t.test(ballvalue, mu = 4.5, alternative = 'greater')
    })
    output$histogram5 <- renderPlot({
        data6 <- DATA()
        data6=as.matrix(data6)
        y_m <- melt(data6)
        histogram(x= ~ value | Var2,data=y_m, xlab="Number",layout=c(5,1) , col = rainbow(10))  # 以5x1的方式呈現圖表
    })
    
    #圓餅圖
    output$piechart <- renderPlotly({
        data7 <- DATA()
        data7=as.data.frame(data7)
        datax1=as.data.frame(table(data7$x1))
        datax2=as.data.frame(table(data7$x2))
        datax3=as.data.frame(table(data7$x3))
        datax4=as.data.frame(table(data7$x4))
        datax5=as.data.frame(table(data7$x5))
        
        p <- plot_ly() %>%
            add_pie(data = datax1 , labels = ~Var1, values = ~Freq,
                    name = "Value 1 ball", domain = list(row = 0, column = 0),hole = 0.6) %>%
            add_pie(data = datax2 , labels = ~Var1, values = ~Freq,
                    name = "Value 2 ball", domain = list(row = 0, column = 1),hole = 0.6) %>%
            add_pie(data = datax3 , labels = ~Var1, values = ~Freq,
                    name = "Value 3 ball", domain = list(row = 0, column = 2),hole = 0.6) %>%
            add_pie(data = datax4 , labels = ~Var1, values = ~Freq,
                    name = "Value 4 ball", domain = list(row = 0, column = 3),hole = 0.6) %>%
            add_pie(data = datax5 , labels = ~Var1, values = ~Freq,
                    name = "Value 5 ball", domain = list(row = 0, column = 4),hole = 0.6) %>%
            layout(title = "Piecharts with different value balls", showlegend = F,
                    grid=list(rows=1, columns=5),
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))    
    })

    ###############################################################
    #爬蟲
    worddata <- reactive({
        url <- "https://1958a.com/forum-71-1.html"
        doc <- read_html(url, encoding = "UTF-8")
        header <- doc %>% html_nodes(".xstt") %>% html_text()
        mixseg = worker()
        test=mixseg[header]
        test1=table(test)
        worddataframe=as.data.frame(test1)
        colnames(worddataframe) <- c("Word", "Freq")
        #deletnumber=!is.na(as.numeric(worddataframe$Word))
        #worddataframe2=worddataframe[deletnumber==FALSE,]
        return(worddataframe)
    })
    
    #直方圖
    output$histword1 <- renderPlot({
        test2 <- worddata()
        subset(test2, Freq>10) %>%
            ggplot( aes(Word , Freq)) +
            geom_bar(stat="identity", fill="darkred", colour="darkgreen") #+
            #theme(axis.text.x=element_text(family="wqy-microhei",angle=45, hjust=1))
        
    })
    
    #樹狀圖
    output$treemap <- renderPlot({
        test3 <- worddata()
        test3$Word<-as.factor(test3$Word)
        subset(test3, Freq>10) %>% ggplot(aes(area=Freq,fill=Word,label=Word))+
            geom_treemap()+geom_treemap_text(fontface='italic',place='centre')#+
            #theme(axis.text.x=element_text(family="wqy-microhei"))
        
    })
    
    #文字雲
    output$cloud <- renderWordcloud2({
        cloudtest <- worddata()
        wordcloud2(cloudtest , color = 'random-light', backgroundColor = 'black' )
    } )
    
    ##################################################################
    #异常侦测
    abdata <- reactive({
        btc <- get_historic_price(start = "2019-01-01")
        btc_ts <- btc %>% rownames_to_column() %>% as.tibble() %>% 
            mutate(date = as.Date(rowname)) %>% select(-one_of('rowname'))
        return(btc_ts)
    })
    
    output$timeseries <- renderPlot({
        btc_ts=abdata()
        btc_ts %>% 
            time_decompose(Price, method = "stl", frequency = "auto", trend = "auto") %>%
            anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
            plot_anomaly_decomposition()
    })
    
    output$anomalydetection <- renderPlot({
        btc_ts=abdata()
        btc_ts %>% 
            time_decompose(Price) %>%
            anomalize(remainder) %>%
            time_recompose() %>%
            plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)
    })
    
    #####################################################
    #地图用
    output$map <- renderPlotly({
        Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoidHJveTgwMTEyNSIsImEiOiJjazJ5Zm4wZjQwN3A1M21ycWpoaHJhcjg0In0.jy7pUHxqfDcG3U_b8X3rkQ')
        df = read.csv('https://raw.githubusercontent.com/bcdunbar/datasets/master/meteorites_subset.csv')
        p <- df %>%
            plot_mapbox(lat = ~reclat, lon = ~reclong,
                        split = ~class, size=2,
                        mode = 'scattermapbox', hoverinfo='name') %>%
            layout(title = 'Meteorites by Class',
                   font = list(color='white'),
                   plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
                   mapbox = list(style = 'dark'),
                   legend = list(orientation = 'h',
                                 font = list(size = 8)),
                   margin = list(l = 25, r = 25,
                                 b = 25, t = 25,
                                 pad = 2))        
    })
    
}

