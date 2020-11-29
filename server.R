library(shiny)
library(ggplot2)
library(dplyr)
library(datasets)
library(rpart)
library(party)
library(fpc)
library(dplyr)

server <- shinyServer(function(input, output, session) {
    
    df2 <- read.csv('sales_data_sample.csv',header = TRUE)
    
    data <- reactive({ 
        req(input$file1) ## requerimiento para solicitud de data
        
        inFile <- input$file1 
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                          choices = names(df), selected = names(df)[2])
        
        updateSelectInput(session, inputId = 'xcoln', label = 'X Variable Num',                              
                          choices = names(df), selected = names(df)[sapply(df, is.numeric)])
        
        return(df)
    })
    
    
    
    output$contents <- renderTable({
        data()
    })
    
    output$ScatPlot <- renderPlot({
        # Scatterplot para 2 variables
        x <- data()[, c(input$xcol, input$ycol)]
        plot(x)
        
    })
    
    observe({
        query <- parseQueryString(session$clientData$url_search)
        bins2 <- query[["bins2"]]
        
        bar_col <- query[["color"]]
        if(!is.null(bins2)){
            2 <- as.integer(bins2)
            updateSliderInput(session, "bins2", value = bins2)
        }
        if(!is.null(bar_col)){
            updateSelectInput(session, "set_col", selected = bar_col)
        }
    })
    
    observe({
        bins2 <- input$bins2
        set_col <-  input$set_col
        link_url <- paste0('http://',
                           session$clientData$url_hostname, ":",
                           session$clientData$url_port,
                           session$clientData$url_pathname,
                           '?bins2=',bins2,"&",
                           'color=',set_col)
        updateTextInput(session,"url",value=link_url)
    })
    
    output$HistoPlot <- renderPlot({
        x2    <- data()[, input$xcoln]
        bins2 <- nrow(data())
        hist(x2, breaks = bins2, col = input$set_col, border = 'green')
        
    })
    
    #sharing_url <- eventReactive(input$generateURL, {
    #    app_url <- "http://ec2-54-85-131-139.compute-1.amazonaws.com/shiny/rstudio/Parcial_Fase_II"
    #    full_url <- paste(app_url, "?TERRITORY=", input$TERRITORY, "&PRODUCTLINE=", input$PRODUCTLINE, "&ORDERNUMBER=", input$ORDERNUMBER, sep="")
    #    a(full_url, href=full_url)
    #})
    
    #output$URLoutput <- renderUI({
    #    sharing_url()
    #})
    
    # Resumen del DataSet
    output$summary <- renderPrint({
        #dataset <- data()
        #dataset <- data %>%
        #    select('SALES', 'TERRITORY', 'ORDERNUMBER')
        #dataset <- data[,c('SALES', 'TERRITORY', 'ORDERNUMBER')]
        dataset <- df2[, c("SALES", "CITY", "YEAR_ID", "PRODUCTLINE","TERRITORY")]
        summary(dataset, digits = 2)
    })
    
    #
    output$Hist2Plot <- renderPlot({
        ggplot(df2, aes(SALES, COUNTRY)) + geom_point(colour='red')+ geom_smooth()
    })
    output$Hist3Plot <- renderPlot({
        #ggplot(df, aes(PRODUCTLINE, SALES)) + geom_point(colour='blue')+ geom_smooth()
        ggplot(df2, aes(PRODUCTLINE)) + geom_bar(colour='blue')
    })
    
    output$Hist4Plot <- renderPlot({
        ggplot(df2, aes(PRODUCTLINE)) + geom_bar(aes(fill = CITY))
    })
    
    #
    
})
