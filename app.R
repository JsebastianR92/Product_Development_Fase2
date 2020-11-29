library(shiny)
library(ggplot2)
library(dplyr)
library(datasets)
library(rpart)
library(party)
library(fpc)
library(dplyr)
library(rsconnect)


# Definicion del UI 
ui <- shinyUI(fluidPage(
    titlePanel("Parcial Fase 2"),
    tabsetPanel(
        tabPanel("Carga de archivo",
                 
                 titlePanel("Carga tu archivo"),
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file1', 'Escoje un archivo .csv',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         tags$br(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"')
                         
                     ),
                     mainPanel(
                         h2("Diego Valle - 20003022"),
                         h2("Sebastian Rodriguez - 20003076"),
                         h4(" Segunda fase del parcial en el curso de product development
                            por favor descarga el set de datos (sales_data_sample) del siguiente github: 
                            https://github.com/JsebastianR92/Product_Development_Fase2"),
                         h5("_____________________________________________________________________________________________________________________"),
                         tableOutput('contents')
                     )
                 )
        ),
        tabPanel("Grafico de Dispersion - Multiples variables",
                 pageWithSidebar(
                     headerPanel(' Escoja 2 variables cualquiera '),
                     sidebarPanel(
                         
                         selectInput('xcol', 'Variable X', ""),
                         selectInput('ycol', 'Variable Y', "", selected = "")
                         
                     ),
                     mainPanel(
                         plotOutput('ScatPlot')
                     )
                 )
        ),
        tabPanel("Histograma",
                 pageWithSidebar(
                     headerPanel('Escoja 1 variable numerica'),
                     sidebarPanel(
                         selectInput('xcoln', 'Variable X Num', ""),

                         
                     ),
                     mainPanel(
                         plotOutput('HistoPlot')
                     )
                 )
        ),
        tabPanel("Reporte gerencial", 
                 h1("Estadistica Descriptiva"),
                 verbatimTextOutput("summary"),
                 plotOutput("Hist2Plot", click = "plot_click"),
                 plotOutput("Hist3Plot", click = "plot_click"),
                 plotOutput("Hist4Plot", click = "plot_click")
                 )

    )
)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
    
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
    output$HistoPlot <- renderPlot({
        x2    <- data()[, input$xcoln]
        bins2 <- nrow(data())
        hist(x2, breaks = bins2, col = 'darkgray', border = 'green')
                
    })
    
    # ------------------------PRUEBAS PARA AWS ---------------------------
    #sharing_url <- eventReactive(input$generateURL, {
    #    app_url <- "http://ec2-54-85-131-139.compute-1.amazonaws.com/shiny/rstudio/Parcial_Fase_II"
    #    full_url <- paste(app_url, "?territory=", input$territory, "&customer=", input$customername, "&order=", input$ordernumber, sep="")
    #    a(full_url, href=full_url)
    #})
    
    #output$URLoutput <- renderUI({
    #    sharing_url()
    #})
    # ------------------FIN DE PRUEBAS PARA AWS --------------------------    
    
    # Resumen del DataSet
    output$summary <- renderPrint({
        #dataset <- data()
        #dataset <- data %>%
        #    select('SALES', 'TERRITORY', 'ORDERNUMBER')
        #dataset <- data[,c('SALES', 'TERRITORY', 'ORDERNUMBER')]
        dataset <- df[, c("SALES", "CITY", "YEAR_ID", "PRODUCTLINE","TERRITORY")]
        summary(dataset, digits = 2)
    })
    
    #
    output$Hist2Plot <- renderPlot({
        ggplot(df, aes(SALES, COUNTRY)) + geom_point(colour='red')+ geom_smooth()
        })
    output$Hist3Plot <- renderPlot({
        #ggplot(df, aes(PRODUCTLINE, SALES)) + geom_point(colour='blue')+ geom_smooth()
        ggplot(df, aes(PRODUCTLINE)) + geom_bar(colour='blue')
    })
    
    output$Hist4Plot <- renderPlot({
        ggplot(df, aes(PRODUCTLINE)) + geom_bar(aes(fill = CITY))
    })

    
    
})

#rsconnect::deployApp('C:/Users/jseba/Documents/Galileo/4to Ciclo/PD/ParcialFase2')

# Run the application 
shinyApp(ui = ui, server = server)
