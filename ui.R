library(shiny)
library(ggplot2)
library(dplyr)
#library(ggpubr)


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
                         selectInput("set_col",
                                     "Seleccione el color: ",
                                     choices=c("aquamarine", "blue", "blueviolet", "darkgray", "chocolate"),
                                     selected='darkgray'),
                         textInput("url",value="", label="URL"),
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