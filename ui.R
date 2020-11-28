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