# Raw_Data.R
library(shiny)
library(DT)

raw_data_ui <- function(id){
  ns <- NS(id)  
  tabItem(tabName = "RawData",
          h2("Your data"),
          fluidRow(
            box(
              title = "Upload your Data (.csv, .xlsx, .xls)",  
              width = 12,   
              fileInput("file1", "Choose CSV or Excel File",
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv",
                          ".xlsx",  
                          ".xls")  
              )
            )
          )
          ,
          
          fluidRow(
            box(
              title = "Data Table",
              width = 12,
              div(style = 'overflow-x: scroll',
                  DTOutput(ns("data_table"))
              )
            )
          )
  )
}

raw_data_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    reactive_data <- reactive({
      req(data())  
      data()       
    })

    output$data_table <- renderDT({ 
      datatable(reactive_data(), options = list(pageLength = 10))  
    })
    
  })
}