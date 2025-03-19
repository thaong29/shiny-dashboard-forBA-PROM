# mod_missing_values.R 

library(shiny)
library(DT)


mod_missing_values_ui <- function(id) {
  ns <- NS(id)  
  tabItem(tabName = "missingvalues",
          h2("Missing values"),
          box(title = "Info", status = "warning", width = 6,
              HTML("This module may take longer to load depending on the size of the dataset.")
          ),
            fluidRow(
              box(title = "Missing Data Summary", status = "primary", width = 6,
                  tableOutput(ns("missing_summary"))
              ),
              box(title = "Missing Data Visualization", status = "primary", width = 6,
                  HTML("Provides an overview of whether data are missing. The number of missing data is also indicated in each column."),
                  plotlyOutput(ns("missingPlot"))
              )
          )
  )
}


mod_missing_values_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$missing_summary <- renderTable({
      #miss_var_summary(data)
      req(data())
      miss_var_summary(data())    
    })
    
    # Missing Data Visualization
    output$missingPlot <- renderPlotly({
      #ggplotly(vis_miss(data)) 
      req(data())
      ggplotly(vis_miss(data()))    
    })
    
    
  })
}