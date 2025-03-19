# documentation.R
library(shiny)
library(DT)


documentation_ui <- function(id){
  # Info
  tabItem(tabName = "Info",
          h2("Information"),
          fluidRow(
            box(title = tags$strong("Documentation"), status = "info",
              includeMarkdown("README.md"),
              width = 12
            )
          ),
          fluidRow(
            box(
              HTML("<p> Developed with R Shiny by Thao Nguyen 2025 </p>"),
              width = 12
            )
          )
  )
  
  
  
}