##########################                     ###########################
########################## Main Dashboard file ###########################
################## to start runApp('main.R') in Terminal ####################

#----------------------------- Packages  --------------------------------#
#------------------------------------------------------------------------#
# Packages
pacman::p_load(
  shiny,
  shinyFiles,
  #shinydashboard,
  DT,
  yaml,
  bs4Dash,
  
  # for Datavisualization and Datamanipulation
  plotly,
  tableone,
  kableExtra,
  ggplot2,
  naniar,
  rlang,
  dplyr,
  lubridate,
  # to work with html
  htmltools,
  rmarkdown,
  markdown

)


#--------------------------- Configuration ------------------------------#
#------------------------------------------------------------------------#
# Zugriff auf Module
# Hier auswählen, welche Module zur Auswertung durchgeführt werden sollen.
config <- yaml::read_yaml("config.yaml")

#----------------------------- Load Data and modules --------------------------------#
#------------------------------------------------------------------------#
# Externe Plot-Funktion: hier können eigene Funktionen und Daten aus externen Files geladen werden
source("data_plot_table_functions.R") 
# Documentation
source("documentation.R")
# Raw Data
source("modules/mod_raw_Data.R")
# Module: Descriptive Statistics
source("modules/mod_descriptive_statistics.R")
# Module: Group comparison
source("modules/mod_group_comparison.R")
# Module: Test statistics
source("modules/mod_test_statistics.R")
# Module: Missing values
source("modules/mod_missing_values.R")

#------------------------- UI User Interface ----------------------------#
#------------------------------------------------------------------------#
# Hier wird die Benutzeroberfläche gestaltet.
# Die erstellten Elemente im Server-Bereich werden hier integriert.

ui <- dashboardPage(
  title = "Statistical Analysis",
  
  #### Aesthetics ####
  skin = "black",
  
  #### Header ####
  dashboardHeader(
    title = tagList(icon("chart-line"), tags$strong("Statistical Analysis")),
    titleWidth = 250
  ),
  
  #### Sidebar content ####
  dashboardSidebar(
    sidebarMenu(
      menuItem("Info", tabName = "Info", icon = icon("circle-info")),      
      menuItem("Raw Data", tabName = "RawData", icon = icon("th")),
      
      menuItem("Descriptive statistics", tabName = "deskriptivestatistics", icon = icon("table")),
      
      menuItem("Group comparison", tabName = "groupcomparison", icon = icon("table")),
      
      menuItem("Test statistics", tabName = "teststatistics", icon = icon("percent")
               # Sub-Items für Test statistics
               # menuSubItem("T-Test", tabName = "ttest")
               # Wilcoxon Test
      ),
      
      menuItem("Module: Missing values", tabName = "missingvalues", icon = icon("notdef")),
      
      menuItem("Download Report", tabName = "Download", icon = icon("download"))
      
    )
  ),
  
  #### Body content ####
  dashboardBody(
    
    ### Documentation ###
    tabItems(tabItem(tabName = "Info", documentation_ui("documentation")),
      
    ### Raw Data ###
    tabItem(tabName = "RawData", raw_data_ui("rawdata")),
      
    ### Descriptive Statistics ###
    tabItem(tabName = "deskriptivestatistics", mod_descriptive_statistics_ui("descriptive")),
    
    ### Group Comparison ###
    tabItem(tabName = "groupcomparison", mod_group_comparison_ui("groupcomparison")),
      
    ### Test statistics ###
    tabItem(tabName = "teststatistics", mod_test_statistics_ui("teststatistics")),

    ### Missing values ###
    # TODO Missing Imputation
    tabItem(tabName = "missingvalues", mod_missing_values_ui("missingvalues")),    
  
      
    ### Download Report ###
    # TODO
    tabItem(tabName = "Download",
              h2("Download your report"),
            h4("Not working yet! Still in progress"),
            
            sidebarLayout(
              sidebarPanel(
                radioButtons("format", "Choose the format:",
                             choices = c("HTML" = "html", "PDF" = "pdf")),
                downloadButton("downloadReport", "Download Report")
              ),
              mainPanel(
                tableOutput("dataPreview")
              )
            )
      )
    )
  )
)

#------------------------- Server des Dashboards ----------------------------#
# Hier passiert die Auswertung (Berechnungen, Erstellen von Tabellen, Plots, ...)

server <- function(input, output, session) {
  # Input Data of User
  data <- reactive({
    req(input$file1)  
    file_ext <- tools::file_ext(input$file1$name)
    switch(file_ext,
           csv = read.csv(input$file1$datapath),
           xlsx = readxl::read_excel(input$file1$datapath),
           xls = readxl::read_excel(input$file1$datapath),
           validate(
             need(file_ext %in% c("csv", "xlsx", "xls"), 
                  "Invalid file type! Please upload a CSV, XLS, or XLSX file.")
           )
    )

  })
  
  # Raw Data 
  raw_data_server("rawdata", data = data)

  
  # Descriptive Statistics
  mod_descriptive_statistics_server("descriptive", data)
  
  # Group Comparison 
  mod_group_comparison_server("groupcomparison", data)
  
    
  # Test Statistics
  mod_test_statistics_server("teststatistics", data)
  
  # Missing Values
  mod_missing_values_server("missingvalues", data = data)
  
  
  # # Download HTML:
  # output$download_report <- downloadHandler(
  #   filename = function() {
  #     paste("report_", Sys.Date(), ".", input$format, sep = "")
  #   },
  #   content = function(file) {
  #     tempReport <- file.path(tempdir(), "report_template.Rmd")
  #     file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
  #     
  #     params <- list(data = data())  # Übergibt Daten als Parameter
  #     
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv()))
  #   }
  # )
  
  
}

#------------------------------ Dashboard starten ---------------------------------#
shinyApp(ui, server)
