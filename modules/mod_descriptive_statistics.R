# mod_descriptie_statistics.R

library(shiny)
library(DT)

## User Interface
mod_descriptive_statistics_ui <- function(id) {
  ns <- NS(id)  
  tagList(
    h2("Descriptive Statistics"),
    HTML("<p>The following tables and bar plots are based on calculations performed using the raw data.</p>"),

      fluidRow(
        # Box für Tabellenübersicht mit mehreren Tabs
        box(title = "Table Overview", status = "primary", width = 6,
            tabsetPanel(
              tabPanel("Table 1",
                       downloadButton(ns("download_summary"), "Download CSV"),
                       HTML("<p>This table displays summary statistics for the dataset, including the minimum, 1st quartile, median, mean, 3rd quartile, and maximum values. These statistics provide a comprehensive overview of the distribution of the data for each variable.</p>"),
                       DTOutput(ns("summary_stats"))
              ),
              tabPanel("Table 2",
                       downloadButton(ns("download_CatTable"), "Download CSV"),
                       HTML("<p>This table displays the counts and percentages (n%) for the variables in the dataset. It provides an overview of the frequency distribution of different categories within each variable.</p>"),
                       DTOutput(ns("data_CatTable"))
              ),
              tabPanel("Table 3",
                       downloadButton(ns("download_ContTable"), "Download CSV"),
                       HTML("<p>This table displays the mean (SD) for the variables in the dataset. It gives a measure of central tendency and variability for each variable.</p>"),
                       DTOutput(ns("data_ContTable"))
              )
            )
        ),
        
        
        # Box für Barplot mit benutzerdefinierter Auswahl und Beschriftungseingabe
        box(title = "Barplot", status = "primary", width = 6,
            HTML("<p>Barplot displaying the frequency distribution of the selected variable.</p>"),
            selectInput(ns("bar_var"), "Choose a variable:", choices = NULL),
            
            # Eingabefelder für benutzerdefinierte Titel und Achsenbeschriftungen
            textInput(ns("bar_title"), "Plot Title", value = "Barplot"),
            textInput(ns("x_label"), "X-axis Label", value = "Category"),
            textInput(ns("y_label"), "Y-axis Label", value = "Frequency"),
            
            plotlyOutput(ns("barPlot"))
        )
      )
  )
}


# Server-Funktion für das Deskriptive Statistik-Modul
mod_descriptive_statistics_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    reactive_data <- reactive({
      req(data())
      data()
    })
    
    
    #Summary Statistiken
    summary_df <- reactive({
      df <- reactive_data()
      vars <- colnames(df)[!colnames(df) %in% c("RTx_Indikation", "Group", "Date", "ID")]  
      summary(df[, vars])
    })
    output$summary_stats <- renderDT({
      datatable(summary_df())
    })
    output$download_summary <- downloadHandler(
      filename = function() { paste0("summary_stats_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(summary_df(), file, row.names = FALSE) }
    )
    


    
    # Kategorische Tabelle
    
    cat_df <- reactive({
      df <- reactive_data()
      df <- as.data.frame(df)
      
      vars <- colnames(df)[!colnames(df) %in% c("RTx_Indikation", "Group", "Date", "ID")]
      tab2 <- CreateCatTable(data = df, vars = vars)
      
      cat_table_df <-print(tab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
      return(cat_table_df)
    })
    output$data_CatTable <- renderDT({
      
      DT::datatable(cat_df())
    })
    output$download_CatTable <- downloadHandler(
      filename = function() { paste0("CatTable_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(cat_df(), file, row.names = TRUE) }
    )
    
    # Kontinuierliche Tabelle
    cont_df <- reactive({
      df <- reactive_data()
      df <- as.data.frame(df)
      
      vars <- colnames(df)[!colnames(df) %in% c("RTx_Indikation", "Group", "Date", "ID")]
      tab2 <- CreateContTable(data = df, vars = vars)
      
      cat_table_df <-print(tab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
      return(cat_table_df)
    })
    
    output$data_ContTable <- renderDT({
      
      DT::datatable(cont_df())
    })
    output$download_ContTable <- downloadHandler(
      filename = function() { paste0("ContTable_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(cont_df(), file, row.names = TRUE) }
    )
    

    observe({
      req(data())  # Daten müssen geladen sein
      
      vars <- colnames(data())
      vars <- vars[!vars %in% c("Group", "Time", "ID", "Date")]  
      
      updateSelectInput(session, "bar_var", choices = vars)
    })
    # # Barplot
    output$barPlot <- renderPlotly({
      req(input$bar_var)
      df <- reactive_data()

      selected_var <- input$bar_var
      plot_title <- input$bar_title
      x_label <- input$x_label
      y_label <- input$y_label

      # Barplot erstellen
      plotBarPlot(df, selected_var, plot_title, x_label, y_label)

    })
    
  })
    
}

