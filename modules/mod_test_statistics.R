#mod_test_statistics.R
library(shiny)
library(DT)
library(dplyr)
mod_test_statistics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Test Statistics"),
    HTML("<p>The following results are based on the t-Test applied to the raw data.</p>"),
    
    # Auswahl des Tests (nur t-Test)
    radioButtons(ns("test_type"), "Choose a test:", 
                 choices = c("t-Test" = "t"), 
                 inline = TRUE),
    
    # Auswahl der Variablen
    selectInput(ns("var_select"), "Choose a variable:", choices = NULL),
    
    # Infobox
    box(
      title = "Test Interpretation", status = "info", width = 12,
      markdown("
    **What does the chosen t-test mean?**
    The independent t-test compares the mean of a numerical variable between two independent groups.

    - **p-value < 0.05** → Statistically significant difference between the groups
    - **p-value ≥ 0.05** → No significant difference between the groups

    - **t-value**: Indicates the size of the difference relative to the variability in the data
    - **Confidence Interval**: Provides the range in which the true mean difference lies
  ")
    ),
    

    box(
      title = "Test Results", status = "primary", width = 12,
      verbatimTextOutput(ns("test_result"))
    )
    
  )
}

mod_test_statistics_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      req(data())
      vars <- colnames(data())
      vars <- vars[!vars %in% c("Group", "Time", "ID", "Date")]
      updateSelectInput(session, "var_select", choices = vars)
    })
    
    output$test_result <- renderPrint({
      req(data(), input$var_select, input$test_type)
      
      df <- data()
      selected_var <- input$var_select
      group_var <- "Group"  # Gruppenvariable festgelegt
      
      if (!group_var %in% colnames(df)) {
        return("⚠️ Fehler: Gruppenvariable 'Group' nicht gefunden.")
      }
      
      # t-Test (immer unabhängig)
      if (input$test_type == "t") {
        if (!is.numeric(df[[selected_var]])) {
          return("⚠️ Fehler: t-Test erfordert eine numerische Variable.")
        }
        
        test_result <- tryCatch(
          t.test(df[[selected_var]] ~ df[[group_var]], data = df, var.equal = FALSE),
          error = function(e) return("⚠️ Fehler beim t-Test: Überprüfe die Daten.")
        )
        
        return(test_result)
      }
      

      return("⚠️ Kein Test ausgewählt.")
    })
    
  })
}
