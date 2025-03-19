# mod_group_comparison.R
library(shiny)
library(DT)


######## User Interface ######## 
mod_group_comparison_ui <- function(id) {
  ns <- NS(id)  
  tagList(
    h2("Group Comparison"),

    fluidRow(
      box(title = "Table Groups", status = "primary", width = 6,
          tabsetPanel(
            tabPanel("Table 1",
                     downloadButton(ns("download_StrataCatTable"), "Download CSV"),
                     HTML("<p>This table displays the counts and percentages (n%) for the variables in the dataset, stratified by the Group variable. It provides an overview of the frequency distribution of different categories within each variable, with the distribution shown separately for each group.</p>"),
                     DTOutput(ns("data_StrataCatTable"))
            ),
            tabPanel("Table 2",
                     downloadButton(ns("download_StrataContTable"), "Download CSV"),
                     HTML("<p>This table displays the mean (SD) for the variables in the dataset, stratified by the Group variable. It provides an overview of the central tendency and variability of the data for each variable, with the values presented separately for each group.</p>"),
                     DTOutput(ns("data_StrataContTable"))
            )
          )
      ),
      
      # Box für Barplot mit benutzerdefinierter Auswahl und Beschriftungseingabe Strata Group 
      box(title = "Barplot", status = "primary", width = 6,
          HTML("<p>Barplot displaying the frequency distribution of the selected variable.</p>"),
          selectInput(ns("group_bar_var"), "Choose a variable:", choices = NULL),

          checkboxInput(ns("show_na"), "Include NA values in Barplot", value = FALSE),

          textInput(ns("bar_title"), "Plot Title", value = "Barplot"),
          textInput(ns("x_label"), "X-axis Label", value = "Category"),
          textInput(ns("y_label"), "Y-axis Label", value = "Frequency"),
          
          plotlyOutput(ns("barPlotGroup"))
      ),
      
      # Box für Data Table mit Differenzen Mittelwerte zur Baseline
      box(title = "Table Differences Mean Values to Baseline", status = "primary", width = 6,
          tabsetPanel(
            tabPanel("Table 1",
                     downloadButton(ns("download_diff_base"), "Download CSV"),
                     HTML("<p>This table displays the mean differences to baseline along with the corresponding confidence intervals.</p>"),
                     DTOutput(ns("data_data_diff_base"))
            )
          )
      ),
      # Box für Lineplot mit benutzerdefinierter Auswahl und Beschriftungseingabe
      box(title = "Lineplot with CI", status = "primary", width = 6,
          HTML("<p>Lineplot displaying the mean with confidence intervals for the selected variable over time.</p>"),
          selectInput(ns("line_var"), "Choose a variable:", choices = NULL),
          

          textInput(ns("line_title"), "Plot Title", value = "Lineplot"),
          textInput(ns("x_label"), "X-axis Label", value = "Week"),
          textInput(ns("y_label"), "Y-axis Label", value = "Mean per week"),
          
          plotlyOutput(ns("LinePlot"))
      ),

    
      box(
        title = "Test Results", 
        downloadButton(ns("download_test_results_table"), "Download CSV"),
        HTML("<p>This table displays the results of the t-test for the mean differences to baseline, along with the corresponding confidence intervals.</p>"),
        status = "primary", 
        width = 6,
        DTOutput(ns("test_results_table"))  
      )
      
      
    )
    
  )
  
}

######## Server-Funktion ######## 
mod_group_comparison_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Ursprünglich Raw Data
    reactive_data <- reactive({
      req(data())
      data()
    })
    
    #----- Stratified Categorial Table -----#
    StrataCatTable <- reactive({
      df <- reactive_data()
      df <- as.data.frame(df)
      
      vars <- colnames(df)[!colnames(df) %in% c("RTx_Indikation", "Group", "Date", "ID")]
      tab2 <- CreateCatTable(data = df, vars = vars, strata = "Group")
      
      table_df <-print(tab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
      return(table_df)
    })
    output$data_StrataCatTable <- renderDT({
      
      DT::datatable(StrataCatTable())
    })
    output$download_StrataCatTable <- downloadHandler(
      filename = function() { paste0("StrataCatTable_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(StrataCatTable(), file, row.names = TRUE) }
    )
    
    #----- Stratified Continuous Table -----#
    StrataContTable <- reactive({
      df <- reactive_data()
      df <- as.data.frame(df)
      
      vars <- colnames(df)[!colnames(df) %in% c("RTx_Indikation", "Group", "Date", "ID")]
      tab2 <- CreateContTable(data = df, vars = vars, strata = "Group")
      
      table_df <-print(tab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
      return(table_df)
    })
    output$data_StrataContTable <- renderDT({
      
      DT::datatable(StrataContTable())
    })
    output$download_StrataContTable <- downloadHandler(
      filename = function() {
        paste0("StrataContTable_", Sys.Date(), ".csv")  
      },
      content = function(file) {

        options(scipen = 999)  
        

        write.csv(StrataContTable(), file, row.names = TRUE)
        

        options(scipen = 0)  
      }
    )
    
    #----- Diff zur Baseline Berechnung + Data Table -----#
    ## adjustiert
    # Differenzen zur Baseline (NAs ersetzen durch mean())
    data_diff_base <- reactive({
      req(reactive_data())
      
      reactive_data() %>%
        prepare_baseline() %>%
        calculate_differences() %>%
        calculate_summary()
    })
    
    output$download_diff_base <- downloadHandler(
      filename = function() { paste0("data_diff_base_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(data_diff_base(), file, row.names = TRUE) }
    )
    
    output$data_data_diff_base <- renderDT({
      req(data_diff_base())
      
      datatable(
        data_diff_base(),
        options = list(
          scrollX = TRUE,  
          scrollY = TRUE,   
          scroller = TRUE, 
          pageLength = 10,             
          columnDefs = list(          
            list(targets = "_all", render = JS("function(data, type, row, meta) { return data; }"))
          )
        )
      )
    })
    
    
    
    #----- Lineplot -----#

    observe({
      req(reactive_data())
      
      vars <- colnames(reactive_data())
      vars <- vars[!vars %in% c("Group", "Time", "ID", "Date")]  # Nicht relevante Spalten ausschließen
      
      updateSelectInput(session, "line_var", choices = vars, selected = vars[1])
    })
    
    output$LinePlot <- renderPlotly({
      validate(
        need("Group" %in% colnames(reactive_data()), "The column 'Group' is missing from the data."),
        need("Time" %in% colnames(reactive_data()), "The column 'Time' is missing from the data.")
      )
      
      req(input$line_var)
      df <- data_diff_base()
      
      selected_var <- input$line_var
      plot_title <- input$line_title
      x_label <- input$x_label
      y_label <- input$y_label
      
      # LinePlot erstellen
      # plotLinePlot(df, selected_var, plot_title, x_label, y_label)
      ci_lower <- sym(paste0(selected_var, "_ci_lower"))
      ci_upper <- sym(paste0(selected_var, "_ci_upper"))
      question_mean <- sym(paste0(selected_var, "_mean"))
      
      p <- ggplot(df, aes(x = Time, y = !!question_mean, color = Group, group = Group)) +
        geom_line(line_width = 1, alpha = 0.6) +
        geom_point(alpha = 0.6) +
        geom_errorbar(aes(ymin = !!ci_lower, ymax = !!ci_upper), width = 0.2, alpha = 0.6) +  # Konfidenzintervalle hinzufügen
        labs(title = plot_title,
             subtitle = "Konfidenzintervalle",
             x = x_label,
             y = y_label,
             color = "Group") +
        theme_minimal()
      
      ggplotly(p)
      
    })
    
    #----- Barplot strata Group -----#

    observe({
      req(reactive_data())
      
      vars <- colnames(reactive_data())
      vars <- vars[!vars %in% c("Group", "Time", "ID", "Date")]  
      
      updateSelectInput(session, "group_bar_var", choices = vars, selected = vars[1])
    })
    
    output$barPlotGroup <- renderPlotly({
      req(input$group_bar_var)  
      
      validate(
        need("Group" %in% colnames(reactive_data()), "The column 'Group' is missing from the data.")
      )
      
      df <- reactive_data()
      selected_var <- input$group_bar_var
      
      req(selected_var %in% colnames(df))
      
      plot_title <- input$bar_title
      x_label <- input$x_label
      y_label <- input$y_label
      
      # User NA Auswahl
      if (input$show_na) {
        df[[selected_var]] <- ifelse(is.na(df[[selected_var]]), "NA", as.character(df[[selected_var]]))
      } else {
        df <- df[!is.na(df[[selected_var]]), ]
      }
      
      # Barplot erstellen
      p <- ggplot(df, aes(x = .data[[selected_var]], fill = .data[["Group"]])) +
        geom_bar(position = "dodge") +
        labs(x = x_label, y = y_label, title = plot_title, fill = "Group") +
        theme_minimal()
      
      ggplotly(p)
    })
    
    
    ##### tTest Results
    # Alle Spalten mit '_mean' finden
    
    t_test_table <- reactive({
      
      req(data_diff_base()) 
      mean_vars <- grep("_mean$", colnames(data_diff_base()), value = TRUE)
      

      results_df <- data.frame(
        Variable = character(0),
        Mean_ART = numeric(0),
        Mean_IGRT = numeric(0),
        Estimate = numeric(0),
        Conf_Low = numeric(0),
        Conf_High = numeric(0),
        P_Value = numeric(0),
        stringsAsFactors = FALSE
      )
      

      for (var in mean_vars) {
        ttest <- t.test(data_diff_base()[[var]] ~ data_diff_base()$Group)
        

        result_row <- data.frame(
          Variable = var,
          Mean_ART = round(ttest$estimate[1], 3), 
          Mean_IGRT = round(ttest$estimate[2], 3), 
          Estimate = round(ttest$estimate[1] - ttest$estimate[2], 3),  
          Conf_Low = round(ttest$conf.int[1], 3),  
          Conf_High = round(ttest$conf.int[2], 3), 
          P_Value = round(ttest$p.value, 3),      
          stringsAsFactors = FALSE
        )
        

        results_df <- rbind(results_df, result_row)
      }
      
      results_df
    })
    
    output$download_test_results_table <- downloadHandler(
      filename = function() { paste0("test_results_table_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(t_test_table(), file, row.names = TRUE) }
    )

    output$test_results_table <- renderDT({
      req(t_test_table())  # Warte auf den Datensatz
  
      datatable(
         t_test_table() ,
        options = list(
          scrollX = TRUE,  # Ermöglicht horizontales Scrollen
          scrollY = TRUE,  # Ermöglicht vertikales Scrollen
          scroller = TRUE,  # Aktiviert optimiertes Scrolling
          pageLength = 10,  # Zeilen pro Seite
          autoWidth = TRUE, # Automatische Spaltenbreitenanpassung
          responsive = TRUE # Macht die Tabelle responsiv, um auf verschiedene Bildschirmgrößen zu reagieren
        ),
        rownames = FALSE
       )
    })
    
    
  })
  
}
