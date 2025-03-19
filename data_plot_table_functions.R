# data_plot_table_function.R

#--------------------------- Table functions -----------------------------#
#-------------------------------------------------------------------------# 
# Funktion für deskriptive Tabelle
createCatTable <- function(data) {
  tab2 <- CreateCatTable(data = data, vars = c(colnames(data)))
  
  print(tab2, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

}

createContTable <- function(data) {
  tab2 <- CreateContTable(data = data, vars = c(colnames(data)))
  
  print(tab2, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
  
}

# Funktion für deskriptive Tabelle Stratifizierung nach best. Gruppe
createStrataCatTable <- function(data, strata_var) {
  tab2 <- CreateCatTable(data = data, vars = colnames(data), strata = strata_var)  
  
  print(tab2, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

}


createStrataContTable <- function(data, strata_var) {
  tab2 <- CreateContTable(data = data, vars = colnames(data), strata = strata_var)  
  
  print(tab2, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

}
#--------------------------- Plot functions -----------------------------#
#------------------------------------------------------------------------# 
# Barplot für eine Variable
plotBarPlot <- function(data, variable, plot_title, x_label, y_label) {
  data[[variable]] <- as.factor(data[[variable]])
  
  p <- ggplot(data, aes(x = !!sym(variable))) +  # Verwende sym() und !! für die Auswertung
          geom_bar(fill = "steelblue") +
          labs(x = x_label, y = y_label, title = plot_title) +
          theme_minimal()
  
  ggplotly(p)
}

plotBarPlotGroup <- function(data, variable, plot_title, x_label, y_label) {
  data[[variable]] <- as.factor(data[[variable]])  
  
  p <- ggplot(data, aes(x = !!sym(variable), fill = !!sym(Group))) +
    geom_bar(position = "dodge") +  
    labs(x = x_label, y = y_label, title = plot_title, fill = as.character(Group)) +
    theme_minimal()
  
  ggplotly(p)
}



plotLinePlot <- function(data, question, plot_title, x_label, y_label){ #TODO: data_avg_CI_SE berechnen
  ci_lower <- sym(paste0(selected_var, "_ci_lower"))
  ci_upper <- sym(paste0(selected_var, "_ci_upper"))
  question_mean <- sym(paste0(selected_var, "_mean"))
  
  p <- ggplot(df, aes(x = Time, y = !!question_mean, color = Group, group = Group)) +
    geom_line(size = 1, alpha = 0.6) +
    geom_point(alpha = 0.6) +
    geom_errorbar(aes(ymin = !!ci_lower, ymax = !!ci_upper), width = 0.2, alpha = 0.6) +  # Konfidenzintervalle hinzufügen
    labs(title = plot_title,
         subtitle = "Konfidenzintervalle",
         x = x_label,
         y = y_label,
         color = "Group") +
    theme_minimal()
  
  ggplotly(p)
}


#-------------------- functions to download Report ----------------------#
#------------------------------------------------------------------------# 
generateReport <- function(file, params_list) {
  # RMarkdown Template
  tempReport <- file.path(tempdir(), "report_template.Rmd")
  file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
  
  # Params von main.R
  rmarkdown::render(tempReport, output_file = file,
                    params = params_list,
                    envir = new.env(parent = globalenv())
  )
}


#-------------------- functions data manipulation ----------------------#
#------------------------------------------------------------------------# 
# Baseline-Transformation vorbereiten
prepare_baseline <- function(df) {
  df %>%
    mutate(
      Time = case_when(
        Time == "1" ~ "baseline",
        TRUE ~ as.character(as.numeric(Time) - 1)
      ),
      Time = factor(Time, levels = c("baseline", sort(unique(Time[Time != "baseline"])))),
      Date = ymd(Date)
    )
}

# NA-Ersatz und Differenzberechnung
calculate_differences <- function(df) {
  df %>%
    select(-Date) %>%
    mutate(across(-c(ID, Group, Time), as.numeric)) %>%
    group_by(Time, Group) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), round(mean(., na.rm = TRUE)), .))) %>%
    group_by(Group, ID) %>%
    mutate(across(-c(Time), ~ . - first(.[Time == "baseline"]))) %>%
    ungroup()
}

# Zusammenfassen mit CI
calculate_summary <- function(df) {
  df %>%
    group_by(Group, Time) %>%
    summarise(
      across(-c(ID), list(
        mean = ~ round(mean(., na.rm = TRUE), 3),
        se = ~ round(sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))), 3),
        ci_lower = ~ round(mean(., na.rm = TRUE) - 
                             qt(0.975, df = sum(!is.na(.)) - 1) * sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))), 3),
        ci_upper = ~ round(mean(., na.rm = TRUE) + 
                             qt(0.975, df = sum(!is.na(.)) - 1) * sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))), 3)
      ), .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
}








