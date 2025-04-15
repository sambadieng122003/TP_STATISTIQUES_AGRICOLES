export_poverty_summary <- function(summary_list, 
                                   plots = list(), 
                                   file = "poverty_summary_report.xlsx") {
  #' @description
  #' Export poverty summary to an Excel report with a table of contents, tables and plots.
  #'
  #' @param summary_list Result from `poverty_summary()`
  #' @param plots Named list of ggplot objects for each output (optional)
  #' @param file Output Excel file path
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  
  library(openxlsx)
  library(ggplot2)
  
  wb <- createWorkbook()
  
  ### 1. Table des matières ----
  addWorksheet(wb, "Table of Contents")
  toc <- data.frame(
    Title = c(
      "Poverty Measures (Table)",
      "Poverty Measures (Plot)",
      "Poverty Distribution (Table)",
      "Poverty Distribution (Plot)",
      "Poverty Composition (Table)",
      "Poverty Composition (Plot)"
    ),
    Sheet = c(
      "Poverty_Measures",
      "Plot_Measures",
      "Poverty_Distribution",
      "Plot_Distribution",
      "Poverty_Composition",
      "Plot_Composition"
    )
  )
  
  writeData(wb, "Table of Contents", toc, startCol = 1, startRow = 1)
  for (i in 1:nrow(toc)) {
    writeFormula(wb, sheet = "Table of Contents", 
                 x = makeHyperlinkString(toc$Sheet[i], row = 1, col = 1),
                 startRow = i + 1, startCol = 3)
  }
  
  ### 2. Ajout des tableaux ----
  add_table_sheet <- function(sheet_name, data, title) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, x = title, startRow = 1, startCol = 1)
    addStyle(wb, sheet_name, createStyle(textDecoration = "bold", fontSize = 14), rows = 1, cols = 1)
    writeDataTable(wb, sheet = sheet_name, x = data, startRow = 3, withFilter = TRUE)
  }
  
  add_table_sheet("Poverty_Measures", summary_list$poverty_measures, "FGT Poverty Measures by Group")
  add_table_sheet("Poverty_Distribution", summary_list$poverty_distribution, "Poverty Distribution")
  add_table_sheet("Poverty_Composition", summary_list$poverty_composition, "Contribution to National Poverty")
  
  ### 3. Ajout des graphiques ----
  add_plot_sheet <- function(sheet_name, plot_obj, title, file_name) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, x = title, startRow = 1, startCol = 1)
    addStyle(wb, sheet_name, createStyle(textDecoration = "bold", fontSize = 14), rows = 1, cols = 1)
    
    # Sauvegarde temporaire du plot en PNG
    tmp <- tempfile(fileext = ".png")
    ggsave(tmp, plot_obj, width = 8, height = 5)
    
    insertImage(wb, sheet = sheet_name, file = tmp, startRow = 3, startCol = 1, width = 8, height = 5, units = "in")
  }
  
  # Vérifie les noms dans la liste de plots et les insère
  if (length(plots) > 0) {
    if (!is.null(plots$poverty_measures)) {
      add_plot_sheet("Plot_Measures", plots$poverty_measures, "Poverty Measures - Plot", "plot_measures.png")
    }
    if (!is.null(plots$poverty_distribution)) {
      add_plot_sheet("Plot_Distribution", plots$poverty_distribution, "Poverty Distribution - Plot", "plot_distribution.png")
    }
    if (!is.null(plots$poverty_composition)) {
      add_plot_sheet("Plot_Composition", plots$poverty_composition, "Poverty Composition - Plot", "plot_composition.png")
    }
  }
  
  ### 4. Sauvegarde finale ----
  saveWorkbook(wb, file, overwrite = TRUE)
  message("✅ Exported poverty report to: ", normalizePath(file))
}

# Résultat de poverty_summary()
summary <- poverty_summary(
  data = welfare,
  separateur = c("region"),
  params = list(var_cons = "pcexp", var_poids = "hhweight", var_seuil = "zref")
)

# Graphiques associés
plots <- list(
  poverty_measures = plot_poverty_measures(summary$poverty_measures, indicator = "headcount"),
  poverty_distribution = plot_poverty_distribution(summary$poverty_distribution, indicator = "share_of_poor"),
  poverty_composition = plot_poverty_composition(summary$poverty_composition, indicator = "contribution_to_headcount")
)

# Export Excel complet
export_poverty_summary(summary, plots = plots, file = "poverty_report.xlsx")



