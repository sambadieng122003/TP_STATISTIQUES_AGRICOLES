plot_poverty_measures <- function(result, indicator = "headcount", save = FALSE, filename = "poverty_plot.png") {
  #' @description
  #' Visualizes poverty indicators (headcount, gap, severity) from `poverty_measures()`.
  #' The function generates a horizontal bar plot with values displayed on bars.
  #' Optionally, the plot can be saved to a PNG file.
  #'
  #' @param result A data.frame returned by `poverty_measures()`
  #' @param indicator A character string: "headcount", "gap", or "severity"
  #' @param save Logical. If TRUE, saves the plot as a PNG file.
  #' @param filename Character. Name of the file if `save = TRUE`
  #'
  #' @return A ggplot object showing the selected poverty measure.
  
  library(ggplot2)
  library(dplyr)
  library(rlang)
  
  # Vérification de l'indicateur choisi
  if (!indicator %in% c("headcount", "gap", "severity")) {
    stop("L'indicateur doit être 'headcount', 'gap' ou 'severity'")
  }
  
  # Sélection de la colonne correspondant à l'indicateur choisi
  indicator_column <- switch(indicator,
                             "headcount" = "poverty_headcount",
                             "gap" = "poverty_gap",
                             "severity" = "poverty_severity")
  
  # Vérification de l'existence de la colonne
  if (!indicator_column %in% names(result)) {
    stop(paste("La colonne", indicator_column, "n'existe pas dans les résultats fournis."))
  }
  
  # Supprimer la ligne "Total" s'il y en a une
  if ("Total" %in% result[[1]]) {
    result <- result %>% filter(.[[1]] != "Total")
  }
  
  # Création du graphique avec ggplot2
  plot <- ggplot(result, aes(x = reorder(!!sym(names(result)[1]), !!sym(indicator_column)),
                             y = !!sym(indicator_column), fill = !!sym(indicator_column))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(!!sym(indicator_column), 1)),
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(title = paste("Poverty", indicator, "by group"),
         x = names(result)[1], y = paste(indicator, "(%)")) +
    theme_minimal() +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    theme(legend.position = "none") +
    ylim(0, max(result[[indicator_column]], na.rm = TRUE) * 1.15)
  
  # Sauvegarde optionnelle du graphique
  if (save) {
    ggsave(filename, plot, width = 8, height = 6)
  }
  
  # Affichage du graphique
  print(plot)
}


result <- poverty_measures(
  data = welfare,
  separateur = c("milieu"),
  params = list(var_cons = "pcexp", var_poids = "hhweight", var_seuil = "zref")
)

plot_poverty_measures(result, indicator = "severity")

