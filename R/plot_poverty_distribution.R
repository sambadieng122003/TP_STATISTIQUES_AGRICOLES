plot_poverty_distribution <- function(result, indicator = "share_poor", save = FALSE, filename = "distribution_plot.png") {
  #' @description
  #' Visualizes poverty distribution results by group.
  #' Can display either share of poor or share of population.
  #'
  #' @param result A data.frame returned by `poverty_distribution()`
  #' @param indicator A character string: "share_poor" or "share_population"
  #' @param save Logical. If TRUE, saves the plot as a PNG file.
  #' @param filename Character. Name of the file if `save = TRUE`
  #'
  #' @return A ggplot object showing the selected distribution measure.
  
  library(ggplot2)
  library(dplyr)
  library(rlang)
  
  # Vérification de l'indicateur choisi
  if (!indicator %in% c("share_of_poor", "share_of_population")) {
    stop("L'indicateur doit être 'share_of_poor' ou 'share_of_population'")
  }
  
  # Vérification de l'existence de la colonne
  if (!indicator %in% names(result)) {
    stop(paste("La colonne", indicator, "n'existe pas dans les résultats fournis."))
  }
  
  # Supprimer la ligne "Total" si elle est présente
  if ("Total" %in% result[[1]]) {
    result <- result %>% filter(.[[1]] != "Total")
  }
  
  # Création du graphique
  plot <- ggplot(result, aes(x = reorder(!!sym(names(result)[1]), !!sym(indicator)),
                             y = !!sym(indicator), fill = !!sym(indicator))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(!!sym(indicator), 1), "%")),
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(title = paste("Poverty", gsub("_", " ", indicator), "by group"),
         x = names(result)[1], y = "Percentage (%)") +
    theme_minimal() +
    scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
    theme(legend.position = "none") +
    ylim(0, max(result[[indicator]], na.rm = TRUE) * 1.15)
  
  # Sauvegarde
  if (save) {
    ggsave(filename, plot, width = 8, height = 6)
  }
  
  print(plot)
}


distribution_result <- poverty_distribution(
  data = welfare,
  separateur = c("milieu"),
  params = list(var_cons = "pcexp", var_poids = "hhweight", var_seuil = "zref")
)

plot_poverty_distribution(distribution_result, indicator = "share_of_poor")
