poverty_summary <- function(data, separateur = NULL, params = list()) {
  #' @description
  #' Computes a summary of poverty indicators using three sub-functions:
  #' poverty_measures(), poverty_distribution(), and poverty_composition().
  #'
  #' @param data A data.frame containing the welfare and weight variables.
  #' @param separateur A character vector of grouping variables (optional).
  #' @param params A list with variable names:
  #'   - var_cons: consumption/expenditure variable
  #'   - var_poids: weight variable
  #'   - var_seuil: poverty line (variable or numeric)
  #'
  #' @return A named list with:
  #'   - poverty_measures: FGT indicators (headcount, gap, severity)
  #'   - poverty_distribution: structure of poverty (share of poor/pop)
  #'   - poverty_composition: contribution of each group to national poverty
  
  # Appel des fonctions de base
  measures     <- poverty_measures(data, separateur, params)
  distribution <- poverty_distribution(data, separateur, params)
  composition  <- poverty_composition(data, separateur, params)
  
  # Regroupement des résultats
  results <- list(
    poverty_measures = measures,
    poverty_distribution = distribution,
    poverty_composition = composition
  )
  
  return(results)
}

library("haven")
welfare_test <- read_dta("data_final_2023.dta")
summary <- poverty_summary(data = welfare_test,
                           separateur = c("milieu"),
                           params = list(var_cons = "pcexp", var_poids = "hhweight", var_seuil = "zref"))

