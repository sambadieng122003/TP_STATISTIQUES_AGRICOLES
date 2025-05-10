#' transfers_summary
#'
#' Evaluates the poverty-reduction efficiency and cost of multiple transfer scenarios.
#'
#' @param data A data.frame containing household-level information
#' @param scenarios A list of scenarios, each being a list with: 
#'   - name (string), 
#'   - condition (quote), 
#'   - amount (numeric), 
#'   - optional id (string)
#' @param baseline Name of the baseline consumption variable (e.g. "pcexp")
#' @param separateur Vector of grouping variables (e.g. c("milieu"))
#' @param params A list with at least: var_poids, var_seuil
#' @param pib Total GDP in FCFA, for computing cost percentage
#' @param save_scenario Logical. If TRUE, saves `welfare_temp` as .dta file
#' @param path_scenario Path where to save the .dta file (default: current dir)
#' @param plot Logical. If TRUE, shows plot_efficiency_bar() (default = TRUE)
#' @param plot_advanced Logical. If TRUE, also shows plot_efficiency() scatterplot
#'
#' @return A list with:
#'   - baseline indicators,
#'   - scenario results (indicators, deltas, cost, cost_pib, efficiency),
#'   - comparison_table,
#'   - plot (optional)
#'
#' @export
transfers_summary <- function(data,
                              scenarios,
                              baseline,
                              params,
                              pib,
                              save_scenario = FALSE,
                              path_scenario = ".",
                              plot = TRUE,
                              plot_advanced = FALSE) {
  library(dplyr)
  library(haven)
  library(ggplot2)
  
  # Créer une version nettoyée de la base : toutes les variables labelled → factor
  data_clean <- data %>%
    mutate(across(where(haven::is.labelled), haven::as_factor))
  
  welfare_temp <- data_clean
  summary_table <- data.frame()
  
  poverty_headcount <- function(cons, poids, seuil) {
    cons <- as.numeric(cons)
    poids <- as.numeric(poids)
    seuil <- as.numeric(seuil)
    H <- mean((cons < seuil) * poids, na.rm = TRUE) / mean(poids, na.rm = TRUE)
    return(H)
  }
  
  H0 <- poverty_headcount(
    cons = data_clean[[baseline]],
    poids = data_clean[[params$var_poids]],
    seuil = data_clean[[params$var_seuil]]
  )
  
  if (is.null(names(scenarios)) || all(names(scenarios) == "")) {
    names(scenarios) <- paste0("s", seq_along(scenarios))
  }
  
  for (i in seq_along(scenarios)) {
    sc <- scenarios[[i]]
    id <- names(scenarios)[i]
    
    if (is.null(sc$amount)) stop(paste0("Montant manquant pour le scénario '", id, "'"))
    amount <- sc$amount
    
    # Appliquer la condition sur la base nettoyée
    eligible <- eval(sc$condition, envir = data_clean)
    
    # Simulation de la consommation
    cons_col <- paste0("cons_sim_", id)
    welfare_temp[[cons_col]] <- as.numeric(data_clean[[baseline]]) + ifelse(eligible, amount, 0)
    
    # Nouveau headcount
    H1 <- poverty_headcount(
      cons = welfare_temp[[cons_col]],
      poids = data_clean[[params$var_poids]],
      seuil = data_clean[[params$var_seuil]]
    )
    
    delta <- as.numeric(H0 - H1)
    cost_total <- sum(ifelse(eligible, amount, 0) * as.numeric(data_clean[[params$var_poids]]), na.rm = TRUE)
    cost_col <- paste0("cost_", id)
    welfare_temp[[cost_col]] <- ifelse(eligible, amount, 0)
    
    efficiency <- ifelse(cost_total == 0, NA, delta / (cost_total / pib))
    
    # Pourcentages
    headcount_pct <- H1 * 100
    delta_pct <- delta * 100
    cost_pct_pib <- (cost_total / pib) * 100
    
    res <- data.frame(
      id = id,
      scenario = if (!is.null(sc$name)) sc$name else id,
      amount = amount,
      headcount = headcount_pct,
      delta_poverty = delta_pct,
      cost = cost_total,
      cost_perc_pib = cost_pct_pib,
      efficiency = efficiency
    )
    
    summary_table <- bind_rows(summary_table, res)
  }
  
  if (save_scenario) {
    save_path <- file.path(path_scenario, "welfare_temp.dta")
    write_dta(welfare_temp, save_path)
  }
  
  if (plot) {
    p <- ggplot(summary_table, aes(x = id, y = efficiency)) +
      geom_col(fill = "steelblue") +
      labs(title = "Efficacité des scénarios",
           x = "Scénario",
           y = "Réduction du taux de pauvreté par % du PIB") +
      theme_minimal()
    print(p)
  }
  
  return(summary_table)
}


scenarios <- list(
  list(
    name = "Universal Transfer",
    condition = quote(TRUE),
    amount = 100000
  ),
  list(
    name = "Rural households",
    condition = quote(milieu == "Rural"),
    amount = 100000
  ),
  list(
    name = "Rural + child under 5",
    condition = quote(milieu == "Rural" & hage < 20),
    amount = 100000
  ),
  list(
    name = "Rural + elder",
    condition = quote(milieu == "Rural" & hage > 65),
    amount = 100000
  ),
  list(
    name = "disability",
    condition = quote(hhandig=="Oui"),
    amount = 100000
  )
)


# Appel de la fonction principale
results_transfer <- transfers_summary(
  data = welfare,
  scenarios = scenarios,
  baseline = "pcexp",
  params = list(var_poids = "hhweight", var_seuil = "zref"),
  pib = 7000000000000,
  save_scenario = TRUE,
  path_scenario = ".",
  plot = TRUE,
  plot_advanced = FALSE
)


