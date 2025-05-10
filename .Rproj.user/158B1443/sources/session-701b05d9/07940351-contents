simulate_transfer <- function(data,
                              condition,
                              montant,
                              var_cons,
                              var_poids = NULL,
                              return_cost = FALSE) {
  #' @description
  #' Simulates a fixed cash transfer for households that meet a given eligibility condition.
  #' Returns a vector of adjusted consumption values and optionally the total cost.
  #'
  #' @param data A household-level dataframe
  #' @param condition A logical expression (quoted), e.g. quote(region == "Rural" & nb_children < 5)
  #' @param montant Numeric. Fixed amount of the transfer (e.g., 100000 FCFA)
  #' @param var_cons Name of the initial consumption variable (e.g., "pcexp")
  #' @param var_poids (Optional) Name of the household weight variable
  #' @param return_cost Logical. If TRUE, returns a list with simulated consumption and total weighted cost
  #'
  #' @return If return_cost = FALSE: a numeric vector of adjusted consumption.  
  #'         If return_cost = TRUE: a list with:
  #'           - cons_sim: numeric vector (simulated consumption),
  #'           - cost: numeric (total cost of the scenario)
  
  # Validation
  if (!var_cons %in% names(data)) stop("Consumption variable not found in data.")
  if (!is.numeric(montant) || montant <= 0) stop("Transfer amount must be a positive number.")
  
  # Evaluate eligibility condition
  eligibles <- eval(condition, envir = data)
  if (!is.logical(eligibles)) stop("Condition must return a logical vector.")
  
  # Simulated consumption
  new_cons <- data[[var_cons]] + ifelse(eligibles, montant, 0)
  
  # Optional cost calculation
  if (return_cost) {
    if (is.null(var_poids)) stop("Weight variable must be specified to compute total cost.")
    if (!var_poids %in% names(data)) stop("Weight variable not found in data.")
    
    weights <- data[[var_poids]]
    total_cost <- sum(ifelse(eligibles, montant * weights, 0), na.rm = TRUE)
    
    return(list(cons_sim = new_cons, cost = total_cost))
  }
  
  return(new_cons)
}
