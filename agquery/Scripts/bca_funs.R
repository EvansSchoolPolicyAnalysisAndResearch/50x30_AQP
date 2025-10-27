# Economic Research Model Functions
# core generators + simulator

q_units <- c("Metric tons"="mt")
p_units <- c("USD"="usd")

################################################################################
# SOME FUNCTIONS FOR UI 
################################################################################

create_info_tooltip <- function(text, tooltip_text, bold = FALSE) {
  label <- if (isTRUE(bold)) tags$strong(text) else tags$span(text, style = "font-weight: normal;")
  tags$span(
    label,
    tags$span(
      "ⓘ",
      title = tooltip_text,
      style = "cursor: help; color: #6c757d; margin-left: 4px;"
    )
  )
}



create_param_table <- function() {
  tags$table(
    border = 0, cellpadding = 20,
    tags$thead(
      tags$tr(tags$th(style = "font-weight: normal;", "Variable"), 
              tags$th(style = "text-align:center; width:80px; font-weight: normal;", "Min"), 
              tags$th(style = "text-align:center; width:80px; font-weight: normal;", "Max"))
    ),
    tags$tbody(
      tags$tr(
        tags$td(create_info_tooltip("Producer Q*", "Initial baseline quantity for producers")),
        tags$td(numericInput("q_production_min", label = NULL, value = 10000, step = 0.1, width = "80px")),
        tags$td(numericInput("q_production_max", label = NULL, value = 10000, step = 0.1, width = "80px"))
      ),
      tags$tr(
        tags$td(create_info_tooltip("Producer P*", "Initial baseline price for producers")),
        tags$td(numericInput("p_production_min", label = NULL, value = 100, step = 0.1, width = "80px")),
        tags$td(numericInput("p_production_max", label = NULL, value = 100, step = 0.1, width = "80px"))
      ),
      tags$tr(
        tags$td(create_info_tooltip("Consumer Q*", "Initial baseline quantity for consumers")),
        tags$td(numericInput("q_consumption_min", label = NULL, value = 10000, step = 0.1, width = "80px")),
        tags$td(numericInput("q_consumption_max", label = NULL, value = 10000, step = 0.1, width = "80px"))
      ),
      tags$tr(
        tags$td(create_info_tooltip("Consumer P*", "Initial baseline price for consumers")),
        tags$td(numericInput("p_consumption_min", label = NULL, value = 100, step = 0.1, width = "80px")),
        tags$td(numericInput("p_consumption_max", label = NULL, value = 100, step = 0.1, width = "80px"))
      )
    )
  )
}


################################################################################
# Generate Regional Parameters From Simulation Configuration Input 
###############################################################################
generate_regional_parameters <- function(n_regions=1,  # Left column 
                                         seed = 123,  
                                         q_consumption_range = c(1, 1), # Left column 
                                         p_consumption_range = c(1, 1), # Left column
                                         q_production_range  = c(1, 1), # Left column
                                         p_production_range  = c(1, 1), # Left column
                                         
                                         supply_price_elasticity_range = c(0.45, 0.55), # Center column
                                         demand_price_elasticity_range = c(-1.20, -0.80), # Center column
                                         income_elasticity_range = c(0.45, 0.55), # Center column
                                         output_growth_range = c(0, 0),  # Center column
                                         income_growth_range = c(0, 0),  # Center column
                                         pop_growth_range    = c(0, 0),  # Center column
                                         
                                         prob_success_range = c(0.5, 0.8),  # Right column
                                         #cost_saving = 0.2,                 # Right column
                                         #adoption_ceiling = 0.3,            # Right column
                                         tax_consumption_range = c(0, 0), # Right column
                                         tax_production_range  = c(0, 0), # Right column
                                         #discount_rate = 0.03,                   # Right column
                                         manual_qp_data = NULL) { # Left column 
  set.seed(seed)
  
  if (!is.null(manual_qp_data)) {
    q_consumption <- manual_qp_data$q_consumption[1:n_regions]
    p_consumption <- manual_qp_data$p_consumption[1:n_regions]
    q_production  <- manual_qp_data$q_production[1:n_regions]
    p_production  <- manual_qp_data$p_production[1:n_regions]
  } else {
    q_consumption <- runif(n_regions, q_consumption_range[1], q_consumption_range[2])
    p_consumption <- runif(n_regions, p_consumption_range[1], p_consumption_range[2])
    q_production  <- runif(n_regions, q_production_range[1],  q_production_range[2])
    p_production  <- runif(n_regions, p_production_range[1],  p_production_range[2])
  }
  
  # Elasticities
  e_consumption <- runif(n_regions, demand_price_elasticity_range[1], demand_price_elasticity_range[2])
  e_production  <- runif(n_regions, supply_price_elasticity_range[1], supply_price_elasticity_range[2])
  income_elasticity <- runif(n_regions, income_elasticity_range[1], income_elasticity_range[2])
  
  # Growth rates 
  pop_growth_rate   <- runif(n_regions, pop_growth_range[1], pop_growth_range[2])
  income_growth_rate<- runif(n_regions, income_growth_range[1], income_growth_range[2])
  pi_production     <- runif(n_regions, output_growth_range[1], output_growth_range[2])
  
  # R&D
  prob_success     <- runif(n_regions, prob_success_range[1], prob_success_range[2])
  research_spillover <- c(1, runif(n_regions - 1, 0, 0.5)) # Assuming that the research is gull in place 1 
  
  # Taxes 
  if (tax_consumption_range[1] == tax_consumption_range[2]) {
    taxes_consumption <- rep(tax_consumption_range[1], n_regions)
  } else {
    taxes_consumption <- runif(n_regions, tax_consumption_range[1], tax_consumption_range[2])
  }
  
  if (tax_production_range[1] == tax_production_range[2]) {
    taxes_production <- rep(tax_production_range[1], n_regions)
  } else {
    taxes_production <- runif(n_regions, tax_production_range[1], tax_production_range[2])
  }
  
  # Lists 
  list(
    q_consumption = q_consumption,
    p_consumption = p_consumption,
    q_production  = q_production,
    p_production  = p_production,
    e_consumption = e_consumption,
    e_production  = e_production,
    pop_growth_rate   = pop_growth_rate,
    income_elasticity = income_elasticity,
    income_growth_rate= income_growth_rate,
    pi_production     = pi_production,
    prob_success      = prob_success,
    research_spillover= research_spillover,
    taxes_consumption = taxes_consumption,
    taxes_production  = taxes_production
  )
}
################################################################################
# Calculate Economic Model Outcomes
################################################################################

calculate_economic_outcomes <- function(regional_params,
                                        cost_saving = 0.2,
                                        adoption_ceiling = 0.3,
                                        discount_rate = 0.03,
                                        cutoffs = c(5, 10, 15, 20),
                                        adoption_shape = "linear",
                                        research_cost_lag = 0,
                                        research_cost_growth = 0) {
  # Number of regions 
  n_regions <- length(regional_params$q_consumption)
  
  # Number of periods 
  T <- cutoffs[4] + 5
  time_vector <- 1:T
  
  #######################
  # Baseline scenario without taxes and research 
  #######################
  
  # Slopes 
  slopes_consumption <- regional_params$e_consumption * regional_params$q_consumption / regional_params$p_consumption
  slopes_production  <- regional_params$e_production  * regional_params$q_production  / regional_params$p_production
  
  # Initial period intercepts 
  intercept_consumption_t0 <- (1 - regional_params$e_consumption) * regional_params$q_consumption
  intercept_production_t0  <- (1 - regional_params$e_production)  * regional_params$q_production
  
  # Growth in consumption 
  pi_consumption <- regional_params$pop_growth_rate + 
    regional_params$income_elasticity * regional_params$income_growth_rate
  
  # Intercepts for all periods in baseline scenario 
  #intercepts_consumption <- as.matrix(intercept_consumption_t0)
  #intercepts_production  <- as.matrix(intercept_production_t0)
  #for (t in 2:T) {
  #  intercepts_consumption <- cbind(
  #    intercepts_consumption, 
  #    intercepts_consumption[, t-1] + pi_consumption * regional_params$q_consumption
  #  )
  #  intercepts_production <- cbind(
  #    intercepts_production, 
  #    intercepts_production[, t-1] - regional_params$pi_production * regional_params$q_production
  #  )
  #}
  
  # Baseline intercepts for demand and supply 
  intercepts_consumption <- matrix(nrow = n_regions, ncol = T)
  intercepts_consumption[,1]<- intercept_consumption_t0
  for (t in 2:T) intercepts_consumption[,t] <- intercepts_consumption[, t-1] + pi_consumption * regional_params$q_consumption
  intercepts_production <- matrix(nrow = n_regions, ncol = T)
  intercepts_production[,1]<- intercept_production_t0
  for (t in 2:T) intercepts_production[,t] <- intercepts_production[, t-1] + regional_params$pi_production * regional_params$q_production
  
  # Global intercepts and slopes for baseline scenario 
  intercepts_consumption_global <- colSums(intercepts_consumption)
  intercepts_production_global  <- colSums(intercepts_production)
  slope_consumption_global      <- sum(slopes_consumption)
  slope_production_global       <- sum(slopes_production)
  
  #######################
  # Scenarios with research and taxes  
  #######################
  
  # Research  
  k_max <- regional_params$prob_success * cost_saving * adoption_ceiling * regional_params$p_production
  
  # Calculate adoption vector based on shape
  a_vector <- rep(NA_real_, T)
  
  if (adoption_shape == "linear") {
    # Original linear (trapezoidal) adoption
    for (t in 1:T) {
      if (t <= cutoffs[1])       a_vector[t] <- 0
      else if (t <= cutoffs[2])  a_vector[t] <- (t - cutoffs[1]) / (cutoffs[2] - cutoffs[1])
      else if (t <= cutoffs[3])  a_vector[t] <- 1
      else if (t <= cutoffs[4])  a_vector[t] <- (cutoffs[4] - t) / (cutoffs[4] - cutoffs[3])
      else                       a_vector[t] <- 0
    }
  } else if (adoption_shape == "sigmoidal") {
    # Sigmoidal adoption using logistic function
    # The sigmoid will be applied to the growth and decline phases
    for (t in 1:T) {
      if (t <= cutoffs[1]) {
        # Before adoption starts
        a_vector[t] <- 0
      } else if (t <= cutoffs[2]) {
        # Growth phase - use sigmoid from 0 to 1
        # Map t from [cutoffs[1], cutoffs[2]] to sigmoid input range
        phase_progress <- (t - cutoffs[1]) / (cutoffs[2] - cutoffs[1])
        # Sigmoid function: 1 / (1 + exp(-k*(x - 0.5)))
        # Using k=10 for steepness, centered at 0.5
        k <- 10
        a_vector[t] <- 1 / (1 + exp(-k * (phase_progress - 0.5)))
      } else if (t <= cutoffs[3]) {
        # Maximum adoption phase
        a_vector[t] <- 1
      } else if (t <= cutoffs[4]) {
        # Decline phase - use inverted sigmoid from 1 to 0
        phase_progress <- (t - cutoffs[3]) / (cutoffs[4] - cutoffs[3])
        k <- 10
        a_vector[t] <- 1 - (1 / (1 + exp(-k * (phase_progress - 0.5))))
      } else {
        # After adoption ends
        a_vector[t] <- 0
      }
    }
  }
  
  k_matrix_spillovers <- matrix(nrow = n_regions, ncol = T)
  for (r in 1:n_regions) k_matrix_spillovers[r, ] <- regional_params$research_spillover[r] * a_vector * k_max[r]
  
  # Production intercepts with research 
  intercepts_production_research <- matrix(nrow = n_regions, ncol = T)
  for (t in 1:T) intercepts_production_research[, t] <- intercepts_production[, t] + k_matrix_spillovers[, t] * slopes_production
  intercepts_production_global_research  <- colSums(intercepts_production_research)
  
  # Tax adjustment - weighted by slopes as per DREAM equation 10
  tax_adjustment_consumption <- sum(regional_params$taxes_consumption * slopes_consumption)
  tax_adjustment_production <- sum(regional_params$taxes_production * slopes_production)
  tax_adjustment <- tax_adjustment_consumption + tax_adjustment_production
  
  # Prices and quantities with taxes 
  prices_taxes <- (intercepts_consumption_global + tax_adjustment - intercepts_production_global) / 
    (slope_production_global - slope_consumption_global)
  prices_producers_taxes <- matrix(rep(prices_taxes, n_regions), nrow = n_regions, ncol = T, byrow = TRUE) + 
    matrix(rep(regional_params$taxes_production, T), nrow = n_regions, ncol = T)
  prices_consumers_taxes <- matrix(rep(prices_taxes, n_regions), nrow = n_regions, ncol = T, byrow = TRUE) + 
    matrix(rep(regional_params$taxes_consumption, T), nrow = n_regions, ncol = T)
  quantities_producers_taxes <- intercepts_production + 
    matrix(rep(slopes_production, T), nrow = n_regions, ncol = T) * prices_producers_taxes
  quantities_consumers_taxes <- intercepts_consumption + 
    matrix(rep(slopes_consumption, T), nrow = n_regions, ncol = T) * prices_consumers_taxes
  
  # Quantities and prices with taxes and research 
  prices_taxes_research <- (intercepts_consumption_global + tax_adjustment - intercepts_production_global_research) / 
    (slope_production_global - slope_consumption_global)
  prices_producers_taxes_research <- matrix(rep(prices_taxes_research, n_regions), nrow = n_regions, ncol = T, byrow = TRUE) + 
    matrix(rep(regional_params$taxes_production, T), nrow = n_regions, ncol = T)
  prices_consumers_taxes_research <- matrix(rep(prices_taxes_research, n_regions), nrow = n_regions, ncol = T, byrow = TRUE) + 
    matrix(rep(regional_params$taxes_consumption, T), nrow = n_regions, ncol = T)
  quantities_producers_taxes_research <- intercepts_production_research + 
    matrix(rep(slopes_production, T), nrow = n_regions, ncol = T) * prices_producers_taxes_research
  quantities_consumers_taxes_research <- intercepts_consumption + 
    matrix(rep(slopes_consumption, T), nrow = n_regions, ncol = T) * prices_consumers_taxes_research
  
  # Research surplus 
  surplus_producers_taxes <- (k_matrix_spillovers + prices_producers_taxes_research - prices_producers_taxes) * 
    (0.5 * quantities_producers_taxes_research + 0.5 * quantities_producers_taxes)
  surplus_consumers_taxes <- (prices_consumers_taxes - prices_consumers_taxes_research) * 
    (0.5 * quantities_consumers_taxes_research + 0.5 * quantities_consumers_taxes)
  surplus_governments_taxes <- matrix(rep(regional_params$taxes_consumption, T), ncol = T) * 
    (quantities_consumers_taxes_research - quantities_consumers_taxes) + 
    matrix(rep(regional_params$taxes_production, T), ncol = T) * 
    (quantities_producers_taxes_research - quantities_producers_taxes)
  
  # Net present value 
  npv_discount_vector <- 1 / (1 + discount_rate)^(0:(T-1))
  npv_producers  <- rowSums(matrix(rep(npv_discount_vector, n_regions), ncol = T, byrow = TRUE) * surplus_producers_taxes)
  npv_consumers  <- rowSums(matrix(rep(npv_discount_vector, n_regions), ncol = T, byrow = TRUE) * surplus_consumers_taxes)
  npv_governments<- rowSums(matrix(rep(npv_discount_vector, n_regions), ncol = T, byrow = TRUE) * surplus_governments_taxes)
  
  # Research costs (negative values)
  research_costs <- rep(0, T)
  for (t in 1:T) {
    if (t <= cutoffs[1]) {
      # During lag period
      research_costs[t] <- -research_cost_lag
    } else if (t <= cutoffs[2]) {
      # During growth period
      research_costs[t] <- -research_cost_growth
    }
    # After growth period, no more costs
  }
  
  # NPV of research costs (per region basis - divide by n_regions)
  npv_research <- sum(npv_discount_vector * research_costs) / n_regions
  npv_research_vector <- rep(npv_research, n_regions)
  
  
  regional_params_df <- data.frame(
    Region = 1:n_regions,
    Pc0 = round(prices_consumers_taxes[, 1], 3),
    Pp0 = round(prices_producers_taxes[, 1], 3),
    Qc0 = round(quantities_consumers_taxes[, 1], 3),
    Qp0 = round(quantities_producers_taxes[, 1], 3),
    Pop_Growth        = round(regional_params$pop_growth_rate, 4),
    Income_Elasticity = round(regional_params$income_elasticity, 3),
    Income_Growth     = round(regional_params$income_growth_rate, 4),
    Prob_Success      = round(regional_params$prob_success, 3),
    Research_Spillover= round(regional_params$research_spillover, 3),
    Tax_Consumption   = round(regional_params$taxes_consumption, 4),
    Tax_Production    = round(regional_params$taxes_production, 4)
  )
  
  list(
    time_vector = time_vector,
    prices = prices_taxes,
    prices_research = prices_taxes_research,
    surplus_producers   = surplus_producers_taxes,
    surplus_consumers   = surplus_consumers_taxes,
    surplus_governments = surplus_governments_taxes,
    research_costs = research_costs,
    npv_producers  = npv_producers,
    npv_consumers  = npv_consumers,
    npv_governments= npv_governments,
    npv_research   = npv_research_vector,
    regional_params_df = regional_params_df,
    k_matrix_spillovers = k_matrix_spillovers,
    a_vector = a_vector,
    n_regions = n_regions,
    cost_saving = cost_saving,
    adoption_ceiling = adoption_ceiling,
    discount_rate = discount_rate,
    cutoffs = cutoffs,
    adoption_shape = adoption_shape,
    research_cost_lag = research_cost_lag,
    research_cost_growth = research_cost_growth
  )
}

#' Wrapper to run the full simulation
run_economic_simulation <- function(n_regions = 1,
                                    seed = 123,
                                    cost_saving = 0.2,
                                    adoption_ceiling = 0.3,
                                    discount_rate = 0.03,
                                    cutoffs = c(5, 10, 15, 20),
                                    adoption_shape = "linear",
                                    research_cost_lag = 0,
                                    research_cost_growth = 0,
                                    ...) {
  regional_params <- generate_regional_parameters(n_regions = n_regions, seed = seed, ...)
  calculate_economic_outcomes(
    regional_params = regional_params,
    cost_saving = cost_saving,
    adoption_ceiling = adoption_ceiling,
    discount_rate = discount_rate,
    cutoffs = cutoffs,
    adoption_shape = adoption_shape,
    research_cost_lag = research_cost_lag,
    research_cost_growth = research_cost_growth
  )
}