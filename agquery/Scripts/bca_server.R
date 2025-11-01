bca_server <- function(input, output, session) {
  
  # 1 DISCOVER ALLOWABLE ARGS FROM PARAMETER GENERATION AND SIMULATION RUNNING FUNCTIONS 
  formal_sim <- setdiff(names(formals(run_economic_simulation)), "...")
  formal_gen <- setdiff(names(formals(generate_regional_parameters)), "...")
  allowed_param_names <- union(formal_sim, formal_gen)
  
  # 2 REACTIVE VALUES FOR STORING SIMULATIONS
  simulations <- reactiveValues(
    counter = 0,
    data = list(),  # List to store all simulation results
    latest_sim_id = NULL  # Track most recent simulation for default tab selection
  )
  
  button_states <- reactiveValues(simulation_running = FALSE)
  
  # Empirical/Open economy simulations storage
  empirical_simulations <- reactiveValues(
    counter = 0,
    data = list(),  # List to store all open economy simulation results
    latest_sim_id = NULL  # Track most recent simulation for default tab selection
  )
  
  empirical_button_states <- reactiveValues(simulation_running = FALSE)
  
  # Market data storage - Based on DREAM model Table C.1
  market_data <- reactiveValues(
    df = data.frame(
      Region = character(),
      Supply_Q = numeric(),
      Demand_C = numeric(),
      Consumer_Price = numeric(),
      Producer_Price = numeric(),
      Supply_Elasticity = numeric(),
      Demand_Elasticity = numeric(),
      Income_Elasticity = numeric(),
      Pop_Growth = numeric(),
      Income_Growth = numeric(),
      Production_Growth = numeric(),
      Tax_Consumption = numeric(),
      Tax_Production = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  # Spillover data storage
  spillover_data <- reactiveValues(
    df = data.frame(
      Region = character(),
      Spillover_theta = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  # Reset empirical state
  reset_empirical_state <- function() {
    empirical_button_states$simulation_running <- FALSE
  }
  
  reset_empirical_state()
  
  # 3 CONTROL VISIBILITY OF RESULTS PANEL - see condition in UI Results panel 
  output$show_results <- reactive(length(simulations$data) > 0)
  outputOptions(output, "show_results", suspendWhenHidden = FALSE)
  
  output$show_empirical_results <- reactive(length(empirical_simulations$data) > 0)
  outputOptions(output, "show_empirical_results", suspendWhenHidden = FALSE)
  
  #-----------------------------------------------------------------------------
  # EMPIRICAL DATA TAB - MARKET DATA TABLE (DREAM Model Structure)
  #-----------------------------------------------------------------------------
  
  # Add market row with default values from closed economy tab
  observeEvent(input$add_market_row, {
    new_row <- data.frame(
      Region = paste0("Region_", nrow(market_data$df) + 1),
      Supply_Q = input$quantity,      # From closed economy
      Demand_C = input$quantity,       # From closed economy
      Consumer_Price = input$price,    # From closed economy
      Producer_Price = input$price,    # From closed economy
      Supply_Elasticity = input$supply_price_elasticity_range[1] / 100,   # From closed economy
      Demand_Elasticity = input$demand_price_elasticity_range[1] / 100,  # From closed economy
      Income_Elasticity = input$income_elasticity_range[1] / 100,  # From closed economy
      Pop_Growth = input$pop_growth_range[1] / 100,  # From closed economy
      Income_Growth = input$income_growth_range[1] / 100,  # From closed economy
      Production_Growth = input$output_growth_range[1] / 100,  # From closed economy
      Tax_Consumption = 0,  # Default to 0
      Tax_Production = 0,   # Default to 0
      stringsAsFactors = FALSE
    )
    market_data$df <- rbind(market_data$df, new_row)
    
    # Update spillover table
    spillover_new_row <- data.frame(
      Region = new_row$Region,
      Spillover_theta = ifelse(nrow(market_data$df) == 1, 1.0, 0.0),
      stringsAsFactors = FALSE
    )
    spillover_data$df <- rbind(spillover_data$df, spillover_new_row)
  })
  
  # Remove market row
  observeEvent(input$remove_market_row, {
    if (nrow(market_data$df) > 0) {
      market_data$df <- market_data$df[-nrow(market_data$df), , drop = FALSE]
      spillover_data$df <- spillover_data$df[-nrow(spillover_data$df), , drop = FALSE]
    }
  })
  
  # Render market data table with column names matching DREAM model
  output$market_data_table <- renderDT({
    datatable(
      market_data$df,
      editable = list(target = "cell", disable = list(columns = c())),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 't',
        language = list(
          emptyTable = "Please add data for at least one region/country using the 'Add Market' button above."
        ),
        columnDefs = list(
          list(width = '120px', targets = 0),
          list(width = '100px', targets = 1:12)
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe',
      selection = 'none',
      colnames = c(
        'Market/Region/Country' = 'Region',
        'Producer Quantity' = 'Supply_Q',
        'Consumer Quantity' = 'Demand_C',
        'Consumer Price' = 'Consumer_Price',
        'Producer Price' = 'Producer_Price',
        'Price Elasticity of Supply' = 'Supply_Elasticity',
        'Price Elasticity of Demand' = 'Demand_Elasticity',
        'Income Elasticity of Demand' = 'Income_Elasticity',
        'Annual Population Growth Rate' = 'Pop_Growth',
        'Annual Income Growth Rate' = 'Income_Growth',
        'Annual Production Growth Rate' = 'Production_Growth',
        'Consumer Tax Rate' = 'Tax_Consumption',
        'Producer Tax Rate' = 'Tax_Production'
      )
    ) %>%
      formatRound(columns = c(2, 3), digits = 0) %>%
      formatRound(columns = c(4, 5), digits = 2) %>%
      formatRound(columns = c(6, 7, 8), digits = 2) %>%
      formatPercentage(columns = c(9, 10, 11, 12, 13), digits = 2)
  }, server = TRUE)  # Changed to TRUE for proper editing
  
  # Handle cell edits for market data
  observeEvent(input$market_data_table_cell_edit, {
    info <- input$market_data_table_cell_edit
    row <- info$row
    col <- info$col + 1  # DT uses 0-based indexing, add 1 for R
    value <- info$value
    
    # Update the data frame
    market_data$df[row, col] <- value
    
    # Update region name in spillover table if Region column was changed
    if (col == 1) {  # Region column
      spillover_data$df[row, "Region"] <- value
    }
  })
  
  #-----------------------------------------------------------------------------
  # EMPIRICAL DATA TAB - SPILLOVER TABLE
  #-----------------------------------------------------------------------------
  
  # Render spillover table
  output$spillover_table <- renderDT({
    datatable(
      spillover_data$df,
      editable = list(target = "cell", disable = list(columns = c(0))),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 't',
        language = list(
          emptyTable = "Please add data for at least one region/country using the 'Add Market' button in the Markets tab."
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe',
      selection = 'none',
      colnames = c(
        'Region/Country' = 'Region',
        'Spillover θ' = 'Spillover_theta'
      )
    ) %>%
      formatRound(columns = c(2), digits = 2)
  }, server = TRUE)  # Changed to TRUE for proper editing
  
  # Handle cell edits for spillover data
  observeEvent(input$spillover_table_cell_edit, {
    info <- input$spillover_table_cell_edit
    row <- info$row
    col <- info$col + 1  # DT uses 0-based indexing, add 1 for R
    value <- info$value
    
    # Update the data frame
    spillover_data$df[row, col] <- value
  })
  
  #-----------------------------------------------------------------------------
  # EMPIRICAL DATA TAB - NAVIGATION BUTTONS
  #-----------------------------------------------------------------------------
  
  observeEvent(input$general_next, {
    updateTabsetPanel(session, "empirical_tabs", selected = "Markets")
  })
  
  observeEvent(input$market_prev, {
    updateTabsetPanel(session, "empirical_tabs", selected = "General")
  })
  
  observeEvent(input$market_next, {
    updateTabsetPanel(session, "empirical_tabs", selected = "R&D and Adoption")
  })
  
  observeEvent(input$tech_prev, {
    updateTabsetPanel(session, "empirical_tabs", selected = "Markets")
  })
  
  observeEvent(input$tech_next, {
    updateTabsetPanel(session, "empirical_tabs", selected = "Spillover")
  })
  
  observeEvent(input$spillover_prev, {
    updateTabsetPanel(session, "empirical_tabs", selected = "R&D and Adoption")
  })  
  
  # Spillover next button now runs simulation and stores it
  observeEvent(input$spillover_next, {
    
    req(nrow(market_data$df) > 0)
    
    loading_id <- showNotification("RUNNING SIMULATION...", type = "message", duration = NULL)
    
    tryCatch({
      # Build parameters from empirical inputs matching DREAM model
      n_regions <- nrow(market_data$df)
      
      # Create regional parameters from market data (Table C.1 structure)
      regional_params <- list(
        q_consumption = market_data$df$Demand_C,
        p_consumption = market_data$df$Consumer_Price,
        q_production = market_data$df$Supply_Q,
        p_production = market_data$df$Producer_Price,
        e_consumption = market_data$df$Demand_Elasticity,
        e_production = market_data$df$Supply_Elasticity,
        pop_growth_rate = market_data$df$Pop_Growth,
        income_elasticity = market_data$df$Income_Elasticity,
        income_growth_rate = market_data$df$Income_Growth,
        pi_production = market_data$df$Production_Growth,
        prob_success = rep(input$emp_prob_success / 100, n_regions),
        research_spillover = spillover_data$df$Spillover_theta,
        taxes_consumption = market_data$df$Tax_Consumption,
        taxes_production = market_data$df$Tax_Production
      )
      
      # Calculate cutoffs from lambda parameters (trapezoidal adoption curve)
      cutoffs <- c(
        input$emp_lambda_R,
        input$emp_lambda_R + input$emp_lambda_A,
        input$emp_lambda_R + input$emp_lambda_A + input$emp_lambda_M,
        input$emp_lambda_R + input$emp_lambda_A + input$emp_lambda_M + input$emp_lambda_D
      )
      
      # Run simulation with empirical data
      sim_result <- calculate_economic_outcomes(
        regional_params = regional_params,
        cost_saving = input$emp_cost_saving / 100,
        adoption_ceiling = input$emp_adoption_ceiling / 100,
        discount_rate = input$emp_discount_rate / 100,
        cutoffs = cutoffs,
        adoption_shape = tolower(input$emp_adoption_shape),
        research_cost_lag = input$emp_research_cost_lag,
        research_cost_growth = input$emp_research_cost_growth
      )
      
      # Increment counter
      empirical_simulations$counter <- empirical_simulations$counter + 1
      sim_id <- as.character(empirical_simulations$counter)
      
      # Generate simulation name
      sim_name <- if (nchar(trimws(input$emp_simulation_name)) > 0) {
        input$emp_simulation_name
      } else {
        paste("Open Economy", empirical_simulations$counter)
      }
      
      # Store simulation inputs
      sim_inputs <- list(
        emp_q_unit = input$emp_q_unit,
        emp_p_unit = input$emp_p_unit,
        emp_year = input$emp_year,
        emp_discount_rate = input$emp_discount_rate,
        emp_prob_success = input$emp_prob_success,
        emp_cost_saving = input$emp_cost_saving,
        emp_adoption_ceiling = input$emp_adoption_ceiling,
        emp_lambda_R = input$emp_lambda_R,
        emp_lambda_A = input$emp_lambda_A,
        emp_lambda_M = input$emp_lambda_M,
        emp_lambda_D = input$emp_lambda_D,
        emp_adoption_shape = input$emp_adoption_shape,
        emp_research_cost_lag = input$emp_research_cost_lag,
        emp_research_cost_growth = input$emp_research_cost_growth,
        market_data = market_data$df,
        spillover_data = spillover_data$df
      )
      
      # Store simulation
      empirical_simulations$data[[sim_id]] <- list(
        name = sim_name,
        data = sim_result,
        inputs = sim_inputs,
        timestamp = Sys.time()
      )
      
      # Track this as the latest simulation for default tab selection
      empirical_simulations$latest_sim_id <- sim_id
      
      # Navigate to Results tab
      updateTabsetPanel(session, "empirical_tabs", selected = "Results")
      
      # Reset simulation name
      updateTextInput(session, "emp_simulation_name", value = "")
      
      removeNotification(loading_id)
      showNotification("SIMULATION COMPLETED. SEE RESULTS.", type = "message", duration = 5)
    }, error = function(e) {
      removeNotification(loading_id)
      showNotification(paste("ERROR RUNNING SIMULATION:", e$message), type = "error", duration = 10)
    })
  })
  
  observeEvent(input$results_prev, {
    updateTabsetPanel(session, "empirical_tabs", selected = "Spillover")
  })
  
  observeEvent(input$results_prev2, {
    updateTabsetPanel(session, "empirical_tabs", selected = "Spillover")
  })
  
  #-----------------------------------------------------------------------------
  # CLEAR EMPIRICAL RESULTS
  #-----------------------------------------------------------------------------
  observeEvent(input$clear_empirical_results, {
    reset_empirical_state()
    showNotification("EMPIRICAL RESULTS CLEARED. READY.", type = "message", duration = 3)
  })
  
  #-----------------------------------------------------------------------------
  # CREATE EMPIRICAL SIMULATION RESULT TAB CONTENT
  #-----------------------------------------------------------------------------
  create_emp_sim_result_content <- function(sim_id, sim_data, sim_inputs, sim_name) {
    
    p_label <- names(p_units)[match(sim_inputs$emp_p_unit, p_units)]
    q_label <- names(q_units)[match(sim_inputs$emp_q_unit, q_units)]
    
    # Calculate totals for NPV
    totals <- c(
      Research = sum(sim_data$npv_research),
      Producers = sum(sim_data$npv_producers),
      Consumers = sum(sim_data$npv_consumers),
      Government = sum(sim_data$npv_governments)
    )
    total_all <- sum(totals)
    
    tagList(
      # Header with download and delete buttons
      fluidRow(
        column(12,
               div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;",
                   h4(paste("Simulation:", sim_name), style="margin: 0;"),
                   div(style="margin-top: 10px;",
                       downloadButton(paste0("download_emp_html_", sim_id), "Download Report (HTML)", class = "btn-primary btn-lg"),
                       actionButton(paste0("delete_emp_sim_", sim_id), HTML("&times; Delete"), class = "btn-secondary btn-lg", 
                                    style="margin-left: 10px;")
                   )
               )
        )
      ),
      
      hr(),
      
      # PARAMETERS SECTION
      fluidRow(
        column(12,
               h4("Model Parameters"),
               card(
                 card_body(
                   fluidRow(
                     column(6,
                            tags$h5("General Parameters", class = "param-section-title", style = "margin-top: 0;"),
                            tags$table(class = "table table-sm table-borderless param-table", style = "margin-bottom: 25px;",
                                       tags$tr(tags$td("Initial Year:"), tags$td(sprintf("%d", sim_inputs$emp_year))),
                                       tags$tr(tags$td("Number of Markets:"), tags$td(sprintf("%d", sim_data$n_regions))),
                                       tags$tr(tags$td("Number of Periods:"), tags$td(sprintf("%d", length(sim_data$time_vector)))),
                                       tags$tr(tags$td("Quantity Unit:"), tags$td(q_label)),
                                       tags$tr(tags$td("Price Unit:"), tags$td(p_label)),
                                       tags$tr(tags$td("NPV Discount Rate:"), tags$td(sprintf("%.1f%%", sim_data$discount_rate * 100)))
                            )
                     ),
                     
                     column(6,
                            tags$h5("R&D Parameters", class = "param-section-title"),
                            tags$table(class = "table table-sm table-borderless param-table", style = "margin-bottom: 0;",
                                       tags$tr(tags$td("Probability of Success:"), tags$td(sprintf("%.1f%%", sim_inputs$emp_prob_success))),
                                       tags$tr(tags$td("Cost Saving:"), tags$td(sprintf("%.1f%%", sim_data$cost_saving * 100))),
                                       tags$tr(tags$td("Adoption Ceiling:"), tags$td(sprintf("%.1f%%", sim_data$adoption_ceiling * 100))),
                                       tags$tr(tags$td("Adoption Shape:"), tags$td(tools::toTitleCase(sim_data$adoption_shape))),
                                       tags$tr(tags$td("Research Cutoff Periods:"), tags$td(paste(sim_data$cutoffs, collapse = ", "))),
                                       tags$tr(tags$td("Research Cost before Adoption:"), tags$td(sprintf("%.0f %s", sim_inputs$emp_research_cost_lag, p_label))),
                                       tags$tr(tags$td("Research Cost during Adoption Growth:"), tags$td(sprintf("%.0f %s", sim_inputs$emp_research_cost_growth, p_label)))
                            )
                     )
                   )
                 )
               )
        )
      ),
      
      hr(),
      
      # MARKETS PARAMETERS TABLE
      fluidRow(
        style = "margin-bottom: 0px;",
        column(12,
               h4("Market Parameters", style = "margin-bottom: 10px;"),
               card(
                 card_body(
                   style = "padding-bottom: 0px; margin-bottom: 0px;",
                   div(style = "margin-bottom: 0px;",
                       DTOutput(paste0("emp_market_config_table_", sim_id))
                   )
                 )
               )
        )
      ),
      
      # NPV SUMMARY (reduced top margin)
      fluidRow(
        style = "margin-top: 0px;",
        column(12,
               h4("Net Present Value Summary", style = "margin-top: 15px; margin-bottom: 10px;"),
               card(
                 card_body(
                   tags$table(class = "table table-sm table-borderless param-table", style = "width: 60%; margin: 0 auto;",
                              tags$tr(tags$td("Sector"), tags$td(paste0("NPV (", p_label, ")"))),
                              tags$tr(style = "border-top: 1px solid #dee2e6;",
                                      tags$td("Research"), tags$td(sprintf("%.2f", totals[1]))
                              ),
                              tags$tr(tags$td("Producers"), tags$td(sprintf("%.2f", totals[2]))),
                              tags$tr(tags$td("Consumers"), tags$td(sprintf("%.2f", totals[3]))),
                              tags$tr(tags$td("Government"), tags$td(sprintf("%.2f", totals[4]))),
                              tags$tr(style = "border-top: 1px solid #dee2e6;",
                                      tags$td("Total"), 
                                      tags$td(sprintf("%.2f", total_all))
                              )
                   )
                 )
               )
        )
      ),
      
      hr(),
      
      # NPV SURPLUS BAR CHART
      h4("NPV Surplus by Market"),
      fluidRow(
        column(12,
               plotlyOutput(paste0("emp_regional_surplus_plot_", sim_id), height = "400px")
        )
      ),
      
      hr(),
      
      # DYNAMIC PLOTS
      h4("Outcomes over Time"),
      fluidRow(
        column(12,
               plotlyOutput(paste0("emp_price_dynamics_plot_", sim_id), height = "400px")
        )
      ),
      
      fluidRow(
        column(12,
               plotlyOutput(paste0("emp_surplus_time_plot_", sim_id), height = "400px")
        )
      ),
      
      fluidRow(
        column(12,
               plotlyOutput(paste0("emp_cumulative_surplus_plot_", sim_id), height = "400px")
        )
      )
    )
  }
  #-----------------------------------------------------------------------------
  # GENERATE EMPIRICAL PLOTS
  #-----------------------------------------------------------------------------
  generate_empirical_plots <- function(sim_id, sim_data, sim_inputs) {
    
    p_label <- names(p_units)[match(sim_inputs$emp_p_unit, p_units)]
    
    # Market configuration table with spillover
    output[[paste0("emp_market_config_table_", sim_id)]] <- renderDT({
      # Merge market data with spillover data
      merged_df <- merge(sim_inputs$market_data, sim_inputs$spillover_data, by = "Region")
      
      datatable(
        merged_df,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 't',
          columnDefs = list(
            list(width = '120px', targets = 0),
            list(width = '90px', targets = 1:13)
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact',
        selection = 'none',
        colnames = c(
          'Market/Region/Country' = 'Region',
          'Producer Quantity' = 'Supply_Q',
          'Consumer Quantity' = 'Demand_C',
          'Consumer Price' = 'Consumer_Price',
          'Producer Price' = 'Producer_Price',
          'Price Elasticity of Supply' = 'Supply_Elasticity',
          'Price Elasticity of Demand' = 'Demand_Elasticity',
          'Income Elasticity of Demand' = 'Income_Elasticity',
          'Annual Population Growth Rate' = 'Pop_Growth',
          'Annual Income Growth Rate' = 'Income_Growth',
          'Annual Production Growth Rate' = 'Production_Growth',
          'Consumer Tax Rate' = 'Tax_Consumption',
          'Producer Tax Rate' = 'Tax_Production',
          'Research Spillover θ' = 'Spillover_theta'
        )
      ) %>%
        formatRound(columns = c(2, 3), digits = 0) %>%
        formatRound(columns = c(4, 5), digits = 2) %>%
        formatRound(columns = c(6, 7, 8), digits = 2) %>%
        formatPercentage(columns = c(9, 10, 11, 12, 13), digits = 2) %>%
        formatRound(columns = c(14), digits = 2)
    })
    
    # NPV Surplus by market
    output[[paste0("emp_regional_surplus_plot_", sim_id)]] <- renderPlotly({
      df_wide <- data.frame(
        Market = sim_inputs$market_data$Region,
        Producers = sim_data$npv_producers,
        Consumers = sim_data$npv_consumers,
        Government = sim_data$npv_governments
      )
      df <- data.frame(
        Market = rep(df_wide$Market, 3),
        NPV_Surplus = c(df_wide$Producers, df_wide$Consumers, df_wide$Government),
        Type = factor(rep(c("Producers", "Consumers", "Government"), each = nrow(df_wide)),
                      levels = c("Producers", "Consumers", "Government"))
      )
      p <- ggplot(df, aes(x = Market, y = NPV_Surplus, fill = Type)) +
        geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
        labs(title = "NPV Surplus by Market", 
             x = "Market", 
             y = paste0("NPV Surplus (", p_label, ")")) +
        theme_minimal() +
        theme(plot.title=element_text(size=16, hjust=.5), legend.position="bottom",
              panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p) %>% 
        layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3))
    })
    
    # Price dynamics
    output[[paste0("emp_price_dynamics_plot_", sim_id)]] <- renderPlotly({
      df <- data.frame(
        Time = rep(sim_data$time_vector, 2) + sim_inputs$emp_year - 1,
        Price = c(sim_data$prices, sim_data$prices_research),
        Scenario = rep(c("Without Research", "With Research"), each = length(sim_data$time_vector))
      )
      p <- ggplot(df, aes(Time, Price, color = Scenario)) +
        geom_line(linewidth=1) + geom_point(size=1.2) +
        labs(title="Equilibrium Prices Over Time", 
             x="Year", 
             y=paste0("Price (", p_label, ")")) +
        theme_minimal() +
        theme(plot.title=element_text(size=16, hjust=.5), legend.position="bottom")
      ggplotly(p) %>% layout(legend=list(orientation="h", x=.5, xanchor="center", y=-0.3))
    })
    
    # Surplus over time
    output[[paste0("emp_surplus_time_plot_", sim_id)]] <- renderPlotly({
      df <- data.frame(
        Time = rep(sim_data$time_vector, 5) + sim_inputs$emp_year - 1,
        Surplus = c(sim_data$research_costs,
                    colSums(sim_data$surplus_producers),
                    colSums(sim_data$surplus_consumers), 
                    colSums(sim_data$surplus_governments),
                    sim_data$research_costs + colSums(sim_data$surplus_producers) + 
                      colSums(sim_data$surplus_consumers) + colSums(sim_data$surplus_governments)),
        Type = factor(rep(c("Research", "Producers", "Consumers", "Government", "Aggregate"), 
                          each = length(sim_data$time_vector)),
                      levels = c("Research", "Producers", "Consumers", "Government", "Aggregate"))
      )
      p <- ggplot(df, aes(Time, Surplus, color = Type)) +
        geom_line(linewidth=1) + geom_point(size=1.2) +
        labs(title="Surplus Over Time", 
             x="Year", 
             y=paste0("Surplus (", p_label, ")")) +
        theme_minimal() +
        theme(plot.title=element_text(size=16, hjust=.5), legend.position="bottom")
      ggplotly(p) %>% layout(legend=list(orientation="h", x=.5, xanchor="center",y=-0.3))
    })
    
    # Cumulative surplus
    output[[paste0("emp_cumulative_surplus_plot_", sim_id)]] <- renderPlotly({
      s <- list(
        cumsum(sim_data$research_costs),
        cumsum(colSums(sim_data$surplus_producers)),
        cumsum(colSums(sim_data$surplus_consumers)),
        cumsum(colSums(sim_data$surplus_governments)),
        cumsum(sim_data$research_costs + colSums(sim_data$surplus_producers) + 
                 colSums(sim_data$surplus_consumers) + colSums(sim_data$surplus_governments))
      )
      df <- data.frame(
        Time = rep(sim_data$time_vector, 5) + sim_inputs$emp_year - 1,
        Surplus = do.call(c, s),
        Type = factor(rep(c("Research", "Producers", "Consumers", "Government", "Aggregate"), 
                          each = length(sim_data$time_vector)),
                      levels = c("Research", "Producers", "Consumers", "Government", "Aggregate"))
      )
      p <- ggplot(df, aes(Time, Surplus, color = Type)) +
        geom_line(linewidth=1) + geom_point(size=1.2) +
        labs(title="Cumulative Surplus Over Time", 
             x="Year", 
             y=paste0("Cumulative Surplus (", p_label, ")")) +
        theme_minimal() +
        theme(plot.title=element_text(size=16, hjust=.5), legend.position="bottom")
      ggplotly(p) %>% layout(legend=list(orientation="h", x=.5, xanchor="center", y=-0.3))
    })
  }
  
  #-----------------------------------------------------------------------------
  # GENERATE EMPIRICAL PLOTS WHEN SIMULATIONS CHANGE
  #-----------------------------------------------------------------------------
  observe({
    req(length(empirical_simulations$data) > 0)
    
    # Generate plots for all simulations
    lapply(names(empirical_simulations$data), function(sim_id) {
      sim <- empirical_simulations$data[[sim_id]]
      generate_empirical_plots(sim_id, sim$data, sim$inputs)
    })
  })
  
  #-----------------------------------------------------------------------------
  # RENDER EMPIRICAL RESULTS TABS UI
  #-----------------------------------------------------------------------------
  output$empirical_results_tabs_ui <- renderUI({
    req(length(empirical_simulations$data) > 0)
    
    # Create list of tabs
    tab_list <- lapply(names(empirical_simulations$data), function(sim_id) {
      sim <- empirical_simulations$data[[sim_id]]
      
      # Create download handler
      output[[paste0("download_emp_html_", sim_id)]] <- downloadHandler(
        filename = function() {
          paste0(gsub(" ", "_", sim$name), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
        },
        content = function(file) {
          p_label <- names(p_units)[match(sim$inputs$emp_p_unit, p_units)]
          q_label <- names(q_units)[match(sim$inputs$emp_q_unit, q_units)]
          
          totals <- c(
            Research = sum(sim$data$npv_research),
            Producers = sum(sim$data$npv_producers),
            Consumers = sum(sim$data$npv_consumers),
            Government = sum(sim$data$npv_governments)
          )
          total_all <- sum(totals)
          
          # Create NPV by market plot
          df_wide <- data.frame(
            Market = sim$inputs$market_data$Region,
            Producers = sim$data$npv_producers,
            Consumers = sim$data$npv_consumers,
            Government = sim$data$npv_governments
          )
          df_npv <- data.frame(
            Market = rep(df_wide$Market, 3),
            NPV_Surplus = c(df_wide$Producers, df_wide$Consumers, df_wide$Government),
            Type = factor(rep(c("Producers", "Consumers", "Government"), each = nrow(df_wide)),
                          levels = c("Producers", "Consumers", "Government"))
          )
          p_npv <- ggplot(df_npv, aes(x = Market, y = NPV_Surplus, fill = Type)) +
            geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
            labs(title = "NPV Surplus by Market", x = "Market", y = paste("NPV Surplus (", p_label, ")")) +
            theme_minimal() +
            theme(plot.title=element_text(size=14, hjust=.5), legend.position="bottom",
                  panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1))
          
          tmp_npv <- tempfile(fileext = ".png")
          ggsave(tmp_npv, p_npv, width = 10, height = 6, dpi = 100)
          img_npv <- base64enc::base64encode(tmp_npv)
          unlink(tmp_npv)
          
          # Create price dynamics plot
          df_price <- data.frame(
            Time = rep(sim$data$time_vector, 2) + sim$inputs$emp_year - 1,
            Price = c(sim$data$prices, sim$data$prices_research),
            Scenario = rep(c("Without Research", "With Research"), each = length(sim$data$time_vector))
          )
          p_price <- ggplot(df_price, aes(Time, Price, color = Scenario)) +
            geom_line(linewidth=1) + geom_point(size=1.2) +
            labs(title="Equilibrium Prices Over Time", x="Year", y=paste("Price (", p_label, ")")) +
            theme_minimal() +
            theme(plot.title=element_text(size=14, hjust=.5), legend.position="bottom")
          
          tmp_price <- tempfile(fileext = ".png")
          ggsave(tmp_price, p_price, width = 10, height = 6, dpi = 100)
          img_price <- base64enc::base64encode(tmp_price)
          unlink(tmp_price)
          
          # Create surplus over time plot
          df_surplus <- data.frame(
            Time = rep(sim$data$time_vector, 5) + sim$inputs$emp_year - 1,
            Surplus = c(sim$data$research_costs,
                        colSums(sim$data$surplus_producers),
                        colSums(sim$data$surplus_consumers), 
                        colSums(sim$data$surplus_governments),
                        sim$data$research_costs + colSums(sim$data$surplus_producers) + 
                          colSums(sim$data$surplus_consumers) + colSums(sim$data$surplus_governments)),
            Type = factor(rep(c("Research", "Producers", "Consumers", "Government", "Aggregate"), 
                              each = length(sim$data$time_vector)),
                          levels = c("Research", "Producers", "Consumers", "Government", "Aggregate"))
          )
          p_surplus <- ggplot(df_surplus, aes(Time, Surplus, color = Type)) +
            geom_line(linewidth=1) + geom_point(size=1.2) +
            labs(title="Surplus Over Time", x="Year", y=paste("Surplus (", p_label, ")")) +
            theme_minimal() +
            theme(plot.title=element_text(size=14, hjust=.5), legend.position="bottom")
          
          tmp_surplus <- tempfile(fileext = ".png")
          ggsave(tmp_surplus, p_surplus, width = 10, height = 6, dpi = 100)
          img_surplus <- base64enc::base64encode(tmp_surplus)
          unlink(tmp_surplus)
          
          # Create cumulative surplus plot
          s <- list(
            cumsum(sim$data$research_costs),
            cumsum(colSums(sim$data$surplus_producers)),
            cumsum(colSums(sim$data$surplus_consumers)),
            cumsum(colSums(sim$data$surplus_governments)),
            cumsum(sim$data$research_costs + colSums(sim$data$surplus_producers) + 
                     colSums(sim$data$surplus_consumers) + colSums(sim$data$surplus_governments))
          )
          df_cumul <- data.frame(
            Time = rep(sim$data$time_vector, 5) + sim$inputs$emp_year - 1,
            Surplus = do.call(c, s),
            Type = factor(rep(c("Research", "Producers", "Consumers", "Government", "Aggregate"), 
                              each = length(sim$data$time_vector)),
                          levels = c("Research", "Producers", "Consumers", "Government", "Aggregate"))
          )
          p_cumul <- ggplot(df_cumul, aes(Time, Surplus, color = Type)) +
            geom_line(linewidth=1) + geom_point(size=1.2) +
            labs(title="Cumulative Surplus Over Time", x="Year", y=paste("Cumulative Surplus (", p_label, ")")) +
            theme_minimal() +
            theme(plot.title=element_text(size=14, hjust=.5), legend.position="bottom")
          
          tmp_cumul <- tempfile(fileext = ".png")
          ggsave(tmp_cumul, p_cumul, width = 10, height = 6, dpi = 100)
          img_cumul <- base64enc::base64encode(tmp_cumul)
          unlink(tmp_cumul)
          
          # Create market configuration table HTML with all parameters including spillover
          market_table_html <- ""
          for (i in 1:nrow(sim$inputs$market_data)) {
            spillover_val <- sim$inputs$spillover_data$Spillover_theta[i]
            market_table_html <- paste0(market_table_html,
                                        "<tr>",
                                        "<td>", sim$inputs$market_data$Region[i], "</td>",
                                        "<td>", sprintf("%.0f", sim$inputs$market_data$Supply_Q[i]), "</td>",
                                        "<td>", sprintf("%.0f", sim$inputs$market_data$Demand_C[i]), "</td>",
                                        "<td>", sprintf("%.2f", sim$inputs$market_data$Consumer_Price[i]), "</td>",
                                        "<td>", sprintf("%.2f", sim$inputs$market_data$Producer_Price[i]), "</td>",
                                        "<td>", sprintf("%.2f", sim$inputs$market_data$Supply_Elasticity[i]), "</td>",
                                        "<td>", sprintf("%.2f", sim$inputs$market_data$Demand_Elasticity[i]), "</td>",
                                        "<td>", sprintf("%.2f", sim$inputs$market_data$Income_Elasticity[i]), "</td>",
                                        "<td>", sprintf("%.2f%%", sim$inputs$market_data$Pop_Growth[i] * 100), "</td>",
                                        "<td>", sprintf("%.2f%%", sim$inputs$market_data$Income_Growth[i] * 100), "</td>",
                                        "<td>", sprintf("%.2f%%", sim$inputs$market_data$Production_Growth[i] * 100), "</td>",
                                        "<td>", sprintf("%.2f%%", sim$inputs$market_data$Tax_Consumption[i] * 100), "</td>",
                                        "<td>", sprintf("%.2f%%", sim$inputs$market_data$Tax_Production[i] * 100), "</td>",
                                        "<td>", sprintf("%.2f", spillover_val), "</td>",
                                        "</tr>")
          }
          
          html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>', sim$name, ' - Open Economy Simulation Results</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 40px; background-color: #f5f5f5; }
    .container { max-width: 1200px; margin: 0 auto; background-color: white; padding: 30px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
    h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }
    h2 { color: #34495e; margin-top: 30px; border-bottom: 2px solid #95a5a6; padding-bottom: 8px; }
    h3 { color: #7f8c8d; margin-top: 20px; }
    .section { margin: 20px 0; }
    .params { background-color: #ecf0f1; padding: 15px; border-radius: 5px; font-family: monospace; white-space: pre-wrap; font-size: 12px; }
    table { border-collapse: collapse; width: 100%; margin: 15px 0; }
    th, td { border: 1px solid #bdc3c7; padding: 12px; text-align: left; }
    th { background-color: #3498db; color: white; }
    tr:nth-child(even) { background-color: #f2f2f2; }
    .plot-container { margin: 30px 0; text-align: center; }
    .footer { margin-top: 40px; padding-top: 20px; border-top: 1px solid #bdc3c7; color: #7f8c8d; font-size: 12px; }
  </style>
</head>
<body>
  <div class="container">
    <h1>Open Economy Simulation Results</h1>
    <h2>', sim$name, '</h2>
    <p><strong>Generated:</strong> ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</p>
    
    <div class="section">
      <h2>Model Parameters</h2>
      <div class="params">
General Parameters
Initial Year: ', sim$inputs$emp_year, '
Number of Markets: ', sim$data$n_regions, '
Number of Periods: ', length(sim$data$time_vector), '
Quantity Unit: ', q_label, '
Price Unit: ', p_label, '

R&D Parameters
Probability of Success: ', sprintf("%.1f", sim$inputs$emp_prob_success), '%
Cost Saving: ', sprintf("%.1f", sim$data$cost_saving * 100), '%
Adoption Ceiling: ', sprintf("%.1f", sim$data$adoption_ceiling * 100), '%
Adoption Shape: ', sim$data$adoption_shape, '
Research Cutoff Periods: ', paste(sim$data$cutoffs, collapse = ", "), '

Financial Parameters
NPV Discount Rate: ', sprintf("%.1f", sim$data$discount_rate * 100), '%
      </div>
    </div>
    
    <div class="section">
      <h2>NPV Summary</h2>
      <table>
        <tr>
          <th>Actor</th>
          <th>NPV Surplus (', p_label, ')</th>
        </tr>
        <tr>
          <td>Research</td>
          <td>', sprintf("%.2f", totals[1]), '</td>
        </tr>
        <tr>
          <td>Producers</td>
          <td>', sprintf("%.2f", totals[2]), '</td>
        </tr>
        <tr>
          <td>Consumers</td>
          <td>', sprintf("%.2f", totals[3]), '</td>
        </tr>
        <tr>
          <td>Government</td>
          <td>', sprintf("%.2f", totals[4]), '</td>
        </tr>
        <tr style="font-weight: bold; background-color: #d5dbdb;">
          <td>Aggregate</td>
          <td>', sprintf("%.2f", total_all), '</td>
        </tr>
      </table>
    </div>
    
    <div class="section">
      <h2>Market Parameters</h2>
      <table>
        <tr>
          <th>Market/Region</th>
          <th>Producer Q</th>
          <th>Consumer Q</th>
          <th>Consumer P</th>
          <th>Producer P</th>
          <th>Supply Elast.</th>
          <th>Demand Elast.</th>
          <th>Income Elast.</th>
          <th>Pop Growth</th>
          <th>Income Growth</th>
          <th>Prod Growth</th>
          <th>Consumer Tax</th>
          <th>Producer Tax</th>
          <th>Spillover θ</th>
        </tr>
        ', market_table_html, '
      </table>
    </div>
    
    <div class="section">
      <h2>Visualizations</h2>
      
      <h3>NPV Surplus by Market</h3>
      <div class="plot-container">
        <img src="data:image/png;base64,', img_npv, '" style="width:100%; max-width:800px;">
      </div>
      
      <h3>Price Dynamics</h3>
      <div class="plot-container">
        <img src="data:image/png;base64,', img_price, '" style="width:100%; max-width:800px;">
      </div>
      
      <h3>Surplus Over Time</h3>
      <div class="plot-container">
        <img src="data:image/png;base64,', img_surplus, '" style="width:100%; max-width:800px;">
      </div>
      
      <h3>Cumulative Surplus Over Time</h3>
      <div class="plot-container">
        <img src="data:image/png;base64,', img_cumul, '" style="width:100%; max-width:800px;">
      </div>
    </div>
    
    <div class="footer">
      <p>Generated by DREAM Benefit-Cost Analysis Tool</p>
    </div>
  </div>
</body>
</html>')
          
          writeLines(html_content, file)
        }
      )
      
      # Create delete handler
      observeEvent(input[[paste0("delete_emp_sim_", sim_id)]], {
        empirical_simulations$data[[sim_id]] <- NULL
        showNotification("SIMULATION DELETED.", type = "message", duration = 3)
      }, ignoreInit = TRUE)
      
      tabPanel(
        title = sim$name,
        value = paste0("emp_result_", sim_id),
        create_emp_sim_result_content(sim_id, sim$data, sim$inputs, sim$name)
      )
    })
    
    # Set selected tab to the most recent simulation
    selected_tab <- if (!is.null(empirical_simulations$latest_sim_id)) {
      paste0("emp_result_", empirical_simulations$latest_sim_id)
    } else {
      NULL
    }
    
    do.call(tabsetPanel, c(list(id = "empirical_results_tabs", selected = selected_tab), tab_list))
  })
  
  #-----------------------------------------------------------------------------
  # EMPIRICAL SUMMARY TABLES
  #-----------------------------------------------------------------------------
  # CLOSED ECONOMY SIMULATION - CREATE SIMULATION RESULT TAB CONTENT
  #-----------------------------------------------------------------------------
  create_sim_result_content <- function(sim_id, sim_data, sim_inputs, sim_name) {
    
    p_label <- names(p_units)[match(sim_inputs$p_unit, p_units)]
    q_label <- names(q_units)[match(sim_inputs$q_unit, q_units)]
    rp <- sim_data$regional_params_df
    
    # Calculate totals for NPV
    totals <- c(
      Research = sum(sim_data$npv_research),
      Producers = sum(sim_data$npv_producers),
      Consumers = sum(sim_data$npv_consumers),
      Government = sum(sim_data$npv_governments)
    )
    total_all <- sum(totals)
    
    tagList(
      # Header with download button
      fluidRow(
        column(12,
               div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;",
                   h4(paste("Simulation:", sim_name), style="margin: 0;"),
                   div(style="margin-top: 10px;",
                       downloadButton(paste0("download_html_", sim_id), "Download Report (HTML)", class = "btn-primary btn-lg"),
                       actionButton(paste0("delete_sim_", sim_id), HTML("&times; Delete"), class = "btn-secondary btn-lg", 
                                    style="margin-left: 10px;")
                   )
               )
        )
      ),
      
      hr(),
      
      # PARAMETERS SECTION
      # PARAMETERS SECTION
      fluidRow(
        column(12,
               h4("Model Parameters"),
               card(
                 card_body(
                   fluidRow(
                     column(6,
                            tags$h5("General Parameters", class = "param-section-title", style = "margin-top: 0;"),
                            tags$table(class = "table table-sm table-borderless param-table", style = "margin-bottom: 25px;",
                                       tags$tr(tags$td("Initial Year:"), tags$td(sprintf("%d", sim_inputs$year))),
                                       tags$tr(tags$td("Number of Periods:"), tags$td(sprintf("%d", length(sim_data$time_vector)))),
                                       tags$tr(tags$td("Quantity Unit:"), tags$td(q_label)),
                                       tags$tr(tags$td("Price Unit:"), tags$td(p_label))
                            ),
                            
                            tags$h5("Initial Equilibrium", class = "param-section-title"),
                            tags$table(class = "table table-sm table-borderless param-table", style = "margin-bottom: 25px;",
                                       tags$tr(tags$td("Consumer Price (Pc0):"), tags$td(sprintf("%.2f %s", rp$Pc0[1], p_label))),
                                       tags$tr(tags$td("Producer Price (Pp0):"), tags$td(sprintf("%.2f %s", rp$Pp0[1], p_label))),
                                       tags$tr(tags$td("Consumer Quantity (Qc0):"), tags$td(sprintf("%.0f %s", rp$Qc0[1], q_label))),
                                       tags$tr(tags$td("Producer Quantity (Qp0):"), tags$td(sprintf("%.0f %s", rp$Qp0[1], q_label)))
                            ),
                            
                            tags$h5("Elasticities", class = "param-section-title"),
                            tags$table(class = "table table-sm table-borderless param-table", style = "margin-bottom: 0;",
                                       tags$tr(tags$td("Supply Price Elasticity:"), tags$td(sprintf("%.1f%%", sim_inputs$supply_price_elasticity_range[1]))),
                                       tags$tr(tags$td("Demand Price Elasticity:"), tags$td(sprintf("%.1f%%", sim_inputs$demand_price_elasticity_range[1]))),
                                       tags$tr(tags$td("Income Elasticity:"), tags$td(sprintf("%.1f%%", sim_inputs$income_elasticity_range[1])))
                            )
                     ),
                     
                     column(6,
                            tags$h5("Growth Rates", class = "param-section-title", style = "margin-top: 0;"),
                            tags$table(class = "table table-sm table-borderless param-table", style = "margin-bottom: 25px;",
                                       tags$tr(tags$td("Population Growth:"), tags$td(sprintf("%.2f%%", rp$Pop_Growth[1] * 100))),
                                       tags$tr(tags$td("Income Growth:"), tags$td(sprintf("%.2f%%", rp$Income_Growth[1] * 100))),
                                       tags$tr(tags$td("Production Growth:"), tags$td(sprintf("%.1f%%", sim_inputs$output_growth_range[1])))
                            ),
                            
                            tags$h5("R&D Parameters", class = "param-section-title"),
                            tags$table(class = "table table-sm table-borderless param-table", style = "margin-bottom: 25px;",
                                       tags$tr(tags$td("Probability of Success:"), tags$td(sprintf("%.1f%%", rp$Prob_Success[1] * 100))),
                                       tags$tr(tags$td("Cost Saving:"), tags$td(sprintf("%.1f%%", sim_data$cost_saving * 100))),
                                       tags$tr(tags$td("Adoption Ceiling:"), tags$td(sprintf("%.1f%%", sim_data$adoption_ceiling * 100))),
                                       tags$tr(tags$td("Adoption Shape:"), tags$td(tools::toTitleCase(sim_data$adoption_shape))),
                                       tags$tr(tags$td("Research Cutoff Periods:"), tags$td(paste(sim_data$cutoffs, collapse = ", ")))
                            ),
                            
                            tags$h5("Taxes and Discount Rate", class = "param-section-title"),
                            tags$table(class = "table table-sm table-borderless param-table", style = "margin-bottom: 0;",
                                       tags$tr(tags$td("Consumer Tax:"), tags$td(sprintf("%.2f%%", rp$Tax_Consumption[1] * 100))),
                                       tags$tr(tags$td("Producer Tax:"), tags$td(sprintf("%.2f%%", rp$Tax_Production[1] * 100))),
                                       tags$tr(tags$td("NPV Discount Rate:"), tags$td(sprintf("%.1f%%", sim_data$discount_rate * 100)))
                            )
                     )
                   )
                 )
               )
        )
      ),
      
      hr(),
      
      fluidRow(
        column(12,
               h4("Net Present Value Summary"),
               card(
                 card_body(
                   tags$table(class = "table table-sm table-borderless param-table", style = "width: 60%; margin: 0 auto;",
                              tags$tr(tags$td("Sector"), tags$td(paste0("NPV (", p_label, ")"))),
                              tags$tr(style = "border-top: 1px solid #dee2e6;",
                                      tags$td("Research"), tags$td(sprintf("%.2f", totals[1]))
                              ),
                              tags$tr(tags$td("Producers"), tags$td(sprintf("%.2f", totals[2]))),
                              tags$tr(tags$td("Consumers"), tags$td(sprintf("%.2f", totals[3]))),
                              tags$tr(tags$td("Government"), tags$td(sprintf("%.2f", totals[4]))),
                              tags$tr(style = "border-top: 1px solid #dee2e6;",
                                      tags$tr(tags$td("Total"), 
                                              tags$td(sprintf("%.2f", total_all)))
                              )
                   )
                 )
               )
        )
      ),
      
      hr(),
      
      # PLOTS
      h4("Outcomes over Time"),
      fluidRow(
        column(12,
               plotlyOutput(paste0("price_dynamics_plot_", sim_id), height = "400px")
        )
      ),
      
      fluidRow(
        column(12,
               plotlyOutput(paste0("surplus_time_plot_", sim_id), height = "400px")
        )
      ),
      
      fluidRow(
        column(12,
               plotlyOutput(paste0("cumulative_surplus_plot_", sim_id), height = "400px")
        )
      )
    )
  }
  
  #-----------------------------------------------------------------------------
  # GENERATE PLOTS FOR CLOSED ECONOMY SIMULATION
  #-----------------------------------------------------------------------------
  generate_plots <- function(sim_id, sim_data, sim_inputs) {
    
    p_label <- names(p_units)[match(sim_inputs$p_unit, p_units)]
    
    # NPV Surplus bar chart
    output[[paste0("npv_surplus_plot_", sim_id)]] <- renderPlotly({
      totals <- c(
        Research = sum(sim_data$npv_research),
        Producers = sum(sim_data$npv_producers),
        Consumers = sum(sim_data$npv_consumers),
        Government = sum(sim_data$npv_governments)
      )
      
      df <- data.frame(
        Actor = factor(names(totals), levels = names(totals)),
        NPV = totals
      )
      
      p <- ggplot(df, aes(x = Actor, y = NPV, fill = Actor)) +
        geom_col(alpha = 0.8, width = 0.7) +
        labs(title = "Net Present Value by Actor", 
             x = "Actor", 
             y = paste0("NPV (", p_label, ")")) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, hjust = 0.5), 
              legend.position = "none",
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank())
      
      ggplotly(p) %>% 
        layout(showlegend = FALSE)
    })
    
    # Price dynamics plot (now first)
    output[[paste0("price_dynamics_plot_", sim_id)]] <- renderPlotly({
      df <- data.frame(
        Time = rep(sim_data$time_vector, 2) + sim_inputs$year - 1,
        Price = c(sim_data$prices, sim_data$prices_research),
        Scenario = rep(c("Without Research", "With Research"), each = length(sim_data$time_vector))
      )
      p <- ggplot(df, aes(Time, Price, color = Scenario)) +
        geom_line(linewidth = 1) + geom_point(size = 1.2) +
        labs(title = "Equilibrium Price Over Time", 
             x = "Time Period", 
             y = paste0("Price (", p_label, ")")) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, hjust = 0.5), legend.position = "bottom")
      ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
    })
    
    # Surplus over time plot
    output[[paste0("surplus_time_plot_", sim_id)]] <- renderPlotly({
      df <- data.frame(
        Time = rep(sim_data$time_vector, 5) + sim_inputs$year - 1,
        Surplus = c(sim_data$research_costs,
                    sim_data$surplus_producers[1, ],
                    sim_data$surplus_consumers[1, ],
                    sim_data$surplus_governments[1, ],
                    sim_data$surplus_producers[1, ] + sim_data$surplus_consumers[1, ] + 
                      sim_data$surplus_governments[1, ] + sim_data$research_costs),
        Type = factor(rep(c("Research", "Producers", "Consumers", "Government", "Aggregate"), 
                          each = length(sim_data$time_vector)),
                      levels = c("Research", "Producers", "Consumers", "Government", "Aggregate"))
      )
      p <- ggplot(df, aes(Time, Surplus, color = Type)) +
        geom_line(linewidth = 1) + geom_point(size = 1.2) +
        labs(title = "Surplus Over Time", 
             x = "Time Period", 
             y = paste0("Surplus (", p_label, ")")) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, hjust = 0.5), legend.position = "bottom")
      ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
    })
    
    # Cumulative surplus plot
    output[[paste0("cumulative_surplus_plot_", sim_id)]] <- renderPlotly({
      df <- data.frame(
        Time = rep(sim_data$time_vector, 5) + sim_inputs$year - 1,
        Surplus = c(cumsum(sim_data$research_costs),
                    cumsum(sim_data$surplus_producers[1, ]),
                    cumsum(sim_data$surplus_consumers[1, ]),
                    cumsum(sim_data$surplus_governments[1, ]),
                    cumsum(sim_data$surplus_producers[1, ] + sim_data$surplus_consumers[1, ] + 
                             sim_data$surplus_governments[1, ] + sim_data$research_costs)),
        Type = factor(rep(c("Research", "Producers", "Consumers", "Government", "Aggregate"), 
                          each = length(sim_data$time_vector)),
                      levels = c("Research", "Producers", "Consumers", "Government", "Aggregate"))
      )
      p <- ggplot(df, aes(Time, Surplus, color = Type)) +
        geom_line(linewidth = 1) + geom_point(size = 1.2) +
        labs(title = "Cumulative Surplus Over Time", 
             x = "Time Period", 
             y = paste0("Cumulative Surplus (", p_label, ")")) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, hjust = 0.5), legend.position = "bottom")
      ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
    })
  }
  
  #-----------------------------------------------------------------------------
  # RENDER RESULTS TABS UI
  #-----------------------------------------------------------------------------
  output$results_tabs_ui <- renderUI({
    req(length(simulations$data) > 0)
    
    # Create list of tabs
    tab_list <- lapply(names(simulations$data), function(sim_id) {
      sim <- simulations$data[[sim_id]]
      
      # Generate plots for this simulation
      generate_plots(sim_id, sim$data, sim$inputs)
      
      # Create download handler
      output[[paste0("download_html_", sim_id)]] <- downloadHandler(
        filename = function() {
          paste0(gsub(" ", "_", sim$name), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
        },
        content = function(file) {
          p_label <- names(p_units)[match(sim$inputs$p_unit, p_units)]
          q_label <- names(q_units)[match(sim$inputs$q_unit, q_units)]
          rp <- sim$data$regional_params_df
          
          totals <- c(
            Research = sum(sim$data$npv_research),
            Producers = sum(sim$data$npv_producers),
            Consumers = sum(sim$data$npv_consumers),
            Government = sum(sim$data$npv_governments)
          )
          total_all <- sum(totals)
          
          # Create NPV bar plot
          df_npv <- data.frame(
            Type = c("Research", "Producers", "Consumers", "Government"),
            NPV_Surplus = totals
          )
          df_npv$Type <- factor(df_npv$Type, levels = c("Research", "Producers", "Consumers", "Government"))
          
          p_npv <- ggplot(df_npv, aes(x = Type, y = NPV_Surplus, fill = Type)) +
            geom_col(alpha = 0.8, width = 0.6) +
            labs(title = "NPV Surplus by Actor", x = "", y = paste("NPV Surplus (", p_label, ")")) +
            theme_minimal() +
            theme(plot.title = element_text(size = 14, hjust = 0.5), 
                  legend.position = "none",
                  panel.grid.minor = element_blank(), 
                  panel.grid.major.x = element_blank())
          
          tmp_npv <- tempfile(fileext = ".png")
          ggsave(tmp_npv, p_npv, width = 10, height = 6, dpi = 100)
          img_npv <- base64enc::base64encode(tmp_npv)
          unlink(tmp_npv)
          
          # Create price dynamics plot
          df_price <- data.frame(
            Time = rep(sim$data$time_vector, 2) + sim$inputs$year - 1,
            Price = c(sim$data$prices, sim$data$prices_research),
            Scenario = rep(c("Without Research", "With Research"), each = length(sim$data$time_vector))
          )
          p_price <- ggplot(df_price, aes(Time, Price, color = Scenario)) +
            geom_line(linewidth = 1) + geom_point(size = 1.2) +
            labs(title = "Equilibrium Price Over Time", x = "Year", y = paste("Price (", p_label, ")")) +
            theme_minimal() +
            theme(plot.title = element_text(size = 14, hjust = 0.5), legend.position = "bottom")
          
          tmp_price <- tempfile(fileext = ".png")
          ggsave(tmp_price, p_price, width = 10, height = 6, dpi = 100)
          img_price <- base64enc::base64encode(tmp_price)
          unlink(tmp_price)
          
          # Create surplus over time plot
          df_surplus <- data.frame(
            Time = rep(sim$data$time_vector, 5) + sim$inputs$year - 1,
            Surplus = c(sim$data$research_costs,
                        sim$data$surplus_producers[1, ],
                        sim$data$surplus_consumers[1, ],
                        sim$data$surplus_governments[1, ],
                        sim$data$surplus_producers[1, ] + sim$data$surplus_consumers[1, ] + 
                          sim$data$surplus_governments[1, ] + sim$data$research_costs),
            Type = factor(rep(c("Research", "Producers", "Consumers", "Government", "Aggregate"), 
                              each = length(sim$data$time_vector)),
                          levels = c("Research", "Producers", "Consumers", "Government", "Aggregate"))
          )
          p_surplus <- ggplot(df_surplus, aes(Time, Surplus, color = Type)) +
            geom_line(linewidth = 1) + geom_point(size = 1.2) +
            labs(title = "Surplus Over Time", x = "Year", y = paste("Surplus (", p_label, ")")) +
            theme_minimal() +
            theme(plot.title = element_text(size = 14, hjust = 0.5), legend.position = "bottom")
          
          tmp_surplus <- tempfile(fileext = ".png")
          ggsave(tmp_surplus, p_surplus, width = 10, height = 6, dpi = 100)
          img_surplus <- base64enc::base64encode(tmp_surplus)
          unlink(tmp_surplus)
          
          # Create cumulative surplus plot
          df_cumul <- data.frame(
            Time = rep(sim$data$time_vector, 5) + sim$inputs$year - 1,
            Surplus = c(cumsum(sim$data$research_costs),
                        cumsum(sim$data$surplus_producers[1, ]),
                        cumsum(sim$data$surplus_consumers[1, ]),
                        cumsum(sim$data$surplus_governments[1, ]),
                        cumsum(sim$data$surplus_producers[1, ] + sim$data$surplus_consumers[1, ] + 
                                 sim$data$surplus_governments[1, ] + sim$data$research_costs)),
            Type = factor(rep(c("Research", "Producers", "Consumers", "Government", "Aggregate"), 
                              each = length(sim$data$time_vector)),
                          levels = c("Research", "Producers", "Consumers", "Government", "Aggregate"))
          )
          p_cumul <- ggplot(df_cumul, aes(Time, Surplus, color = Type)) +
            geom_line(linewidth = 1) + geom_point(size = 1.2) +
            labs(title = "Cumulative Surplus Over Time", x = "Year", y = paste("Cumulative Surplus (", p_label, ")")) +
            theme_minimal() +
            theme(plot.title = element_text(size = 14, hjust = 0.5), legend.position = "bottom")
          
          tmp_cumul <- tempfile(fileext = ".png")
          ggsave(tmp_cumul, p_cumul, width = 10, height = 6, dpi = 100)
          img_cumul <- base64enc::base64encode(tmp_cumul)
          unlink(tmp_cumul)
          
          html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>', sim$name, ' - Closed Economy Simulation Results</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 40px; background-color: #f5f5f5; }
    .container { max-width: 1200px; margin: 0 auto; background-color: white; padding: 30px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
    h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }
    h2 { color: #34495e; margin-top: 30px; border-bottom: 2px solid #95a5a6; padding-bottom: 8px; }
    h3 { color: #7f8c8d; margin-top: 20px; }
    .section { margin: 20px 0; }
    .params { background-color: #ecf0f1; padding: 15px; border-radius: 5px; font-family: monospace; white-space: pre-wrap; font-size: 12px; }
    table { border-collapse: collapse; width: 100%; margin: 15px 0; }
    th, td { border: 1px solid #bdc3c7; padding: 12px; text-align: left; }
    th { background-color: #3498db; color: white; }
    tr:nth-child(even) { background-color: #f2f2f2; }
    .plot-container { margin: 30px 0; text-align: center; }
    .footer { margin-top: 40px; padding-top: 20px; border-top: 1px solid #bdc3c7; color: #7f8c8d; font-size: 12px; }
  </style>
</head>
<body>
  <div class="container">
    <h1>Closed Economy Simulation Results</h1>
    <h2>', sim$name, '</h2>
    <p><strong>Generated:</strong> ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</p>
    
    <div class="section">
      <h2>Model Parameters</h2>
      <div class="params">
General Parameters
Initial Year: ', sim$inputs$year, '
Number of Periods: ', length(sim$data$time_vector), '
Quantity Unit: ', q_label, '
Price Unit: ', p_label, '

Initial Equilibrium
Consumer Price (Pc0): ', sprintf("%.3f", rp$Pc0[1]), ' ', p_label, '
Producer Price (Pp0): ', sprintf("%.3f", rp$Pp0[1]), ' ', p_label, '
Consumer Quantity (Qc0): ', sprintf("%.3f", rp$Qc0[1]), ' ', q_label, '
Producer Quantity (Qp0): ', sprintf("%.3f", rp$Qp0[1]), ' ', q_label, '

Elasticities
Supply Price Elasticity: ', sprintf("%.1f", sim$inputs$supply_price_elasticity_range[1]), '%
Demand Price Elasticity: ', sprintf("%.1f", sim$inputs$demand_price_elasticity_range[1]), '%
Income Elasticity: ', sprintf("%.1f", sim$inputs$income_elasticity_range[1]), '%

R&D Parameters
Cost Saving: ', sprintf("%.1f", sim$data$cost_saving * 100), '%
Adoption Ceiling: ', sprintf("%.1f", sim$data$adoption_ceiling * 100), '%
Adoption Shape: ', sim$data$adoption_shape, '
Research Cutoff Periods: ', paste(sim$data$cutoffs, collapse = ", "), '

Financial Parameters
NPV Discount Rate: ', sprintf("%.1f", sim$data$discount_rate * 100), '%
      </div>
    </div>
    
    <div class="section">
      <h2>NPV Summary</h2>
      <table>
        <tr>
          <th>Actor</th>
          <th>NPV Surplus (', p_label, ')</th>
        </tr>
        <tr>
          <td>Research</td>
          <td>', sprintf("%.2f", totals[1]), '</td>
        </tr>
        <tr>
          <td>Producers</td>
          <td>', sprintf("%.2f", totals[2]), '</td>
        </tr>
        <tr>
          <td>Consumers</td>
          <td>', sprintf("%.2f", totals[3]), '</td>
        </tr>
        <tr>
          <td>Government</td>
          <td>', sprintf("%.2f", totals[4]), '</td>
        </tr>
        <tr style="font-weight: bold; background-color: #d5dbdb;">
          <td>Aggregate</td>
          <td>', sprintf("%.2f", total_all), '</td>
        </tr>
      </table>
    </div>
    
    <div class="section">
      <h2>Visualizations</h2>
      
      <h3>NPV Surplus by Actor</h3>
      <div class="plot-container">
        <img src="data:image/png;base64,', img_npv, '" style="width:100%; max-width:800px;">
      </div>
      
      <h3>Price Dynamics</h3>
      <div class="plot-container">
        <img src="data:image/png;base64,', img_price, '" style="width:100%; max-width:800px;">
      </div>
      
      <h3>Surplus Over Time</h3>
      <div class="plot-container">
        <img src="data:image/png;base64,', img_surplus, '" style="width:100%; max-width:800px;">
      </div>
      
      <h3>Cumulative Surplus Over Time</h3>
      <div class="plot-container">
        <img src="data:image/png;base64,', img_cumul, '" style="width:100%; max-width:800px;">
      </div>
    </div>
    
    <div class="footer">
      <p>Generated by DREAM Benefit-Cost Analysis Tool</p>
    </div>
  </div>
</body>
</html>')
          
          writeLines(html_content, file)
        }
      )
      
      # Create delete handler
      observeEvent(input[[paste0("delete_sim_", sim_id)]], {
        simulations$data[[sim_id]] <- NULL
        showNotification("SIMULATION DELETED.", type = "message", duration = 3)
      }, ignoreInit = TRUE)
      
      tabPanel(
        title = sim$name,
        value = paste0("result_", sim_id),
        create_sim_result_content(sim_id, sim$data, sim$inputs, sim$name)
      )
    })
    
    # Set selected tab to the most recent simulation
    selected_tab <- if (!is.null(simulations$latest_sim_id)) {
      paste0("result_", simulations$latest_sim_id)
    } else {
      NULL  # Let Shiny choose default
    }
    
    do.call(tabsetPanel, c(list(id = "results_tabs", selected = selected_tab), tab_list))
  })
  
  #-----------------------------------------------------------------------------
  # RUN CLOSED ECONOMY SIMULATION
  #-----------------------------------------------------------------------------
  observeEvent(input$run_simulation, {
    
    req(!isTRUE(button_states$simulation_running))
    button_states$simulation_running <- TRUE
    
    shinyjs::disable("run_simulation")
    loading_id <- showNotification("RUNNING SIMULATION...", type = "message", duration = NULL)
    
    # Build params
    params <- list()
    params$n_regions <- 1
    params$seed <- 123
    params$q_consumption_range <- c(input$quantity, input$quantity)
    params$p_consumption_range <- c(input$price, input$price)
    params$q_production_range <- c(input$quantity, input$quantity)
    params$p_production_range <- c(input$price, input$price)
    params$supply_price_elasticity_range <- c(input$supply_price_elasticity_range[1], 
                                              input$supply_price_elasticity_range[1]) / 100
    params$demand_price_elasticity_range <- c(input$demand_price_elasticity_range[1], 
                                              input$demand_price_elasticity_range[1]) / 100
    params$income_elasticity_range <- c(input$income_elasticity_range[1], 
                                        input$income_elasticity_range[1]) / 100
    params$output_growth_range <- c(input$output_growth_range[1], 
                                    input$output_growth_range[1]) / 100
    params$income_growth_range <- c(input$income_growth_range[1], 
                                    input$income_growth_range[1]) / 100
    params$pop_growth_range <- c(input$pop_growth_range[1], 
                                 input$pop_growth_range[1]) / 100
    params$prob_success_range <- c(input$prob_success_range[1], 
                                   input$prob_success_range[1]) / 100
    params$cost_saving <- input$cost_saving / 100
    params$adoption_ceiling <- input$adoption_ceiling / 100
    params$adoption_shape <- input$adoption_shape
    params$research_cost_lag <- input$research_cost_lag
    params$research_cost_growth <- input$research_cost_growth
    # Convert tax percentages to absolute $ per unit based on initial price
    params$tax_production_range <- c(input$tax_production_range[1], 
                                     input$tax_production_range[1]) / 100 * input$price
    params$tax_consumption_range <- c(input$tax_consumption_range[1], 
                                      input$tax_consumption_range[1]) / 100 * input$price
    params$discount_rate <- input$discount_rate / 100
    
    # Add cutoffs vector - convert durations to cumulative years
    params$cutoffs <- cumsum(c(input$cutoff_lag, input$cutoff_adoption, 
                               input$cutoff_max, input$cutoff_decline))
    
    params <- params[intersect(names(params), allowed_param_names)]
    
    tryCatch({
      # Run simulation
      sim_result <- do.call(run_economic_simulation, params)
      
      # Increment counter
      simulations$counter <- simulations$counter + 1
      sim_id <- as.character(simulations$counter)
      
      # Generate simulation name
      sim_name <- if (nchar(trimws(input$simulation_name)) > 0) {
        input$simulation_name
      } else {
        paste("Simulation", simulations$counter)
      }
      
      # Store simulation inputs
      sim_inputs <- list(
        year = input$year,
        q_unit = input$q_unit,
        p_unit = input$p_unit,
        price = input$price,
        quantity = input$quantity,
        supply_price_elasticity_range = input$supply_price_elasticity_range,
        demand_price_elasticity_range = input$demand_price_elasticity_range,
        income_elasticity_range = input$income_elasticity_range,
        output_growth_range = input$output_growth_range,
        income_growth_range = input$income_growth_range,
        pop_growth_range = input$pop_growth_range,
        prob_success_range = input$prob_success_range,
        cost_saving = input$cost_saving,
        adoption_ceiling = input$adoption_ceiling,
        tax_production_range = input$tax_production_range,
        tax_consumption_range = input$tax_consumption_range,
        discount_rate = input$discount_rate,
        cutoff_lag = input$cutoff_lag,
        cutoff_adoption = input$cutoff_adoption,
        cutoff_max = input$cutoff_max,
        cutoff_decline = input$cutoff_decline,
        adoption_shape = input$adoption_shape,
        research_cost_lag = input$research_cost_lag,
        research_cost_growth = input$research_cost_growth
      )
      
      # Store simulation
      simulations$data[[sim_id]] <- list(
        name = sim_name,
        data = sim_result,
        inputs = sim_inputs,
        timestamp = Sys.time()
      )
      
      # Track this as the latest simulation for default tab selection
      simulations$latest_sim_id <- sim_id
      
      # Generate plots
      generate_plots(sim_id, sim_result, sim_inputs)
      
      # Create download handler (including HTML report generation code omitted for brevity)
      # Create delete handler
      observeEvent(input[[paste0("delete_sim_", sim_id)]], {
        simulations$data[[sim_id]] <- NULL
        showNotification("SIMULATION DELETED.", type = "message", duration = 3)
      }, ignoreInit = TRUE)
      
      # Reset button properly
      shinyjs::enable("run_simulation")
      updateTextInput(session, "simulation_name", value = "")
      removeNotification(loading_id)
      showNotification("SIMULATION COMPLETED. SEE RESULTS BELOW.", type = "message", duration = 5)
      
    }, error = function(e) {
      # Reset button properly on error
      shinyjs::enable("run_simulation")
      removeNotification(loading_id)
      showNotification(paste("ERROR:", e$message), type = "error", duration = 10)
    })
    
    button_states$simulation_running <- FALSE
  })
  
}