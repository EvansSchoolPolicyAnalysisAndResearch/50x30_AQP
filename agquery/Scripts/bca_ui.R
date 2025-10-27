# BCA UI Component
# This returns the UI for the BCA tab

bca_ui <- function() {
  tagList(
    useShinyjs(),
    
    tags$head(tags$style(HTML("
      /* only style inside #bca-root, do not touch html/body/.container-fluid */
      #bca-root .tab-content, 
      #bca-root .tab-pane { height:auto !important; overflow:visible !important; }
      #bca-root .inline-btns { margin-top:8px; display:flex; gap:8px; }
      #bca-root .inline-inputs .form-group { 
        margin-bottom:10px; display:grid; grid-template-columns:100px 1fr; 
        align-items:center; gap:10px; 
      }
      #bca-root .inline-inputs .form-group label { margin-bottom:0; text-align:left; }
    "))),
    
    # local wrapper to contain all BCA ui
    div(
      id = "bca-root",
    
    div(style="padding:15px; border:1px solid #ddd; margin-bottom:20px;",
        
        tabsetPanel(id = "main_tabs",
                    
                    ##############################################################################
                    # ABOUT TAB 
                    ##############################################################################
                    tabPanel("About",     
                             
                             h3("Benefit-Cost Simulation Overview"),
                             h5("This tool provides a means to simulate ''returns to commodity-oriented 
             research in an open-economy setting, allowing for price and technology spillover effects
             between a country in which the research originates and the rest of the world'' (Alston et al, 1998)."),
                             h5("Configure and run simulations in the Simulation tab. Each simulation run creates a new tab 
             in the results panel below."),
                             hr()
                    ),    
                    
                    ##############################################################################
                    # SIMULATION TAB 
                    ##############################################################################
                    tabPanel("Closed Economy",   
                             h3("Configuration"),
                             
                             # Simulation Name Input - Full Width
                             fluidRow(
                               column(4,
                                      textInput("simulation_name", 
                                                label = create_info_tooltip("Simulation Name", "Leave blank for auto-generated names (e.g., 'Simulation 1', 'Simulation 2')"), 
                                                value = "", 
                                                placeholder = "Enter a name (optional)")
                               ),
                               column(8)
                             ),
                             tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                             
                             #-----------------------------------------------------------------------
                             # THREE COLUMN LAYOUT
                             #-----------------------------------------------------------------------
                             fluidRow(   
                               #-----------------------------------------------------------------------
                               # LEFT COLUMN
                               #-----------------------------------------------------------------------
                               column(4,
                                      h4("Periods, Quantities, and Prices"),
                                      tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                      
                                      h5("Initial Year and Research Adoption Timeline", style = "font-style: italic;"),
                                      
                                      # Inline inputs for year and cutoffs - all in one container
                                      div(class = "inline-inputs",
                                          numericInput("year",
                                                       create_info_tooltip("Inital Year", "Year of quantity and price data"),
                                                       value = 2025, min = 2000, max = 2100),
                                          numericInput("cutoff_lag",
                                                       create_info_tooltip("Adoption Lag", "Duration in years of lag before adoption begins"),
                                                       value = 5, min = 0, max = 50, step = 1),
                                          numericInput("cutoff_adoption",
                                                       create_info_tooltip("Adoption Growth", "Duration in years of adoption phase to reach maximum"),
                                                       value = 5, min = 0, max = 50, step = 1),
                                          numericInput("cutoff_max",
                                                       create_info_tooltip("Adoption at Maximum", "Duration in years at maximum adoption level"),
                                                       value = 5, min = 0, max = 50, step = 1),
                                          numericInput("cutoff_decline",
                                                       create_info_tooltip("Adoption Decline", "Duration in years of decline phase until adoption ends"),
                                                       value = 5, min = 0, max = 50, step = 1),
                                          selectInput("adoption_shape",
                                                      create_info_tooltip("Adoption Shape", "Choose linear (trapezoidal) or sigmoidal (S-curve) adoption pattern over time"),
                                                      choices = c("Linear" = "linear", "Sigmoidal" = "sigmoidal"),
                                                      selected = "linear")
                                      ),
                                      
                                      tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                      
                                      h5("Units and Research Costs", style = "font-style: italic;"),
                                      div(class = "inline-inputs",
                                          selectizeInput("q_unit", 
                                                         create_info_tooltip("Quantity Unit", "FAQ"), 
                                                         choices = q_units, selected = "",
                                                         options=list(placeholder="Select Unit...")),
                                          selectizeInput("p_unit", 
                                                         create_info_tooltip("Price Unit", "FAQ"), 
                                                         choices = p_units, selected = "",
                                                         options=list(placeholder="Select Unit...")),
                                          numericInput("research_cost_lag",
                                                       create_info_tooltip("Yearly Cost During Lag", "Annual research cost during the lag period before adoption begins"),
                                                       value = 1000, min = 0, step = 1000),
                                          numericInput("research_cost_growth",
                                                       create_info_tooltip("Yearly Cost During Growth", "Annual research cost during the adoption growth period"),
                                                       value = 500, min = 0, step = 1000)
                                      ),
                                      tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                      
                                      h5("Equilibrium Quantity and Price in Initial Period",
                                         style = "font-style: italic;"),
                                      div(class = "inline-inputs",
                                          numericInput("price",
                                                       create_info_tooltip("Price", "Equilibrium price in market"),
                                                       value = 100, min = 0),
                                          numericInput("quantity",
                                                       create_info_tooltip("Quantity", "Equilibrium quantity in market"),
                                                       value = 5000, min = 0)
                                      )
                               ),
                               
                               #-----------------------------------------------------------------------
                               # CENTER COLUMN 
                               #-----------------------------------------------------------------------
                               column(4, 
                                      h4("Elasticities and Growth Rates"),
                                      tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                      
                                      h5("Elasticities", style = "font-style: italic;"),
                                      sliderInput("supply_price_elasticity_range",
                                                  create_info_tooltip("Price Elasticity of Supply", "Range of price elasticity values across regions"),
                                                  min = 0, max = 100, value = c(40), step = 1, post = "%"),
                                      sliderInput("demand_price_elasticity_range",
                                                  create_info_tooltip("Price Elasticity of Demand", "Range of price elasticity values across regions"),
                                                  min = -200, max = 0, value = c(-100), step = 1, post = "%"),
                                      sliderInput("income_elasticity_range",
                                                  create_info_tooltip("Income Elasticity of Demand", "Range of income elasticity values across regions"),
                                                  min = 0, max = 200, value = c(50), step = 1, post = "%"),
                                      tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                      
                                      h5("Growth Rates", style = "font-style: italic;"),
                                      sliderInput("output_growth_range",
                                                  create_info_tooltip("Production Growth", "Annual production growth rate range"),
                                                  min = -0.5, max = 5, value = c(0), step = 0.1, post = "%"),
                                      sliderInput("income_growth_range",
                                                  create_info_tooltip("Income Growth", "Annual income growth rate range"),
                                                  min = -0.5, max = 5, value = c(0), step = 0.1, post = "%"),
                                      sliderInput("pop_growth_range",
                                                  create_info_tooltip("Population Growth", "Annual population growth rate range"),
                                                  min = -0.5, max = 2, value = c(0), step = 0.1, post = "%")
                               ),
                               
                               #-----------------------------------------------------------------------
                               # RIGHT COLUMN 
                               #-----------------------------------------------------------------------
                               column(4,
                                      h4("R&D, Taxes, and Discount Rate"),
                                      tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                      
                                      h5("R&D Parameters", style = "font-style: italic;"),
                                      sliderInput("prob_success_range",
                                                  create_info_tooltip("Probability of Success", "Range of R&D success probabilities"),
                                                  min = 0, max = 100, value = c(50), step = 5, post = "%"),
                                      sliderInput("cost_saving",
                                                  create_info_tooltip("Cost Savings", "Percentage cost reduction from successful R&D"),
                                                  min = 0, max = 50, value = 10, step = 1, post = "%"),
                                      sliderInput("adoption_ceiling",
                                                  create_info_tooltip("Adoption Ceiling", "Maximum market adoption percentage"),
                                                  min = 0, max = 100, value = 50, step = 5, post = "%"),
                                      
                                      
                                      tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                      h5("Taxes and Discount Rate", style = "font-style: italic;"),
                                      sliderInput("tax_production_range",
                                                  create_info_tooltip("Producer Taxes", "Tax rate range on producer activities"),
                                                  min = -5, max = 15, value = c(0), step = 0.1, post = "%"),
                                      sliderInput("tax_consumption_range",
                                                  create_info_tooltip("Consumer Taxes", "Tax rate range on consumer purchases"),
                                                  min = -5, max = 15, value = c(0), step = 0.1, post = "%"),
                                      sliderInput("discount_rate",
                                                  create_info_tooltip("Net Present Value Discount Rate", "FAQ"),
                                                  min=0, max=10, value=3, step=0.1, post="%")
                               )
                             ),
                             
                             #-------------------------------------------------------------------------
                             # BUTTONS FOR RUNNING SIMULATION
                             #-------------------------------------------------------------------------
                             hr(),
                             div(align="center",
                                 actionButton("run_simulation", "RUN SIMULATION", class = "btn-primary btn-lg")
                             ),
                             
                             #-------------------------------------------------------------------------
                             # RESULTS PANEL WITH DYNAMIC TABS
                             #-------------------------------------------------------------------------
                             hr(), 
                             conditionalPanel(
                               condition = "output.show_results",
                               h3("Results"),
                               uiOutput("results_tabs_ui")
                             )
                    ), 
                    
                    ##############################################################################
                    # OPEN ECONOMIES TAB 
                    ##############################################################################
                    tabPanel("Open Economies",
                             h3("Configuration"),
                             
                             tabsetPanel(id = "empirical_tabs",
                                         
                                         #----------------------------------------------------------------------
                                         # MARKETS TAB - Based on DREAM Table C.1 structure
                                         #----------------------------------------------------------------------
                                         tabPanel("Markets",
                                                  h4("Market Parameters"),
                                                  p("Configure market-level data for multiple regions/countries. Based on DREAM model (Alston et al., 1998)."),
                                                  tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                                  
                                                  # Simulation Name Input
                                                  fluidRow(
                                                    column(4,
                                                           textInput("emp_simulation_name", 
                                                                     label = create_info_tooltip("Simulation Name", "Leave blank for auto-generated names (e.g., 'Open Economy 1', 'Open Economy 2')"), 
                                                                     value = "", 
                                                                     placeholder = "Enter a name (optional)")
                                                    ),
                                                    column(8)
                                                  ),
                                                  
                                                  tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                                  
                                                  # Units, Year, and Discount Rate
                                                  fluidRow(
                                                    column(3,
                                                           selectizeInput("emp_q_unit", 
                                                                          create_info_tooltip("Quantity Unit", "FAQ"), 
                                                                          choices = q_units, selected = "mt",
                                                                          options=list(placeholder="Select Unit..."))
                                                    ),
                                                    column(3,
                                                           selectizeInput("emp_p_unit", 
                                                                          create_info_tooltip("Price Unit", "FAQ"), 
                                                                          choices = p_units, selected = "usd",
                                                                          options=list(placeholder="Select Unit..."))
                                                    ),
                                                    column(3,
                                                           numericInput("emp_year",
                                                                        create_info_tooltip("Initial Year", "Year of quantity and price data"),
                                                                        value = 2025, min = 2000, max = 2100)
                                                    ),
                                                    column(3,
                                                           sliderInput("emp_discount_rate",
                                                                       create_info_tooltip("Net Present Value Discount Rate", "FAQ"),
                                                                       min = 0, max = 15, value = 3, step = 0.1, post = "%")
                                                    )
                                                  ),
                                                  
                                                  tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                                  
                                                  # Add/Remove buttons above table
                                                  fluidRow(
                                                    column(12,
                                                           div(style="margin-bottom: 10px;",
                                                               actionButton("add_market_row", "Add Market", icon = icon("plus"), class = "btn-success"),
                                                               actionButton("remove_market_row", "Remove Last Market", icon = icon("minus"), class = "btn-warning")
                                                           ),
                                                           DTOutput("market_data_table")
                                                    )
                                                  ),
                                                  
                                                  hr(),
                                                  div(align="center",
                                                      actionButton("market_next", "NEXT: R&D and Adoption →", class = "btn-primary")
                                                  )
                                         ),
                                         
                                         #----------------------------------------------------------------------
                                         # R&D AND ADOPTION TAB
                                         #----------------------------------------------------------------------
                                         tabPanel("R&D and Adoption",
                                                  h4("R&D and Adoption Parameters"),
                                                  tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                                  
                                                  fluidRow(
                                                    #----------------------------------------------------------------------
                                                    # LEFT COLUMN - Adoption Timeline
                                                    #----------------------------------------------------------------------
                                                    column(6,
                                                           h5("Research Adoption Timeline", style = "font-style: italic;"),
                                                           
                                                           div(class = "inline-inputs",
                                                               numericInput("emp_lambda_R",
                                                                            create_info_tooltip("Adoption Lag", "Duration in years of lag before adoption begins"),
                                                                            value = 5, min = 0, max = 100, step = 1),
                                                               numericInput("emp_lambda_A",
                                                                            create_info_tooltip("Adoption Growth", "Duration in years of adoption phase to reach maximum"),
                                                                            value = 5, min = 0, max = 100, step = 1),
                                                               numericInput("emp_lambda_M",
                                                                            create_info_tooltip("Adoption at Maximum", "Duration in years at maximum adoption level"),
                                                                            value = 5, min = 0, max = 100, step = 1),
                                                               numericInput("emp_lambda_D",
                                                                            create_info_tooltip("Adoption Decline", "Duration in years of decline phase until adoption ends"),
                                                                            value = 5, min = 0, max = 100, step = 1),
                                                               selectInput("emp_adoption_shape",
                                                                           create_info_tooltip("Adoption Shape", "Choose linear (trapezoidal) or sigmoidal (S-curve) adoption pattern over time"),
                                                                           choices = c("Linear" = "linear", "Sigmoidal" = "sigmoidal"),
                                                                           selected = "linear")
                                                           ),
                                                           
                                                           tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                                           
                                                           h5("Research Costs", style = "font-style: italic;"),
                                                           div(class = "inline-inputs",
                                                               numericInput("emp_research_cost_lag",
                                                                            create_info_tooltip("Yearly Cost During Lag", "Annual research cost during the lag period before adoption begins"),
                                                                            value = 1000, min = 0, step = 1000),
                                                               numericInput("emp_research_cost_growth",
                                                                            create_info_tooltip("Yearly Cost During Growth", "Annual research cost during the adoption growth period"),
                                                                            value = 500, min = 0, step = 1000)
                                                           )
                                                    ),
                                                    
                                                    #----------------------------------------------------------------------
                                                    # RIGHT COLUMN - R&D Parameters
                                                    #----------------------------------------------------------------------
                                                    column(6,
                                                           h5("R&D Parameters", style = "font-style: italic;"),
                                                           sliderInput("emp_prob_success",
                                                                       create_info_tooltip("Probability of Success", "Range of R&D success probabilities"),
                                                                       min = 0, max = 100, value = 50, step = 5, post = "%"),
                                                           sliderInput("emp_cost_saving",
                                                                       create_info_tooltip("Cost Savings", "Percentage cost reduction from successful R&D"),
                                                                       min = 0, max = 50, value = 10, step = 1, post = "%"),
                                                           sliderInput("emp_adoption_ceiling",
                                                                       create_info_tooltip("Adoption Ceiling", "Maximum market adoption percentage"),
                                                                       min = 0, max = 100, value = 50, step = 5, post = "%")
                                                    )
                                                  ),
                                                  
                                                  hr(),
                                                  div(align="center",
                                                      actionButton("tech_prev", "← PREVIOUS: Markets", class = "btn-secondary"),
                                                      actionButton("tech_next", "NEXT: Spillover →", class = "btn-primary")
                                                  )
                                         ),
                                         
                                         #----------------------------------------------------------------------
                                         # SPILLOVER TAB
                                         #----------------------------------------------------------------------
                                         tabPanel("Spillover",
                                                  h4("Research Spillover Configuration"),
                                                  p("Configure research spillover effects from one region to others (θ_ji). The first region typically has θ=1, others have 0 < θ < 1."),
                                                  tags$hr(style="border-top:1px dotted #999; margin:10px 0;"),
                                                  
                                                  fluidRow(
                                                    column(12,
                                                           DTOutput("spillover_table")
                                                    )
                                                  ),
                                                  
                                                  hr(),
                                                  div(align="center",
                                                      actionButton("spillover_prev", "← PREVIOUS: R&D and Adoption", class = "btn-secondary"),
                                                      actionButton("spillover_next", "RUN SIMULATION", class = "btn-primary")
                                                  )
                                         ),
                                         
                                         #----------------------------------------------------------------------
                                         # RESULTS TAB - Multiple simulations with tabs like closed economy
                                         #----------------------------------------------------------------------
                                         tabPanel("Results",
                                                  
                                                  conditionalPanel(
                                                    condition = "!output.show_empirical_results",
                                                    h4("Simulation Results"),
                                                    p("Configure parameters in the previous tabs and click 'RUN SIMULATION' to see results here."),
                                                    hr(),
                                                    div(align="center",
                                                        actionButton("results_prev", "← PREVIOUS: Spillover", class = "btn-secondary")
                                                    )
                                                  ),
                                                  
                                                  conditionalPanel(
                                                    condition = "output.show_empirical_results",
                                                    h3("Simulation Results"),
                                                    uiOutput("empirical_results_tabs_ui"),
                                                    hr(),
                                                    div(align="center",
                                                        actionButton("results_prev2", "← PREVIOUS: Spillover", class = "btn-secondary")
                                                    )
                                                  )
                                         )
                             )
                    )
        )
    )
  )
  )
}