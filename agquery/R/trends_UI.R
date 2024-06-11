trends_ui <- function(id) { 
ns <- NS(id)
  #tabPanel("Trends Explorer", 
tagList(
        #shinyjs::useShinyjs(),
         fluidRow(column(4, uiOutput(ns("trendsErr")))),
         fluidRow(column(4, uiOutput(ns('policiesBox1')))),
         fluidRow(column(4, uiOutput(ns('pathwaysBox1')))),
         fluidRow(column(2, uiOutput(ns('msgText')) #, column(4, conditionalPanel(condition="input.policiesBox1!='None'", radioGroupButtons("trendChooser", "", choices=list(`Change Since Previous Survey`='prevSurv', `Long-term Trend`='trend')))
         )),
         column(6, conditionalPanel(condition="input.policiesBox1!='None'", uiOutput(ns("trendVarChoose")))
         ),
         fluidRow(column(6, dataTableOutput(ns('trendsTable'))),
                  column(6,
                         plotOutput(ns('currMap')),
                         plotOutput(ns('trendMap')),
                         uiOutput(ns("plotsErr")))
         ),
         fluidRow(column(12, uiOutput(ns("droppedVars")))),
         fluidRow(column(6, br(), bsCollapse(
           bsCollapsePanel("Detailed Information",
                           dataTableOutput(ns('flagsTable')))
         )))
)
}


trends_server <- function(id, goalNames, policy_path, indicatorCategories, pathway_link, indicator_list) {
  moduleServer(id, function(input, output, session) {
    
     output$policiesBox1 <- renderUI({
        ns <- session$ns
      if(any(goalNames!="")){ 
        goalNames <- goalNames[goalNames!=""] #Cleaning
        selectInput(ns('policiesIn1'), "Select the Policy Priority:", choices=c("None", goalNames)) 
      } else {
        selectInput(ns('policiesIn1'), "Select the Policy Priority:", choices="None")
      }
      })
    
    
    observeEvent(input$policiesBox1, {
      if(input$policiesBox1!="None" & is.list(policy_path)){
        inputChk <- is.null(input$pathwaysIn1)
        pathway_sub <- policy_path %>% filter(goalName==input$policiesBox1)
        #pathway_sub <- pathway_link %>% filter(goalName==input$policiesBox1)
        pathway_list <- as.list(c(pathway_sub$pathwayID,0)) 
        names(pathway_list) <- c(pathway_sub$Pathway, "All")
        output$pathwaysBox1 <- renderUI({
          ns <- session$ns
          selectInput(ns("pathwaysIn1"), "Choose a pathway (optional)", choices=pathway_list) 
          })
        
        if(!inputChk){
          shinyjs::disable('pathwaysIn1')
          shinyjs::disable('policiesBox1')
          showNotification("Loading, please wait")
          trendsOut <- updateTrends(indicatorCategories, pathway_link, indicator_list)
          
          #output$msgText <- renderUI(HTML("<h3>Related Variables</h3>"))
          
          names(trendsOut$trendVarList) <- c("Select...", trendsOut$data_table$labelName)
          trendsOut$data_table <- trendsOut$data_table %>% rename(Variable=labelName) %>% select(-shortName)
          trendsOut$flag_table <- trendsOut$flag_table %>% rename(Variable=labelName, Notes=flag_text) %>% select(-shortName) %>% relocate(Notes, .after=last_col())
          output$trendsTable <- renderDataTable(data_table, options=list(searching=F, pageLength=15), rownames=F)
          output$flagsTable <- renderDataTable(flag_table, options=list(searching=F, pageLength=15), rownames=F)
          output$trendVarChoose <- renderUI({
            ns <- session$ns
            selectInput(ns('trendIn'), "Choose a variable to map:", choices=trendsOut$trendVarList)
            })
          
          shinyjs::enable('pathwaysIn1')
          shinyjs::enable('policiesBox1')
        }
      }
    })
    
    
    
    })
}