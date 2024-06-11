updateBoxes <- function(indics){
  output$indicsBox <- renderUI(selectInput('indicsIn', HTML("<b>Select Indicator</b>"), choices=indics, size=length(indics), selectize=F)) 
  output$corrsBox <- renderUI(selectInput('corrsIn', HTML('<b>Select Correlate</b>'), choices=indics, size=length(indics), selectize=F))
  groups_sub <- groups_list %>% filter(level=="All" | level==input$policiesIn2)
  output$groupsBtn <- renderUI(radioButtons("groupsChk", "Selecting Grouping Variable", choiceNames=c("None", groups_sub$label), choiceValues=c("", groups_sub$varName)))
}


comparisons_ui <- function(id, input, output, year_list, instrument_list) {
  ns <- shiny::NS(id)
  #tabPanel("Data Explorer",
  tagList(
         fluidRow(column(4,uiOutput(ns("explorerErr")))),
         fluidRow(column(12, uiOutput(ns('policiesBox2')))),
         fluidRow(column(12, uiOutput(ns("pathwaysBox2")))),
         #conditionalPanel(condition="input.policiesBox2!='None'",
                          fluidRow(column(4, uiOutput(ns('pathwaysBox2')))),
                          fluidRow(column(12, radioGroupButtons(ns('yearBtn'), label="Survey Year", choices=year_list, selected=max(instrument_list$year)))),
                          fluidRow(column(6, wellPanel(style="background-color: #ededed; border-color: #9c9c9c;",
                                                       
                                                       fluidRow(column(6, align='center', uiOutput('indicsBox')),
                                                                column(6, align='center', uiOutput('corrsBox'))),
                                                       fluidRow(column(6, align='center', uiOutput('indicsDesc')), column(6, align='center', uiOutput('corrsDesc'))),
                                                       hr(),
                                                       checkboxInput('yChk', 'Omit 0s from Indicator'),
                                                       radioButtons("disAgg_admin", HTML("<b>Select Administrative Level</b>"), choiceNames=c("Province","Household"), choiceValues=c("province", "hhid")),
                                                       uiOutput("groupsBtn"),
                                                       #radioButtons("groupsChk", "Selecting Grouping Variable", choiceNames=c("None", groups_list$label), choiceValues=c("", groups_list$varName)),
                                                       actionButton('submitBtn', "Compare Variables"))),
                                   column(6, 
                                          #plotOutput('corrPlot'),
                                          plotlyOutput('heatMap'))),
                          br(),
                          br(),
                          fluidRow(column(6, uiOutput('indicHeader')) ,column(6, uiOutput('corrHeader'))),
                          fluidRow(column(6, plotOutput('indicatorHist')), column(6, plotOutput('corrHist'))),
                          fluidRow(column(6, plotOutput('indicatorMap')), column(6, plotOutput('corrMap'))),
                          fluidRow(plotOutput('scatterPlot')),
                          fluidRow(uiOutput('plotInterp'))
         #)
)
}

comparisons_server <- function(id, goalNames) {
  moduleServer(id,
    function(input, output, session) {
      ns <- session$ns
      output$policiesBox2 <- renderUI({
      if(exists("goalNames")){ 
        selectInput(ns('policiesIn2'), "Select the Policy Priority:", choices=c("None", goalNames)) 
      } else {
        selectInput(ns('policiesIn2'), "Select the Policy Priority:", choices="None")
      }
      })
      
      
      
    })
}