options(shiny.error=browser,
        shiny.autoload.r=F) #For debugging 
library(shiny)
library(shinyBS)
library(tidyr)
library(shinythemes)
library(tools)
library(ggplot2)
library(dplyr)
library(cowplot)
library(stringr)
library(readstata13)
library(DT)
library(glue)
library(rlang)
library(shinyWidgets)
library(sf)
library(gridExtra)
library(ggdist)
library(openxlsx)
library(purrr)
library(rintrojs)
library(corrplot)
library(plotly)
library(bslib)
library(thematic)
library(ragg)
library(viridis)
library(heatmaply)
library(shinyjs)
library(reshape2)
library(ggtext)


lapply(list.files("Scripts", full.names=T), FUN=source)



thematic_shiny(
  font = "auto",
  sequential = colorRampPalette(colors = c("white", "#440154FF"))(12),
  qualitative = c("#440154FF",  "#21908CFF", "#3B528BFF", "#5DC863FF", "#FDE725FF")
)
options(shiny.useragg = TRUE)



ui <- fluidPage(theme=bslib::bs_theme(version="3", bg = "white", fg = "#3B528BFF", info="#474481", primary = "#440154FF",
                base_font = bslib::font_google("Open Sans")), 
                fluidRow(style="background-color:#cadafa;",
                         column(4, align='center', HTML("<br><img src=moa_logo.png width='300'></img>")),
                         column(4, fluidRow(HTML("<h1 style='text-align:center; font-weight:900;'>Cambodia Agricultural Survey Policy & Data Explorer</h1>")),
                                   fluidRow(HTML("<p style='text-align:center;'>(Version 0.1-Beta)</p>"))),
                         column(4, align='center', HTML("<br><image src=cam_flag.png width='200'></img>")),
                         ),
                fluidRow(style="background-color:#cadafa;", br()),

                navbarPage(title="", theme=bslib::bs_theme(version="3", bg = "white", fg = "#3B528BFF", info="#474481", primary = "#440154FF",
                                                           base_font = bslib::font_google("Open Sans")),
                           tabPanel("About", icon=icon("signs-post"), column(1),column(10, #To do: move this to a separate file.
                                                                                              wellPanel(HTML(
                                                                                                "The 50x30 Cambodia Data explorer can rapidly summarize and visualize the Cambodian Agricultural Survey data. It provides tools for exploring policy instruments to achieve agricultural development goals and connecting those instruments to information available in the CAS surveys. Export report-ready graphs and raw data for follow-up analyses. Inputs and interface elements are also user-modifiable for a custom data analysis environment."
                                                                                              )),
                                                                                              img(src='logic-model.png', width='80%'),
                                                                                              hr(),
                                                                                              fluidRow(column(8, HTML(paste('<h3>Purpose</h3><br><p>The 50x30 Cambodia Data Explorer bridges the gap between survey data collection and policy decisionmaking. It provides the opportunity to combine knowledge from scholarly research in agricultural policy with observed trends in variables collected in the field. These trends can inform progress toward established goals or aid in the formation of new programs. The results of those policies become visible in new data collection, which is added through updates. This version shows information related to the following policy priorities:',
                                                                                                                            '<ul>',
                                                                                                                            paste(lapply(pathway_names, FUN=function(x){paste0("<li>",x, "</li>")}), collapse=" "),
                                                                                                                            '</ul></p>',
                                                                                                                            '<h3>Code and Data Availability</h3>',
                                                                                                                            '<p> The Stata code used to process the data is publicly available at (Git Repository TBD). <br> The app source code and related files can be downloaded at (Git repository TBD)</p>',
                                                                                                                            '<p> The raw data for the 50x30 survey are located at <a href="https://nada.nis.gov.kh/index.php/catalog/36">https://nada.nis.gov.kh/index.php/catalog/36</a>.</p>',
                                                                                                                            '<h3>Using the Cambodia 50x30 App</h3> <p>The Cambodian Agricultural Survey contains information on household production of crops and livestock that can be used to understand trends in small-scale farmer contributions to national supply and the economic conditions small-scale producers face.</p>',
                                                                                                                            '<h3> Tabs </h3>',
                                                                                                                            '<h4> User Guide </h4>',
                                                                                                                            '<p>This tab provides step-by-step instructions, tips, and frequently asked questions (FAQs) about the data explorer.',
                                                                                                                            '<h4>Policy Goals and Instruments</h4>',
                                                                                                                            '<p>This tab overviews select academic and gray literature for policy pathways that can generate ideas for effective policies and programs that can help shift key variables of agricultural development</p>',
                                                                                                                            '<h4>Variable Maps and Statistics</h4>',
                                                                                                                            '<p>This tab shows summary statistics, year-over-year changes, and spatial distributions of variables related to the policy goals. Begin by selecting a policy goal, then optionally choose an instrument to show the most relevant variables for that instrument. Maps illustrate the province-level means as of the most recent survey and the change in means since the previous survey.</p>',
                                                                                                                            '<h4>Variable Correlations</h4>',
                                                                                                                            '<p>This tab allows for direct comparisons of variables and provides detailed graphs and summaries of correlations.</p>',
                                                                                                                            '<h4>Secondary Data Sources</h4>',
                                                                                                                            '<p>This tab contains a table of additional sources of useful data, such as import/export statistics, exchange rates, and food balances.</p>',
                                                                                                                            '<br>',
                                                                                                                            '<h3>Inquire</h3>',
                                                                                                                            '<p>This tool is maintained by <i>responsible party</i> who has <i>contact info</i>.</p><br>',
                                                                                                                            '<h3>Citation</h3>',
                                                                                                                            '<p>If you use this app for scholarly research or modify it for alternative uses, please use this attribution: </p>',
                                                                                                                            '<p> Tomes, A.L., Kenne, S., Wood, S.R., and Anderson, C.L. (2024). 50x30 Cambodia Data Explorer. v0.1. DOI: <a href="https://doi.org/10.6069/GPPQ-2X85">https://doi.org/10.6069/GPPQ-2X85</a>',
                                                                                                                            '<br><br>',
                                                                                                                            "<img src='evans2.jpg' width='30%' align='center'></img>",
                                                                                                                            "<br>&nbsp;"
                                                                                                                            )
                                                                                                                      #column(2, align='left', HTML("<img src='evans2.jpg' width='100%'></img>")))
                                                                                              )
                                                                                              )
                                                                                              )
                           )
                           ),
                           
                           tabPanel("Policy Goals and Instruments", icon=icon("landmark-dome"),

                                    fluidRow(HTML('<p><h3>Policy Instruments by Goal</h3></p>
                             <p>This table presents policy instruments (tax/subsidy, regulatory, information) in support of a particular goal, and the expected most direct effect on market price, quantity, quality and timeliness, followed by CAS variables and relevant evidence where available. The predicted changes in price and quantity assume competitive markets and do not consider intermediaries.</p><br>')
                                    #downloadButton('downloadPathways',
                                    #                 label='Download Policy Pathways',
                                    #                 icon=icon('file-csv'))
                                    ),
                                    fluidRow(HTML('<p><i>This reference set of variables may be extended and revised by suitably trained users through revisions to the source Excel file '),
                                             downloadLink('downloadPathways', label='here.'),
                                             HTML('See User Guide.</i></p><br>')),
                             fluidRow(uiOutput("path_table"), uiOutput("path_tbl_err"))
                           ),

                           tabPanel("Variable Maps and Statistics", icon=icon("magnifying-glass-chart"),
                                    shinyjs::useShinyjs(),
                                    fluidRow(HTML('<p><i>The variables summarized here may be extended and revised by suitably trained users by editing the source Excel files, including '),
                                             downloadLink('indicsDL1', label='the pathways table,'),  #this would be easier with modules
                                             downloadLink('indicsDL2', label='the indidicator list,'),
                                             downloadLink('indicsDL3', label='and/or the linking sheet.'),
                                             HTML('See User Guide.</i></p><br>')),
                                    fluidRow(column(4, uiOutput("trendsErr"))),
                                    #fluidRow(column(4, selectInput('policiesBox1', "Select a policy goal", choices=c("None", goalNames)))),
                                    fluidRow(column(4, selectInput('policiesBox1', "Select a policy goal", choices=c("None", goalNames)))),
                                    conditionalPanel(condition="input.policiesBox1!='None'", 
                                                     fluidRow(column(4, uiOutput('pathwaysBox'))),
                                                     fluidRow(column(4, radioGroupButtons('totsBtns', label="Choose Statistic to Present", choices=c("Mean","Total")))),
                                                     fluidRow(column(5, uiOutput('msgText')),
                                                              column(1),
                                                              column(6,  uiOutput("trendVarChoose"))
                                                     ),
                                                     fluidRow(column(6, dataTableOutput('trendsTable'),
                                                                     downloadButton('downloadSummary',
                                                                                    label='Download Table Data',
                                                                                    icon=icon('file-csv'))),
                                             column(6,
                                                    plotOutput('currMap'),
                                                    plotOutput('trendMap'),
                                                    #plotOutput('obsMap'),
                                                    plotOutput('timePlot'),
                                                    uiOutput("plotsErr"))),

                                    fluidRow(column(12, uiOutput("droppedVars"))),
                                    fluidRow(column(6, bsCollapse(
                                      bsCollapsePanel("Detailed Information",
                                                      dataTableOutput('flagsTable'),
                                                      downloadButton('downloadFlags',
                                                                     label='Download Table Data',
                                                                     icon=icon('file-csv')))
                                    ))))

                           ),
                           
                           tabPanel("Variable Correlations", icon=icon("chart-line"),
                                    fluidRow(HTML('<p><i>The variables summarized here may be extended and revised by suitably trained users by editing the source Excel files, including '),
                                             downloadLink('relsDL1', label='the pathways table,'),
                                             downloadLink('relsDL2', label='the indidicator list,'),
                                             downloadLink('relsDL3', label='and/or the linking sheet.'),
                                             HTML('See User Guide.</i></p><br>')),
                                    fluidRow(column(4,uiOutput("explorerErr"))),
                                    fluidRow(column(5, selectInput('policiesBox2', "Select a policy goal", choices=c("None", goalNames)))),
                                    conditionalPanel(condition="input.policiesBox2!='None'",
                                                     fluidRow(column(8, uiOutput('dataPathBox'))),
                                                     fluidRow(column(4, radioGroupButtons('yearBtn', label="Survey Year", choices=year_list, selected=max(instrument_list$year))),
                                                              column(8,actionButton('makeHeatMap',"Show Heatmap"))),
                                                     fluidRow(column(4, wellPanel(style="background-color: #ededed; border-color: #9c9c9c; padding=10;",
                                                                                  fluidRow(column(6, uiOutput('indicsBox')),
                                                                                           column(6, uiOutput('corrsBox'))),
                                                                                  fluidRow(column(6, align='center', uiOutput('indicsDesc')), column(6, align='center', uiOutput('corrsDesc'))),
                                                                                  hr(),
                                                                                  fluidRow(checkboxInput('yChk', 'Omit 0s from Y Variable')),
                                                                                  fluidRow(radioButtons("disAgg_admin", HTML("<b>Select Administrative Level</b>"), choiceNames=c("Province","Household"), choiceValues=c("province", "hhid"))),
                                                                                  fluidRow(uiOutput("groupsBtn")),
                                                                                  fluidRow(actionButton('submitBtn', "Compare Variables")),
                                                                                  fluidRow(HTML("&nbsp;")),
                                                                                  fluidRow(downloadButton('downloadRawShort', 'Download Selected Raw Data',icon=icon('file-csv')),
                                                                                           downloadButton('downloadRawLong', 'Download All Listed Raw Data', icon=icon('file-csv')))
                                                     )
                                                     ),
                                                     column(8,
                                                            #plotOutput('corrPlot'),
                                                            plotlyOutput('heatMap'))),
                                                     fluidRow(HTML("&nbsp;")),
                                                     fluidRow(HTML("&nbsp;")),
                                                     fluidRow(column(6, uiOutput('indicHeader')) ,column(6, uiOutput('corrHeader'))),
                                                     fluidRow(column(6, plotOutput('indicatorHist')), column(6, plotOutput('corrHist'))),
                                                     fluidRow(column(6, plotOutput('indicatorMap')), column(6, plotOutput('corrMap'))),
                                                     fluidRow(plotOutput('scatterPlot')),
                                                     fluidRow(uiOutput('plotInterp'))
                                    )
                           ),
                           # tabPanel("Reporting: Fast", icon=icon("newspaper"),
                           #          fluidRow(selectInput("lsChoose", choices=c("Chickens","Pigs","Cattle","Buffalo")), actionButton("newRept", "Generate Report")),
                           #          fluidRow(plotOutput("rep1plot"), plotOutput("rep1map")),
                           #          fluidRow(plotOutput("rep2plot"), plotOutput("rep2map")),
                           #          fluidRow(plotOutput("rep3plot"), plotOutput("rep3map")),
                           #          fluidRow(plotOutput("rep4plot"), plotOutput("rep4map"))
                           # ),
                           # tabPanel("Reporting: Pretty", icon=icon("newspaper"),
                           #          fluidRow(verbatimTextOutput("Placeholder")),
                           #          valueBox(title="Livestock Ownership Rate, Among Agricultural Holdings", showcase=textOutput("vb1text")),
                           #          valueBox(title="Livestock Ownership Rate, Among Livestock-raising holdings", showcase=textOutput("vb2text")),
                           #          valueBox(title="Percentage of Holdings Raising Livestock Among Agricultural Households", showcase=plotOutput("vb3plot")),
                           #          valueBox(title="Percentage of Holdings Raising Livestock Among Livestock Households", showcase=plotOutput("vb4plot"))
                           #          
                           #          #fluidRow(valueBox(title="Livestock Production Rates", 
                           #          #                  value=textOutput("vb1Text"),
                           #          #                  showcase=plotlyOutput("repPlot1")), 
                           #          #         plotOutput("vb1_map"))
                           #          ),
                           tabPanel("Secondary Data Sources", icon=icon("database"),
                                    fluidRow(HTML("<p>This table shows additional sources of contextual information. Updates can be made by downloading the "),
                                    downloadLink("secSourcesDL", "associated spreadsheet."), HTML("</p>")),
                                    fluidRow(DTOutput('secsources'))
                          ),
                          tabPanel("User Guide", icon=icon("readme"),
                                   includeHTML('www/Instructions_50x30_D2.html')
                          )
                )

)

server <- function(input, output, session) {
  corMat <- function(shortNames, labelNames, data_out){
    cor_matrix <- cor(data_out, use="pairwise.complete.obs")
    par(mar = c(5, 5, 4, 2) - 2)
    #corrPlot <- corrplot.mixed(cor_matrix, order = 'AOE')
    #output$corrPlot <- renderPlot(corrplot(cor_matrix, order = 'AOE',col=colorRampPalette(c("white","lightblue","red"))(100)))
    #print(corrPlot) 
    res <- match(rownames(cor_matrix), shortNames)
    rownames(cor_matrix) <- labelNames[res]
    res <- match(colnames(cor_matrix), shortNames)
    colnames(cor_matrix) <- labelNames[res]
    #print(cor_matrix)
    p_matrix <- matrix(nrow = ncol(data_out), ncol = ncol(data_out))
    for(i in seq_len(ncol(data_out))) {
      for(j in seq_len(ncol(data_out))) {
        test_result <- cor.test(data_out[, i], data_out[, j], method = "pearson")
        p_matrix[i, j] <- test_result$p.value
      }
    }
    #print(p_matrix)
    p_matrix[upper.tri(p_matrix)] <- NA
    hover_text <- matrix("", nrow = ncol(data_out), ncol = ncol(data_out))
    for(i in 1:nrow(p_matrix)) {
      for(j in 1:ncol(p_matrix)) {
        if (!is.na(p_matrix[i, j])) {
          cor_value <- cor_matrix[i, j]
          p_value <- p_matrix[i, j]
          # Construct the hover text
          if (p_value>=0.00001) {
            hover_text[i, j] <- paste0("P-value: ", format(p_value, digits = 3))
          }
          if (p_value<0.00001) {
            hover_text[i, j] <- paste0("P-value: ", "<0.00001")
            p_matrix[i,j] <- 0.00001
          }
        }
      }
    }
    #print(hover_text)
    #cor_matrix[upper.tri(cor_matrix)] <- NA
    hover_text[upper.tri(hover_text)] <- NA
    heatMap <- heatmaply_cor(cor_matrix,
                             node_type = "scatter",
                             point_size_mat = -log10(p_matrix),
                             point_size_name = "-log10(p-value)",
                             label_names=c("Row", "Column", "Correlation"),
                             custom_hovertext = hover_text,
                             Colv=NA, Rowv=NA, plot_method="ggplot") %>%
      layout(title = "Correlation Heatmap", margin = list(t = 60), height=1000)
    return(heatMap)
  }

  
  cor.test.p <- function(x){
    FUN <- function(x, y) cor.test(x, y)[["p.value"]]
    z <- outer(
      colnames(x), 
      colnames(x), 
      Vectorize(function(i,j) FUN(x[,i], x[,j]))
    )
    dimnames(z) <- list(colnames(x), colnames(x))
    z
  }
  
  observeEvent(input$newRept, {
    lsVars <- c("num_",
                "peak_num_",
                "vax_pct_",
                "sale_price_")
    #aggs_list <- if(length(input$repGroups)>0) input$repGroups else ""
    
    
    data_out <- getData(lsvars)
    
    means_out <- data_out$means_out 
    totals_out <- data_out$totals_out 
    
    #output$vb3plot <- 
    
  })
  
  output$secsources <- renderDT(ext_data, escape=F, options=list(dom="t"), rownames=F)
  
  observeEvent(input$indicsIn, {
    if(with(indicator_list, exists(paste0("survey_question_", input$yearBtn)))){
    output$indicsDesc <- renderUI(HTML(sprintf('<table style="border: 3px #ddd; border-style: groove; padding: 9px;">
                                               <tr><td style="border: 3px #ddd; border-style: groove; padding: 9px;">%s</td></tr>
                                               <tr><td style="border: 3px #ddd; border-style: groove; padding: 9px;">%s</td></tr>
                                               ',
                                               #<tr><td style="border: 3px #ddd; border-style: groove; padding: 9px;">Mean: %s</td><tr>
                                               #<tr><td style="border: 3px #ddd: border-style: groove; padding: 9px;">Stdev: %s</td><tr>
                                               indicator_list[[paste0("survey_question_", input$yearBtn)]][indicator_list$shortName==input$indicsIn], 
                                               indicator_list$ques_text[indicator_list$shortName==input$indicsIn] 
                                               #round(weighted.mean(na.omit(data[[input$indicsIn]]), data[["weight"]][which(!is.na(data[[input$indicsIn]]))]), 2),
                                               #round(sd(na.omit(data[[input$indicsIn]])),2)
    )))
    } else {
      ouput$indicsDesc <- renderUI(verbatimTextOutput("Survey details not found for selected indicator."))
    }
  })
  
  observeEvent(input$corrsIn, {
    if(with(indicator_list, exists(paste0("survey_question_", input$yearBtn)))){
    output$corrsDesc <- renderUI(HTML(sprintf('<table style="border: 3px #ddd; border-style: groove; padding: 9px;">
                                              <tr><td style="border: 3px #ddd; border-style: groove; padding: 9px;">%s</td></tr>
                                              <tr><td style="border: 3px #ddd; border-style: groove; padding: 9px;">%s</td></tr>', 
                                              indicator_list[[paste0("survey_question_", input$yearBtn)]][indicator_list$shortName==input$corrsIn], 
                                              indicator_list$ques_text[indicator_list$shortName==input$corrsIn])))
    } else {
      ouput$corrsDesc <- renderUI(verbatimTextOutput("Survey details not found for selected indicator."))
    }
  })
  
  
  #fluidRow(column(6, align='center', selectInput('indicsIn', HTML("<b>Select Indicator</b>"), choices=indics, size=length(indics), selectize=F),
  #                #                uiOutput('indicsDesc'), 
  #                #column(6, pickerInput('corrsIn', HTML('<b>Select Correlate</b>'), choices=indics, options=list(style='btn-info', size=length(indics))))),
  #                #column(6, align='center', selectInput('corrsIn', HTML('<b>Select Correlate</b>'), choices=indics, size=length(indics), selectize=F),
  
  updateBoxes <- function(indics){
    output$indicsBox <- renderUI(selectInput('indicsIn', HTML("<b>Select Y Variable</b>"), choices=indics)) #, size=length(indics) , selectize=F)) 
    output$corrsBox <- renderUI(selectInput('corrsIn', HTML('<b>Select X Variable</b>'), choices=indics)) #, size=length(indics), selectize=F))
    groups_sub <- groups_list %>% filter(level=="All" | level==input$policiesBox2)
    output$groupsBtn <- renderUI(radioButtons("groupsChk", "Selecting Grouping Variable", choiceNames=c("None", groups_sub$label), choiceValues=c("", groups_sub$varName)))
  }

  
  #data_table_out <- observe({makeDataTable(input$policiesBox1, indicatorCategories, indicator_list, dataset_list)})
  
   observeEvent(input$policiesBox1, {
     if(input$policiesBox1!="None" & is.list(policy_path)){
     inputChk <- is.null(input$pathwaysIn1)
       #pathway_sub <- policy_path %>% filter(goalName==input$policiesBox1)
       #pathway_list <- as.list(c(0, pathway_sub$pathwayID))
       #names(pathway_list) <- c("All", pathway_sub$Pathway)
       output$pathwaysBox <- renderUI(selectInput("pathwaysIn1", "Choose a pathway (optional)", choices=polic_Names[[input$policiesBox1]]))
         shinyjs::disable('pathwaysIn1')
         shinyjs::disable('policiesBox1')
         showNotification("Loading, please wait")
         data_table_out <<- makeDataTable(input$policiesBox1, indicatorCategories, indicator_list, dataset_list) #Need to change some names here
         
         shinyjs::enable('pathwaysIn1')
         shinyjs::enable('policiesBox1')
         updateVarTable()
   }
   }, ignoreInit=T)
   
   #observeEvent(input$pathwaysIn1, {
  #   updateVarTable()
  # }, ignoreInit=T)

  # observeEvent(input$pathwaysIn1, {
  #   shinyjs::disable('pathwaysIn1')
  #   shinyjs::disable('policiesBox1')
  #   showNotification("Loading, please wait")
  #   updateTrends() # TO DO: Find a way to avoid recalculating this every time
  #   shinyjs::enable('pathwaysIn1')
  #   shinyjs::enable('policiesBox1')
  # }) 
   
   #ALT NOTE TO ADD ERROR HANDLING HERE.
 output$msgText <- renderUI(HTML("<h3>Variable Summary Table</h3><br><p><i>This table presents household-level averages or totals of all CAS respondents.</i></p>"))
  
 makeDataTable <- function(policiesIn, indicatorCategories, indicator_list, dataset_list){
 #makeDataTable <- reactive({ 
   indics_out <- indicatorCategories %>% filter(goalName==policiesIn) %>% select(shortName) %>% distinct() %>% unlist()
    indics_out <- indicator_list$shortName[which(str_to_lower(indicator_list$shortName) %in% str_to_lower(indics_out))] %>% unique() #TO DO: Include some cleaning code in the startup script 
    denoms <- getDenoms(indics_out, indicator_list)
    data_files <- getFiles(indicator_list, dataset_list, c(indics_out, denoms$denominator))
    
    if(nrow(data_files)==0){
      showNotification("No data files related to the selected pathway were found", type="error")
    } else {
      data <- getData(files=data_files, xvars=indics_out, denoms=denoms, adm_level=NA, source_call="pathwaysIn1")
      if(is.list(data)){
        if(is.list(data$droppedVars)){
          #output$droppedVars <- renderText(paste("The following variables were missing from the indicators_list spreadsheet or were all NA and were not processed:", paste(unique(dropped_vars), collapse=", ")))
        }
        
        #Still need to making the naming less stupid here.
        data_out <- data$outdata
        
        #if(totsBtns=="Means") { 
        #  data_out <- data$nat_means
        #} else {
        #  data_out <- data$nat_tots
        #}
        
        #values_source <- input$totsBtns
        
        #indics_out <- names(data_out)[which(names(data_out) %in% indics_out)] #filter out any variables that weren't processed
        #data_table <- data.frame(shortName=indics_out) #TODO: Simplify
        #flag_table <- data_table #make a copy for metadata.
        #flag_table[[paste0(min(data_out$year), " N obs")]] <- NA
        #flag_table[[paste0(max(data_out$year), " N obs")]] <- NA
        flag_table <- data_out %>% pivot_wider(id_cols="shortName", names_from="year", values_from="Obs", names_glue="{year} N obs")
        
        flag_table <- merge(flag_table, indicator_list %>% select(shortName, labelName, flag_text), by="shortName")
        
        data_table <- merge(data_out, indicator_list %>% select(shortName, labelName, units), by="shortName")
        data_table <- data_table %>% select(shortName, labelName, year, units, matches(input$totsBtns))
        
       
        data_table <- data_table %>% filter(year==min(data_table$year) | year==max(data_table$year)) %>%
            pivot_wider(id_cols=c("shortName", "labelName","units"), names_from="year", values_from=input$totsBtns, names_glue="{year} {.value}") %>%
          rename(Variable=labelName, Units=units)
        
        
        #data_table$Units <- ""
        #data_table[[paste0(min(data_out$year), " Mean")]] <- NA #Column 4
        #data_table[[paste0(max(data_out$year), " Mean")]] <- NA #Column 5
        #data_table$`Annual Change` <- NA #Column 6
        #data_table$`Long Term Trend` <- NA #Column 7
        
        #for(var in indics_out){
        #  sub_data <- data_out %>% select(all_of(c(var, "year", "weight"))) %>% na.omit()
        #  if(nrow(sub_data)==0 | !is.numeric(sub_data[[var]])){
        #    next
        #  } else {
        #    data_table$Units[data_table$shortName==var] <- indicator_list$units[indicator_list$shortName==var]
        #    if(length(unique(sub_data$year))<2){
        #      year <- unique(sub_data$year)
        #      data_table[[paste0(year, " Mean")]][data_table$shortName==var] <- signif(inject(with(sub_data,weighted.mean(!!sym(var), weight))),4)
        #      flag_table[[paste0(year, " N obs")]][flag_table$shortName==var] <- nrow(sub_data)
        #      data_table[data_table$shortName==var, 6] <- "N/A"
        #    } else if(length(unique(sub_data$year>=2))) {
        #      prevYear <- max(sub_data$year[sub_data$year!=max(sub_data$year)]) 
        #      min_mean <- inject(with(sub_data %>% filter(year==prevYear), weighted.mean(!!sym(var), weight)))
        #      max_mean <- inject(with(sub_data %>% filter(year==max(sub_data$year)), weighted.mean(!!sym(var), weight)))
        #      min_n <- nrow(sub_data %>% filter(year==prevYear))
        #      max_n <- nrow(sub_data %>% filter(year==max(sub_data$year)))
        #      if(min_mean==0){
        #        if(max_mean > 0){
        #          chg = Inf
        #        } else {
        #          chg = -Inf
        #        }
        #      } else {
        #        chg=signif((max_mean-min_mean)/min_mean, 2)
        #      }
        #      
        #      data_table[data_table$shortName==var, 4] <- signif(min_mean,4)
        #      data_table[data_table$shortName==var, 5] <- signif(max_mean,4)
        #      flag_table[[paste0(min(data_out$year), " N obs")]][flag_table$shortName==var] <- min_n
        #      flag_table[[paste0(max(data_out$year), " N obs")]][flag_table$shortName==var] <- max_n
        #      data_table[data_table$shortName==var, 6] <- chg
        #    }
            # 
        data_table <- data.frame(data_table)
        names(data_table) <- str_replace_all(names(data_table), "X", "")
        names(data_table) <- str_replace_all(names(data_table), ".", " ")
        data_table$Trend <- signif((data_table[,5]-data_table[,4])/data_table[,4], 4)
        
            # I'll fix this later. 
        
            # reg_data <- sub_data %>% na.omit() %>% group_by(year) %>% summarize(mean=weighted.mean(!!sym(var), weight))
            # reg_data$mean <- with(reg_data, log(mean+0.5*min(mean[mean>0])))
            # if(length(unique(reg_data$year))>2){ #Future releases should change this to >2; currently here for testing
            #   reg_res <- tryCatch(lm(mean~year, data=reg_data), error=function(e){return("")})
            #   if(is.list(reg_res)){
            #     pct_diff <- round((exp(reg_res$coefficients[[2]])-1),3)
            #     if(!is.na(pct_diff)){
            #       data_table[data_table$shortName==var, 7] <- pct_diff
            #     }
            #   }
            # }
          #}
        #}
        #
        #if(all(is.na(data_table$`Long Term Trend`))){
        #  data_table <- data_table %>% select(-`Long Term Trend`)
          pct_col <- 5 #because shortname gets dropped; this is dumb
        #} else {
        #  pct_cols <- c(5,6)
        #}
        
        #trendVarList <- as.list(c("0", data_table$shortName))
        #names(trendVarList) <- c("Select...", data_table$labelName)
        #data_table <- data_table %>% rename(Variable=labelName)
        flag_table <- flag_table %>% rename(Variable=labelName, Notes=flag_text) %>% relocate(Notes, .after=last_col())
        
        #data_table_out <<- data_table #Save this to the global environment to make it accessible to the download handler. 
        #flag_table_out <<- flag_table 
        #odd workaround because format isn't working with index selection
        
        
        data_table[,4:5] <- format(data_table[,4:5], big.mark=',', scientific=F, digits=4, nsmall=0, drop0trailing=T)
        # dt_names_adj <- names(data_table)[which(str_detect(names(data_table), input$totsBtns))] 
        # for(dtname in dt_names_adj) {
        #   data_table[[dtname]] <- format(data_table[[dtname]], big.mark=',', scientific=F, digits=4, nsmall=0, drop0trailing=T)
        # }
        # #output$trendsTable <- renderDataTable(data_table)
        return(list(data_table=data_table, flag_table=flag_table, pct_cols=pct_col))
      }
    
    }
  }

  filterVarTable <- function(dt_out, pathway_link, pathwayTarget, indicator_list){
    if(pathwayTarget==0){
      return(dt_out)
    } else {
    indics_out <- pathway_link %>% filter(pathwayID==pathwayTarget) %>% select(shortName) %>% distinct()
    return(indics_out <- merge(indics_out, dt_out, by="shortName"))
    }
  }
  
  updateVarTable <- function(){
    pathwaysIn <- if(is.null(input$pathwaysIn1)){
      0
    } else {
      input$pathwaysIn1
    }
  output$trendsTable <- renderDataTable(
     DT::datatable(filterVarTable(data_table_out$data_table, pathway_link, pathwaysIn, indicator_list) %>% select(-shortName), 
                   options=list(searching=F, pageLength=15, dom='tip'), rownames=F)  %>%
                                          formatPercentage(data_table_out$pct_cols)
     )
  output$flagsTable <- renderDataTable(
    DT::datatable(
        filterVarTable(data_table_out$flag_table, pathway_link, pathwaysIn, indicator_list) %>% select(-shortName), 
        options=list(searching=F, pageLength=15), rownames=F)
  )
  output$trendVarChoose <- renderUI({
    dt_out <- filterVarTable(data_table_out$data_table, pathway_link, pathwaysIn, indicator_list) 
    trendVarList <- as.list(c("0", dt_out$shortName))
    names(trendVarList) <- c("Select...", dt_out$Variable)
   selectInput('trendIn', "Choose a variable to map:", choices=trendVarList)
  })
}

  observeEvent(input$trendIn, { #probably a future efficiency update to do here.
    if(input$trendIn!="0"){
      showNotification("Processing, please wait")
      #session$sendCustomMessage("disableButton", "start_proc")
      shinyjs::disable('trendIn')
      adm_level_in="province" #Adjustable
      
      denoms <- getDenoms(input$trendIn, indicator_list)
      data_files <- getFiles(indicator_list, dataset_list, input$trendIn) #AT: There's probably a simpler way to pack all of this into the getData function but that's a do later item. 
      tempdata <- getData(data_files, xvars=input$trendIn, denoms=denoms, adm_level=adm_level_in, source_call="trendmaps")
      
      if(is.list(tempdata)){ #To do: better error handling
        data_out <- tempdata$outdata %>% select(all_of(adm_level_in, "year", input$totsBtns)) #Find a way to kill mapdata?
        
        # if(totsBtns=="Means"){
        # data_out <- tempdata$means_out 
        # nat_out <- tempdata$nats_out
        # } else {
        #   data_out <- tempdata$tots_out
        #   nat_out <- tempdata$tots_out %>% group_by(year) %>% summarize(across(where(is.numeric), sum)) #Include disaggregates later?
        # }
        
        #data_out$province_num <- tryCatch(as.numeric(data_out$province), error=function(e){
        #  return(data_out %>% mutate(province=as.numeric(factor(province))))
        #})
        
        n_row <- nrow(data_out) 
        data_out <- na.omit(data_out)
        data_out <- data_out[data_out$province!="",]
        if(nrow(data_out) < n_row){
          output$plotsErr <- renderUI(HTML("<i>Note: some observations removed due to missing province information</i>"))
        }
        max_year <- max(data_out$year)
        min_year <- min(data_out$year)
        
        if(min_year!=max_year){
          df_min_year=data_out %>% filter(year==min_year)
          df_max_year=data_out %>% filter(year==max_year)
          diff <- data_out %>% pivot_wider(names_from=year, values_from=input$totsBtns)
          diff[,4] <- diff[,3]-diff[,2]
          names(diff)[[4]] <- input$trendIn
          #diff$province_num <- df_max_year$province_num
          #Temp fix because province variable keeps changing
          if(is.numeric(df_max_year$province)){
            xShp_currMap <- merge(khm_shp, df_max_year, by="province", all.x=T)
            xShp_trendMap <- merge(khm_shp, diff, by="province", all.x=T)
            
          } else {
            xShp_currMap <- merge(khm_shp, df_max_year, by.x="ADM1_EN", by.y="province", all.x=T) #changed y from province_num to province. Issue with the province_num not following alphabetical order meaning a numerical merge isn't good.
            xShp_trendMap <- merge(khm_shp, diff, by.x="ADM1_EN", by.y="province", all.x=T)
            
          }
          
          currMap <- monoColorMap(xShp_currMap, input$trendIn, paste0(indicator_list$labelName[indicator_list$shortName == input$trendIn], ", ", max_year, " Values"), indicator_list$units[indicator_list$shortName==input$trendIn])
          trendMap <- biColorMap(xShp_trendMap, input$trendIn, paste0(indicator_list$labelName[indicator_list$shortName == input$trendIn], ", ", min_year, " - ", max_year, " Trend"), indicator_list$units[indicator_list$shortName==input$trendIn])
          timePlot <- timeSeriesPlot(data_out, input$trendIn)
          output$currMap <- renderPlot(currMap)
          output$trendMap <- renderPlot(trendMap)
          #output$timePlot <- renderPlotly(timePlot)
        } else {
          showNotification("No trends to show for selected variable", type="warning")
        }
      } else {
        showNotification("Error getting data", type="error")
      }
      shinyjs::enable('trendIn')
    }
  })
  
  #Update the maps when a new variable is selected: we need two maps, one with the current variable and one with the difference.
  #observeEvent(input$trendIn, {
  #  mapdata <- getData() #TODO: There's probably a more efficient way to do this using reactive values.
  #})
  
  updateTrendsTab <- function(){
    #Placeholder!
  }
  
  
  #The idea here is that we want to keep the policies aligned so that people don't have to toggle back and forth, but that generates a major computational load given all the things conditioned on these boxes right now. Commenting it out, will come back to it when conditions improve.
  #observeEvent(input$policiesBox2, {
  #  updateSelectInput(session, "policiesBox1", selected=input$policiesBox2)
  #})
  
  
  observeEvent(input$submitBtn, {
    #updatePlots(maps=T)
    showNotification("Processing...")
    data_files <- getFiles(indicator_list, dataset_list, c(input$indicsIn, input$corrsIn)) %>% filter(year==input$yearBtn) #To fix, probably roll year into getFiles function. 
    aggs_list <- input$groupsChk #ALT Note: Right now this is an unnecessary step, but if we ever end up needing to have multiple disaggregation criteria, it's probably better to do it this way.
    all_data <- getData(data_files, xvars=input$corrsIn, yvars=input$indicsIn, adm_level=input$disAgg_admin, aggs_list=aggs_list, source_call="explorer", drop_0s = input$yChk)
    if(any(all_data!="")){
      #else if(tab=="trend"){
      #ALT - might be easier than what we do now with the maps in a separate area. Maybe build out later.
      #}
      mapdata <- all_data$mapdata
      outdata <- all_data$means_out #Note to go back and fix the naming here.
      outdata <- na.omit(outdata)
      if(nrow(outdata)==0){
        showNotification("Error: No non-n/a observations in dataset", type="error") 
      } else { 
        
        xvars = input$corrsIn
        yvars = input$indicsIn
        if(!all(c(xvars, yvars) %in% names(outdata))){
          showNotification("Error: one or both variables is missing from the dataset. Did you capitalize everything the same way?")
        } else {
          output$indicHeader <- renderUI(HTML(sprintf('<div style="border: 1px solid #ddd; padding: 9px; margin-bottom: 0px; line-height: 1.2; text-align: center; border-radius: 3px;"> %s </div>'
                                                      , indicator_list$labelName[indicator_list$shortName==input$indicsIn])))
          output$corrHeader <- renderUI(HTML(sprintf('<div style="border: 1px solid #ddd; padding: 9px; margin-bottom: 0px; line-height: 1.2; text-align: center; border-radius: 3px;"> %s </div>'
                                                     , indicator_list$labelName[indicator_list$shortName==input$corrsIn])))
          adm_level <- input$disAgg_admin
          varslist <- c(xvars, yvars)
          bins <- ifelse(adm_level=="province", 6, 30)
          #heatmapdata <- getData()$tempheatmapdata
          #outdata <- heatmapdata %>% select(all_of(c(xvars,yvars)))
          xlab <- indicator_list$labelName[indicator_list$shortName==xvars]
          ylab <- indicator_list$labelName[indicator_list$shortName==yvars]
          
          corrAxis <- indicator_list$axisName[indicator_list$shortName==xvars]
          indicAxis <- indicator_list$axisName[indicator_list$shortName==yvars]
          
          res <- eval(parse_expr(sprintf("with(outdata, cor.test(%s, %s))", xvars, yvars)))
          if(is.na(res$p.value)){
            res_out <- ""
            
          } else {
          if(res$p.value <= 0.01){ 
            adj="<span style='color: #44ce1b;'>very high</span>"
          } else if(res$p.value <= 0.05) {
            adj="<span style='color: #bbdb44;'>high</span>"
          } else if(res$p.value <= 0.1) {
            adj="<span style='color: #f7e379;'>moderate</span>"
          }  else if(res$p.value <= 0.2) {
            adj="<span style='color: #f2a134;'>low</span>"
          } else {
            adj = "<span style='color: #e51f1f;'>no</span>"
          }
          
          res_out <- sprintf("<span style='font-size: 20px;'>There is %s%% (%s%% - %s%%) correlation between <span style='color: #0a2167;'><b>%s</b></span> and <br><span style='color: #0a2167;'><b>%s</b></span>. There is %s confidence in this result.</span>", 
                             round(res$estimate[[1]]*100, 1), round(res$conf.int[[1]]*100, 1), round(res$conf.int[[2]]*100, 1),
                             xlab, ylab, adj)
          }
          
          if(input$groupsChk==""){
            #function(outdata, yvars, bins, indicAxis, titleLab){
            corrHist <- makeHist(outdata, xvars, bins, corrAxis,  xlab)
            indicatorHist <- makeHist(outdata,yvars,bins,indicAxis, ylab)
            scatterPlot <- makeScatter(outdata, xvars, yvars, xlab, ylab, res_out)
          } else {
            aggs_lab = groups_list$shortName[groups_list$varName==aggs_list]
            if(!is.factor(outdata[[aggs_list]])){
              flevels = groups_list[which(groups_list$varName==aggs_list),]$Levels %>% str_split(., ",") %>% unlist()
              flabels = groups_list[which(groups_list$varName==aggs_list),]$Labels %>% str_split(., ",") %>% unlist()
              outdata[[aggs_list]] <- factor(outdata[[aggs_list]], levels=flevels, labels=flabels)
            }
            #makeHistGrps <- function(outdata, yvars, bins, aggs_list, indicAxis, titleLab, aggs_lab)
            corrHist <- makeHistGrps(outdata, xvars, bins, aggs_list, corrAxis, xlab, aggs_lab)
            indicatorHist <- makeHistGrps(outdata,yvars,bins,aggs_list,indicAxis, ylab, aggs_lab)
            scatterPlot <- makeScatterGrps(outdata,xvars,yvars,aggs_list,xlab,ylab,aggs_lab, res_out)
          }
            corrTitle <- paste("Map of", indicator_list$labelName[indicator_list$shortName == xvars], "by Province")
            corrUnits <- indicator_list$units[indicator_list$shortName==xvars]
            
            indicTitle <- paste("Map of", indicator_list$labelName[indicator_list$shortName == yvars], "by Province")
            indicUnits <- indicator_list$units[indicator_list$shortName==yvars]
            
            if(is.numeric(mapdata$province)){
              mapdata <- merge(khm_shp, mapdata, by="province", all.x=T)
            } else {
              mapdata <- merge(khm_shp, mapdata, by.x="ADM1_EN", by.y="province", all.x=T)
            }
            
            if((min(na.omit(mapdata[[xvars]])) < 0) & (max(na.omit(mapdata[[xvars]])) > 0)){ 
              corrMap <- biColorMap(mapdata, xvars, corrTitle, corrUnits) 
            } else {
              corrMap <- monoColorMap(mapdata, xvars, corrTitle, corrUnits)
            }
            
            if(min(na.omit(mapdata[[yvars]])) < 0 & max(na.omit(mapdata[[yvars]])) > 0){
              indicatorMap <- biColorMap(mapdata, yvars, indicTitle, indicUnits) 
            } else {
              indicatorMap <- monoColorMap(mapdata, yvars, indicTitle, indicUnits)
            }
          
          
          output$indicatorHist <- renderPlot(indicatorHist)
          output$corrHist <- renderPlot(corrHist)
          output$scatterPlot <- renderPlot(scatterPlot)
          output$corrMap <- renderPlot(corrMap)
          output$indicatorMap <- renderPlot(indicatorMap)
          #output$plotInterp <- renderUI(HTML(res_out))
          
        }
      }
    }
  })
  
 
  
  observeEvent(input$makeHeatMap, {
    if(input$policiesBox2=="None"){
       showNotification("Please select a policy priority first") 
    } else {
      if(is.list(pathway_link) & is.list(indicator_list)) {
        indics_out <- getIndics(pathway_link %>% filter(pathwayID!=0), indicator_list, indic_inventory, input$policiesBox2, input$pathwaysIn2, input$yearBtn)
        indics_out <- unlist(indics_out)
        indics_out <- data.frame(shortName=indics_out)
        indics_out <- merge(indics_out, indicator_list, by="shortName")
      data_files <- getFiles(indicator_list, dataset_list, indics_out$shortName) %>% filter(year==input$yearBtn)
      #data_files_select <- indics_out %>% 
      #  select(file) %>% 
      #  distinct() %>%
      #  unlist() #Using tolower here helps filter out differences in capitalization 
      #survey_pref <- indics_out$survey[indics_out$year==input$yearBtn] # TO FIX; this line no longer does anything.
      #data_files <- lapply(data_files_select, FUN=function(x){dataset_list[which(str_detect(str_to_lower(dataset_list), str_to_lower(x)))]}) %>% unique() %>% unlist()  #Drop duplicates if they're somehow in there.
      #data_files <- dataset_list %>% select(which(str_to_lower(dataset_list) %in% str_to_lower(data_files_select)))
      #data_files <- dataset_list[which(str_detect(str_to_lower(dataset_list), str_to_lower(data_files_select)))] %>% as.data.frame()
      #data_files <- as.data.frame(data_files)
      #names(data_files) <- "file.name"
      #data_files$year <- str_extract(data_files$file.name, "[0-9]{4}") #Might be unnecessary 
      #data_files <- filter(data_files, year==input$yearBtn)
      
      for(file in data_files$file.name){
        #infile <- list.files("Data", sprintf("%s_%s_%s", survey_pref, input$yearBtn, file), ignore.case=T, full.names=T) #this differs from the other file loading subroutine in getData - should probably make them consistent.
        #if(length(infile)!=0){
        temp <- read.csv(paste0("Data/",file))
        if(!exists("data_out")){
          data_out <- temp
        } else {
          temp <- temp %>% select(all_of(c(names(temp)[which(!(names(temp) %in% names(data_out)))], "hhid"))) #Fix for redundant input.
          data_out <- merge(data_out, temp, by="hhid")
        }
      }
      #} 
      #data_out <- data_out %>% mutate(indicatorCategory=tolower(indicatorCategory)) %>% subset(indicatorCategory==target_policy, select=all_of(indicator_list$shortName)) %>% na.omit()
      if(exists("data_out")){
        indics <- as.list(indics_out$shortName)
        indic_shortNames <- unlist(indics, use.names=F)
        data_out <- data_out %>% select(any_of(indic_shortNames)) #Won't throw an error if names are missing)
        if(ncol(data_out) < length(indic_shortNames)){
          indics_missing <- indics[which(!(indic_shortNames %in% names(data_out)))]
          showNotification(paste("Variable(s)", paste(indics_missing, collapse=", "), "not found in the dataset"), type="warning")
        }
        varnames <- data.frame(shortName=names(data_out))
        varnames <- merge(varnames, indicator_list %>% select(shortName, labelName), by="shortName")
        #label_names <- indicator_list$labelName[which(indicator_list$shortName %in% names(data_out))]
        #ALT: Fix for bad input, specific to CAS variable coding (if someone exports labels instead of values); possible to remove if we return to dta input or with different data.
        missing_vars <- NULL
        for(currVar in names(data_out)){
          if(all(is.na(data_out[[currVar]])) | all(na.omit(data_out[[currVar]]==0))){
            missing_vars <- c(missing_vars, currVar)
          } else {
            if(!is.numeric(data_out[[currVar]])){
              data_out <- data_out %>% mutate_at(currVar, list(~ recode(., 'None'='0', 'No'='0', 'Yes'='1')))
              data_out[[currVar]] <- as.numeric(data_out[[currVar]])
              if(all(is.na(data_out[[currVar]])) | all(na.omit(data_out[[currVar]]==0))){
                missing_vars <- c(missing_vars, currVar)
              }
            }
          }
        }
        if(!is.null(missing_vars)){
          data_out <- data_out %>% select(!matches(missing_vars))
          showNotification(paste("Variable(s)", paste(missing_vars, collapse = ", "), "were non-numeric and were removed from the dataset"), type="warning")
        }
        output$heatMap <- renderPlotly(corMat(varnames$shortName, varnames$labelName, data_out))
      }
      } else {
        showNotification("Error in input files; one or more not found.", type="error")
    }
      

    }
    
  })
  

  observeEvent(input$pathwaysIn2, {
    #target_policy=tolower(input$policiesBox2)
    #if(target_policy!="none"){
    if(input$policiesBox2!="None"){
      if(is.list(pathway_link) & is.list(indicator_list)) {
      indics <- getIndics(pathway_link, indicator_list, indic_inventory, input$policiesBox2, input$pathwaysIn2, input$yearBtn)
      updateBoxes(indics) #Might need to global this

      } else {
        showNotification("Error in input files; one or more not found.", type="error")
      }
    }
  })

  observeEvent(input$yearBtn, {
    if(input$policiesBox2!="None"){
      if(is.list(pathway_link) & is.list(indicator_list)) {
        indics <- getIndics(pathway_link, indicator_list, indic_inventory, input$policiesBox2, input$pathwaysIn2, input$yearBtn)
        updateBoxes(indics) #Might need to global this
      }
    }
  }, ignoreInit = T)
  
  observeEvent(input$policiesBox2, {
    if(input$policiesBox2!="None"){
      if(is.list(pathway_link) & is.list(indicator_list)) {
    #pathway_sub <- policy_path %>% filter(goalName==input$policiesBox2)
    #pathway_list <- as.list(c(0, pathway_sub$pathwayID))
    #names(pathway_list) <- c("All", pathway_sub$Pathway)
    output$dataPathBox <- renderUI(selectInput("pathwaysIn2", "Choose a pathway (optional)", choices=polic_Names[[input$policiesBox2]]))
    if(!is.null(input$pathwaysIn2)){
      if(input$policiesBox2!="None"){
        if(is.list(pathway_link) & is.list(indicator_list)) {
          indics <- getIndics(pathway_link, indicator_list, indic_inventory, input$policiesBox2, input$pathwaysIn2, input$yearBtn)
          updateBoxes(indics) #Might need to global this
        }
      }
    }

      }
    }
  })


output$downloadRawShort <- downloadHandler(
  filename="raw_data_export.csv",
  content=function(file){
    aggs_list = input$groupsChk
    if(aggs_list==""){
      aggs_list <- NULL
    }
    indics <- getIndics(pathway_link, indicator_list, indic_inventory, input$policiesBox2, input$pathwaysIn2, input$yearBtn)
    indics <- indics[c(indics==input$indicsIn, indics==input$corrsIn)]  #Kludge
    data_files <- getFiles(indicator_list, dataset_list, indics) %>% filter(year==input$yearBtn) #Long  term we should just roll this into the getData function. It just makes the argument list even longer.
    rawData <- getData(data_files, yvars=input$indicsIn, xvars=input$corrsIn, adm_level=input$disAgg_admin, aggs_list=aggs_list, drop_0s=input$yChk)
    write.csv(rawData$tempdata, file, row.names=F)
  }
)

output$downloadRawLong <- downloadHandler(
  filename="raw_data_export.csv",
  content=function(file){
    aggs_list = input$groupsChk
    if(aggs_list==""){
      aggs_list <- NULL
    }
    
    indics <- getIndics(pathway_link, indicator_list, indic_inventory, input$policiesBox2, input$pathwaysIn2, input$yearBtn)
    data_files <- getFiles(indicator_list, dataset_list, indics)
    rawData <- getData(data_files, xvars=indics, adm_level = input$disAgg_admin, aggs_list=aggs_list, drop_0s=input$yChk) #Think about changing this to a cached reactive expression. #Drop 0s won't do anything because we treat it all as xvars
    write.csv(rawData$tempdata, file, row.names=F)
  }
)

output$downloadSummary <- downloadHandler(
  filename="summary_table_export.csv", 
  content=function(file){
  write.csv(data_table_out$data_table, file, row.names=F)
  }
)

output$downloadFlags <- downloadHandler(
  filename="data_annotations.csv",
  content=function(file){
    write.csv(data_table_out$flags_table, file, row.names=F)
  }
)

output$downloadExcel <- downloadHandler(
  filename = "CAS_indicators_demo.xlsx",
  content = function(file) {
    write.xlsx(indicator_list,file)
  })

output$downloadPathways <- downloadHandler(
  filename = "Policy_Pathways.csv",
  content = function(file) {
    write.csv(policy_path,file,row.names = FALSE)
  })

output$downloadRaw <- downloadHandler(
  filename = "processed_data.csv",
  content = function(file) {
    write.csv(data,file,row.names = FALSE)
  },
  contentType="text/csv")


output$indicsDL1 <- downloadHandler(
  filename="Policy_Pathways.csv",
  content=function(file){
    file.copy("Update/Policy_Pathways.csv", file)
  }
)

output$indicsDL2 <- downloadHandler(
  filename="indicators.xlsx",
  content=function(file) {
    file.copy("Update/indicators.xlsx", file)
  },
  contentType="application/vnd.ms-excel"
)

output$indicsDL3 <- downloadHandler(
  filename="Policy_Link.csv",
  content=function(file){
    file.copy("Update/Policy_Link.csv")
  },
  contentType="text/csv"
)

output$relsDL1 <- downloadHandler(
  filename="Policy_Pathways.csv",
  content=function(file){
    file.copy("Update/Policy_Pathways.csv", file)
  }
)

output$relsDL2 <- downloadHandler(
  filename="indicators.xlsx",
  content=function(file) {
    file.copy("Update/indicators.xlsx", file)
  },
  contentType="application/vnd.ms-excel"
)

output$relsDL3 <- downloadHandler(
  filename="Policy_Link.csv",
  content=function(file){
    file.copy("Update/Policy_Link.csv")
  },
  contentType="text/csv"
)

output$secSourcesDL <- downloadHandler(
  filename="Secondary_Sources.csv",
  content=function(file){
    file.copy("Update/Secondary_Sources.csv")
  },
  contentType="text/csv"
)

#To do: compact this for better display.
if(exists("pathwaysDT")){
path_tabs <- lapply(pathway_names, function(x){ 
  pathwaysFilt <- pathwaysDT[pathwaysDT$`Policy Goal`==x,] %>% select(-`Policy Goal`, -Instrument) %>% rename(Instrument=Implementation) #ALT: TEMP RENAME PENDING PERMANENT DECISION HERE
  pathwaysDT_out <- datatable(pathwaysFilt,
                              filter=list(position='top', clear=F),
                              rownames=F,
                              escape=F,
                              options=list(columnDefs=list(list(className="dt-center", #targets=c('P','Q', 'Quality'))),
                                                                targets=c('Producer unit costs', 'Final prod price', 'Final prod Q', 'Prod Quality')),
                                                           list(width='20%', targets=8)),
                                           scrollX=T,
                                           pageLength=10,
                                           lengthMenu=c(2,5,10),
                                           searching=T, 
                                           autoWidth=T)) %>%
    formatStyle(c('Producer unit costs', 'Final prod price', 'Final prod Q', 'Prod Quality'), color=styleEqual(c("\U2B07","\U2B06", "\U2B0D", "="), c("#e03d3d","#32a852", "darkgrey", "darkgrey")), fontSize="250%")
  
    return(tabPanel(title=paste("Policy Goal: ", x),
           fluidRow(column(10,renderDataTable(pathwaysDT_out)))
           ))

  
})
output$path_table <- renderUI({
  do.call(tabsetPanel, path_tabs) %>% return()
})
} else {
  output$path_tbl_err <- renderUI(verbatimTextOutput("Error: Pathways file not found or improperly formatted"))
}


}

shinyApp(ui = ui, server = server)
