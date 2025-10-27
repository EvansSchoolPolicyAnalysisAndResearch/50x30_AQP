options(shiny.error=browser,
        shiny.autoload.r=F) #For debugging 
suppressWarnings(
  suppressMessages({
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
library(leaflet)
library(terra)
library(tidyterra)
library(flextable)
}))

lapply(list.files("Scripts", full.names=T), FUN=source)
options(dplyr.summarise.inform = FALSE)

# Source BCA app components
source("Scripts/bca_funs.R")
source("Scripts/bca_ui.R")
source("Scripts/bca_server.R")


thematic_shiny(
  font = "auto",
  sequential = colorRampPalette(colors = c("white", "#440154FF"))(12),
  qualitative = c("#440154FF",  "#21908CFF", "#3B528BFF", "#5DC863FF", "#FDE725FF")
)
options(shiny.useragg = TRUE)


ui <- fluidPage(useShinyjs(),
                theme=bslib::bs_theme(version="5", bg = "white", fg = "#3B528BFF", info="#474481", primary = "#440154FF", #primary="#CA054D",
                                      base_font = bslib::font_google("Open Sans"),
                                      heading_font=bslib::font_google("Open Sans")), 
                fluidRow(style="background-color:#cadafa;",
                         
                         column(2, align='center', HTML("<br><img src=moa_logo.png width='200'></img>")),
                         column(3, fluidRow(HTML("&nbsp;<br><h4 style='text-align:center; font-weight:900;'>Cambodia Agriculture Survey<br>Policy & Data Explorer</h4>")),
                                fluidRow(HTML("<p style='text-align:center;'>(Version 0.2.5-Beta)</p>"))),
                         column(2, align='center', HTML("<br><img src=cam_flag.png width='150'></img>")),
                         column(5)
                ),
                fluidRow(style="background-color:#cadafa;", br()),
                
                navbarPage(title="", theme=bslib::bs_theme(version="5", preset='pulse',  #bg = "white", fg = "#3B528B", info="#474481", primary = "#440154FF",
                                                           base_font = bslib::font_google("Open Sans"),
                                                           heading_font=bslib::font_google("Open Sans")),
                           header=
                             tags$style(HTML(
                               '
                               .selectize-input.items.full.has-options.has-items {font-size: 1.0em}
                               .shiny-input-select {font-size: 1.0em}
                               .selectize-input {font-size: 1.0em}
                               .radio-group-buttons {font-size: 1.0em}
                               .btn.btn-default.shiny-download-link {--bs-btn-line-height: 0.8; font-size:1.0em}
                               .btn.btn-default.action-button {--bs-btn-line-height: 1.0; font-size:1.0em}
                                '
                             )),
                           tabPanel("Policy Context", icon=icon("signs-post"),
                                    tabsetPanel(
                                      tabPanel("About AgQuery 50x30",
                                               fluidRow(column(1), column(8, HTML("<div style='font-size:0.9em'>"),
                                                                          includeHTML('Docs/landing_page_en.html'),
                                                                          HTML("</div>")))
                                      ),
                                      
                                      tabPanel("Agricultural Sector Overview", br(),
                                               layout_columns(col_widths=c(4,8),
                                                              card(leafletOutput("areaMap")),
                                                              uiOutput("wbvalueBoxes")
                                               ),
                                                              uiOutput("overviewCards")
                                      ),
                                      tabPanel("Commodity Overviews",
                                               uiOutput("commodTabs")
                                      ),
                                      tabPanel("Interactive Maps", br(),
                                               selectInput('mapsSelect', "Choose a layer", choices=c("Select Option", "Surface Water", "Irrigation", "Land Use", "Cashew Cultivation")),
                                               leafletOutput('intMap', height="60vh", width="60vw")
                                      )
                                    )
                           ),
                           tabPanel("Identifying Feasible Options: Stakeholders & Decision Criteria", icon=icon("landmark-dome"),
                                    tabsetPanel(
                                      tabPanel("Policy Instruments by Goal",
                                               HTML('<div style="font-size: 0.9em; margin: 20 0 0 0;">'),
                                               fluidRow(HTML('<p><h3>Policy Instruments by Goal</h3></p>
                             <p>This table presents policy instruments (tax/subsidy, regulatory, information) in support of a particular goal, and the expected most direct effect on market price, quantity, quality, timeliness, and feasibility. The predicted changes in price and quantity assume competitive markets and do not consider intermediaries.</p><br>')
                                                        #downloadButton('downloadPathways',
                                                        #                 label='Download Policy Pathways',
                                                        #                 icon=icon('file-csv'))
                                               ),
                                               fluidRow(HTML('<p><i>This reference set of variables may be extended and revised by suitably trained users through revisions to the source Excel file '),
                                                        downloadLink('downloadPathways', label='here.'),
                                                        HTML('See User Guide.</i></p><br>')),
                                               fluidRow(column(12, uiOutput("path_table"), uiOutput("path_tbl_err"))),
                                               HTML('</div>')
                                      ),
                                      tabPanel("Stakeholder Mapping",
                                               HTML("<table><tr><td>Poultry Stakeholder Map</td><td>Cashew Stakeholder Map</td></tr>
                                               <tr><td><img src='poultry_stakeholder_map.png' width='80%'</img></td>
                                                    <td><img src='cashew_stakeholder_map.png' width='80%'</img></td></tr></table>"))
                                    )
                           ),
                           tabPanel("Gather Evidence & Assess Data", icon=icon("magnifying-glass-chart"),
                                    shinyjs::useShinyjs(),
                                    tabsetPanel(
                                      tabPanel("Data Summaries",
                                               HTML('<div style="font-size: 0.9em;">'),
                                               fluidRow(HTML('<p><i>The variables summarized here may be extended and revised by suitably trained users by editing the source Excel files, including '),
                                                        downloadLink('indicsDL1', label='the pathways table,'),  #this would be easier with modules
                                                        downloadLink('indicsDL2', label='the indicator list,'),
                                                        downloadLink('indicsDL3', label='and/or the linking sheet.'),
                                                        HTML('See User Guide.</i></p><br>')),
                                               fluidRow(column(4, uiOutput("trendsErr"))),
                                               #fluidRow(column(4, selectInput('policiesBox1', "Select a policy goal", choices=c("None", goalNames)))),
                                               fluidRow(column(4, selectInput('policiesBox1', "Select a policy goal", choices=c("None", goalNames)))),
                                               conditionalPanel(condition="input.policiesBox1!='None'", 
                                                                fluidRow(column(4, uiOutput('pathwaysBox'))),
                                                                fluidRow(column(4, radioGroupButtons('totsBtns', label="Choose Statistic to Present", choices=c("Mean","Total"), size='sm'))),
                                                                fluidRow(column(5, uiOutput('msgText')),
                                                                         column(1),
                                                                         column(6, uiOutput("trendVarChoose"), 
                                                                                radioGroupButtons('admButtons1', label="Choose Admin Level", choiceNames=c("Province", "Zone"), choiceValues=c("province", "zone"), size='sm')
                                                                         )
                                                                ),
                                                                fluidRow(column(6, dataTableOutput('trendsTable'),
                                                                                downloadButton('downloadSummary',
                                                                                               label='Download Table Data',
                                                                                               icon=icon('file-csv')),
                                                                                HTML("<br><hr><br>"),
                                                                                #bsCollapse(
                                                                                # bsCollapsePanel("Detailed Information",
                                                                                accordion(open=F,
                                                                                          accordion_panel("Detailed Information", 
                                                                                                          dataTableOutput('flagsTable'),
                                                                                                          downloadButton('downloadFlags',
                                                                                                                         label='Download Table Data',
                                                                                                                         icon=icon('file-csv'))))),
                                                                         column(6,
                                                                                fluidRow(uiOutput('naVals'),
                                                                                uiOutput('noTrend'), 
                                                                                uiOutput("plotsErr")),
                                                                                fluidRow(plotOutput('currMap')), 

                                                                                fluidRow(column(10),column(2,     
                                                                                                           uiOutput('dlCurrMapOut')
                                                                                                           #downloadButton('dlcurrMap', label="", icon=icon('file-arrow-down'))
                                                                                )),
                                                                                fluidRow(plotOutput('trendMap'), 
                                                                                         
                                                                                         #downloadButton('dltrendMap', label="", icon=icon('file-arrow-down'))
                                                                                ),
                                                                                #plotOutput('obsMap'),
                                                                                fluidRow(plotlyOutput('timePlot')),
                                                                                #plotOutput('provPlot'),
                                                                                fluidRow()
                                                                         )
                                                                ),
                                                                
                                                                fluidRow(column(12, uiOutput("droppedVars"))),
                                                                HTML("</div>")
                                               )
                                      ),
                                      tabPanel("Evidence from the Literature",
                                               DTOutput("evidence_tab"))
                                    )
                           ),
                           
                           tabPanel("Interpreting Data Relationships", icon=icon("chart-line"),
                                    HTML('<div style="font-size: 0.9em">'),
                                    fluidRow(HTML('<p><i>The variables summarized here may be extended and revised by suitably trained users by editing the source Excel files, including '),
                                             downloadLink('relsDL1', label='the pathways table,'),
                                             downloadLink('relsDL2', label='the indicator list,'),
                                             downloadLink('relsDL3', label='and/or the linking sheet.'),
                                             HTML('See User Guide.</i></p><br>')),
                                    fluidRow(column(4,uiOutput("explorerErr"))),
                                    fluidRow(column(5, selectInput('policiesBox2', "Select a policy goal", choices=c("None", goalNames)))),
                                    conditionalPanel(condition="input.policiesBox2!='None'",
                                                     fluidRow(column(8, uiOutput('dataPathBox'))),
                                                     fluidRow(column(4, radioGroupButtons('yearBtn', label="Survey Year", choices=year_list, selected=max(year_list), size='sm')),
                                                              column(8, br(), actionButton('makeHeatMap',"Show Heatmap"))),
                                                     fluidRow(column(4, wellPanel(style="background-color: #ededed; border-color: #9c9c9c; padding=10;",
                                                                                  fluidRow(column(6, uiOutput('indicsBox')),
                                                                                           column(6, uiOutput('corrsBox'))),
                                                                                  fluidRow(column(6, align='center', uiOutput('indicsDesc')), column(6, align='center', uiOutput('corrsDesc'))),
                                                                                  hr(),
                                                                                  fluidRow(checkboxInput('yChk', 'Omit 0s from Y Variable')),
                                                                                  fluidRow(radioButtons("disAgg_admin", HTML("<b>Select Administrative Level</b>"), choiceNames=c("Zone", "Province","Household"), choiceValues=c("zone", "province", "hhid"))),
                                                                                  fluidRow(column(10, uiOutput("groupsBtn"))),
                                                                                  fluidRow(actionButton('submitBtn', "Compare Variables")),
                                                                                  hr(),
                                                                                  fluidRow(HTML("<b>Download Data</b>")),
                                                                                  br(),
                                                                                  fluidRow(column(6, downloadButton('downloadRawShort', 'Download Selected Raw Data',icon=icon('file-csv'))),
                                                                                           column(6, downloadButton('downloadRawLong', 'Download All Listed Raw Data', icon=icon('file-csv')))
                                                                                  ),
                                                                                  br(),
                                                                                  fluidRow(column(9, HTML('<p style="font-size:10px"><i>Note: clicking "Download Selected Raw Data" will download only the "X" and "Y" variables chosen in the boxes above. "Download All Listed Raw Data" will instead download all. The data will be processed according to the selections made in terms of omitting zeroes, administrative level, and grouping.</i></p>')))
                                                     )
                                                     ),
                                                     column(8,
                                                            #plotOutput('corrPlot'),
                                                            plotlyOutput('heatMap'))),
                                                     fluidRow(HTML("&nbsp;")),
                                                     fluidRow(HTML("&nbsp;")),
                                                     fluidRow(column(6, uiOutput('indicHeader')), column(6, uiOutput('corrHeader'))),
                                                     fluidRow(column(6, uiOutput('indicatorHist') 
                                                     ), 
                                                     column(6, uiOutput('corrHist')
                                                     )
                                                     ),
                                                     fluidRow(column(6, plotOutput('indicatorMap') 
                                                                     #downloadButton("dlindicMap", label="", icon=icon("file-arrow-down"))
                                                     ), 
                                                     column(6, plotOutput('corrMap') 
                                                            #downloadButton("dlcorrMap", label="", icon=icon("file-arrow-down"))
                                                     )
                                                     ),
                                                     fluidRow(plotOutput('scatterPlot')),
                                                     fluidRow(uiOutput('plotInterp'))
                                    ),
                                    HTML('</div>')
                           ),
                           
                           #Percent of ag holdings raising %animal%
                           #Percent of ls holdings raising %animal%
                           #Percent of FHH holdings raising %animal% + map
                           #Percent of holdings with vocational/technical training
                           #Percent of holdings with formal association
                           #Percent of holdings with informal association. 
                           
                           
                           # tabPanel("Validation", icon=icon("question"),
                           #          HTML('<div style="font-size: 0.9em">'),
                           #          shinyjs::useShinyjs(),
                           #          fluidRow(column(4, selectInput('powerBiVars', "Choose Variable", choices=c("fhh", 
                           #                                                                                     "raised_Poultry", 
                           #                                                                                     "raised_Cattle", 
                           #                                                                                     "raised_Buffalo",
                           #                                                                                     "raised_Pigs", 
                           #                                                                                     "raised_Livestock", 
                           #                                                                                     "ag_comm", 
                           #                                                                                     "ag_assoc", 
                           #                                                                                     "ag_extension",
                           #                                                                                     "num_Chickens",
                           #                                                                                     "num_Cattle",
                           #                                                                                     "num_Pigs",
                           #                                                                                     "num_Buffalo",
                           #                                                                                     "sale_rate_Cattle",
                           #                                                                                     "sale_rate_Pigs",
                           #                                                                                     "sale_rate_Buffalo",
                           #                                                                                     "sale_price_obs_Cattle",
                           #                                                                                     "sale_price_obs_Pigs",
                           #                                                                                     "sale_price_obs_Buffalo",
                           #                                                                                     "prod_value_Cattle",
                           #                                                                                     "prod_value_Pigs",
                           #                                                                                     "prod_value_Buffalo"
                           #                                                                                     )
                           #                                         )
                           #                          )
                           #                   ),
                           #          #fluidRow(column(4, uiOutput('lstype'))),
                           #          fluidRow(column(4, selectInput('popVars', 'Select Sample Population',
                           #                                         choices=c("ag_hh", 
                           #                                                   "raised_Livestock", 
                           #                                                   "raised_Poultry", 
                           #                                                   "raised_Cattle", 
                           #                                                   "raised_Livestock", 
                           #                                                   "raised_Buffalo", 
                           #                                                   "raised_Pigs", 
                           #                                                   "fhh", 
                           #                                                   "ag_comm",
                           #                                                   "ag_assoc",
                           #                                                   "ag_extension")
                           #                                         )
                           #                          )
                           #                   ),
                           #          fluidRow(column(4, radioGroupButtons('yearBtn2', label="Survey Year", choices=year_list, selected=max(instrument_list$year), size='sm'))),
                           #                   fluidRow(column(4, radioGroupButtons("admBtn2", label="Choose Geographic Aggregation", choiceNames=c("Province","Zone"), choiceValues=c("province","zone"), size='sm'))),
                           #                   fluidRow(column(4, radioGroupButtons('totsBtns2', label="Choose Statistic to Graph", choices=c("Mean","Total","Obs"), size='sm'))),
                           #                   fluidRow(column(4, actionButton("goBut", "Go"))),
                           #                   br(),
                           #                   hr(),
                           #                   br(),
                           #                   fluidRow(column(4, uiOutput('vBoxOut'))), 
                           #                   fluidRow(column(6, dataTableOutput('trendsTable2')),
                           #                            column(6, plotOutput('provPlot2'))
                           #          ),
                           #          HTML('</div>')
                           #  ),
                           
                           
                           
                           tabPanel("Additional Sources for Evaluating Options", icon=icon("database"),
                                    tabsetPanel(
                                      tabPanel("Sources",
                                               HTML('<div style="font-size: 0.9em">'),
                                               fluidRow(HTML("<p>This table shows additional sources of contextual information. Updates can be made by downloading the "),
                                                        downloadLink("secSourcesDL", "associated spreadsheet."), HTML("</p>")),
                                               fluidRow(DTOutput('secsources')),
                                               HTML('</div>')
                                      ),
                                      tabPanel("Benefit-Cost Analysis",
                                               bca_ui()
                                      )
                                    )
                           ),
                           
                           
                           tabPanel("User Guide and App Diagnostics", icon=icon("stethoscope"),
                                    tabsetPanel(
                                      tabPanel("User Guide",
                                               fluidRow(column(1),
                                               column(8, 
                                               HTML("<div style='font-size:0.9em'>"),
                                               includeHTML('Docs/user_guide.html'),
                                               HTML("</div>")
                                               ))
                                      ),
                                      tabPanel("App Diagnostics",
                                               uiOutput("data_list_status"),
                                               uiOutput("inst_list_status"),
                                               uiOutput("indics_status"),
                                               uiOutput("pathway_link_status"),
                                               uiOutput("groups_status"),
                                               uiOutput("pathway_table_status"),
                                               uiOutput("indicators_diags")
                                      )
                                    )
                                    
                           )
                           
                )
)

server <- function(input, output, session) {
  ##Export/Import plots
  #Superceded by the new setup but leaving this code here as a reminder to save the plot objects instead of images.
  #imp_exp_data <- read.csv("Extdata/import_export_data.csv")
  #output$poulImpVal <- renderPlotly(imp_exp_plot(imp_exp_data, product="poultry", units="value", direction="imports"))
  #output$poulImpVol <- renderPlotly(imp_exp_plot(imp_exp_data, product="poultry", units="volume", direction="imports"))
  #output$cashExpVol <- renderPlotly(imp_exp_plot(imp_exp_data, product="cashew", units="volume", direction="exports"))
  #output$cashExpVal <- renderPlotly(imp_exp_plot(imp_exp_data, product="cashew", units="value", direction="exports"))
  bca_server(input, output, session)
  ########## Diagnostics
  #To do: add more specific errors to the startup.R code and add handling for columns with missing info.
  if(exists("dataset_list")){
    output$data_list_status <- renderUI(HTML("<p style='color: #5ac447'>Data are available.</p>"))
  } else {
    output$data_list_status <- renderUI(HTML("<p style='color: #c92031'>Data not found.</p>"))
  } # To do: add error handling
  
  if(is.list(instrument_list)){
    output$inst_list_status <- renderUI(HTML("<p style='color: #5ac447'>Instrument list is present and correctly formatted.</p>"))
  } else {
    output$inst_list_status <- renderUI(HTML("<p style='color: #c92031'>Instrument list is missing or improperly formatted.</p>"))
  }
  
  if(is.list(indicator_list)) {
    output$indics_status <- renderUI(HTML("<p style='color: #5ac447'>Variable list is present and correctly formatted.</p>"))
  } else {
    output$indics_status <- renderUI(HTML("<p style='color: #c92031'>Variable list is missing or improperly formatted.</p>"))
  }
  
  if(is.list(pathway_link)){
    output$pathway_link_status <- renderUI(HTML("<p style='color: #5ac447'>Pathway linking table is present and correctly formatted.</p>"))
  } else {
    output$pathway_link_status <- renderUI(HTML("<p style='color: #c92031'>Pathway linking table is missing or improperly formatted.</p>"))
  }
  
  if(is.list(groups_list)){
    output$groups_status <- renderUI(HTML("<p style='color: #5ac447'>Grouping variable metadata are present and correctly formatted.</p>"))
  } else {
    output$groups_status <- renderUI(HTML("<p style='color: #c92031'>Grouping variable metadata are present and correctly formatted.</p>"))
  }
  
  if(is.list(policy_path)){
    output$pathway_table_status <- renderUI(HTML("<p style='color: #5ac447'>Policy pathway table is present and correctly formatted.</p>"))
  } else {
    output$pathway_table_status <- renderUI(HTML("<p style='color: #c92031'>Policy pathway table is present and correctly formatted.</p>"))
  }
  
  #This is a bit of a squirrely way to handle this; code is used twice below so it avoids duplication, but it uses a lot of global environment dfs that aren't specified in the function call.
  indicatorCheck <- function(y){
    datasetPres <- y %in% indic_inventory$shortName
    indicatorPres <- y %in% indicator_list$shortName 
    goalPres <- y %in% pathway_link$shortName 
    if(goalPres){
      goalnum <- max(pathway_link |> filter(shortName==y) |> select(pathwayID))
      pathPres <- any(goalnum == policy_path$pathwayID)
    } else {
      pathPres <- F
    }
    if(indicatorPres) {
      rowpop <- indicator_list |> filter(shortName==y)
      if(nrow(rowpop)==1){
        dups <- F
      } else {
        dups <- T
      }
      if(dups){
        rowpop <- rowpop[1,]
      }
    }
    
    data_files <- indic_inventory |> filter(shortName==y) |> select(file) |> unique()
    dup_files <- length(data_files) > 1
    data_years <- indic_inventory |> filter(shortName==y) |> select(year) |> distinct() |> as.data.frame()
    res <- data.frame(shortName=y, 
                      Variable= if(indicatorPres) rowpop$labelName else "NA",
                      `Survey Years Available` = if(datasetPres) paste(data_years[,1], collapse=", ") else "NA", 
                      `In Dataset`= if(datasetPres) as.character(icon("check")) else as.character(icon("xmark")),
                      `In Indicators List`=if(indicatorPres) as.character(icon("check")) else as.character(icon("xmark")),
                      `Assigned to a Policy Goal` = if(goalPres) as.character(icon("check")) else as.character(icon("xmark")),
                      `Assigned to a Pathway` = if(pathPres) as.character(icon("check")) else as.character(icon("xmark"))
    )
    return(res)
  }
  
  if(is.list(indic_inventory) & is.list(instrument_list) & is.list(indicator_list) & is.list(pathway_link) & is.list(policy_path)) {
    files <- unique(indic_inventory$file)
    indics_list <- lapply(files, FUN=function(x){
      indics_sub <- indic_inventory |> filter(file==x)
      indics_chk <- lapply(unique(indics_sub$shortName), FUN=indicatorCheck)
      return(do.call(rbind, indics_chk))
    })
    rogueIndics <- indicator_list$shortName[which(!(indicator_list$shortName %in% indic_inventory$shortName))]
    indics_chk2 <- lapply(rogueIndics, FUN=indicatorCheck)
    indics_list <- c(indics_list, indics_chk2)
    names(indics_list) <- c(files, "Not Found in File")
    output$indicators_diags <- renderUI({
      tabs <- lapply(1:length(indics_list), FUN=function(x){
        names(indics_list[[x]]) <- str_replace_all(names(indics_list[[x]]), "\\.", " ")
        tabPanel(title=names(indics_list)[[x]],
                 DT::renderDT(indics_list[[x]], escape=F))
      })
      return(do.call(tabsetPanel, tabs))
    })
    
  }
  
  #######
  
  
  #### Additional UI elements
  
  if(is.list(source_data)){
    output$evidence_tab <- renderDT(source_data, escape=F, rownames=F)
  }
  
  #See startupVbs.R in Scripts
  if(length(wbVbs)>0){
    output$wbvalueBoxes <- renderUI(layout_column_wrap(width=1/3, !!!wbVbs)) 
  }
  
  overviewCards <- c(extdataCasCards, extdataImgCards)
  if(length(overviewCards) > 0){
    output$overviewCards <- renderUI(layout_column_wrap(width=1/3, !!!overviewCards))
  }
  
  #if(length(extdataImgCards)>0){
  #  output$imgCards <- renderUI(extdataImgCards)
  #}
  
  #if(length(extdataCasCards)>0){
  #  output$casCards <- renderUI(extdataCasCards)
  #}
  
  if(length(commod_tabs_out)>0){
    output$commodTabs <- renderUI({do.call(tabsetPanel, commod_tabs_out)})
  }
  
  
  
  ### MAPPING 
  
  #To do: relocate to startup if we're making it permanent. Also functionalize this so it can be modified.
  
  
  
  ##Static Maps
  output$areaMap <- renderLeaflet(leaflet() |>
                                    addTiles(urlTemplate=source_string) |> 
                                    setView(104.568,12.994,zoom=5)
  )
  
  output$landUse <- renderPlot(ggplot()+geom_spatraster(data=land_use, aes(fill=luse_areas))+
                                 scale_fill_manual(values=luse_colors, name="Land Use Class", na.translate=F)+
                                 geom_sf(data=khm_province, fill=NA, color='white')+
                                 theme_map(font_size=12))
  
  output$cashewMap <- renderPlot(ggplot() + geom_spatraster(data=cashews, aes(fill=b1))+
                                   scale_fill_gradient(name="Probability\n(Percent)", low="#FEE8C8", high="#E34A33", na.value="transparent")+
                                   geom_sf(data=khm_province,color="darkgray", fill=NA)+
                                   theme_map(font_size=12))
  
  
  observeEvent(input$mapsSelect, {
    #selectInput('mapsSelect', choices=c("Select Option", "Surface Water", "Irrigation", "Land Use", "Cashew Cultivation"))
    if(input$mapsSelect!="Select Option") {
      output$intMap <- switch(input$mapsSelect,
                              `Surface Water`=renderLeaflet(
                                leaflet() |> 
                                  addTiles(urlTemplate=source_string) |>
                                  setView(104.568,12.994,zoom=7) |>
                                  addRasterImage(surfwater, colors=surfwater_pal, opacity=0.8) |> 
                                  addLegend(title="Percent Change", pal=surfwater_leg, values=values(surfwater), labFormat=labelFormat(transform=function(x) sort(x, decreasing=T)))
                              ),
                              `Irrigation` = renderLeaflet(
                                leaflet() |> 
                                  addTiles(urlTemplate=source_string) |> 
                                  setView(104.568,12.994,zoom=7) |>
                                  addRasterImage(crop_areas, colors=croppal, opacity=0.8) |>
                                  addLegend(colors=c('darkgreen', 'goldenrod'), values=values(crop_areas), labels=c("Irrigated","Rainfed"))
                              ),
                              `Land Use` = renderLeaflet(
                                leaflet() |> 
                                  addTiles(urlTemplate=source_string) |>
                                  setView(104.568,12.994,zoom=7) |>
                                  addRasterImage(land_use, colors=luse_pal, opacity=0.9) |> 
                                  addLegend(colors=luse_colors, values=values(land_use), labels=luse_areas)
                              ),
                              `Cashew Cultivation`=renderLeaflet(
                                leaflet() |>
                                  addTiles(urlTemplate=source_string) |> 
                                  setView(104.568,12.994,zoom=7) |>
                                  addRasterImage(cashews, colors=cashewpal, opacity=0.8) |>
                                  addLegend(title="Probability (Percent)", pal=cashewpal, values=values(cashews))
                              )
                              
      )
    }
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
      output$indicsDesc <- renderUI(verbatimTextOutput("Survey details not found for selected indicator."))
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
      output$corrsDesc <- renderUI(verbatimTextOutput("Survey details not found for selected indicator."))
    }
  })
  
  
  #fluidRow(column(6, align='center', selectInput('indicsIn', HTML("<b>Select Indicator</b>"), choices=indics, size=length(indics), selectize=F),
  #                #                uiOutput('indicsDesc'), 
  #                #column(6, pickerInput('corrsIn', HTML('<b>Select Correlate</b>'), choices=indics, options=list(style='btn-info', size=length(indics))))),
  #                #column(6, align='center', selectInput('corrsIn', HTML('<b>Select Correlate</b>'), choices=indics, size=length(indics), selectize=F),
  
  updateBoxes <- function(indics){
    output$indicsBox <- renderUI(selectInput('indicsIn', HTML("<b>Select Y Variable</b>"), choices=indics)) #, size=length(indics) , selectize=F)) 
    output$corrsBox <- renderUI(selectInput('corrsIn', HTML('<b>Select X Variable</b>'), choices=indics)) #, size=length(indics), selectize=F))
    
  }
  
  observeEvent(input$yearBtn, {
    groups_sub <- groups_list |> filter(level=="All" | level==input$policiesBox2)
    yeargroups <- tryCatch(read.csv(sprintf("Data/Cambodia_CAS_%s_groups.csv", input$yearBtn), nrows=1) |> names(), 
                           error=function(e){return(NULL)})
    if(!is.null(yeargroups)){
      groups_sub = groups_sub |> filter(varName %in% yeargroups)
      if(nrow(groups_sub)!=0){
        output$groupsBtn <- renderUI(radioButtons("groupsChk", HTML("<b>Selecting Grouping Variable</b>"), choiceNames=c("None", groups_sub$label), choiceValues=c("", groups_sub$varName)))
      } else {
        output$groupsBtn <- renderUI(radioButtons("groupsChk", HTML("<b>Selecting Grouping Variable</b>"), choiceNames="None Available", choiceValues=""))
      } 
    } else {
      output$groupsBtn <- renderUI(radioButtons("groupsChk", HTML("<b>Selecting Grouping Variable</b>"), choiceNames="No groups file found", choiceValues=""))
    }
    
  })
  
  #data_table_out <- observe({makeDataTable(input$policiesBox1, indicatorCategories, indicator_list, dataset_list)})
  
  observeEvent(input$policiesBox1, {
    if(input$policiesBox1!="None" & is.list(policy_path)){
      inputChk <- is.null(input$pathwaysIn1)
      input0Chk <- if(!inputChk) input$pathwaysIn1==0 else F
      #pathway_sub <- policy_path |> filter(goalName==input$policiesBox1)
      #pathway_list <- as.list(c(0, pathway_sub$pathwayID))
      #names(pathway_list) <- c("All", pathway_sub$Pathway)
      #output$pathwaysBox <- renderUI(selectInput("pathwaysIn1", "Choose a pathway (optional)", choices=polic_Names[[input$policiesBox1]]))
      output$pathwaysBox <- renderUI(pickerInput("pathwaysIn1", "Choose a pathway (optional)", choices=polic_Names[[input$policiesBox1]], options=list(style="selectize-input"), choicesOpt=list(disabled=polic_activ[[input$policiesBox1]])))
      shinyjs::disable('pathwaysIn1')
      shinyjs::disable('policiesBox1')
      shinyjs::disable('totsBtns')
      showNotification("Loading, please wait")
      data_table_out <<- makeDataTable(input$policiesBox1, indicatorCategories, indicator_list, dataset_list) #Need to change some names here
      
      shinyjs::enable('pathwaysIn1')
      shinyjs::enable('policiesBox1')
      shinyjs::enable('totsBtns')
      #updateVarTable <- function(pathwaysIn1=NULL, policiesIn1, obsyear, totsBtns)
      if(!inputChk & input0Chk){ #Sniping a special case where the observer doesn't trigger if you're on "All Instruments" to fix with reactive values.
      updateVarTable(input$pathwaysIn1, input$policiesBox1, unique(indic_inventory$year), input$totsBtns)
      }
    }
  }, ignoreInit=T)
  
  observeEvent(input$pathwaysIn1, {
    updateVarTable(input$pathwaysIn1, input$policiesBox1, unique(indic_inventory$year), input$totsBtns)
  }, ignoreInit=T)
  
  observeEvent(input$totsBtns, {
    updateVarTable(input$pathwaysIn1, input$policiesBox1, unique(indic_inventory$year), input$totsBtns)
  }, ignoreInit=T)
  
  #ALT NOTE TO ADD ERROR HANDLING HERE.
  output$msgText <- renderUI(HTML("<h4>Variable Summary Table</h4><br><p><i>This table presents household-level averages or national totals of all CAS respondents who participated in activities related to the policy goal.</i></p>"))

  #Leaving this in app.R because it requires translation
  updateVarTable <- function(pathwaysIn1=NULL, policiesIn1, obsyear, totsBtns){
    pathwaysIn <- if(is.null(pathwaysIn1)){
      "0"
    } else {
      pathwaysIn1
    }
    filtered_tab <- filterVarTable(data_table_out$data_table, pathway_link, pathwaysIn, indicator_list, totsBtns)
    output$trendsTable <- renderDataTable(
      DT::datatable(filtered_tab |> select(-shortName), 
                    options=list(searching=F, pageLength=15, dom='tip'), rownames=F)  |>
        formatPercentage(5) #Hard coded, to fix
    )
    output$flagsTable <- renderDataTable(
      DT::datatable(
        filterFlagTable(data_table_out$flag_table, pathway_link, pathwaysIn, indicator_list) |> select(-shortName), 
        options=list(searching=F, pageLength=15), rownames=F)
    )
    output$trendVarChoose <- renderUI({
      trendVarList <- getIndics(pathway_link, indicator_list, indic_inventory, policiesIn1, pathwaysIn, obsyear, cats=T)
      trendVarList <- c("0", trendVarList)
      names(trendVarList)[[1]] <- "Select..."
      #trendVarList <- as.list(c("0", filtered_tab$shortName))
      #names(trendVarList) <- c("Select...", filtered_tab$Variable)
      selectizeInput('trendIn', "Choose a variable to map:", choices=trendVarList)
    })
  }
  
  observeEvent(ignoreInit=T, list(input$trendIn, input$admButtons1), { #probably a future efficiency update to do here.
    if(input$trendIn!="0"){
      
      showNotification("Processing, please wait")
      #session$sendCustomMessage("disableButton", "start_proc")
      shinyjs::disable('trendIn')
      adm_level_in=input$admButtons1 #Adjustable
      #We can speed this up by filtering out files where the variables are not present. To do later.
      
      denoms <- getDenoms(input$trendIn, indicator_list)
      data_files <- getFiles(indicator_list, dataset_list, input$trendIn) #AT: There's probably a simpler way to pack all of this into the getData function but that's a do later item. 
      tempdata <- getData(data_files, xvars=input$trendIn, denoms=denoms, adm_level=adm_level_in, source_call="trendmaps")
      
      if(is.list(tempdata)){ #To do: better error handling
        data_out <- tempdata$outdata |> select(all_of(c(adm_level_in, "year", input$totsBtns))) #Find a way to kill mapdata?
        n_row <- nrow(data_out) 
        data_out <- na.omit(data_out)
        data_out <- data_out[data_out[[adm_level_in]]!="",]
        if(nrow(data_out) < n_row){
          output$plotsErr <- renderUI(HTML(sprintf("<i>Note: some observations removed due to missing %s information</i><br>", adm_level_in)))
        } else {
          output$plotsErr <- NULL
        }
        max_year <- max(data_out$year)
        min_year <- min(data_out$year)
        
        if(min_year!=max_year){
          df_min_year=data_out |> filter(year==min_year)
          df_max_year=data_out |> filter(year==max_year)
          diff <- data_out |> pivot_wider(names_from=year, values_from=input$totsBtns)
          diff[,4] <- diff[,3]-diff[,2]
          #Messy, to fix
          names(diff)[[4]] <- input$trendIn
          names(df_max_year)[[3]] <- input$trendIn
          names(data_out)[[3]] <- input$trendIn
          curr_map <- get(paste0("khm_", adm_level_in))
          xShp_currMap <- merge(curr_map, df_max_year, by=adm_level_in, all.x=T)
          xShp_trendMap <- merge(curr_map, diff, by=adm_level_in, all.x=T)
          
          currMap <- monoColorMap(xShp_currMap, input$trendIn, paste0(indicator_list$labelName[indicator_list$shortName == input$trendIn], ", ", max_year, " ", input$totsBtns), indicator_list$units[indicator_list$shortName==input$trendIn])
          trendMap <- biColorMap(xShp_trendMap, input$trendIn, paste0(indicator_list$labelName[indicator_list$shortName == input$trendIn], ", ", min_year, " - ", max_year, " Trend"), indicator_list$units[indicator_list$shortName==input$trendIn])
          timePlot <- timeSeriesPlot(data_table_out$data_table, input$trendIn, input$totsBtns)
          if(any(is.na(df_max_year[[input$totsBtns]]))){
            output$naVals <- renderUI(HTML("<i>Note: gray shaded areas do not have observations for the selected variable.</i><br>"))
          } else {
            output$naVals <- NULL
          }
          output$currMap <- renderPlot(currMap)
          output$trendMap <- renderPlot(trendMap)
          output$timePlot <- renderPlotly(timePlot)
          
          output$noTrend <- NULL
          #output$provPlot <- renderPlot(provPlot)
        } else {
          df_max_year=data_out |> filter(year==max_year)
          names(df_max_year)[[3]] <- input$trendIn
          curr_map <- get(paste0("khm_", adm_level_in))
          xShp_currMap <- merge(curr_map, df_max_year, by=adm_level_in, all.x=T)
          currMap <- monoColorMap(xShp_currMap, input$trendIn, paste0(indicator_list$labelName[indicator_list$shortName == input$trendIn], ", ", max_year, " ", input$totsBtns), indicator_list$units[indicator_list$shortName==input$trendIn])
          output$currMap <- renderPlot(currMap)
          output$noTrend <- renderUI(HTML("<i>Selected variable was found in only one survey year.</i><br>"))
          #showNotification("No trends to show for selected variable", type="warning")
        }
        output$dlCurrMapOut <- renderUI(downloadButton('dlcurrMap', label="", icon=icon('file-arrow-down')))
        output$dlcurrMap <- downloadHandler(
          filename=function(){
            paste0("cas-", max_year, "-", adm_level_in, "-", input$trendIn, ".csv")
          },
          content=function(file){
            if(with(df_max_year, exists("province"))){
              df_max_year <- merge(prov_lookup, df_max_year, by="province") |> select(-province) |> rename(province=province_name)
            }
            write.csv(df_max_year, file, row.names=F)
          }
        )
      } else {
        showNotification("Error getting data", type="error")
      }
      shinyjs::enable('trendIn')
    }
  })
  
  
  observeEvent(input$submitBtn, {
    #updatePlots(maps=T)
    showNotification("Processing...")
    data_files <- getFiles(indicator_list, dataset_list, c(input$indicsIn, input$corrsIn)) |> filter(year==input$yearBtn) #To fix, probably roll year into getFiles function. 
    aggs_list <- input$groupsChk #ALT Note: Right now this is an unnecessary step, but if we ever end up needing to have multiple disaggregation criteria, it's probably better to do it this way.
    denoms <- getDenoms(c(input$corrsIn, input$indicsIn), indicator_list)
    adm_level <- input$disAgg_admin
    #Used for labels
    if(adm_level=="hhid"){
      adm_level_in <- "province" #Default to province for mapping
    } else {
      adm_level_in <- adm_level
    }
    all_data <- getData(data_files, xvars=input$corrsIn, yvars=input$indicsIn, denoms=denoms, adm_level=adm_level, aggs_list=aggs_list, source_call="explorer", drop_0s = input$yChk)
    if(any(all_data!="")){
      #else if(tab=="trend"){
      #ALT - might be easier than what we do now with the maps in a separate area. Maybe build out later.
      #}
      mapdata <- all_data$mapdata
      outdata <- all_data$outdata 
      outdata <- outdata |> select(c(any_of(c(adm_level, aggs_list)), shortName, Mean)) |> 
        pivot_wider(names_from="shortName", values_from="Mean")
      outdata <- na.omit(outdata)
      if(nrow(outdata)==0){
        showNotification("Error: No non-n/a observations in dataset", type="error") 
      } else { 
        
        xvars = input$corrsIn
        yvars = input$indicsIn
        if(!all(c(xvars, yvars) %in% names(outdata))){
          showNotification("Error: one or both variables is missing from the dataset. Did you capitalize everything the same way?")
        } else {
          #output$indicHeader <- renderUI(HTML(sprintf('<div style="border: 1px solid #ddd; padding: 9px; margin-bottom: 0px; line-height: 1.2; text-align: center; border-radius: 3px;"> %s </div>'
          #                                            , indicator_list$labelName[indicator_list$shortName==input$indicsIn])))
          #output$corrHeader <- renderUI(HTML(sprintf('<div style="border: 1px solid #ddd; padding: 9px; margin-bottom: 0px; line-height: 1.2; text-align: center; border-radius: 3px;"> %s </div>'
          #                                          , indicator_list$labelName[indicator_list$shortName==input$corrsIn])))
          
          varslist <- c(xvars, yvars)
          bins <- ifelse(adm_level=="province", 6, 30)
          #heatmapdata <- getData()$tempheatmapdata
          #outdata <- heatmapdata |> select(all_of(c(xvars,yvars)))
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
              flevels = groups_list[which(groups_list$varName==aggs_list),]$Levels |> str_split(., ",") |> unlist()
              flabels = groups_list[which(groups_list$varName==aggs_list),]$Labels |> str_split(., ",") |> unlist()
              outdata[[aggs_list]] <- factor(outdata[[aggs_list]], levels=flevels, labels=flabels)
            }
            #makeHistGrps <- function(outdata, yvars, bins, aggs_list, indicAxis, titleLab, aggs_lab)
            corrHist <- makeHistGrps(outdata, xvars, bins, aggs_list, corrAxis, xlab, aggs_lab)
            indicatorHist <- makeHistGrps(outdata,yvars,bins,aggs_list,indicAxis, ylab, aggs_lab)
            scatterPlot <- makeScatterGrps(outdata,xvars,yvars,aggs_list,xlab,ylab,aggs_lab, res_out)
          }
          corrTitle <- sprintf("Map of %s by %s", indicator_list$labelName[indicator_list$shortName == xvars], str_to_title(adm_level_in))
          corrUnits <- indicator_list$units[indicator_list$shortName==xvars]
          
          indicTitle <- sprintf("Map of %s by %s", indicator_list$labelName[indicator_list$shortName == yvars], str_to_title(adm_level_in))
          indicUnits <- indicator_list$units[indicator_list$shortName==yvars]
          if(adm_level!="hhid"){
            
            if(adm_level=='province' & is.numeric(outdata[[adm_level]])){
              outdata <- merge(outdata, prov_lookup, by="province")
              outdata <- outdata |> select(-province) |> rename(province=province_name)
            }
            corrTab <- outdata |> 
              ungroup() |> 
              select(any_of(c(adm_level, aggs_list, xvars))) |> 
              mutate_if(is.character, ~case_match(., "" ~ NA, .default=.)) |> 
              mutate_at(xvars, ~signif(., 4)) |>
              na.omit()
            
            indicatorTab <- outdata |> 
              ungroup() |> 
              select(any_of(c(adm_level, aggs_list, yvars))) |> 
              mutate_if(is.character, ~case_match(., "" ~ NA, .default=.)) |> 
              mutate_at(yvars, ~signif(., 4)) |>
              na.omit()
            

            
            if(!any(aggs_list=="")){
              names(corrTab) <- c(str_to_title(adm_level), groups_list$label[groups_list$varName==aggs_list], paste0(indicator_list$labelName[indicator_list$shortName==xvars], " (HH Avg)")) #This won't work with multiple disagg vars; fix later.
              corrTabFlx <- flextable(corrTab)
              corrTabFlx <- merge_v(corrTabFlx, j=str_to_title(adm_level)) |>
                autofit() |>
                htmltools_value() 
              
              names(indicatorTab) <- c(str_to_title(adm_level), groups_list$label[groups_list$varName==aggs_list], paste0(indicator_list$labelName[indicator_list$shortName==yvars], " (HH Avg)")) #This won't work with multiple disagg vars; fix later.
              indicatorTabFlx <- flextable(indicatorTab)
              indicatorTabFlx <- merge_v(indicatorTabFlx, j=str_to_title(adm_level)) |>
                autofit() |>
                htmltools_value()
              #indicatorTab <- merge_v(indicatorTab, j=str_to_title(adm_level))
            } else {
              names(corrTab) <- c(str_to_title(adm_level), paste0(indicator_list$labelName[indicator_list$shortName==xvars], " (HH Avg)")) #This won't work with multiple disagg vars; fix later.
              names(indicatorTab) <- c(str_to_title(adm_level), paste0(indicator_list$labelName[indicator_list$shortName==yvars], " (HH Avg)")) #This won't work with multiple disagg vars; fix later.
              corrTabFlx <- flextable(corrTab) |>
                autofit() |>
                htmltools_value()
              indicatorTabFlx <- flextable(indicatorTab) |>
                autofit() |>
                htmltools_value()
            }
            
          } else {
            corrTabFlx <- HTML("Tables are not displayed at the household level because they would be too long.")
            indicatorTabFlx <- HTML("Tables are not displayed at the household level because they would be too long.")
          }
          
          #if(is.numeric(mapdata$province)){
          curr_map <- get(paste0("khm_",adm_level_in))
          mapdata <- merge(curr_map, mapdata, by=adm_level_in, all.x=T)
          
          #} else {
          # mapdata <- merge(khm_shp, mapdata, by.x="ADM1_EN", by.y="province", all.x=T)
          #}
          
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
          
          
          
          output$indicatorHist <- renderUI(navset_card_pill(
            placement="above",
            nav_spacer(),
            nav_panel(title=icon("chart-simple"), 
                      renderPlot(indicatorHist)),
            nav_panel(title=icon("table"), renderUI(indicatorTabFlx)),
            card_footer(downloadButton("dlIndicHist", label="", icon=icon("file-arrow-down"))) #this isn't the correct syntax, but the documented way to add a footer doesn't work so we'll just do it this way and eat the warnings.
          ))
          
          output$corrHist <- renderUI(navset_card_pill(
            placement="above",
            nav_spacer(),
            nav_panel(title=icon("chart-simple"), renderPlot(corrHist)),
            nav_panel(title=icon("table"), renderUI(corrTabFlx)),
            card_footer(downloadButton("dlCorrHist", label="", icon=icon("file-arrow-down"))) 
          )
          )
          
          output$dlCorrHist <- downloadHandler(
            filename = function(){
              paste0("cas-", input$yearBtn, "-", xvars, ".csv")
            },
            content = function(file){
              write.csv(corrTab, file, row.names=F)
            }
          )
          
          output$dlIndicHist <- downloadHandler(
            filename=function(){
              paste0("cas-", input$yearBtn, "-", yvars, ".csv")
            }, 
            content=function(file){
              write.csv(indicatorTab, file)
            }
          )
          
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
        indics_out <- getIndics(pathway_link |> filter(pathwayID!=0), indicator_list, indic_inventory, input$policiesBox2, input$pathwaysIn2, input$yearBtn)
        indics_out <- unlist(indics_out)
        indics_out <- data.frame(shortName=indics_out)
        indics_out <- merge(indics_out, indicator_list, by="shortName")
        data_files <- getFiles(indicator_list, dataset_list, indics_out$shortName) |> filter(year==input$yearBtn)
        #data_files_select <- indics_out |> 
        #  select(file) |> 
        #  distinct() |>
        #  unlist() #Using tolower here helps filter out differences in capitalization 
        #survey_pref <- indics_out$survey[indics_out$year==input$yearBtn] # TO FIX; this line no longer does anything.
        #data_files <- lapply(data_files_select, FUN=function(x){dataset_list[which(str_detect(str_to_lower(dataset_list), str_to_lower(x)))]}) |> unique() |> unlist()  #Drop duplicates if they're somehow in there.
        #data_files <- dataset_list |> select(which(str_to_lower(dataset_list) %in% str_to_lower(data_files_select)))
        #data_files <- dataset_list[which(str_detect(str_to_lower(dataset_list), str_to_lower(data_files_select)))] |> as.data.frame()
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
            temp <- temp |> select(all_of(c(names(temp)[which(!(names(temp) %in% names(data_out)))], "hhid"))) #Fix for redundant input.
            data_out <- merge(data_out, temp, by="hhid")
          }
        }
        #} 
        #data_out <- data_out |> mutate(indicatorCategory=tolower(indicatorCategory)) |> subset(indicatorCategory==target_policy, select=all_of(indicator_list$shortName)) |> na.omit()
        if(exists("data_out")){
          indics <- as.list(indics_out$shortName)
          indic_shortNames <- unlist(indics, use.names=F)
          data_out <- data_out |> select(any_of(indic_shortNames)) #Won't throw an error if names are missing)
          if(ncol(data_out) < length(indic_shortNames)){
            indics_missing <- indics[which(!(indic_shortNames %in% names(data_out)))]
            showNotification(paste("Variable(s)", paste(indics_missing, collapse=", "), "not found in the dataset"), type="warning")
          }
          varnames <- data.frame(shortName=names(data_out))
          varnames <- merge(varnames, indicator_list |> select(shortName, labelName), by="shortName")
          #label_names <- indicator_list$labelName[which(indicator_list$shortName %in% names(data_out))]
          #ALT: Fix for bad input, specific to CAS variable coding (if someone exports labels instead of values); possible to remove if we return to dta input or with different data.
          missing_vars <- NULL
          for(currVar in names(data_out)){
            if(all(is.na(data_out[[currVar]])) | all(na.omit(data_out[[currVar]]==0))){
              missing_vars <- c(missing_vars, currVar)
            } else {
              if(!is.numeric(data_out[[currVar]])){
                data_out <- data_out |> mutate_at(currVar, list(~ recode(., 'None'='0', 'No'='0', 'Yes'='1')))
                data_out[[currVar]] <- as.numeric(data_out[[currVar]])
                if(all(is.na(data_out[[currVar]])) | all(na.omit(data_out[[currVar]]==0))){
                  missing_vars <- c(missing_vars, currVar)
                }
              }
            }
          }
          if(!is.null(missing_vars)){
            data_out <- data_out |> select(!matches(missing_vars))
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
        indics <- getIndics(pathway_link, indicator_list, indic_inventory, input$policiesBox2, input$pathwaysIn2, input$yearBtn, cats=T)
        updateBoxes(indics) #Might need to global this
        
      } else {
        showNotification("Error in input files; one or more not found.", type="error")
      }
    }
  })
  
  observeEvent(input$yearBtn, {
    if(input$policiesBox2!="None"){
      if(is.list(pathway_link) & is.list(indicator_list)) {
        indics <- getIndics(pathway_link, indicator_list, indic_inventory, input$policiesBox2, input$pathwaysIn2, input$yearBtn, cats=T)
        updateBoxes(indics) #Might need to global this
      }
    }
  }, ignoreInit = T)
  
  observeEvent(input$policiesBox2, {
    if(input$policiesBox2!="None"){
      if(is.list(pathway_link) & is.list(indicator_list)) {
        #pathway_sub <- policy_path |> filter(goalName==input$policiesBox2)
        #pathway_list <- as.list(c(0, pathway_sub$pathwayID))
        #names(pathway_list) <- c("All", pathway_sub$Pathway)
       #output$dataPathBox <- renderUI(selectInput("pathwaysIn2", "Choose a pathway (optional)", choices=polic_Names[[input$policiesBox2]]))
        output$dataPathBox <- renderUI(pickerInput("pathwaysIn2", "Choose a pathway (optional)", choices=polic_Names[[input$policiesBox2]], options=list(style="selectize-input"), choicesOpt=list(disabled=polic_activ[[input$policiesBox2]])))
        
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
  
  
  #To do: compact this for better display.
  if(exists("pathwaysDT")){
    formatCols <- vector()
    for(i in 1:length(names(pathwaysDT))){
      items <- unique(pathwaysDT[,i]) 
      if(any(c("\U2B07","\U2B06", "\U2B0D", "=") %in% items)){
        formatCols <- c(formatCols, names(pathwaysDT)[[i]])
      }
    }
    path_tabs <- lapply(pathway_names, function(x){ 
      pathwaysFilt <- pathwaysDT[pathwaysDT$`Policy Goal`==x,] |> select(-`Policy Goal`) |> rename(`Instrument Category`=Instrument) |> rename(Instrument=Implementation) #ALT: TEMP RENAME PENDING PERMANENT DECISION HERE
      pathwaysDT_out <- datatable(pathwaysFilt,
                                  filter=list(position='top', clear=F),
                                  rownames=F,
                                  escape=F,
                                  options=list(columnDefs=list(list(className="dt-center", #targets=c('P','Q', 'Quality'))),
                                                                    targets=formatCols) #,
                                                               #list(width='20%', targets=8)
                                  ),
                                  scrollX=T,
                                  pageLength=10,
                                  lengthMenu=c(2,5,10),
                                  searching=T, 
                                  autoWidth=T)) |>
        formatStyle(formatCols, color=styleEqual(c("\U2B07","\U2B06", "\U2B0D", "="), c("#e03d3d","#32a852", "darkgrey", "darkgrey")), fontSize="250%")
      
      return(tabPanel(title=paste("Policy Goal: ", x),
                      fluidRow(column(10,renderDT(pathwaysDT_out)))
      ))
      
      
    })
    output$path_table <- renderUI({
      do.call(tabsetPanel, path_tabs)
    })
  } else {
    output$path_tbl_err <- renderUI(verbatimTextOutput("Error: Pathways file not found or improperly formatted"))
  }
  
  

  
  output$downloadRawShort <- downloadHandler(
    filename="raw_data_export.csv",
    content=function(file){
      aggs_list = input$groupsChk
      if(aggs_list==""){
        aggs_list <- NULL
      }
      indics <- getIndics(pathway_link, indicator_list, indic_inventory, input$policiesBox2, input$pathwaysIn2, input$yearBtn)
      indics <- indics[c(indics==input$indicsIn, indics==input$corrsIn)]  #Kludge
      data_files <- getFiles(indicator_list, dataset_list, indics) |> filter(year==input$yearBtn)
      denoms <- getDenoms(indics, indicator_list)
      rawData <- getData(data_files, yvars=input$indicsIn, xvars=input$corrsIn, denoms=denoms, adm_level=input$disAgg_admin, aggs_list=aggs_list, drop_0s=input$yChk)
      write.csv(rawData$outdata, file, row.names=F)
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
      denoms <- getDenoms(indics, indicator_list)
      rawData <- getData(data_files, xvars=indics, denoms=denoms, adm_level = input$disAgg_admin, aggs_list=aggs_list, drop_0s=input$yChk) #Think about changing this to a cached reactive expression. #Drop 0s won't do anything because we treat it all as xvars
      write.csv(rawData$outdata, file, row.names=F)
    }
  )
  
  output$downloadSummary <- downloadHandler(
    filename="summary_table_export.csv", 
    content=function(file){
      write.csv(filterVarTable(data_table_out$data_table), file, row.names=F)
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
    filename = "Policy_Pathways.xlsx",
    content = function(file) {
      file.copy("Update/Policy_Pathways.xlsx")
    },
    contentType="application/vnd.ms-excel")
  
  # output$downloadRaw <- downloadHandler(
  #   filename = "processed_data.csv",
  #   content = function(file) {
  #     write.csv(data,file,row.names = FALSE)
  #   },
  #   contentType="text/csv")
  
  
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
    filename="Policy_Pathways.xlsx",
    content=function(file){
      file.copy("Update/Policy_Pathways.xlsx", file)
    },
    contentType="application/vnd.ms-excel" 
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
  
  
  
  #Tab that holds this content is currently commented out.
  observeEvent(input$goBut, {
    varIn <- input$powerBiVars
    plotStat <- input$totsBtns2
    popVar <- input$popVars
    adm_level <- input$admBtn2
    denoms <- getDenoms(varIn, indicator_list)
    datafiles <- getFiles(indicator_list, dataset_list, c(varIn, popVar)) |> filter(year==input$yearBtn2)
    data_out <- getFiltData(files=datafiles, xvars=varIn, denoms=denoms, filter=input$popVars, adm_level=adm_level)
    if(any(is.list(data_out))){
      
      vType <- indicator_list |> filter(shortName==varIn) |> select(units)
      if(any(vType %in% "boolean")){
        vBoxVal <- renderText(paste0(signif(data_out$natdata$Mean[[1]],2)*100, "%", " of ", popVar)) #Should only have 1 value
        vBoxShow <- renderText(paste(format(data_out$natdata$Total[[1]], big.mark=","), "hhs total", varIn))
        #vBoxName <- renderText(paste(input$powerBiVars, "hhs among", input$popVars))
        output$vBoxOut <- renderUI(value_box(title="National Summary", showcase=bsicons::bs_icon("house"), value=vBoxVal, p(vBoxShow))) #, p(vBoxName)))
      } else {
        vBoxVal <- renderText(paste0(signif(data_out$natdata$Mean[[1]],2), " average")) #Should only have 1 value
        vBoxShow <- renderText(paste(format(data_out$natdata$Total[[1]], big.mark=","), "total", input$powerBiVars, "in", popVar))
        #vBoxName <- renderText(paste(input$powerBiVars, "hhs among", input$popVars))
        output$vBoxOut <- renderUI(value_box(title="National Summary", showcase=bsicons::bs_icon("house"), value=vBoxVal, p(vBoxShow))) #, p(vBoxName)))
      }
      if(max(data_out$outdata$Total) > 1000000000) {
        data_out$outdata$Total <- data_out$outdata$Total/1000000000 |> round(., digits=4)
        bns <- T
      } else if(max(data_out$outdata$Total) > 1000000){
        data_out$outdata$Total <- data_out$outdata$Total/1000000 |> round(., digits=4)
        mns <- T
        bns <- F
      } else {
        mns <- F
        bns <- F
      }
      
      #xShp_currMap <- merge(khm_shp, data_out$mapdata, by="province", all.x=T)
      if(adm_level=="province"){
        merged_tab <- merge(na.omit(data_out$outdata), khm_province, by="province", all.x=T) |> select(all_of(c("ADM1_EN", "Mean", "Total", "Obs"))) |> mutate(Mean=signif(Mean, 4), Total=signif(Total,4)) |> rename(Province=ADM1_EN)
        provPlot <- reportChart(merged_tab, "Province", plotStat, "", paste(varIn, "among", popVar))
      } else {
        merged_tab <- merge(na.omit(data_out$outdata), khm_zones, by="zone", all.x=T) |> select(all_of(c("zone", "Mean", "Total", "Obs"))) |> mutate(Mean=signif(Mean, 4), Total=signif(Total,4)) |> rename(Zone=zone) #issue with zone being a reserved name?
        provPlot <- reportChart(merged_tab, "Zone", plotStat, "", paste(varIn, "among", popVar))
      }
      
      if(bns==T){
        names(merged_tab)[names(merged_tab)=="Total"] <- "Total (BN)"
      } else if(mns==T){
        names(merged_tab)[names(merged_tab)=="Total"] <- "Total (MM)"
      }
      merged_tab$Total <- format(merged_tab$Total, big.mark=",")
      output$trendsTable2 <- DT::renderDT(merged_tab, rownames=F)
      output$provPlot2 <- renderPlot(provPlot, height=600)
    }
  })
  
}

shinyApp(ui = ui, server = server)
