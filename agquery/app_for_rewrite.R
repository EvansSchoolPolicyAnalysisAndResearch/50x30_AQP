options(shiny.error=browser) #For debugging 
library(shiny)
library(shinyBS)
library(tidyr)
#library(shinythemes)
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

thematic_shiny(
  font = "auto",
  sequential = colorRampPalette(colors = c("white", "#440154FF"))(12),
  qualitative = c("#440154FF",  "#21908CFF", "#3B528BFF", "#5DC863FF", "#FDE725FF")
)
options(shiny.useragg = TRUE)

import::from(spatstat.geom, weighted.median)

#root_dir <- paste0(getwd(), "/")
root_dir <- ""

#LOADING IN AND VALIDATING SPREADSHEETS
#Some of this could probably be cached for faster startup

indicatorCategories <- tryCatch(read.xlsx("Update/indicatorCategories.xlsx"),
                                error=function(e) {
                                  return(F)
                                  }) 
if(is.list(indicatorCategories)){
  if(("shortName" %in% names(indicatorCategories)) & ncol(indicatorCategories) > 1) {
  indicatorCategories <- indicatorCategories %>% 
    melt() %>% 
    filter(value==1) %>% 
    select(-value) %>% 
    rename(goalName=variable)
goalNames <- str_to_title(unique(indicatorCategories$goalName))
  }
}

dataset_list <- list.files("Data", pattern="*.csv")
instrument_list <- tryCatch(readxl::read_xlsx("Update/instrument_list.xlsx"),
         error=function(e){return(F)})
if(is.list(instrument_list)){
  colnm_instr <- c("survey","wave","country","year","yearlabel") #including only the essentials right now
  if(any(!(colnm_instr %in% names(instrument_list)))){
    instrument_list <- F
  } else {
  year_list <- as.list(instrument_list$year)
  names(year_list) <- instrument_list$yearlabel
  }
}

indicator_list <- tryCatch(readxl::read_xlsx("Update/indicators.xlsx"),
                           error=function(e){return(F)}
                           )

if(is.list(indicator_list)){
  colnm_indic <- c("shortName", "labelName","axisName", "file", "wins_limit", "units", "denominator") #Again, only what we minimally need to operate. Note we're moving flags to a new sheet
if(any(!(colnm_indic %in% names(indicator_list)))){
  indicator_list <- F
    }
  }


groups_list <- tryCatch(readxl::read_xlsx("Update/grouping_vars.xlsx"),
                        error=function(e){return(F)})
if(is.list(groups_list)){
  colnm_grps <- c("varName","label","shortName","Levels","Labels","level") #need to fix names here
  if(any(!(colnm_grps %in% names(groups_list)))){
    groups_list <- F
  }
}

policy_path <- tryCatch(read.csv("Update/Policy_Pathways.csv", header = TRUE),
                        error=function(e){return(F)}) #It's possible to pass bad inputs here that will just render as garbage on the table panel; let 'em
if(is.list(policy_path)){
pathwaysDT <- policy_path %>% select(-c(pathwayID, goalName))
names(pathwaysDT) <- str_replace_all(names(pathwaysDT), "\\.", " ")
pathway_names <- unique(policy_path$Policy.Goal)
}
#pathways <- readxl::read_xlsx(paste0(root_dir,"Update/Policy_Pathways.xlsx"))

#TODO - we should not need to do this during runtime
#pathways <- pathways[-c(10:13)]
#colnames(pathways)[8] <- "Related Indicator(s)"

#TODO - probably best to have all of these as CSV. Either way, we need consistency

pathway_link <- tryCatch(read.csv("Update/Policy_Link.csv") %>% distinct(), #Remove duplicates (bad input protection)
                        error=function(e){return(F)})
if(is.list(pathway_link)){
  colnm_link <- c("pathwayID", "goalName","shortName")
  if(any(!(colnm_link %in% names(pathway_link)))){
    pathway_link <- F
  }
}






khm_shp <- st_read(paste0(root_dir, "Spatial/cam_prov_merge.shp"), quiet=T)
khm_shp$ADM1_EN[khm_shp$ADM1_EN=="Oddar Meanchey"] <- "Otdar Meanchey" #Temp fix due to disagreement between 50x30 spelling and shapefile.
#Implement fuzzy matching later?

ui <- fluidPage(bg = "white", fg = "#3B528BFF", info="#474481", primary = "#440154FF",
                base_font = bslib::font_google("Open Sans"), 
                fluidRow(column(3, align='center', HTML("<img src=moa_logo.png width='40%'></img>")),
                         column(2, HTML("<h2>CAS Survey Data Explorer</h2>")),
                         column(3, align='center', HTML("<image src=cam_flag.png width='30%'></img>"))),
                #img(src='moa_logo.png', width='10%'),
                navbarPage(title="", theme = bslib::bs_theme(version="3",
                                                                                         bg = "white", fg = "#3B528BFF", info="#474481", primary = "#440154FF",
                                                                                         base_font = bslib::font_google("Open Sans")), 
                           tabPanel("Introduction", column(1),column(10, #To do: move this to a separate file.
                                                                     wellPanel(HTML(
                                                                       "The 50x30 Cambodia Data explorer is a tool to rapidly summarize and visualize the Cambodian Agricultural Survey data. In conjunction with other forms of analysis, this app..."
                                                                     )),
                                                                     img(src='logic-model.png', width='80%'),
                                                                     hr(),
                                                                     fluidRow(column(8, HTML(paste('<h3>Purpose</h3><br><p>The 50x30 Cambodia Data Explorer is a way to view and compare information from the Cambodian Agricultural Surveys to address the following policy priorities:',
                                                                                                   '<ul>',
                                                                                                   lapply(pathway_names, FUN=function(x){paste0("<li>",x, "</li>")}),
                                                                                                   '</ul></p>',
                                                                                                   '<h3>Using the Cambodia 50x30 App</h3> <p>The Cambodian Agricultural Survey contains information on household production of crops and livestock that can be used to understand trends in small-scale farmer contributions to national supply and the economic conditions small-scale producers face.',
                                                                                                   'Selecting a policy priority will allow you to narrow down the indicators to those considered most relevant.</p>',
                                                                                                   '<h3> Tabs </h3>',
                                                                                                   '<h4> Instructions </h4>',
                                                                                                   '<p>This tab provides step-by-step instructions, tips, and frequently asked questions (FAQs) about the data explorer.',
                                                                                                   '<h4>Policy Pathways</h4>',
                                                                                                   '<p>This tab overviews select academic and gray literature for policy pathways that can generate ideas for effective policies and programs that can help shift key indicators of agricultural development</p>',
                                                                                                   '<h4>Trends Explorer</h4>',
                                                                                                   '<p>This tab shows changes in variables between surveys and across provinces.</p>',
                                                                                                   '<h4>Data Explorer</h4>',
                                                                                                   '<p>This tab allows for direct comparisons of indicators and provides detailed graphs and summaries of correlations.</p>',
                                                                                                   '<h4>Downloads</h4>',
                                                                                                   '<p>This tab allows you to download the spreadsheets used to run the app and the processed survey data.</p>',
                                                                                                   '<br>',
                                                                                                   '<h3>Code and Data Availability</h3>',
                                                                                                   '<p> The Stata code used to process the data is publicly available at (Git Repository TBD). <br> The app source code and related files can be downloaded at (Git repository TBD)</p>',
                                                                                                   '<br>',
                                                                                                   '<h3>Inquire</h3>',
                                                                                                   '<p>This tool is maintained by <i>responsible party</i> who has <i>contact info</i>.</p>',
                                                                                                   '<br><br>',
                                                                                                   '<p> The raw data for the 50x30 survey is located at <a href="https://nada.nis.gov.kh/index.php/catalog/36">https://nada.nis.gov.kh/index.php/catalog/36</a>.</p><br><br><br>')
                                                                     )
                                                                     )
                                                                     )
                           )
                           ),
                           tabPanel("Policy Pathways",
                                    fluidRow(HTML('<p><h3>The Policy Pathways</h3></p>
                             <p>This table shows the results from a literature survey illustrating the contributions of different aspects of agricultural production on the policy priorities. This information can be used to explore relationships between indicators in the Data tab. The table can be downloaded as an excel sheet using the button below:</p><br>')),
                             fluidRow(dataTableOutput("path_table"), uiOutput("path_tbl_err"))
                           ),
                           tabPanel("Instructions",
                                    includeHTML('www/Instructions_50x30_D2.html')
                           ),
                           
                           ###########
                           # Trends Explorer Tab
                           ###########
                           tabPanel("Trends Explorer", shinyjs::useShinyjs(),
                                    fluidRow(column(4, uiOutput("trendsErr"))),
                                    fluidRow(column(4, selectInput('policiesBox1', "Select a policy priority", choices=c("None", goalNames)))),
                                    fluidRow(column(4, uiOutput('pathwaysBox1'))),
                                    fluidRow(column(2, uiOutput('msgText')) #, column(4, conditionalPanel(condition="input.policiesBox1!='None'", radioGroupButtons("trendChooser", "", choices=list(`Change Since Previous Survey`='prevSurv', `Long-term Trend`='trend')))
                                    ),
                                    column(6, conditionalPanel(condition="input.policiesBox1!='None'", uiOutput("trendVarChoose"))
                                    ),
                                    fluidRow(column(6, dataTableOutput('trendsTable')),
                                             column(6,
                                                    plotOutput('currMap'),
                                                    plotOutput('trendMap'),
                                                    uiOutput("plotsErr"))
                                    ),
                                    fluidRow(column(12, uiOutput("droppedVars"))),
                                    fluidRow(column(6, br(), bsCollapse(
                                      bsCollapsePanel("Detailed Information",
                                                      dataTableOutput('flagsTable'))
                                    )))
                           ),
                           
                           #################
                           # Data Explorer Tab
                           #################
                           tabPanel("Data Explorer",
                                    fluidRow(column(4,uiOutput("explorerErr"))),
                                    fluidRow(column(12, uiOutput('policiesBox2'))),
                                    fluidRow(column(12, uiOutput("pathwaysBox2"))),
                                    conditionalPanel(condition="input.policiesBox2!='None'",
                                                     fluidRow(column(4, uiOutput('pathwaysBox2'))),
                                                     fluidRow(column(12, radioGroupButtons('yearBtn', label="Survey Year", choices=year_list, selected=max(instrument_list$year)))),
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
                                    )
                           ),
                           tabPanel("Downloads", column(4, fluidRow(downloadButton('downloadExcel',
                                                                                   label='Download Indicators',
                                                                                   icon=icon('file-excel'))),
                                                        br(),
                                                        br(),
                                                        fluidRow(downloadButton('downloadRaw',
                                                                                label="Download Processed Data",
                                                                                icon=icon('file-csv'))),
                                                        br(),
                                                        br(),
                                                        fluidRow(downloadButton('downloadPathways',
                                                                                label='Download Policy Pathways',
                                                                                icon=icon('file-csv'))))
                                    
                           )
                )
)



server <- function(input, output, session) {
  
  output$policiesBox2 <- renderUI({if(exists("goalNames")){ 
    selectInput('policiesIn2', "Select the Policy Priority:", choices=c("None", goalNames)) 
  } else {
    selectInput('policiesIn2', "Select the Policy Priority:", choices="None")
    }
    })
  
  
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
  


  
  
  observeEvent(input$policiesBox1, {
    if(input$policiesBox1!="None" & is.list(policy_path)){
      inputChk <- is.null(input$pathwaysIn1)
      pathway_sub <- policy_path %>% filter(goalName==input$policiesBox1)
      #pathway_sub <- pathway_link %>% filter(goalName==input$policiesBox1)
      pathway_list <- as.list(c(pathway_sub$pathwayID,0)) 
      names(pathway_list) <- c(pathway_sub$Pathway, "All")
      output$pathwaysBox1 <- renderUI(selectInput("pathwaysIn1", "Choose a pathway (optional)", choices=pathway_list))
      
      if(!inputChk){
        shinyjs::disable('pathwaysIn')
        shinyjs::disable('policiesBox1')
        showNotification("Loading, please wait")
        updateTrends()
        shinyjs::enable('pathwaysIn')
        shinyjs::enable('policiesBox1')
      }
    }
  })
  
  observeEvent(input$pathwaysIn1, {
    shinyjs::disable('pathwaysIn1')
    shinyjs::disable('policiesBox1')
    showNotification("Loading, please wait")
    updateTrends()
    shinyjs::enable('pathwaysIn1')
    shinyjs::enable('policiesBox1')
  })
  
  observeEvent(input$policiesIn2, {
    if(input$policiesIn2!="None"){
    pathway_sub <- policy_path %>% filter(goalName==input$policiesIn2)
               pathway_list <- as.list(c(pathway_sub$pathwayID,0)) 
               names(pathway_list) <- c(pathway_sub$Pathway, "All")
               output$pathwaysBox2 <- renderUI(selectInput("pathwaysIn2", "Choose a pathway (optional)", choices=pathway_list))
    }
  }
  )
  
  observeEvent(input$trendIn, {
    if(input$trendIn!="0"){
      showNotification("Processing, please wait")
      #session$sendCustomMessage("disableButton", "start_proc")
      shinyjs::disable('trendIn')
      data_files_select <- indicator_list[which(indicator_list$shortName %in% input$trendIn),] %>% select(file) %>% distinct() %>% unlist() #TODO: Clean this up
      data_files <- dataset_list[which(str_detect(str_to_lower(dataset_list), str_to_lower(data_files_select)))] %>% as.data.frame()
      names(data_files) <- "file.name"
      data_files$year <- str_extract(data_files$file.name, "[0-9]{4}")
      
      #data_out <- getData(data_files$file.name, data_files$year, xvars=input$trendIn, adm_level="province", source_call="trendmaps")
      data_out <- getData(data_files, xvars=input$trendIn, adm_level="province", source_call="trendmaps")
      
      if(!any(data_out=="")){
        data_out <- data_out$tempdata
        
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
          diff <- data_out %>% pivot_wider(names_from=year, values_from=input$trendIn)
          diff[,4] <- diff[,3]-diff[,2]
          names(diff)[[4]] <- input$trendIn
          #diff$province_num <- df_max_year$province_num
          
          xShp_currMap <- merge(khm_shp, df_max_year, by.x="ADM1_EN", by.y="province", all.x=T) #changed y from province_num to province. Issue with the province_num not following alphabetical order meaning a numerical merge isn't good.
          xShp_trendMap <- merge(khm_shp, diff, by.x="ADM1_EN", by.y="province", all.x=T)
          
          
          currMap <- ggplot(xShp_currMap, aes_string(fill = input$trendIn)) +
            geom_sf() +
            ggtitle(paste(indicator_list$labelName[indicator_list$shortName == input$trendIn], ", ", max_year, " Values")) +
            #scale_fill_gradient2(low = "darkred", mid = "white", high = "darkblue", midpoint = 0, limit = c(min(xShp_currMap[[input$trendIn]], na.rm = TRUE), max(xShp_currMap[[input$trendIn]], na.rm = TRUE)), 
            #                     name =  str_wrap(paste(indicator_list$labelName[indicator_list$shortName == input$trendIn], min_year, "values", sep=" "), 10))+
            scale_fill_gradient2(low = "darkred", mid = "white", high = "darkblue", midpoint = 0, limit = c(min(xShp_currMap[[input$trendIn]], na.rm = TRUE), max(xShp_currMap[[input$trendIn]], na.rm = TRUE)))+
            theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
          
          trendMap <- ggplot(xShp_trendMap, aes_string(fill = input$trendIn)) +
            geom_sf() +
            ggtitle(paste0(indicator_list$labelName[indicator_list$shortName == input$trendIn], ", ", min_year, "-", max_year, " Difference")) +
            #scale_fill_gradient2(low = "darkred", mid = "white", high = "darkblue", midpoint = 0, limit = c(min(xShp_trendMap[[input$trendIn]], na.rm = TRUE), max(xShp_trendMap[[input$trendIn]], na.rm = TRUE)), 
            #                     name =  str_wrap(paste(indicator_list$labelName[indicator_list$shortName == input$trendIn],", ",paste0(min_year,"-",max_year), "difference", sep=" "), 10)) +
            scale_fill_gradient2(low = "darkred", mid = "white", high = "darkblue", midpoint = 0, limit = c(min(xShp_trendMap[[input$trendIn]], na.rm = TRUE), max(xShp_trendMap[[input$trendIn]], na.rm = TRUE))) +
            theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
          
          output$currMap <- renderPlot(currMap)
          output$trendMap <- renderPlot(trendMap)
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
    updatePlots(maps=T)
    
    
  })
  
  
  observeEvent(input$policiesIn2, {
    #target_policy=tolower(input$policiesBox2)
    #if(target_policy!="none"){
    if(input$policiesIn2!="None"){
      if(is.list(pathway_link) & is.list(indicator_list)) {
      indics_out <- pathway_link %>% filter(goalName==input$policiesIn2) %>% merge(., indicator_list, by="shortName") #Almost certainly a better way to do this.
      indics <- as.list(indics_out$shortName)
      names(indics) <- indics_out$labelName 
      indics <- unique(indics)
      updateBoxes(indics) #Might need to global this
      data_files_select <- indics_out %>% 
        select(file) %>% 
        distinct() %>%
        unlist() #Using tolower here helps filter out differences in capitalization 
      #survey_pref <- indics_out$survey[indics_out$year==input$yearBtn] # TO FIX; this line no longer does anything.
      data_files <- lapply(data_files_select, FUN=function(x){dataset_list[which(str_detect(str_to_lower(dataset_list), str_to_lower(x)))]}) %>% unique() %>% unlist()  #Drop duplicates if they're somehow in there.
      #data_files <- dataset_list %>% select(which(str_to_lower(dataset_list) %in% str_to_lower(data_files_select)))
      #data_files <- dataset_list[which(str_detect(str_to_lower(dataset_list), str_to_lower(data_files_select)))] %>% as.data.frame()
      data_files <- as.data.frame(data_files)
      names(data_files) <- "file.name"
      data_files$year <- str_extract(data_files$file.name, "[0-9]{4}") #Might be unnecessary 
      data_files <- filter(data_files, year==input$yearBtn)
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
        #truncated_pretty_names <- substr(pretty_names, 1, 35)
        #print(pretty_names)
        cor_matrix <- cor(data_out, use="pairwise.complete.obs")
        par(mar = c(5, 5, 4, 2) - 2)
        
        #corrPlot <- corrplot.mixed(cor_matrix, order = 'AOE')
        #output$corrPlot <- renderPlot(corrplot(cor_matrix, order = 'AOE',col=colorRampPalette(c("white","lightblue","red"))(100)))
        #print(corrPlot) 
        res <- match(rownames(cor_matrix), varnames$shortName)
        rownames(cor_matrix) <- varnames$labelName[res]
        res <- match(colnames(cor_matrix), varnames$shortName)
        colnames(cor_matrix) <- varnames$labelName[res]
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
            cor_value <- cor_matrix[i, j]
            p_value <- p_matrix[i, j]
            # Construct the hover text
            hover_text[i, j] <- paste0("P-value: ", format(p_value, digits = 3))
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
        output$heatMap <- renderPlotly(heatMap)
        #output$corrPlot <- renderPlot(corrPlot)
      }
      } else {
        showNotification("Error in input files; one or more not found.", type="error")
      }
    } 
  })
  



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
  })

#To do: compact this for better display.
if(exists("pathwaysDT")){
output$path_table <- renderDataTable(pathwaysDT,
                                     options = list(scrollX = TRUE,
                                                    pageLength = 5,
                                                    lengthMenu = c(2, 5, 10),
                                                    searching = FALSE,
                                                    autoWidth = TRUE,
                                                    columnDefs = list(list(width = '200px', targets = "_all"))
                                     ),
                                     rownames = FALSE
)
} else {
  output$path_tbl_err <- renderUI(verbatimTextOutput("Error: Pathways file not found or improperly formatted"))
}

}

shinyApp(ui = ui, server = server)
