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

thematic_shiny(
  font = "auto",
  sequential = colorRampPalette(colors = c("white", "#440154FF"))(12),
  qualitative = c("#440154FF",  "#21908CFF", "#3B528BFF", "#5DC863FF", "#FDE725FF")
  )
options(shiny.useragg = TRUE)

import::from(spatstat.geom, weighted.median)

#root_dir <- paste0(getwd(), "/")
root_dir <- ""

dataset_list <- list.files("Data", pattern="*.csv")
instrument_list <- readxl::read_xlsx("Update/instrument_list.xlsx")
year_list <- as.list(instrument_list$year)
names(year_list) <- instrument_list$yearlabel

#Update INDICATOR list to change which indicators are available
indicator_list <- readxl::read_xlsx(paste0(root_dir,"Update/indicators.xlsx"))

#Update GROUP LIST to modify the grouping variables.
groups_list <- readxl::read_xlsx(paste0(root_dir, "Update/grouping_vars.xlsx"))
group_cats <- unique(groups_list$level)
#group_labs <- unique(groups_list$level_lab)

#group_cats <- group_cats[group_cats!="Hidden" & group_cats!="Within Household"] #Filter out categories that we want to include but not have boxes for (because, for crops e.g. we have boxes elsewhere)

#Update this file to change filters
filters_list <- readxl::read_xlsx(paste0(root_dir, "Update/filterset.xlsx"))

#Admin levels, mainly for tagging datasets after manipulation
adm_list <- readxl::read_xlsx(paste0(root_dir, "Update/adm_levels.xlsx"))

khm_shp <- st_read(paste0(root_dir, "Spatial/cam_prov_merge.shp"))

#TODO: Why do we have two of these?
policy_path <- read.csv(paste0(root_dir,"Update/Policy_Pathways.csv"), header = TRUE)
pathways <- readxl::read_xlsx(paste0(root_dir,"Update/Policy_Pathways.xlsx"))

#TODO - we should not need to do this during runtime
pathways <- pathways[-c(10:13)]
colnames(pathways)[8] <- "Related Indicator(s)"

#TODO - probably best to have all of these as CSV. Either way, we need consistency
policy_link <- read.csv("Update/Policy_Link.csv") 
pathway_link <- policy_link %>% select(Goal.Id, Pathway, Pathway.Id) %>% distinct()


ui <- navbarPage(header=tags$head(
  #     
  #    
  singleton(HTML(
    '
    <script type="text/javascript">
    $(document).ready(function() {
    
    // disable start_proc button after a click
    Shiny.addCustomMessageHandler("disableButton", function(message) {
    $(".shiny-input-select").attr("disabled","true");
    $(".btn-default").attr("disabled", "true");
    });
    
    // Enable start_proc button when computation is finished
    Shiny.addCustomMessageHandler("enableButton", function(message) {
    $(".shiny-input-select").removeAttr("disabled");
    $(".btn-default").removeAttr("disabled");
    });
})
    </script>
    '
  ))
),
  title=HTML("<b>50x30 Cambodia Data Explorer</b>"), theme = bslib::bs_theme(version="3",
  bg = "white", fg = "#3B528BFF", info="#474481", primary = "#440154FF",
  base_font = bslib::font_google("Open Sans")), 
                tabPanel("Introduction", column(1),column(10,
                        wellPanel(
                        "The 50x30 Cambodia Data Explorer is a tool to inform policy-making in conjunction with other resources. Reviewing academic and grey literature for policy pathways can generate ideas for effective policies and programs that can help shift key indicators of agricultural development. Utilizing other tools and data from additional sources (e.g. FAO) can provide additional context for policy-making."
                        ),
                        img(src='logic-model.png', width='80%'),
                        hr(),
                        fluidRow(column(8, HTML('<p>The 50x30 agricultural survey contains household-level information that is potentially valuable 
                        to policy-makers. Each indicator contains data on the agricultural practices of sampled Cambodian households. Click the "Download Indicators" 
                        button to the right to view the names of indicators present in this tool.</p><br>
                        <p> The raw data for the 50x30 survey is located at <a href="https://nada.nis.gov.kh/index.php/catalog/36">https://nada.nis.gov.kh/index.php/catalog/36</a>. The processed version of the data we use in this app can be downloaded by clicking the "Download Processed Data" button (right).</span></p>
                        '))
                        )
                )
                ),
                tabPanel("Policy Pathways",
                         fluidRow(HTML('<p><h3>The Policy Pathways</h3></p>
                             <p>This table shows the results from a literature survey illustrating the contributions of different aspects of agricultural production on the policy priorities. This information can be used to explore relationships between indicators in the Data tab. The table can be downloaded as an excel sheet using the button below:</p><br>')),
                         fluidRow(dataTableOutput("path_table"))
                         ),
                tabPanel("Instructions",
                         includeHTML('www/Instructions_50x30_D2.html')
                         ),
                tabPanel("Trends Explorer", shinyjs::useShinyjs(),
                         fluidRow(column(4, selectInput('policiesBox1', "Policy Priority", choices=c("None", str_to_title(unique(indicator_list$indicatorCategory)))))),
                         fluidRow(column(4, uiOutput('pathwaysBox'))),
                         fluidRow(column(2, uiOutput('msgText')), column(4,
                                  conditionalPanel(condition="input.policiesBox1!='None'", radioGroupButtons("trendChooser", "", choices=list(`Change Since Previous Survey`='prevSurv', `Long-term Trend`='trend')))
                                  ),
                                  column(6, conditionalPanel(condition="input.policiesBox1!='None'", uiOutput("trendVarChoose"))
                         )),
                         fluidRow(column(6, dataTableOutput('trendsTable')),
                         column(6,
                                plotOutput('currMap'),
                                plotOutput('trendMap'))
                         ),
                         fluidRow(column(12, uiOutput("droppedVars")))
                         ),
                tabPanel("Data Explorer",
                fluidRow(column(12, selectInput('policiesBox2', "Policy Priority", choices=c("None", str_to_title(unique(indicator_list$indicatorCategory)))))), #radioButtons('policySelect', 'Policy Goal', choices=unique(indicator_list$policy_priority)))),
                fluidRow(column(12, radioGroupButtons('yearBtn', label="Survey Year", choices=year_list, selected=max(instrument_list$year)))),
                fluidRow(column(6, wellPanel(style="background-color: #ededed; border-color: #9c9c9c;",
                    
                    fluidRow(column(6, align='center', uiOutput('indicsBox')),
                    column(6, align='center', uiOutput('corrsBox'))),
                    fluidRow(column(6, align='center', uiOutput('indicsDesc')), column(6, align='center', uiOutput('corrsDesc'))),
                    hr(),
                    checkboxInput('yChk', 'Omit 0s from Indicator'),
                    radioButtons("disAgg_admin", HTML("<b>Select Administrative Level</b>"), choiceNames=c("Province","Household"), choiceValues=c("province", "hhid")),
                    #uiOutput("groupsChk"),
                    radioButtons("groupsChk", "Selecting Grouping Variable", choiceNames=c("None", groups_list$label), choiceValues=c("", groups_list$varName)),
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



server <- function(input, output, session) {
  #This is how we did the grouping vars in the original AgQuery+ - it's a little overengineered now because we don't need to make separate sets of radio buttons. Moved to the update box.
  #output$groupsChk <- renderUI({
  #  groupCheck <- lapply(1:length(group_cats), function(x){
  #    groupnames <- groups_list[which(groups_list$level %in% group_cats[[x]]),]
  #    radioButtons(group_cats[[x]], label=HTML("<b>Select Grouping Variable</b>"), choiceNames=c("None",groupnames$label), choiceValues=c("",groupnames$varName))
  #  })
  #})
  
  
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
  
 observeEvent(input$indicsIn, {
    #corrvals <- corr_list$corrSN[corr_list$indicatorSN %in% input$indicsIn] %>% unique()
    #corrs_in <- indicator_list[indicator_list$shortName %in% corrvals,]
    #corrs_list <- as.list(corrs_in$shortName)
    #names(corrs_list) <- corrs_in$labelName
    #output$corrChk <- renderUI(pickerInput("corrsIn", HTML("<b>Choose Correlate</b>"), choices=corrs_list, options=list(style="btn-info"), size=(length(corrs_list)+1)))

    #output$ttip <- renderUI(popify(bsButton("ttipSurvey", label=HTML("Source<br>question"), size = "medium", block=F),
    #                                            indicator_list$survey_question[indicator_list$shortName==input$indicsIn], indicator_list$ques_text[indicator_list$shortName==input$indicsIn],
    #                               placement="right", options = list(container = "body")))
   
    output$indicsDesc <- renderUI(HTML(sprintf('<table style="border: 3px #ddd; border-style: groove; padding: 9px;">
                                               <tr><td style="border: 3px #ddd; border-style: groove; padding: 9px;">%s</td></tr>
                                               <tr><td style="border: 3px #ddd; border-style: groove; padding: 9px;">%s</td></tr>
                                               ',
                                               #<tr><td style="border: 3px #ddd; border-style: groove; padding: 9px;">Mean: %s</td><tr>
                                               #<tr><td style="border: 3px #ddd: border-style: groove; padding: 9px;">Stdev: %s</td><tr>
                                               indicator_list$survey_question[indicator_list$shortName==input$indicsIn], 
                                               indicator_list$ques_text[indicator_list$shortName==input$indicsIn] 
                                               #round(weighted.mean(na.omit(data[[input$indicsIn]]), data[["weight"]][which(!is.na(data[[input$indicsIn]]))]), 2),
                                               #round(sd(na.omit(data[[input$indicsIn]])),2)
                                               )))
    })
  
  observeEvent(input$corrsIn, {
    output$corrsDesc <- renderUI(HTML(sprintf('<table style="border: 3px #ddd; border-style: groove; padding: 9px;">
                                              <tr><td style="border: 3px #ddd; border-style: groove; padding: 9px;">%s</td></tr>
                                              <tr><td style="border: 3px #ddd; border-style: groove; padding: 9px;">%s</td></tr>', 
                                              indicator_list$survey_question[indicator_list$shortName==input$corrsIn], 
                                              indicator_list$ques_text[indicator_list$shortName==input$corrsIn])))
  })
  
  
  #fluidRow(column(6, align='center', selectInput('indicsIn', HTML("<b>Select Indicator</b>"), choices=indics, size=length(indics), selectize=F),
  #                #                uiOutput('indicsDesc'), 
  #                #column(6, pickerInput('corrsIn', HTML('<b>Select Correlate</b>'), choices=indics, options=list(style='btn-info', size=length(indics))))),
  #                #column(6, align='center', selectInput('corrsIn', HTML('<b>Select Correlate</b>'), choices=indics, size=length(indics), selectize=F),
  
  updateBoxes <- function(indics){
    output$indicsBox <- renderUI(selectInput('indicsIn', HTML("<b>Select Indicator</b>"), choices=indics, size=length(indics), selectize=F)) 
    output$corrsBox <- renderUI(selectInput('corrsIn', HTML('<b>Select Correlate</b>'), choices=indics, size=length(indics), selectize=F))
  }
  
  
  observeEvent(input$policiesBox1, {
    if(input$policiesBox1!="None"){
      inputChk <- is.null(input$pathwaysIn)
      pathway_sub <- pathway_link %>% filter(Goal.Id==input$policiesBox1)
      pathway_list <- as.list(c(0, pathway_sub$Pathway.Id)) 
      names(pathway_list) <- c("All", pathway_sub$Pathway)
      output$pathwaysBox <- renderUI(selectInput("pathwaysIn", "Pathway", choices=pathway_list))
      
      if(!inputChk){
        updateTrends()
      }
    }
  
    })
  observeEvent(input$pathwaysIn, {
    updateTrends()
  }) 
  updateTrends <- function(){
    if(input$policiesBox1=="None"){
      output$msgText <- renderUI(HTML("<h4>Select a policy priority above to get started</h4>"))
    } else {
    
    #updateSelectInput(session, "policiesBox2", selected=input$policiesBox1)
    if(input$pathwaysIn==0) {
      indics_out <- policy_link %>% filter(Goal.Id==input$policiesBox1) %>% select(Variable) %>% distinct() %>% unlist()
    } else { 
      indics_out <- policy_link %>% filter(Pathway.Id==input$pathwaysIn) %>% select(Variable) %>% distinct() %>% unlist()
    }
    #ALT: Moving this to policiesbox2
    #indics <- as.list(indics_out)
    #names(indics) <- indicator_list$labelName[which(indicator_list$shortName %in% indics)]
    #updateBoxes(indics) #Might need to global this
    
    #Panel call for reference
    #tabPanel("Trends Explorer",
    #         selectInput('policiesBox1', "Policy Pathway", choices=str_to_title(unique(indicator_list$indicatorCategory))),
    #         uiOutput('pathwaysBox'),
    #         h3("Related Variables"),
    #         radioGroupButtons("trendChooser", "", choices=list(`Change Since Previous Survey`='prevSurv', `Long-term Trend`='trend')),
    #         uiOutput('trendsTable'),
    #         uiOutput('currMap'),
    #         uiOutput('trendMap')
    #),
    
    
    #data_files <- as.data.frame(dataset_list[str_detect(str_to_lower(dataset_list), str_to_lower(input$policiesBox1))]) #Might need to store this as a global later. 
    data_files_select <- indicator_list[which(indicator_list$shortName %in% indics_out),] %>% select(file) %>% distinct() %>% unlist() #TODO: Clean this up
    data_files <- dataset_list[which(str_detect(str_to_lower(dataset_list), str_to_lower(data_files_select)))] %>% as.data.frame()
    names(data_files) <- "file.name"
    data_files$year <- str_extract(data_files$file.name, "[0-9]{4}") #Might be unnecessary 
    #This gets tricky for variables that are only collected every three years in each of the rotating modules. 
    #if(input$trendChooser=='prevSurv') {
    #  data_files <- data_files %>% filter(year==max(year) | year==max(data_files$year[data_files$year!=max(data_files$year)])) #get highest and second highest values
    #} 
    
    #Get variables from the  
    data_out <- getData(data_files$file.name, data_files$year, indics_out)$tempdata 
    indics_out <- names(data_out)[which(names(data_out) %in% indics_out)] #filter out any variables that weren't processed
    data_table <- data.frame(shortName=indics_out) #TODO: Simplify
    data_table <- merge(data_table, indicator_list %>% select(shortName, labelName), by="shortName")
    data_table[[paste0(min(data_out$year), " Mean")]] <- NA
    data_table[[paste0(min(data_out$year), " N obs")]] <- NA
    data_table[[paste0(max(data_out$year), " Mean")]] <- NA
    data_table[[paste0(max(data_out$year), " N obs")]] <- NA
    data_table$Trend <- ""
    
    for(var in indics_out){ #TO DO: ADD WEIGHTING
      sub_data <- data_out %>% select(all_of(c(var, "year"))) %>% na.omit()
      if(nrow(sub_data)==0 | !is.numeric(sub_data[[var]])){
        next
      } else {
      if(length(unique(sub_data$year))<2){
        data_table$Trend[data_table$shortName==var] <- "N/A" 
      } else if(length(unique(sub_data$year>=2))) {
        min_mean <- mean(sub_data[[var]][sub_data$year==min(sub_data$year)]) #TO DO: Eliminate redundancies around year tracking.
        max_mean <- mean(sub_data[[var]][sub_data$year==max(sub_data$year)])
        min_n <- sum(as.numeric(!is.na(sub_data[[var]][sub_data$year==min(sub_data$year)])))
        max_n <- sum(as.numeric(!is.na(sub_data[[var]][sub_data$year==max(sub_data$year)])))
        if(min_mean==0){ 
         if(max_mean > 0){
           chg = "+Inf"
         } else if(max_mean < 0) {
           chg="-Inf"
         } else {
           chg="⮕ 0%"
         }
        } else {  
          diff=signif((max_mean-min_mean)/min_mean*100, 2)
        
        if(diff>5){
          dir_arrow <- "⬆ "
        } else if(diff < -5) {
          dir_arrow <- "⬇ "
        } else {
          dir_arrow <- "⮕ "
        }
        
        chg=paste0(dir_arrow, diff, "%")  
        }
        data_table[[paste0(min(data_out$year), " Mean")]][data_table$shortName==var] <- signif(min_mean,4)
        data_table[[paste0(max(data_out$year), " Mean")]][data_table$shortName==var] <- signif(max_mean,4)
        data_table[[paste0(min(data_out$year), " N obs")]][data_table$shortName==var] <- min_n
        data_table[[paste0(max(data_out$year), " N obs")]][data_table$shortName==var] <- max_n
        data_table$Trend[data_table$shortName==var] <- chg
      } #Implement regression later?
        
      }
    }
    output$msgText <- renderUI(HTML("<h3>Related Variables</h3>"))
    trendVarList <- as.list(c("0", data_table$shortName))
    names(trendVarList) <- c("Select...", data_table$labelName)
    data_table <- data_table %>% rename(Variable=labelName) %>% select(-shortName)
    output$trendsTable <- renderDataTable(data_table, options=list(paging=F, searching=F), rownames=F)
    
    output$trendVarChoose <- renderUI(selectInput('trendIn', "Choose a variable to map:", choices=trendVarList))
    #output$trendsTable <- renderDataTable(data_table)
    }
  }
  
  observeEvent(input$trendIn, {
    if(input$trendIn!="0"){
      showNotification("Processing, please wait")
      #session$sendCustomMessage("disableButton", "start_proc")
      shinyjs::disable('trendIn')
      data_files_select <- indicator_list[which(indicator_list$shortName %in% input$trendIn),] %>% select(file) %>% distinct() %>% unlist() #TODO: Clean this up
      data_files <- dataset_list[which(str_detect(str_to_lower(dataset_list), str_to_lower(data_files_select)))] %>% as.data.frame()
      names(data_files) <- "file.name"
      data_files$year <- str_extract(data_files$file.name, "[0-9]{4}")
      
      data_out <- getData(data_files$file.name, data_files$year, xvars=input$trendIn, adm_level="province", source_call="trendmaps")
      
      if(!any(data_out=="")){
        data_out <- data_out$tempdata
      
      data_out$province_num <- as.numeric(data_out$province)
      max_year <- max(data_out$year)
      min_year <- min(data_out$year)
      
      df_min_year=data_out %>% filter(year==min_year)
      df_max_year=data_out %>% filter(year==max_year)
      diff <- df_max_year-df_min_year
      diff$province_num <- df_max_year$province_num
      
      xShp_currMap <- merge(khm_shp, df_max_year, by.x="province", by.y="province_num")
      xShp_trendMap <- merge(khm_shp, diff, by.x="province", by.y="province_num")
      
      currMap <- ggplot(xShp_currMap, aes_string(fill = input$trendIn)) +
        geom_sf() +
        ggtitle(paste(indicator_list$labelName[indicator_list$shortName == input$trendIn], ", ", max_year, " Values")) +
        labs(fill = "") +
        #theme_map()
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      
      trendMap <- ggplot(xShp_trendMap, aes_string(fill = input$trendIn)) +
        geom_sf() +
        ggtitle(paste0(indicator_list$labelName[indicator_list$shortName == input$trendIn], ", ", min_year, "-", max_year, " Difference")) +
        labs(fill = "") +
        #theme_map()
      theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      
      output$currMap <- renderPlot(currMap)
      output$trendMap <- renderPlot(trendMap)
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
  
  
  observeEvent(input$policiesBox2, {
    target_policy=tolower(input$policiesBox2)
    if(target_policy!="none"){
      indics_out <- indicator_list %>% filter(indicatorCategory==input$policiesBox2)
      indics <- as.list(indics_out$shortName)
      names(indics) <- indics_out$labelName
      updateBoxes(indics) #Might need to global this
    files_list <- indicator_list[which(tolower(indicator_list$indicatorCategory)==target_policy),] %>% select(file) %>% unique() #Using tolower here helps filter out differences in capitalization 
    survey_pref <- instrument_list$survey[instrument_list$year==input$yearBtn]
    for(file in files_list){
      infile <- list.files("Data", sprintf("%s_%s_%s", survey_pref, input$yearBtn, file), ignore.case=T, full.names=T) #this differs from the other file loading subroutine in getData - should probably make them consistent.
      if(length(infile)!=0){
      temp <- read.csv(infile)
      if(!exists("data_out")){
        data_out <- temp
      } else {
        data_out <- merge(data_out, temp, by="hhid")
      }
      }
    } 
    #data_out <- data_out %>% mutate(indicatorCategory=tolower(indicatorCategory)) %>% subset(indicatorCategory==target_policy, select=all_of(indicator_list$shortName)) %>% na.omit()
    if(exists("data_out")){
    data_out <- data_out[,(names(data_out) %in% indicator_list$shortName[which(tolower(indicator_list$indicatorCategory)==target_policy)])]
    varnames <- data.frame(shortName=names(data_out))
    varnames <- merge(varnames, indicator_list %>% select(shortName, labelName), by="shortName")
    #label_names <- indicator_list$labelName[which(indicator_list$shortName %in% names(data_out))]
    #ALT: Fix for bad input, specific to CAS variable coding (if someone exports labels instead of values); possible to remove if we return to dta input or with different data.
    for(currVar in names(data_out)){
      if(all(is.na(data_out[[currVar]])) | all(data_out[[currVar]]==0)){
        data_out <- data_out %>% select(!matches(currVar))
        }
      if(!is.numeric(data_out[[currVar]])){
      data_out <- data_out %>% mutate_at(currVar, list(~ recode(., 'None'='0', 'No'='0', 'Yes'='1')))
      data_out[[currVar]] <- as.numeric(data_out[[currVar]])
        }
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
      layout(title = "Correlation Heatmap", margin = list(t = 60), height=600)
    output$heatMap <- renderPlotly(heatMap)
    #output$corrPlot <- renderPlot(corrPlot)
    }
    }
  })
  
  #observeEvent(input$yChk, {
  #  updatePlots()
  #}, ignoreInit=T)
  
  #observeEvent(input$disAgg_admin, {
  #  updatePlots(maps=F)
  #}, ignoreInit=T)
  
  #observeEvent(input$Household, { #Hard coding input$Household even though the name affected by the spreadsheet. Might want to adjust how the spreadsheet gets handled in the future.
  #  updatePlots(maps=F)
  #}, ignoreInit=T)
  
  #observeEvent(input$corrsIn, {
  #  updatePlots()
  #}, ignoreInit=T)
  
	#corrvals <- reactive({
  #  req(input$indicsIn) # Ensure that indicsIn is not NULL
  #  corr_list$corrSN[corr_list$indicatorSN %in% input$indicsIn] %>% unique()
  #})		
	
  getData <- function(files, years, xvars, yvars=NULL, adm_level="hhid", aggs_list=NULL, source_call="none", drop_0s=F){
    varslist <- c(xvars, yvars)
    aggs_list <- c(aggs_list, "year")
    out_flag <- F
    exit <- F
    for(file in files){
      df <- tryCatch(read.csv(paste0("Data/", file)), 
                     error=function(e){
                       showNotification(paste("File", file, "not found"), type="error")
                       break
                     }) #can simplify using full paths in list.files
      varslist_short <- names(df)[which(names(df) %in% varslist)]
      if(length(varslist_short)==0){
        showNotification(paste("No variables for policy priority", input$policyBox1, "were found in", str_extract(file, "2[0-9]{3}")))
      } else {
        df <- df %>% mutate(year = as.numeric(str_extract(file, "2[0-9]{3}"))) %>% 
          select(all_of(c("hhid","province", varslist_short, "weight", aggs_list))) #At some point we're going to need to figure out how to undo the hard coding of province for portability to other countries.
        
        #TODO: Fix this w/r/t the trends page. 
        if(drop_0s){
          df <- df[df[[yvars]]!=0,]
        }
        
        for(currVar in varslist_short) {
          #Error handling
          if(!(currVar %in% indicator_list$shortName) | all(is.na(df[[currVar]]))){
            varslist_short <- varslist_short[-which(varslist_short==currVar)]
            if(!exists("dropped_vars")){
              dropped_vars <- currVar
            } else {
              dropped_vars <- c(dropped_vars, currVar)
            }
            next
          }
        }
        if(length(varslist_short)==0){ 
        showNotification(paste("Error: Data file", file, "is empty or only contains variables not listed in indicators_list"), type="error")
          } else {
        for(currVar in varslist_short){
          
          #Error handling: in case the data export still has the Stata labels (protects against bad exports; might be better to use dtas instead to avoid this entirely).
          if(!is.numeric(df[[currVar]])){
            df <- df %>% mutate_at(currVar, list(~ recode(., 'None'='0', 'No'='0', 'Yes'='1')))
            df[[currVar]] <- as.numeric(df[[currVar]])
          }
          
          var_unit <- subset(indicator_list, shortName %in% currVar)$units[[1]] #Should only be 1 list item
          var_continuous <- max(c("count","ratio", "boolean") %in% var_unit)==0
          if(var_continuous==T) { 
            l_wins_threshold <- (indicator_list$wins_limit[[which(indicator_list$shortName %in% currVar)]])/100
            u_wins_threshold <- 1-l_wins_threshold
            
            if(!is.numeric(l_wins_threshold)) { #I.e., spreadsheet cell was empty or boxes were somehow made blank
              l_wins_threshold <- 0
            }
            if(!is.numeric(u_wins_threshold)){
              u_wins_threshold <- 1
            }
            
            #Use zeros in the spreadsheet for vars that you don't want to winsorize
            lim <- quantile(df[[currVar]],probs=c(l_wins_threshold, u_wins_threshold), na.rm=T)
            df[[currVar]][df[[currVar]] < lim[1]] <- lim[1] 
            df[[currVar]][df[[currVar]] > lim[2]] <- lim[2] 
          }
        }
        
        if(exists("dropped_vars")){
        output$droppedVars <- renderText(paste("The following variables were missing from the indicators_list spreadsheet or were all NA and were not processed:", paste(unique(dropped_vars), collapse=", ")))
        }
        
        if (any(aggs_list %in% "livestock_area")) {
          #tempdata$livestock_area <- cut(tempdata$livestock_area, c(-1, 0, 0.01, 0.05, 0.1, max(tempdata$livestock_area)* 1.1), c("0 ha", "<=0.01 ha", ">0.01 - 0.05 ha", ">0.05 - 0.1 ha", ">0.1 - 0.25 ha"))
          df$livestock_area <- cut(df$livestock_area, c(-1, 0, 0.01, 0.05, 0.1, max(df$livestock_area)* 1.1), c("0 ha", "<=0.01 ha", ">0.01 - 0.05 ha", ">0.05 - 0.1 ha", ">0.1 - 0.25 ha"))																																																	
        }
        
        if(!exists("subsetdata")){
          subsetdata <- df
        } else { 
          subsetdata <- bind_rows(subsetdata, df)
        }
        }
      }
    }
    
    if(exists("subsetdata")){
      if(!nrow(subsetdata)==0){
        #Long term: Might need to find a different way to handle this.
        if(adm_level!="hhid"){
          outdata <- subsetdata %>% select(all_of(c(aggs_list, adm_level))) %>% distinct()
          for(currVar in varslist_short){
            tempdata <- subsetdata %>% select(all_of(c(aggs_list, adm_level, currVar, "weight"))) %>%
              group_by(!!!syms(c(aggs_list, adm_level))) %>% 
              na.omit() %>%
              summarize(value=weighted.mean(!!sym(currVar), weight))
            names(tempdata)[names(tempdata)=="value"] <- currVar
            if(!exists('outdata')){
              outdata <- tempdata
            } else {
              outdata <- merge(outdata, tempdata, by=c(aggs_list, adm_level))
            }
          } 
          } else {
          outdata <- subsetdata
        }
        
        pivotbyvars <- c('province', 'name')
        groupbyvars <- c('province', varslist_short, "weight", aggs_list)
        #groupbyvars <- groupbyvars[nzchar(groupbyvars)]
        if(source_call!="data"){ #Minor kludge because we're getting a pivot longer error here if there's variables missing.
        mapdata <- subsetdata  %>% select(all_of(groupbyvars)) %>% 
          na.omit() %>% 
          pivot_longer(., varslist_short) %>% 
          group_by(across(all_of(pivotbyvars))) %>% 
          summarize(value=weighted.mean(value, weight)) %>%
          pivot_wider()
        names(mapdata)[names(mapdata)=="value"] <- currVar
        mapdata$province_num <- as.numeric(mapdata$province)
        xShp <- merge(khm_shp, mapdata, by.x="province", by.y="province_num")
        return(list(tempdata=outdata, mapdata=xShp))
        } else {
          return(list(tempdata=outdata))
        }
      } 
    }
     else {
      showNotification("Error: Data not found", type="error")
      return("")
    }
  }

										  

  
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
  
  output$path_table <- renderDataTable(pathways,
                                       options = list(scrollX = TRUE,
                                                      pageLength = 2,
                                                      lengthMenu = c(2, 5, 10),
                                                      searching = FALSE,
                                                      autoWidth = TRUE,
                                                      columnDefs = list(list(width = '200px', targets = "_all"))),
                                       rownames = FALSE
                                       )
  
  updatePlots <- function(tab="data", maps=T){
    #Old way
    #aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    #aggs_list <- aggs_list[aggs_list!=""]
    
    if(tab=="data"){ 
      #getData <- function(files, years, xvars, yvars=NULL, adm_level="hhid", aggs_list=NULL, source_call=NULL)
      #Find better way to do this.
      aggs_list = input$groupsChk
      if(aggs_list==""){
        aggs_list <- NULL
      }
      #messy error handling here
      all_data <- getData(paste0("CAS_", input$yearBtn, "_", tolower(input$policiesBox2), ".csv"), xvars=input$indicsIn, yvars=input$corrsIn, adm_level=input$disAgg_admin, aggs_list=aggs_list, source_call="explorer")
    } 
    if(any(all_data!="")){
    #else if(tab=="trend"){
    #ALT - might be easier than what we do now with the maps in a separate area. Maybe build out later.
    #}
    mapdata <- all_data$mapdata
    outdata <- all_data$tempdata #Note to go back and fix the naming here.
    outdata <- na.omit(outdata)
    if(nrow(outdata)==0){
      showNotification("Error: No non-n/a observations in dataset", type="error") 
      } else { 
    xvars = input$corrsIn
    yvars = input$indicsIn
    if(!all(c(xvars, yvars) %in% names(outdata))){
      #Error notification here?
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
    xlab = indicator_list$labelName[indicator_list$shortName==xvars]
    ylab = indicator_list$labelName[indicator_list$shortName==yvars]

    if(input$groupsChk==""){
      indicatorHist <- ggplot(outdata, aes_string(x=yvars))+
        geom_histogram(bins = bins) +
        labs(x=xlab, y="Number of Observations")+
        ggtitle(str_to_title(paste("Histogram of", ylab))) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      corrHist <- ggplot(outdata, aes(x = !!sym(xvars))) +
        geom_histogram(bins = bins) +
        labs(x = xlab, y = "Number of Observations") +
        ggtitle(str_to_title(paste("Histogram of", indicator_list$labelName[indicator_list$shortName == xvars]))) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
        
      scatterPlot <- ggplot(outdata, aes(x=!!sym(xvars), y=!!sym(yvars))) + #only one yvar for now
        geom_point() +
        stat_smooth(method="lm") +
        labs(x=xlab, y=ylab) +
        ggtitle(str_to_title(paste("Scatterplot of",str_to_title(ylab), "\n",  "and", str_to_title(xlab )))) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      #print(heatMap) 
    } else {
      aggs_lab = groups_list$shortName[groups_list$varName==aggs_list]
      if(!is.factor(outdata[[aggs_list]])){
        flevels = groups_list[which(groups_list$varName==aggs_list),]$Levels %>% str_split(., ",") %>% unlist()
        flabels = groups_list[which(groups_list$varName==aggs_list),]$Labels %>% str_split(., ",") %>% unlist()
        outdata[[aggs_list]] <- factor(outdata[[aggs_list]], levels=flevels, labels=flabels)
      }
      corrHist <- ggplot(outdata, aes_string(x=xvars, group=aggs_list, fill=aggs_list))+
        geom_histogram(bins = bins)+
        #geom_density(fill=NA)+scale_color_discrete(guide='none')+
        labs(x=xlab, y="Number of Observations", fill=aggs_lab)+
        ggtitle(str_to_title(paste("Histogram of", xlab)))  +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
              
      indicatorHist <- ggplot(outdata, aes_string(x=yvars, group=aggs_list, fill=aggs_list))+
        geom_histogram(bins = bins)+
        #geom_density(fill=NA)+scale_color_discrete(guide='none')+
        labs(x=xlab, y="Number of Observations", fill=aggs_lab)+
        ggtitle(str_to_title(paste("Histogram of", ylab))) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      
      scatterPlot <- ggplot(outdata, aes(x=!!sym(xvars), y=!!sym(yvars), group=!!sym(aggs_list), color=!!sym(aggs_list)))+ #only one yvar for now
        geom_point()+
        stat_smooth(method="lm")+
        labs(x=indicator_list$labelName[indicator_list$shortName==xvars], y=indicator_list$labelName[indicator_list$shortName==yvars], color=aggs_lab)+
        ggtitle(paste("Scatterplot of",str_to_title(ylab), "\n",  "and", str_to_title(xlab ))) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      
    }
    if(maps==T){
    corrMap <- ggplot(mapdata, aes_string(fill = xvars)) +
      geom_sf() +
      ggtitle(str_to_title(paste("Map of", indicator_list$labelName[indicator_list$shortName == xvars], "by Province"))) +
      labs(fill = "") + 
      theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
    indicatorMap <- ggplot(mapdata, aes_string(fill = yvars)) +
      geom_sf() +
      ggtitle(str_to_title(paste("Map of", indicator_list$labelName[indicator_list$shortName == yvars], "by Province"))) +
      labs(fill = "") +
      theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
    }
    res <- eval(parse_expr(sprintf("with(outdata, cor.test(%s, %s))", xvars, yvars)))
    
    if(res$p.value > 0.2) {
      adj = "<font color='#e51f1f'>no</font>"
    } else if(res$p.value <= 0.2) {
      adj="<font color='#f2a134'>low</font>"
    } else if(res$p.value <= 0.1) {
      adj="<font color='#f7e379'>moderate</font>"
    } else if(res$p.value <= 0.05) {
      adj="<font color='#bbdb44'>high</font>"
    } else if(res$p.value <= 0.01) {
      adj="<font color='#44ce1b'>very high</font>"
    }
    
    res_out <- sprintf("<div style='font-family: 'Open Sans';'><br><br>There is %s%% (%s%% - %s%%) correlation between <font color='#0a2167'><b>%s</b></font> and <font color='#0a2167'><b>%s</b></font>. There is %s confidence in this result.</div>", 
    round(res$estimate[[1]]*100, 1), round(res$conf.int[[1]]*100, 1), round(res$conf.int[[2]]*100, 1),
    xlab, ylab, adj
    )
    
    output$indicatorHist <- renderPlot(indicatorHist)
    output$corrHist <- renderPlot(corrHist)
    output$scatterPlot <- renderPlot(scatterPlot)
    output$plotInterp <- renderUI(HTML(res_out))

    if(maps==T){
    output$indicatorMap <- renderPlot(indicatorMap)
    output$corrMap <- renderPlot(corrMap)
    }
    }
    }
  }
  }
}

shinyApp(ui = ui, server = server)
