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

policy_path <- read.csv(paste0(root_dir,"Update/Policy_Pathways.csv"), header = TRUE)
pathways <- readxl::read_xlsx(paste0(root_dir,"Update/Policy_Pathways.xlsx"))

#TODO - we should not need to do this during runtime
pathways <- pathways[-c(10:13)]
colnames(pathways)[8] <- "Related Indicator(s)"

ui <- navbarPage(title=HTML("<b>50x30 Cambodia Data Explorer</b>"), theme = bslib::bs_theme(version="3",
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
                         fluidRow(dataTableOutput("path_table"))),
                tabPanel("Instructions",
                         includeHTML('www/Instructions_50x30_D2.html')),
                tabPanel("Trends Explorer",
                         fluidRow(column(4, selectInput('policiesBox1', "Policy Priority", choices=c("None", str_to_title(unique(indicator_list$indicatorCategory)))))),
                         fluidRow(column(4, uiOutput('pathwaysBox'))),
                         fluidRow(column(2, uiOutput('msgText')), column(4,
                                  conditionalPanel(condition="input.policiesBox1!='None'", radioGroupButtons("trendChooser", "", choices=list(`Change Since Previous Survey`='prevSurv', `Long-term Trend`='trend'))),
                                  conditionalPanel(condition="input.policiesBox1!='None'", uiOutput("trendVarChoose"))
                         )),
                         fluidRow(column(6, dataTableOutput('trendsTable')),
                         column(6,uiOutput('currMap'),
                         uiOutput('trendMap'))
                         )),
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
                    uiOutput("groupsChk"),
                    actionButton('submitBtn', "Compare Variables")))),
                column(6, 
                #plotOutput('corrPlot'),
                plotlyOutput('heatMap')),
                br(),
                br(),
  fluidRow(column(6, uiOutput('indicHeader')) ,column(6, uiOutput('corrHeader'))),
  fluidRow(column(6, plotOutput('indicatorHist')), column(6, plotOutput('corrHist'))),
  fluidRow(column(6, plotOutput('indicatorMap')), column(6, plotOutput('corrMap'))),
  fluidRow(plotOutput('scatterPlot')),
  fluidRow(uiOutput('plotInterp')),
  tabPanel("Downloads", column(4, fluidRow(downloadButton('downloadExcel',
                                                          label='Download Indicators',
                                                          icon=icon('file-excel'))),
                               br(),
                               br(),
                               fluidRow(downloadButton('downloadRaw',
                                                       label="Download Processed Data",
                                                       icon=icon('file-csv'))))
  ),
  br(),
  br(),
  fluidRow(downloadButton('downloadPathways',
                          label='Download Policy Pathways',
                          icon=icon('file-csv')))

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
    #names(corrs_list) <- corrs_in$prettyName
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
    output$trendVarChoose <- renderUI(selectInput('trendIn', "Choose a variable to map:", choices=indics))
  }
  
  observeEvent(input$policiesBox1, {
    test <- T
    
    #updateSelectInput(session, "policiesBox2", selected=input$policiesBox1)
    if(input$policiesBox1=="None") {
      indics_out <- indicator_list
    } else {
      indics_out <- indicator_list %>% filter(indicatorCategory==input$policiesBox1)
    }
    indics <- indics_out$shortName
    names(indics) <- indics_out$labelName
    updateBoxes(indics) #Might need to global this
    if(test==T) { 
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
    if(input$policiesBox1=="None"){
      output$msgText <- renderUI(HTML("<h4>Select a policy priority above to get started</h4>"))
    } else {
    data_files <- as.data.frame(dataset_list[str_detect(str_to_lower(dataset_list), str_to_lower(input$policiesBox1))]) #Might need to store this as a global later. 
    names(data_files) <- "file.name"
    data_files$year <- str_extract(data_files$file.name, "[0-9]{4}") #Might be unnecessary 
    #This gets tricky for variables that are only collected every three years in each of the rotating modules. 
    #if(input$trendChooser=='prevSurv') {
    #  data_files <- data_files %>% filter(year==max(year) | year==max(data_files$year[data_files$year!=max(data_files$year)])) #get highest and second highest values
    #} 
    
    #Get variables from the  
    #xvars_list <- ... #Still need to create this file from the policy pathways 
    xvars_list <- indicator_list$shortName #TEMPORARY: UPDATE WHEN CLAIRE SENDS PATHWAY VARIABLES
    data_out <- getData(data_files$file.name, data_files$year, xvars_list)$tempdata 
    data_table <- as.data.frame(xvars_list) 
    names(data_table) <- "shortName"
    data_table <- merge(data_table, indicator_list %>% select(shortName, labelName), by="shortName")
    data_table$Trend <- "" 
    for(var in xvars_list){ #TO DO: ADD WEIGHTING
      sub_data <- data_out %>% select(all_of(c(var, "year"))) %>% na.omit()
      if(nrow(sub_data)==0 | !is.numeric(sub_data[[var]])){
        next
      } else {
      if(length(unique(sub_data$year))<2){
        data_table$Trend[data_table$shortName==var] <- "N/A" 
      } else if(length(unique(sub_data$year>=2))) {
        min_mean <- mean(sub_data[[var]][sub_data$year==min(sub_data$year)]) #TO DO: Eliminate redundancies around year tracking.
        max_mean <- mean(sub_data[[var]][sub_data$year==max(sub_data$year)])
        diff=signif((max_mean-min_mean)/min_mean*100, 2)
        
        if(diff>5){
          dir_arrow <- "⬆ "
        } else if(diff < -5) {
          dir_arrow <- "⬇ "
        } else {
          dir_arrow <- "⮕ "
        }
        
        chg=paste0(dir_arrow, diff, "%")  
        data_table$Trend[data_table$shortName==var] <- chg
      } #Implement regression later?
        
      }
    }
    output$msgText <- renderUI(HTML("<h3>Related Variables</h3>"))
    data_table <- data_table %>% select(Variable=labelName, Trend)
    output$trendsTable <- renderDataTable(data_table, options=list(paging=F, searching=F), rownames=F)
    #output$trendsTable <- renderDataTable(data_table)
    }
    }
  })
  
  updateTrendsTab <- function(){
    #Placeholder!
  }
  
  
  #The idea here is that we want to keep the policies aligned so that people don't have to toggle back and forth, but that generates a major computational load given all the things conditioned on these boxes right now. Commenting it out, will come back to it when conditions improve.
  #observeEvent(input$policiesBox2, {
  #  updateSelectInput(session, "policiesBox1", selected=input$policiesBox2)
  #})
  

  observeEvent(input$submitBtn, {
    updatePlots(maps=T)
    output$indicHeader <- renderUI(HTML(sprintf('<div style="border: 1px solid #ddd; padding: 9px; margin-bottom: 0px; line-height: 1.2; text-align: center; border-radius: 3px;"> %s </div>'
                                                , indicator_list$prettyName[indicator_list$shortName==input$indicsIn])))
    output$corrHeader <- renderUI(HTML(sprintf('<div style="border: 1px solid #ddd; padding: 9px; margin-bottom: 0px; line-height: 1.2; text-align: center; border-radius: 3px;"> %s </div>'
                                               , indicator_list$prettyName[indicator_list$shortName==input$corrsIn])))
  })
  
  
  observeEvent(input$policiesBox2, {
    target_policy=tolower(input$policiesBox2)
    #group_vars 
    if(target_policy!="none"){
    files_list <- indicator_list[which(tolower(indicator_list$indicatorCategory)==target_policy),] %>% select(file) %>% unique() #Using tolower here helps filter out differences in capitalization 
    survey_pref <- instrument_list$survey[instrument_list$year==input$yearBtn]
    for(file in files_list){
      temp <- read.csv(sprintf("Data/%s_%s_%s.csv", survey_pref, input$yearBtn, file))
      if(!exists("data_out")){
        data_out <- temp
      } else {
        data_out <- append(merge(data_out, temp, by="hhid"))
      }
    } 
    df <- df %>% mutate(indicatorCategroy=tolower(indicatorCategory)) %>% subset(indicatorCategory==target_policy, select=all_of(indicator_list$shortName)) %>% na.omit()
    
    varnames <- colnames(df)
    matched_indices <- match(varnames, indicator_list$shortName)
    label_names <- indicator_list$labelName[matched_indices]
    #truncated_pretty_names <- substr(pretty_names, 1, 35)
    #print(pretty_names)
    cor_matrix <- cor(df)
    par(mar = c(5, 5, 4, 2) - 2)
    
    #corrPlot <- corrplot.mixed(cor_matrix, order = 'AOE')
    #output$corrPlot <- renderPlot(corrplot(cor_matrix, order = 'AOE',col=colorRampPalette(c("white","lightblue","red"))(100)))
    #print(corrPlot) 
    
    rownames(cor_matrix) <- label_names
    colnames(cor_matrix) <- label_names
    #print(cor_matrix)
    p_matrix <- matrix(nrow = ncol(df), ncol = ncol(df))
    for(i in seq_len(ncol(df))) {
      for(j in seq_len(ncol(df))) {
        test_result <- cor.test(df[, i], df[, j], method = "pearson")
        p_matrix[i, j] <- test_result$p.value
      }
    }
    #print(p_matrix)
    p_matrix[upper.tri(p_matrix)] <- NA
    hover_text <- matrix("", nrow = ncol(df), ncol = ncol(df))
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
	
  getData <- function(files, years, xvars, yvars=NULL, adm_level="hhid", aggs_list=NULL, source_call=NULL){
    varslist <- c(xvars, yvars)
    aggs_list <- c(aggs_list, "year")
    for(file in files){
      df <- tryCatch(read.csv(paste0("Data/", file)), 
                     error=function(e){
                       showNotification(paste("File", file, "not found"), type="error")
                       break
                     }) #can simplify using full paths in list.files
      varslist_short <- varslist[which(varslist %in% names(df))]
      if(length(varslist_short)==0){
        showNotification(paste("No variables for policy priority", input$policyBox1, "were found in", str_extract(file, "2[0-9]{3}")))
      } else {
        if(length(varslist_short) < length(varslist)){
          showNotification("Warning: Not all variables in the list provided were found in the data", type="warning")
        }
        df <- df %>% mutate(year = as.numeric(str_extract(file, "2[0-9]{3}"))) %>% 
          select(all_of(c("hhid","province", varslist_short, "weight", aggs_list))) #At some point we're going to need to figure out how to undo the hard coding of province for portability to other countries.
        
        #TODO: Fix this w/r/t the trends page. 
        if(input$yChk){
          df <- df[df[[yvars]]!=0,]
        }
        
        for(currVar in varslist_short) {
          #Kludge: remove when we've fixed the csv export issue
          if(!is.numeric(df[[currVar]])){
            df <- df %>% mutate_at(currvar, funs(recode(., 'None'='0', 'No'='0', 'Yes'='1')))
            df[[currVar]] <- as.numeric(df[[currVar]])
          }
          ##End kludge
          
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
    
    if(exists("subsetdata")){
      if(!nrow(subsetdata)==0){
        
        if(adm_level=="province"){
          pivotbyvars <- c(aggs_list, adm_level, 'name')
          pivotbyvars <- pivotbyvars[nzchar(pivotbyvars)]
          outdata <- subsetdata %>% pivot_longer(., varslist) %>% 
            group_by(across(all_of(pivotbyvars))) %>% 
            summarize(value=weighted.mean(value, weight)) %>%
            pivot_wider()
        } else {
          outdata <- subsetdata
        }
        
        pivotbyvars <- c('province', 'name')
        groupbyvars <- c('province', xvars, yvars, "weight", aggs_list)
        groupbyvars <- groupbyvars[nzchar(groupbyvars)]
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
      } 
      
    } else {
      showNotification("Error: Data not found", type="error")
    }
  }
    # Compute correlation p-values
										  

  
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
    aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    aggs_list <- aggs_list[aggs_list!=""]
    all_data <- getData()
    mapdata <- all_data$mapdata
    outdata <- all_data$tempdata #Note to go back and fix the naming here.
    xvars = input$corrsIn
    yvars = input$indicsIn
    adm_level <- input$disAgg_admin
    varslist <- c(xvars, yvars)
    bins <- ifelse(adm_level=="province", 6, 30)
    #heatmapdata <- getData()$tempheatmapdata
    #outdata <- heatmapdata %>% select(all_of(c(xvars,yvars)))
    xlab = indicator_list$prettyName[indicator_list$shortName==xvars]
    ylab = indicator_list$prettyName[indicator_list$shortName==yvars]
    if(length(aggs_list)==0){
      indicatorHist <- ggplot(outdata, aes_string(x=yvars))+
        geom_histogram(bins = bins) +
        labs(x=xlab, y="Number of Observations")+
        ggtitle(str_to_title(paste("Histogram of", ylab))) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      corrHist <- ggplot(outdata, aes(x = !!sym(xvars))) +
        geom_histogram(bins = bins) +
        labs(x = xlab, y = "Number of Observations") +
        ggtitle(str_to_title(paste("Histogram of", indicator_list$prettyName[indicator_list$shortName == xvars]))) +
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
        labs(x=indicator_list$prettyName[indicator_list$shortName==xvars], y=indicator_list$prettyName[indicator_list$shortName==yvars], color=aggs_lab)+
        ggtitle(paste("Scatterplot of",str_to_title(ylab), "\n",  "and", str_to_title(xlab ))) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      
    }
    if(maps==T){
    corrMap <- ggplot(mapdata, aes_string(fill = xvars)) +
      geom_sf() +
      ggtitle(str_to_title(paste("Map of", indicator_list$prettyName[indicator_list$shortName == xvars], "by Province"))) +
      labs(fill = "") + 
      theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
    indicatorMap <- ggplot(mapdata, aes_string(fill = yvars)) +
      geom_sf() +
      ggtitle(str_to_title(paste("Map of", indicator_list$prettyName[indicator_list$shortName == yvars], "by Province"))) +
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

shinyApp(ui = ui, server = server)
