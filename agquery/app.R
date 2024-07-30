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
library(ggtext)
library(spatstat)
import::from(spatstat.geom, weighted.median)
lapply(list.files("Scripts", full.names=T), FUN=source)


thematic_shiny(
  font = "auto",
  sequential = colorRampPalette(colors = c("white", "#440154FF"))(12),
  qualitative = c("#440154FF",  "#21908CFF", "#3B528BFF", "#5DC863FF", "#FDE725FF")
  )
options(shiny.useragg = TRUE)

import::from(spatstat.geom, weighted.median)

#root_dir <- paste0(getwd(), "/")
root_dir <- ""

data <- read.dta13("Data/khm_w1_poultry.dta")
#corr_list <- readxl::read_xlsx(paste0(root_dir, "Update/corr_list.xlsx"))

#Update INDICATOR list to change which indicators are available
indicator_list <- readxl::read_xlsx(paste0(root_dir,"Update/indicators.xlsx"))
#indicator_cats <- unique(indicator_list$indicatorCategory)
#indicators_sub <- indicator_list[indicator_list$shortName %in% unique(corr_list$indicatorSN),]
indics <- indicator_list$shortName
names(indics) <- indicator_list$labelName
names <- indicator_list$labelName

#Update GROUP LIST to modify the grouping variables.
groups_list <- readxl::read_xlsx(paste0(root_dir, "Update/grouping_vars.xlsx"))
group_cats <- unique(groups_list$level)
group_labs <- unique(groups_list$level_lab)
group_cats <- group_cats[group_cats!="Hidden" & group_cats!="Within Household"] #Filter out categories that we want to include but not have boxes for (because, for crops e.g. we have boxes elsewhere)

#Update this file to change filters
filters_list <- readxl::read_xlsx(paste0(root_dir, "Update/filterset.xlsx"))

#Admin levels, mainly for tagging datasets after manipulation
adm_list <- readxl::read_xlsx(paste0(root_dir, "Update/adm_levels.xlsx"))

khm_shp <- st_read(paste0(root_dir, "Spatial/cam_prov_merge.shp"))

policy_path <- read.csv(paste0(root_dir,"Update/Policy_Pathways.csv"), header = TRUE)
pathways <- readxl::read_xlsx(paste0(root_dir,"Update/Policy_Pathways.xlsx"))
pathways <- pathways[-c(10:13)]
colnames(pathways)[8] <- "Related Indicator(s)"

ui <- navbarPage(title=HTML("<b>50x30 Cambodia Data Explorer</b>"), theme = bslib::bs_theme(version="3",
  bg = "white", fg = "#3B528BFF", info="#474481", primary = "#440154FF",
  base_font = bslib::font_google("Open Sans")), 
                tabPanel("Introduction", column(2),column(8,
                        wellPanel(
                        "The 50x30 Cambodia Data Explorer is a tool to inform policy-making in conjunction with other resources. Reviewing academic and grey literature for policy pathways can generate ideas for effective policies and programs that can help shift key indicators of agricultural development. Utilizing other tools and data from additional sources (e.g. FAO) can provide additional context for policy-making."
                        ),
                        img(src='logic-model.png', width=750),
                        hr(),
                        HTML('<p class="c3"><span class="c0 c6">The 50x30 agricultural survey contains household-level information that is potentially valuable 
                        to policy-makers. Each indicator contains data on the agricultural practices of sampled Cambodian households. Click the "Download Indicators" 
                        button below to view the names of indicators present in this tool. Click the "Download Policy Pathways" button to view a preliminary review of
                        literature that may be relevant for using this tool for policy-making.</span></p> 
                 '),
                        fluidRow(column(2), column(10, downloadButton('downloadExcel',
                                                                       label='Download Indicators',
                                                                       icon=icon('file-excel')),
                                                        downloadButton('downloadPathways',
                                                  label='Download Policy Pathways',
                                                 icon=icon('file-csv'))
                        )),
                        br(),
                        br(),
                        HTML('<p class="c5"><span class="c6 c0">
The raw data for this project can be located at </span>
<a class="c1" href="https://nada.nis.gov.kh/index.php/catalog/36">https://nada.nis.gov.kh/index.php/catalog/36</a>.</span></p>
<p class="c3"><span class="c0 c6">Our processed data can be downloaded using the button below.</span></p>'),
                        downloadButton('downloadRaw',
                                        label="Download Processed Data",
                                        icon=icon('file-csv')),
                        hr(),
                        HTML('<p class="c5"><span class="c6 c0">
See Policy Pathways below: </span></p>'),
                        fluidRow(column(12, dataTableOutput("path_table"))),
                        br()
                )
                ),
                tabPanel("Instructions",
                         includeHTML('www/Instructions_50x30_D2.html')),
                tabPanel("Data",
                fluidRow(column(1), column(11,radioButtons('policySelect', 'Policy Goal', choices=c('Poultry Consumption', 'Beef Consumption')))),
                fluidRow(column(6, wellPanel(style="background-color: #ededed; border-color: #9c9c9c;",
                    #fluidRow(column(8, pickerInput("indicsIn", HTML("<b>Select Indicator</b>"), choices=indics, options=list(style="btn-info"))), column(4, uiOutput("ttip"))),
                    
                    fluidRow(column(6, selectInput('indicsIn', HTML("<b>Select Indicator</b>"), choices=indics, size=length(indics), selectize=F)), 
                             #column(6, pickerInput('corrsIn', HTML('<b>Select Correlate</b>'), choices=indics, options=list(style='btn-info', size=length(indics))))),
                             column(6, selectInput('corrsIn', HTML('<b>Select Correlate</b>'), choices=indics, size=length(indics), selectize=F))),
                    uiOutput("indicDesc"),
                    #uiOutput("corrChk"),
                    checkboxInput('yChk', 'Omit 0s from Indicator'),
                    radioButtons("disAgg_admin", HTML("<b>Select Administrative Level</b>"), choiceNames=c("Province","Household"), choiceValues=c("province", "hhid")),
                    uiOutput("groupsChk"),
                    actionButton('submitBtn', "Generate Plots"))),
                column(6, 
                plotOutput('corrPlot'), plotlyOutput('heatMap'))),
                br(),
                br(),
  fluidRow(column(6, uiOutput('indicHeader')) ,column(6, uiOutput('corrHeader'))),
  fluidRow(column(6, plotOutput('indicatorHist')), column(6, plotOutput('corrHist'))),
  fluidRow(column(6, plotOutput('indicatorMap')), column(6, plotOutput('corrMap'))),
  fluidRow(plotOutput('scatterPlot')),
  fluidRow(uiOutput('plotInterp'))
               
  )
)


<<<<<<< Updated upstream
server <- function(input, output, session) {
  output$groupsChk <- renderUI({
    groupCheck <- lapply(1:length(group_cats), function(x){
      groupnames <- groups_list[which(groups_list$level %in% group_cats[[x]]),]
      radioButtons(group_cats[[x]], label=HTML("<b>Select Grouping Variable</b>"), choiceNames=c("None",groupnames$label), choiceValues=c("",groupnames$varName))
    })
  })
=======

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
    return(heatMap)
  }
>>>>>>> Stashed changes
  
  
  
 observeEvent(input$indicsIn, {
    #corrvals <- corr_list$corrSN[corr_list$indicatorSN %in% input$indicsIn] %>% unique()
    #corrs_in <- indicator_list[indicator_list$shortName %in% corrvals,]
    #corrs_list <- as.list(corrs_in$shortName)
    #names(corrs_list) <- corrs_in$prettyName
    #output$corrChk <- renderUI(pickerInput("corrsIn", HTML("<b>Choose Correlate</b>"), choices=corrs_list, options=list(style="btn-info"), size=(length(corrs_list)+1)))

    #output$ttip <- renderUI(popify(bsButton("ttipSurvey", label=HTML("Source<br>question"), size = "medium", block=F),
    #                                            indicator_list$survey_question[indicator_list$shortName==input$indicsIn], indicator_list$ques_text[indicator_list$shortName==input$indicsIn],
    #                               placement="right", options = list(container = "body")))

    })
  
  observeEvent(input$submitBtn, {
    updatePlots(maps=T)
    output$indicHeader <- renderUI(HTML(sprintf('<div style="border: 1px solid #ddd; padding: 9px; margin-bottom: 0px; line-height: 1.2; text-align: center; border-radius: 3px;"> %s </div>'
                                                , indicator_list$prettyName[indicator_list$shortName==input$indicsIn])))
    output$corrHeader <- renderUI(HTML(sprintf('<div style="border: 1px solid #ddd; padding: 9px; margin-bottom: 0px; line-height: 1.2; text-align: center; border-radius: 3px;"> %s </div>'
                                               , indicator_list$prettyName[indicator_list$shortName==input$corrsIn])))
  })
  
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
  
  observeEvent(input$policySelect, {
    #heatmapdata <- getData()$tempheatmapdata
    #tempdata <- getData()$tempdata
    df <- subset(data, select =all_of(indicator_list$shortName)) %>% na.omit() #ALT KLUDGE
    varnames <- colnames(df)
    matched_indices <- match(varnames, indicator_list$shortName)
    label_names <- indicator_list$labelName[matched_indices]
    #truncated_pretty_names <- substr(pretty_names, 1, 35)
    #print(pretty_names)
    cor_matrix <- cor(df)
    par(mar = c(5, 5, 4, 2) - 2)
    
    #corrPlot <- corrplot.mixed(cor_matrix, order = 'AOE')
    output$corrPlot <- renderPlot(corrplot(cor_matrix, order = 'AOE',col=colorRampPalette(c("white","lightblue","red"))(100)))
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
    print(p_matrix)
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
    heatMap <- heatmaply_cor(cor_matrix, 
                             node_type = "scatter", 
                             point_size_mat = -log10(p_matrix), 
                             point_size_name = "-log10(p-value)",
                             label_names=c("Row", "Column", "Correlation"), 
                             custom_hovertext = hover_text,
                             Rowv=NULL,
                             Colv=NULL,
                             dendogram = c("none", "none"),
                             show_dendrogram=F) %>% 
      layout(title = "Correlation Heatmap", margin = list(t = 60), height=600)
    output$heatMap <- renderPlotly(heatMap)
    #output$corrPlot <- renderPlot(corrPlot)
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
  
	corrvals <- reactive({
    req(input$indicsIn) # Ensure that indicsIn is not NULL
    corr_list$corrSN[corr_list$indicatorSN %in% input$indicsIn] %>% unique()
  })		
	
  getData <- function(){
    adm_level_in <- input$disAgg_admin
    aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    #ALT: kludge, to find permanent fix later
    if(is.null(aggs_list)){aggs_list <- ""}
    xvars = input$corrsIn
	xvars_all = as.vector(corrvals())
    
	
	yvars = input$indicsIn
    adm_level <- input$disAgg_admin
    varslist <- c(xvars, yvars)
	varslist_all <- c(xvars_all,yvars)								  
    #if (length(input$corrsIn) == 0) {
    #  showNotification("No correlates were selected", type = "warning")
    #  return()  
    #} else 
	if(aggs_list!=""){ 
	 #tempdata <- data %>% select(all_of(c(adm_level_in, xvars, yvars, "weight", aggs_list))) %>% na.omit()
      subsetdata <- data %>% select(all_of(c(adm_level_in, xvars, yvars, "weight", aggs_list))) %>% na.omit()
    } else {
      #tempdata <- data %>% select(all_of(c(adm_level_in, xvars, yvars, "weight"))) %>% na.omit()
      subsetdata <- data %>% select(all_of(c(adm_level_in, xvars, yvars, "weight"))) %>% na.omit() # JM: If you use corrvals instead of xvars_all, the app crashes 
    }
    if(input$yChk){
          #tempdata <- tempdata[tempdata[[yvars]]!=0,]
      subsetdata <- subsetdata[subsetdata[[yvars]]!=0,]
    }
    for(currVar in varslist) {
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
        lim <- quantile(subsetdata[[currVar]],probs=c(l_wins_threshold, u_wins_threshold), na.rm=T)
        subsetdata[[currVar]][subsetdata[[currVar]] < lim[1]] <- lim[1] 
        subsetdata[[currVar]][subsetdata[[currVar]] > lim[2]] <- lim[2] 
      }
    }
    
    if (any(aggs_list %in% "livestock_area")) {
      #tempdata$livestock_area <- cut(tempdata$livestock_area, c(-1, 0, 0.01, 0.05, 0.1, max(tempdata$livestock_area)* 1.1), c("0 ha", "<=0.01 ha", ">0.01 - 0.05 ha", ">0.05 - 0.1 ha", ">0.1 - 0.25 ha"))
      subsetdata$livestock_area <- cut(subsetdata$livestock_area, c(-1, 0, 0.01, 0.05, 0.1, max(subsetdata$livestock_area)* 1.1), c("0 ha", "<=0.01 ha", ">0.01 - 0.05 ha", ">0.05 - 0.1 ha", ">0.1 - 0.25 ha"))																																																	
    }
    
	if(adm_level_in=="province"){
	  pivotbyvars <- c(aggs_list, adm_level_in, 'name')
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
	  pivot_longer(., varslist) %>% 
	  group_by(across(all_of(pivotbyvars))) %>% 
	  summarize(value=weighted.mean(value, weight)) %>%
	  pivot_wider()
	names(mapdata)[names(mapdata)=="value"] <- currVar
	mapdata$province_num <- as.numeric(mapdata$province)
	xShp <- merge(khm_shp, mapdata, by.x="province", by.y="province_num")
	return(list(tempdata=outdata, mapdata=xShp))
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
                                       rownames = FALSE,
                                       )
  
  updatePlots <- function(maps=T){
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
        labs(x=indicator_list$longName[indicator_list$shortName==yvars], y="Number of Observations")+
        ggtitle(str_to_title(paste("Histogram of", ylab))) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      corrHist <- ggplot(outdata, aes(x = !!sym(xvars))) +
        geom_histogram(bins = bins) +
        labs(x = indicator_list$longName[indicator_list$shortName == xvars], y = "Number of Observations") +
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
        labs(x=indicator_list$longName[indicator_list$shortName==xvars], y="Number of Observations", fill=aggs_lab)+
        ggtitle(str_to_title(paste("Histogram of", xlab)))  +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
              
      indicatorHist <- ggplot(outdata, aes_string(x=yvars, group=aggs_list, fill=aggs_list))+
        geom_histogram(bins = bins)+
        #geom_density(fill=NA)+scale_color_discrete(guide='none')+
        labs(x=indicator_list$longName[indicator_list$shortName==yvars], y="Number of Observations", fill=aggs_lab)+
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
