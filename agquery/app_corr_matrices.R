options(shiny.error=browser) #For debugging 
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
library(heatmaply)
library(bslib)
library(thematic)
library(ragg)
library(viridis)

thematic_shiny(
  font = "auto",
  sequential = colorRampPalette(colors = c("white", "#440154FF"))(12),
  qualitative = c("#440154FF",  "#21908CFF", "#3B528BFF", "#5DC863FF", "#FDE725FF")
)
options(shiny.useragg = TRUE)

import::from(spatstat.geom, weighted.median)

#root_dir <- paste0(getwd(), "/")
root_dir <- ""

#Raw data and list of indicators and their correlates 
data <- read.dta13("Data/khm_w1_poultry.dta")
corr_list <- readxl::read_xlsx(paste0(root_dir, "Update/corr_list.xlsx")) # List of indicators and their correlates 

#Update INDICATOR list to change which indicators are available
indicator_list <- readxl::read_xlsx(paste0(root_dir,"Update/indicators.xlsx"))
indicator_cats <- unique(indicator_list$indicatorCategory)
indicators_sub <- indicator_list[indicator_list$shortName %in% unique(corr_list$indicatorSN),]
indics <- indicators_sub$shortName
names(indics) <- indicators_sub$prettyName
names <- indicators_sub$prettyName

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


ui <- navbarPage(title="50x30 Cambodia Data Explorer", theme = bslib::bs_theme(
  bg = "white", fg = "#3B528BFF", primary = "#440154FF",
  base_font = bslib::font_google("Open Sans")), 
  #tabPanel("Introduction"),
  #tabPanel("Instructions"),
  tabPanel("Data",
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(8, pickerInput("indicsIn", HTML("<b>Select Indicator</b>"), choices=indics, options=list(style="btn-secondary"))), column(4, uiOutput("ttip"))),
               uiOutput("indicDesc"),
               uiOutput("corrChk"),
               checkboxInput('yChk', 'Omit 0s from Indicator'),
               radioButtons("disAgg_admin", HTML("<b>Select Administrative Level</b>"), choiceNames=c("Province","Household"), choiceValues=c("province", "hhid")),
               uiOutput("groupsChk")
             ),
             mainPanel(
               #fluidRow(column(6, uiOutput('indicHeader')) ,column(6, uiOutput('corrHeader'))),
               #fluidRow(column(6, plotOutput('indicatorHist')), column(6, plotOutput('corrHist'))),
               #fluidRow(column(6, plotOutput('indicatorMap')), column(6, plotOutput('corrMap'))),
               #fluidRow(plotOutput('scatterPlot')),
               #fluidRow(uiOutput('plotInterp')),
               fluidRow(plotOutput('corrPlot')),
               fluidRow(plotlyOutput('heatMap'))
             )
           )))


server <- function(input, output, session) {
  output$groupsChk <- renderUI({
    groupCheck <- lapply(1:length(group_cats), function(x){
      groupnames <- groups_list[which(groups_list$level %in% group_cats[[x]]),]
      radioButtons(group_cats[[x]], label=HTML("<b>Select Grouping Variable</b>"), choiceNames=c("None",groupnames$shortName), choiceValues=c("",groupnames$varName))
    })
  })
  
  observeEvent(input$indicsIn, {
    corrvals <- corr_list$corrSN[corr_list$indicatorSN %in% input$indicsIn] %>% unique()
    print(input$indicsIn)
    print(corrvals)
    corrs_in <- indicator_list[indicator_list$shortName %in% corrvals,]
    corrs_list <- as.list(corrs_in$shortName)
    names(corrs_list) <- corrs_in$prettyName
    output$corrChk <- renderUI(selectizeInput("corrsIn", HTML("<b>Choose Correlate</b>"), choices=corrs_list))
    output$indicHeader <- renderUI(HTML(sprintf('<div style="border: 1px solid #ddd; padding: 9px; margin-bottom: 0px; line-height: 1.2; text-align: center; border-radius: 3px;"> %s </div>'
                                                , indicator_list$prettyName[indicator_list$shortName==input$indicsIn])))
    output$corrHeader <- renderUI(HTML(sprintf('<div style="border: 1px solid #ddd; padding: 9px; margin-bottom: 0px; line-height: 1.2; text-align: center; border-radius: 3px;"> %s </div>'
                                               , indicator_list$prettyName[indicator_list$shortName==input$corrsIn])))
    output$ttip <- renderUI(popify(bsButton("ttipSurvey", label=HTML("Source<br>question"), size = "medium", block=F),
                                   indicator_list$survey_question[indicator_list$shortName==input$indicsIn], indicator_list$ques_text[indicator_list$shortName==input$indicsIn],
                                   placement="right", options = list(container = "body")))
    
  })
  
  observeEvent(input$yChk, {
    updatePlots()
  }, ignoreInit=T)
  
  observeEvent(input$disAgg_admin, {
    updatePlots(maps=F)
  }, ignoreInit=T)
  
  observeEvent(input$Household, { #Hard coding input$Household even though the name affected by the spreadsheet. Might want to adjust how the spreadsheet gets handled in the future.
    updatePlots(maps=F)
  }, ignoreInit=T)
  observeEvent(input$corrsIn, {
    updatePlots()
  }, ignoreInit=T)
  
  
  corrvals <- reactive({
    req(input$indicsIn) # Ensure that indicsIn is not NULL
    corr_list$corrSN[corr_list$indicatorSN %in% input$indicsIn] %>% unique()
  })
  
  
  getData <- function(){
    adm_level_in <- input$disAgg_admin
    aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    xvars = input$corrsIn
    xvars_all = as.vector(corrvals())
    print(input$corrsIn)
    print(xvars_all)
    yvars = input$indicsIn
    adm_level <- input$disAgg_admin
    varslist <- c(xvars, yvars)
    varslist_all <- c(xvars_all,yvars)
    print(yvars)
    print(varslist)
    print(varslist_all)
    if (length(input$corrsIn) == 0) {
      showNotification("No correlates were selected", type = "warning")
      return()  
    } else if(aggs_list!=""){ 
      #tempdata <- data %>% select(all_of(c(adm_level_in, xvars, yvars, "weight", aggs_list))) %>% na.omit()
      subsetdata <- data %>% select(all_of(c(adm_level_in, xvars_all, yvars, "weight", aggs_list))) %>% na.omit()
    } else {
      #tempdata <- data %>% select(all_of(c(adm_level_in, xvars, yvars, "weight"))) %>% na.omit()
      subsetdata <- data %>% select(all_of(c(adm_level_in, xvars_all, yvars, "weight"))) %>% na.omit() # JM: If you use corrvals instead of xvars_all, the app crashes 
    }
    if(input$yChk){
      #tempdata <- tempdata[tempdata[[yvars]]!=0,]
      subsetdata <- subsetdata[subsetdata[[yvars]]!=0,]
    }
    for(currVar in varslist_all) {
      print(currVar)
      var_unit <- subset(indicator_list, shortName %in% currVar)$units[[1]] #Should only be 1 list item
      print(var_unit)
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
      subsetdata$livestock_area <- cut(subset$livestock_area, c(-1, 0, 0.01, 0.05, 0.1, max(subsetdata$livestock_area)* 1.1), c("0 ha", "<=0.01 ha", ">0.01 - 0.05 ha", ">0.05 - 0.1 ha", ">0.1 - 0.25 ha"))
    }
    if(adm_level_in=="province"){
      pivotbyvars <- c(aggs_list, adm_level_in, 'name')
      pivotbyvars <- pivotbyvars[nzchar(pivotbyvars)]
      outdata <- subsetdata %>% pivot_longer(., varslist) %>% 
        group_by(across(all_of(pivotbyvars))) %>% 
        summarize(value=weighted.mean(value, weight)) %>%
        pivot_wider()
      #outsubsetdata <- subsetdata %>% pivot_longer(., varslist) %>% 
      #  group_by(across(all_of(pivotbyvars))) %>% 
      #  summarize(value=weighted.mean(value, weight)) %>%
      #  pivot_wider()      
    } else {
      #outdata <- tempdata
      outdata <- subsetdata
    }
    pivotbyvars <- c('province', 'name')
    groupbyvars <- c('province', xvars, yvars, "weight", aggs_list)
    groupbyvars <- groupbyvars[nzchar(groupbyvars)]
    return(list(tempheatmapvars = xvars_all, tempheatmapdata=subsetdata))
  }
  
  # Compute correlation p-values
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
  
  updatePlots <- function(maps=T){
    aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    aggs_list <- aggs_list[aggs_list!=""]
    xvars = input$corrsIn
    yvars = input$indicsIn
    adm_level <- input$disAgg_admin
    varslist <- c(xvars, yvars)
    bins <- ifelse(adm_level=="province", 6, 30)
    heatmapdata <- getData()$tempheatmapdata
    outdata <- heatmapdata %>% select(all_of(c(xvars,yvars)))
    #xlab = indicator_list$prettyName[indicator_list$shortName==xvars]
    #ylab = indicator_list$prettyName[indicator_list$shortName==yvars]
    if(length(aggs_list)==0){

      df <- subset(heatmapdata, select = -c(province, weight) )
      varnames <- colnames(df)
      matched_indices <- match(varnames, indicator_list$shortName)
      pretty_names <- indicator_list$prettyName[matched_indices]
      #truncated_pretty_names <- substr(pretty_names, 1, 35)
      print(pretty_names)
      cor_matrix <- cor(df)
      par(mar = c(5, 5, 4, 2) - 2)
      
      #corrPlot <- corrplot.mixed(cor_matrix, order = 'AOE')
      output$corrPlot <- renderPlot(corrplot(cor_matrix, order = 'AOE',col=colorRampPalette(c("white","lightblue","red"))(100)))
      #print(corrPlot) 
      
      rownames(cor_matrix) <- pretty_names
      colnames(cor_matrix) <- pretty_names
      print(cor_matrix)
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
      print(hover_text)
      heatMap <- heatmaply_cor(cor_matrix, 
                               node_type = "scatter", 
                               point_size_mat = -log10(p_matrix), 
                               point_size_name = "-log10(p-value)",
                               label_names=c("Row", "Column", "Correlation"), 
                               custom_hovertext = hover_text,
                               row_v=NULL,
                               col_v=row_v,
                               show_dendogram = c(FALSE, FALSE)) %>% 
        layout(title = "Correlation Heatmap", margin = list(t = 60), height=600)
      print(heatMap) 
    } else {
      
    }
    if(maps==T){
      
    }
    #res <- eval(parse_expr(sprintf("with(outdata, cor.test(%s, %s))", xvars, yvars)))
    
    #if(res$p.value > 0.2) {
    #  adj = "<font color='#e51f1f'>no</font>"
    #} else if(res$p.value <= 0.2) {
    #  adj="<font color='#f2a134'>low</font>"
    #} else if(res$p.value <= 0.1) {
    #  adj="<font color='#f7e379'>moderate</font>"
    #} else if(res$p.value <= 0.05) {
    #  adj="<font color='#bbdb44'>high</font>"
    #} else if(res$p.value <= 0.01) {
    #  adj="<font color='#44ce1b'>very high</font>"
    #}
    
    #res_out <- sprintf("<div style='font-family: 'Open Sans';'><br><br>There is %s%% (%s%% - %s%%) correlation between <font color='#0a2167'><b>%s</b></font> and <font color='#0a2167'><b>%s</b></font>. There is %s confidence in this result.</div>", 
    #                   round(res$estimate[[1]]*100, 1), round(res$conf.int[[1]]*100, 1), round(res$conf.int[[2]]*100, 1),
    #                   xlab, ylab, adj
    #)
    
    #output$indicatorHist <- renderPlot(indicatorHist)
    #output$corrHist <- renderPlot(corrHist)
    #output$scatterPlot <- renderPlot(corrPlot)
    #output$plotInterp <- renderUI(HTML(res_out))
    output$heatMap <- renderPlotly(heatMap)
    #output$corrPlot <- renderPlot(corrPlot)
    if(maps==T){
      #output$indicatorMap <- renderPlot(indicatorMap)
      #output$corrMap <- renderPlot(corrMap)
    }
  }
}

shinyApp(ui = ui, server = server)
