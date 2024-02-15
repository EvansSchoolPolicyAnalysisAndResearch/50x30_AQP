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

data <- read.dta13("Data/khm_w1_poultry.dta")
corr_list <- readxl::read_xlsx(paste0(root_dir, "Update/corr_list.xlsx"))

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
                tabPanel("Introduction"),
                tabPanel("Instructions"),
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
  fluidRow(column(6, uiOutput('indicHeader')) ,column(6, uiOutput('corrHeader'))),
  fluidRow(column(6, plotOutput('indicatorHist')), column(6, plotOutput('corrHist'))),
  fluidRow(column(6, plotOutput('indicatorMap')), column(6, plotOutput('corrMap'))),
  fluidRow(plotOutput('scatterPlot')),
  fluidRow(uiOutput('plotInterp'))
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
  
  getData <- function(){
    adm_level_in <- input$disAgg_admin
    aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    xvars = input$corrsIn
    yvars = input$indicsIn
    adm_level <- input$disAgg_admin
    varslist <- c(xvars, yvars)
    if (length(input$corrsIn) == 0) {
      showNotification("No correlates were selected", type = "warning")
      return()  
    } else if(aggs_list!=""){ 
      tempdata <- data %>% select(all_of(c(adm_level_in, xvars, yvars, "weight", aggs_list))) %>% na.omit()
    } else {
      tempdata <- data %>% select(all_of(c(adm_level_in, xvars, yvars, "weight"))) %>% na.omit()
    }
    if(input$yChk){
      tempdata <- tempdata[tempdata[[yvars]]!=0,]
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
        lim <- quantile(tempdata[[currVar]],probs=c(l_wins_threshold, u_wins_threshold), na.rm=T)
        tempdata[[currVar]][tempdata[[currVar]] < lim[1]] <- lim[1] 
        tempdata[[currVar]][tempdata[[currVar]] > lim[2]] <- lim[2] 
      }
    }
    
    if (any(aggs_list %in% "livestock_area")) {
      tempdata$livestock_area <- cut(tempdata$livestock_area, c(-1, 0, 0.01, 0.05, 0.1, max(tempdata$livestock_area)* 1.1), c("0 ha", "<=0.01 ha", ">0.01 - 0.05 ha", ">0.05 - 0.1 ha", ">0.1 - 0.25 ha"))
    }
    
    if(adm_level_in=="province"){
      pivotbyvars <- c(aggs_list, adm_level_in, 'name')
      pivotbyvars <- pivotbyvars[nzchar(pivotbyvars)]
      outdata <- tempdata %>% pivot_longer(., varslist) %>% 
        group_by(across(all_of(pivotbyvars))) %>% 
        summarize(value=weighted.mean(value, weight)) %>%
        pivot_wider()
    } else {
      outdata <- tempdata
    }
    
    pivotbyvars <- c('province', 'name')
    groupbyvars <- c('province', xvars, yvars, "weight", aggs_list)
    groupbyvars <- groupbyvars[nzchar(groupbyvars)]
    mapdata <- tempdata  %>% select(all_of(groupbyvars)) %>% 
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
  


  updatePlots <- function(maps=T){
    aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    aggs_list <- aggs_list[aggs_list!=""]
    mapdata <- getData()$mapdata
    outdata <- getData()$tempdata #Note to go back and fix the naming here.
    xvars = input$corrsIn
    yvars = input$indicsIn
    adm_level <- input$disAgg_admin
    varslist <- c(xvars, yvars)
    bins <- ifelse(adm_level=="province", 6, 30)
   
    xlab = indicator_list$prettyName[indicator_list$shortName==xvars]
    ylab = indicator_list$prettyName[indicator_list$shortName==yvars]
    if(length(aggs_list)==0){
      indicatorHist <- ggplot(outdata, aes_string(x=yvars))+
        geom_histogram(bins = bins) +
        labs(x=indicator_list$`Long Name`[indicator_list$shortName==yvars], y="Number of Observations")+
        ggtitle(paste("Histogram of", ylab)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      corrHist <- ggplot(outdata, aes(x = !!sym(xvars))) +
        geom_histogram(bins = bins) +
        labs(x = indicator_list$`Long Name`[indicator_list$shortName == xvars], y = "Number of Observations") +
        ggtitle(paste("Histogram of", indicator_list$prettyName[indicator_list$shortName == xvars])) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
        
      scatterPlot <- ggplot(outdata, aes(x=!!sym(xvars), y=!!sym(yvars))) + #only one yvar for now
        geom_point() +
        stat_smooth(method="lm") +
        labs(x=xlab, y=ylab) +
        ggtitle(paste("Scatterplot of",str_to_title(ylab), "\n",  "and", str_to_title(xlab ))) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
        
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
        labs(x=indicator_list$`Long Name`[indicator_list$shortName==xvars], y="Number of Observations", fill=aggs_lab)+
        ggtitle(paste("Histogram of", xlab))  +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
              
      indicatorHist <- ggplot(outdata, aes_string(x=yvars, group=aggs_list, fill=aggs_list))+
        geom_histogram(bins = bins)+
        #geom_density(fill=NA)+scale_color_discrete(guide='none')+
        labs(x=indicator_list$`Long Name`[indicator_list$shortName==yvars], y="Number of Observations", fill=aggs_lab)+
        ggtitle(paste("Histogram of", ylab)) +
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
      ggtitle(paste("Map of", str_to_title(indicator_list$prettyName[indicator_list$shortName == xvars]), "by Province")) +
      labs(fill = "") + 
      theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
    indicatorMap <- ggplot(mapdata, aes_string(fill = yvars)) +
      geom_sf() +
      ggtitle(paste("Map of", str_to_title(indicator_list$prettyName[indicator_list$shortName == yvars]), "by Province")) +
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
