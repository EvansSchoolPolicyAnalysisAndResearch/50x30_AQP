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
#install.packages("spatstat")
# renv::deactivate()

import::from(spatstat.geom, weighted.median)

theme_set(theme_cowplot()) #Graphing theme. 

`%!in%` <- Negate(`%in%`) #I.e., return all unmatched from expression x %in% y; why this is not a core function of R is a real humdinger.

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


ui <- fluidPage(theme=bs_theme(base_font=font_google("Nunito Sans"),
                               header_font=font_google("Josefin Sans")), 
                titlePanel("50x30 Cambodia Data Explorer"),
                wellPanel(
                  fluidRow(
                  column(4,
                  selectInput("indicsIn", "Select Indicator", choices=indics),
                    uiOutput("indicDesc"),
                  checkboxInput('yChk', 'Omit 0s from Indicator'),
                    radioButtons("disAgg_admin", "Administrative Level", choiceNames=c("Province","Household"), choiceValues=c("province", "hhid")),
                    #uiOutput("groupsChk")
                  ),
                  column(4, fluidRow(uiOutput("groupsChk")), 
                            fluidRow(uiOutput("corrChk")),
                         ),
                  column(3,         #uiOutput("corrInfo")
                                    HTML("Information on correlate variables can go here?")
                                    #checkboxInput('xChk', 'Omit 0s from Correlate(s)')
                                    #popify(bsButton("ttip1",label="",icon=icon("question"),style = "inverse", size = "extra-small", block=F), "Using Winsorization","Winsorization controls extreme values by setting all values greater than the 99th percentile to the value of the 99th percentile. In previous verisions of AgQuery, this option was the default.", trigger="hover",placement="right", options = list(container = "body"))
                                    
                             ),
                             #column(3, uiOutput("corrInfo")),
                  column(1, actionButton("submit", HTML("Compare<br>Correlates"), style="color: #f0ead6; background-color:#4169e1; font-size:18px"))
                )),
  hr(),
  fluidRow(column(4, uiOutput('indicHeader'),
                  plotOutput('indicatorHist'),
                  plotOutput('indicatorMap')),
           column(8, uiOutput("chartOut"))
  )
  #fluidRow(column, 4, uiOutput("yvarOut"),
  #         column, 8, uiOutput("xvarOut"))
  #DTOutput("regResult")
)



server <- function(input, output, session) {
  #bs_themer()
  
  output$groupsChk <- renderUI({
    groupCheck <- lapply(1:length(group_cats), function(x){
      groupnames <- groups_list[which(groups_list$level %in% group_cats[[x]]),]
      radioButtons(group_cats[[x]], label=HTML(group_labs[[x]]), choiceNames=c("None",groupnames$shortName), choiceValues=c("",groupnames$varName))
    })
  })
  
  observeEvent(input$indicsIn, {
    corrvals <- corr_list$corrSN[corr_list$indicatorSN %in% input$indicsIn] %>% unique()
    corrs_in <- indicator_list[indicator_list$shortName %in% corrvals,]
    corrs_items <- lapply(corrs_in$prettyName, FUN=function(x){
      ttip <- popify(bsButton(paste0("ttip_", corrs_in$shortName[which(corrs_in$prettyName==x)]), label="",icon=icon("question"),style = "inverse", size = "extra-small", block=F), "",
                     corr_list$reason[which(corrs_in$prettyName==x)],placement="right", options = list(container = "body"))
      return(c(x, ttip))
    })
    output$corrChk <- renderUI(checkboxGroupInput("corrsIn", "Correlates", choiceNames=corrs_items, choiceValues=corrs_in$shortName))
    output$indicDesc <- renderUI(HTML(sprintf("<p align='center'><table style='background-color:white; border:2px outset; border-color:#e0dbc8'><tr><td style='border:2px outset; border-color:#e0dbc8; padding:15px'><b>Survey Question: </b> %s </td style='border:2px outset; border-color:#e0dbc8; padding:15px'></tr> <tr><td style='border:2px outset; border-color:#e0dbc8; padding:15px'>%s </td></tr></table><br><br></p>", 
                                              indicator_list$survey_question[indicator_list$shortName==input$indicsIn], indicator_list$ques_text[indicator_list$shortName==input$indicsIn])))
    
    
    aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    aggs_list <- aggs_list[aggs_list!=""]
    if(length(aggs_list)==0){
      plot1 <- ggplot(getData()$tempdata, aes(x=!!sym(input$indicsIn)))+
        geom_histogram(fill="red", alpha=0.4)+
        labs(x=indicator_list$`Long Name`[indicator_list$shortName==input$indicsIn], y="Number of Observations")+
        geom_density(aes(y=after_stat(count)), fill=NA)
      
    } else {
      plot1 <- ggplot(getData()$tempdata, aes(x=!!sym(input$indicsIn)))+
        geom_histogram(aes(fill=!!sym(aggs_list)), alpha=0.4)+
        labs(x=indicator_list$`Long Name`[indicator_list$shortName==input$indicsIn], y="Number of Observations")+
        geom_density(aes(y=after_stat(count), group=!!sym(aggs_list), color=!!sym(aggs_list)), fill=NA)
    }
    plot5 <- ggplot(getData()$mapdata, aes_string(fill=input$indicsIn))+
      geom_sf()+
      theme_map()+
      #ggtitle(paste0("Map of ", str_to_title(indicator_list$prettyName[indicator_list$shortName==input$indicsIn]), " by Province")) + 
      labs(fill="")
    output$indicatorHist <- renderPlot(plot1)
    output$indicatorMap <- renderPlot(plot5)
    output$indicHeader <- renderUI(HTML(sprintf("<h3>Data Preview - %s</h3>", input$indicsIn)))
    })
  
  
  observeEvent(input$disAgg_admin, {
    aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    aggs_list <- aggs_list[aggs_list!=""]
    if(length(aggs_list)==0){
    plot1 <- ggplot(getData()$tempdata, aes(x=!!sym(input$indicsIn)))+
      geom_histogram(fill="red", alpha=0.4)+
      labs(x=indicator_list$`Long Name`[indicator_list$shortName==input$indicsIn], y="Number of Observations")+
      geom_density(aes(y=after_stat(count)), fill=NA)

    } else {
      plot1 <- ggplot(getData()$tempdata, aes(x=!!sym(input$indicsIn)))+
        geom_histogram(aes(fill=!!sym(aggs_list)), alpha=0.4)+
        labs(x=indicator_list$`Long Name`[indicator_list$shortName==input$indicsIn], y="Number of Observations")+
        geom_density(aes(y=after_stat(count), group=!!sym(aggs_list), color=!!sym(aggs_list)), fill=NA)
    }
    output$indicatorHist <- renderPlot(plot1)
  })
  
  observeEvent(input$Household, {
    aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    aggs_list <- aggs_list[aggs_list!=""]
    if(length(aggs_list)==0){
      plot1 <- ggplot(getData()$tempdata, aes(x=!!sym(input$indicsIn)))+
        geom_histogram(fill="red", alpha=0.4)+
        labs(x=indicator_list$`Long Name`[indicator_list$shortName==input$indicsIn], y="Number of Observations")+
        geom_density(aes(y=after_stat(count)), fill=NA)
      
    } else {
      plot1 <- ggplot(getData()$tempdata, aes(x=!!sym(input$indicsIn)))+
        geom_histogram(aes(fill=!!sym(aggs_list)), alpha=0.4)+
        labs(x=indicator_list$`Long Name`[indicator_list$shortName==input$indicsIn], y="Number of Observations")+
        geom_density(aes(y=after_stat(count), group=!!sym(aggs_list), color=!!sym(aggs_list)), fill=NA)
    }
    output$indicatorHist <- renderPlot(plot1)
  }) #Hard coding this even though it's affected by the spreadsheet. Might want to adjust how the spreadsheet gets handled in the future.
  
  getData <- function(){
    currVar <-  input$indicsIn
    adm_level_in <- input$disAgg_admin
    aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    aggs_list <- aggs_list[aggs_list!=""]
    if(adm_level_in!="province"){
      tempdata <- data %>% select(all_of(c(adm_level_in, "province", input$indicsIn, "weight", aggs_list))) %>% na.omit()
    } else {
      tempdata <- data %>% select(all_of(c(adm_level_in,  input$indicsIn, "weight", aggs_list))) %>% na.omit()
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
      lim <- quantile(tempdata[[currVar]],probs=c(l_wins_threshold, u_wins_threshold), na.rm=T)
      tempdata[[currVar]][tempdata[[currVar]] < lim[1]] <- lim[1] 
      tempdata[[currVar]][tempdata[[currVar]] > lim[2]] <- lim[2] 
    }
    
    if(length(aggs_list)!=0){
      aggs_lab = groups_list$shortName[groups_list$varName==aggs_list]
      if(!is.factor(tempdata[[aggs_list]])){
        flevels = groups_list[which(groups_list$varName==aggs_list),]$Levels %>% str_split(., ",") %>% unlist()
        flabels = groups_list[which(groups_list$varName==aggs_list),]$Labels %>% str_split(., ",") %>% unlist()
        tempdata[[aggs_list]] <- factor(tempdata[[aggs_list]], levels=flevels, labels=flabels)
      }
    }
    
    #This could get simplified if we end up not going past the hhid/province model (or made more complex once we introduce multiple waves. )
    if(adm_level_in!="hhid") {  
      tempdata_out <- tempdata %>% 
        group_by(!!!syms(c(aggs_list, adm_level_in))) %>% 
        summarize(value=weighted.mean(!!sym(currVar), weight))
      names(tempdata_out)[names(tempdata_out)=="value"] <- currVar #Working around the fact that dplyr doesn't like to assign values to symbols
      #mapdata <- tempdata
      #mapdata$province_num <- as.numeric(mapdata$province)
      #xShp <- merge(khm_shp, mapdata, by.x="province", by.y="province_num")
    } else {
      tempdata_out <- tempdata
    }
      mapdata <- tempdata %>% 
        group_by(province) %>% 
        summarize(value=weighted.mean(!!sym(currVar), weight))
      names(mapdata)[names(mapdata)=="value"] <- currVar
      mapdata$province_num <- as.numeric(mapdata$province)
      xShp <- merge(khm_shp, mapdata, by.x="province", by.y="province_num", all.x=T)
    
    return(list(tempdata=tempdata_out, mapdata=xShp))
  }
  

  
  
  observeEvent(input$submit, {
    adm_level_in <- input$disAgg_admin
    aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    xvars = input$corrsIn
    yvars = input$indicsIn
    adm_level <- input$disAgg_admin
    varslist <- c(xvars, yvars)
    if(aggs_list!=""){
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
    
    
    if(adm_level_in!="hhid"){
      pivotbyvars <- c(aggs_list, adm_level_in, 'name')
      pivotbyvars <- pivotbyvars[nzchar(pivotbyvars)]
      outdata <- tempdata %>% pivot_longer(., varslist) %>% 
        group_by(across(all_of(pivotbyvars))) %>% 
        summarize(value=weighted.mean(value, weight)) %>%
        pivot_wider()
    } else {
      outdata <- tempdata
    }
    
    if(adm_level_in != "province") {
      pivotbyvars <- c(aggs_list, 'province', 'name')
      pivotbyvars <- pivotbyvars[nzchar(pivotbyvars)]
      
      groupbyvars <- c('province', xvars, yvars, "weight", aggs_list)
      groupbyvars <- groupbyvars[nzchar(groupbyvars)]
      mapdata <- data  %>% select(all_of(groupbyvars)) %>% 
        na.omit() %>% 
        pivot_longer(., varslist) %>% 
        group_by(across(all_of(pivotbyvars))) %>% 
        summarize(value=weighted.mean(value, weight)) %>%
        pivot_wider()
    } else {
      mapdata <- outdata
    }
    
    #layout <- rbind(c(1,1,2,2),
    #                c(1,1,2,2),
    #                c(4,4,5,5),
    #                c(4,4,5,5),
    #                c(3,3,3,3),
    #                c(3,3,3,3))
    
    #outTable <- 
    
    #regform <- paste(yvars, "~",paste(xvars, collapse="+"))
    #if(aggs_list!="") {
    #  regform <- paste(regform, "+", paste(aggs_list, collapse="+"))
    #} 
    #regres <- lm(formula(regform), data=tempdata)
    #regresOut <- data.frame(summary(regres)$coefficients)
    #regresOut$Names <- row.names(regresOut)
    #regresOut <- regresOut %>% relocate(Names, .before = everything()) %>% 
    #  select(Names, Estimate, `Std..Error`, Pr...t..) %>% 
    #  rename(`Cond. Coefficient` = Estimate, SE = `Std..Error`, `P Value` = Pr...t..) %>% 
    #  mutate(`Cond. Coefficient` = signif(`Cond. Coefficient`, 3), SE = signif(SE, 3), `P Value` = signif(`P Value`, 2), 
    #         Effect = ifelse(`P Value` <= 0.01, "Highly Significant", 
    #                         ifelse(`P Value` <= 0.05, "Significant", ifelse(`P Value` <= 0.2, "Weakly Associated", "Not Significant"))))
    #output$regResult <- renderDataTable(datatable(regresOut) %>% formatStyle('Effect', color=styleEqual(c("Highly Significant", "Significant", "Weakly Associated", "Not Significant"), c('#44ce1b', '#bbdb44', '#f2a134', '#e51f1f'))))         

    output$chartOut <- renderUI({
      varCharts <- lapply(1:length(xvars), function(x){
        #tempdata_out <- tempdata
        #if(input$xChk){
        #  tempdata_out <- tempdata_out[tempdata_out[[xvars[[x]]]]!=0,]
        #}
        
        #if(input$yChk){
        #  tempdata_out <- tempdata_out[tempdata_out[[yvars[[1]]]]!=0,]
        #}
        
        
        #if(max(tempdata_out[[xvars[[x]]]]) - min(tempdata[[xvars[[x]]]]) > 10000){
        #  tempdata[[xvars[[x]]]] <- log(tempdata[[xvars[[x]]]]+1)
        #}
        #if(max(tempdata[[yvars[[1]]]]) - min(tempdata[[yvars[[1]]]]) > 10000){
        #  tempdata[[yvars[[1]]]] <- log(tempdata[[yvars[[1]]]]+1)
        #}
        xlab = indicator_list$prettyName[indicator_list$shortName==xvars[[x]]]
        ylab = indicator_list$prettyName[indicator_list$shortName==yvars[[1]]]
        if(aggs_list==""){
          plot2 <- ggplot(outdata, aes(x=!!sym(xvars[[x]])))+
            geom_histogram(aes(y=after_stat(density)), fill="red", alpha=0.4)+
            labs(x=indicator_list$`Long Name`[indicator_list$shortName==xvars[[x]]], y="")+
            geom_density(fill=NA)+

            ggtitle(paste("Density plot of", indicator_list$prettyName[indicator_list$shortName==xvars[[x]]]))
          #plot1 relocated up to new section.
          #plot1 <- ggplot(outdata, aes(x=!!sym(yvars)))+
          #  geom_histogram(aes(y=after_stat(density)), fill="red", alpha=0.4)+
          #  labs(x=indicator_list$`Long Name`[indicator_list$shortName==yvars[[1]]], y="Relative Number of Observations")+
          #  geom_density(fill=NA)+
          #  ggtitle(paste("Density plot of", indicator_list$prettyName[indicator_list$shortName==yvars[[1]]]))
          plot3 <- ggplot(outdata, aes(x=!!sym(xvars[[x]]), y=!!sym(yvars)))+ #only one yvar for now
            geom_point()+
            theme_minimal(base_size=14)+
            stat_smooth(method="lm")+
            labs(x=xlab, y=ylab)
        } else {
          aggs_lab = groups_list$shortName[groups_list$varName==aggs_list]
          if(!is.factor(outdata[[aggs_list]])){
          flevels = groups_list[which(groups_list$varName==aggs_list),]$Levels %>% str_split(., ",") %>% unlist()
          flabels = groups_list[which(groups_list$varName==aggs_list),]$Labels %>% str_split(., ",") %>% unlist()
          outdata[[aggs_list]] <- factor(outdata[[aggs_list]], levels=flevels, labels=flabels)
          }
          plot2 <- ggplot(outdata, aes_string(x=xvars[[x]], group=aggs_list, fill=aggs_list, color=aggs_list))+
            geom_histogram(aes(y=after_stat(density)), alpha=0.4)+
            geom_density(fill=NA)+scale_color_discrete(guide='none')+
            labs(x=indicator_list$`Long Name`[indicator_list$shortName==xvars[[x]]], y="", fill=aggs_lab)+
            ggtitle(paste("Density plot of", xlab))
          plot1 <- ggplot(outdata, aes_string(x=yvars[[1]], group=aggs_list, fill=aggs_list, color=aggs_list))+
            geom_histogram(aes(y=after_stat(density)), position="dodge", alpha=0.4)+
            geom_density(fill=NA)+scale_color_discrete(guide='none')+
            labs(x=indicator_list$`Long Name`[indicator_list$shortName==yvars[[1]]], y="Relative Number of Observations", fill=aggs_lab)+
            ggtitle(paste("Density plot of", ylab))
          plot3 <- ggplot(outdata, aes(x=!!sym(xvars[[x]]), y=!!sym(yvars[[1]]), group=!!sym(aggs_list), color=!!sym(aggs_list)))+ #only one yvar for now
            geom_point()+
            theme_minimal(base_size=14)+
            stat_smooth(method="lm")+
            labs(x=indicator_list$prettyName[indicator_list$shortName==xvars[[x]]], y=indicator_list$prettyName[indicator_list$shortName==yvars[[1]]], color=aggs_lab)
        }
        mapdata$province_num <- as.numeric(mapdata$province)
        xShp <- merge(khm_shp, mapdata, by.x="province", by.y="province_num")

        plot4 <- ggplot(xShp, aes_string(fill=xvars[[x]]))+
          geom_sf()+
          theme_map()+
          ggtitle(paste0("Map of ", str_to_title(indicator_list$prettyName[indicator_list$shortName==xvars[[x]]]), " by Province")) + 
          labs(fill="")
        plot5 <- ggplot(xShp, aes_string(fill=yvars[[1]]))+
          geom_sf()+
          theme_map()+
          ggtitle(paste0("Map of ", str_to_title(indicator_list$prettyName[indicator_list$shortName==yvars[[1]]]), " by Province"))+
          labs(fill="")
        #chartLayout <- arrangeGrob(plot1, plot2,plot3, layout_matrix=layout)
        #chartLayout <- arrangeGrob(grobs=c(ggplotly(plot1),plot2,plot3), layout_matrix=layout)
        
        res <- eval(parse_expr(sprintf("with(outdata, cor.test(%s, %s))", xvars[[x]], yvars[[1]])))
        
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
      
       
        
        tabPanel(title=xlab, 
                 #fluidRow(column(6, renderPlot(plot1)), column(6, renderPlot(plot2))),
                 fluidRow(column(8, renderPlot(plot2))
                          ), 
                 fluidRow(column(8, renderPlot(plot4))
                          ),
                 #fluidRow(renderTable(outTable)),
                  fluidRow(column(9, renderPlotly(ggplotly(plot3))
                           ),
                          column(3, HTML(sprintf("<br><br>There is %s%% (%s%% - %s%%) correlation between <font color='#3562ab'><b>%s</b></font> and <font color='#3562ab'><b>%s</b></font>. There is %s confidence in this result.", 
                                                round(res$estimate[[1]]*100, 1), round(res$conf.int[[1]]*100, 1), round(res$conf.int[[2]]*100, 1),
                                                xlab, ylab, adj))
                                   )
                 )
        )
        
      })
      do.call(tabsetPanel, varCharts) %>% return()
      
       
    })  
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
