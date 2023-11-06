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
#install.packages("spatstat")
# renv::deactivate()

import::from(spatstat.geom, weighted.median)

theme_set(theme_cowplot()) #Graphing theme. 

`%!in%` <- Negate(`%in%`) #I.e., return all unmatched from expression x %in% y; why this is not a core function of R is a real humdinger.

#root_dir <- paste0(getwd(), "/")
root_dir <- ""

data <- read.dta13("Data/nga_w4_hh_vars.dta")
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


ui <- fluidPage(
  uiOutput("chartOut"),
  DTOutput("regResult"),
  hr(),
  fluidRow(column(4, selectInput("indicsIn", "Select Indicator", choices=indics))),
  fluidRow(column(4, uiOutput("corrChk")),
           column(4, radioButtons("disAgg_admin", "Administrative Level", choiceNames=c("Zone","State","LGA","EA","Household"), choiceValues=c("zone", "state", "lga", "ea", "hhid")),
                  checkboxInput('yChk', 'Omit 0s from Indicator'),
                  checkboxInput('xChk', 'Omit 0s from Correlate(s)')),
           column(3, uiOutput("groupsChk")),
           column(1, actionButton("submit", "Go"))
  )
)

server <- function(input, output, session) {
  
  
  output$groupsChk <- renderUI({
    groupCheck <- lapply(1:length(group_cats), function(x){
      groupnames <- groups_list[which(groups_list$level %in% group_cats[[x]]),]
      checkboxGroupInput(group_cats[[x]], label=HTML(group_labs[[x]]), choiceNames=groupnames$shortName, choiceValues=groupnames$varName)
    })
  })
  
  observeEvent(input$indicsIn, {
    corrvals <- corr_list$corrSN[corr_list$indicatorSN %in% input$indicsIn] %>% unique()
    corrs_in <- indicator_list[indicator_list$shortName %in% corrvals,]
    output$corrChk <- renderUI(checkboxGroupInput("corrsIn", "Correlates", choiceNames=corrs_in$prettyName, choiceValues=corrs_in$shortName))
  })
  
  observeEvent(input$submit, {
    adm_level_in <- input$disAgg_admin
    aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
    xvars = input$corrsIn
    yvars = input$indicsIn
    adm_level <- input$disAgg_admin
    varslist <- c(xvars, yvars)
    tempdata <- data %>% select(all_of(c(adm_level_in, xvars, yvars, "weight", aggs_list))) %>% na.omit()
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
        lim <- quantile(tempdata[[currVar]],probs=c(l_wins_threshold, u_wins_threshold), na.rm=T) #Note to address NAs here, introduced by indicator_weight being NA.
        tempdata[[currVar]][tempdata[[currVar]] < lim[1]] <- lim[1] # HS: I don't get this line ALT: This selects every observation in the variable column that's smaller than the lower winsorization threshold and sets it to the lower winsorization threshold.
        tempdata[[currVar]][tempdata[[currVar]] > lim[2]] <- lim[2] # HS: I don't get this line ALT: As above, but for the upper winsorization threshold. There's probably a better way to notate this using dplyr
      }
    }
    
    if(length(aggs_list)>0){
      #ALT: Temporary fix til we correct farm size in the compilation code
      if (any(aggs_list %in% "farmsize")) {
        tempdata$farmsize <- sapply(tempdata$farmsize, switch, 
                                    "0 ha"=0, 
                                    ">0 - 2 ha"=1,
                                    "2 - 4 ha"=2,
                                    ">4 ha"=3)
      }
    }
    
    if(adm_level_in!="hhid"){
      tempdata <- tempdata %>% pivot_longer(., varslist) %>% 
        group_by(across(all_of(c(aggs_list, adm_level_in, 'name')))) %>% 
        summarize(value=weighted.mean(value, weight)) %>%
        pivot_wider()
    }
    
    
    layout <- rbind(c(1,1,2,2),
                    c(1,1,2,2),
                    c(3,3,3,3),
                    c(3,3,3,3))
    
    #outTable <- 
    
    regform <- paste(yvars, "~",paste(xvars, collapse="+"))
    if(length("aggs_list")>0) {
      regform <- paste(regform, "+", paste(aggs_list, collapse="+"))
    } 
    regres <- lm(formula(regform), data=tempdata)
    regresOut <- data.frame(summary(regres)$coefficients)
    regresOut$Names <- row.names(regresOut)
    regresOut <- regresOut %>% relocate(Names, .before = everything()) %>% 
      select(Names, Estimate, `Std..Error`, Pr...t..) %>% 
      rename(`Cond. Coefficient` = Estimate, SE = `Std..Error`, `P Value` = Pr...t..) %>% 
      mutate(`Cond. Coefficient` = signif(`Cond. Coefficient`, 3), SE = signif(SE, 3), `P Value` = signif(`P Value`, 2), 
             Effect = ifelse(`P Value` <= 0.01, "Highly Significant", 
                             ifelse(`P Value` <= 0.05, "Significant", ifelse(`P Value` <= 0.2, "Weakly Associated", "Not Significant"))))
    output$regResult <- renderDataTable(datatable(regresOut) %>% formatStyle('Effect', color=styleEqual(c("Highly Significant", "Significant", "Weakly Associated", "Not Significant"), c('#44ce1b', '#bbdb44', '#f2a134', '#e51f1f'))))         

    output$chartOut <- renderUI({
      varCharts <- lapply(1:length(xvars), function(x){
        tempdata_out <- tempdata
        if(input$xChk){
          tempdata_out <- tempdata_out[tempdata_out[[xvars[[x]]]]!=0,]
        }
        
        if(input$yChk){
          tempdata_out <- tempdata_out[tempdata_out[[yvars[[1]]]]!=0,]
        }
        
        
        if(max(tempdata_out[[xvars[[x]]]]) - min(tempdata[[xvars[[x]]]]) > 10000){
          tempdata[[xvars[[x]]]] <- log(tempdata[[xvars[[x]]]]+1)
        }
        if(max(tempdata[[yvars[[1]]]]) - min(tempdata[[yvars[[1]]]]) > 10000){
          tempdata[[yvars[[1]]]] <- log(tempdata[[yvars[[1]]]]+1)
        }
        if(length(aggs_list)==0){
          
          plot2 <- ggplot(tempdata, ensym(x=xvars[[x]]))+
            geom_histogram(aes(y=after_stat(density)))+
            labs(x="", y="")+
            geom_density(fill=NA)+
            ggtitle(paste("Density plot of", indicator_list$prettyName[indicator_list$shortName==xvars[[x]]]))
          plot1 <- ggplot(tempdata, ensym(x=yvars[[1]]))+
            geom_histogram(aes(y=after_stat(density)))+
            labs(x="", y="")+
            geom_density(fill=NA)+
            ggtitle(paste("Density plot of", indicator_list$prettyName[indicator_list$shortName==yvars[[1]]]))
          plot3 <- ggplot(tempdata, aes_string(x=xvars[[x]], y=yvars[[1]], group=aggs_list, color=aggs_list))+ #only one yvar for now
            geom_point()+
            theme_minimal(base_size=14)+
            stat_smooth(method="lm")+
            labs(x="", y=indicator_list$prettyName[indicator_list$shortName==yvars[[1]]])+
            theme(axis.text.x=element_blank())
        } else {
          plot2 <- ggplot(tempdata, aes_string(x=xvars[[x]], group=aggs_list, fill=aggs_list))+
            geom_histogram(aes(y=..density..))+
            geom_density(fill=NA)+
            labs(x="", y="")+
            ggtitle(paste("Density plot of", indicator_list$prettyName[indicator_list$shortName==xvars[[x]]]))
          plot1 <- ggplot(tempdata, aes_string(x=yvars[[1]], group=aggs_list, fill=aggs_list))+
            geom_histogram(aes(y=..density..))+
            geom_density(fill=NA)+
            labs(x="", y="")+
            ggtitle(paste("Density plot of", indicator_list$prettyName[indicator_list$shortName==yvars[[1]]]))
          plot3 <- ggplot(tempdata, aes_string(x=xvars[[x]], y=yvars[[1]], group=aggs_list, color=aggs_list))+ #only one yvar for now
            geom_point()+
            theme_minimal(base_size=14)+
            stat_smooth(method="lm")+
            labs(x="", y=indicator_list$prettyName[indicator_list$shortName==yvars[[1]]])+
            theme(axis.text.x=element_blank())
        }
        #chartLayout <- arrangeGrob(plot1, plot2,plot3, layout_matrix=layout)
        #chartLayout <- arrangeGrob(grobs=c(ggplotly(plot1),plot2,plot3), layout_matrix=layout)
        
        res <- eval(parse_expr(sprintf("with(tempdata, cor.test(%s, %s))", xvars[[x]], yvars[[1]])))
        
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
      
       
        
        tabPanel(title=indicator_list$prettyName[indicator_list$shortName==xvars[[x]]], 
                 fluidRow(column(6, renderPlot(plot1), column(6, renderPlot(plot2)))),
                 #fluidRow(renderTable(outTable)),
                  fluidRow(renderPlotly(ggplotly(plot3))),
                          fluidRow(HTML(sprintf("There is %s%% (%s%% - %s%%) correlation between <font color='#3562ab'><b>%s</b></font> and <font color='#3562ab'><b>%s</b></font>. There is %s confidence in this result.", 
                                                round(res$estimate[[1]]*100, 1), round(res$conf.int[[1]]*100, 1), round(res$conf.int[[2]]*100, 1),
                                                indicator_list$prettyName[indicator_list$shortName==xvars[[x]]], indicator_list$prettyName[indicator_list$shortName==yvars[[1]]], adj)))
                 )
        
      })
      do.call(tabsetPanel, varCharts) %>% return()
      
       
    })  
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
