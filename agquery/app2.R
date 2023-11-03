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
#install.packages("spatstat")
# renv::deactivate()

import::from(spatstat.geom, weighted.median)

theme_set(theme_cowplot()) #Graphing theme. 

`%!in%` <- Negate(`%in%`) #I.e., return all unmatched from expression x %in% y; why this is not a core function of R is a real humdinger.

#root_dir <- paste0(getwd(), "/")
root_dir <- ""

data <- read.dta13("Data/nga_w4_hh_vars.dta")
#Update INSTRUMENT list to change which surveys are available.
instrument_list <- readxl::read_xlsx(paste0(root_dir,"Update/instrument_list.xlsx"))
#instrument_list <- readxl::read_xlsx(paste0(root_dir,"Update/instrument_list_new_03202023.xlsx")) # HS 3/20/23
instrument_list$checkbox_names <- paste0(instrument_list$abbr, instrument_list$year)

#Currency conversion table
ccons <- instrument_list %>% select(abbr, wave, inflation, gdp_ppp, cons_ppp, gdp_ppp_2017, cons_ppp_2017)
ccons$unit <- "currency" #Used for the merge later.

#Update INDICATOR list to change which indicators are available
indicator_list <- readxl::read_xlsx(paste0(root_dir,"Update/indicators.xlsx"))
indicator_cats <- unique(indicator_list$indicatorCategory)

#Update GROUP LIST to modify the grouping variables.
groups_list <- readxl::read_xlsx(paste0(root_dir, "Update/grouping_vars.xlsx"))
group_cats <- unique(groups_list$level)
group_labs <- unique(groups_list$level_lab)
group_cats <- group_cats[group_cats!="Hidden"] #Filter out categories that we want to include but not have boxes for (because, for crops e.g. we have boxes elsewhere)
group_labs <- group_labs[group_labs!="Hiui <- navbarPage(dden"]

#Update this file to change filters
filters_list <- readxl::read_xlsx(paste0(root_dir, "Update/filterset.xlsx"))

#Admin levels, mainly for tagging datasets after manipulation
adm_list <- readxl::read_xlsx(paste0(root_dir, "Update/adm_levels.xlsx"))


ui <- fluidPage(
  uiOutput("chartOut")
  hr()
  fluidRow(column(3,
                  uiOutput("indicChk")),
  column(3, uiOutput("corrChk")),
  column(3, checkboxGroupInput("disAgg_admin", "Administrative Level", choiceNames=c("Zone","State","LGA","EA","Household"), choiceValues=c("zone", "state", "lga", "ea", "hhid")))
  column(2, uiOutput("groupsChk")),
  column(1, actionButton("submit", "Go"))
  )
)

server <- function(input, output, session) {
  output$indicChk <- renderUI({
    checkboxnames <- unique(indicator_list$indicator_name)
    
    
    varCheckboxes <- lapply(1:length(checkboxnames), function(x){
      itemnames <- instrument_list[which(instrument_list$country %in% checkboxnames[[x]]),]
      checkboxGroupInput(checkboxnames[[x]], label=checkboxnames[[x]], choiceNames=with(itemnames, paste(survey, yearlabel)), choiceValues=itemnames$checkbox_names)
    })
  })
  
  output$varGroups <- renderUI({
    groupCheck <- lapply(1:length(group_cats), function(x){
      groupnames <- groups_list[which(groups_list$level %in% group_cats[[x]]),]
      checkboxGroupInput(group_cats[[x]], label=HTML(group_labs[[x]]), choiceNames=groupnames$shortName, choiceValues=groupnames$varName)
    })
  })
  
  observeEvent(input$indicNames, {
    corrvals <- corr_list$corrSN[corr_list$indicatorSN %in% input$indicIn] %>% unique()
    corrnames <- indicator_list$prettyName[indicator_list$prettyName %in% corrvals]
    output$corrChk <- renderUI(checkboxGroupInput, choiceNames=corrnames, choiceValues=corrvals)
  })

  getData <- eventReactive(input$submitData, {
    adm_level_in <- input$disAgg_admin
    
    xvars = 
    yvars =
    ###  GROUPING: ADMINISTRATIVE REGION ###
    adm_level <- input$disAgg_admin #Should be only one element
    
    
    
      ##### OUTPUT DATA #####  
      for(x in 1:length(varlist_sub)) {
        #update 10/20: Updating the code to ensure the correct weights are being used.
        
        currVar <- varlist_sub[[x]] #04.11 Making the rest of this section less unwieldy; I'm open to alternatives
        weightformula <- indicator_list$weight_formula[which(indicator_list$shortName==currVar)]
        tempdata$in_weight <- switch(input$weights,
                                     `Original Weights` = data$weight,
                                     `Total-Population-Adjusted` = data$weight_pop_tot,
                                     `Urban/Rural-Population-Adjusted` = data$weight_pop_rururb,
                                     `No Weight (Sample Means Only)` = 1)
        tempdata$indicator_weight <- with(tempdata, eval(str2expression(weightformula))) #ALT: This could be replaced with "denom"
        #data2 <- data %>% select(all_of(c(all_adm, currVar, aggs_list, "indicator_weight")))
        # This is a really inelegant solution to filtering out binary/count vars for winsorization and for squashing
        var_unit <- subset(indicator_list, shortName %in% currVar)$units[[1]] #Should only be 1 list item
        var_continuous <- max(c("count","ratio", "boolean") %in% var_unit)==0
        variable <- sym(currVar)
        tempdata <- tempdata[!is.na(data[[currVar]]),] 
        if(nrow(tempdata) == 0){
          #exit <- T
          next #Skip code below and go back to the top of the loop. For variables that were included in the final data files but don't have any associated observations.
        }
        #Squashing datasets: if hh-crop vars are selected but no crop disaggregation or if realigned to commodity classes 
        #ALT: The squashing needs to happen before we apply weights because otherwise we lose some denominators.
        
        if(indicator_files[[j]]=="hh-crop_vars" | indicator_files[[j]]=="plot_vars") {
          denom <- indicator_list$denominator[[which(indicator_list$shortName==currVar)]] 
          # denom <- ifelse(is.na(denom), 0, denom) # HS 4/25: not sure if this is kosher, but want to see if this fixes our all-NA issue
          if(var_unit=="boolean") {
            tempdata <- tempdata %>% group_by(!!!syms(c(all_adm, aggs_list)), indicator_weight) %>% summarize(var = max(!!variable))  
          } else if(!is.na(denom)) {
            tempdata <- tempdata %>% group_by(!!!syms(c(all_adm, aggs_list)), indicator_weight) %>% summarize(var = weighted.mean(!!variable, dplyr::coalesce(!!sym(denom),0), na.rm = T)) #HS 4/25: na.rm only removes NAs from variable, not denom; thus, I implmented this coalsece solution (thanks stackoverflow)
          } else {
            tempdata <- tempdata %>% group_by(!!!syms(c(all_adm, aggs_list)), indicator_weight) %>% summarize(var = sum(!!variable))
          }
        } 
        else {
          tempdata <- tempdata %>% select(all_of(c(all_adm, currVar, aggs_list, "indicator_weight")))
        }
        names(tempdata)[names(tempdata)=="var"] <- currVar #dplyr doesn't like assigning results to strings or symbols so we need to use a temp name, then replace after; probably a better way to do this.
        ## APPLY WEIGHTS TO INDICATORS ##
        
        
        #ALT: An advantage of working with only a single indicator at a time is that we can tailor this depending on what our active variable is. 
        
        ## WINSORIZATION ## 
        if(input$wins==T & var_continuous==T) { 
          if(input$custom_winsor==T) {
            l_wins_threshold <- input$lower_wins/100
            u_wins_threshold <- input$upper_wins/100
          } else { 
            l_wins_threshold <- (indicator_list$wins_limit[[which(indicator_list$shortName %in% currVar)]])/100
            u_wins_threshold <- 1-l_wins_threshold
          }
          
          
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
        
        #Filter down to panel households
        if(input$panelcheck){
          tempdata <- tempdata[which(tempdata$hhid %in% panel_hhs$hhid & tempdata$ea %in% panel_hhs$ea),]
        }
        
        
        #CURRENCY CONVERSION: Relocated up from line ~710
        if(str_detect(var_unit, "currency")) {
          ccons_sub <- ccons %>% filter(abbr==instruments_sub$abbr[[i]], wave==instruments_sub$wave[[i]])
          #This was a lot more elegant but it also didn't work. See spreadsheet for input formatting.
          #ccons_conv <- (1+(ccons_sub$inflation*as.numeric(input$infl))) * switch(input$ccons,
          #                                                                        `LCU`=1,
          #                                                                        `PPP GDP$`=ccons_sub$gdp_ppp,
          #                                                                        `Consumption PPP$`=ccons_sub$cons_ppp)
          #Add this to the list of things that we can probably do more elegantly after the demo. 
          if(input$infl) {
            ccons_conv <- 1/(ccons_sub$inflation * switch(input$ccons,
                                                          `LCU`=1,
                                                          `PPP GDP$`=ccons_sub$gdp_ppp_2017,
                                                          `Consumption PPP$`=ccons_sub$cons_ppp_2017))
          } else {
            ccons_conv <- 1/switch(input$ccons,
                                   `LCU`=1,
                                   `PPP GDP$`=ccons_sub$gdp_ppp,
                                   `Consumption PPP$`=ccons_sub$cons_ppp)
          }
          tempdata[[currVar]] <- tempdata[[currVar]]*ccons_conv
        }
        
        
        ####### CALCULATING SUMMARY STATISTICS ###########
        tempdata <- na.omit(tempdata) #already removed na data values but I'm running into an issue where the geographic variables are missing
        #data3 <- output_data(tempdata, syms(c(aggs_list, adm_level)), sym(currVar), indicator_weight, currVar)
        ##ALT: New aggregate groupings code
        if(length(aggs_list)>0){
          if(any(aggs_list %in% "crop_name")) {
            if(length(unique(tempdata$crop_name))>1) {
              tempdata$crop_name <- factor(tempdata$crop_name)
              #todo: better names for crops as factor labels
            }
          }
          #ALT: Temporary fix til we correct farm size in the compilation code
          if (any(aggs_list %in% "farmsize")) {
            tempdata$farmsize <- sapply(tempdata$farmsize, switch, 
                                        "0 ha"=0, 
                                        ">0 - 2 ha"=1,
                                        "2 - 4 ha"=2,
                                        ">4 ha"=3)
          }
          if(length(which(aggs_list %!in% "crop_name")) > 0){
            for(agg in aggs_list[which(aggs_list %!in% "crop_name")]){
              tempdata[[agg]] <- as.character(tempdata[[agg]])
              flevels = groups_list[which(groups_list$varName==agg),]$Levels %>% str_split(., ",") %>% unlist()
              flabels = groups_list[which(groups_list$varName==agg),]$Labels %>% str_split(., ",") %>% unlist()
              tempdata[[agg]] <- factor(tempdata[[agg]], levels=flevels, labels=flabels)
              #Todo: better names for categorical farm size labels
            }
          }
        }
        #ALT: This line gets run regardless of whether there are aggs in aggs_list; should find a less messy way to do the lines above 
        data3 <- output_data(tempdata, syms(c(aggs_list, adm_level)), sym(currVar), indicator_weight, currVar)
        #Back to your regularly scheduled aggs wrangling program.
        
        #Special case - if someone only selects one crop, or if they apply a filter to an aggregation category and it ends up with a single level. Probably a neater way to do this, but it works.
        count_aggs_list <- 0
        if(length(aggs_list) > 0) {
          for(m in 1:length(aggs_list)){
            if(length(unique(data3[[aggs_list[[m]]]])) > 1){
              count_aggs_list <- count_aggs_list+1
            }
          }
        }
        if(count_aggs_list > 0) {
          for(m in 1:length(aggs_list)){
            #Todo: variables to make this look less terrible.
            if(length(unique(data3[[aggs_list[[m]]]]))>1) {
              data3_temp <- output_data(tempdata, syms(c(aggs_list[-m])), sym(currVar), indicator_weight, currVar)
              if(adm_num > 0) data3_temp[[adm_level]] <- "National" else data3_temp$geography <- instruments_sub$country[[i]]
              data3_temp[[aggs_list[[m]]]] <- max(as.numeric(data3[[aggs_list[[m]]]]))+1
              data3_temp[[aggs_list[[m]]]] <- factor(data3_temp[[aggs_list[[m]]]], levels=max(as.numeric(data3[[aggs_list[[m]]]]))+1, labels="All")
              data3[[adm_level]] <- as.character(data3[[adm_level]])
              data3 <- bind_rows(data3, data3_temp)
            }
          }
        }
        if(count_aggs_list > 1 | (count_aggs_list==0 & adm_num > 0)){
          data3_temp <- output_data(tempdata, list(), sym(currVar), indicator_weight, currVar)
          if(adm_num > 0) data3_temp[[adm_level]] <- "National" else data3_temp$geography <- instruments_sub$country[[i]]
          for(agg in aggs_list){
            if(length(unique(data3[[agg]]))>1) {
              data3_temp[[agg]] <- max(as.numeric(data3[[agg]]))+1
              data3_temp[[agg]] <- factor(data3_temp[[agg]], levels=max(as.numeric(data3[[agg]]))+1, labels="All")
            } else {
              data3_temp[[agg]] <- unique(data3[[agg]])
            }
          }
          data3 <- bind_rows(data3, data3_temp)
        }
        if(count_aggs_list>2 | (count_aggs_list>0 & adm_num > 0)){
          for(m in 1:length(aggs_list)){
            if(length(unique(data3[[aggs_list[[m]]]]))>1) {
              data3_temp <- output_data(tempdata, syms(aggs_list[m]), sym(currVar), indicator_weight, currVar)
              if(adm_num > 0) data3_temp[[adm_level]] <- "National" else  data3_temp$geography <- instruments_sub$country[[i]]
              #ALT: Flagging because there has to be a more sensible way to do this.
              for(agg in aggs_list[-m]){
                if(length(unique(data3[[agg]]))>1) {
                  data3_temp[[agg]] <- max(as.numeric(data3[[agg]]))+1
                  data3_temp[[agg]] <- factor(data3_temp[[agg]], levels=max(as.numeric(data3[[agg]]))+1, labels="All")
                } else {
                  data3_temp[[agg]] <- unique(data3[[agg]])
                }
              }
              data3 <- rbind(data3, data3_temp)
            }
          }
        }
        
        
        ##ALT END NEW CODE
        data3$abbr <- instruments_sub$abbr[[i]]
        data3$Country <- instruments_sub$country[[i]]
        data3$Year <- instruments_sub$yearlabel[[i]]
        data3$wave <- instruments_sub$wave[[i]]
        
        # Create data4 from data3 if it doesn't exist; else combine the two
        if(!exists("data4")) data4 <- data3 else data4 <- bind_rows(data4,data3)
        
        tempdata$abbr <- instruments_sub$abbr[[i]]
        tempdata$Country <- instruments_sub$country[[i]]
        tempdata$Wave <- instruments_sub$yearlabel[[i]]
        
        if(with(output_rawdata, exists(currVar))) {
          output_rawdata[[currVar]] <- bind_rows(output_rawdata[[currVar]], tempdata)
        } else {
          output_rawdata <- c(output_rawdata, list(tempdata))
          names(output_rawdata)[[length(output_rawdata)]] <- currVar
        }
        
      
      } 
      if(exists("data4")) {
        if(!exists("return_data")) return_data <- data4 else return_data <- bind_rows(return_data, data4)
        rm(data4)
      }
      
      
    }
    if(exists("return_data")) {
      if(!exists("return_data2")) { 
        return_data2 <- return_data 
      } else if(!exit) { 
        return_data2 <- bind_rows(return_data2, return_data)
      }
      
      rm(return_data) #Housekeeping. In rare circumstances it may be possible to skip the loop but still have the return_data object from the previous run. (ALT: as of the 11/29 edits I don't know if this is still true. Leaving it in anyway.)
    } # closes "if data exists" conditional
  } # closes "if there is no error" conditional
} # closes "for each instrument" loop around line 450
} # closes "for each country" loop around line 439





if(exists("return_data2")) {
  indicator_merge <- indicator_list %>% select(shortName, units)
  return_data2 <- merge(return_data2, indicator_merge, by.x="indicator", by.y="shortName", all.x=T, sort=F)
  return_data2[[adm_level]] <- return_data2[[adm_level]] %>% as.character() %>% tolower() %>% toTitleCase()
  ## ALT: This is the final polish step for displaying the data in tables; trimming down decimal points, sprucing up the unit labels, etc.
  return_data2$mean <- sapply(return_data2$mean, FUN=function(x){ifelse(x < 1000, signif(x, 4), round(x,0))})
  return_data2$median <- sapply(return_data2$median, FUN=function(x){ifelse(x < 1000, signif(x, 4), round(x,0))})
  return_data2$se <- signif(return_data2$se, 5)
  return_data2$total <- round(return_data2$total, 0)
  #return_data2$total[which(return_data2$units %!in% c("count", "ratio"))] <- NA #Remove totals that aren't meaningful
  return_data2$total[which(return_data2$units %in% "ratio")] <- NA #Ironically some ratios are meaningful; note to fix. 
  return_data2$units <- str_replace(return_data2$units,"currency", input$ccons)
  #output_list <- list("output"=return_data2, "raw_data"=output_rawdata, "sourcefile"=sourcefile)
  output_list <- list("output"=return_data2, "raw_data"=output_rawdata)
  updateCheckboxInput(session, "onSuccess", value=T)
  return(output_list)
} else if(err_out==F) { #If err_out is toggled on, then the user has already seen an error message.
  showNotification("Error: No data. The indicator(s) may not be available in the instrument(s) you selected.", type="error")
  return("")
} else return("")

    
    
    
  })
  
  }
  