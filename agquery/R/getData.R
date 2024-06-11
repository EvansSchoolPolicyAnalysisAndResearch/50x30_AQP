getData <- function(files, xvars, yvars=NULL, adm_level="hhid", aggs_list=NULL, source_call="none", drop_0s=F){
  varslist <- c(xvars, yvars)
  aggs_list <- c(aggs_list, "year")
  years <- files$year %>% unique()
  out_flag <- F
  exit <- F
  files$survey <- str_extract(files$file.name, "([aA-zZ]+_2[0-9]{3})", group=1)
  surveys <- unique(files$survey)
  for(survey in surveys){
    files_in <- files$file.name[files$survey==survey]
    for(file in files_in){ #TODO NOTES FOR 5/1: Dealing with multiple files (should combine them first?) - check and make sure the 0 filter is doing what it's supposed to.
      df_in <- tryCatch(read.csv(paste0("Data/", file)), 
                        error=function(e){
                          showNotification(paste("File", file, "not found"), type="error")
                          break
                        }) #can simplify using full paths in list.files
      if(exists("df", mode="list")){
        df <- merge(df, df_in, by=c("hhid", "province"))
      } else {
        df <- df_in
      }
      rm(df_in)
    }
    if(!with(df, exists("weight"))){
      weights <- tryCatch(read.csv(sprintf("Data/%s_weights.csv",survey)),
                          error=function(e){
                            showNotification("Weights file missing; unweighted averages will be shown", type="warning")
                            df$weight <- 1
                          })
      if(exists("weights")){
        mergeNames <- names(df)[which(names(df) %in% names(weights))] #Slightly more flexible
        if(length(mergeNames)==0){
          showNotification("Error in merging weights file: ID column not found. Unweighted averages will be shown.", type="error")
        }
        df <- merge(df, weights, by=mergeNames)
        rm(weights)
      }
    }
    if(length(aggs_list > 1)){
      if(!all(aggs_list[aggs_list!="year"] %in% names(df))){
        groups <- tryCatch(read.csv(sprintf("Data/%s_groups.csv", survey)) %>% select(any_of(c("hhid",aggs_list[aggs_list!="year"]))), #Should fix this workaround with year
                           error=function(e){
                             showNotification(paste("Grouping file for the survey", survey, "was not found. No groups were applied."))
                             aggs_list <- "year"
                             return("")
                           })
        if(is.list(groups)){
          if(ncol(groups)==1){
            showNotification(paste("Grouping variable", aggs_list[aggs_list!="year"], "was not found"))
            aggs_list <- "year"
          } else {
            mergeNames <- names(df)[which(names(df) %in% names(groups))] 
            df <- merge(df, groups, by="hhid")
          }
        }
      }
    }
    varslist_short <- names(df)[which(names(df) %in% varslist)]
    if(length(varslist_short)==0){
      showNotification(paste("No variables for the selected policy priority were found in", survey))
    } else {
      df <- df %>% mutate(year = as.numeric(str_extract(file, "2[0-9]{3}"))) %>% 
        select(all_of(c("hhid","province", varslist_short, "weight", aggs_list))) #At some point we're going to need to figure out how to undo the hard coding of province for portability to other countries.
      
      #TODO: Fix this w/r/t the trends page. 
      if(drop_0s){
        df <- df %>% filter(!!sym(yvars)!=0)
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
        if(exists("df", mode="list")){
          if(!nrow(df)==0){  
            if(adm_level=="hhid"){
              if(!exists('outdata')){
                outdata <- df
              } else { 
                outdata <- bind_rows(outdata, df)
              }
            } else {
              tempdata <- df %>% 
                group_by(!!!syms(c(adm_level, aggs_list))) %>% 
                summarize(across(all_of(varslist_short), ~ weighted.mean(.x, w=weight, na.rm=T)))
              if(!exists('outdata')){
                outdata <- tempdata
              } else { 
                outdata <- bind_rows(outdata, tempdata) 
              } 
            }
            
            mapdata_temp <- df %>% group_by(province, year) %>% #there's still a major efficiency issue here. 
              summarize(across(all_of(varslist_short), ~weighted.mean(.x, w=weight, na.rm=T)))
            
            if(!exists("mapdata")){
              mapdata <- mapdata_temp 
            } else {
              mapdata <- bind_rows(mapdata, mapdata_temp)
            }
            rm(df)
          }
        }
      }
    }
  }
  if(exists("dropped_vars")){
    output$droppedVars <- renderText(paste("The following variables were missing from the indicators_list spreadsheet or were all NA and were not processed:", paste(unique(dropped_vars), collapse=", ")))
  }
  if(exists("outdata")){
    return(list(tempdata=outdata, mapdata=mapdata)) #really need to fix the names here.
  } else {
    return("")
  }
  
}