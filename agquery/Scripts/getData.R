not_all_na <- function(x) any(!is.na(x)) #Helper function

getIndics <- function(pathway_link, indicator_list, indic_inventory, policy, pathway, obsyear, cats=F){
  if(pathway!="0"){ 
    indics_out <- pathway_link %>% filter(goalName==policy, pathwayID==pathway) %>% merge(., indicator_list, by="shortName") 
    
  } else {
    indics_out <- pathway_link %>% filter(goalName==policy) %>% merge(., indicator_list, by="shortName")
  }
  if(cats==T){
    indics_out <- merge(indics_out, indic_inventory %>% filter(year %in% obsyear), by="shortName") %>% select(shortName, category, labelName) %>% distinct()  %>% arrange(category)
    indics <- lapply(unique(indics_out$category), FUN=function(x){
      sub_indics <- indics_out %>% filter(category==x)
      temp_indics <- as.list(sub_indics$shortName)
      names(temp_indics) <- sub_indics$labelName
      return(temp_indics)
    })
    names(indics) <- unique(indics_out$category)
  } else {
    indics_out <- merge(indics_out, indic_inventory %>% filter(year %in% obsyear), by="shortName") %>% select(shortName, labelName) %>% distinct()
    indics <- as.list(indics_out$shortName)
    names(indics) <- indics_out$labelName 
  }
  return(indics)
}

getDenoms <- function(indics, indicator_list){
  indicator_list %>% filter(shortName %in% indics) %>% select(shortName, denominator) %>% na.omit()
}


getFiles <- function(indicator_list, dataset_list, indicators){ #Small helper function to get the files formatted for getData
  data_files_select <- indicator_list[which(indicator_list$shortName %in% indicators),] %>% select(file) %>% distinct() %>% unlist() #TODO: Clean this up
  dataset_list_lwr <- str_to_lower(dataset_list)
  data_files_select <- str_to_lower(data_files_select)
  matching_files <- lapply(data_files_select, FUN=function(x){which(str_detect(dataset_list_lwr, x))}) %>% unlist()
  data_files <- dataset_list[matching_files] %>% as.data.frame()
  names(data_files) <- "file.name"
  data_files$year <- str_extract(data_files$file.name, "[0-9]{4}")
  return(data_files)
}


#TO DO: Integrate changes already in getFiltData: killing mapdata, upper and lower winsorization thresholds.
getData <- function(files, xvars, yvars=NULL, denoms=NULL, adm_level="hhid", aggs_list="", source_call="none", drop_0s=F){
  varslist <- c(xvars, yvars, if(!is.null(denoms)) denoms$denominator)
  aggs_list <- c(aggs_list[nzchar(aggs_list)], "year") 
  years <- files$year %>% unique()
  out_flag <- F
  exit <- F
  files$survey <- str_extract(files$file.name, "([aA-zZ]+_2[0-9]{3})", group=1)
  surveys <- unique(files$survey)
  for(survey in surveys){
    files_in <- files$file.name[files$survey==survey]
    for(file in files_in){ #TODO NOTES FOR 5/1: Dealing with multiple files (should combine them first?) - check and make sure the 0 filter is doing what it's supposed to.
      df_raw <- tryCatch(read.csv(paste0("Data/", file)), 
                         error=function(e){
                           showNotification(paste("File", file, "not found"), type="error")
                           break
                         }) #can simplify using full paths in list.files
      if(exists("df_survey", mode="list")){
        df_survey <- merge(df_survey, df_raw, by=c("hhid", "province", "zone")) #To fix
      } else {
        df_survey <- df_raw
      }
      rm(df_raw)
    }
    df_survey <- df_survey |> select(where(not_all_na))
    varslist_short <- names(df_survey)[which(names(df_survey) %in% c(xvars,yvars))]
    if(length(varslist_short)==0){
    #  showNotification(sprintf("The selected %s not found in %s", if(length(varslist)==1) "variable was" else "variables were", survey), type="error")
    } else {
      for(currVar in varslist_short) {
        #Error handling
        if(!(currVar %in% indicator_list$shortName) | all(is.na(df_survey[[currVar]]))){
          varslist_short <- varslist_short[-which(varslist_short==currVar)]
          if(!exists("dropped_vars")){
            dropped_vars <- currVar
          } else {
            dropped_vars <- c(dropped_vars, currVar)
          }
        }
      }
      #Dealing with bad denominators
      
      if(length(varslist_short)==0 & source_call!="trendmaps"){ 
        showNotification(sprintf("The selected variable(s) in %s did not contain non-NA and non-0 values.", survey), type="error")
    } else {
      # if(!is.null(denoms)){
      #   #Filter to drop unneded rows first.
      #   denoms <- denoms |> filter(shortName %in% varslist_short)
      #   if(nrow(denoms)> 0){
      #   denoms_keep <- vector()
      #   for(i in 1:nrow(denoms)){
      #     if(with(df_survey, exists(denoms$denominator[[i]]))){
      #       denoms_keep <- c(denoms_keep, i)
      #     }
      #   }
      #   if(length(denoms_keep)>0){
      #     denoms <- denoms[denoms_keep,]
      #   } else {
      #     denoms <- NULL
      #   }
      #   } else {
      #     denoms <- NULL
      #   }
      # }
      # 
      if(!with(df_survey, exists("weight"))){
        weights <- tryCatch(read.csv(sprintf("Data/%s_weights.csv",survey)),
                            error=function(e){
                              showNotification("Weights file missing; unweighted averages will be shown", type="warning")
                              df_survey$weight <- 1
                            })
        if(exists("weights")){
          mergeNames <- names(df_survey)[which(names(df_survey) %in% names(weights))] #Slightly more flexible
          if(length(mergeNames)==0){
            showNotification(sprintf("Error in merging weights file for %s: ID column not found. Unweighted averages will be shown.", survey), type="error")
            df_survey$weight <- 1
          } else {
            df_survey <- merge(df_survey, weights, by=mergeNames)
          }
          rm(weights)
        }
      }
      
      if(length(aggs_list > 1)){
        if(!all(aggs_list[aggs_list!="year"] %in% names(df_survey))){
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
              mergeNames <- names(df_survey)[which(names(df_survey) %in% names(groups))] 
              df_survey <- merge(df_survey, groups, by="hhid")
            }
          }
        }
      }
      df_survey <- df_survey %>% mutate(year = as.numeric(str_extract(file, "2[0-9]{3}"))) %>% 
        #select(all_of(c("hhid", if(!is.na(adm_level)) adm_level, varslist_short, if(!is.null(denoms)) denoms$denominator, "weight", aggs_list))) #At some point we're going to need to figure out how to undo the hard coding of province for portability to other countries.
        select(all_of(c(if(!is.na(adm_level)) adm_level, 
                        if(isTRUE(adm_level=="hhid")) "province",
                        varslist_short)), 
               any_of(c("weight", aggs_list, if(!is.null(denoms)) denoms$denominator))) #To fix.
      #TO DO: ERROR HANDLING IF DENOM IS MISSING
      if(drop_0s==T){
        df_survey <- df_survey %>% filter(!!sym(yvars)!=0)
      }
      
      for(currVar in varslist_short){
        
        #Error handling: in case the data export still has the Stata labels (protects against bad exports; might be better to use dtas instead to avoid this entirely).
        if(!is.numeric(df_survey[[currVar]])){
          df_survey <- df_survey %>% mutate_at(currVar, list(~ recode(., 'None'='0', 'No'='0', 'Yes'='1')))
          df_survey[[currVar]] <- as.numeric(df_survey[[currVar]])
        }
        
        
        var_unit <- subset(indicator_list, shortName %in% currVar)$units[[1]] #Should only be 1 list item
        #var_continuous <- max(c("count","ratio", "boolean") %in% var_unit)==0 #I don't think this is necessary because we don't have custom Winsorization limits in the interface
        #if(var_continuous==T) { 
        l_wins_threshold <- (indicator_list$wins_limit[[which(indicator_list$shortName %in% currVar)]])/100
        u_wins_threshold <- 1-l_wins_threshold
        
        if(!is.numeric(l_wins_threshold)) { #I.e., spreadsheet cell was empty or boxes were somehow made blank
          l_wins_threshold <- 0
        }
        if(!is.numeric(u_wins_threshold)){
          u_wins_threshold <- 1
        }
        
        #Use zeros in the spreadsheet for vars that you don't want to winsorize
        lim <- quantile(df_survey[[currVar]],probs=c(l_wins_threshold, u_wins_threshold), na.rm=T)
        #easier to understand than trying a mutate_at?
        df_survey[[currVar]][df_survey[[currVar]] < lim[1]] <- lim[1] 
        df_survey[[currVar]][df_survey[[currVar]] > lim[2]] <- lim[2] 
        #}
        
      }
      if(exists("df", mode="list")){ #really need to get in the habit of avoiding reserved words.
        df <- bind_rows(df, df_survey)
      } else {
        df <- df_survey
      }
      
    }
    } #Commented out "Else" clause above.
    rm(df_survey)
  }
  if(exists("df", mode="list")){
    varslist_short <- names(df)[which(names(df) %in% c(xvars,yvars))] #This will be dropped if the last survey is missing the variable so we need to recreate it. Might want to clean up the names.
    if(!nrow(df)==0){  
      #Doing this down here to avoid messing up household data export, although hhdata might or might not have denoms at this point. 
      for(currVar in varslist_short){
        if(!is.null(denoms)){
          df[[paste0("weight.", currVar)]] <- if(any(currVar %in% denoms$shortName)){
            denominator <- denoms$denominator[which(denoms$shortName==currVar)]
            df$weight*df[[denominator]]
          } else {
            df$weight
          }
        } else { 
          df[[paste0("weight.", currVar)]] <- df$weight
        }
        
        
        names(df)[names(df)==currVar] <- paste0("value.", currVar)
      }
      #If denoms are also target variables, they'll be safe behind "value" and "weight"
      df <- df %>% select(-any_of(c(denoms$denominator, "weight"))) %>% 
        pivot_longer(cols=starts_with(c("value", "weight")), names_to=c(".value","shortName"), names_sep="[.]") 
      #can combine these once mapdata problem is solved. 
      if(adm_level %in% "hhid"){
        outdf <- df %>% select(-weight) %>% rename(Mean=value) #Kludge but that's how it's coming back - fix later.
        if(!exists('outdata')){
          outdata <- outdf
        } else { 
          outdata <- bind_rows(outdata, outdf)
        } 
      } else {
        tempdata <- df %>% 
          group_by(!!!syms(na.omit(c(adm_level, aggs_list, "shortName")))) %>%   #Na omit is awkward but that's how we get the national stats
          summarize(Mean=weighted.mean(value, w=weight, na.rm=T), Total=sum(value*weight, na.rm=T), Obs=sum(!is.na(value)))
        tempdata$Mean[is.nan(tempdata$Mean)] <- NA
        if(exists("outdata")){ 
          outdata <- bind_rows(outdata, tempdata) 
        } else {
          outdata <- tempdata
        }
      }
      
      # mapdata_temp <- df %>% group_by(province, year, shortName) %>% #there's still a major efficiency issue here. #Also this hard coding needs to be undone.
      #  summarize(Mean=weighted.mean(value, w=weight, na.rm=T), Total=sum(value*weight, na.rm=T), Obs=sum(!is.na(value))) %>%
      # pivot_wider(id_cols=c("province", "year"), names_from=shortName, values_from=c("Mean", "Total", "Obs"))
      # 
      
      if(adm_level %in% "hhid" | adm_level %in% "province"){
        mapdata_temp <- df %>% group_by(province, year, shortName) %>%
          summarize(Mean=weighted.mean(value, w=weight, na.rm=T)) %>% 
          pivot_wider(id_cols=c("province", "year"), names_from=shortName, values_from="Mean")
      } else if(adm_level %in% "zone") {
        mapdata_temp <- df |> group_by(zone, year, shortName) |> 
          summarize(Mean=weighted.mean(value, w=weight, na.rm=T)) |>
          pivot_wider(id_cols=c('zone', 'year'), names_from=shortName, values_from="Mean")
      } else {
        mapdata_temp <- tempdata #Kludge: there's no need to include mapdata if we're doing national summaries (e.g. adm_level is na), but we need to add a way to handle if missing.
      }
      if(!exists("mapdata")){
        mapdata <- mapdata_temp
      } else {
        mapdata <- bind_rows(mapdata, mapdata_temp)
      }
      
      rm(df)
    }
  }
  
  
  
  
  if(!exists("droppedVars")){
    droppedVars <- ""
  }
  if(exists("outdata")){
    
    #return(list(means_out=means_out, totals_out=totals_out, nat_means=nat_means, nat_tots=nat_tots, outdata=outdata, mapdata=mapdata, droppedVars=droppedVars)) #really need to fix the names here.
    return(list(outdata = outdata, mapdata=mapdata))
  } else {
    return("")
  }
  
}

#Duplicate, to integrate into the above, but I'm trying not to break things #Todo: fix order 

getFiltData <- function(files, xvars, yvars=NULL, denoms=NULL, aggs_list="", filter="",  adm_level="hhid", source_call="none", drop_0s=F){
  varslist <- c(xvars, if(!is.null(yvars)) yvars, if(!is.null(denoms)) denoms$denominator)
  aggs_list <- c(aggs_list[nzchar(aggs_list)], "year") 
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
        df <- merge(df, df_in, by="hhid", all.x=T, all.y=T)
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
          df$weight <- 1
        } else {
          df <- merge(df, weights, by=mergeNames)
        }
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
    if(with(df, exists(filter))){
      df <- df %>% filter(!!sym(filter)==1) #Keep only yes for everything. 
    }
    #Fix denominator issues. This is super messy.
    varslist_short <- names(df)[which(names(df) %in% c(xvars,yvars))]
    if(length(varslist_short)==0){
      showNotification(paste("No variables for the selected policy priority were found in", survey))
    } else {
      df <- df %>% mutate(year = as.numeric(str_extract(file, "2[0-9]{3}"))) %>% 
        select(all_of(c("hhid",adm_level, varslist_short, if(!is.null(denoms)) denoms$denominator, "weight", aggs_list))) #At some point we're going to need to figure out how to undo the hard coding of province for portability to other countries.
      
      
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
        if(source_call!="trendmaps"){
          showNotification(paste("Error: Data file", file, "is empty or only contains variables not listed in indicators_list"), type="error")
        }
      } else {
        for(currVar in varslist_short){
          
          #Error handling: in case the data export still has the Stata labels (protects against bad exports; might be better to use dtas instead to avoid this entirely).
          if(!is.numeric(df[[currVar]])){
            df <- df %>% mutate_at(currVar, list(~ recode(., 'None'='0', 'No'='0', 'Yes'='1')))
            df[[currVar]] <- as.numeric(df[[currVar]])
          }
          
          
          var_unit <- subset(indicator_list, shortName %in% currVar)$units[[1]] #Should only be 1 list item
          #var_continuous <- max(c("count","ratio", "boolean") %in% var_unit)==0 #I don't think this is necessary because we don't have custom Winsorization limits in the interface
          #if(var_continuous==T) { 
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
          #df <- df %>% #mutate(ifelse(!!sym(currVar) < lim[1], lim[1], !!sym(currVar))) %>% #Taking out lower wins threshold
          #  mutate(ifelse(!!sym(currVar) > lim[2], lim[2], !!sym(currVar)))
          #df[[currVar]][df[[currVar]] < lim[1]] <- lim[1] 
          df[[currVar]][df[[currVar]] > lim[2]] <- lim[2] 
          #}
          #Winsorize only based on observed values
          #df <- mutate(ifelse(is.na(!!sym(currVar)), 0, !!sym(currVar)))
          df[[currVar]][is.na(df[[currVar]])] <- 0
        }
        if(drop_0s==T){
          df <- df %>% filter(!!sym(yvars)!=0)
        }
        
        if(exists("df", mode="list")){
          if(!nrow(df)==0){  
            #Doing this down here to avoid messing up household data export, although hhdata might or might not have denoms at this point. 
            for(currVar in varslist_short){
              if(!is.null(denoms)){
                df[[paste0("weight.", currVar)]] <- if(any(currVar %in% denoms$shortName)){
                  denominator <- denoms$denominator[which(denoms$shortName==currVar)]
                  df$weight*df[[denominator]]
                } else {
                  df$weight
                }
              } else { 
                df[[paste0("weight.", currVar)]] <- ifelse(is.na(df$weight), 0, df$weight)
              }
              
              
              names(df)[names(df)==currVar] <- paste0("value.", currVar)
            }
            #If denoms are also target variables, they'll be safe behind "value" and "weight"
            df <- df %>% select(-any_of(c(denoms$denominator, "weight"))) %>% 
              pivot_longer(cols=starts_with(c("value", "weight")), names_to=c(".value","shortName"), names_sep="[.]") 
            #can combine these once mapdata problem is solved. 
            if(adm_level %in% "hhid"){
              outdf <- df %>% select(-weight) %>% rename(Mean=value) #Kludge but that's how it's coming back - fix later.
              if(!exists('outdata')){
                outdata <- outdf
              } else { 
                outdata <- bind_rows(outdata, outdf)
              } 
            } else {
              tempdata <- df %>% 
                group_by(!!!syms(na.omit(c(adm_level, aggs_list, "shortName")))) %>%   #Na omit is awkward but that's how we get the national stats
                summarize(Mean=weighted.mean(value, w=weight, na.rm=T), Total=sum(value*weight, na.rm=T), Obs=sum(!is.na(value)))
              
              nat_data <- df %>%
                summarize(Mean=weighted.mean(value, w=weight, na.rm=T), Total=sum(value*weight, na.rm=T), Obs=sum(!is.na(value)))
              
              if(exists("outdata")){ 
                outdata <- bind_rows(outdata, tempdata)
                nat_out <- bind_rows(natdata, nat_out)
              } else {
                outdata <- tempdata
                nat_out <- nat_data
              }
            }
            
            # mapdata_temp <- df %>% group_by(province, year, shortName) %>% #there's still a major efficiency issue here. #Also this hard coding needs to be undone.
            #  summarize(Mean=weighted.mean(value, w=weight, na.rm=T), Total=sum(value*weight, na.rm=T), Obs=sum(!is.na(value))) %>%
            # pivot_wider(id_cols=c("province", "year"), names_from=shortName, values_from=c("Mean", "Total", "Obs"))
            # 
            
            #mapdata_temp <- df %>% group_by(province, year, shortName) %>%
            #  summarize(Mean=weighted.mean(value, w=weight, na.rm=T)) %>%
            #  mutate(Mean=signif(Mean,4)) %>%
            #  pivot_wider(id_cols=c("province", "year"), names_from=shortName, values_from="Mean")
            
            #if(!exists("mapdata")){
            #  mapdata <- mapdata_temp
            #} else {
            #  mapdata <- bind_rows(mapdata, mapdata_temp)
            #}
            
            rm(df)
          }
        }
      }
    }
  }
  
  if(!exists("droppedVars")){
    droppedVars <- ""
  }
  if(exists("outdata")){
    #return(list(means_out=means_out, totals_out=totals_out, nat_means=nat_means, nat_tots=nat_tots, outdata=outdata, mapdata=mapdata, droppedVars=droppedVars)) #really need to fix the names here.
    #return(list(outdata = outdata, mapdata=mapdata, natdata=nat_out))
    return(list(outdata = outdata, natdata=nat_out))
  } else {
    return("")
  }
  
}