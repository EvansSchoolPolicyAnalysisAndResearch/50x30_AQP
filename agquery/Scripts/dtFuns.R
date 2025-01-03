makeDataTable <- function(policiesIn, indicatorCategories, indicator_list, dataset_list){
  if(input$policiesBox1!="None" & is.list(policy_path)){
    policiesIn <- input$policiesBox1
    indics_out <- indicatorCategories |> filter(goalName==policiesIn) |> select(shortName) |> distinct() |> unlist()
    indics_out <- indicator_list$shortName[which(str_to_lower(indicator_list$shortName) %in% str_to_lower(indics_out))] |> unique() #TO DO: Include some cleaning code in the startup script 
    denoms <- getDenoms(indics_out, indicator_list)
    data_files <- getFiles(indicator_list, dataset_list, c(indics_out, denoms$denominator))
    
    if(nrow(data_files)==0){
      showNotification("No data files related to the selected pathway were found", type="error")
    } else {
      data <- getData(files=data_files, xvars=indics_out, denoms=denoms, adm_level=NA, source_call="pathwaysIn1")
      if(is.list(data)){
        if(is.list(data$droppedVars)){
          #output$droppedVars <- renderText(paste("The following variables were missing from the indicators_list spreadsheet or were all NA and were not processed:", paste(unique(dropped_vars), collapse=", ")))
        }
        
        #Still need to making the naming less stupid here.
        data_out <- data$outdata
        flag_table <- data_out |> pivot_wider(id_cols="shortName", names_from="year", values_from="Obs", names_glue="{year} N obs")
        flag_table <- merge(indicator_list |> select(shortName, labelName, flag_text), flag_table, by="shortName")
        
        data_table <- merge(data_out, indicator_list |> select(shortName, labelName, units), by="shortName")
        flag_table <- flag_table |> rename(Variable=labelName, Notes=flag_text) |> relocate(Notes, .after=last_col())
        
        return(list(data_table=data_table, flag_table=flag_table))
        
      }
    }
  }
}

filterFlagTable <- function(dt_out, pathway_link, pathwayTarget, indicator_list){
  if(pathwayTarget!=0){
    indics_out <- pathway_link |> filter(pathwayID==pathwayTarget) |> select(shortName) |> distinct()
    dt_out <- inner_join(dt_out, indics_out, by="shortName")
  } 
  return(dt_out)
}


filterVarTable <- function(dt_out, pathway_link, pathwayTarget, indicator_list, stat){
  if(pathwayTarget!=0){
    indics_out <- pathway_link |> filter(pathwayID==pathwayTarget) |> select(shortName) |> distinct()
    dt_out <- inner_join(dt_out, indics_out, by="shortName")
  } 
  dt_out <- dt_out |> select(shortName, labelName, year, units, matches(stat))
  if(stat=="Total"){
    dt_out <- dt_out |> filter(units!="ratio") #Exclude ratios from totals because they're already counted in a different indicator.
  }
  for(i in 1:nrow(dt_out)){
    poprow <- dt_out[i,]
    if(isTRUE(poprow$units=="boolean")) {
      if(stat=="Total") poprow$units <- "N households"
      if(stat=="Mean") poprow$units <- "% of households"
    }
    if(isTRUE(poprow$units=="kg") & isTRUE(poprow[[stat]] > 1000)){
      poprow[[stat]] <- poprow[[stat]]/1000
      poprow$units <- "Tonnes"
    } else if(isTRUE(poprow[[stat]] > 1000000)) {
      poprow$units <- paste(poprow$units, " (MM)")
      poprow[[stat]] <- poprow[[stat]]/1000000
    }
    dt_out[i,] <- poprow
  }
  dt_out <- dt_out |> filter(year==min(dt_out$year) | year==max(dt_out$year)) |>
    pivot_wider(id_cols=c("shortName", "labelName","units"), names_from="year", values_from=stat, names_glue="{year} {.value}") |>
    rename(Variable=labelName, Units=units)
  
  dt_out <- data.frame(dt_out)
  names(dt_out) <- str_replace_all(names(dt_out), "X", "")
  names(dt_out) <- str_replace_all(names(dt_out), "[.]", " ")
  dt_out$Trend <- signif((dt_out[,5]-dt_out[,4])/dt_out[,4], 4)
  dt_out[,4:5] <- format(signif(dt_out[,4:5], 4), big.mark=',', justify="right", scientific=F, digits=4, nsmall=0, drop0trailing=T)
  
  return(dt_out)
}