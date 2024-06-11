
updateTrends <- function(indicatorCategories, pathway_link, indicator_list){
  if(input$policiesBox1=="None"){
    output$msgText <- renderUI(HTML("<h4>Select a policy priority above to get started</h4>"))
  } else {
    
    #updateSelectInput(session, "policiesBox2", selected=input$policiesBox1)
    #TODO: Long term it makes more sense to have a single file for this operation; the central challenge is the fact that users will probably want to view indicators without having a pathway in mind.
    if(input$pathwaysIn1==0){
      indics_out <- indicatorCategories %>% filter(goalName==input$policiesBox1) %>% select(shortName) %>% distinct() %>% unlist()
    } else {
      indics_out <- pathway_link %>% filter(pathwayID==input$pathwaysIn1) %>% select(shortName) %>% distinct() %>% unlist()
    } 
    
    #data_files <- as.data.frame(dataset_list[str_detect(str_to_lower(dataset_list), str_to_lower(input$policiesBox1))]) #Might need to store this as a global later. 
    #Need to be more consistent in tracking case
    data_files_select <- indicator_list[which(indicator_list$shortName %in% indics_out),] %>% 
      select(file) %>% 
      distinct() %>% 
      unlist() #TODO: Clean this up
    data_files <- lapply(data_files_select, FUN=function(x){dataset_list[which(str_detect(str_to_lower(dataset_list), str_to_lower(x)))]}) %>% unique() %>% unlist()  #Drop duplicates if they're somehow in there.
    #data_files <- dataset_list %>% select(which(str_to_lower(dataset_list) %in% str_to_lower(data_files_select)))
    #data_files <- dataset_list[which(str_detect(str_to_lower(dataset_list), str_to_lower(data_files_select)))] %>% as.data.frame()
    data_files <- as.data.frame(data_files)
    names(data_files) <- "file.name"
    data_files$year <- str_extract(data_files$file.name, "[0-9]{4}") #Might be unnecessary 
    #This gets tricky for variables that are only collected every three years in each of the rotating modules. 
    #if(input$trendChooser=='prevSurv') {
    #  data_files <- data_files %>% filter(year==max(year) | year==max(data_files$year[data_files$year!=max(data_files$year)])) #get highest and second highest values
    #} 
    
    #Get variables from the 
    data_out <- getData(data_files, indics_out)
    if(!any(data_out=="")){
      data_out <- data_out$tempdata
      indics_out <- names(data_out)[which(names(data_out) %in% indics_out)] #filter out any variables that weren't processed
      data_table <- data.frame(shortName=indics_out) #TODO: Simplify
      flag_table <- data_table #make a copy for metadata.
      flag_table[[paste0(min(data_out$year), " N obs")]] <- NA
      flag_table[[paste0(max(data_out$year), " N obs")]] <- NA
      flag_table <- merge(data_table, indicator_list %>% select(shortName, labelName, flag_text))
      data_table <- merge(data_table, indicator_list %>% select(shortName, labelName), by="shortName")
      
      data_table[[paste0(min(data_out$year), " Mean")]] <- NA
      #data_table[[paste0(min(data_out$year), " N obs")]] <- NA #Moved these to the metadata table. 
      data_table[[paste0(max(data_out$year), " Mean")]] <- NA
      #data_table[[paste0(max(data_out$year), " N obs")]] <- NA
      data_table$Units <- ""
      data_table$Trend <- ""
      data_table$LTT_Trend <- ""
      
      for(var in indics_out){
        sub_data <- data_out %>% select(all_of(c(var, "year", "weight"))) %>% na.omit()
        if(nrow(sub_data)==0 | !is.numeric(sub_data[[var]])){
          next
        } else {
          if(length(unique(sub_data$year))<2){
            year <- unique(sub_data$year)
            data_table[[paste0(year, " Mean")]][data_table$shortName==var] <- signif(inject(with(sub_data,weighted.mean(!!sym(var), weight))),4)
            flag_table[[paste0(year, " N obs")]][flag_table$shortName==var] <- nrow(sub_data)
            data_table$Trend[data_table$shortName==var] <- "N/A"
          } else if(length(unique(sub_data$year>=2))) {
            min_mean <- inject(with(sub_data %>% filter(year==min(sub_data$year)), weighted.mean(!!sym(var), weight)))
            max_mean <- inject(with(sub_data %>% filter(year==max(sub_data$year)), weighted.mean(!!sym(var), weight)))
            min_n <- nrow(sub_data %>% filter(year==min(sub_data$year)))
            max_n <- nrow(sub_data %>% filter(year==max(sub_data$year)))
            if(min_mean==0){ 
              if(max_mean > 0){
                chg = "+Inf"
              } else if(max_mean < 0) {
                chg="-Inf"
              } else {
                chg="⮕ 0%"
              }
            } else {  
              diff=signif((max_mean-min_mean)/min_mean*100, 2)
              
              if(diff>5){
                dir_arrow <- "⬆ "
              } else if(diff < -5) {
                dir_arrow <- "⬇ "
              } else {
                dir_arrow <- "⮕ "
              }
              
              chg=paste0(dir_arrow, diff, "%")  
            }
            data_table[[paste0(min(data_out$year), " Mean")]][data_table$shortName==var] <- signif(min_mean,4)
            data_table[[paste0(max(data_out$year), " Mean")]][data_table$shortName==var] <- signif(max_mean,4)
            flag_table[[paste0(min(data_out$year), " N obs")]][flag_table$shortName==var] <- min_n
            flag_table[[paste0(max(data_out$year), " N obs")]][flag_table$shortName==var] <- max_n
            data_table$Trend[data_table$shortName==var] <- chg
          } #Implement regression later?
          
        }
      }
     
      trendVarList <- as.list(c("0", data_table$shortName))
      return(list(data_table=data_table, flag_table=flag_table, trendVarList=trendVarList))
    }
  }
}