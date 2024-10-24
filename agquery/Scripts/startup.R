

#root_dir <- paste0(getwd(), "/")
root_dir <- ""

#LOADING IN AND VALIDATING SPREADSHEETS
#Some of this could probably be cached for faster startup

# indicatorCategories <- tryCatch(read.xlsx("Update/indicatorCategories.xlsx"),
#                                 error=function(e) {
#                                   return(F)
#                                 }) 
# if(is.list(indicatorCategories)){
#   if(("shortName" %in% names(indicatorCategories)) & ncol(indicatorCategories) > 1) {
#     indicatorCategories <- indicatorCategories %>% 
#       melt() %>% 
#       filter(value==1) %>% 
#       select(-value) %>% 
#       rename(goalName=variable)
#     goalNames <- str_to_title(unique(indicatorCategories$goalName))
#   } else {
#     goalNames <- ""
#   }
# }

#Dropping the indicator categories spreadsheet for efficiency

dataset_list <- list.files("Data", pattern="*.csv")
years <- lapply(dataset_list, FUN=function(x){str_extract(x, "[0-9]{4}")}) %>% unique()
for(year in years){
  names <- lapply(dataset_list[which(str_detect(dataset_list, year))], function(x){
    dat <- read.csv(paste0("Data/",x), nrows=1)
    outdf <- data.frame(shortName=names(dat))
    outdf$file <- str_extract(x, "_([aA-zZ]+).csv", group=1)
    return(outdf)
  })
  names <- do.call("rbind", names)
  names$year <- year
  names <- distinct(names)
  #names <- unlist(names) %>% unique()
  if(!exists("indic_inventory")){
    indic_inventory <- names
  } else {
    indic_inventory <- bind_rows(indic_inventory, names)
  }
 }

instrument_list <- tryCatch(readxl::read_xlsx("Update/instrument_list.xlsx"),
                            error=function(e){return(F)})
if(is.list(instrument_list)){
  colnm_instr <- c("survey","wave","country","year","yearlabel") #including only the essentials right now
  if(any(!(colnm_instr %in% names(instrument_list)))){
    instrument_list <- F
  } else {
    year_list <- as.list(instrument_list$year)
    names(year_list) <- instrument_list$yearlabel
  }
}

indicator_list <- tryCatch(readxl::read_xlsx("Update/indicators.xlsx"),
                           error=function(e){return(F)}
)

if(is.list(indicator_list)){
  colnm_indic <- c("shortName", "labelName","axisName", "file", "wins_limit", "units", "denominator") #Again, only what we minimally need to operate. Note we're moving flags to a new sheet
  if(any(!(colnm_indic %in% names(indicator_list)))){
    indicator_list <- F
  }
}

pathway_link <- tryCatch(read.csv("Update/Policy_Link.csv") %>% distinct(), #Remove duplicates (bad input protection)
                         error=function(e){return(F)})
if(is.list(pathway_link)){
  colnm_link <- c("pathwayID", "goalName","shortName")
  if(any(!(colnm_link %in% names(pathway_link)))){
    pathway_link <- F
  } else {
    goalNames <- str_to_title(unique(pathway_link$goalName))
    indicatorCategories <- pathway_link %>% select(goalName, shortName) %>% distinct()
  }
}




groups_list <- tryCatch(readxl::read_xlsx("Update/grouping_vars.xlsx"),
                        error=function(e){return(F)})
if(is.list(groups_list)){
  colnm_grps <- c("varName","label","shortName","Levels","Labels","level") #need to fix names here
  if(any(!(colnm_grps %in% names(groups_list)))){
    groups_list <- F
  }
}

policy_path <- tryCatch(read.csv("Update/Policy_Pathways.csv", header = TRUE),
                        error=function(e){return(F)}) #Need to add name enforcement to prevent crashes on targeted styles
if(is.list(policy_path)){
  #if(is.list(pathway_link)){
  #  if(is.list(indicator_list)){
  #    available_indics <- merge(pathway_link, indicator_list, by="shortName")
  #    available_indics <- available_indics %>% group_by(pathwayID) %>% 
  #      mutate(Available.CAS.Indicators=paste0(labelName, collapse="<br>")) %>%
  #      select(pathwayID, Available.CAS.Indicators) %>% distinct()
  #  } else {
  #  available_indics <- pathway_link %>% group_by(pathwayID) %>%
  #    mutate(Available.CAS.Indicators=paste0(shortName, collapse="<br>")) %>%
  #    select(pathwayID, Available.CAS.Indicators)
  #  }
  #  policy_path <- merge(policy_path, available_indics, by="pathwayID")
  #  policy_path <- policy_path %>% relocate(Available.CAS.Indicators, .before=Evidence)
#}
  pathwaysDT <- policy_path %>% select(-c(pathwayID, goalName))
  #pathwaysDT$Available.CAS.Indicators <- gsub(pattern="\n", replacement="<br>", x=pathwaysDT$Available.CAS.Indicators)

  pathway_names <- unique(policy_path$Policy.Goal)
  short_Pathways <- unique(policy_path$goalName)
  names(pathwaysDT) <- str_replace_all(names(pathwaysDT), "\\.", " ")
  
  polic_Names <- lapply(short_Pathways, FUN=function(x){
    policy_path_sub <- policy_path %>% filter(goalName==x)
    inst_names <- unique(policy_path_sub$Instrument)
    temp_list <- lapply(inst_names, FUN=function(y){
      tempnames <- policy_path_sub$Implementation[policy_path_sub$Instrument==y]
      tempvals <- as.list(policy_path_sub$pathwayID[policy_path_sub$Instrument==y])
      
      names(tempvals) <- tempnames
      return(tempvals)
    })
    names(temp_list) <- inst_names
    temp_list <- c(list(`All Instruments`=0), temp_list)
    return(temp_list)
  })
  names(polic_Names) <- short_Pathways
}

ext_data <- tryCatch(read.csv("Update/Secondary_Sources.csv"), 
  error=function(e){return(F)}
)
if(is.list(ext_data)){
  colnm_ext <- c("Source","Relevant.Variables", "Location")
  if(any(!(colnm_ext %in% names(ext_data)))){
    ext_data <- F
  } else {
    ext_data$Location <- lapply(ext_data$Location, FUN=function(x){sprintf("<a href=%s>%s</a>", x,x)})
    names(ext_data) <- str_replace_all(names(ext_data), "\\.", " ")
  }
}

#TODO - probably best to have all of these as CSV. Either way, we need consistency






khm_shp <- st_read(paste0(root_dir, "Spatial/cam_prov_merge.shp"), quiet=T)
khm_shp$ADM1_EN[khm_shp$ADM1_EN=="Oddar Meanchey"] <- "Otdar Meanchey" #Temp fix due to disagreement between 50x30 spelling and shapefile.
#Implement fuzzy matching later?