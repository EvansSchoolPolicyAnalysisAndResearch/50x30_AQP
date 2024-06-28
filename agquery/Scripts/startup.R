

#root_dir <- paste0(getwd(), "/")
root_dir <- ""

#LOADING IN AND VALIDATING SPREADSHEETS
#Some of this could probably be cached for faster startup

indicatorCategories <- tryCatch(read.xlsx("Update/indicatorCategories.xlsx"),
                                error=function(e) {
                                  return(F)
                                }) 
if(is.list(indicatorCategories)){
  if(("shortName" %in% names(indicatorCategories)) & ncol(indicatorCategories) > 1) {
    indicatorCategories <- indicatorCategories %>% 
      melt() %>% 
      filter(value==1) %>% 
      select(-value) %>% 
      rename(goalName=variable)
    goalNames <- str_to_title(unique(indicatorCategories$goalName))
  }
}

dataset_list <- list.files("Data", pattern="*.csv")
years <- lapply(dataset_list, FUN=function(x){str_extract(x, "[0-9]{4}")}) %>% unique()
for(year in years){
  names <- lapply(dataset_list[which(str_detect(dataset_list, year))], function(x){
    dat <- read.csv(paste0("Data/",x), nrows=1)
    outdf <- data.frame(shortName=names(dat))
    outdf$file <- str_extract(x, "_([aA-zZ]+).csv", group=1)
    return(outdf)
  })
  names <- do.call("rbind", names) %>% distinct()
  names$year <- year
  #names <- unlist(names) %>% unique()
  if(!exists("indic_inventory")){
    indic_inventory <- names
  } else {
    indic_inventory <- rbind(indic_inventory, names)
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


groups_list <- tryCatch(readxl::read_xlsx("Update/grouping_vars.xlsx"),
                        error=function(e){return(F)})
if(is.list(groups_list)){
  colnm_grps <- c("varName","label","shortName","Levels","Labels","level") #need to fix names here
  if(any(!(colnm_grps %in% names(groups_list)))){
    groups_list <- F
  }
}

policy_path <- tryCatch(read.csv("Update/Policy_Pathways.csv", header = TRUE),
                        error=function(e){return(F)}) #It's possible to pass bad inputs here that will just render as garbage on the table panel; let 'em
if(is.list(policy_path)){
  pathwaysDT <- policy_path %>% select(-c(pathwayID, goalName))
  names(pathwaysDT) <- str_replace_all(names(pathwaysDT), "\\.", " ")
  pathway_names <- unique(policy_path$Policy.Goal)
}
#pathways <- readxl::read_xlsx(paste0(root_dir,"Update/Policy_Pathways.xlsx"))

#TODO - we should not need to do this during runtime
#pathways <- pathways[-c(10:13)]
#colnames(pathways)[8] <- "Related Indicator(s)"

#TODO - probably best to have all of these as CSV. Either way, we need consistency

pathway_link <- tryCatch(read.csv("Update/Policy_Link.csv") %>% distinct(), #Remove duplicates (bad input protection)
                         error=function(e){return(F)})
if(is.list(pathway_link)){
  colnm_link <- c("pathwayID", "goalName","shortName")
  if(any(!(colnm_link %in% names(pathway_link)))){
    pathway_link <- F
  }
}






khm_shp <- st_read(paste0(root_dir, "Spatial/cam_prov_merge.shp"), quiet=T)
khm_shp$ADM1_EN[khm_shp$ADM1_EN=="Oddar Meanchey"] <- "Otdar Meanchey" #Temp fix due to disagreement between 50x30 spelling and shapefile.
#Implement fuzzy matching later?