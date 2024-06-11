options(shiny.error=browser) #For debugging 
library(shiny)
library(shinyBS)
library(tidyr)
#library(shinythemes)
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
library(heatmaply)
library(shinyjs)
library(reshape2)
import::from(spatstat.geom, weighted.median)


dir(path="R", full.names=T) |> map(~ source(.))


###GLOBAL NAMESPACE ITEMS

thematic_shiny(
  font = "auto",
  sequential = colorRampPalette(colors = c("white", "#440154FF"))(12),
  qualitative = c("#440154FF",  "#21908CFF", "#3B528BFF", "#5DC863FF", "#FDE725FF")
)
options(shiny.useragg = TRUE)

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
  } else {
    goalNames <- ""
  }
}

dataset_list <- list.files("Data", pattern="*.csv")
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

#TODO - probably best to have all of these as CSV. Either way, we need consistency

pathway_link <- tryCatch(read.csv("Update/Policy_Link.csv") %>% distinct(), #Remove duplicates (bad input protection)
                         error=function(e){return(F)})
if(is.list(pathway_link)){
  colnm_link <- c("pathwayID", "goalName","shortName")
  if(any(!(colnm_link %in% names(pathway_link)))){
    pathway_link <- F
  }
}


ui <- fluidPage(bg = "white", fg = "#3B528BFF", info="#474481", primary = "#440154FF",
                base_font = bslib::font_google("Open Sans"), 
                fluidRow(column(3, align='center', HTML("<img src=moa_logo.png width='40%'></img>")),
                         column(2, HTML("<h2>CAS Survey Data Explorer</h2>")),
                         column(3, align='center', HTML("<image src=cam_flag.png width='30%'></img>"))),
                #img(src='moa_logo.png', width='10%'),
                navbarPage(title="", theme = bslib::bs_theme(version="3",
                                                             bg = "white", fg = "#3B528BFF", info="#474481", primary = "#440154FF",
                                                             base_font = bslib::font_google("Open Sans")), 
                           tabPanel(title="Introduction",
                                    icon=icon("home"),
                                    intro_ui("intro", pathway_names)),
                           tabPanel(title="Explore Indicators",
                                    icon=icon("magnifying-glass"),
                                    shinyjs::useShinyjs(),
                                    trends_ui("trends")),
                           tabPanel(title="Explore Relationships",
                                    icon=icon("chart-area"),
                                    #comparisons_ui("comps", input, output, year_list, instrument_list)
                                    )
                ))


server <- function(input, output, session){
  trends_server("trends", goalNames, policy_path, indicatorCategories, pathway_link, indicator_list)
  
  #comparisons_server('comps', goalNames)
}


shinyApp(ui = ui, server = server)