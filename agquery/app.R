########################################
# A very special note on development   #
########################################
#This project uses renv to store copies of the necessary packages and make the project exportable/buildable in docker.
#Unfortunately, renv does not like network paths and will fail to find the cache if used on the network drive.
#On top of this, renv will try to reinstall all packages from scratch in a new library if the project is run from a different version of r.
#This makes it pretty inconvenient to have multiple users/machines working on the project at the same time.
#To solve this, renv should be deactivated until it's time to push a new update out to the server. 
#Steps:
#After pushing an update, run renv::deactivate() (should only cause problems if you're working on the R drive version of the folder; downloading the github to your local machine should not result in issues)
#Before pushing a new update, run renv::activate() and renv::snapshot() if the package library has changed.

#ALT note: these should really get updated to imports soon b/c masking is causing issues.
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
#install.packages("spatstat")
# renv::deactivate()

import::from(spatstat.geom, weighted.median)

theme_set(theme_cowplot()) #Graphing theme. 

`%!in%` <- Negate(`%in%`) #I.e., return all unmatched from expression x %in% y; why this is not a core function of R is a real humdinger.

#root_dir <- paste0(getwd(), "/")
root_dir <- ""

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
group_labs <- group_labs[group_labs!="Hidden"]

#Update CROPS files to change the crops
crops <- readxl::read_xlsx(paste0(root_dir,"Update/crops_list.xlsx"))
crops <- crops[order(crops$Order, crops$Crop),] #Custom order
crop_cats <- unique(crops$Category)
list_cropboxes <- sapply(crop_cats, function(x){paste0(x,"_chk")})

list_categories <- lapply(indicator_cats, function(x){paste0(x, "_chk")}) #Not strictly necessary, but here to avoid the possibility of name conflicts. Might remove in future update

#Update this file to change filters
filters_list <- readxl::read_xlsx(paste0(root_dir, "Update/filterset.xlsx"))

#Admin levels, mainly for tagging datasets after manipulation
adm_list <- readxl::read_xlsx(paste0(root_dir, "Update/adm_levels.xlsx"))

#This chunk is for automatically producing a stripped down dataset from the filters spreadsheet
for(i in 1:nrow(instrument_list)){ # for each instrument to be included in AQP
  file_stub <- paste0(root_dir, "Data/", instrument_list$abbr[[i]], "_", instrument_list$wave[[i]]) # file stub in the form file/data/tanzania_w1
  if(!file.exists(paste0(file_stub, "_filterset.dta" ))) { # if we haven't already created a filterset
    hh_file <- tryCatch({read.dta13(paste0(file_stub, "_household_variables.dta"))}, # trycatch = capture
                        error=function(cond){
                          return("")
                        })
    if(!is.data.frame(hh_file)){
      warning(paste0("File for ", instrument_list$country[[i]], " ", instrument_list$wave[[i]], " not found.", type="warning"))
    } else {
      hh_file <- hh_file %>% select(c("hhid", filters_list$var)) 
      save.dta13(hh_file, paste0(file_stub,"_filterset.dta"))
    }
  }
}


# Putting tooltip stuff up here to reduce clutter downstream
ttip1_button <- popify(bsButton("ttip1",label="",icon=icon("question"),style = "inverse", size = "extra-small", block=F), "Using Winsorization","Winsorization controls extreme values by setting all values greater than the 99th percentile to the value of the 99th percentile. In previous verisions of AgQuery, this option was the default.", trigger="hover",placement="right", options = list(container = "body"))
ttip2_button <- popify(bsButton("ttip2",label="",icon=icon("question"),style = "inverse", size = "extra-small", block=F), "Weighting","Weights are initially determined when sampling to approximate the odds of a given household being chosen. Because the weighted household sizes post-survey may not directly correspond to total population, it is possible to use a reweighting strategy here that adjusts weights so that the sum of household size times household weight is either equal to the total population size during the survey year. The third option allows the reweighting values to be adjusted by the urban/rural population ratio determined by the World Bank for the survey year.", trigger="hover", placement="right", options = list(container = "body"))
ttip3_button <- popify(bsButton("ttip3",label="",icon=icon("question"),style = "inverse", size = "extra-small", block=F), "Currency Conversion","This option controls the format of the output: uninflated local currency (LCU), 2016-adjusted LCU, or 2016 purchasing power parity (PPP) USD.", trigger="hover", placement="right", options = list(container = "body"))
ttip4_button <- popify(bsButton("ttip4",label="",icon=icon("question"),style = "inverse", size = "extra-small", block=F), "Disabling Graphs", "If enabled, this options prevents graphs from displaying on the next page, which can reduce processing time and improve performance when large numbers of variables are selected. It will still be possible to download the graphs.", trigger="hover",placement="right", options = list(container = "body"))
ttip5_button <- popify(bsButton("ttip5",label="",icon=icon("question"),style = "inverse", size = "extra-small", block=F), "Grouping Crops by Commodity Type", "If this switch is enabled, crops will be grouped by the commodity classes listed in the central tab panel if crop aggregation is selected. Filtering by crops will restrict the definition of each commodity group to only the crops selected (and empty groups will be omitted)", trigger="hover", placement="right", options=list(container="body"))
ttip6_button <- popify(bsButton("ttip6",label="",icon=icon("exclamation"),style="inverse",size = "extra-small", block=F), "Warning", "The weights in an LSMS-ISA survey are designed to be representative at the national and (in some cases) a regional or zone level. Weighted results at lower levels may not be representative.", trigger="hover",placement="right",options=list(container="body"))
ttip7_button <- popify(bsButton("ttip7",label="",icon=icon("question"),style = "inverse", size = "extra-small", block=F), "Panel Subsample", "This option will filter down to households that appear in all the selected waves for each country for longitudinal analyses. Note that panel refreshes or changes to sampling protocol across years might result in low sample size. Refer to the survey BIDs for more information.", trigger="hover", placement="right", options=list(container="body"))
#ttip8 for crop grouping checkbox.

#Intro strings for reduced clutter, easy editing. 
intros <- list("<b>Step 1:</b> Select a survey. You can select a single box to see one survey's data, or multiple years within a country to see a time series. You can also select multiple countries for cross-country comparison, although note that while we seek to standardize estimates across countries, country preferences and circumstances can lead to differences in survey coverage, administration or instrument design such that cross-country indicator comparisons should be viewed critically. Try selecting Nigeria GHS 2010-2011, the first wave of the Nigeria LSMS survey.",
               "This section contains advanced options that you can use to fine-tune the data that are feeding into AgQuery+ and format the output",
               "Weights are used to ensure that statistics estimated with the sample are unbiased estimates of the population parameters. By default, AgQuery+ uses household-level weights determined from the sampling proababilities calculated by the statistics bureau. You may choose instead to use weights that have been readjusted to match the reported national population (Population-Adjusted Weight) or weights adjusted to match populations in the rural/urban strata. These adjustments account for changes in population that occur between construction of the sampling frame and the survey period and can help smooth over gaps created by households that were unreachable. Finally, you may also omit weights entirely and view the sample means, which might be preferable for aggregation at fine geographic scales (such as LGA or EA), as the weights are designed to be representative only at the national level.",
               "Under currency conversion, you can either view results in the local currency, international purchasing power parity adjustment (an exchange rate differential cost of a basket of goods to also reflect differences in country price levels) based either on national GDP or a private consumption goods basket, both sourced from the World Bank.",
               "Uncheck this box if you would prefer results in nominal rather than real (i.e., inflation-adjusted) terms.",
               "This checkbox will restrict the sample to the survey panel, including only the households that appear in all of the selected surveys in each country. In cases where the panel has been refreshed, the entire sample may be dropped, and you will get an error.",
               "Winsorization sets the upper and lower limits of the sample to (typically) the 99th and 1st percentiles to reduce the effect of outliers. The default option uses EPAR's thresholds, which can have no lower limit or Winsorize to the 95th rather than 99th percentile depending on the variable. If this box is unchecked, the original values will be used, which can result in unrealistically high means for some variables.",
               "Checking this box allows you to override EPAR's Winsorization thresholds.",
               "Use these boxes to set lower and upper thresholds; set 'Lower' to 0 in order to only Winsorize on the right (upper) side of the distribution and 'Upper' to 100 to only Winsorize on the left (lower).",
               "<b>Step 2:</b> Choose up to 10 variables that you'd like to report. You can mix and match variables from different categories. Try choosing 'Total value of crop sold' and 'Hectares planted, household level' under 'Household Crop Production'",
               "<b>Step 4:</b> This column allows you to apply additional restrictions to the dataset; doing so is optional.",
               "Use these buttons to create or delete custom filter criteria. You can restrict the sample by income, crop production, or many other criteria.",
               "Press this button for a full list",
               "Checking this box allows you to group crop-production-related variables by crop or commodity groups (such as staples, roots and tubers, legumes, or fruits and nuts)", 
               "Use these buttons to toggle between grouping the results by individual crops and commodity groups",
               "The list of available crop species is quite long, so it might be beneficial to restrict to only the crops you are interested in. Checking this box allows you to create a filter.",
               "This panel lists all the crops by their commodity class. If the 'Crop Species' option is chosen, then selecting crops will restrict the sample to only the chosen crops. If the 'Commodities' option is chosen, then only the commodity groups containing selected crops will be reported, and only those crops will be considered members of that group (e.g., selecting Barley and Maize under Staples will result in a Staples group aggregating over Barley and Maize but not the rest of the crops in the Staples category). Using these options can make your output considerably neater. If you do not group or filter, results will be aggregated by over all crops. <b>Check the box for 'Rice' before moving to the next step.</b>",
               "<b>Step 3:</b> Here, you can change how the results are aggregated. Leaving this section empty will result in the means at the survey and national level. Choosing a lower geographic level allows results to be mapped. Note that not all grouping criteria are available for all variables (e.g., it's impossible to separate household income by the plot manager's gender) and only the applicable criteria will be applied to each variable. Within-household variables apply to farmed plots, and allow you to sort the results based on the gender of the plot manager, whether multiple crops were grown on the plot, and whether the plot had been measured by GPS, respectively.",
               "Try selecting 'Zone' for geography...",
               "and 'Male/Female Household Head' under 'Household'",
               "Finally, you can click the 'GO' button to get your results! When the program is finished processing the data, you will be immediately taken to the results. <b>Continue the tutorial to see an explanation of advanced options.</b>")


####################### UI: BASIC ########################
ui <- navbarPage(header=tags$head(HTML('
<script async src="https://www.googletagmanager.com/gtag/js?id=G-QY5P3P932P"></script>
<script>
window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag(\'js\', new Date());

  gtag(\'config\', \'G-QY5P3P932P\');
</script>'), 
                                  tags$style(HTML(
                                    '.filter_class .form-group {
  margin-bottom: -15px;
  margin-top: -15px;
  }
      .filter_class .col-sm-5, .col-sm-2, .col-sm-3, .col-sm-4 {
  padding-left: 1px;
  padding-right: 1px;
  }
    .filter_class .selectize-control.single.plugin-selectize-plugin-a11y {
    min-height: 39px;
    }
    .selectize-control.single .selectize-input:after{
                                      content: none;
                                    }
                                    '
                                    
                                  )),
                                  #This block creates a javascript thingy that disables action buttons while calculations are running (from a google groups thread)
                                  singleton(HTML(
                                    '
    <script type="text/javascript">
    $(document).ready(function() {
    
    // disable start_proc button after a click
    Shiny.addCustomMessageHandler("disableButton", function(message) {
    $(".btn-default").attr("disabled", "true");
    });
    
    // Enable start_proc button when computation is finished
    Shiny.addCustomMessageHandler("enableButton", function(message) {
    $(".btn-default").removeAttr("disabled");
    });
})
    </script>
    '
                                  ))), 
                 id="mainpage", title="AgQuery+", 
                 
                 
                 
                 ####################### UI: COL 1: SELECT SURVEY & ADVANCED OPTIONS ########################                 
                 tabPanel("Input", introjsUI(),
                          #fluidRow(HTML('<div style="margin-left: 15px"><h5><i><b>Dev Note:</b> The raw data for each variable are cached to make them available for export. Selecting a large number variables at once can use up available memory and may cause the app to crash.</i></h5></div>')),
                          fluidRow(
                            column(2, tags$div(style="
                            margin-bottom: 15px;
                            margin-left: 15px;
                            margin-right: 15px;
                                     ",
                                               HTML("<h4>Select survey(s)</h4><i><sup>Choose at least 1; choose multiple for time series</sup></i>"),
                                               introBox(uiOutput("Surveys"), data.step=1, data.intro=intros[[1]]),
                                               HTML("<br>"),
                                               introBox(bsCollapse(id="adv_opts", bsCollapsePanel(title="Advanced Options",
                                                                                                  fluidRow(introBox(selectInput("weights",c("Choose Weights",ttip2_button), list("Original Weights", "Total-Population-Adjusted", "Urban/Rural-Population-Adjusted", "No Weight (Sample Means Only)")), data.step=15, data.intro=intros[[3]]),
                                                                                                           introBox(selectInput("ccons", c("Choose a currency conversion",ttip3_button), list("LCU","PPP GDP$","Consumption PPP$")), data.step=16, data.intro=intros[[4]]),
                                                                                                           introBox(checkboxInput("infl", label="Adjust for inflation (2017=1)", TRUE), data.step=17, data.intro=intros[[5]]),
                                                                                                           introBox(checkboxInput("panelcheck", label=c("Restrict to Panel Households", ttip7_button), FALSE), data.step=18, data.intro=intros[[6]]),
                                                                                                           #checkboxInput("graphing_opt",label=c("Suspend Graphing",ttip4_button)),
                                                                                                           introBox(checkboxInput("wins",label=c("Use Winsorization", ttip1_button), TRUE), data.step=19, data.intro=intros[[7]]),
                                                                                                           introBox(checkboxInput("custom_winsor",label="Use custom Winsorization thresholds"), data.step=20, data.intro=intros[[8]])
                                                                                                  ),
                                                                                                  introBox(fluidRow(
                                                                                                    column(2), 
                                                                                                    column(5, 
                                                                                                      numericInput("lower_wins", label=HTML("<sub>Lower</sub>"),value=1,min=0,max=50)
                                                                                                      ),
                                                                                                    column(5, 
                                                                                                           numericInput("upper_wins", label=HTML("<sub>Upper</sub>"), value=99, min=50, max=100)
                                                                                                    ) 
                                                                                                  ), data.step=21, data.intro=intros[[9]]),
                                                                                                  fluidRow(actionButton("adv_opts_reset", "Restore to Defaults"))
                                               )), data.step=14, data.intro=intros[[2]]),
                                               fluidRow(HTML("&nbsp; &nbsp;"), actionButton("tut", "Tutorial", style="background-color: #4272f5;
                                                                                            color: #FFFFFF")),
                                               fluidRow(HTML("&nbsp;&nbsp;<sub>Ver 0.2.0-beta (06.02.23)</sub>")),
                                               fluidRow(conditionalPanel('false',checkboxInput("openPanel", ""), checkboxInput("openPanel2", ""), checkboxInput("onSuccess",""))))),
                            
                            ####################### UI: COL 2: CHOOSE VARIABLES ########################
                            column(3, HTML("<h4>Choose Variable(s)</h4><i><sup>Choose at least 1 and no more than 10</sup></i>"),
                                   introBox(uiOutput("varSelect2"), data.step=2, data.intro=intros[[10]]),
                                   style="padding-right: 35px"
                            ),
                            
              
                            ####################### UI: COL 4: GROUP RESULTS BY ########################
                            column(2,
                                   fluidRow(introBox(HTML("<h4>Group full results by: </h4>"), data.step=3, data.intro=intros[[18]])),
                                   fluidRow(HTML("<b> Administrative Region</b>"), ttip6_button),
                                   fluidRow(HTML("<font size = -1> <i> For spatial visualization, an adminstrative region lower than 'Country' is required </i> </font>")),   
                                   introBox(radioButtons("disAgg_admin", label=NULL, choiceNames=c("Country",
                                                                                                   "Zone (Div 1)", #/Region (TZA)",
                                                                                                   "State (Div 2)", #/Region (ETH)", 
                                                                                                   # "Zone (ETH)/District (TZA)", 
                                                                                                   "LGA (Div 4)", #/Woreda (ETH)", 
                                                                                                   #  "City (ETH)/Ward (TZA)", 
                                                                                                   # "Subcity (ETH)", 
                                                                                                   #"Kebele (ETH)/Village (TZA)", 
                                                                                                   "EA (Enumeration Area)"),
                                                         # choiceValues=c("div0","div1","div2","div3", "div4","div5", "div6", "div7", "div8"), selected="div0"),
                                                         choiceValues=c("div0","div1","div2",  "div4", "div8"), selected="div0"), data.step=4, data.intro=intros[[19]]),
                                   introBox(uiOutput("varGroups"), data.step=5, data.intro=intros[[20]]),
                                   style =  "margin-bottom: 15px;
                                          margin-left: 18px;
                                          padding-right: 45px;"
                                   
                            ),
                            
                            
                            
                            
                            ####################### UI: COL 3: FILTER DATA ########################
                            column(3, 
                                   fluidRow(introBox(HTML("<h4>Subsample the data using filters: </h4>"), data.step=6, data.intro=intros[[11]])),
                                   # Set up help button
                                   fluidRow(HTML("<b>Custom filter definitions</b> <i>(Optional)</i>:")), 
                                   fluidRow(#HTML("<font size = -2> <i> Click 'Add Row' to begin</i></font>&nbsp &nbsp"), 
                                     introBox(bsButton("filterHelp", label="Help", size="extra-small", block="F"), data.step=8, data.intro=intros[[13]])),
                                   fluidRow(tags$div(id="groupboxes", style="
                                       margin-bottom: 15px;
                                       margin-left: 15px;
                                       margin-right: 15px;
                                     "
                                   )),
                                   
                                   # BUTTONS: Add row/remove row/clear all
                                   fluidRow(introBox(actionButton("NewRow", "Add Row"), actionButton("DelRow", "Remove Row"), actionButton("DelAll", "Clear All"), data.step=7, data.intro=intros[[12]])),
                                   
                                   # Crop Grouping/Filtering
                                   fluidRow(HTML("<br><br><b>Crop Grouping/Filtering</b> <i>(Optional)</i>")),
                                   fluidRow(introBox(checkboxInput("cropgroup", label="Group Relevant Variables by Crop/Commodity"), data.step=9, data.intro=intros[[14]])),
                                   
                                   # HS 3/20/23: Is this section the conditional menu below Filter Crops/Commodity Classes?
                                   #fluidRow(column(1), column(2,
                                   fluidRow(
                                     conditionalPanel(condition="input.cropgroup==true", column(11, wellPanel(
                                       fluidRow(column(6, introBox(radioButtons("cropRadio",label="Grouping Type",choiceNames=c("Crop Species", "Commodities"), choiceValues=c("crop","commodity")), data.step=10, data.intro=intros[[15]]))),
                                       fluidRow(column(8, introBox(checkboxInput("cropfilter",label=c("Filter Crops/Commodity Classes", ttip5_button)), data.step=11, data.intro=intros[[16]]))),
                                       fluidRow(column(12, conditionalPanel(condition="input.cropfilter==true",
                                                                            introBox(uiOutput("cropsList"), data.step=12, data.intro=intros[[17]]), 
                                                                            actionButton("cropbutton","Clear Selection"))))
                                     )
                                     )
                                     )
                                   )
                            ),
                            
                            
                            
                            ####################### UI: INPUT, COL 6: GO  ########################
                            column(1,
                                   align = "left",
                                   introBox(actionButton("submitData", "GO",
                                                         style = "color: white;
                                                        background-color: #719177;
                                                        height: 40px;
                                                        width: 65px;
                                                        font-size: 18px"), data.step=13, data.intro=intros[[21]])
                            )
                          ),
                          
                          
                          ####################### UI: TABS (top menu ribbon) ########################
                 ),
                 tabPanel("Output", conditionalPanel(condition="input.onSuccess==true",
                          fluidRow(dataTableOutput("newtable")),
                          fluidRow(HTML("&nbsp;")),
                          fluidRow(HTML("&nbsp;")),
                          fluidRow(column(6,HTML("<b>Indicator Definitions</b><br>"), dataTableOutput("defTable"))),
                          #column(6, HTML("<b>Grouping Variables</b><br>"), dataTableOutput("groupsTable"))),
                          fluidRow(HTML("<b><i><br>Note: Not all variables are available in all countries or waves, and not all grouping criteria apply to all variables. Data that are unavailable have been omitted.</i></b></p>")),
                          fluidRow(downloadButton("downloadData", "Download Summary Stats"), downloadButton("downloadMeta", "Download Metadata"), downloadButton("downloadRaw", "Export Raw Data"))
                 )),
                 
                 tabPanel("Summary Graphs", conditionalPanel(condition="input.onSuccess==true", 
                          fluidRow(column(8, uiOutput("plotsTab")),
                                   column(4, fluidRow(uiOutput("xAxisGraph")),
                                          fluidRow(uiOutput("indicSelectgraph")), 
                                          fluidRow(uiOutput("groupSelectgraph")),
                                          #fluidRow(radioButtons("chartStyle", "Chart Style", choiceNames=c("Line", "Bar", "Box", "Scatter"), choiceValues=c("Line", "Bar", "Box", "Scatter"))),
                                          #ALT: Cutting boxplots for now because they don't work. 
                                          fluidRow(radioButtons("chartStyle", "Chart Style", choiceNames=c("Line", "Bar", "Scatter"), choiceValues=c("Line", "Bar", "Scatter"))),
                                          fluidRow(actionButton("submitGraph", "GO",
                                                                style = "color: white;
                                                                     background-color: #719177;
                                                                     height: 40px;
                                                                    width: 60;
                                                                    font-size: 18px")))),
                          fluidRow(HTML("<i>  Note: You can save charts individually by right clicking and selecting 'Save image as...'</i>")))
                 ),
                 tabPanel("Data Distributions", conditionalPanel(condition="input.onSuccess==true",
                          fluidRow(uiOutput("boxPlots")),
                          fluidRow(uiOutput("densGroups")),
                          fluidRow(HTML("<i>Note: Vertical bar represents the combined sample median</i>")))
                 ),
                 tabPanel("Spatial Visualization",
                          conditionalPanel(condition="input.onSuccess==true", 
                          column(9, uiOutput("mapsDisp")),
                          column(3, radioButtons("mapstotSelect",label="Value to Plot", choiceNames=c("Mean", "Total"), choiceValues=c("mean","total"), selected="mean"), uiOutput("ctrySelect"), uiOutput("indicSelect"), uiOutput("groupSelect"),                                 
                                 actionButton("submitMap", "GO",
                                              style = "color: white;
                                                        background-color: #719177;
                                                        height: 40px;
                                                        width: 60px;
                                                        font-size: 18px")))
                 ),
                 tabPanel("Correlation Tables",
                          plotOutput("corrPlot"),
                          tableOutput("corrTable")),
                 tabPanel("Changelog/Dev Diary",
                          fluidRow(includeHTML(paste0(root_dir, "Update/changelog.html")))
                 )
)





########################################################### SERVER ############################################################
server <- function(input, output, session) {
  
tut_on <<- F
  
observeEvent(input$tut, {
      tut_on <<- T
      shinyBS::updateCollapse(session, "adv_opts", open="Advanced Options")
      shinyBS::updateCollapse(session, "varSelect", open="Household Crop Production")
      introjs(session, events=list(#onbeforechange =readCallback("switchTabs"),
                                     onchange = I(' 
                                      (function(obj){
                                        if (obj._currentStep==9){
                                         Shiny.setInputValue("cropgroup", true, {priority: "event"});
                                         $("#cropgroup").prop("checked", true);
                                        }
                                          if (obj._currentStep==11){
                                          Shiny.setInputValue("cropfilter", true, {priority: "event"});
                                          $("#cropfilter").prop("checked", true);
                                          }
                                          })(this);
                                         ')))
      tut_on <<- F
      })

  #observeEvent(input$openPanel, {
  #  if(input$openPanel==T) {
  #  updateCollapse(session,"adv_opts", open="Advanced Options")
  #  }
  #})
  
  #observeEvent(input$openPanel2, {
  #  if(input$openPanel2==T){
      #updateCollapse(session, "varSelect", open="Household Crop Production")
  #    updateCheckboxGroupInput(session, "Household Crop Production_chk", selected=c("ha_planted_hh", "value_crop_sales"))
  #  }
  #})
  
  
  # n_groups <<- 0
  # Filter data: "add row"
  #ALT: Noodling on replacement for current filter setup
  #observeEvent(input$newGroup, {
  #  insertUI("#groupboxes", tags$div(id=paste0("groupselectors",n_groups), class="filter_class",
  #                                   fluidRow(selectInput(paste0("parentSelect",n_groups), "", choices="any of", "all of"), bsButton()),
  #                                   fluidRow(column(2),
  #                                            column(3, pickerInput(paste0("groupPicker",n_row), label="", choices=filters_list$var, options=list(`live-search`=T))),
  #                                            column(2, selectInput(paste0("groupCriteria",n_row), label="", choices=c("==",">","<"))),
  #                                            column(2, numericInput(paste0("groupNum",n_row), label="", value=0)))))
  #}) 
  #
  #addRow <- function(n_row, parentid){
  #  
  #}
  n_row <<- 0
  observeEvent(input$NewRow, { # Documentation: Use observeEvent whenever you want to perform an action in response to an event.
    n_row <<- n_row+1
    if(n_row==1){
      insertUI("#groupboxes",
               tags$div(id=paste0("groupselectors",n_row), class="filter_class",
                        fluidRow(
                          column(3, pickerInput(paste0("groupPicker",n_row), label="", choices=filters_list$var, options=list(`live-search`=T))),
                          column(2, selectInput(paste0("groupCriteria",n_row), label="", choices=c("==",">","<"))),
                          column(2, numericInput(paste0("groupNum",n_row), label="", value=0))
                        )
               ),
               where="beforeEnd"
      )
    } else {
      insertUI("#groupboxes",
               tags$div(id=paste0("groupselectors",n_row), class="filter_class",
                        fluidRow(
                          column(2, selectInput(paste0("groupConj",n_row), label="", choices=list(and="&", or="|"))),
                          column(3, pickerInput(paste0("groupPicker",n_row), label="", choices=filters_list$var, options=list(`live-search`=T))),
                          column(2, selectInput(paste0("groupCriteria",n_row), label="", choices=c("==",">","<"))),
                          column(2, numericInput(paste0("groupNum",n_row), label="", value=0))
                        )
               ),
               where="beforeEnd"
      )
    }
  })
  
  
  # Response/Warning message (pop up in bottom right corner) for deleting rows (col 3)
  observeEvent(input$DelRow, {
    if(n_row==0) {
      showNotification("Nothing to remove",type="warning")
    } else {
      removeUI(paste0("#groupselectors",n_row))
      n_row <<- n_row-1
    }
  })
  
  # Response/Warning message (pop up in bottom right corner) for deleting rows (col 3)
  observeEvent(input$DelAll, {
    if(n_row==0){
      showNotification("Nothing to remove.", type="warning")
    } else {
      for(k in 1:n_row){
        removeUI(paste0("#groupselectors",k))
      }
      n_row <<-0
    }
  })
  
  ###### Create interactive "Help" pop-up window for Filter Data #####
  observeEvent(input$filterHelp, {
    showModal(modalDialog(title="Setting Custom Filters",
                          HTML("<p>Using the filter option restricts the sample to only those households that meet your specified criterion, such as crop households (crop_hh = 1). As opposed to grouping, which calculates summary statistics for different subgroups using the entire dataset, filtering calculates summary statistics for a subsample of the dataset. Multiple filters can be combined using the 'and' and 'or' dropdowns. Each statement is evaluated sequentially and so order can be important (e.g., filtering based on crop_hh = 1 OR livestock_hh = 1 AND bottom_40_peraeq=1 will not necessarily produce the same subsample as bottom_40_peraeq=1 AND crop_hh=1 OR livestock_hh=1). Refer to the table below to help translate the shorthand variable names and see the range of possible values. </p>"),
                          DT::renderDataTable(datatable(filters_list)),
                          size="l",
                          easyClose=T))
  })
  
  #ALT: Old, to remove
  ###### Create conditional menus for Crop Grouping/Filtering #####
  # Commodities
  #observeEvent(input$cropgroup, {
  #  if(input$cropgroup==T) updateCheckboxInput(session,"commodopts",value=F)
  #})
  
  # Crop Species
  #observeEvent(input$commodopts, {
  #  if(input$commodopts==T) updateCheckboxInput(session,"cropgroup",value=F)
  #})
  
  observeEvent(input$cropgroup, {
    if(input$cropgroup==F) {
      updateCheckboxInput(session, "cropfilter", value=F)
      updateRadioButtons(session, "cropRadio", selected="crop")
      lapply(1:length(list_cropboxes), function(x){updateCheckboxGroupInput(session, list_cropboxes[[x]], selected=character(0))})
      }
  })
  
  ###### CREATING output_data function to calculate summ stats#####
  # Data and stats for outputs    
  output_data <- function(data, by, variable, weight, name){
    outdata <- data %>% group_by(!!!by) %>% summarize(.groups="keep", 
                                                      mean=weighted.mean(!!variable, w={{ weight }}),
                                                      median=weighted.median(!!variable, w={{ weight }}),
                                                      se=sd({{ weight }}*!!variable)/(sum({{ weight }})/length(!!variable))/sqrt(length(!!variable)), 
                                                      n=length(!!variable), 
                                                      total.weight=sum({{weight}}))
    outdata$total = outdata$mean*outdata$total.weight
    outdata$indicator <- name
    outdata <- outdata %>% select(indicator, !!!by, mean, median, total, se, n, total.weight)
    return(outdata)
  }
  
  
  
  
  ######################## UI ELEMENTS THAT MUST BE DYNAMICALLY GENERATED #######################
  # Function to choose variables in Col 4
  output$varSelect2 <- renderUI({
    varCollapse <- lapply(1:length(indicator_cats), function(x) {
      bsCollapsePanel(title=indicator_cats[[x]],
                      checkboxGroupInput(inputId=list_categories[[x]],label="", choiceNames=indicator_list$indicator[(which(indicator_list$indicatorCategory %in% indicator_cats[[x]]))], 
                                         choiceValues=indicator_list$shortName[(which(indicator_list$indicatorCategory %in% indicator_cats[[x]]))],width="90%"))
    })
    do.call(bsCollapse, c(varCollapse, id="varSelect")) %>% return()
  })
  
  # ALT: This is to dynamically generate the "grouping variables" checkboxes in the second column based on the grouping vars spreadsheet.
  output$varGroups <- renderUI({
    groupCheck <- lapply(1:length(group_cats), function(x){
      groupnames <- groups_list[which(groups_list$level %in% group_cats[[x]]),]
      checkboxGroupInput(group_cats[[x]], label=HTML(group_labs[[x]]), choiceNames=groupnames$shortName, choiceValues=groupnames$varName)
    })
  })
  
  # Function to select survey/country/instrument(s) in Col 1
  output$Surveys <- renderUI({
    checkboxnames <- unique(instrument_list$country)
    varCheckboxes <- lapply(1:length(checkboxnames), function(x){
      itemnames <- instrument_list[which(instrument_list$country %in% checkboxnames[[x]]),]
      checkboxGroupInput(checkboxnames[[x]], label=checkboxnames[[x]], choiceNames=with(itemnames, paste(survey, yearlabel)), choiceValues=itemnames$checkbox_names)
    })
  })
  
  # Function to filter by crops (Col 3)
  output$cropsList <- renderUI({
    varCrops <- lapply(1:length(crop_cats), function(x){
      cnames <- crops %>% filter(Category %in% crop_cats[[x]]) %>% select(Crop) %>% unname()
      cvals <- crops %>% filter(Category %in% crop_cats[[x]]) %>% select(Abbr) %>% unname()
      tabPanel(title=crop_cats[[x]],
               checkboxGroupInput(list_cropboxes[[x]],label="",choiceNames=c(cnames[[1]]),choiceValues=c(cvals[[1]])))
    })
    do.call(tabsetPanel,varCrops) %>% return()
  })
  
  # Function to select household characteristics (i.e. mhh/fhh) in Col 2
  #ALT: This generates the dropdown boxes in the user-generated filters in column 3
  output$hhFilterlist <- renderUI({
    checkboxGroupInput("hhFilters",label="Household Characteristics", choiceNames=filters_list$name, choiceValues=filters_list$condition)
  })
  
  # Function to select crops for Filter Crops/Commodity Classes in Col 3
  observeEvent(input$cropbutton, {
    lapply(1:length(list_cropboxes), function(x){updateCheckboxGroupInput(session, list_cropboxes[[x]], selected=character(0))})
  })
  
  
  ################### Start of GET DATA function #################  
  # Submit input options to get data ("Go button") & error messages  
  getData <- eventReactive(input$submitData,{
    session$sendCustomMessage("disableButton", "start_proc")
    output$mapsDisp <- NULL
    output$boxTab <- NULL
    output$boxPlots <- NULL
    output$plotsTab <- NULL
    updateCheckboxInput(session, "onSuccess", value=F)
    output_rawdata <- list() 
    active_instruments <- sapply(unique(instrument_list$country), function(x){input[[x]]}) %>% unlist()
    if(is.null(active_instruments)) {
      showNotification("Please choose at least one survey", type="error")
      return("")
    } else instruments <- instrument_list[match(active_instruments, instrument_list$checkbox_names),]
    varlist <- lapply(list_categories, function(x){
      input[[x]]
    })
    varlist <- unlist(varlist)
    if(length(varlist)==0){
      showNotification("Please select at least one indicator.", type="error")
      return("")
    } else if(length(varlist)>10) {
      showNotification("Too many indicators selected. Please try again with 10 or fewer.", type="error")
      return("")
    }
    
    err_out <- F
    countries <<- unique(instruments$country)
    
    ## Instrument selection ##    
    for(h in 1:length(countries)) {
      instruments_sub <- instruments[instruments$country==countries[[h]],]
      if(input$panelcheck & nrow(instruments_sub > 1)){
        for(i in 1:nrow(instruments_sub)){
          panel_hhs_in <- read.dta13(paste0(root_dir, "Data/",instruments_sub$abbr[[i]],"_",instruments_sub$wave[[i]],"_hhea.dta"))
          if(i==1){
            panel_hhs <- panel_hhs_in
          } else {
            panel_hhs <- panel_hhs[which(panel_hhs$hhid %in% panel_hhs_in$hhid & panel_hhs$ea %in% panel_hhs_in$ea),]
          }
        }
      }
      for(i in 1:nrow(instruments_sub)) {
        if(!err_out){
          adm_level_in <- input$disAgg_admin
          showNotification(paste("Processing", countries[[h]], "instrument", i, "of", nrow(instruments_sub)), duration = 10)
          indicator_files <- unique(indicator_list$file[which(indicator_list$shortName %in% varlist)])
          for(j in 1:length(indicator_files)) {
            if(!exists(paste0(instruments_sub$abbr[[i]],"_",instruments_sub$wave[[i]],"_",indicator_files[[j]]))) {
              assign(paste0(instruments_sub$abbr[[i]],"_",instruments_sub$wave[[i]],"_",indicator_files[[j]]),
                     tryCatch({read.dta13(paste0(root_dir, "Data/",instruments_sub$abbr[[i]],"_",instruments_sub$wave[[i]],"_",indicator_files[[j]], ".dta"))
                     },
                     error=function(cond) {
                       return("")
                     }) #, #Comma needed if line below isn't commented
                     #envir = .GlobalEnv 
                     #ALT 11.17: First stress test suggests that we'll want to conserve RAM, going to disable this for now.
                     #Leave it in, though, b/c uncommenting can help speed up debugging.
              )
            }
            data <- tryCatch({get(paste0(instruments_sub$abbr[[i]],"_",instruments_sub$wave[[i]],"_",indicator_files[[j]]))
            },
            error=function(cond) {
              return("")
            })
            if(!is.data.frame(data)) {
              showNotification(paste("Error: The file", paste0(instruments_sub$abbr[[i]],"_",instruments_sub$wave[[i]],"_",indicator_files[[j]]), "was requested but is missing. Please contact the administrator."), type="error")
              break
            }
            
            ###  GROUPING: ADMINISTRATIVE REGION ###
            adm_sub <- adm_list[which(adm_list$country %in% instruments_sub$abbr[[i]]),]
            adm_flag <- F
            while(!any(adm_sub$div %in% adm_level_in)){
              res <- str_remove(adm_level_in, "div") %>% as.numeric()
              res <- res+1
              adm_level_in <- paste0("div",res)
              adm_flag <- T
            }
            
            adm_num <<- str_remove(adm_level_in, "div") %>% as.numeric() #This makes mapping easier.
            adm_level <<- adm_sub[which(adm_sub$div %in% adm_level_in),]$name #Should be only one element
            if(adm_flag & i==1){ #Second test is just to see if this is the first time we've seen this country to avoid duplicating messages.
              showNotification(paste0("Note: the selected administrative level does not exist in ", instruments_sub$country[[i]], ". Results are grouped by ", adm_level, " instead."), type="warning")
            }
            
            if(n_row > 0) {
              hh_attribs <- tryCatch({read.dta13(paste0(root_dir, "Data/",instruments_sub$abbr[[i]],"_",instruments_sub$wave[[i]],"_filterset.dta"))},
                                     error=function(cond) {
                                       return("")
                                     })
              if(!is.data.frame(hh_attribs)){
                showNotification(paste0("Error: Filter file is missing for ", instruments_sub$country[[i]], " ", instruments_sub$wave[[i]]))
                break
              }
              
              filter_crit <- ""
              
              for(k in 1:n_row) {
                if(n_row==1) {
                  conj <- ""
                } else conj <- input[[paste0("groupConj",k)]]
                #if(k==1 & n_row > 2) filter_crit <- rep("(", n_row-2)
                #if(k > 1 & n_row > 2) closep <- ")" else closep <- ""
                filter_crit <- paste(filter_crit, conj, input[[paste0("groupPicker",k)]], input[[paste0("groupCriteria", k)]], input[[paste0("groupNum",k)]])
                
              }
              hh_attribs <- tryCatch({
                do.call(subset, list(hh_attribs, str2lang(filter_crit)))
              },
              error=function(cond){
                return("")
              })
              if(!is.data.frame(hh_attribs)){
                showNotification("Error: at least one filter criterion was not found. Try deleting the filterset files and reloading the app.", type="warning")
                err_out <- T #This is here to prevent errors from triggering on multiple files and skipping the rest of the loop entirely, as opposed to "exit", which just records whether a dataset generation operation failed but continues with the rest of the selected sets.
                break
              } else if(nrow(hh_attribs)==0) {
                showNotification("Error: selected filters excluded the entire dataset. Please revise your filters and retry.", type="error")
                err_out <- T
                break
              } else data <- data[data$hhid %in% hh_attribs$hhid,]
            }                        
            
            
            ## FILTERING: CROPS (crop_name)
            if(with(data,exists("crop_name")) & input$cropfilter) {
              active_Cropnames <- lapply(1:length(list_cropboxes), function(x){input[[list_cropboxes[[x]]]]}) %>% unlist()
              if(length(active_Cropnames)>0) {
                test_crops <- data %>% filter(crop_name %in% active_Cropnames)
                if(nrow(test_crops)==0) {
                  showNotification(paste0(
                    "Warning: Selected crops were not present in ", toupper(instruments_sub$abbr[[i]]), " ", instruments_sub$year[[i]],  "; no filters were applied."), type="warning")
                } else (data <- test_crops) #Replace data with filtered dataset.
              } else {
                showNotification("Warning: No crops were selected, so no crop filters were applied.", type="warning")
              }
            }
            
            
            ## FILTERING: COMMODITY CLASSES ##   
            #This reaggregates crops along commodity classes.
            if(with(data, exists("crop_name")) & input$cropgroup & input$cropRadio=="commodity") {
              data$crop_name <- merge(data, crops, by.x="crop_name", by.y="Abbr", all.x=T, sort=F)[["Category"]]
            }
            varlist_sub <- names(data)[names(data) %in% varlist] #Filter out missing variables
            if(length(varlist_sub)==0) {
              exit <- T
              break
            }
            colindex <- which(names(data) %in% varlist_sub)
            if(length(colindex)==0) { 
              exit <- T
              break
            } else exit <- F
            all_adm <- adm_sub[which(adm_sub$name %in% names(data)),]$name #Problem: Not all admin levels (i.e., plot id) are available in all files. 
            aggs_list <- lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}) %>% unlist()
            if(input$cropgroup) aggs_list <- c(aggs_list, "crop_name")
            aggs_list <- names(data)[names(data) %in% aggs_list] #Filtering out irrelevant modifiers
            
            
            ##### OUTPUT DATA #####  
            for(x in 1:length(varlist_sub)) {
              #update 10/20: Updating the code to ensure the correct weights are being used.
              tempdata <- data
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
              
              #if(!exists("sourcefile")){
              #  sourcefile <- data.frame(var = currVar,
              #                           source = indicator_files[j])
              #} else{
              #  if(currVar %!in% sourcefile$var){
              #    tempsource <- data.frame(var = currVar,
              #                             source = indicator_files[j])
              #    sourcefile <- bind_rows(sourcefile, tempsource)
              #    rm(tempsource)
              #  }
              #} # closes else loop
            } # closes for loop that begins OUTPUT DATA section 
            
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
      #ALT: Trying to improve the display of the total column; issue is this has to be on a per-indicator basis and that's going to get messy.
      #if(max(return_data2$total) > 1000000000) {
      #  return_data2$total <- signif(return_data2$total/1000000000, 4)
      #  total_suffix <<- "(BN)"
      #} else if(max(return_data2$total) > 1000000) {
      #  return_data2$total <- signif(return_data2$total/1000000, 4)
      #  total_suffix <<- "(MM)"
      #} else {
      #  return_data2$total <- round(return_data2$total,0)
      #  total_suffix <<- ""
      #}
      return_data2$total <- round(return_data2$total, 0)
      #return_data2$total[which(return_data2$units %!in% c("count", "ratio"))] <- NA #Remove totals that aren't meaningful
      return_data2$total[which(return_data2$units %in% "ratio")] <- NA #Ironically some ratios aren't meaningful; note to fix. 
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
  #################### END OF getDATA ###########################    
  
  
  
  ####################### Data distribution plotting (Tab 4) #######################
  observeEvent(input$submitData, {
    #if(!input$graphing_opt) {
      raw_data <- tryCatch({getData()$raw_data},
                           error=function(cond){
                             return("")
                           })
      if(is.list(raw_data)){
        namelist <- names(raw_data)
        varBoxes <- lapply(1:length(raw_data), function(x){
          active_name <- namelist[[x]] #Makes following code easier to read.
          plotdata <- raw_data[[x]]
          data_unit <- indicator_list$units[[which(indicator_list$shortName %in% active_name)]]
          if(str_detect(data_unit, "currency")) {
            data_unit <- str_replace(data_unit, "currency", input$ccons)
            #######################            
            labelval <- list(labels=scales::label_dollar(prefix=''))
          } else labelval <- list(breaks=scales::breaks_extended())
          xlabel <- paste0(indicator_list$prettyName[[which(indicator_list$shortName %in% active_name)]], " (", data_unit, ")") 
          
          
          if(max(na.omit(plotdata[[active_name]]))-min(na.omit(plotdata[[active_name]][plotdata[[active_name]]>0])) > 100000) {
            plotdata[[active_name]] <- plotdata[[active_name]]+1 #Simple transform to keep 0s from becoming NAs
            #            scale_x_continuous(trans='log', breaks=scales::breaks_log(n=8, base=10), label=scales::label_comma(accuracy=1, n=8))
            labelval <- list(trans='log', breaks=scales::breaks_log(n=6, base=10), label=scales::label_comma(accuracy=1, n=6)) #ALT: Dropping this down to max at 1 mil
            xlabel <- paste0(xlabel, " - Log Transformed")
          }
          aggs_list <- unlist(lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}))
          if(input$cropgroup) aggs_list <- c(aggs_list, "crop_name")
          plotdata <- plotdata %>% discard(~all(is.na(.)))  # "." here is a standin for the dataframe (plotdata) 
          aggs_list <- aggs_list[which(aggs_list %in% names(plotdata))]
          if(length(aggs_list)==0) {
            ggplot(plotdata, aes(x=!!sym(active_name)))+
              geom_density(alpha=0.4, fill="red")+
              geom_vline(aes(xintercept=val), plotdata %>% group_by(Country, Wave) %>% summarize(val=median(!!sym(active_name))))+
              facet_grid(vars(Country), vars(Wave))+
              do.call(scale_x_continuous, labelval)+
              labs(x=xlabel)
          } else if(length(levels(interaction(plotdata[aggs_list]))) > 8){
            ggplot(plotdata, aes(x=!!sym(active_name)))+
              geom_density(alpha=0.4, fill="red")+
              geom_vline(aes(xintercept=val), plotdata %>% group_by(Country, Wave) %>% summarize(val=median(!!sym(active_name))))+
              facet_grid(vars(Country), vars(Wave))+
              do.call(scale_x_continuous, labelval)+
              labs(x=xlabel, caption="Too many combinations of groups; only the overall distribution is shown")
          } else {
            ggplot(plotdata, aes(x=!!sym(active_name), fill=interaction(!!!syms(aggs_list))))+
              geom_density(alpha=0.4)+
              geom_vline(aes(xintercept=val), plotdata %>% group_by(Country, Wave) %>% summarize(val=median(!!sym(active_name))))+
              facet_grid(vars(Country), vars(Wave))+
              do.call(scale_x_continuous, labelval)+
              scale_fill_discrete(name=paste(aggs_list, collapse=", "))+
              labs(x=xlabel)
          }
        })
        boxTab <- lapply(1:length(varBoxes), function(x){
          tabPanel(title=names(raw_data)[[x]],
                   renderPlot(varBoxes[[x]]))
        })
        output$boxPlots <- renderUI({
          do.call(tabsetPanel,boxTab) %>% return()
        })
        #output$densGroups <- renderUI(radioGroupButtons("groupButtons", "Select Grouping Variable(s)", choiceNames=c("None", ), choiceValues=c("None", groupsDefs$`Variable Label`)))
     
          }
    #}
  })
  
  #######NEW CORRELATION TABLES CODE#######
  #Problems we need to solve: observations at different levels (how do we deal with crop disaggregation?) 
  #Dealing with year for when user selects multiple waves
  #Add check here so we don't have to deal with multiple countries (not an issue for 50x30 because each app will be for a single country)
  #Workflow: get everything summarized to the household level if it isn't currently at the household level. - mean for 1/0, total for everything else? 
    #For sub-household variables, go wide by crops (this could produce a lot of variables!)
    #Weighted means for things with denominators?
  
  rawdat_names <- names(getData()$raw_data)
  
 
  for(i in 1:length(rawdat_names)){
    dat <- getData()$raw_data[[i]]
    sourcefile <- indicator_list$file[which(indicator_list$shortName %in% rawdat_names[[i]])]
    
    aggs_list <- unlist(lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}))
    if(input$cropgroup) aggs_list <- c(aggs_list, "crop_name")
    aggs_list <- aggs_list[which(aggs_list %in% names(dat))]
    if(length(aggs_list)>0) if(any(is.na(aggs_list))) {
      aggs_list <- character(0)
    }
    if(sourcefile=="hh_vars"){
      if(j==1){
        hh_vars <- dat %>% select(-c(geography, abbr)) %>% relocate(!!sym(rawdat_names[[i]]), .after=last_col())
        j = j+1
      } else {
        hh_vars <- merge(hh_vars, dat[c("hhid", "Wave", rawdat_names[[i]], "ea", aggs_list)], by=c("hhid", "ea", "Wave", aggs_list))
      }
    } else if(sourcefile=="hh-crop_vars"){
      if(k==1){
        hh_crop_vars <- dat %>% select(-c(geography, abbr)) %>% relocate(!!sym(rawdat_names[[i]]), .after=last_col())
        k = k+1
      } else {
        hh_crop_vars <- merge(hh_crop_vars, dat[c("hhid", "Wave", rawdat_names[[i]], "ea", aggs_list)], by=c("hhid", "ea", "Wave", aggs_list))
      }
    } else if(sourcefile=="pers_vars"){
      if(l==1){
        pers_vars <- dat %>% select(-c(geography, abbr)) %>% relocate(!!sym(rawdat_names[[i]]), .after=last_col())
        l = l+1
      } else {
        pers_vars <- merge(hh_crop_vars, dat[c("hhid", "Wave", rawdat_names[[i]], "ea", "indiv", aggs_list)], by=c("hhid", "ea", "Wave", "indiv", aggs_list))
      }
    } else if(sourcefile=="plot_vars") {
      if(m==1){
        plot_vars <- dat %>% select(-c(geography, abbr)) %>% relocate(!!sym(rawdat_names[[i]]), .after=last_col())
        m = m+1
      } else {
        plot_vars <- merge(plot_vars, dat[c("hhid", "Wave", rawdat_names[[i]], "ea", "plot_id", aggs_list)], by=c("hhid", "ea", "Wave", "plot_id", aggs_list))
      }
    }
  
  ####################### POST-PROCESSING FOR OUTPUT TAB, GENERATING GRAPHING UI ELEMENTS ####################### 
  observeEvent(input$submitData, {
    dataset <- tryCatch({getData()$output},
                        error=function(cond){
                          return("")
                        })
    if(is.data.frame(dataset)) {
      #I do a little cleaning here so that the charts can still use the abbreviations.
      if(with(dataset, exists("geography"))) dataset <- dataset %>% select(-geography)
      #dataset <- dataset %>% select(!c(conv, wave, abbr, total.weight)) %>% relocate(Country, Year, indicator, units) #Exclude conversion factors from output
      dataset <- dataset %>% select(!c(wave, abbr, total.weight)) %>% relocate(Country, Year, indicator, units)
      dataset <- dataset %>% relocate(mean, total, se, n, .after=last_col())
      dataset[dataset$units %in% "Currency"] <- input$ccons 
      groupsDefs <- groups_list[which(groups_list$varName %in% names(dataset)),] %>% select(`Variable Label`=varName, `Abbreviated Name`=shortName, Definition=label, Notes=definition)
      names(dataset) <- sapply(names(dataset), function(x){if((x %in% groups_list$varName)!=0) groups_list$shortName[groups_list$varName %in% x] else x})
      names(dataset) <- names(dataset) %>% str_replace_all(., "_", " ") %>% str_to_title(.)
      dataset$Se <- ifelse(is.na(dataset$Se), "NA", as.character(dataset$Se))
      names(dataset) <- ifelse(grepl("Lga", names(dataset)) | grepl("Ea", names(dataset)), toupper(names(dataset)), names(dataset))
      #names(dataset)[which(names(dataset)=="Total")] <- paste("Total", total_suffix)
      downloadTable <<- dataset #Add to global environment so it is available to download handler.
      output$newtable <- DT::renderDataTable(datatable(dataset, rownames=F, filter="top"))
      definitionTab <- indicator_list %>% subset(shortName %in% unique(dataset$Indicator)) %>% select(shortName, indicator, notes)
      names(definitionTab) <- c("Abbreviated Name", "Definition", "Notes")
      definitionTabExp <<- bind_rows(definitionTab, groupsDefs) #To global env 
      output$defTable <- DT::renderDataTable(datatable(definitionTab, rownames=F, options=list(paging=F, searching=F), width="50%", class="compact"))            
      output$groupsTable <- DT::renderDataTable(datatable(groupsDefs, rownames=F, options=list(paging=F, searching=F), width="50%", class="compact"))
      ### Mapping
      output$ctrySelect <- renderUI(radioButtons("ctryButtons", "Select Country", choices=unique(dataset$Country)))
      #output$indicSelect <- renderUI(checkboxGroupInput("indicButtons","Select Indicator", choices=unique(dataset$Indicator)))
      #ALT: I think we can only handle one indicator at a time.
      output$indicSelect <- renderUI(radioButtons("indicButtons","Select Indicator", choices=unique(dataset$Indicator)))
      output$groupSelect <- renderUI(radioButtons("groupButtons", "Select Grouping Variable", choiceNames=c("None",groupsDefs$`Abbreviated Name`), choiceValues=c("None", groupsDefs$`Variable Label`)))
      #output$groupSelect <- renderUI(checkboxGroupInput("groupButtons", "Select Grouping Variable", choiceNames=c(groupsDefs$`Abbreviated Name`), choiceValues=c( groupsDefs$`Variable Label`)))
      #### New graphing section
      #output$groupSelectgraph <- renderUI(checkboxGroupButtons("groupGraph", "Select Grouping Variable(s)", choiceNames=c("None", "Year", groupsDefs$`Abbreviated Name`, "Geography"), choiceValues=c("None", "Year", groupsDefs$`Variable Label`, adm_level))) 
      #output$xAxisGraph <- renderUI(radioGroupButtons("xAxisLabs", "X Axis Variable", choices=c("Geography", "Year", unique(dataset$Indicator))))
      #output$indicSelectgraph <- renderUI(checkboxGroupButtons("indicGraph","Select Indicator(s)", choices=c("Clear",unique(dataset$Indicator))))
      
      output$groupSelectgraph <- renderUI(checkboxGroupInput(inputId = "groupGraph",
                                                             label = "Select Grouping Variable(s)",
                                                             choiceNames=c("Year", groupsDefs$`Abbreviated Name`, ifelse(adm_level %in% c("ea", "lga"), toupper(adm_level), str_to_title(adm_level))),
                                                             choiceValues=c("Year", groupsDefs$`Variable Label`, adm_level))) 
                                                             #choiceNames=c("Year", groupsDefs$`Abbreviated Name`, str_to_title(adm_level)), #Updated "Geography" to adm_level 
                                                             #choiceValues=c("Year", groupsDefs$`Variable Label`, adm_level))) 
      #output$xAxisGraph <- renderUI(radioButtons("xAxisLabs", "X Axis Variable", choiceNames=c(str_to_title(adm_level), "Year", unique(dataset$Indicator)), choiceValues=c("Geography", "Year", unique(dataset$Indicator))))
      output$xAxisGraph <- renderUI(radioButtons("xAxisLabs", "X Axis Variable", choiceNames=c(ifelse(adm_level %in% c("ea", "lga"), toupper(adm_level), str_to_title(adm_level)), "Year", unique(dataset$Indicator)), choiceValues=c("Geography", "Year", unique(dataset$Indicator))))
      output$indicSelectgraph <- renderUI(checkboxGroupInput("indicGraph", label = HTML("Y axis variable(s) <i><font style='font-weight:normal'>(min. 1)</font></i>"), choices=c(unique(dataset$Indicator)))) # HKS: would like to unbold "(min. 1)" but cannot solve (see fluidRow and HTML attempts); come back to later
      showNotification("Done!")
      updateNavbarPage(session, "mainpage", selected="Output")
    }
    session$sendCustomMessage("enableButton", "start_proc")
  })
  
  observeEvent(input$groupGraph, {
    if(any(input$groupGraph %in% "None")) updateCheckboxGroupButtons(session, "groupGraph", selected=character(0))
  })
  
  observeEvent(input$indicGraph, {
    if(any(input$indicGraph %in% "Clear")) updateCheckboxGroupButtons(session, "indicGraph", selected=character(0))
  })
  
  
  
  
  
  ####################### Graphing  ########################
  observeEvent(input$submitGraph, {
    if(length(input$indicGraph)==0) {
      showNotification("Select at least one indicator", type="error")
    } else {
      dataset <- tryCatch({getData()$output},
                          error=function(cond){
                            return("")
                          })
      if(is.data.frame(dataset)) {
        exit <- F
        output$plotsTab <- NULL
        xGroups <- NULL
        if(input$xAxisLabs=="Geography") {
          #if(length(input$groupGraph %in% adm_level)!=0) 
          if(any(input$groupGraph %in% adm_level)) {
            showNotification("Error: Geography cannot be both a grouping variable and an axis variable. Switch to time axis or remove geography as a grouping variable and try again", type="error")
            exit <- T
          } else if(adm_level=="Country" & length(unique(dataset$Country))==1) {
            showNotification("There are too few countries in dataset for this graphing option", type="Error")
            exit <- T
          } else if(length(countries)>1){
            xAxis <- "interaction(Country, !!sym(adm_level))" 
            xAxisLabel = paste0("Country, ", adm_level)
            xGroups <- c("Country", adm_level)
          } else {
            xAxis <- "!!sym(adm_level)"
            xAxisLabel <- str_to_title(adm_level)
            xGroups <- adm_level
          }
        } else if(input$xAxisLabs=="Year"){
          if(any(input$groupGraph %in% "Year")) {
            showNotification("Error: Time cannot be both a grouping variable and an axis variable. Switch to another axis or remove year as a grouping variable.")
            exit <- T
          } else {
            xAxis <- "Year"
            xAxisLabel <- "Year"
            xGroups <- "Year"
          }
        } else {
          xAxis <- input$xAxisLabs
          xGroups <- NULL
          xUnits <- indicator_list$units[indicator_list$shortName %in% input$xAxisLabs]
          if(str_detect(xUnits, "currency")) xUnits <- str_replace(xUnits, "currency", input$ccons)
          xAxisLabel <- paste0(indicator_list$prettyName[indicator_list$shortName %in% input$xAxisLabs], " (", xUnits, ")")
        }
        if(!exit){
          #ALT Likely no longer necessary as of 6/5 updates.
          #if(length(input$groupGraph) > 0){
          #  for(j in 1:length(input$groupGraph)){
          #    dataset[[input$groupGraph[[j]]]] <- factor(dataset[[input$groupGraph[[j]]]])
          #  }
          #}
          chartType <- switch(input$chartStyle,
                              Line = "geom_line(",
                              Scatter = "geom_point(",
                              Bar = "geom_col(position=position_dodge(), color='black', ",
                              Box = "geom_boxplot(")
          grobsList <- list()
          for(i in 1:length(input$indicGraph)) {
            indic <- input$indicGraph[[i]]
            yUnits <- indicator_list$units[indicator_list$shortName %in% indic]
            if(str_detect(yUnits, "currency")) yUnits <- str_replace(yUnits, "currency", input$ccons)
            yAxisLabel <- paste0(indicator_list$prettyName[indicator_list$shortName %in% indic], " (",  yUnits, ")")
            if(length(input$groupGraph %in% adm_level)!=0) if(any(input$groupGraph %in% adm_level)) geoLegend <- str_to_title(adm_level) else geoLegend <- NULL
            if(length(input$groupGraph) > 1){
              if(input$chartStyle=="Box") {
                intstr <- paste0("interaction(", paste(input$groupGraph, collapse=", "), ", ", paste(xGroups, collapse=", "),")")
              } else {
                intstr <- paste0("interaction(", paste(input$groupGraph, collapse=", "),")")
              }
              if(input$chartStyle=="Scatter" | input$chartStyle == "Line") { 
                colorStr <- paste0(", color=", intstr) 
                legendStr <- paste0(", color='",paste(c(geoLegend, groups_list$shortName[which(groups_list$varName %in% input$groupGraph)]), collapse=", "),"'")
              } else {
                colorStr <- paste0(", fill=", intstr)
                legendStr <- paste0(", fill='",paste(c(geoLegend, groups_list$shortName[which(groups_list$varName %in% input$groupGraph)]), collapse=", "), "'")
              }
              groupStr <- paste0(", group=",intstr)
              if(input$chartStyle %in% c("Bar","Line")) {
                # To eliminate the disaggregation options that the users didn't select  
                data_out <- dataset %>% 
                  group_by(indicator, !!!syms(c(input$groupGraph, xGroups))) %>% 
                  summarize(mean=weighted.mean(mean, w=total.weight))
                collapse_groups <- names(data_out)[which(names(data_out) %!in% c("indicator", "mean"))]
                data_out <- data_out %>% pivot_wider(id_cols=all_of(collapse_groups), names_from=indicator, values_from=mean)
                assign(paste0("chartdata_", i),  data_out)
              } else {
                collapse_groups <- names(dataset)[which(names(dataset) %!in% c("indicator", "mean", "se", "n", "median", "total", "conv", "units", "total.weight"))]
                assign(paste0("chartdata_", i), dataset %>% filter(!!sym(adm_level)!="National") %>% pivot_wider(id_cols=all_of(collapse_groups), names_from=indicator, values_from=mean))
              }
            } else if(length(input$groupGraph)==1){
              if(input$chartStyle=="Box") {
                groupStr <- paste0(", group=interaction(", paste(xGroups, input$groupGraph, collapse=", "), ")")
              }
              groupStr <- paste0(", group=", input$groupGraph)
              if(input$chartStyle=="Scatter" | input$chartStyle == "Line") {
                colorStr <- paste0(", color=", input$groupGraph)
                if(!is.null(geoLegend)) legendStr <- paste0(", color='", geoLegend, "'") else legendStr <- paste0(", color='", groups_list$shortName[which(groups_list$varName %in% input$groupGraph)], "'")
              } else {
                colorStr <- paste0(", fill=", input$groupGraph)
                if(!is.null(geoLegend)) legendStr <- paste0(", color='", geoLegend, "'") else legendStr <- paste0(", fill='", groups_list$shortName[which(groups_list$varName==input$groupGraph)],"'")
              }
              if(input$chartStyle %in% c("Bar","Line")) {
                data_out <- dataset %>% 
                  group_by(indicator, !!!syms(c(input$groupGraph, xGroups))) %>% 
                  summarize(mean=weighted.mean(mean, w=total.weight))
                collapse_groups <- names(data_out)[which(names(data_out) %!in% c("indicator", "mean"))]
                data_out <- data_out %>% pivot_wider(id_cols=all_of(collapse_groups), names_from=indicator, values_from=mean)
                assign(paste0("chartdata_", i),  data_out)
              } else { 
                collapse_groups <- names(dataset)[which(names(dataset) %!in% c("indicator", "mean", "se", "n", "median", "total", "conv", "units", "total.weight"))]
                assign(paste0("chartdata_", i), dataset %>% filter(!!sym(adm_level)!="National") %>% pivot_wider(id_cols=all_of(collapse_groups), names_from=indicator, values_from=mean))
              }
            } else {
              groupStr <- ", group=1"
              colorStr <- ""
              legendStr <- ""
              
              if(input$chartStyle %in% c("Bar","Line") & !is.null(xGroups)){
                data_out <- dataset %>% 
                  group_by(indicator, !!!syms(xGroups)) %>% 
                  summarize(mean=weighted.mean(mean, w=total.weight))
                collapse_groups <- names(data_out)[which(names(data_out) %!in% c("indicator", "mean"))]
                data_out <- data_out %>% pivot_wider(id_cols=all_of(collapse_groups), names_from=indicator, values_from=mean)
                assign(paste0("chartdata_", i), data_out)
              } else {
                collapse_groups <- names(dataset)[which(names(dataset) %!in% c("indicator", "mean", "se", "n", "median", "total", "conv", "units", "total.weight"))]
                assign(paste0("chartdata_", i), dataset %>% filter(!!sym(adm_level)!="National") %>% pivot_wider(id_cols=all_of(collapse_groups), names_from=indicator, values_from=mean))
              }
            }
            if(input$xAxisLabs=="Geography") {
              #axisStr <- "+ theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))"
              axisStr <- "+ theme(axis.text.x = element_text(angle = 45, hjust=1))"
            } else {
              axisStr <- ""
            }
            
            chartStr <- paste0("ggplot()+", chartType, "data=get('chartdata_", i, "'), aes(x=", xAxis, ", y=", indic, groupStr, colorStr, "))",
                               "+labs(x='\n", xAxisLabel, "', y='", yAxisLabel, "'", legendStr, "\n)", axisStr)
            assign(paste0("chart", i), eval(parse_expr(chartStr)))
            grobsList <- c(grobsList, paste0("chart", i))
          }
          output$plotsTab <- renderUI({
            lapply(1:length(grobsList), function(x){
              plotOutput(outputId = grobsList[[x]])
              output[[grobsList[[x]]]] <- renderPlot(get(grobsList[[x]]))
            })
          })
        }
      } else showNotification("Error: No Dataset Found. Run a query on the Input tab before making graphs.", type="error")
    }
  })
  
  
  
  
  ####################### MAPPING SECTION #########################
  observeEvent(input$submitMap, {
    dataset <- tryCatch({getData()$output},
                        error=function(cond){
                          return("")
                        })
    
    # Two styles of graphics - one for EAs & LGAs, the other for zones and states etc.
    if(is.data.frame(dataset)){
      if(adm_level=="geography") {
        showNotification("The data need to be grouped at a subnational level for this feature to be informative.", type="error") 
      } else {
        showNotification("Working, please wait...")
        #updateActionButton(session, "mapButton", enabled=F)
        plotlist <- list()
        plottestlist <- list()
        titlelist <- list()
        
        if(!(input$groupButtons %in% "None")) grouplabels <- syms(c(adm_level, input$groupButtons)) else grouplabels <- sym(adm_level)
        
        if(length(unique(filter(dataset, indicator %in% input$indicButtons)[[input$groupButtons]])) > 5) {
          showNotification("We're still working on displaying that many groups at the same time. Try again with a grouping variable that has fewer categories.", type="error")
        } else {
          dataset <- dataset %>% filter(!!sym(adm_level) != "National")
          if(input$mapstotSelect=="mean") {
            l_bound <- with(dataset %>% dplyr::filter(Country %in% input$ctryButtons, indicator %in% input$indicButtons) %>% group_by(!!!grouplabels, Year) %>% summarize(mean=weighted.mean(mean, w=total.weight)), min(mean))
            u_bound <- with(dataset %>% dplyr::filter(Country %in% input$ctryButtons, indicator %in% input$indicButtons) %>% group_by(!!!grouplabels, Year) %>% summarize(mean=weighted.mean(mean, w=total.weight)), max(mean))
          } else {
            l_bound <- with(dataset %>% dplyr::filter(Country %in% input$ctryButtons, indicator %in% input$indicButtons) %>% group_by(!!!grouplabels, Year) %>% summarize(total=sum(total)), min(total))
            u_bound <- with(dataset %>% dplyr::filter(Country %in% input$ctryButtons, indicator %in% input$indicButtons) %>% group_by(!!!grouplabels, Year) %>% summarize(total=sum(total)), max(total))
          }
          
          if(u_bound > 1e+9){
            dataset[[input$mapstotSelect]] <- dataset[[input$mapstotSelect]]/1e+9
            u_bound <- u_bound/1e+9
            l_bound <- l_bound/1e+9
            if(l_bound < 1e-2 & l_bound > 0) l_bound <- 0 #pretty sure ggplot does this automatically but it can't hurt
            total_suffix <- "(BN)"
          } else if(u_bound > 1e+6) {
            dataset[[input$mapstotSelect]] <- dataset[[input$mapstotSelect]]/1e+6
            u_bound <- u_bound/1e+6
            l_bound <- l_bound/1e+6
            if(l_bound > 0 & l_bound < 1e-2) l_bound <- 0
            total_suffix <- "(MM)"
          } else if(u_bound > 1e+3) {
            dataset[[input$mapstotSelect]] <- dataset[[input$mapstotSelect]]/1e+3
            u_bound <- u_bound/1e+3
            l_bound <- l_bound/1e+3
            if(l_bound < 1e-2 & l_bound > 0) l_bound <- 0
            total_suffix <- "(Thou)"
            } else total_suffix <- ""
          map_units = indicator_list$units[indicator_list$shortName %in% input$indicButtons]
          if(str_detect(map_units, "currency")) map_units <- str_replace(map_units, "currency", input$ccons)
          if(input$mapstotSelect=="total" & str_detect(map_units, "/household")) {
            map_units <- str_replace(map_units, "/household", "")
          }
          map_units <- paste(map_units, total_suffix)
          
          ### years = a small dataset of unique years for which instruments exist 
          years <- unique(dataset$Year)
          years <- years[sort.list(years)]
          if(adm_num > 3) {
            ctryname <- paste0(input$ctryButtons, "_", "div2") #Default to state for point data. TZA does not have a level 2, so the shapefiles labeled as level 2 in the folder are actually region-level (level 1)
            if(!exists(ctryname)) { #Saves time if a shapefile has already been loaded for a previous map request
              assign(ctryname, st_read(paste0(root_dir,"Spatial/", ctryname, ".shp")), envir=.GlobalEnv) #Just use state for point stuff
            }
            
            ### Get coords from div_coords.dta files
            for(i in 1:length(years)){
              coords <- read.dta13(paste0("Data/", input$ctryButtons, "_", dataset$wave[[i]], "_", paste0("div",adm_num), "_coords.dta")) #lga/ea id, lat/lon
              coords[[adm_level]] <- coords[[adm_level]] %>% str_to_lower() %>% str_to_title()
              geodata <- dataset %>% dplyr::filter(Country %in% input$ctryButtons, indicator %in% input$indicButtons, Year %in% years[[i]])
              if(input$mapstotSelect=="mean"){
                geodata <- geodata %>% group_by(!!!grouplabels) %>% summarize(value=weighted.mean(mean, w=total.weight))
              } else {
                geodata <- geodata %>% group_by(!!!grouplabels) %>% summarize(value=sum(total))
              }
              ### Get Shapefile
              #shp <- merge(get(ctryname),geodata, by=adm_level, all.x=T) %>% st_as_sf() #Make sure the shapefiles have the same names
              shp <- get(ctryname) %>% st_as_sf()
              
              ### Merge shapefiles by adm_level
              
              if((input$groupButtons %in% "None")) {
                #This lays some minor groundwork for multiple group selections.
                geodata <- merge(geodata, coords, by=adm_level)
                
                
                assign(paste0("outplot", i), ggplot()+
                         geom_sf(data=shp)+
                         geom_point(data=geodata, aes(x=LON, y=LAT, color=value))+
                         scale_color_continuous(name=map_units, limits=c(u_bound, l_bound), trans='reverse')+
                         theme_map()+
                         ggtitle(label=paste0(input$ctryButtons, " ", years[[i]], " ",indicator_list$prettyName[indicator_list$shortName %in% input$indicButtons]))
                )      
                
                plotlist <- c(plotlist, paste0("outplot",i))
              } else {
                group_levels <- unique(dataset[[input$groupButtons]])
                groupslist <- list()
                
                for(j in 1:length(group_levels)){
                  if(input$mapstotSelect=="mean") {
                    geodata2 <- dataset %>% dplyr::filter(Country %in% input$ctryButtons, indicator %in% input$indicButtons, Year %in% years[[i]], !!sym(input$groupButtons) %in% group_levels[[j]]) %>% 
                      group_by(!!sym(adm_level)) %>% 
                      summarize(.groups="keep", value=weighted.mean(mean, w=total.weight))
                  } else {
                    geodata2 <- dataset %>% dplyr::filter(Country %in% input$ctryButtons, indicator %in% input$indicButtons, Year %in% years[[i]], !!sym(input$groupButtons) %in% group_levels[[j]]) %>% 
                      group_by(!!sym(adm_level)) %>% 
                      summarize(.groups="keep", value=sum(total))
                  }
                  geodata2 <- merge(geodata2, coords, by=adm_level)
                  assign(paste0("outplot",i,".",j), 
                         as_grob(ggplot()+
                                   geom_sf(data=shp)+
                                   geom_point(data=geodata2, aes(x=LON, y=LAT, color=value))+
                                   scale_color_continuous(name=map_units, limits=c(u_bound, l_bound), trans='reverse')+
                                   labs(caption=paste0(groups_list$shortName[which(groups_list$varName==input$groupButtons)], " = ", group_levels[[j]], "\n\n\n"))+
                                   ggtitle(paste0(indicator_list$prettyName[indicator_list$shortName %in% input$indicButtons]))+
                                   theme_map() +
                                   theme(plot.caption = element_text(hjust = 0, vjust = -1, size = 12),
                                         plot.title = element_text(size = 18))
                         )
                  )
                  groupslist <- c(groupslist, paste0("outplot",i,".",j))
                }
                plottestlist <- c(plottestlist, list(groupslist))
                titlelist <- c(titlelist, paste0(indicator_list$prettyName[indicator_list$shortName %in% input$indicButtons], ", ", years[[i]]))
                plotlist <- c(plotlist, paste0("outplot",i))
              }
              
            }
          } else {
            #Update this line to use codes instead of names
            ctryname <- paste0(input$ctryButtons, "_", "div", adm_num)
            dataset[[adm_level]] <- as.factor(dataset[[adm_level]]) %>% str_to_title() #Case matters when merging with shapefile
            if(!exists(ctryname)) {
              assign(ctryname, st_read(paste0(root_dir,"Spatial/", ctryname, ".shp")), envir=.GlobalEnv)
            }
            for(i in 1:length(years)) {
              #geodata <- dataset %>% dplyr::filter(Country %in% input$ctryButtons, indicator %in% input$indicButtons, Year %in% years[[i]])
              #geodata <- geodata %>% group_by(!!!grouplabels) %>% summarize(mean=weighted.mean(mean, w=total.weight))
              
              if(input$groupButtons %in% "None") {
                geodata <- dataset %>% dplyr::filter(Country %in% input$ctryButtons, indicator %in% input$indicButtons, Year %in% years[[i]])
                if(input$mapstotSelect=="mean") {
                  geodata <- geodata %>% group_by(!!sym(adm_level)) %>% summarize(value=weighted.mean(mean, w=total.weight))
                } else {
                  geodata <- geodata %>% group_by(!!sym(adm_level)) %>% summarize(value=sum(total))
                }
                shp <- merge(get(ctryname),geodata, by.x=paste0("div", adm_num), by.y=adm_level, all.x=T) %>% st_as_sf()
                assign(paste0("outplot",i), ggplot()+
                         geom_sf(data=shp, mapping=aes(fill=value), color="white")+
                         scale_fill_continuous(name=map_units, limits=c(u_bound, l_bound), trans='reverse')+
                         theme_map()+
                         ggtitle(label=paste0(input$ctryButtons, " ", years[[i]], " ", indicator_list$prettyName[indicator_list$shortName %in% input$indicButtons]))+
                         theme(plot.title = element_text(size = 18))
                       
                )
                plotlist <- c(plotlist, paste0("outplot",i))
              } else {
                group_levels <- unique(dataset[[input$groupButtons]])
                groupslist <- list()
                
                for(j in 1:length(group_levels)){
                  #Fractal plots!
                  #Crunch dataset down when multiple grouping variables are chosen - I'm not ready to do multivariate plots just yet.
                  if(input$mapstotSelect=="mean") { 
                  geodata2 <- dataset %>% dplyr::filter(Country %in% input$ctryButtons, indicator %in% input$indicButtons, Year %in% years[[i]], !!sym(input$groupButtons) %in% group_levels[[j]]) %>% 
                    group_by(!!sym(adm_level)) %>% 
                    summarize(.groups="keep", value=weighted.mean(mean, w=total.weight))
                  } else {
                    geodata2 <- dataset %>% dplyr::filter(Country %in% input$ctryButtons, indicator %in% input$indicButtons, Year %in% years[[i]], !!sym(input$groupButtons) %in% group_levels[[j]]) %>% 
                      group_by(!!sym(adm_level)) %>% 
                      summarize(.groups="keep", value=sum(total))
                  }
                  shp <- merge(get(ctryname), geodata2, by.x=paste0("div", adm_num), by.y=adm_level, all.x=T) %>% st_as_sf()
                  assign(paste0("outplot",i,".",j), 
                         as_grob(ggplot()+
                                   geom_sf(data=shp, mapping=aes(fill=value), color="white")+
                                   labs(caption=paste0(groups_list$label[which(groups_list$varName==input$groupButtons)], " = ", group_levels[[j]], "\n\n\n"))+
                                   theme_map()+
                                   scale_fill_continuous(name=map_units, limits=c(u_bound, l_bound), trans='reverse')+
                                   theme(plot.caption = element_text(hjust = 0, vjust = -1, size = 12),
                                         plot.title = element_text(size = 18))
                                 #ggtitle(paste0(indicator_list$prettyName[indicator_list$shortName %in% input$indicButtons]))
                                 
                         )
                  )
                  groupslist <- c(groupslist, paste0("outplot",i,".",j))
                }
                plottestlist <- c(plottestlist, list(groupslist))
                titlelist <- c(titlelist, paste0(indicator_list$prettyName[indicator_list$shortName %in% input$indicButtons], ", ", years[[i]])) 
                plotlist <- c(plotlist, paste0("outplot",i))
              }
            }
          }
          if(input$groupButtons %in% "None") {
            output$mapsDisp <- renderUI({
              lapply(1:length(plotlist), function(x){
                plotOutput(outputId = plotlist[[x]])
                output[[plotlist[[x]]]] <- renderPlot(get(plotlist[[x]]))
              })
            }) 
          } else {
            output$mapsDisp <- renderUI({
              lapply(1:length(plotlist), function(x){
                plotOutput(outputId = plotlist[[x]])
                output[[plotlist[[x]]]] <- renderPlot(grid.arrange(grobs=mget(unlist(plottestlist[[x]]), inherits=T), 
                                                                   top= grid::textGrob(paste0(titlelist[[x]], "\n"), gp = grid::gpar(fontsize = 17)), ncol=2))
              })
              
            })
          }
        }
      }
    } else showNotification("You need to select some data first", type="error")
  })
  
  observeEvent(input$adv_opts_reset,{
    updateSelectInput("weights", selected=`Original Weights`)
    updateSelectInput("ccons", selected=LCU)
    updateCheckboxInput("infl", value=T)
    updateCheckboxInput("graphing_opt", value=F)
    #updateCheckboxInput("comodopts", value=F)
    updateCheckboxInput("wins", value=T)
    updateCheckboxInput("custom_winsor", value=F)
    updateNumericInput("lower_wins", value=1)
    updateNumericInput("upper_wins", value=99)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      #write.csv(getData()[input$newtable_rows_all,], con, row.names=F)
      write.csv(downloadTable[input$newtable_rows_all,], con, row.names=F)
    },
    contentType="text/csv"
  )
  
  getCharts <- function(){
    sess_id <- str_trunc(session$token, 5, ellipsis="")
    local_chartnames <- vapply(chart_list, FUN.VALUE=character(1), FUN=function(x){ paste0(x, "_", sess_id,".png")})
    local_chartnames <- paste0(root_dir,"Temp/",local_chartnames)
    for(i in 1:length(all_charts)){
      ggsave(local_chartnames[[i]],plot=all_charts[[i]])
    }
    return(local_chartnames)
  }
  
  saveRawdata <- function(){
    rawdat_names <- names(getData()$raw_data)
    j <- 1
    k <- 1
    l <- 1
    m <- 1
    
    for(i in 1:length(rawdat_names)){
      dat <- getData()$raw_data[[i]]
      #sourcefile <- getData()$sourcefile$source[which(getData()$sourcefile$var==rawdat_names[[i]])]
      sourcefile <- indicator_list$file[which(indicator_list$shortName %in% rawdat_names[[i]])]
      
      aggs_list <- unlist(lapply(group_cats[group_cats!="Hidden"], function(x){input[[x]]}))
      if(input$cropgroup) aggs_list <- c(aggs_list, "crop_name") # what does this mean? input$cropgroup doesn't make sense to me as a conditional? unlist the implication is "if exists"
      aggs_list <- aggs_list[which(aggs_list %in% names(dat))]
      if(length(aggs_list)>0) if(any(is.na(aggs_list))) {
        aggs_list <- character(0)
        #aggs_list <- unlist(replace(aggs_list, !sapply(aggs_list, length), 0))
      }
      if(sourcefile=="hh_vars"){
        if(j==1){
          hh_vars <- dat %>% select(-c(geography, abbr)) %>% relocate(!!sym(rawdat_names[[i]]), .after=last_col())
          j = j+1
        } else {
          hh_vars <- merge(hh_vars, dat[c("hhid", "Wave", rawdat_names[[i]], "ea", aggs_list)], by=c("hhid", "ea", "Wave", aggs_list))
        }
      } else if(sourcefile=="hh-crop_vars"){
        if(k==1){
          hh_crop_vars <- dat %>% select(-c(geography, abbr)) %>% relocate(!!sym(rawdat_names[[i]]), .after=last_col())
          k = k+1
        } else {
          hh_crop_vars <- merge(hh_crop_vars, dat[c("hhid", "Wave", rawdat_names[[i]], "ea", aggs_list)], by=c("hhid", "ea", "Wave", aggs_list))
        }
      } else if(sourcefile=="pers_vars"){
        if(l==1){
          pers_vars <- dat %>% select(-c(geography, abbr)) %>% relocate(!!sym(rawdat_names[[i]]), .after=last_col())
          l = l+1
        } else {
          pers_vars <- merge(hh_crop_vars, dat[c("hhid", "Wave", rawdat_names[[i]], "ea", "indiv", aggs_list)], by=c("hhid", "ea", "Wave", "indiv", aggs_list))
        }
      } else if(sourcefile=="plot_vars") {
        if(m==1){
          plot_vars <- dat %>% select(-c(geography, abbr)) %>% relocate(!!sym(rawdat_names[[i]]), .after=last_col())
          m = m+1
        } else {
          plot_vars <- merge(plot_vars, dat[c("hhid", "Wave", rawdat_names[[i]], "ea", "plot_id", aggs_list)], by=c("hhid", "ea", "Wave", "plot_id", aggs_list))
        }
      }
    }
    
    ##### Exporting Raw Data ####
    outdat <- vector("list", 4)
    
    if(exists("hh_vars")) {
      outdat[[1]] <- hh_vars %>% relocate(Country, Wave, ea, hhid)
      names(outdat)[[1]] <- "Households"
    }
    
    if(exists("hh_crop_vars")) {
      if("crop_name" %in% names(hh_crop_vars)){
        outdat[[2]] <- hh_crop_vars %>% relocate(Country, Wave, ea, hhid, crop_name)}
      else{
        outdat[[2]] <- hh_crop_vars %>% relocate(Country, Wave, ea, hhid)}
      names(outdat)[[2]] <- "Household-Level Crops"
    }
    
    if(exists("plot_vars")) {
      outdat[[3]] <- plot_vars  %>% relocate(Country, Wave, ea, hhid, plot_id)
      names(outdat)[[3]] <- "Plots"
    } 
    
    if(exists("pers_vars")) {
      outdat[[4]] <- pers_vars %>% relocate(Country, Wave, ea, hhid, indiv)
      names(outdat)[[4]] <- "Plot Managers"
    }
    res <- sapply(outdat, FUN=is.null)
    return(outdat[which(!res)])
  }
  
  
  output$downloadRaw <- downloadHandler(
    filename=function() {
      paste0("raw_data_", Sys.Date(), ".xlsx")
    },
    content=function(con) {
      outdat <- saveRawdata()
      wb <- createWorkbook(creator="EPAR")
      for(i in 1:length(outdat)){
        addWorksheet(wb, names(outdat)[[i]])
        writeData(wb, names(outdat)[[i]], outdat[[i]], rowNames=F)
      }
      
      saveWorkbook(wb, file=con, overwrite=TRUE)
    },
    contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  
  # Download Summary Stats #
  output$downloadChart <- downloadHandler(
    filename=function(){paste0("charts-", Sys.Date(), ".zip")},
    content=function(con){zip(con, files=getCharts()
    )},
    contentType="application/zip"
  )
  
  
  # Download Meta Data #
  output$downloadMeta <- downloadHandler(
    filename="output_metadata.csv",
    content=function(con){
      write.csv(definitionTabExp, con, row.names = F)
    },
    contentType="text/csv"
  )
  
  observeEvent(input$downloadRaw, {
    showNotification(paste("Working... This operation may take a while."), duration = 10)
  })
  
  session$onSessionEnded(function() {
    file.remove(list.files(path=paste0(root_dir, "Temp/"), pattern=str_trunc(session$token, 5, ellipsis=""), full.names=T))
  })
}



####################### run app #######################
shinyApp(ui = ui, server = server)