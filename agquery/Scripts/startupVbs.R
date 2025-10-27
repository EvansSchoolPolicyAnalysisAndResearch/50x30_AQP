#### FUNCTIONS ####

mdUrl <- function(cap_string){
  linkinfo <- str_extract(cap_string, "\\[(.+?)\\]\\((.+?)\\)", group=c(1,2)) #Takes only the first match.
  while(!any(is.na(linkinfo))){
    cap_string <- str_replace(cap_string, "\\[(.+?)\\]\\((.+?)\\)", sprintf('<a href="%s" target=_blank>%s</a>', linkinfo[[2]], linkinfo[[1]]))
    linkinfo <- str_extract(cap_string, "\\[(.+?)\\]\\((.+?)\\)", group=c(1,2))
  }
  cap_string <- str_replace_all(cap_string, "\n", "")
    return(cap_string)
}

wbDataPrep <- function(filename) {
  read.csv(paste0("Extdata/World Bank/",filename), skip=4) |> 
    pivot_longer(cols=starts_with("X"), 
                 names_to="year", 
                 values_to = "val", 
                 names_prefix="X", 
                 values_drop_na=T) |> 
    mutate(year = as.numeric(year)) |> 
    filter((Country.Name=="Cambodia" | Country.Name=="Viet Nam" | Country.Name=="Thailand") & year>2000)
}

wbDataNames <- function(filename){
  data_row <- read.csv(paste0("Extdata/World Bank/",filename), skip=4)[1,]
  indicname <- data_row$Indicator.Name
  units <- gsub(".*\\((.*)\\).*", "\\1", indicname)
  title <- gsub("(.*)(\\(.*\\)).*", "\\1", indicname)
  subtitle <- data_row$Indicator.Code
  return(list(units=units, title=title, subtitle=subtitle))
}

wbTrend <- function(wbData, depth){
  indicName <- wbData$Indicator.Name[[1]]
  recVal <- wbData |> filter(year==max(wbData$year))
  recVal <- recVal$val[[1]]
  oldVal <- wbData |> filter(year==max(wbData$year)-depth)
  oldVal <- oldVal$val[[1]]
  interp <- (recVal - oldVal)/oldVal/depth
  dir <- if(interp>0) "Up" else "Down"
  trend <- sprintf("%s %.1f%% per year since %i", dir, interp*100, max(wbData$year-depth))
  if(str_detect(indicName, "%")){
    recValOut <- paste0(signif(recVal,2),"%")
  } else {
    recValOut <- format(recVal,big.mark=",")
  }
  return(list(recVal=recValOut, trend=trend))
}

procCasTabs <- function(path){
  casDataFiles <- list.files(path,pattern="cas-")
  if(length(casDataFiles)>0){
    casDataFiles <- data.frame(casDataFiles)
    names(casDataFiles) <- "SourceFile"
    casDataFiles$shortName <- str_extract(casDataFiles$SourceFile, "([A-z_]+).csv", group=1) 
    if (exists("indicator_list") && is.data.frame(indicator_list)) {
      var_info <- indicator_list |> dplyr::select(shortName, file, axisName)
    } else {
      var_info <- data.frame(shortName = character(), file = character(), axisName = character())
    }
    casDataFiles <- left_join(casDataFiles, var_info, by="shortName")
    tabCards <- lapply(1:nrow(casDataFiles), FUN=function(y){
      casFile <- casDataFiles$SourceFile[[y]]
      casName <- casDataFiles$shortName[[y]]
      if(!is.na(casName)){
        casYear <- str_extract(casFile, "[0-9]{4}")
        
        casTab <- read.csv(paste0(path, "/", casFile))
        #Strip row names if they got loaded
        if(names(casTab)[[1]]=="X") {
          casTab <- casTab[,-1]
        }
        names(casTab) <- str_replace_all(names(casTab), "[.]", " ")
        casTabFlx <- flextable(casTab)
        if(ncol(casTab>2)){
          casTabFlx <- merge_v(casTabFlx, j=names(casTab)[[1]])
        }
        casTabTitle <- paste0(casDataFiles$axisName[[y]], ", ", casYear)
        return(card(card_header(casTabTitle),
                    renderUI({
                      casTabFlx |>
                        autofit() |>
                        htmltools_value()
                    }),
                    card_footer(HTML("<i>Source: Cambodia Agriculture Survey</i>")),
                    theme="light")
        )
      }
    })
  } else {
    return(list())
  }
}

#Images and html charts.
procImgs <- function(path){
  img_info <- tryCatch(read.csv(paste0(path, "/image_information.csv")),
                       error=function(e){
                         errdf <- data.frame(image_name=str_extract(list.files(path, pattern=".png|.jpg|.tif|.gif|.bmp|.html"), "([A-z_ ]+)\\.", group=1))
                         if(nrow(errdf)>0){
                         errdf$title <- NA
                         errdf$source <- NA
                       return(errdf)
                         } else {
                           return(data.frame())
                         }
                         } #Just get a list of images if the csv is missing; probably a more efficient way to do this.
  )
  if(nrow(img_info)>0) {
  parentDir <- str_extract(path, "/([A-z]+)$", group=1)
  imgs <- data.frame(src=paste0(parentDir, "/", list.files(path, pattern=".png|.jpg|.tif|.gif|.bmp|.html")))
  addResourcePath(parentDir, path)
  imgs$image_name <- str_extract(imgs$src, "/([A-z_ ]+)\\.", group=1)
  img_info <- right_join(img_info, imgs, by="image_name")
  imgCards <- lapply(1:nrow(img_info), FUN=function(y){
      imgsrc <- img_info$src[[y]]
      img_title <- img_info$title[[y]]
      img_caption <- img_info$source[[y]]
      if(!is.na(imgsrc)){
        return(card(
          if(!is.na(img_title)) card_header(img_title),
          #if(!str_detect(imgsrc, ".html")) card_image(imgsrc) else card_body(as_fill_item(tags$iframe(srcdoc=paste(readLines(imgsrc), collapse="\n")))),
          if(!str_detect(imgsrc, ".html")) card_image(src=imgsrc) else card_body(as_fill_item(tags$embed(src=imgsrc))),
          if(!is.na(img_caption)) card_footer(HTML(mdUrl(img_caption))),
          full_screen=T
        )
        )
      }
    })
  return(imgCards)
  } else {
    return(list())
  }
}

##############################################

## This code gets executed at startup.

wbDataFiles <- list.files("Extdata/World Bank", pattern="^API.+[0-9]{4}\\.csv$")
if(length(wbDataFiles) > 0) {
  colors <- c("primary", "success", "info")
  wbN <- 1
  vbs <- list()

  wbVbs <- lapply(1:length(wbDataFiles), FUN=function(x) {
    wbFile <- wbDataFiles[[x]]
    wbData <- wbDataPrep(wbFile) |> filter(Country.Name=="Cambodia") #long term might need to add this filter to the function to reduce confusion; leaving it for now in case we want comparatives.
    
    if(nrow(wbData)==0){
      # if(!exists("wbErrors")){
      #   wbErrors <- paste("Error in", wbFile)
      # } else {
      #   wbErrors <- paste(wbErrors, wbFile, sep=", ")
      # }
      return(NULL)
    } else {
      wbDNames <- wbDataNames(wbFile)
      wbText <- wbTrend(wbData, 5) # How many years to do the trend over?
      spacer <- if(x >= 3) floor((x-1)/3) %% 2 else 0
      return(value_box(title=wbDNames$title,
                       HTML(sprintf('<a href="https://data.worldbank.org/indicator/%s?locations=KH" target="_blank", style="color: #ffffff">%s</a>', wbDNames$subtitle, wbDNames$subtitle)),
                       value=p(wbText$recVal),
                       showcase=renderPlotly(sparkline(wbData, "year", "val", sprintf("%s (%s)", wbDNames$title, wbDNames$units))),
                       full_screen=T,
                       theme=colors[[(((x+spacer) %% 3)+1)]])
      )
    }
  })
}

#Single line to process any CAS output tables found in the main directory.
extdataCasCards <- procCasTabs("Extdata/Overview")
extdataImgCards <- procImgs("Extdata/Overview")

##### Commodity directories

commod_dirs <- list.dirs('Extdata/Commodities', recursive=F, full.names=F)
commod_tabs_out <- lapply(commod_dirs, FUN=function(x){
  tabCards <- procCasTabs(paste0("Extdata/Commodities/", x))
  imgCards <- procImgs(paste0("Extdata/Commodities/",x))
  ### Error handling needed here.
  outCards <- c(tabCards, imgCards)
  if(!all(is.na(outCards))){
    outCards <- outCards[!is.na(outCards)]
    return(tabPanel(x, layout_column_wrap(width=1/2, min_height="200px", !!!outCards)))
  }
})