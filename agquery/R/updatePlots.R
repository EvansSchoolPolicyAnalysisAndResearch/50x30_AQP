updatePlots <- function(tab="data", maps=T){
  if(tab=="data"){ 
    #getData <- function(files, years, xvars, yvars=NULL, adm_level="hhid", aggs_list=NULL, source_call=NULL)
    #Find better way to do this.
    aggs_list = input$groupsChk
    if(aggs_list==""){
      aggs_list <- NULL
    }
    #messy error handling here
    file <- paste0("CAS_", input$yearBtn, "_", tolower(input$policiesBox2), ".csv")
    file <- as.data.frame(file)
    names(file) <- "file.name"
    file$year <- input$yearBtn
    all_data <- getData(file, xvars=input$indicsIn, yvars=input$corrsIn, adm_level=input$disAgg_admin, aggs_list=aggs_list, source_call="explorer", drop_0s = input$yChk)
  } 
  if(any(all_data!="")){
    #else if(tab=="trend"){
    #ALT - might be easier than what we do now with the maps in a separate area. Maybe build out later.
    #}
    mapdata <- all_data$mapdata
    outdata <- all_data$tempdata #Note to go back and fix the naming here.
    outdata <- na.omit(outdata)
    if(nrow(outdata)==0){
      showNotification("Error: No non-n/a observations in dataset", type="error") 
    } else { 
      xvars = input$corrsIn
      yvars = input$indicsIn
      if(!all(c(xvars, yvars) %in% names(outdata))){
        showNotification("Error: one or both variables is missing from the dataset. Did you capitalize everything the same way?")
      } else {
        output$indicHeader <- renderUI(HTML(sprintf('<div style="border: 1px solid #ddd; padding: 9px; margin-bottom: 0px; line-height: 1.2; text-align: center; border-radius: 3px;"> %s </div>'
                                                    , indicator_list$labelName[indicator_list$shortName==input$indicsIn])))
        output$corrHeader <- renderUI(HTML(sprintf('<div style="border: 1px solid #ddd; padding: 9px; margin-bottom: 0px; line-height: 1.2; text-align: center; border-radius: 3px;"> %s </div>'
                                                   , indicator_list$labelName[indicator_list$shortName==input$corrsIn])))
        adm_level <- input$disAgg_admin
        varslist <- c(xvars, yvars)
        bins <- ifelse(adm_level=="province", 6, 30)
        #heatmapdata <- getData()$tempheatmapdata
        #outdata <- heatmapdata %>% select(all_of(c(xvars,yvars)))
        xlab <- indicator_list$labelName[indicator_list$shortName==xvars]
        ylab <- indicator_list$labelName[indicator_list$shortName==yvars]
        
        indicAxis <- indicator_list$axisName[indicator_list$shortName==xvars]
        corrAxis <- indicator_list$axisName[indicator_list$shortName==yvars]
        if(input$groupsChk==""){
          indicatorHist <- makeHist(outdata, yvars, bins, indicAxis, ylab)
          corrHist <- makeHist(outdata, xvars, bins, corrAxis, xlab)
          scatterPlot <- makeScatter(outdata, xvars, yvars, xlab, ylab)
        } else {
          aggs_lab = groups_list$shortName[groups_list$varName==aggs_list]
          if(!is.factor(outdata[[aggs_list]])){
            flevels = groups_list[which(groups_list$varName==aggs_list),]$Levels %>% str_split(., ",") %>% unlist()
            flabels = groups_list[which(groups_list$varName==aggs_list),]$Labels %>% str_split(., ",") %>% unlist()
            outdata[[aggs_list]] <- factor(outdata[[aggs_list]], levels=flevels, labels=flabels)
          }
          
          corrHist <- makeHistGrps(outdata, xvars, bins, aggs_list, corrAxis, xlab, aggs_lab)
          indicatorHist <- makeHistGrps(outdata, yvars, bins, aggs_list, indicAxis, ylab, aggs_lab)
          scatterPlot <- makeScatterGrps(outdata, xvars, yvars, aggs_list, xlab, ylab)
        }
        if(maps==T){
          mapdata <- merge(khm_shp, mapdata, by.x="ADM1_EN", by.y="province", all.x=T)
          corrTitle <- paste("Map of", indicator_list$labelName[indicator_list$shortName == xvars], "by Province")
          corrUnits <- indicator_list$units[indicator_list$shortName==xvars]
          
          indicTitle <- paste("Map of", indicator_list$labelName[indicator_list$shortName == yvars], "by Province")
          indicUnits <- indicator_list$units[indicator_list$shortName==yvars]
          
          if(min(mapdata[[xvars]]) < 0 & max(mapdata[[xvars]]) > 0){ 
            corrMap <- bicolorMap(mapdata, xvars, corrTitle, corrUnits) 
          } else {
            corrMap <- monoColorMap(mapdata, xvars, corrTitle, corrUnits)
          }
          
          if(min(mapdata[[yvars]]) < 0 & max(mapdata[[yvars]]) > 0){
            indicatorMap <- bicolorMap(mapdata, yvars, corrTitle, corrUnits) 
          } else {
            indicatorMap <- monoColorMap(mapdata, yvars, indicTitle, indicUnits)
          }
        }
        res <- eval(parse_expr(sprintf("with(outdata, cor.test(%s, %s))", xvars, yvars)))
        
        if(res$p.value <= 0.01){ 
          adj="<font color='#44ce1b'>very high</font>"
        } else if(res$p.value <= 0.05) {
          adj="<font color='#bbdb44'>high</font>"
        } else if(res$p.value <= 0.1) {
          adj="<font color='#f7e379'>moderate</font>"
        }  else if(res$p.value <= 0.2) {
          adj="<font color='#f2a134'>low</font>"
        } else {
          adj = "<font color='#e51f1f'>no</font>"
        }
        
        res_out <- sprintf("<div style='font-family: 'Open Sans';'><br><br>There is %s%% (%s%% - %s%%) correlation between <font color='#0a2167'><b>%s</b></font> and <font color='#0a2167'><b>%s</b></font>. There is %s confidence in this result.</div>", 
                           round(res$estimate[[1]]*100, 1), round(res$conf.int[[1]]*100, 1), round(res$conf.int[[2]]*100, 1),
                           xlab, ylab, adj
        )
        
        output$indicatorHist <- renderPlot(indicatorHist)
        output$corrHist <- renderPlot(corrHist)
        output$scatterPlot <- renderPlot(scatterPlot)
        output$plotInterp <- renderUI(HTML(res_out))
        
        if(maps==T){
          output$indicatorMap <- renderPlot(indicatorMap)
          output$corrMap <- renderPlot(corrMap)
        }
      }
    }
  }
}