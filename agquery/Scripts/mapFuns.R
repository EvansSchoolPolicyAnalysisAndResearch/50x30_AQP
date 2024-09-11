
biColorMap <- function(xShp, fillVal, plotTitle, units, noteLab){
  plotOut <- ggplot(xShp, aes(fill = !!sym(fillVal)))+
    geom_sf() +
    ggtitle(plotTitle) +
    scale_fill_gradient2(low = "darkred", mid = "white", high = "darkblue", midpoint = 0, limit = c(min(xShp[[fillVal]], na.rm = TRUE), max(xShp[[fillVal]], na.rm = TRUE)), name=units)+
    theme(plot.background = element_rect(fill = "transparent", color = NA), 
          panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18)) +
    labs(caption = paste(noteLab))
  return(plotOut)
}

monoColorMap <- function(xShp, fillVal, plotTitle, units, noteLab){
  plotOut <- ggplot(xShp, aes(fill = !!sym(fillVal)))+
    geom_sf() +
    ggtitle(plotTitle) +
    scale_fill_gradient(low = "white", high = "darkblue", limit = c(min(xShp[[fillVal]], na.rm = TRUE), max(xShp[[fillVal]], na.rm = TRUE)), name=units)+
    theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18)) +
    labs(caption = paste(noteLab))
  return(plotOut)
}

makeHistGrps <- function(outdata, yvars, bins, aggs_list, indicAxis, titleLab, aggs_lab) {
  ggplot(outdata, aes_string(x=yvars, group=aggs_list, fill=aggs_list))+
    geom_histogram(bins = bins)+
    #geom_density(fill=NA)+scale_color_discrete(guide='none')+
    labs(x=indicAxis, y="Number of Observations", fill=aggs_lab)+
    ggtitle(str_to_title(paste("Histogram of", titleLab))) +
    theme(plot.background = element_rect(fill = "transparent", color = NA), 
          panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.title = element_text(hjust = 0.5, size = 14), 
          axis.ticks = element_blank(), 
          plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
          axis.text = element_text(size=12))
          labs(caption = "Here is a footnote")
}

makeHist <- function(outdata, yvars, bins, indicAxis, titleLab, noteLab){
  ggplot(outdata, aes(x=!!sym(yvars)))+
    geom_histogram(bins = bins) +
    labs(x=indicAxis, y="Number of Observations")+
    ggtitle(str_to_title(paste("Histogram of", titleLab))) +
    theme(plot.background = element_rect(fill = "transparent", color = NA), 
          panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.title = element_text(hjust = 0.5, size = 14), 
          axis.ticks = element_blank(), 
          plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
          axis.text=element_text(size=12)) +
    labs(caption = paste(noteLab))
}


makeScatterGrps <- function(outdata, xvars, yvars, aggs_list, xlab, ylab, aggs_lab, annot){
  scatterPlot <- ggplot(outdata, aes(x=!!sym(xvars), y=!!sym(yvars), group=!!sym(aggs_list), color=!!sym(aggs_list)))+ #only one yvar for now
    geom_point()+
    stat_smooth(method="lm", show.legend=F)+
    labs(x=xlab, y=ylab, color=aggs_lab)+
    ggtitle(paste("Scatterplot of",str_to_title(ylab), "\n",  "and", str_to_title(xlab ))) +
    theme(plot.background = element_rect(fill = "transparent", color = NA), 
          panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.text = element_text(size=12),
          axis.title = element_text(hjust = 0.5, size = 14), 
          axis.ticks = element_blank(), 
          plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
          legend.title=element_text(size=14),
          legend.text=element_text(size=12))+
    annotate(geom="richtext", label=annot, x=(max(outdata[[xvars]])+min(outdata[[xvars]]))/2, y=max(outdata[[yvars]])) 
}

makeScatter <- function(outdata, xvars, yvars, xlab, ylab, annot, noteLab){
  ggplot(outdata, aes(x=!!sym(xvars), y=!!sym(yvars))) + #only one yvar for now
    geom_point() +
    stat_smooth(method="lm")+
    labs(x=xlab, y=ylab) +
    ggtitle(str_to_title(paste("Scatterplot of",str_to_title(ylab), "\n",  "and", str_to_title(xlab )))) +
    theme(plot.background = element_rect(fill = "transparent", color = NA), 
          panel.background = element_blank(), 
          panel.grid = element_blank(), 
          axis.text = element_text(size=12),
          axis.title = element_text(hjust = 0.5, size = 14), 
          axis.ticks = element_blank(), 
          plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
          legend.title=element_text(size=14),
          legend.text=element_text(size=12))+
    annotate(geom="richtext", label=annot, x=(max(outdata[[xvars]])+min(outdata[[xvars]]))/2, y=max(outdata[[yvars]])) +
    labs(caption = paste(noteLab))
}