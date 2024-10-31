
biColorMap <- function(xShp, fillVal, plotTitle, units){
  plotOut <- ggplot(xShp, aes(fill = !!sym(fillVal)))+
    geom_sf() +
    ggtitle(plotTitle) +
    scale_fill_gradient2(low = "darkred", mid = "white", high = "darkblue", midpoint = 0, 
                         limit = c(min(xShp[[fillVal]], na.rm = TRUE), max(xShp[[fillVal]], na.rm = TRUE)), name=units, 
                         labels=function(x) format(x, big.mark=",", scientific=FALSE))+
    theme(plot.background = element_rect(fill = "transparent", color = NA), 
          panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
          legend.text=element_text(size=12, color="black"),
          legend.title=element_text(size=12, color="black"),
          legend.key.height = unit(0.1, "npc"))
  return(plotOut)
}

monoColorMap <- function(xShp, fillVal, plotTitle, units){
  plotOut <- ggplot(xShp, aes(fill = !!sym(fillVal)))+
    geom_sf() +
    ggtitle(plotTitle) +
    scale_fill_gradient(low = "white", high = "darkblue", 
                        limit = c(min(xShp[[fillVal]], na.rm = TRUE), max(xShp[[fillVal]], na.rm = TRUE)), 
                        name=units, labels=function(x) format(x, big.mark=",", scientific=F))+
    theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), 
          panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
          plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
          legend.text=element_text(size=12, color="black"),
          legend.title=element_text(size=12, color="black"),
          legend.key.height = unit(0.1, "npc"))
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
          axis.text = element_text(size=12))+
    scale_x_continuous(labels=scales::comma)
}

makeHist <- function(outdata, yvars, bins, indicAxis, titleLab){
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
          axis.text=element_text(size=12))+
    scale_x_continuous(labels=scales::comma)
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
    annotate(geom="richtext", label=annot, x=(max(outdata[[xvars]])+min(outdata[[xvars]]))/2, y=max(outdata[[yvars]]))+
    scale_y_continuous(labels=scales::comma)+
    scale_x_continuous(labels=scales::comma)
}

makeScatter <- function(outdata, xvars, yvars, xlab, ylab, annot){
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
    annotate(geom="richtext", label=annot, x=(max(outdata[[xvars]])+min(outdata[[xvars]]))/2, y=0.9*max(outdata[[yvars]]))+
    scale_y_continuous(labels=scales::comma)+
    scale_x_continuous(labels=scales::comma)
}

reportChart <- function(outdata, xvars, yvars, xlab, ylab){
  outdata <- arrange(outdata, !!sym(yvars))
  groupcats <- outdata[[xvars]]
  outdata[[xvars]] <- factor(outdata[[xvars]], levels=groupcats, labels=groupcats)
  short_yvars <- signif(outdata[[yvars]], 3)
  
  ggplot(outdata, aes(x=!!sym(xvars), y=!!sym(yvars)))+
    geom_col(fill="darkblue")+
    geom_text(aes(label=short_yvars), nudge_y=-0.03, color="white", size=14, size.unit='pt')+
    coord_flip()+
    theme_minimal()+
    theme(panel.grid.major=element_blank(), axis.text=element_text(size=14))+
    labs(x=xlab, y=ylab)+
    scale_y_continuous(labels=scales::comma)
}

timeSeriesPlot <- function(outdata, yvars, statname){
  outdata <- outdata %>% filter(shortName==yvars)
  lab <- unique(outdata$labelName)
  ggplot(outdata, aes(x=year, y=!!sym(statname)))+
    geom_point()+
    geom_line(linewidth=1)+
    scale_y_continuous(labels=scales::comma)+
    theme_minimal()+
    theme(legend.position="none", panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), axis.text=element_text(size=12))+
    scale_x_continuous(breaks=scales::breaks_pretty())+
    labs(x="",y=lab[[1]])
}