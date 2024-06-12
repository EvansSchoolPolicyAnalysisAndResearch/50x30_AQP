#paste0(plotTitle, ", ", year, " Values")
#Relocate this to the plot call ^^^ 

bicolorMap <- function(xShp, fillVal, plotTitle, units){
  plotOut <- ggplot(xShp, aes(fill = !!sym(fillVal)))+
  geom_sf() +
  ggtitle(plotTitle) +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkblue", midpoint = 0, limit = c(min(xShp[[fillVal]], na.rm = TRUE), max(xShp_currMap[[fillVal]], na.rm = TRUE)), name=units)+
  theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
return(plotOut)
}

monoColorMap <- function(xShp, fillVal, plotTitle, year, units){
  plotOut <- ggplot(xShp, aes(fill = !!sym(fillVal)))+
    geom_sf() +
    ggtitle(paste0(plotTitle, ", ", year, " Values")) +
    scale_fill_gradient(low = "white", high = "darkblue", limit = c(min(xShp[[fillVal]], na.rm = TRUE), max(xShp_currMap[[fillVal]], na.rm = TRUE)), name=units)+
    theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
  return(plotOut)
}

makeHistGrps <- function(outdata, yvars, bins, aggs_list, indicAxis, titleLab, aggs_lab)
  ggplot(outdata, aes_string(x=yvars, group=aggs_list, fill=aggs_list))+
  geom_histogram(bins = bins)+
  #geom_density(fill=NA)+scale_color_discrete(guide='none')+
  labs(x=xlab, y="Number of Observations", fill=aggs_lab)+
  ggtitle(str_to_title(paste("Histogram of", ylab))) +
  theme(plot.background = element_rect(fill = "transparent", color = NA), 
        panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.title = element_text(hjust = 0.5, size = 14), 
        axis.ticks = element_blank(), 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
        axis.text = element_text(size=12))

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
        axis.text=element_text(size=12))
}


makeScatterGrps <- function(outdata, xvars, yvars, aggs_list, xlab, ylab)
  ggplot(outdata, aes_string(x=yvars, group=aggs_list, fill=aggs_list))+
  geom_histogram(bins = bins)+
  #geom_density(fill=NA)+scale_color_discrete(guide='none')+
  labs(x=xlab, y="Number of Observations", fill=aggs_lab)+
  ggtitle(str_to_title(paste("Histogram of", ylab))) +
  theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))


makeScatter <- function(outdata, xvars, yvars, xlab, ylab)
  ggplot(outdata, aes_string(x=yvars))+
  geom_histogram(bins = bins)+
  #geom_density(fill=NA)+scale_color_discrete(guide='none')+
  labs(x=xlab, y="Number of Observations", fill=aggs_lab)+
  ggtitle(str_to_title(paste("Histogram of", ylab))) +
  theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_blank(), panel.grid = element_blank(), axis.title = element_text(hjust = 0.5, size = 14), axis.ticks = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
