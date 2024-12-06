# Figures for the Cambodia 50x30 App
# 
# 


imp_exp_plot <- function(data, product, units, direction){
  if(units=="value"){
    unit_filt <- "1 million USD"
    unit_lab <- "Millions of $US"
  } else {
    unit_filt <- "1000 t"
    unit_lab <- "Thousands of Tons"
  }
 plot_data <- data %>% filter(product_general==product & unit == unit_filt & flow==direction)
 colors <- list(CAC = "darkorchid3", COMTRADE="blue", FAOSTAT="darkgreen", OEC="orange3")
 levels <- unique(plot_data$dataset)
ggplot(plot_data,
  aes(x = year, y = value, color = dataset)) +
  geom_line(linewidth=1) +
  geom_point() +
  theme_minimal(base_size=12) +
  scale_color_manual(values=colors[levels], name="Dataset Source")+
  labs(x = "Year",
       y = unit_lab) +
  theme(legend.position = "right", axis.text.x=element_text(size=12), axis.text.y=element_text(size=12))
}