#### FAO Food Balances
# protein supply
faofb <- read.csv("Extdata/faofb.csv")
psupp <- faofb |> filter(Element.Code==674)
psupp_plot <- ggplot(psupp, aes(x=Year, y=Value, group=Item, fill=Item))+
  geom_area(color="white", alpha=0.6)+
  theme_minimal()+
  scale_x_continuous(breaks=scales::breaks_pretty())+
  labs(x="", y="Protein supplied (g/capita/day)")+
  theme(legend.position="bottom")
htmlwidgets::saveWidget(ggplotly(psupp_plot) |>
                          plotly::layout(legend=list(orientation='h',
                                                     xanchor='center',
                                                     x=0.5)), "Extdata/Commodities/Poultry/psupp_plot.html")


##imports/domestic supply
domsupp <- faofb |> filter(Element.Code!=674 & Element.Code!=5511)
domsupp$Element <- ifelse(domsupp$Element=="Import quantity", "Imports", "Domestic Supply")
domsupp_plot <- ggplot(domsupp, aes(x=Year, y=Value, group=Element, color=Element))+
  geom_line()+
  facet_wrap(vars(Item), scales="free_y")+
  theme_minimal()+
  scale_color_discrete(name="")+
  scale_x_continuous(breaks=scales::breaks_pretty())+
  labs(x="", y="Quantity (thousands of tons)")

htmlwidgets::saveWidget(ggplotly(domsupp_plot), "Extdata/Commodities/Poultry/domsupp_plot.html")