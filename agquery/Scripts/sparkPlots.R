sparkline <- function(plotdata, xvar, yvar, axislab){
  plotdata <- plotdata |> select(all_of(c(xvar, yvar)))
  names(plotdata) <- c("xvar","yvar")
  plot_ly(plotdata) |> 
    add_lines(
      x = ~xvar, y=~yvar,
      color = I("white"), span=I(1),
      fill = 'tozeroy', alpha=0.2) |> 
    layout(
      xaxis = list(visible = F, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = axislab),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    config(displayModeBar = F) %>%
    htmlwidgets::onRender(
      "function(el) {
      el.closest('.bslib-value-box')
        .addEventListener('bslib.card', function(ev) {
          Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
          Plotly.relayout(el, {'yaxis.visible': ev.detail.fullScreen});
        })
    }"
    )
}

grouped_sparkline <- function(plotdata, xvar, yvar, groupvar){
  plotdata <- plotdata |> select(all_of(c(xvar, yvar, groupvar)))
  names(plotdata) <- c("xvar","yvar", "groupvar")
  plot_ly(plotdata) |> 
    add_lines(
      x = ~xvar, y=~yvar, color=~groupvar,
      colors = c("darkred", "darkblue", "darkgreen"), 
      span=I(1)) |> 
    layout(
      xaxis = list(visible = F, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = axislab),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "darkgray"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) |> 
    config(displayModeBar = F) %>%
    htmlwidgets::onRender(
      "function(el) {
      el.closest('.bslib-value-box')
        .addEventListener('bslib.card', function(ev) {
          Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
        })
    }"
    )
}


