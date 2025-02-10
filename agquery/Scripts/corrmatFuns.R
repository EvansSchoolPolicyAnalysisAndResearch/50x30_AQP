corMat <- function(shortNames, labelNames, data_out){
  cor_matrix <- cor(data_out, use="pairwise.complete.obs")
  par(mar = c(5, 5, 4, 2) - 2)
  #corrPlot <- corrplot.mixed(cor_matrix, order = 'AOE')
  #output$corrPlot <- renderPlot(corrplot(cor_matrix, order = 'AOE',col=colorRampPalette(c("white","lightblue","red"))(100)))
  #print(corrPlot) 
  res <- match(rownames(cor_matrix), shortNames)
  rownames(cor_matrix) <- labelNames[res]
  res <- match(colnames(cor_matrix), shortNames)
  colnames(cor_matrix) <- labelNames[res]
  #print(cor_matrix)
  p_matrix <- matrix(nrow = ncol(data_out), ncol = ncol(data_out))
  for(i in seq_len(ncol(data_out))) {
    for(j in seq_len(ncol(data_out))) {
      test_result <- cor.test(data_out[, i], data_out[, j], method = "pearson")
      p_matrix[i, j] <- test_result$p.value
    }
  }
  #print(p_matrix)
  p_matrix[upper.tri(p_matrix)] <- NA
  hover_text <- matrix("", nrow = ncol(data_out), ncol = ncol(data_out))
  for(i in 1:nrow(p_matrix)) {
    for(j in 1:ncol(p_matrix)) {
      if (!is.na(p_matrix[i, j])) {
        cor_value <- cor_matrix[i, j]
        p_value <- p_matrix[i, j]
        # Construct the hover text
        if (p_value>=0.00001) {
          hover_text[i, j] <- paste0("P-value: ", format(p_value, digits = 3))
        }
        if (p_value<0.00001) {
          hover_text[i, j] <- paste0("P-value: ", "<0.00001")
          p_matrix[i,j] <- 0.00001
        }
      }
    }
  }
  #print(hover_text)
  #cor_matrix[upper.tri(cor_matrix)] <- NA
  hover_text[upper.tri(hover_text)] <- NA
  heatMap <- heatmaply_cor(cor_matrix,
                           node_type = "scatter",
                           point_size_mat = -log10(p_matrix),
                           point_size_name = "-log10(p-value)",
                           label_names=c("Row", "Column", "Correlation"),
                           custom_hovertext = hover_text,
                           Colv=NA, Rowv=NA, plot_method="ggplot") |>
    layout(title = "Correlation Heatmap", margin = list(t = 60), height=1000)
  return(heatMap)
}

cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}
