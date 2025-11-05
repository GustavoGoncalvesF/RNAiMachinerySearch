# This function plots a heatmap to represent the expression of RNAi core genes across samples.

library(ComplexHeatmap)
library(edgeR)

heatmap.plot <- function(rnai_hits, expression_df, normalize = c("logCPM", "zscore", "none"), save = FALSE, path = NULL){
  # Merge keeping only protein annotation's present in the expression dataframe, and preserve they order
  gene_present <- rnai_hits$GeneID[rnai_hits$GeneID %in% rownames(expression_df)]
  heatmap_data <- expression_df[gene_present, , drop = FALSE]

  # Change rownames to ProteinAnnotation
  rownames(heatmap_data) <- rnai_hits$ProteinAnnotation[rnai_hits$GeneID %in% gene_present]

  # Input validation
  if (nrow(heatmap_data) ==0){
    stop("Cannot plot a heatmap because none of the RNAi hits are found in the expression matrix.")
  }

  # Normalizing data before plot
  normalize <- match.arg(normalize)

  mat <- as.matrix(heatmap_data)

  if (normalize == "logCPM") {
    dge <- edgeR::DGEList(counts = mat)
    mat <- edgeR::cpm(dge, log = TRUE, prior.count = 1)
  }

  if (normalize == "zscore") {
    dge <- edgeR::DGEList(counts = mat)
    mat <- edgeR::cpm(dge, log = TRUE, prior.count = 1)
    mat <- t(scale(t(mat)))
  }

  if (normalize == "none") {
    mat <- as.matrix(mat)
  }

  # Plot
  legend_name <- switch(
    normalize,
    "logCPM" = "log2 CPM",
    "zscore" = "Z-score",
    "none"   = "Expression"
  )
  plot <- ComplexHeatmap::Heatmap(mat, name = legend_name, row_names_side = "left", cluster_columns = FALSE, row_names_gp = gpar(fontsize = 10))

  # Draw in viewer
  ComplexHeatmap::draw(plot)

  # Export plot
  if (isTRUE(save)) {
    if (is.null(path)) {
      path <- file.path(getwd(), "heatmap_plot.png")
    }
    png(path, width = 2000, height = 1800, res = 300)
    draw(plot)
    dev.off()
    message("Heatmap plot saved in: ", path)
  }

  return(plot)
}
