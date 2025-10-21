# This function plots a heatmap to represent founded RNAi core genes expression amoung samples

library(ComplexHeatmap)

heatmap.plot <- function(rnai_hits, expr_matrix){
  # Merge keeping only protein annotation's present in the expression matrix, and preserve they order
  gene_present <- rnai_hits$GeneID[rnai_hits$GeneID %in% rownames(expr_matrix)]
  heatmap_data <- expr_matrix[gene_present, , drop = FALSE]
  rownames(heatmap_data) <- rnai_hits$ProteinAnnotation[rnai_hits$GeneID %in% gene_present]

  # Input validation
  if (is.null(heatmap_data)){
    stop("Cannot plot heatmap because no one of RNAi hits are finded in expression matrix.")
  }

  # Normalizing data before plot
  heatmap_data_z <- t(scale(t(as.matrix(heatmap_data))))

  ComplexHeatmap::Heatmap(heatmap_data_z, name = "Z-score", row_names_side = "left", cluster_columns = FALSE, row_names_gp = gpar(fontsize = 10))
}
