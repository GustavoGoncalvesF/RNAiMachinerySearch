# This function plots a heatmap to represent the expression of RNAi core genes among samples.

library(ComplexHeatmap)

heatmap.plot <- function(rnai_hits, expression_df){
  # Merge keeping only protein annotation's present in the expression dataframe, and preserve they order
  gene_present <- rnai_hits$GeneID[rnai_hits$GeneID %in% rownames(expression_df)]
  heatmap_data <- expression_df[gene_present, , drop = FALSE]

  # Input validation
  if (nrow(heatmap_data) ==0){
    stop("Cannot plot a heatmap because none of the RNAi hits are found in the expression matrix.")
  }

  # Normalizing data before plot
  heatmap_data_z <- t(scale(t(as.matrix(heatmap_data))))

  ComplexHeatmap::Heatmap(heatmap_data_z, name = "Z-score", row_names_side = "left", cluster_columns = FALSE, row_names_gp = gpar(fontsize = 10))
}
