# This function filters the rnai_hits dataframe to keep only relevant contigs in the downstream analysis
# The first filter keep a gene if it has a cpm >= to "cpm_cut" in all samples of a group
# The second filter keep a gene if it has cpm >= 20 in at least 3 samples (global minimum expression criterion)

library(edgeR)

expr.filter <- function(raw_rnai_hits, expression_df, groups_df, cpm_cut_group = 1, cpm_cut_global = 10){
  # Merge keeping only protein annotation's present in the expression dataframe, and preserve they order
  gene_present <- raw_rnai_hits$GeneID[raw_rnai_hits$GeneID %in% rownames(expression_df)]
  expr_sub <- expression_df[gene_present, , drop = FALSE]

  # Calculate CPM with edgeR
  cpm <- edgeR::cpm(expr_sub)

  #Pick up the groups and number of replicates
  groups <- groups_df$REP
  samples <- groups_df$SAMPLE
  min_reps <- max(table(groups_df$SAMPLE))

  # Group samples and aplly  first filter (per groups)
  keep_genes <- apply(cpm, 1, function(gene_row) {
    tapply(seq_along(gene_row), samples, function(idxs) all(gene_row[idxs] >= cpm_cut_group)) |> any()
  })
  expr_filtered <- expr_sub[keep_genes, , drop = FALSE]

  # Apply second filter (global)
  keep_genes2 <- rowSums(edgeR::cpm(expr_filtered) >= cpm_cut_global) >= min_reps
  expr_filtered <- expr_filtered[keep_genes2, , drop = FALSE]

  # Update found genes list with the filtered ones
  filtered_rnai_hits <- raw_rnai_hits[raw_rnai_hits$GeneID %in% rownames(expr_filtered), , drop = FALSE]

  # Return filtered rnai_hits dataframe
  return(filtered_rnai_hits)
}

