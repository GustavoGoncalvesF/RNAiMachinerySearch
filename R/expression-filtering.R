# This function filters the raw rnai hits data frame to keep only relevant contigs in the downstream analysis
# The first filter keep a gene if it has a cpm >= to "cpm_cut_group" in all samples of a group (group minimum expression criterion)
# The second filter keep a gene if it has cpm >= to "cpm_cut_global" in at least the minimal replicates number (global minimum expression criterion)

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

  # Group samples and applies first filter (per groups)
  keep_genes <- apply(cpm, 1, function(gene_row) {
    tapply(seq_along(gene_row), samples, function(idxs) all(gene_row[idxs] >= cpm_cut_group)) |> any()
  })
  expr_filtered <- expr_sub[keep_genes, , drop = FALSE]

  # Apply second filter (global)
  keep_genes2 <- rowSums(edgeR::cpm(expr_filtered) >= cpm_cut_global) >= min_reps
  expr_filtered <- expr_filtered[keep_genes2, , drop = FALSE]

  # Update found genes list with the filtered ones
  filtered_rnai_hits <- raw_rnai_hits[raw_rnai_hits$GeneID %in% rownames(expr_filtered), , drop = FALSE]

  # Report
  total_genes <- nrow(expr_sub)
  after_group <- sum(keep_genes)
  after_global <- sum(keep_genes2)

  cat("~~ Report of genes filtering ~~\n",
      "Total pre filter:", total_genes, "genes.\n",
      "Removed by group minimum CPM criterion:", total_genes - after_group, "genes.\n",
      "Removed by global minimum CPM criterion:", after_group - after_global, "genes.\n",
      "Total post filter:", after_global, "genes.\n")

  # Return filtered rnai_hits dataframe
  return(filtered_rnai_hits)
}
