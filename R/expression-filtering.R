# This function filters the rnai_hits dataframe to keep only relevant contigs in the downstream analysis
library(edgeR)
library(dplyr)

expr.filter <- function(raw_rnai_hits, expression_df, groups_df, cpm_cut = 1){
  # Input validation
  if (!all(c("SAMPLE","REP") %in% colnames(groups_df))) stop("Samples files must have exactly SAMPLE and REP columns.")

  # Assemble vector of samples in expression_df columns order
  samples_order <- colnames(expression_df)
  if (is.null(samples_order)) stop("Expression dataframe dont have correct column names.")

  # Verify if all expression_df columns are in samples files
  if (!all(samples_order %in% groups_df$SAMPLE)) stop("Some column names in expression dataframe cannot be finded in samples file.")

  # Group columns index for SAMPLE
  groups_df <- groups_df[match(samples_order, groups_df$SAMPLE), ]
  sample_groups <- split(seq_along(samples_order), groups_df$SAMPLE)

  # Get CPM
  cpm_matrix <- edgeR::cpm(as.matrix(expression_df))

  # For each group, test "CPM >= cpm_cut in each replicate
  keep_by_group <- sapply(sample_groups, function(cols){
    rowSums(cpm_mat[, cols, drop = FALSE] >= cpm_cut) == lenght(cols)
  })

  # Keep if its TRUE in almost one group
  if (is.matrix(keep_by_group)){
    keep <- apply(keep_by_group, 1, any)
  } else{
    keep <- keep_by_group
  }
}

