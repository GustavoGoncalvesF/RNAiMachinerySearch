# This function do counts about match genes and print total/per categories numbers

report.machinery <- function(rnai_hits) {
  # Define categories to count
  expected_categories <- c(
    "Argonautes",
    "Auxiliary RISC-associated",
    "Dicer family",
    "dsRNA binding",
    "dsRNA transport/uptake"
  )

  # Do counts per categories keeping the expected levels
  counts_df <- as.data.frame(table(factor(rnai_hits$Category, levels = expected_categories)))
  colnames(counts_df) <- c("Category","Count")

  # Named vector to lookup
  counts_named <- setNames(as.integer(counts_df$Count), counts_df$Category)

  # Print
  cat("##########~~ Reporting hits of core genes from RNAi machinery ~~##########\nTotal:", nrow(rnai_hits), "hits.\n")

  for (cat_name in expected_categories) {
    count <- counts_named[cat_name]
    if (is.na(count)) count <- 0
    cat(cat_name, ":", count, "hits.\n")
  }
}
