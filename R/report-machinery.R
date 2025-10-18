# This function do counts about match genes and print total/per categories numbers

report.machinery <- function(rnai_hits) {
  # Set Category column as factor, to avoid disarrange
  rnai_hits$Category <- factor(rnai_hits$Category, levels = unique(rnai_hits$Category))
  category_counts <- dplyr::count(rnai_hits, Category, sort=FALSE)
  cat("##~~ Reporting hits of core genes from RNAi machinery ~~##\n", "Total: ", nrow(rnai_hits), "hits.\n", "Argonautes: ", category_counts[1,2], "hits.\n", "Auxiliary RISC-associated: ", category_counts[2,2], "hits.\n", "Dicer family: ", category_counts[3,2], "hits.\n", "dsRNA binding: ", category_counts[4,2], "hits.\n", "dsRNA transport/uptake: ", category_counts[5,2], "hits.")

}
