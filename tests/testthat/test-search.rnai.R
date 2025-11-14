# tests/testthat/test-search.rnai.R

library(testthat)
library(RNAiMachinerySearch)
library(readxl)

# Load example annotation data
annotation_file <- system.file("extdata", "annotation_report_example.xls", package = "RNAiMachinerySearch")
annotation_df <- read.table(annotation_file, sep = "\t", header = TRUE, quote = "", comment.char = "", fill = TRUE)

# Test basic execution
test_that("search.rnai runs correctly with example data", {
  result <- search.rnai(annotation_df, "sprot_Top_BLASTX_hit")

  # Output type
  expect_s3_class(result, "data.frame")

  # Expected columns
  expect_true(all(c("GeneID", "ProteinAnnotation", "Category", "Function") %in% colnames(result)))

  # At least one contig
  expect_gt(nrow(result), 0)
})

# Output data integrity tests
test_that("output integrity of search.rnai", {
  result <- search.rnai(annotation_df, "sprot_Top_BLASTX_hit")

  # GeneID not duplicated
  expect_false(any(duplicated(result$GeneID)))

  # ProteinAnnotation in PROTEIN_ORGANISM format
  expect_true(all(grepl("^[A-Za-z0-9]+_[A-Za-z0-9]+", result$ProteinAnnotation)))

})

# Invalid inputs tests
test_that("search.rnai handles invalid inputs correctly", {

  # Non-existent column
  expect_error(search.rnai(annotation_df, "nonexistent_column"))

  # annotation_df its not a data.frame
  expect_error(search.rnai(list(), "sprot_Top_BLASTX_hit"))

  # column its NULL
  expect_error(search.rnai(annotation_df, NULL))

  # column its empty
  expect_error(search.rnai(annotation_df, ""))
})

# Minimum integration tests
test_that("search.rnai calls report.machinery and uses gene_list correctly", {
  result <- search.rnai(annotation_df, "sprot_Top_BLASTX_hit")

  # Column Category done using gene_list
  expect_false(any(is.na(result$Category)))

})
