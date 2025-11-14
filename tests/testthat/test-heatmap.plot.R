# tests/testthat/test-heatmap.plot.R

library(testthat)
library(RNAiMachinerySearch)
library(ComplexHeatmap)
library(edgeR)

# Load example annotation, expression and grouping data
annotation_file <- system.file("extdata", "annotation_report_example.xls", package = "RNAiMachinerySearch")
annotation_df <- read.table(annotation_file, sep = "\t", header = TRUE, quote = "", comment.char = "", fill = TRUE)

expression_file <- system.file("extdata", "raw_counts_example.txt", package = "RNAiMachinerySearch")
expression_df <- read.table(expression_file, header = TRUE, row.names = 1, sep = " ", stringsAsFactors = FALSE)

# Generate raw RNAi hits for testing
raw_rnai_hits <- search.rnai(annotation_df, "sprot_Top_BLASTX_hit")

# Test basic execution
test_that("heatmap.plot returns a ComplexHeatmap object", {
  result <- heatmap.plot(raw_rnai_hits, expression_df)
  expect_s4_class(result, "Heatmap")
})

# Test logCPM normalization
test_that("heatmap.plot handles logCPM normalization", {
  result <- heatmap.plot(raw_rnai_hits, expression_df, normalize = "logCPM")
  expect_s4_class(result, "Heatmap")
})

# Test z-score normalization
test_that("heatmap.plot handles zscore normalization", {
  result <- heatmap.plot(raw_rnai_hits, expression_df, normalize = "zscore")
  expect_s4_class(result, "Heatmap")
})

# Test none normalization
test_that("heatmap.plot handles no normalization", {
  result <- heatmap.plot(raw_rnai_hits, expression_df, normalize = "none")
  expect_s4_class(result, "Heatmap")
})

# Test fails if no RNAi hits are in expression data
test_that("heatmap.plot errors if no matching genes found", {
  bad_expr <- expression_df[0, , drop = FALSE]
  expect_error(heatmap.plot(raw_rnai_hits, bad_expr), "'expression_df' cannot be empty.")
})
