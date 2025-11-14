# tests/testthat/test-expr.filter.R

library(testthat)
library(RNAiMachinerySearch)
library(readxl)
library(edgeR)

# Load example annotation, expression and grouping data
annotation_file <- system.file("extdata", "annotation_report_example.xls", package = "RNAiMachinerySearch")
annotation_df <- read.table(annotation_file, sep = "\t", header = TRUE, quote = "", comment.char = "", fill = TRUE)

expression_file <- system.file("extdata", "raw_counts_example.txt", package = "RNAiMachinerySearch")
expression_df <- read.table(expression_file, header = TRUE, row.names = 1, sep = " ", stringsAsFactors = FALSE)

groups_file <- system.file("extdata", "samples_grouping_example.txt", package = "RNAiMachinerySearch")
groups_df <- read.table(groups_file, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Generate raw RNAi hits for testing
raw_rnai_hits <- search.rnai(annotation_df, "sprot_Top_BLASTX_hit")

# Test basic execution
test_that("expr.filter runs without errors and returns data.frame", {
  filtered <- expr.filter(raw_rnai_hits, expression_df, groups_df)

  # Output type
  expect_s3_class(filtered, "data.frame")

  # Columns preserved
  expect_true(all(c("GeneID", "ProteinAnnotation", "Category", "Function") %in% colnames(filtered)))
})

# Test thresholds effect
test_that("expr.filter respects CPM thresholds", {
  # Use high thresholds to remove some genes
  filtered_high <- expr.filter(raw_rnai_hits, expression_df, groups_df, cpm_cut_group = 10000, cpm_cut_global = 10000)

  # Expect fewer or zero genes than original raw_rnai_hits
  expect_lte(nrow(filtered_high), nrow(raw_rnai_hits))

  # Use low thresholds to retain most genes
  filtered_low <- expr.filter(raw_rnai_hits, expression_df, groups_df, cpm_cut_group = 0, cpm_cut_global = 0)
  expect_gte(nrow(filtered_low), nrow(filtered_high))
})

# Test invalid inputs
test_that("expr.filter handles invalid inputs correctly", {
  # raw_rnai_hits not a data.frame
  expect_error(expr.filter(list(), expression_df, groups_df))

  # expression_df not a data.frame
  expect_error(expr.filter(raw_rnai_hits, list(), groups_df))

  # groups_df missing required columns
  expect_error(expr.filter(raw_rnai_hits, expression_df, data.frame(A=1,B=2)))

  # Negative thresholds
  expect_error(expr.filter(raw_rnai_hits, expression_df, groups_df, cpm_cut_group = -1))
  expect_error(expr.filter(raw_rnai_hits, expression_df, groups_df, cpm_cut_global = -10))
})

# Test filtering logic
test_that("expr.filter output genes are subset of raw_rnai_hits", {
  filtered <- expr.filter(raw_rnai_hits, expression_df, groups_df)
  expect_true(all(filtered$GeneID %in% raw_rnai_hits$GeneID))
})

# Test reporting (non-fatal side effect)
test_that("expr.filter prints a report without errors", {
  expect_output(expr.filter(raw_rnai_hits, expression_df, groups_df), "~~ Report of genes filtering ~~")
})
