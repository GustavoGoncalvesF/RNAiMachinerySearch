# tests/testthat/test-stackedbars.plot.R

library(testthat)
library(RNAiMachinerySearch)
library(plotly)

# Load example annotation, expression and grouping data
annotation_file <- system.file("extdata", "annotation_report_example.xls", package = "RNAiMachinerySearch")
annotation_df <- read.table(annotation_file, sep = "\t", header = TRUE, quote = "", comment.char = "", fill = TRUE)

expression_file <- system.file("extdata", "raw_counts_example.txt", package = "RNAiMachinerySearch")
expression_df <- read.table(expression_file, header = TRUE, row.names = 1, sep = " ", stringsAsFactors = FALSE)

groups_file <- system.file("extdata", "samples_grouping_example.txt", package = "RNAiMachinerySearch")
groups_df <- read.table(groups_file, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Generate raw RNAi hits for testing
raw_rnai_hits <- search.rnai(annotation_df, "sprot_Top_BLASTX_hit")

# Generate filtered RNAi hits for testing
filtered_rnai_hits <- expr.filter(raw_rnai_hits, expression_df, groups_df)

# Basic execution test
test_that("stackedbars.plot returns a plotly object", {
  result <- stackedbars.plot(raw_rnai_hits, expression_df, groups_df)
  expect_s3_class(result, "plotly")
})

# Input validation: missing required columns in rnai_hits
test_that("stackedbars.plot fails with missing required columns in rnai_hits", {
  bad_df <- raw_rnai_hits[, -1]  # remove GeneID column
  expect_error(
    stackedbars.plot(bad_df, expression_df, groups_df),
    "GeneID"
  )
})

# Input validation: no expression columns matching SAMPLE groups
test_that("stackedbars.plot fails when sample columns do not match SAMPLE names", {
  bad_expr <- expression_df
  colnames(bad_expr) <- c("A1", "A2", "B1")  # intentionally mismatched names
  expect_error(
    stackedbars.plot(raw_rnai_hits, bad_expr, groups_df),
    "No columns found for group"
  )
})

# Edge case: only one RNAi gene in rnai_hits
test_that("stackedbars.plot works when a category has only one gene", {
  rnai_one <- raw_rnai_hits[1, , drop = FALSE]
  expr_one <- expression_df[1, , drop = FALSE]

  result <- stackedbars.plot(rnai_one, expr_one, groups_df)
  expect_s3_class(result, "plotly")
})
