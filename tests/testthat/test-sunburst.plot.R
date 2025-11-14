# tests/testthat/test-sunburst.plot.R

library(testthat)
library(RNAiMachinerySearch)
library(plotly)
library(htmlwidgets)
library(readxl)

# Load example annotation and generate RNAi hits
annotation_file <- system.file("extdata", "annotation_report_example.xls", package = "RNAiMachinerySearch")
annotation_df <- read.table(annotation_file, sep = "\t", header = TRUE, quote = "", comment.char = "", fill = TRUE)

raw_rnai_hits <- search.rnai(annotation_df, "sprot_Top_BLASTX_hit")

# Test basic execution
test_that("sunburst.plot runs without errors and returns a plotly object", {
  plot <- sunburst.plot(raw_rnai_hits)

  expect_s3_class(plot, "plotly")
})

# Test input validation
test_that("sunburst.plot handles invalid inputs gracefully", {
  # rnai_hits not a data.frame
  expect_error(sunburst.plot(list()))

  # save not logical
  expect_error(sunburst.plot(raw_rnai_hits, save = "yes"))

  # path not character
  expect_error(sunburst.plot(raw_rnai_hits, save = TRUE, path = 123))
})

# Test minimal dataset
test_that("sunburst.plot handles minimal RNAi hits data", {
  minimal_hits <- raw_rnai_hits[1:2,]
  plot <- sunburst.plot(minimal_hits)
  expect_s3_class(plot, "plotly")
})
