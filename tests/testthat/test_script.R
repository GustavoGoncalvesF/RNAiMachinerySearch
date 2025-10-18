# Script used to test the execution flow of the package

# Input full table
  file <- file.path("C:/Users/gugam/Documents/RNAseq/trinotate_annotation_report.xls")

# Input partial tables
  file <- system.file("extdata", "partial_table1.xls", package = "rnaiMachinerySearch")
  annotation_df <- read.table(file,
                      sep = "\t",
                      header = TRUE,
                      quote = "",
                      comment.char = "",
                      fill = TRUE)

# Testing search function
rnai_hits <- search.rnai(annotation_df,"sprot_Top_BLASTX_hit")

# Testing report print function
report.machinery(rnai_hits)

# Testing sunburst plot function
sunburst.plot(rnai_hits)

