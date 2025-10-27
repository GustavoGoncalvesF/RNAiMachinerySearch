# Script used to test the execution flow of the package

library(readxl)

# Input full table
file1 <- file.path("C:/Users/gugam/Documents/Data/mahanarva_annotation_report.xls")
file1 <- file.path("C:/Users/gugam/Documents/Data/metisa_annotation_report.xlsx")
file1 <- file.path("C:/Users/gugam/Documents/Data/highfive_annotation_report.xlsx")
file1 <- file.path("C:/Users/gugam/Documents/Data/centropomus_annotation_report.tsv")
file1 <- system.file("extdata", "partial_table2.xls", package = "RNAiMachinerySearch")
file1 <- system.file("extdata", "partial_table2.xls", package = "RNAiMachinerySearch")

# Input annotation tables
annotation_df <- read.table(file1, sep = "\t", header = TRUE, quote = "", comment.char = "", fill = TRUE)
annotation_df <- readxl::read_excel(file1, sheet = 1) # < for xlsxs

# Input expression matrix
file2 <- file.path("C:/Users/gugam/Documents/Data/raw_counts.txt")
expression_df <- read.table(file2, header = TRUE, row.names = 1, sep = " ", stringsAsFactors = FALSE)

# Input groups file
file3 <- system.file("extdata", "samples.txt", package = "RNAiMachinerySearch")
groups_df <- read.table(file3, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Testing search function
raw_rnai_hits <- search.rnai(annotation_df,"sprot_Top_BLASTX_hit")

#Testing filter function
filtered_rnai_hits <- expr.filter(raw_rnai_hits, expression_df, groups_df, cpm_cut_group = 1, cpm_cut_global = 20)

# Testing sunburst plot function
sunburst.plot(raw_rnai_hits)
sunburst.plot(filtered_rnai_hits)

# Testing heatmap plot function
heatmap.plot(raw_rnai_hits, expression_df)
heatmap.plot(filtered_rnai_hits, expression_df)
