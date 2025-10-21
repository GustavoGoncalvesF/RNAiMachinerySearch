# Script used to test the execution flow of the package

# Input full table
file1 <- file.path("C:/Users/gugam/Documents/Data/mahanarva_annotation_report.xls")
file1 <- file.path("C:/Users/gugam/OneDrive/Documentos/Data/mahanarva_annotation_report.xls")
file1 <- file.path("C:/Users/gugam/Documents/Data/centropomus_annotation_report.tsv")

# Input partial tables
file1 <- system.file("extdata", "partial_table1.xls", package = "rnaiMachinerySearch")
annotation_df <- read.table(file1, sep = "\t", header = TRUE, quote = "", comment.char = "", fill = TRUE)

# Input expression matrix
file2 <- file.path("C:/Users/gugam/Documents/Data/raw_counts_filtered.txt")
expr_matrix <- read.table(file2, header = TRUE, row.names = 1, sep = " ", stringsAsFactors = FALSE)

# Testing search function
rnai_hits <- search.rnai(annotation_df,"sprot_Top_BLASTX_hit")

# Testing sunburst plot function
sunburst.plot(rnai_hits)

# Testing heatmap plot function
heatmap.plot(rnai_hits, expr_matrix)
