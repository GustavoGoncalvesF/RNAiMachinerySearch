# RNAiMachinerySearch

RNAiMachinerySearch is an R package designed to streamline the identification and exploration of core genes involved in RNA interference (RNAi).  
This machinery is essential for gene silencing mediated by double-stranded RNA (dsRNA) and can help researchers understand and manipulate RNAi pathways in their organisms of interest.  

With RNAiMachinerySearch, you can quickly screen, filter, and visualize RNAi-related genes from transcriptomic datasets, facilitating downstream analyses and functional studies.

![Logo](https://github.com/GustavoGoncalvesF/RNAiMachinerySearch/blob/master/man/figures/logo.png)

## Installation
```r
# Required packages
install.packages("devtools")
install.packages("BiocManager")
install.packages("edgeR")

# Install RNAiMachinerySearch
devtools::install_github("GustavoGoncalvesF/RNAiMachinerySearch",
                          repos = c("https://cloud.r-project.org/",
                                   BiocManager::repositories()))
```
## Workflow

The package workflow allows you to:

1. Search for RNAi core genes from annotation data.

2. Filter hits based on expression thresholds.

3. Visualize results using interactive plots (sunburst, stacked bar charts, heatmaps).

![Workflow](https://github.com/GustavoGoncalvesF/RNAiMachinerySearch/blob/master/man/figures/workflow.png)

## User's Guide

A detailed User's Guide is available here:

[Download Users Guide (PDF)](docs/RNAiMachinerySearchUsersGuide.pdf)

## Quick Example
```r
library(RNAiMachinerySearch)

# Load example data
annotation_file <- system.file("extdata", "annotation_report_example.xls", package = "RNAiMachinerySearch")
annotation_df <- read.table(annotation_file, sep = "\t", header = TRUE, quote = "", fill = TRUE)

expression_file <- system.file("extdata", "raw_counts_example.txt", package = "RNAiMachinerySearch")
expression_df <- read.table(expression_file, header = TRUE, row.names = 1, sep = " ")

groups_file <- system.file("extdata", "samples_grouping_example.txt", package = "RNAiMachinerySearch")
groups_df <- read.table(groups_file, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Search and filter RNAi hits
raw_rnai_hits <- search.rnai(annotation_df, "sprot_Top_BLASTX_hit")
filtered_rnai_hits <- expr.filter(raw_rnai_hits, expression_df, groups_df)

# Generate plots
sunburst.plot(filtered_rnai_hits)
stackedbars.plot(filtered_rnai_hits, expression_df, groups_df)
heatmap.plot(filtered_rnai_hits, expression_df)
```
## Notes
- Example data is provided within the package for quick testing.
- Generated plots can be optionally saved as interactive HTML (sunburst and stacked bar) or PNG (heatmap).
