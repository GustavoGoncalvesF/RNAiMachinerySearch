# Searching the core genes of RNAi machinery from a annotation table coming in data.frame format

# This function identify the genes based in they UniProt names
library(stringr)

search.rnai <- function(annotation_df, column) {
  # Catalog of machinery's core genes
  gene_list <- read.table(system.file("extdata","gene_list.txt", package = "rnaiMachinerySearch"), sep = ",", header = TRUE, stringsAsFactors = FALSE)
  catalog <- split(gene_list$Gene, gene_list$Category)

  # Pick up the functional annotation column
  col_data <- annotation_df[[column]]

  # Input validation
  if (is.null(col_data)) {
    stop("Cannot find the speficied column in the dataframe, please check the name and try again.")
  }

  # Looking for matches between catalog and annotations
  matches <- lapply(names(catalog), function(cat) {
    patterns <- catalog[[cat]]
    patterns_esc <- vapply(patterns, function(x) paste0("\\Q", x, "\\E"), FUN.VALUE = "")
    type <- paste0("(^|[^A-Za-z0-9])(", paste(patterns_esc, collapse = "|"), ")([^A-Za-z0-9]|$)") #< ensures the annotation be exactly the same as gene name
    lines <- grepl(type, col_data, ignore.case = TRUE, perl = TRUE)
    subset <- annotation_df[lines, , drop = FALSE]

    if (nrow(subset) == 0) return(NULL)  # Ignore categories without matches

    subset$Category <- cat  # Add categories column
    return(subset)
  })
  # Join every single one match in a unique data frame and rename columns
  rnai_hits <- dplyr::select(do.call(rbind, matches), 1,column,"Category")
  colnames(rnai_hits) <- c("GeneID", "ProteinAnnotation", "Category")

  # Filtering the annotation column to keep only UniProt name and removing duplicates contigs results
  rnai_hits$ProteinAnnotation <- sub('.*?([A-Za-z0-9_]+)_([A-Za-z0-9_]+).*', '\\1_\\2', rnai_hits$ProteinAnnotation)

  # Add functions column based in genes short name
  rnai_hits$GeneShort <- sub("_.*", "", rnai_hits$ProteinAnnotation)
  rnai_hits$Function <- gene_list$Function[match(rnai_hits$GeneShort, gene_list$Gene)]

  report.machinery(rnai_hits)
    return(rnai_hits)

}
