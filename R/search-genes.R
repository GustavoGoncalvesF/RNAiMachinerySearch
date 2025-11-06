#' @title Search for RNAi Core Genes in Annotation Data
#'
#' @description
#' Identifies genes related to the RNA interference (RNAi) machinery in a given annotation data frame.
#' The function compares UniProt annotation names against a curated catalog of RNAi core components,
#' classifies each match by functional category, and outputs a structured data frame ready for downstream analyses.
#'
#' @param annotation_df A data frame containing functional annotation results (e.g., Trinotate output).
#' @param column A character string specifying the column name within \code{annotation_df} where UniProt annotation names are stored.
#'
#' @return
#' A data frame \code{raw_rnai_hits} containing:
#' \itemize{
#'   \item \strong{GeneID}: The gene or contig identifier from the input annotation.
#'   \item \strong{ProteinAnnotation}: The matched UniProt protein name.
#'   \item \strong{Category}: The RNAi functional category assigned to each match.
#'   \item \strong{Function}: The molecular function of each identified RNAi core gene.
#' }
#'
#' @details
#' The function automatically filters annotation entries based on a curated catalog \code{gene_list.txt}
#' of RNAi machinery genes, stored in the package's \code{extdata} directory.
#' Input validation is internally performed through \code{validate.inputs()} to ensure consistent formats.
#' Different contigs with the same UniProt name annotation will be kept with a prefix ".1, .2, [...]".
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' annotation <- read.table("annotation_report.xls")
#' rnai_hits <- search.rnai(annotation_df = annotation, column = "Top_BLASTX_hit")
#' }
#'
#' @importFrom dplyr select
#' @export

search.rnai <- function(annotation_df, column) {
  #Load catalog of machinery's core genes
  gene_list <- read.table(system.file("extdata","gene_list.txt", package = "RNAiMachinerySearch"), sep = ",", header = TRUE, stringsAsFactors = FALSE)
  catalog <- split(gene_list$Gene, gene_list$Category)

  # Input validation
  validate.inputs(annotation_df = annotation_df, column = column)

  # Pick up the functional annotation column
  col_data <- annotation_df[[column]]

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
  raw_rnai_hits <- dplyr::select(do.call(rbind, matches), 1,column,"Category")
  colnames(raw_rnai_hits) <- c("GeneID", "ProteinAnnotation", "Category")

  # Filtering the annotation column to keep only UniProt name
  raw_rnai_hits$ProteinAnnotation <- sub('.*?([A-Za-z0-9_]+)_([A-Za-z0-9_]+).*', '\\1_\\2', raw_rnai_hits$ProteinAnnotation)

  # Add functions column based in genes short name (them remove geneshort column)
  raw_rnai_hits$GeneShort <- sub("_.*", "", raw_rnai_hits$ProteinAnnotation)
  raw_rnai_hits$Function <- gene_list$Function[match(raw_rnai_hits$GeneShort, gene_list$Gene)]
  raw_rnai_hits <- raw_rnai_hits[, -which(names(raw_rnai_hits) == "GeneShort")]

  # Remove duplicated GeneIDs
  raw_rnai_hits <- raw_rnai_hits[!duplicated(raw_rnai_hits$GeneID), ]

  # Make ProteinAnnotation unique by putting sufix in duplicates
  raw_rnai_hits$ProteinAnnotation <- make.unique(raw_rnai_hits$ProteinAnnotation)

  report.machinery(raw_rnai_hits)
    return(raw_rnai_hits)
}
