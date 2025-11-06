#' @title Plot an Interactive Sunburst Chart of RNAi Hits
#'
#' @description
#' This function generates an interactive sunburst chart to visualize the distribution
#' of RNAi core genes by category and function within a transcriptome. The chart allows
#' users to explore each RNAi category and its associated genes in a hierarchical structure.
#'
#' @param rnai_hits A data frame containing RNAi hits, typically obtained from
#'   \code{\link{search.rnai}} or \code{\link{expr.filter}}. Must contain the columns
#'   "ProteinAnnotation", "Category", and "Function".
#' @param save Logical; if \code{TRUE}, the plot will be saved as an HTML file.
#'   Default is \code{FALSE}.
#' @param path Character string specifying the file path to save the HTML file.
#'   If \code{NULL}, the plot will be saved in the current working directory
#'   with the default name "sunburst_plot.html".
#'
#' @return A \code{plotly} object representing the interactive sunburst chart.
#'
#' @examples
#' \dontrun{
#' # Generate and display sunburst chart
#' raw_rnai_hits <- search.rnai(annotation_df = annotation, column = "Top_BLASTX_hit")
#' sunburst_plot <- sunburst.plot(raw_rnai_hits)
#'
#' # Save the plot
#' sunburst.plot(raw_rnai_hits, save = TRUE, path = "my_sunburst.html")
#' }
#'
#' @importFrom plotly plot_ly layout %>%
#' @importFrom htmlwidgets saveWidget
#' @importFrom stats setNames
#' @importFrom utils read.csv
#'
#' @export

sunburst.plot <- function(rnai_hits, save = FALSE, path = NULL){
  # Input validation
  validate.inputs(rnai_hits = rnai_hits,
                  save = save,
                  path = path
  )

  # Catalog of colors by category
  category_colors <- read.csv(system.file("extdata", "category_colors.txt", package = "RNAiMachinerySearch"), stringsAsFactors = FALSE)
  category_colors <- setNames(category_colors$Color, category_colors$Category)

  # Prepare data frame from rna_hits
  data <- data.frame(
  labels = c(unique(rnai_hits$Category), rnai_hits$ProteinAnnotation),
  parents = c(rep("", length(unique(rnai_hits$Category))), rnai_hits$Category),
  hovertext = c(rep("", length(unique(rnai_hits$Category))), rnai_hits$Function)
  )

  # Assign colors
  cat_colors <- category_colors[unique(rnai_hits$Category)]
  prot_colors <- category_colors[rnai_hits$Category]
  data$color <- c(cat_colors, prot_colors)

  # Ploting with plotly
  plot <- plotly::plot_ly(
    data,
    labels = ~labels,
    parents = ~parents,
    type = "sunburst",
    branchvalues = "total",
    hovertext = ~hovertext,
    hoverinfo = "text+label",
    marker = list(colors = ~color)
   ) %>%
    plotly::layout(title = "Distribution of genes by categories")

  # Export plot
  if (isTRUE(save)) {
    if (is.null(path)) {
      path <- file.path(getwd(), "sunburst_plot.html")
    }
    htmlwidgets::saveWidget(plot, path)
    message("Sunburst plot saved in: ", path)
  }

  return(plot)
}
