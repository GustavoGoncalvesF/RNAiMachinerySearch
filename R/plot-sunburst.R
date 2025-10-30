# This function plots a sunburst chart to represent founded genes in each category, plus his functions in RNAi machinery

library(plotly)
library(htmlwidgets)

sunburst.plot <- function(rnai_hits, save = FALSE, path = NULL){
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
  plot <- plot_ly(
    data,
    labels = ~labels,
    parents = ~parents,
    type = "sunburst",
    branchvalues = "total",
    hovertext = ~hovertext,
    hoverinfo = "text+label",
    marker = list(colors = ~color)
   ) %>%
  layout(title = "Distribution of genes by categories")

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
