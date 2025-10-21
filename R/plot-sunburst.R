# This function plots a sunburst chart to represent founded genes in each category, plus his functions in RNAi machinery

library(plotly)

sunburst.plot <- function(rnai_hits){
  data <- data.frame(
  labels = c(unique(rnai_hits$Category), rnai_hits$ProteinAnnotation),
  parents = c(rep("", length(unique(rnai_hits$Category))), rnai_hits$Category),
  hovertext = c(rep("", length(unique(rnai_hits$Category))), rnai_hits$Function)
    )
    return(
      plot_ly(
        data,
        labels = ~labels,
        parents = ~parents,
        type = "sunburst",
        branchvalues = "total",
        hovertext = ~hovertext,
        hoverinfo = "text+label"
      ) %>%
      layout(title = "Distribution of genes by categories")
)
}
