#Função que plota o sunburst dos genes encontrados
library(plotly)

sunburst.plot <- function(tabela){
    data <- data.frame(
      labels = c(unique(tabela$Category), tabela$ProteinAnnotation),
      parents = c(rep("", length(unique(tabela$Category))), tabela$Category),
      hovertext = c(rep("", length(unique(tabela$Category))), tabela$Function)
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
      layout(title = "Distribuição de genes por categoria")
)
}
