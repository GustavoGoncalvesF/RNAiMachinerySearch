#Função que plota o sunburst dos genes encontrados
library(plotly)

plot_sunburst <- function(tabela){
    data <- data.frame(
      labels = c(unique(tabela$Category), tabela$ProteinAnnotation),
      parents = c(rep("", length(unique(tabela$Category))), tabela$Category)
    )
return(
    plot_ly(
      data,
      labels = ~labels,
      parents = ~parents,
      type = "sunburst",
      branchvalues = "total"
    ) %>%
      layout(title = "Distribuição de genes por categoria")
)
}
