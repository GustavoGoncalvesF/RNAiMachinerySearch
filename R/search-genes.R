# Buscar os genes da maquinaria de RNAi a partir da tabela de anotação vinda em data.frame

#Esta função identifica os genes relacionados à maquinaria de RNAi

search.rnai <- function(tabela, coluna) {
  #Lista de genes da maquinaria
  categorias <- list(
   "Dicer family" = c("DCR1","DCR2","RNC"),
   "Argonautes" = c("AGO1","AGO2","AGO3","PIWI","AUB","SPNE"),
   "dsRNA binding" = c("LOQS","DGCR8"),
   "dsRNA transport/uptake" = c("SID1","SID2","CLH","INX2","EPN","SCRB1"),
   "Auxiliary RISC-associated" = c("ARMI","DDX3","CLP1","GAWKY","SND1","TSN","TSNAX","MAEL","HENMT","STAU1")
  )
  #Extraindo a coluna de interesse
  col_data <- tabela[[coluna]]

  #Informando erro caso coluna esteja vazia
  if (is.null(col_data)) {
    stop("A coluna especificada não foi encontrada na tabela.")
  }

  #Procurando correspondências
  resultados <- lapply(names(categorias), function(cat) {
    padroes <- categorias[[cat]]
    padroes_esc <- vapply(padroes, function(x) paste0("\\Q", x, "\\E"), FUN.VALUE = "")
    pattern <- paste0("(^|[^A-Za-z0-9])(", paste(padroes_esc, collapse = "|"), ")([^A-Za-z0-9]|$)")
    linhas <- grepl(pattern, col_data, ignore.case = TRUE, perl = TRUE)
    subset <- tabela[linhas, , drop = FALSE]

    if (nrow(subset) == 0) return(NULL)  # <- ignora categorias sem resultados

    subset$Categoria <- cat  # adiciona coluna com o nome da categoria
    return(subset)
  })
  # Unir tudo em um único data frame e renomear as colunas
  resultado_final <- dplyr::select(do.call(rbind, resultados), "X.gene_id","sprot_Top_BLASTX_hit","Categoria")
  colnames(resultado_final) <- c("GeneID", "ProteinAnnotation", "Category")
    return(resultado_final)
}
