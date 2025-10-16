# Função que faz contagem de genes encontrados e printa total/categorias

report.machinery <- function(tabela) {

  #primeiro definir a coluna category como fator, para evitar que desorganize na contagem
  tabela$Category <- factor(tabela$Category, levels = unique(tabela$Category))
  contagem <- dplyr::count(tabela, Category, sort=FALSE)
  cat("Relatório de genes da maquinaria de RNAi encontrados:\n", "Total global: ", nrow(tabela), "genes.\n", "Dicer family: ", contagem[1,2], "genes.\n", "Argonautes: ", contagem[2,2], "genes.\n", "dsRNA binding: ", contagem[3,2], "genes.\n", "dsRNA transport/uptake: ", contagem[4,2], "genes.\n", "Auxiliary RISC-associated: ", contagem[5,2], "genes.")

}
