# Script utilizado para testar o pacote

# Chamando tabela 1 para testes
library (readxl)
  arquivo <- system.file("extdata", "tabela.teste1.xls", package = "rnaiMachinerySearch")
  tabela <- read.table(arquivo,
                      sep = "\t",
                      header = TRUE,
                      quote = "",
                      comment.char = "",
                      fill = TRUE)
