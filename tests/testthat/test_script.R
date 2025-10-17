<<<<<<< HEAD
# Script utilizado para testar o pacote #vg
=======
# Script utilizado para testar o pacote #vgss
>>>>>>> 99866699dc0dc3c78e815c150ccf092c2d2a7cf0

# Chamando tabela 1 para testes
  arquivo <- file.path("C:/Users/gugam/Documents/RNAseq/trinotate_annotation_report.xls")

# Chamando tabela para testes
  arquivo <- system.file("extdata", "tabela.teste2.xls", package = "rnaiMachinerySearch")
  tabela <- read.table(arquivo,
                      sep = "\t",
                      header = TRUE,
                      quote = "",
                      comment.char = "",
                      fill = TRUE)
# Testando a busca
resultado <- search.rnai(tabela,"sprot_Top_BLASTX_hit")

# Testando print report
report.machinery(resultado)

 # Testando plotly
sunburst.plot(resultado)

