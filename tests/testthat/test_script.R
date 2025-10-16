# Script utilizado para testar o pacote

# Chamando tabela 1 para testes
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
plot_sunburst(resultado)

resultado$ProteinAnnotation <- sub('^"|\\^.*', '', resultado$ProteinAnnotation)
