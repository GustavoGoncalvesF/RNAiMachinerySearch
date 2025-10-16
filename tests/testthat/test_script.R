# Script utilizado para testar o pacote

# Chamando tabela 1 para testes

  arquivo <- system.file("extdata", "tabela.teste1.xls", package = "rnaiMachinerySearch")
  tabela <- readxl::read_excel(arquivo)
