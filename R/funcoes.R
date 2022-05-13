# Funções 


# 1) Função que padroniza os nomes das colunas dos datasets -----------------------------------
padroniza_colunas <- function(nome_arquivo) {
  clean_names(nome_arquivo)
}


# 2) Função para inserção de coluna e valores -------------------------------------------------

cria_coluna <- function(base, valor) {
  mutate(base, region =valor)
}


# 3) Função para inserção da coluna classe ----------------------------------------------------

cria_coluna_classe <- function(base, valor) {
  mutate(base, class =valor)
}
