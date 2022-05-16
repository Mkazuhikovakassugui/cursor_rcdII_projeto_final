#----------------------------------------------------------------------------------------------#
#                    Projeto Final - Curso Linguagem R para Ciência de Dados II
#                                   Professores: Caio e Beatriz
#                                    Aluno: Marcio Vakassugui
#----------------------------------------------------------------------------------------------#



# 1. NSTALAÇÃO DE PACOTES E LEITURA DA BASE ---------------------------------------------------
# 
# remotes::install_github("curso-r/basesCursoR")
# imdb <- basesCursoR::pegar_base("imdb_completa")
# imdb_pessoas <- basesCursoR::pegar_base("imdb_pessoas")
# imdb_avaliacoes <- basesCursoR::pegar_base("imdb_avaliacoes")

library(lubridate)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(ggtext)
library(forcats)
library(readr)


write_rds(imdb, "data-raw/imdb.rds")
# write_rds(imdb_avaliacoes, "data-raw/imdb_avaliacoes.rds")
# write_rds(imdb_pessoas, "data-raw/imdb_pessoas.rds")

imdb <- read_rds("data-raw/imdb.rds")
#imdb_avaliacoes <- read_rds("data-raw/imdb_avaliacoes.rds")
#imdb_pessoas <- read_rds("data-raw/imdb_pessoas.rds")



# 2. QUAL O MÊS DO ANO COM O MAIOR NÚMERO DE FILMES: E O DIA DO ANO? --------------------------

# Deve-se descobrir qual mês em todo o conjunto de dados teve o maior número de filmes lançados.

## verificar os tipos das variáveis --------------------------

glimpse(imdb)

#### ------------------> data_lancamento = "chr"   obs. dados no formato y-m-d e apenas y
#### ------------------> receita         = "chr"   obs. note o texto como ex. $5000 na conversão 
#### ------------------> receita_eua     = "chr"   obs. note o texto como ex. %5000 na conversão

## entendendo a diferença entre ano e data de lançamento --------------------------------------

### observando o caso filme "Sable" - ano 2017 e data de lançamento 2019-01-01 entende-se que o 
### filme é de 2017, porém seu lançamento ocorreu no dia 01-01-2019

## alterar o tipo da variável data_lancamento de "chr" para "date -----------------------------

### desconsideraremos as dados com data de lançamento incompletos

imdb <- imdb |>    
  mutate(
    data_lancamento = ymd(data_lancamento)               # conversão do tipo "chr" para "date".
  )

### ------------------> 4563 linhas com dado "data de lancamento" = NA gerados
  
### analisar os valores missing
skimr::skim(imdb)

### ------------------> 4563 missings, que corresponde a 94,7 % de dados presentes

## Criar as colunas dia e mes da data de lançamento usando lubridate -------------------------

imdb_datas <- imdb |> 
  mutate(
    mes = floor_date(data_lancamento, "month") # arredonda para baixo o mês mais próximo
  )

### ------------------> com floor_date -> mês como o valor do primeiro dia do mês de lançamento.


### Obter um resumo com o ano, mês e quantidade de filmes lançados no referido mês.
mes_maior_lancamento <- imdb_datas |>                   # mês com o maior número de lançamentos.
  group_by(mes) |>        
  summarise(qte = n()) |>             
  separate(  
    col = mes,                         
    into = c("ano", "mes")) |>                                     
  drop_na() |> 
  slice_max(
    order_by = qte
  ) 


## obter o mês com o maior número de lançamento de filmes

mes_maior_lancamento |>                                                                # Tabela.
  kable(
    col.names = c("Ano de Lançamento", "Mês", "Quantidade de Filmes Lançados")
  ) |> 
  kable_styling(
    html_font = "get_schwifty",
    font_size = 12,       
    position = "center",
    full_width = TRUE, 
    fixed_thead = list(enabled = T, background = "#E9EBED"),
  ) |> 
  column_spec(1,
              bold = FALSE,
              background = "#022859", 
              color = "#ADBF6B") |>      
  column_spec(2, 
              bold = FALSE,
              background = "#022859",
              color = "#ADBF6B") |>
  column_spec(3,
              bold = FALSE,
              background = "#022859",
              color = "#ADBF6B")


## lista dos filmes deste mês com as melhores avaliações no mês de outubro

mes_maior_lancamento <- imdb_datas |>                   # mês com o maior número de lançamentos.
  group_by(mes) |>        
  summarise(qte = n(), titulo, data_lancamento, nota_imdb, num_avaliacoes, pais, duracao) |>
  filter(mes == "2018-10-1" & num_avaliacoes >= 10000) |> 
  arrange(desc(nota_imdb)) |> 
  slice_head(n=30) |> 
  View()
  
  


# 3) O dia com o maior número de lançamentos ao longo da história -------------------------

dia_maior_lancamento <- imdb |> 
  mutate(dia = day(data_lancamento)) |> 
  group_by(dia) |> 
  summarise(qtde = n()) |> 
  drop_na() |> 
  slice_max(order_by = qtde)

dia_maior_lancamento |>                                                                # Tabela.
  kable(
    col.names = c("Dia de Lançamento", 
                  "Quantidade de Filmes Lançados")
  ) |> 
  kable_styling(
    html_font = "get_schwifty",
    font_size = 10,       
    position = "center",
    full_width = TRUE, 
    fixed_thead = list(enabled = T, background = "#B2DDF7")
  ) |> 
  column_spec(1,
              bold = FALSE,
              background = "#022859", 
              color = "#FFFFFF"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              background = "#022859",
              color = "#FFFFFF"
  )


# 4) Top 5 dos paises com maior quantidade de filmes --------------------------------------

top_5_paises <- imdb |> 
  group_by(pais) |> 
  summarise(qtde = n()) |> 
  arrange(desc(qtde)) |> 
  slice_head(n=5)


top_5_paises |>                                                                        
  kbl(
    align = "l",                                            # alinhamento do texto do cabeçalho.
    col.names = c("País",                                           # define o nome das colunas.
                  "Quantidade de Filmes Lançados")
  ) |> 
  kable_styling(
    html_font = "get_schwifty",
    bootstrap_options = "basic",
    font_size = 10,       
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, background = "#B2DDF7")
  ) |> 
  column_spec(1,                                            # altera as configurações da tabela.
              bold = FALSE,
              background = "#022859", 
              color = "#FFFFFF",
              width = "5cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              background = "#022859",
              color = "#FFFFFF",
              width = "5cm"
  ) 


## Colocando o resultado em um gráfico de barras com os 20 paises

top_20_paises <- imdb |> 
  group_by(pais) |> 
  summarise(qtde = n()) |> 
  arrange(desc(qtde)) |> 
  slice_head(n=16)

cor <- c(                                        # paleta  de cores para os elementos gráficos.
  "#c42847", "#ffad05", "#7d5ba6", "#00bbf9", "#2bc016",
  "#B2DDF7", "#B2DDF7", "#B2DDF7", "#B2DDF7", "#B2DDF7",
  "#B2DDF7", "#B2DDF7", "#B2DDF7", "#B2DDF7", "#B2DDF7",
  "#365F6D"
)

top_20_paises |> 
  ggplot(aes(y = fct_reorder(pais, 
                             qtde, 
                             .desc = FALSE),
             x = qtde, 
             label = qtde
             )
         ) +
    geom_col(fill = cor)+
  geom_label(                                                            # formatar os labels.
    size = 3, 
    alpha = 0,
    label.size = NA,    
    fontface = "bold",    
    color = "#FFFFFF",
    hjust = -0.3
  ) +
   geom_curve(aes(y = 14,                                      # formatar curva para anotação.
                  x = 9000,  
                  xend = 15000,     
                  yend = 10   
   ),
   arrow = arrow(                                        # formatar a seta da curva.
     type = "closed",          
     length = unit(0.02, "npc")     
   ),
   curvature = -0.14,  
   color = "#B2DDF7"   
  ) +
  geom_curve(aes(y = 1,                                      # formatar curva para anotação.
                 x = 3000,  
                 xend = 8000,     
                 yend = 2   
  ),
  arrow = arrow(                                        # formatar a seta da curva.
    type = "closed",          
    length = unit(0.02, "npc")     
  ),
  curvature = 0,  
  color = "#B2DDF7"
  )+
  labs(
    title = "Países com maior quantidade de lançamentos de filmes",
    subtitle = "Destaque para os cinco paises com maiores notas",
    x = "Quantidade de Filmes Lançados",
    y = "Paises"
  )+
  scale_x_continuous(
    breaks = seq(0, 29000,10000),
    limits = c(0,29000)
  )+
  theme_classic()+
  theme(
    panel.background = element_rect(fill = "#022859"),                   # fundo do gráfico.
    plot.background = element_rect(fill = "#B2DDF7"),         # fundo da moldura retangular.
    legend.position = "none",
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_markdown(                                      # título do gráfico.
      size = 35,
      face = "plain",
      family = "FrankyOutline",
      hjust = 0.5,
      margin = unit(c(0.3, 0, 0.5, -0,2), "cm")
    ), 
    plot.subtitle = element_markdown(
      size = 10,
      family = "LexieReadable",   
      hjust = 0,
    ),
    text = element_text(                                                # textos do gráfica.
      family = "LexieReadable",    
      color = "#022859",
      size = 10,
      hjust = 0,
      face = "bold"
    ),
    axis.text.x = element_text(                                           # texto do eixo x.
      color = "#022859",       
      size = 7.5,
      face = "bold",
      margin = unit(c(0.3, 0, 0.5, 0), "cm")  
    ),
    axis.text.y = element_markdown(                                       # texto do eixo y.
      color = "#022859",    
      size = 9.5,
      face = "bold",
      family = "LexieReadable", 
    ),
    axis.ticks.x = element_line(color = "#1F1F1F"),                         # ticks do eixo x.
    axis.line.x = element_line(color = "#1F1F1F"),                   # cor da linha do eixo x.
    axis.ticks.y = element_line(color = "#1F1F1F"),             # padrões dos ticks do eixo x.
    axis.line.y = element_line(
      color = "#000000",                                           # cor da linha do eixo x.
      size = 0.4,
    ),
    axis.title = element_text(                                    # texto do título do eixo x.
      face = "bold",    
      size = 10,
      hjust = 0.5,
    )
  )+    
  annotate( "text",
            x = 15000, y = 9, label = "Relação dos cinco países com mais lançamentos 
            registrados na base IMDB",
            color = "#B2DDF7", 
            size = 3.4,
            family = "LexieReadable",
            fontface = "bold"
  )+
  annotate( "text",
            x = 14000, y = 2, label = "Décima sexta posição do Brasil",
            color = "#B2DDF7", 
            size = 3.4,
            family = "LexieReadable",
            fontface = "bold"
  )
  

skimr::skim(imdb)
glimpse(imdb)



# os filmes brasileiros com maior ranking

imdb |> 
  filter(pais == "Brazil" & num_avaliacoes >= 5000) |> 
  summarise( titulo, 
             ano, 
             genero, 
             nota_imdb,
             num_avaliacoes)|>
  arrange(desc(nota_imdb))





# 5) Listar todas as moedas nas colunas orcamento e receita -------------------------------

## Listar todas as moedas na coluna orcamento
# orcamento_sep <- imdb |> 
#   select(orcamento) |> 
#   drop_na() |> 
#   separate(
#     orcamento,
#     sep =  " ", 
#     into = c("moeda", "valor")
#   )
# 
# moedas_orcamento <- orcamento_sep |> 
#   select(moeda) |>
#   distinct(moeda) 
# 
# moedas <- as.vector(moedas_orcamento)
  
moedas_orcamento <- imdb |> 
  select(orcamento) |>
  mutate(
    moeda = str_extract(string = orcamento,
                        pattern = "[\\w | \\$].* " )
    ) |> 
  distinct(moeda) |> 
  drop_na()

moedas_orcamento <- moedas_orcamento |> 
  mutate(moeda = str_trim(moeda)) |> 
  mutate(moeda = str_replace(moeda, pattern = "\\$", replacement = "USD"))



## importar a tabela xls "siglas_moedas_paises"

tab_moedas <- readxl::read_xlsx("data-raw/siglas_moedas_paises.xlsx")


## juntar as tabelas moedas_receita e tab_moedas por meio da coluna moeda

moedas_orcamento_unificada<- left_join(x = moedas_orcamento, y = tab_moedas, by = "moeda", copy = TRUE) |> 
  distinct(moeda, divisa) |> 
  mutate(divisa = str_to_title(divisa))
  
moedas_orcamento_unificada |> 
  kbl()
