#----------------------------------------------------------------------------------------------#
#                    Projeto Final - Curso Linguagem R para Ciência de Dados II
#                                   Professores: Caio e Beatriz
#                                    Aluno: Marcio Vakassugui
#----------------------------------------------------------------------------------------------#



# 1. NSTALAÇÃO DE PACOTES ---------------------------------------------------------------------

library(readr)                                                                                      # leitura.
library(tidyverse)                                                                              # manipulação.
library(lubridate)                                                                     # manipulação de datas.
library(forcats)                                                                    # maninpulação de factors.
library(kableExtra)                                                                                 # tabelas.
library(plotly)                                                                      # extensão para gráficos.
library(gghighlight)                                                                 # extensão para gráficos.
library(ggtext)                                                                      # extensão para gráficos.
library(ggthemes)                                                                     # temas para relatórios.
library(here)                                                                           # endereços relativos.
library(scales)                                                               # formatação de labels de plots.
source("R/funcoes_imdb.R")                                                      # leitura das funções criadas.

# 2. CARREGAR AS BASES -------------------------------------------------------------------------
imdb <- basesCursoR::pegar_base("imdb_completa")
imdb_pessoas <- basesCursoR::pegar_base("imdb_pessoas")
imdb_avaliacoes <- basesCursoR::pegar_base("imdb_avaliacoes")

# 2. ANÁLISES ----------------------------------------------------------------------------------

## 2.1 QUAL O MÊS DO ANO COM O MAIOR NÚMERO DE FILMES: E O DIA DO ANO? -------------------------

### Descobrir qual mês em todo o conjunto de dados teve o maior número de filmes lançados

### verificar os tipos das variáveis

glimpse(imdb)

#### ------------------> data_lancamento = "chr"
#### ------------------> receita         = "chr"
#### ------------------> receita_eua     = "chr"
#### ------------------> orcamento       = "chr"

glimpse(imdb_pessoas)
glimpse(imdb_avaliacoes)

### Diferença entre ano e data de lançamento

### Ano              =   ano de produção 
### Data_lancamento  =   data da estreia do filme

### Alterar o tipo de data_lancamento para date

imdb_novo <- imdb |>    
  mutate(
    data_lancamento = ymd(data_lancamento)                              # conversão do tipo "chr" para "date".
  )

### ------------------> 4563 linhas com dado "data de lancamento" = NA gerados por coerção
  
### Analisar os valores missing

skimr::skim(imdb_novo)

### ------------------> 27,6 % de dados fornecidos em orcamento
### ------------------> 36,1 % de dados fornecidos em receita
### ------------------> 17,9 % de dadow fornecidos em receita_eua
### ------------------> demais dados com índices de preenchimento superiores a 86,3%


### Gráfico "Evolução da Indústria Cinematográfica ------------------------------------------------

### Curiosidade da base, descobrir como ocorreu a evolução da indústria cinematográfica em termos de quatidade
### de obras produzidas ao longo da história. Gerar gráfico para a seção 1 do relatório com os resultados.

p01 <- imdb_novo |> 
  mutate(ano_lancamento = year(data_lancamento)) |>                     # cria a coluna com ano de lançamento.
  select(ano_lancamento) |> 
  drop_na() |>
  group_by(ano_lancamento) |> 
  summarise(qte = n()) |>                                                 
  slice_head(n=112) |>                 # excluimos os dados de 2021, pois possui dados de apenas alguns meses.        
  ggplot()+
  aes(x = ano_lancamento, 
      y = qte)+
  annotate("rect",                  # cria um destaque em azul para os anos de maior produção cinematográfica.
           xmin = 2010,
           xmax = 2026,
           ymin = 2950, 
           ymax = 3500,
           fill = "#99D8FF", 
           alpha = 0.3
  ) +
  annotate("text",                                     # cria anotação de texto para a área destacada em azul.
           x = 1970, 
           y = 3281,
           label = "Auge da produção cinematográfica", 
           size = 3
  ) +
  geom_segment(aes(y = 3281,                                                       # cria a linha da anotação.
                   x = 1992,  
                   xend = 2009,     
                   yend = 3281   
  ), 
  size = 0.3
  ) +
  geom_line(color = "#4C586B")+          # gráfico lollypop é formado pela combinação dos geom's line e point.
  geom_point(color = "#A71D31",                                                           
             shape = 7,
             size = 0.7
  )+
  gghighlight(ano_lancamento >=1946,                  # destaca no gráfico os dados dos anos a partir de 1960.
              unhighlighted_params = list(colour = "#4C586B")
  )+
  labs(title = "Total de filmes por ano",                           
       x = "
    Ano",
       y = "Quantidade 
    
    "              # distância do titulo y dos valores com axix.text.y e margin não possível devido ao plotly.
  )+
  scale_x_continuous(breaks = seq(0,                                    
                                  2022, 
                                  10
                                  )
  ) +
  scale_y_continuous(breaks = seq(0,                                  
                                  4000, 
                                  500
  ),
  labels = number_format(accuracy = 0.1,
                         big.mark = ".",
                         decimal.mark = ",",
  )
  ) +
  theme_classic()   

# Gráfico dinâmico com plotly

p01 |> 
  ggplotly()  


### Tabela 01: Maior quantidade de filmes em um mês ------------------------------------------------

### nesta análise vamos descobrir qual o mês com o maior número de filmes por ano. Ainda não determinaremos
### qual o mês com o maior número de filmes no geral. Elaborar a tabela 01 para o relatório.

# criar a coluna mes a partir da data de lançamento do filme.

imdb_datas <- imdb_novo |>
  mutate(mes = month(data_lancamento,                                                         
                     label = TRUE, 
                     abbr = TRUE
                     )
         )

# encontrar o mês com o maior número de lançamentos discriminados por ano

mes_maior_lancamento <- imdb_datas |>                        
  group_by(mes,
           ano
  ) |>        
  summarise(qte = n()) |>             
  drop_na() |> 
  slice_max(qte) |> 
  arrange(desc(qte))


# define condição para formatar o valor de qte para a tabela 01 (destacar o resultado em vermelho)

mes_maior_lancamento_tab01 <- mes_maior_lancamento

mes_maior_lancamento_tab01[[3]] <-  cell_spec(mes_maior_lancamento_tab01[[3]],    # destacar a qte  na tabela.
                                              color = ifelse(mes_maior_lancamento_tab01[[3]] >= 380,
                                                             "#B80C09",
                                                             "#3F88C5"
                                                             ),
                                              bold = TRUE
                                              )

### selecionar a primeira linha

mes_maior_lancamento_tab01 <- mes_maior_lancamento_tab01 |>   
  head(n=1)


### Tabela 01 - maior quantidade de filmes em um mês

tab01 <-  mes_maior_lancamento_tab01 |>                      
  kbl(align = "l",                                                                   # alinhamento dos textos.
        col.names = c("Mês",                                                        
                    "Ano de Lançamento", 
                    "Quantidade de Filmes"
                    ),
      full_width = TRUE,
      escape = FALSE
  ) |> 
  kable_styling(bootstrap_options = c("striped",                                   # formata estilo da tabela.
                                      "condensed"
  ),
  font_size = 10,
  full_width = TRUE, 
  fixed_thead = list(enabled = TRUE, 
                     background = "#EDF6FD"
  ) 
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                                                  # configurações das colunas.
              bold = FALSE,
              width = "10cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "10cm"
  ) |>
  column_spec(3,
              bold = FALSE,
              width = "10cm"
  ) |> 
  footnote(general = "Maior quantidade de filmes em um mês. Fonte: Base de dados IMDB.",            
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 01:"
  )

# visualiza a tabela 01

tab01    


### Determinar não apenas o mês com maior número de lançamentos por ano, mas os 13 meses para a construção
### do gráfico 02

### Gráfico 02 - meses com maiores totais de lançamentos por ano

# encontrar os 13 meses com maiores totais de lançamentos por ano

meses_mais_qte <- mes_maior_lancamento |> 
  group_by(qte, ano) |>
  summarise(mes) |> 
  arrange(desc(qte)) |> 
  unite(col = "mes_ano", c("mes","ano"), sep = "/") |>       # cria a variável no formato mes/ano para o plot.
  arrange(desc(qte)) |> 
  head(13) 

# paleta de cores que será usada nos gráficos 

cores_plot02 <- c("#CC5A71", "#536265", "#536265", "#536265", "#536265","#536265",
                  "#536265", "#536265", "#536265", "#536265", "#536265","#536265",
                  "#536265")

### Gráfico 01 - meses com maiores totais de lançamentos --------------------------------------------

plot02 <- meses_mais_qte|> 
  ggplot(aes(x = fct_reorder(mes_ano, 
                             qte,
                             .desc = TRUE    
  ), 
  y = qte, 
  fill = mes_ano,
  label = qte
  )
  )+                                        # gráfico lollypop - a partir de geom_point e geom_segment.
  geom_point(size = 5,                                                             # cria o elemento circular.
             color = cores_plot02
  )+
  geom_label(size = 4, 
             alpha = 0, 
             label.size = NA, 
             color = cores_plot02,   
             vjust = -1
  ) +
  geom_segment(aes(x = mes_ano,                                                    # cria o elemento segmento.
                   xend = mes_ano,
                   y = 0,
                   yend = qte
  ),
  color = cores_plot02,
  size = 1.5,  
  )+
  labs(title = "Total de lançamentos por *<span style = 'color:#CC5A71;'>mês </span>*",
       subtitle ="Os meses com maior quantidade de estreias",        
       y = "Quantidade de filmes
    ",
       x = "
    Meses do ano"
  ) +
  theme_classic()+                                                               # seleção do tema do gráfico.
  scale_y_continuous(breaks = seq(0,
                                  500,
                                  50
  ),
  limits = c(0, 
             500
  ),
  expand = expansion(add = c(0,
                             0)
  )
  )+                                   
  theme_imdb()

# Visualiza o gráfico meses com maiores totais de lançamentos

plot02    
  
  
### Descoberto qual o mês com o maior número de lançamentos por ano, encontrar quais são os filmes deste mês
##$ com as maiores notas imdb. Filtrar aqueles com mais de 100000 avaliações. Para a tabela 02 do relatório.

### Tabela 02: Top 5 filmes

### Obtenção da relação dos 5 filmes  de outubro de 2018 por ranking

mes_maior_lancamento <- imdb_datas |>                                
  group_by(mes) |>        
  summarise(qte = n(),
            titulo,
            data_lancamento, 
            nota_imdb, 
            num_avaliacoes,
            pais,
            ano = year(data_lancamento)) |>
  filter(mes == "out" & ano == 2018 & num_avaliacoes >= 100000) |> # filtros 2018 e mais de 100000 avaliacões.
  arrange(desc(nota_imdb)) |> 
  slice_head(n=5)

mes_maior_lancamento$mes <- NULL                                                        # Exclusão de colunas. 
mes_maior_lancamento$qte <- NULL

### Tabela 02 - com os 5 filmes  de outubro de 2018 com melhores rankings ----

tab02 <- mes_maior_lancamento |> 
  select(titulo, data_lancamento, nota_imdb, num_avaliacoes, pais) |> 
  kbl(
    align = "l",                                                          # alinhamento do texto do cabeçalho.
    col.names = c("título",                                                     # define o nome das variáveis.
                  "lançamento", 
                  "nota",
                  "avaliações",
                  "país"),
  ) |> 
  kable_styling(                                                          # altera as configurações da tabela.
    bootstrap_options = c("striped", "condensed"),
    html_font = "",
    font_size = 10,
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, background = "#EDF6FD"),
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                                        # altera as configurações das colunas.
              bold = FALSE,
              width = "10cm",
  ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "4cm"
              
  ) |>
  column_spec(3,
              bold = FALSE,
              width = "2cm"
  ) |> 
  column_spec(4,
              bold = FALSE,
              width = "4cm"
  ) |> 
  column_spec(5,
              bold = FALSE,
              width = "10cm"
  ) |> 
  footnote(general = "Top 5 filmes Nota Média/ Outubro 2018. Fonte: Base de dados IMDB.",
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 02:",
  )

# visualiza a tabela top 5 filmes

tab02

### DESCOBRIR O MÊS COM O MAIOR NÚMERO DE LANÇAMENTOS NO GERAL

### Analisando toda a base, qual o mês com o maior número de lançamentos

mes_maior_lancamento_geral <- imdb_datas |>                        
  group_by(mes
           ) |>        
  summarise(qte = n()) |>             
  drop_na() |>
  arrange(desc(qte))


# configura qte do mes com mais lançamento para tabela 03

mes_maior_lancamento_tab03 <- mes_maior_lancamento_geral

mes_maior_lancamento_tab03[[2]] <- cell_spec(mes_maior_lancamento_tab03[[2]],
                                            color = ifelse(mes_maior_lancamento_tab03[[2]] >=8000,
                                                           "red", ""),
                                            bold = ifelse(mes_maior_lancamento_tab03[[2]] >=8000,
                                                          TRUE, FALSE)
                                            )


### Tabela 03 - Mês com maior número de lançamentos  -----------------------------------------------

tab03 <- mes_maior_lancamento_tab03 |> 
  kbl(align = "l",                                                                   # alinhamento dos textos.
      col.names = c("Mês",                                                        
                    "Quantidade de Filmes"
                    ),
      full_width = TRUE,
      escape = FALSE
      ) |> 
  kable_styling(bootstrap_options = c("striped",
                                      "condensed"
                                      ),
                font_size = 10,
                full_width = TRUE, 
  fixed_thead = list(enabled = TRUE, 
                     background = "#EDF6FD"
                     ) 
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                                                  # configurações das colunas.
              bold = FALSE,
              width = "10cm"
              ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "10cm"
              ) |>
  footnote(general = "Maior quantidade de filmes em um mês. Fonte: Base de dados IMDB.",            
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 03:"
           )

# visualiza a tabela 03

tab03    

  
## 4.3 CONSIDERANDO APENAS ORÇAMENTOS E RECEITAS EM DÓLARES -------------------------------------------

#### preparar a base para encontrar a respostas para as questões que envolvem lucro (filmes em dólares apenas)

imdb_filmes_dolares <- imdb_novo |>
  filter(str_detect(orcamento, pattern = "\\$.*"))

### eliminar o símbolo $ nos valores das colunas orçamento e receita

imdb_filmes_dolares <- imdb_filmes_dolares |> 
  mutate(orcamento = str_remove(orcamento, pattern = "\\$ "),
         receita = str_remove(receita, pattern = "\\$ "))

### alterar o tipo das variáveis orcamento e  receita

imdb_filmes_dolares[[10]]<- as.numeric(imdb_filmes_dolares[[10]])
imdb_filmes_dolares[[11]] <- as.numeric(imdb_filmes_dolares[[11]])

### calcular o lucro dos filmes

imdb_filmes_dolares <- imdb_filmes_dolares |> 
  mutate(
    lucro = receita - orcamento
  ) |>
  drop_na()

## 4.4 QUAL O DIA COM O MAIOR NÚMERO DE LANÇAMENTOS---------------------------------------------------

### Assim como descobrimos o mês com o maior número de lançamentos, descobriremos qual o dia com maior número
### de lançamentos

### Tabela 04 - Dia com o maior número de lancamentos

### Obter o dia com o maior número de lançamentos
dia_maior_lancamento <- imdb_novo |> 
  mutate(dia = day(data_lancamento)) |> 
  group_by(dia) |> 
  summarise(qtde = n()) |> 
  drop_na() |> 
  slice_max(order_by = qtde)                         # obtem o maior valor ordenado pela quantidade de filmes.


### configurar valor qtde lançamentos para a tabela 04 (destacar o resultado em vermelho)

dia_maior_lancamento[[2]][1] <- cell_spec(dia_maior_lancamento[[2]][1], 
                                          color = ifelse(dia_maior_lancamento[[2]][1]==7260,
                                                         "red",
                                                         ""
                                                         ),
                                          bold = ifelse(dia_maior_lancamento[[2]][1]==7260,
                                                        TRUE,
                                                        FALSE)
                                          )
### Tabela 04 - dia com o maior número de lançamentos ----

tab04 <- dia_maior_lancamento |>                                                                
  kbl(align = "l",                                                      
      col.names = c("Dia",                                                
                    "Quantidade de Filmes"
      ),
      escape = FALSE
  ) |> 
  kable_styling(                                                               # configura o estilo da tabela.
    bootstrap_options = c("striped",
                          "condensed"
    ),
    font_size = 10,
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE,
                       background = "#EDF6FD"
    )
  ) |> 
  kable_classic_2() |>                                                              # define o tema da tabela.
  column_spec(1,                                                        
              bold = FALSE,
              width = "15cm"
  ) |>      
  column_spec(2,
              bold = FALSE,
              width = "15cm"
  ) |> 
  footnote(general = "Dia com maior número de lançamentos. fonte: Base de dados IMDB.",       
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 03:",
  )

### Visualiza tabela dia com o maior número de filmes lançados

tab04


### Grafico 03 -total de lançamentos por dia ----

plot03<- imdb_novo |> 
  mutate(dia = day(data_lancamento)) |> 
  group_by(dia) |> 
  summarise(qtde = n()) |> 
  drop_na() |> 
  ggplot()+                           
  aes(x = dia, 
      y = qtde, 
      fill = dia, 
      label = qtde)+
  geom_point( size = 5,                                                              # cria elemento circular.
              color = "#CC5A71" 
  )+
  geom_label(size = 3.5, 
             alpha = 0,  
             label.size = NA, 
             color = "#CC5A71",     
             vjust = -1,       
  ) +
  geom_segment(aes(x = dia,                                                         # criar elemento segmento.
                   xend = dia,
                   y = 0,
                   yend = qtde
  ),
  size = 1.5, 
  color = "#CC5A71"  
  )+
  gghighlight(dia == 1 | dia == 31,                                                  # destaca os dias 1 e 31. 
              unhighlighted_params = list(colour = "#4C586B"
              ) 
  )+
  labs(title = "Total de lançamentos por *<span style = 'color:#CC5A71;'>dia </span>*",
       subtitle ="Os dias com maior e menor quantidade de estreias", 
       y = "Quantidade de filmes
    ",
       x = "
    Dias do mês"
  ) +
  theme_classic()+                          
  scale_y_continuous(breaks = seq(0,
                                  8000,
                                  1000
  ),                                              # formatar escala do eixo y. 
  limits = c(0,
             8000
  ),
  expand = c(0,
             0
  )
  ) +                                        
  scale_x_continuous(breaks = seq(0, 
                                  31,
                                  1
  )
  ) +                                                          # formatar escala do eixo x.
  theme_imdb()

# Visualiza o gráfico total de lançamentos por dia

plot03          


## 4.5 QUAL OS 5 PAÍSES COM MAIS FILMES PRODUZIDOS-------------------------------------------------

# Tabela 04 - paises maiores produtores de filmes

### Obter os 5 países com mais filmes na base
top_5_paises <- imdb_novo |> 
  separate_rows(pais, sep = ", ") |> 
  group_by(pais) |> 
  summarise(qtde = n()) |> 
  arrange(desc(qtde)) |> 
  slice_head(n=5)


### Tabela 05 paises maiores produtores de filmes ----
tab05 <- top_5_paises |>                                                                        
  kbl(align = "l",                                      
      col.names = c("País",                   
                    "Quantidade de Filmes"
      ),
  ) |> 
  kable_styling(  bootstrap_options = c("striped",                                # define o estilo da tabela.
                                        "condensed"
  ),
  html_font = "Arial",
  font_size = 10,
  full_width = TRUE, 
  fixed_thead = list(enabled = TRUE, 
                     background = "#EDF6FD"
  )
  ) |> 
  kable_classic_2() |>                                                              # define o tema da tabela.
  column_spec(1,                                                
              bold = FALSE,
              width = "15cm"
  ) |>      
  column_spec(2,                        
              bold = FALSE,
              width = "15cm"  
  ) |> 
  footnote(general = "Países maiores produtores de filmes. Fonte: Base de dados IMDB.",
           footnote_as_chunk = TRUE,                  
           fixed_small_size = TRUE,
           general_title = "Tabela 04:"
  )

# Visualiza a tabela paises maiores produtores de filmes

tab05


# gráfico 04: paises e totais de lançamentos

### obter os 16 países com maiores quantidades de filmes lançados

top_22_paises <- imdb_novo |> 
  separate_rows(pais, sep = ", ") |> 
  group_by(pais) |> 
  summarise(qtde = n()) |> 
  arrange(desc(qtde)) |> 
  slice_head(n=22) 
  

# Paleta de cores para os elementos gráficos

cor <- c(                                               
  "#c42847", "#ffad05", "#7d5ba6", "#00bbf9", "#2bc016",
  "#536265", "#536265", "#536265", "#536265", "#536265",
  "#536265", "#536265", "#536265", "#536265", "#536265",
  "#536265", "#536265", "#536265", "#536265", "#536265",
  "#536265", "#536265"
)

### Gráfico 04 - com os 16 países com maior número de filmes produzidos ----

plot04 <- top_22_paises |> 
  ggplot(aes(y = fct_reorder(pais,      # reordenar a sequência dos países em ordem decrescente da quantidade.
                             qtde, 
                             .desc = FALSE
  ),
  x = qtde, 
  label = qtde
  )
  ) +
  geom_col(fill = cor)+
  geom_label(size = 4.5,                                                                
             alpha = 0,
             label.size = NA,    
             fontface = "bold",  
             color = cor,
             hjust = -0.3
  ) +
  geom_curve(aes(y = 18,                                                       # formatar curva para anotação.
                 x = 8000,  
                 xend = 16000,     
                 yend = 16
  ),
  arrow = arrow(type = "closed",                                         # formata a seta da linha.
                length = unit(0.02,
                              "npc"
                )
  ),
  curvature = 0,  
  color = "#000000" 
  ) +
  geom_curve(aes(y = 1,                                                        # formatar curva para anotação.
                 x = 2500,  
                 xend = 11000,     
                 yend = 2
  ),
  arrow = arrow(type = "closed",                                         # formata a seta da linha.
                length = unit(0.02,
                              "npc"
                ) 
  ),
  curvature = 0,                                                                        # 0 = reta.
  color = "#000000"
  )+
  labs(                                                               
    title = "Países com maior quantidade de lançamentos de filmes",
    subtitle = "Destaque para os cinco paises com maiores notas",
    x = "Quantidade de filmes lançados",
    y = "Paises"
  )+
  scale_x_continuous(breaks = seq(0,
                                  35000,
                                  2500
  ),
  limits = c(0,
             35000
  ),
  expand = expansion(add = c(0,
                             1500
  )
  ),
  labels = scales::number_format(accuracy = 1,
                                 big.mark = "."
  )
  )+
  scale_y_discrete(expand = expansion(add = c(1,
                                              0
  )
  )
  )+
  theme_classic()+                                                                 # define o tema do gráfico.
  theme_imdb()+    
  annotate( "text",
            x = 21000, y = 15.5, label = "Relação dos cinco países com mais lançamentos 
            registrados na base IMDB",
            color = "#000000", 
            size = 4,
            family = ""
  )+
  annotate( "text",
            x = 14000, y = 2, label = "Vigésima segunda posição",
            color = "#000000", 
            size = 4,
            family = ""
  )

# visualiza o gráfico 04 - paise e quantidades de filmes

plot04


### Análise dos filmes brasileiros com maior ranking

top_5_filmes_brasil <- imdb_novo |> 
  separate_rows(pais, sep = ", ") |> 
  filter(pais == "Brazil") |> 
  summarise( titulo, 
             ano, 
             genero, 
             nota_imdb,
             num_avaliacoes)|>
  arrange(desc(nota_imdb)) |> 
  slice_head(n=5)

### Tabela 05 - dos filmes brasileiros com maior ranking ----

tab05 <- top_5_filmes_brasil |>  
  kbl(
    align = "l",                                                      
    col.names = c("Título",                                             
                  "Ano",
                  "Gênero",
                  "Nota IMDB",
                  "Numero de Avaliações"
    ),
  ) |> 
  kable_styling(bootstrap_options = c("striped",                                  # altera o estilo da tabela.
                                      "condensed"
  ),

  font_size = 10,   
  full_width = TRUE, 
  fixed_thead = list(enabled = TRUE,
                     background = "#EDF6FD"
  )
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                                          # altera as configurações da tabela.
              bold = FALSE,
              # background = "#022859", 
              # color = "#FFFFFF",
              width = "10cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              # background = "#022859",
              # color = "#FFFFFF",
              width = "5cm"
  ) |> 
  column_spec(3,                                                          # altera as configurações da tabela.
              bold = FALSE,
              # background = "#022859", 
              # color = "#FFFFFF",
              width = "7cm"
  ) |>
  column_spec(4,                                                          # altera as configurações da tabela.
              bold = FALSE,
              # background = "#022859", 
              # color = "#FFFFFF",
              width = "5cm"
  ) |>
  column_spec(5,                                                          # altera as configurações da tabela.
              bold = FALSE,
              # background = "#022859", 
              # color = "#FFFFFF",
              width = "5cm"
  ) |>
  footnote(general = "Filmes nacionais com a melhores notas Imdb. Fonte: Base de dados IMDB.",                     # inseri o rodapé.
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 05:",
  )

# Visualiza a tabela filmes nacionais com maiores notas medias

tab05


## 4.6 LISTE TODAS AS MOEDAS QUE APARECEM NAS COLUNAS ORÇAMENTO E RECEITA DA BASE IMDB_COMPLETA --------

# Tabela 06: relação de moedas da base

### lista de todas as moedas da base - coluna orçamento

moedas_orcamento <- imdb_novo |> 
  select(orcamento) |>
  mutate(moeda = str_extract(string = orcamento,                    # cria a coluna com os símbolos de moedas.
                             pattern = "[\\w | \\$].* " )        # regex para extração dos símbolos de moedas.
  ) |> 
  distinct(moeda) |>                      
  drop_na() |> 
  arrange(moeda)

moedas_orcamento <- moedas_orcamento |> 
  mutate(moeda = str_trim(moeda)) |> 
  mutate(moeda = str_replace(moeda, pattern = "\\$",            # substituir o símbolo de moeda de $ para USD.
                             replacement = "USD"
                             )
         )

### Base de dados em xlsx com as moedas e suas divisas para operação de join

### caminho relativo do arquivo

tab_moedas <- readxl::read_xlsx("data-raw/siglas_moedas_paises.xlsx")

### juntar as tabelas moedas_receita e tab_moedas por meio da coluna moeda 

moedas_orcamento_unificada<- left_join(x = moedas_orcamento,
                                       y = tab_moedas, 
                                       by = "moeda",
                                       copy = TRUE
) |> 
  distinct(moeda, divisa) |> 
  mutate(divisa = str_to_title(divisa))

### Tabela 06 - Relação de moedas na base ----

tab07 <- moedas_orcamento_unificada |> 
  kbl(
    align = "l",                                                      
    col.names = c("Moeda",                                                    
                  "Divisa"
    )
  ) |> 
  kable_styling(                                                                    # define estilo da tabela.
    bootstrap_options = c("striped", "condensed"),
    html_font = "",
    font_size = 10,       
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, background = "#EDF6FD")
  ) |> 
  kable_classic_2() |>                                                              # define o tema da tabela.
  column_spec(1,                                                    
              bold = FALSE,
              width = "15cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "15cm"
  ) |> 
  footnote(general = "Relação de moedas da base. Fonte:Base de dados IMDB.",
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 06:",
  )

# visualiza a tabela 06 - relação de moedas da base

tab07

## 4.7 QUAIS GÊNEROS COM O MAIORES LUCROS ---------------------------------------------------------

### separação da coluna genero para posterior pivotamento

imdb_filmes_dolares_genero <- imdb_filmes_dolares |> 
  separate(col = genero,
           into = c("genero1",
                    "genero2", 
                    "genero3"
           ),
           sep = ","
  )

### pivotar a base de wide para long

imdb_filmes_dolares_genero <- imdb_filmes_dolares_genero |> 
  pivot_longer(
    cols = c("genero1",
             "genero2", 
             "genero3"
    ),
    names_to = "tipos_generos",
    values_to = "generos",
    values_drop_na = TRUE
  ) |> 
  mutate(generos = str_trim(generos))                 # remove os espaços em branco no início e fim da string.

#$$ 10 gêneros com os maiores lucros

lucros_genero <- imdb_filmes_dolares_genero |>
  group_by(generos) |>
  summarise(lucro_total = sum(lucro)) |> 
  arrange(desc(lucro_total)) |> 
  slice_head(n=10) |> 
  mutate(lucro_total = paste("US$",         # formatar o valor obtido para o formato com US$ para a tabela 07.
                             format_dolar(lucro_total)      # função format_dolar() disponível no diretorio R.
  )
  )

### Tabela 07 - Maiores lucros por gênero ----

tab08 <- lucros_genero |> 
  kbl(
    align = "l",                                      
    col.names = c("Gêneros",                          
                  "Lucro Total"
    )
  ) |> 
  kable_styling(bootstrap_options = c("striped",                                # altera os estilos da tabela.
                                      "condensed"
  ),
  html_font = "",
  font_size = 10,  
  full_width = TRUE, 
  fixed_thead = list(enabled = TRUE,
                     background = "#EDF6FD"
  )
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                                                    # define o tema da tabela.
              bold = FALSE,
              width = "15cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "15cm"
  ) |> 
  footnote(general = "Maiores lucros por gênero. Fonte: Base de dados IMDB.",
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 07:",
  )

# Visualiza a tabela 07 - Maiores lucros por gênero

tab08


### obter as dez categorias com maiores lucros pela quantidade de filmes da categoria

lucros_genero_filme <- imdb_filmes_dolares_genero |>
  group_by(generos) |>
  summarise(lucro_total = sum(lucro)/n()) |> 
  arrange(desc(lucro_total)) |> 
  slice_head(n=10) |> 
  mutate(lucro_total = paste("US$",
                             format_dolar(lucro_total)
  )
  )


### Tabela 08 - generos com maiores lucros por quantidade de filmes ----

tab09 <- lucros_genero_filme |> 
  kbl(
    align = "l",                            
    col.names = c("Gêneros",                  
                  "Lucro Total por Filme")
  ) |> 
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    html_font = "",
    font_size = 10,       
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, background = "#EDF6FD")
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                                          # altera as configurações da tabela.
              bold = FALSE,
              width = "15cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "15cm"
  ) |> 
  footnote(general = "Lucros por gênero e quantidades de filmes. Base de dados IMDB.",
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 08:",
  )

# Visualiza a tabela 08 - lucros por gênero e quantidade de filmes

tab09


## 4.8 QUAIS OS GÊNEROS COM AS MAIORES NOTAS MÉDIAS -----------------------------------------------

## join de imdb e imdb_avaliacoes

imdb_join_aval <- left_join(imdb_filmes_dolares_genero,
                            imdb_avaliacoes,
                            by = "id_filme"
)

## separação das variáveis da coluna genero

imdb_join_aval_generos <- imdb_join_aval |> 
  separate(col = generos,
           into = c("genero_aval1",
                    "genero_aval2", 
                    "genero_aval3"
           ),
           sep = ","
  )

### pivotamento da base para long

imdb_join_aval_generos <- imdb_join_aval_generos |> 
  pivot_longer(
    cols = c("genero_aval1", 
             "genero_aval2", 
             "genero_aval3"),
    names_to = "tipos_generos_aval",
    values_to = "generos_aval",
    values_drop_na = TRUE
  ) |> 
  mutate(generos_aval = str_trim(generos_aval))


### join de imdb_join_aval_generos e imdb_avaliacoes

imdb_medias_genero <- left_join(imdb_join_aval_generos,
                                imdb_avaliacoes
) 


### descobrir as maiores notas médias por gêneros considerando o número de avaliações superiores a 10000

maiores_nota_genero <- imdb_medias_genero |> 
  group_by(generos_aval) |> 
  summarise(nota_media = mean(nota_imdb)) |> 
  arrange(desc(nota_media)) 

### gráfico 05 - notas média por genero ----

plot05 <- maiores_nota_genero|> 
  ggplot()+
  aes(x = fct_reorder(generos_aval,nota_media,
                      .desc = TRUE
  ),
  y = nota_media, 
  fill = generos_aval
  ) +
  geom_point(size = 5)+
  geom_segment(aes(x = generos_aval,
                   xend = generos_aval,
                   y = 5,
                   yend =  nota_media),
               linetype = 1,
               size = 0.1
  )+
  labs(title = "Gêneros e suas médias",
       x = "Gêneros",
       y = "Médias"
  )+
  scale_y_continuous(breaks = seq(0,
                                  8.5,
                                  0.5
  ),
  limits = c(5, 
             8.5
  ),
  expand = expansion(add = c(0,
                             0
  )
  )
  ) +
  scale_x_discrete(labels = c("Film-Noir", 
                              "Documentário", 
                              "Biografia",
                              "História", 
                              "Guerra", 
                              "Western",
                              "Animação", 
                              "Drama", 
                              "Musica", 
                              "Crime",
                              "Esporte", 
                              "Musical",
                              "Aventura",
                              "Romance",
                              "Mistério", 
                              "Sci-Fi", 
                              "Suspense", 
                              "Ação", 
                              "Fantasia", 
                              "Comédia", 
                              "Família",
                              "Terror"
  )
  ) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = -0.2, 
                                   size = 7
  ),
  axis.text.y = element_text(size = 7),
  axis.line.x = element_line(color = "#1F1F1F"), 
  axis.line.y = element_line(color = "#1F1F1F"), 
  axis.title.x = element_text(size = 11,
                              face = "plain"
  ),
  axis.title.y = element_text(size = 11, 
                              face = "plain"
  ),
  plot.background = element_rect(fill = "#FFFFFF"),
  panel.background = element_rect(fill = "#FFFFFF"),
  plot.margin = unit(c(1,
                       1,
                       1, 
                       1
  ),
  "cm"
  ),                                       
  plot.title = element_markdown(                                       
    size = 16,
    family = "",                                                                
    margin = unit(c(0,
                    0,
                    0.5,
                    0
    ),
    "cm"
    )     
  ), 
  text = element_text(family = "",   
                      color = "#000000",
                      size = 9
  ),  
  axis.title = element_text(size = 14,
                            hjust = 0.5
  ),
  legend.position = "none"   
  )

# Visualizar o gráfico notas médias por gêneros

plot05 |> 
  ggplotly(
    tooltip = c("y",                            # mostrar no gráfico dinamicamente apenas nota média e gênero.
                "generos_aval"
    )
  )

## 4.9 ANÁLISE DO FILME FAVORITO -----------------------------------------------------------------

### considerando apenas o valores em dólares

imdb_filmes_dolares <- imdb_novo |>
  filter(str_detect(orcamento, pattern = "\\$.*"))

### eliminar o símbolo $ nos valores das colunas orçamento e receita

imdb_filmes_dolares <- imdb_filmes_dolares |> 
  mutate(orcamento = str_remove(orcamento, pattern = "\\$ "),
         receita = str_remove(receita, pattern = "\\$ "))

### alterar o tipo das variáveis orcamento e  receita

imdb_filmes_dolares[[10]]<- as.numeric(imdb_filmes_dolares[[10]])
imdb_filmes_dolares[[11]] <- as.numeric(imdb_filmes_dolares[[11]])

### calcular o lucro dos filmes

imdb_filmes_dolares <- imdb_filmes_dolares |> 
  mutate(
    lucro = receita - orcamento
  ) |>
  drop_na(lucro)

### juntar as bases para as demais análises

imdb_base_completa <- left_join(imdb_filmes_dolares, imdb_avaliacoes, by = "id_filme")

### Renomear a variável direcao para nome para fajzer o join com imdb_pessoas
imdb_base_completa <- imdb_base_completa |> 
  rename("nome" = "direcao")

### join entre imdb_base_completa e imdb_pessoas
imdb_completa_pessoas <- left_join(imdb_base_completa, 
                                   imdb_pessoas,
                                   by = "nome"
)

### Retornar a designação da variável nome para direcao
imdb_completa_pessoas <- imdb_completa_pessoas |> 
  rename("direcao" = "nome")


### tabela 10 - James Cameron ----

### Selecionar dados de James Cameron

dados_james <- imdb_completa_pessoas |> 
  filter(titulo == "Avatar") |> 
  select(titulo, direcao, local_nascimento, data_nascimento) |> 
  mutate(idade = year(Sys.Date())-year(data_nascimento))   # calcula a idade de James Cameron na data de hoje.

dados_james[[5]] <- cell_spec(dados_james[[5]],
                              color = ifelse(dados_james[[5]] == 68,
                                             "#B80C09",
                                             "#3F88C5"
                                             )
                              )


tab10 <- dados_james |> 
  kbl(align = "l",   
      col.names = c("Filme",
                    "Diretor", 
                    "Local de Nascimento",
                    "Data de Nascimento",
                    "Idade Atual"
      ),
      full_width = TRUE,
      escape = FALSE
  ) |> 
  kable_styling(bootstrap_options = c("striped",                                  # define o estilo da tabela.
                                      "condensed"
  ),
  html_font = "",
  font_size = 10,
  full_width = TRUE, 
  fixed_thead = list(enabled = TRUE,
                     background = "#EDF6FD"
  )
  ) |> 
  kable_classic_2() |>                                                              # define o tema da tabela.
  column_spec(1,                                                        
              bold = FALSE,
              width = "6cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "6cm"
  ) |> 
  column_spec(3, 
              bold = FALSE,
              width = "6cm"
  ) |> 
  column_spec(4, 
              bold = FALSE,
              width = "6cm"
  ) |> 
  column_spec(5, 
              bold = FALSE,
              width = "6cm"
  ) |> 
  footnote(general = "James Cameron. Base de dados IMDB.",  
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 09:"
  )

# visualiza a tabela 09 - James Cameron

tab10


# tabela 11 - Filmes de Cameron na Base

### encontra os filmes de Cameron e cria coluna lucro com moeda US$ para a tabela 10

filmes_cameron <- imdb_completa_pessoas |> 
  filter(direcao == "James Cameron") |> 
  select(titulo, data_lancamento,
         genero,
         nota_imdb, 
         lucro
  ) |> 
  arrange(data_lancamento) |>                                            # cria coluna lucro para a tabela 10.
  mutate(lucro = paste("US$",
                       format_dolar(lucro)
  )
  )                       

### Tabela 10 - Filmes de Cameron ----

tab11 <- filmes_cameron|>
  kbl(
    align = "l",                                               
    col.names = c("Título",                         
                  "Data de Lançamento",
                  "Gênero",
                  "Nota IMDB",
                  "lucro"
    ),
    full_width = TRUE
  ) |> 
  kable_styling(                                                                  # altera o estilo da tabela.
    bootstrap_options = c("striped", 
                          "condensed"
    ),
    font_size = 10,
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, 
                       background = "#EDF6FD"
    )
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                                                    # define o tema da tabela.
              bold = FALSE,
              width = "5cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "4.5cm"
  ) |> 
  column_spec(3, 
              bold = FALSE,
              width = "5cm"
  ) |> 
  column_spec(4, 
              bold = FALSE,
              width = "5cm"
  ) |> 
  column_spec(5, 
              bold = FALSE,
              width = "5cm"
  ) |>
  footnote(general = "Filmes de Cameron na base. Fonte: Base de dados IMDB.",   
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 10:"
  )

# visualiza a tabela 11 - Filmes de Cameron

tab11


lucro_medio_filmes <- imdb_completa_pessoas |> 
  filter(direcao == "James Cameron") |> 
  summarise(lucro_medio = round(mean(lucro),2)) |> 
  mutate(lucro_medio = paste("US$",format_dolar(lucro_medio)))


### tabela 12 - Avatar ranking e nota

### criar a coluna ranking

imdb_novo <- imdb_novo |> 
  mutate(ranking = 0)

# ordena os filmes em ordem decrescente da nota e preenche a coluna ranking 

imdb_novo_ordenado <- imdb_novo |> 
  arrange(desc(nota_imdb)) |> 
  mutate(ranking = c(1: length(imdb_novo$ranking)))

### configura coluna ranking para tabela 12

imdb_tab12 <- imdb_novo_ordenado |> 
  filter(titulo == "Avatar") |> 
  mutate(total_filmes = nrow(ranking)) |> 
  select(titulo, direcao,
         nota_imdb,
         ranking) 

imdb_tab12[[4]] <- cell_spec(imdb_tab12[[4]], 
                             color = ifelse(imdb_tab12[[4]] >=2600,
                                            "#B80C09",
                                            "#3F88C5"
                                            )
                             )

### Tabela 12 - com dados de Avatar e sua posição no ranking

tab12 <- imdb_tab12 |> 
  kbl(align = "l",  
      col.names = c("Título",           
                    "Direção",
                    "Nota IMDB",
                    "Ranking IMDB"
      ),
      full_width = TRUE,
      escape = FALSE
  ) |> 
  kable_styling(                                                                  # altera o estilo da tabela.
    bootstrap_options = c("striped",
                          "condensed"
    ),
    font_size = 10,
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, 
                       background = "#EDF6FD"
    )
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                                        # altera as configurações das colunas.
              bold = FALSE,
              width = "5cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "5cm"
  ) |> 
  column_spec(3, 
              bold = FALSE,
              width = "5cm"
  ) |> 
  column_spec(4, 
              bold = FALSE,
              width = "5cm"
  ) |>
  footnote(general = "Avatar - ranking e nota. fonte: Base de dados IMDB.",     
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 11:"
  )

# visualiza a tabela 11 - Avatar ranking e nota

tab12


# tabela 13 - Avatar - ranking de lucro

# cria a coluna ranking_lucro

imdb_filmes_dolares <- imdb_filmes_dolares |> 
  mutate(ranking = 0)

# preenche a coluna ranking dos filmes de acordo com sua renda em dólares
imdb_filmes_dolares_ordenado <- imdb_filmes_dolares |> 
  arrange(desc(lucro)) |> 
  mutate(ranking_dolar = c(1: length(imdb_filmes_dolares$ranking))) |> 
  select(titulo,
         direcao,
         lucro,
         ranking_dolar
  ) 

plot13 <- imdb_filmes_dolares |> 
  filter(direcao == "James Cameron") |>
  select(titulo, receita, orcamento, nota_imdb) |> 
  mutate(titulo = factor(titulo, titulo)) |> 
  mutate(text = paste("receita:", receita,"orçamento:", orcamento, "nota:", nota_imdb, sep = ",")) |> 
  ggplot(aes(x = receita, y = orcamento, size = nota_imdb, fill = titulo, text = text))+
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Nota Imdb") +
  viridis::scale_color_viridis(discrete=TRUE, guide="none") +
  hrbrthemes::theme_ipsum()+
  theme(legend.position = "none")+
  scale_y_continuous(expand =expansion(add = c(0,
                                               0.5)
                                       ),
                     labels = scales::number_format(accuracy = 0.1,
                                                    decimal.mark = ","
                                                    )
                     )+
  scale_x_continuous(expand =expansion(add = c(0,
                                               0.5)
                                       ),
                     labels = scales::number_format(accuracy = 0.1,
                                                    decimal.mark = ","
                                                    )
                     )


  

pp13 <-  ggplotly(plot13)
  

pp13



### Tabela 13 - Avatar ranking de lucro ----

imdb_tab13<- imdb_filmes_dolares_ordenado |> 
  filter(titulo == "Avatar") |> 
  mutate(lucro = paste("US$",
                       format_dolar(lucro)                        # cria coluna lucro com US$ para a tabela12.
                       )
         )

imdb_tab13[[4]] <- cell_spec(imdb_tab13[[4]],
                             color = ifelse(imdb_tab13[[4]] == 1, 
                                            "#B80C09",
                                            "#3F88C5"
                                            )
                             )

tab13 <- imdb_tab13 |> 
  kbl(align = "l",  
      col.names = c("Título",                   
                    "Direção",
                    "Lucro",
                    "Ranking dólar"
      ),
      full_width = TRUE,
      escape = FALSE
  ) |> 
  kable_styling(bootstrap_options = c("striped",                                  # define o estilo da tabela.
                                      "condensed"
  ),
  font_size = 10,
  full_width = TRUE, 
  fixed_thead = list(enabled = TRUE, 
                     background = "#EDF6FD"
  )
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                                        # altera as configurações das colunas.
              bold = FALSE,
              width = "5cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "5cm"
  ) |> 
  column_spec(3, 
              bold = FALSE,
              width = "5cm"
  ) |> 
  column_spec(4, 
              bold = FALSE,
              width = "5cm"
  ) |>
  footnote(general = "Avatar - ranking de lucro. Fonte: Base de dados IMDB.",                         
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 12:",
  )


# visualiza tabela 13 Avatar ranking de lucro

tab13


### Tabela 14 - dia de lançamento ----

### seleciona dados do filme Avatar

imdb_tab14 <- imdb_novo_ordenado |> 
  filter(titulo == "Avatar") |> 
  mutate(data_lancamento_dia = wday(data_lancamento,
                                    week_start = getOption("lubridate.week.start",
                                                           7
                                    ),
                                    abbr = FALSE,
                                    label = TRUE
  )
  ) |>
  select(titulo, data_lancamento, data_lancamento_dia) 


### configura a coluna data_lancamento_dia para destaque na tabela 14
imdb_tab14[[3]] <- cell_spec(imdb_tab14[[3]], 
                                color = ifelse(imdb_tab14[[3]] == "sexta",
                                               "#B80C09",
                                               ""
                                               )
                                )

# Tabela 14 - Dia da semana do lançamento

tab14 <- imdb_tab14|> 
  kbl(align = "l",    
      col.names = c("Título", 
                    "Data de lançamento",
                    "Dia da semana"
      ),
      full_width = TRUE,
      escape = FALSE
  ) |> 
  kable_styling(bootstrap_options = c("striped",                                  # define o estilo da tabela.
                                      "condensed"
  ),
  font_size = 10,
  full_width = TRUE, 
  fixed_thead = list(enabled = TRUE,
                     background = "#EDF6FD"
  )
  ) |> 
  kable_classic_2() |>                                                              # define o tema da tabela.
  column_spec(1,                                                 
              bold = FALSE,
              width = "7.5cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "7.5cm"
  ) |> 
  column_spec(3, 
              bold = FALSE,
              width = "5cm"
  ) |>
  footnote(general = "Dia da semana do lançamento. Fonte: Base de dados IMDB.",                    
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 13:",
  )

# visualiza a tabela 14 - dia de lançamento

tab14


### Tabela 15 - filmes lançados no mesmo dia ----

tab15 <- imdb_novo_ordenado |> 
  filter(data_lancamento == "2010-01-15") |> 
  select(titulo,
         data_lancamento,
         genero,
         nota_imdb,
         pais
  ) |>
  kbl(align = "l",  
      col.names = c("Título",
                    "Data de Lançamento",
                    "Gênero",
                    "Nota IMDB",
                    "País"
      ),
      full_width = TRUE
  ) |> 
  kable_styling(bootstrap_options = c("striped",                                  # define o estilo da tabela.
                                      "condensed"
  ),
  font_size = 10,
  full_width = TRUE, 
  fixed_thead = list(enabled = TRUE, 
                     background = "#EDF6FD"
  )
  ) |> 
  kable_classic_2() |>                                                              #define o tema da tabela.
  column_spec(1,                                                
              bold = FALSE,
              width = "5cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "5cm"
  ) |> 
  column_spec(3, 
              bold = FALSE,
              width = "5cm"
  ) |> 
  column_spec(4, 
              bold = FALSE,
              width = "5cm"
  ) |>
  column_spec(5, 
              bold = FALSE,
              width = "5cm"
  ) |>
  footnote(general = "Filmes lançados no mesmo dia. Fonte:Base de dados IMDB.",           
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Tabela 14:",
  )
# visualiza a tabela 15 - filmes lançados no mesmo dia

tab15


# Gráfico distribuição da nota do filme avatar por idade

# renomear as colunas

imdb_avaliacoes <- imdb_avaliacoes |> 
  rename("[00-18)" = "nota_media_idade_0_18",
         "[18-30)" = "nota_media_idade_18_30",
         "[30-45)" = "nota_media_idade_30_45",
         "[45-60)" = "nota_media_idade_45_mais")

# join das base imdb e imdb_avaliações

imdb_join_avatar_idades <- left_join(imdb_novo, imdb_avaliacoes, by = "id_filme") |> 
  filter(titulo == "Avatar") |>
  pivot_longer("faixas_etarias",
               values_to = "nota_media_idade", 
               cols =c("[00-18)",
                       "[18-30)",
                       "[30-45)",
                       "[45-60)"
               )
  ) |> 
  group_by(faixas_etarias) |> 
  summarise(nota_media_idade)


# cria paleta de cores para o gráfico

cor <- c("#c42847", 
         "#ffad05", 
         "#7d5ba6", 
         "#00bbf9"
)

### Gráfico 06 - notas por faixa etária ----

plot06 <- imdb_join_avatar_idades |> 
  ggplot(aes(x=faixas_etarias, 
             y = nota_media_idade, 
             label = nota_media_idade
  )
  )+
  geom_col(width = 0.5,
           fill = cor)+
  geom_label(nudge_y = 0.5)+
  labs(title = "Distribuição das Notas por Idade",
       subtitle = "Notas médias por intervalo de classes do filme Avatar",
       x = "Idade",
       y = "Nota Média")+
  theme_classic()+
  theme_imdb() +
  scale_y_continuous(expand =expansion(add = c(0,
                                               0.5)
                                       ),
                     labels = scales::number_format(accuracy = 0.1,
                                                    decimal.mark = ","
                                                    )
                     )

# visualiza o grafico notas por faixa etária

plot06



# tradução da descrição do filme Avatar

imdb |> 
  filter(titulo == "Avatar") |> 
  select(descricao) |> 
  write_csv("data/avatar_descricao.csv")
´