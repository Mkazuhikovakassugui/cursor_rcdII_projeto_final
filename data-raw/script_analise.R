#----------------------------------------------------------------------------------------------#
#                    Projeto Final - Curso Linguagem R para Ciência de Dados II
#                                   Professores: Caio e Beatriz
#                                    Aluno: Marcio Vakassugui
#----------------------------------------------------------------------------------------------#



# 1. NSTALAÇÃO DE PACOTES E LEITURA DA BASE ---------------------------------------------------

remotes::install_github("curso-r/basesCursoR")
imdb <- basesCursoR::pegar_base("imdb_completa")
imdb_pessoas <- basesCursoR::pegar_base("imdb_pessoas")
imdb_avaliacoes <- basesCursoR::pegar_base("imdb_avaliacoes")

library(readr)                                                                                      # leitura.
library(tidyverse)                                                                              # manipulação.
library(lubridate)                                                                     # manipulação de datas.
library(forcats)                                                                    # maninpulação de factors.
library(kableExtra)                                                                                 # tabelas.
library(plotly)                                                                      # extensão para gráficos.
library(gghighlight)                                                                 # extensão para gráficos.
library(ggtext)                                                                      # extensão para gráficos.
library(ggthemes)                                                                     # temas para relatórios.



# 2. SALVAR AS BASES EM DATA-RAW ------------------------------------------------------------------------


write_rds(imdb, "data-raw/imdb.rds")
# write_rds(imdb_avaliacoes, "data-raw/imdb_avaliacoes.rds")
# write_rds(imdb_pessoas, "data-raw/imdb_pessoas.rds")


# 3. LEITURA DAS BASES EM DATA-RAW ----------------------------------------------------------------------

imdb <- read_rds("data-raw/imdb.rds")
imdb_avaliacoes <- read_rds("data-raw/imdb_avaliacoes.rds")
imdb_pessoas <- read_rds("data-raw/imdb_pessoas.rds")



# 4. ANÁLISES -------------------------------------------------------------------------------------------

## 4.1 QUAL O MÊS DO ANO COM O MAIOR NÚMERO DE FILMES: E O DIA DO ANO? ----------------------------------

### Descobrir qual mês em todo o conjunto de dados teve o maior número de filmes lançados----------------

### verificar os tipos das variáveis --------------------------------------------------------------------

glimpse(imdb)

#### ------------------> data_lancamento = "chr"
#### ------------------> receita         = "chr"
#### ------------------> receita_eua     = "chr"
#### ------------------> orcamento       = "chr"

### Diferença entre ano e data de lançamento -----------------------------------------------------------

### Ano              =   ano de produção 
### Data_lancamento  =   data da estreia do filme

### Alterar o tipo de data_lancamento para date ---------------------------------------------------------

imdb <- imdb |>    
  mutate(
    data_lancamento = ymd(data_lancamento)                              # conversão do tipo "chr" para "date".
  )

### ------------------> 4563 linhas com dado "data de lancamento" = NA gerados por coerção
  
### Analisar os valores missing -------------------------------------------------------------------------

skimr::skim(imdb)

### ------------------> 27,6 % de dados fornecidos em orcamento
### ------------------> 36,1 % de dados fornecidos em receita
### ------------------> 17,9 % de dadow fornecidos em receita_eua
### ------------------> demais dados com índices de preenchimento superiores a 86,3%


### Altera o tipo da variável data_lancamento para date

imdb <- imdb |>                                              
  mutate(data_lancamento = ymd(data_lancamento))    

### Gráfico meses com maiores lançamentos de filmes na base IMDB
  
p_filmes_ano <- imdb |> 
  mutate(ano_lancamento = year(data_lancamento)) |>                            # cria a coluna ano_lancamento.
  select(ano_lancamento) |> 
  drop_na() |>
  group_by(ano_lancamento) |> 
  summarise(qte = n()) |>                                 # resume por ano e quantidade de filmes em cada ano.
  slice_head(n=112) |>             # linha 113 possui dados de 2021 de alguns meses. Exclui esta linha.
  ggplot()+
  aes(x = ano_lancamento, 
      y = qte)+
  annotate("rect",                                                # cria anotação retangular ao redor de 2018.
           xmin = 2010,
           xmax = 2026,
           ymin = 2950, 
           ymax = 3500,
           fill = "#99D8FF", 
           alpha = 0.3
  ) +
  annotate("text",                                               # cria anotação de texto sobre o ano de 2018.
           x = 1960, 
           y = 3281,
           label = "2018 - ano com maior número lançamentos da história", 
           size = 3
  ) +
  geom_segment(aes(y = 3281,                                              # cria a linha da anotação de texto.
                   x = 1992,  
                   xend = 2009,     
                   yend = 3281   
  ),
  size = 0.3,
  fill = "#000000"   
  ) +
  geom_line(color = "blue")+                                                          # cria gráfico de linha.
  geom_point(color = "red",                                 # cria gráfico de pontos sobre o gráfico de linha.
             shape = 7,
             size = 0.5
  )+
  gghighlight(ano_lancamento >=1960,                    # destaca os dados com ano_lançamento superior a 1960.
              unhighlighted_params = list(colour = "#4C586B"
              )
  )+
  labs(title = "Total de filmes por ano",                            # define os textos para o título e eixos.
       x = "
    Ano",
       y = "Quantidade
    
    "
  )+
  scale_x_continuous(breaks = seq(0,                   # define a escala do eixo x de 0 a 2022, passo 10 anos.
                                  2022, 
                                  10
  )
  ) +
  scale_y_continuous(breaks = seq(0,                    # define a escala do eixo y de 0 a 4000, passo de 500.
                                  4000, 
                                  500
  )
  ) +
  theme_classic()                                                                 # escolhe o tema do gráfico.

p_filmes_ano |> 
  ggplotly()                                                                                # gera a o plotly.
# gera a o plotly.

 

######################################################################################################

### Cria a coluna mes

imdb_datas <- imdb |>
  mutate(mes = month(data_lancamento, label = TRUE, abbr = TRUE))  

### Obtém o mês com o maior número de lançamentos.
mes_maior_lancamento <- imdb_datas |>                        
  group_by(mes, ano) |>        
  summarise(qte = n()) |>             
  drop_na() |> 
  slice_max(qte) |> 
  arrange(desc(qte))

### cria tabela com dados do mês e maior número de estreias.

tab_mes_maior_lancamento <- mes_maior_lancamento |>   
  head(n=1) |> 
  kbl(
    align = "l",                                               # alinhamento do texto do cabeçalho à esquerda.
    col.names = c("Mês",                               # define o nome dos cabeçalhos da tabela.
                  "Ano de Lançamento", 
                  "Quantidade de Filmes Lançados"),
    full_width = TRUE,
  ) |> 
  kable_styling(                                                          # altera as configurações da tabela.
    html_font = "get_schwifty",
    bootstrap_options = "basic",
    font_size = 10,
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, background = "#EDF6FD")
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                                        # altera as configurações das colunas.
              bold = FALSE,
              #background = "#022859", 
              #color = "#FFFFFF",
              width = "3cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              # background = "#022859",
              # color = "#FFFFFF",
              width = "3cm"
  ) |>
  column_spec(3,
              bold = FALSE,
              # background = "#022859",
              # color = "#FFFFFF",
              width = "4cm"
  ) |> 
  footnote(general = "Base de dados IMDB (Internet Movie Database).",                           # cria rodapé.
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Fonte:",
  )

tab_mes_maior_lancamento


# Cria o gráfico com 

meses_mais_qte <- mes_maior_lancamento |> 
  group_by(qte, ano) |> 
  summarise(mes) |> 
  arrange(desc(qte)) |> 
  unite(col = "mes_ano", c("mes","ano"), sep = "") |> 
  #mutate(mes_ano = as_factor(mes_ano)) |> 
  arrange(desc(qte)) |> 
  head(20) 

 
cores_plot02 <- c("#CC5A71", "#536265", "#536265", "#536265", "#536265","#536265",
                  "#536265", "#536265", "#536265", "#536265", "#536265","#536265",
                  "#536265")

plot01 <- meses_mais_qte|> 
  ggplot(aes(x = fct_reorder(mes_ano, qte, .desc = TRUE), 
             y = qte, 
             fill = mes_ano,
             label = qte))+                               # cria o gráfico de lollypop - combinando geom_point e geom_segment.
  geom_point(                                                                        # cria elemento circular.
    size = 5,                                                                                      # diâmetro.
    color = cores_plot02                                                                         # cor vermelha.
  )+
  geom_label(                                                                            # formatar os labels.
    size = 4,                                                               # diâmetro do círculo do lollypop.
    alpha = 0,                                                                     # elimina o fundo do label.
    label.size = NA,                                                  # elimina a moldura retangular do label.
    color = cores_plot02,                                                           # cor do círculo em vermelho.
    vjust = -1,                                                           # posição do label acima do círculo.
  ) +
  geom_segment(aes(x = mes_ano,                                                       # criar o elemento segmento.
                   xend = mes_ano,
                   y = 0,
                   yend = qte
                   ),
               color = cores_plot02,
               size = 1.5,                                                                           # largura do segmento.
               #color = "#CC5A71"                                                             # cor do segmento em vermelho.
  )+
  labs(                                                                # editar o texto do título em markdown.
    title = "Total de lançamentos por *<span style = 'color:#CC5A71;'>mes </span>*",
    subtitle ="Os meses com maior e menor quantidade de estreias",          # editao o texto subtítulo em text.
    y = "Quantidade de filmes
    ",
    x = "
    Meses do ano"
  ) +
  theme_classic()+                                                               # seleção do tema do gráfico.
  scale_y_continuous(breaks = seq(0, 400, 50),
                     limits = c(0,400)) +                                        # formatar escala do eixo y.  
  theme(                                                          # customizar:
    panel.background = element_rect(fill = "#FFFFFF"),                                     # fundo do gráfico.
    plot.background = element_rect(fill = "#FFFFFF"),                           # fundo da moldura retangular.
    plot.margin = unit(c(1, 1, 1, 1), "cm"),                                          # distância das margens.
    plot.title = element_markdown(                                            # título do gráfico em markdown.
      size = 24,
      family = "Arial",                                                                     # fonte do título.
      margin = unit(c(0, 0, 0.5, 0), "cm")                                                # margens do título.
    ), 
    plot.subtitle = element_text(                                                        #textos do subtitulo.
      size = 14,
      family = "Arial",   
    ),
    text = element_text(                                                                  # textos do gráfico.
      family = "Arial",    
      color = "#000000",
      size = 14
    ),
    axis.text.x = element_text(                                                             # texto do eixo x.
      color = "#000000",       
      size = 15,
      margin = unit(c(0.3, 0, 0.5, 0), "cm")  
    ),
    axis.text.y = element_markdown(                                                         # texto do eixo y.
      color = "#000000",    
      size = 15,
      family = "Arial", 
    ),
    axis.ticks.x = element_line(color = "#1F1F1F"),                                         # ticks do eixo x.
    axis.line.x = element_line(color = "#1F1F1F"),                                   # cor da linha do eixo x.
    axis.ticks.y = element_line(color = "#1F1F1F"),                             # padrões dos ticks do eixo x.
    axis.line.y = element_line(
      color = "#000000",                                                             # cor da linha do eixo x.
      size = 0.4,
    ),
    axis.title = element_text(                                                    # texto do título do eixo x.
      size = 16,
      hjust = 0.5,
    ),
    legend.position = "none"                                                               # exclui a legenda.
  )



plot01
  
  
  
  








## gráfico com as maiores quantidades de lançamentos por mêses do ano



tab_mes_maior_lancamento <- mes_maior_lancamento |>   # cria tab. com dados do mês e maior número de estreias.
  kbl(
    align = "l",                                               # alinhamento do texto do cabeçalho à esquerda.
    col.names = c("Ano de Lançamento",                               # define o nome dos cabeçalhos da tabela.
                  "Mês", 
                  "Quantidade de Filmes Lançados"),
    full_width = TRUE,
  ) |> 
  kable_styling(                                                          # altera as configurações da tabela.
    html_font = "get_schwifty",
    bootstrap_options = "basic",
    font_size = 10,
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, background = "#EDF6FD")
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                                        # altera as configurações das colunas.
              bold = FALSE,
              #background = "#022859", 
              #color = "#FFFFFF",
              width = "3cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              # background = "#022859",
              # color = "#FFFFFF",
              width = "3cm"
  ) |>
  column_spec(3,
              bold = FALSE,
              # background = "#022859",
              # color = "#FFFFFF",
              width = "4cm"
  ) |> 
  footnote(general = "Base de dados IMDB (Internet Movie Database).",                           # cria rodapé.
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Fonte:",
  )

tab_mes_maior_lancamento                                                          # visualiza a tabela gerada.


## Manipulação da base para obtenção da relação dos 15 filmes  de outubro de 2018 por ranking 

mes_maior_lancamento <- imdb_datas |>                                # lista dos 15 filmes com melhor ranking.
  group_by(mes) |>        
  summarise(qte = n(),
            titulo,
            data_lancamento, 
            nota_imdb, 
            num_avaliacoes,
            pais,
            ano = year(data_lancamento)) |>
  filter(mes == "out" & ano == 2018 & num_avaliacoes >= 10000) |> 
  arrange(desc(nota_imdb)) |> 
  slice_head(n=15)

mes_maior_lancamento$mes <- NULL                                                        # Exclusão de colunas. 
mes_maior_lancamento$qte <- NULL

## Tabela com os 15 filmes  de outubro de 2018com melhores rankings

mes_maior_lancamento |>                                                               
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
    html_font = "get_schwifty",
    font_size = 10,
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, background = "#EDF6FD"),
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                                        # altera as configurações das colunas.
              bold = FALSE,
              width = "3cm",
  ) |>      
  column_spec(2, 
              bold = FALSE,
              width = "1cm"
              
  ) |>
  column_spec(3,
              bold = FALSE,
              width = "1cm"
  ) |> 
  column_spec(4,
              bold = FALSE,
              width = "1cm"
  ) |> 
  column_spec(5,
              bold = FALSE,
              width = "4cm"
  ) |> 
  footnote(general = "Base de dados IMDB (Internet Movie Database).",
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Fonte:",
  )


## 4.2 O DIA COM O MAIOR NÚMERO DE LANÇAMENTOS DE FILMES ---------------------------------------------

#### Manipulação da base para a obtencão do dia com o maior número de lançamentos---------------------

dia_maior_lancamento <- imdb |> 
  mutate(dia = day(data_lancamento)) |> 
  group_by(dia) |> 
  summarise(qtde = n()) |> 
  drop_na() |> 
  slice_max(order_by = qtde)

### Tabela com os dados do dia com maior número de lançamentos --------------------------------------

dia_maior_lancamento |>                                                                
  kbl(align = "l",                                                        # alinhamento do texto do cabeçalho.
      col.names = c("Dia",                                                        # define o nome das colunas.
                  "Quantidade de Filmes Lançados"
                  ),
      ) |> 
  kable_styling(bootstrap_options = c("striped",                                    # define estilo da tabela.
                                      "condensed"),
                html_font = "get_schwifty",
                font_size = 10,
                full_width = TRUE, 
                fixed_thead = list(enabled = TRUE,
                                   background = "#EDF6FD"
                                   )
                ) |> 
  kable_classic_2() |>                                                              # define o tema da tabela.
  column_spec(1,                                                            # define os padrões da coluna dia.
              bold = FALSE,
              width = "5cm"
              ) |>      
  column_spec(2,                                # define os padrões da coluna "Quantidade de Filmes Lançados".
              bold = FALSE,
              width = "5cm"
              ) |> 
  footnote(general = "Base de dados IMDB (Internet Movie Database).",               # cria o rodapé da tabela.
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Fonte:",
           )


### Grafico dias com maior quantidade de filmes -----------------------------------------------------------

plot_dia_filmes <- imdb |> 
  mutate(dia = day(data_lancamento)) |> 
  group_by(dia) |> 
  summarise(qtde = n()) |> 
  drop_na() |> 
  ggplot()+                               # cria o gráfico de lollypop - combinando geom_point e geom_segment.
  aes(x = dia, 
      y = qtde, 
      fill = dia, 
      label = qtde)+
  geom_point(                                                                        # cria elemento circular.
    size = 5,                                                                                      # diâmetro.
    color = "#CC5A71"                                                                          # cor vermelha.
  )+
  geom_label(                                                                            # formatar os labels.
    size = 4,                                                               # diâmetro do círculo do lollypop.
    alpha = 0,                                                                     # elimina o fundo do label.
    label.size = NA,                                                  # elimina a moldura retangular do label.
    color = "#CC5A71",                                                           # cor do círculo em vermelho.
    vjust = -1,                                                           # posição do label acima do círculo.
  ) +
  geom_segment(aes(x = dia,                                                       # criar o elemento segmento.
                   xend = dia,
                   y = 0,
                   yend = qtde
  ),
  size = 1.5,                                                                           # largura do segmento.
  color = "#CC5A71"                                                             # cor do segmento em vermelho.
  )+
  gghighlight(dia == 1 | dia == 31,                                                  # destaca os dias 1 e 31. 
              unhighlighted_params = list(colour = "#4C586B")                      # cor cinza para os demais.
  )+
  labs(                                                                # editar o texto do título em markdown.
    title = "Total de lançamentos por *<span style = 'color:#CC5A71;'>dia </span>*",
    subtitle ="Os dias com maior e menor quantidade de estreias",          # editao o texto subtítulo em text.
    y = "Quantidade de filmes
    ",
    x = "
    Dias do mês"
  ) +
  theme_classic()+                                                               # seleção do tema do gráfico.
  scale_y_continuous(breaks = seq(0, 8000, 1000),
                     limits = c(0,8000)) +                                        # formatar escala do eixo y.  
  scale_x_continuous(breaks = seq(0, 31, 1)) +                                    # formatar escala do eixo x.
  theme(                                                          # customizar:
    panel.background = element_rect(fill = "#FFFFFF"),                                     # fundo do gráfico.
    plot.background = element_rect(fill = "#FFFFFF"),                           # fundo da moldura retangular.
    plot.margin = unit(c(1, 1, 1, 1), "cm"),                                          # distância das margens.
    plot.title = element_markdown(                                            # título do gráfico em markdown.
      size = 24,
      family = "Arial",                                                                     # fonte do título.
      margin = unit(c(0, 0, 0.5, 0), "cm")                                                # margens do título.
    ), 
    plot.subtitle = element_text(                                                        #textos do subtitulo.
      size = 14,
      family = "Arial",   
    ),
    text = element_text(                                                                  # textos do gráfico.
      family = "Arial",    
      color = "#000000",
      size = 14
    ),
    axis.text.x = element_text(                                                             # texto do eixo x.
      color = "#000000",       
      size = 15,
      margin = unit(c(0.3, 0, 0.5, 0), "cm")  
    ),
    axis.text.y = element_markdown(                                                         # texto do eixo y.
      color = "#000000",    
      size = 15,
      family = "Arial", 
    ),
    axis.ticks.x = element_line(color = "#1F1F1F"),                                         # ticks do eixo x.
    axis.line.x = element_line(color = "#1F1F1F"),                                   # cor da linha do eixo x.
    axis.ticks.y = element_line(color = "#1F1F1F"),                             # padrões dos ticks do eixo x.
    axis.line.y = element_line(
      color = "#000000",                                                             # cor da linha do eixo x.
      size = 0.4,
    ),
    axis.title = element_text(                                                    # texto do título do eixo x.
      size = 16,
      hjust = 0.5,
    ),
    legend.position = "none"                                                               # exclui a legenda.
  ) 

plot_dia_filmes                                                                         # visualiza o gráfico.

## 4.3  TOP 5 PAISES COM MAIOR NÚMERO DE LANÇAMENTOS -------------------------------------------------

### Manipulação da base para a obtencão dos 5 países com mais filmes na base --------------------------

top_5_paises <- imdb |> 
  group_by(pais) |> 
  summarise(qtde = n()) |> 
  arrange(desc(qtde)) |> 
  slice_head(n=5)

### Tabela com os dados do dia com maior número de lançamentos ----------------------------------------

top_5_paises |>                                                                        
  kbl(align = "l",                                                        # alinhamento do texto do cabeçalho.
      col.names = c("País",                                                       # define o nome das colunas.
                  "Quantidade de Filmes Lançados"
                  ),
      ) |> 
  kable_styling(  bootstrap_options = c("striped",                                # define o estilo da tabela.
                                        "condensed"),
                  html_font = "get_schwifty",
                  font_size = 10,
                  full_width = TRUE, 
                  fixed_thead = list(enabled = TRUE, 
                                     background = "#EDF6FD"
                                     )
                  ) |> 
  kable_classic_2() |>                                                              # define o tema da tabela.
  column_spec(1,                                                   # altera as configurações da coluna "País".
              bold = FALSE,
              width = "5cm"
  ) |>      
  column_spec(2,                          # altera as configurações da coluna "Quantidade de Filmes Lançados".
              bold = FALSE,
              width = "5cm"
  ) |> 
  footnote(general = "Base de dados IMDB (Internet Movie Database).",               # cria o rodapé da tabela.
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Fonte:",
  )

### Gráfico lançamentos por dia ---------------------------------------------------------------------

plot_dia_filmes <- imdb |> 
  mutate(dia = day(data_lancamento)) |> 
  group_by(dia) |> 
  summarise(qtde = n()) |> 
  drop_na() |> 
  ggplot()+                               # construir gráfico lollypop combinando o geom_point e geom_segment.
  aes(x = dia, 
      y = qtde, 
      fill = dia,
      label = qtde)+
  geom_point(                                                                              # gráfico de ponto.
    size = 5,
    color = "#CC5A71"
  )+
  geom_label(                                                                            # formatar os labels.
    size = 4, 
    alpha = 0,
    label.size = NA,    
    fontface = "plain",    
    color = "#CC5A71",
    vjust = -1,
    parse = TRUE
  ) +
  geom_segment(aes(x = dia,                                                             # gráfico de segmento.
                   xend = dia,
                   y = 0,
                   yend = qtde
  ),
  size = 1.5,
  color = "#CC5A71"
  )+
  gghighlight(dia == 1 | dia == 31,                                                 # destacar os dias 1 e 31.
              unhighlighted_params = list(colour = "#4C586B")
  )+
  labs(                                                              # editar o texto do título como markdown.
    title = "Total de lançamentos por *<span style = 'color:#CC5A71;'>dia </span>*",
    subtitle ="Os dias com maior e menor quantidade de estreias",             # editar o subtítulo como texto.
    y = "Quantidade de filmes
    ",
    x = "
    Dias do mês"
  ) +  
  theme_classic()+                                                               # seleção do tema do gráfico.
  scale_y_continuous(breaks = seq(0, 8000, 1000),
                     limits = c(0,8000)) +                                        # formatar escala do eixo y.  
  scale_x_continuous(breaks = seq(0, 31, 1)) +                                    # formatar escala do eixo x.
  theme(                                                          # customizar:
    panel.background = element_rect(fill = "#FFFFFF"),                                     # fundo do gráfico.
    plot.background = element_rect(fill = "#FFFFFF"),                           # fundo da moldura retangular.
    plot.margin = unit(c(1, 1, 1, 1), "cm"),                                          # distância das margens.
    plot.title = element_markdown(                                                        # título do gráfico.
      size = 24,  
      face = "plain",    
      family = "Arial",                                                                     # fonte do título.
      hjust = 0,
      margin = unit(c(0, 0, 0.5, 0), "cm")                                                # margens do título.
    ), 
    plot.subtitle = element_text(
      size = 14,
      family = "Arial",   
      hjust = 0,
      face = "plain"
    ),
    text = element_text(                                                                  # textos do gráfica.
      family = "Arial",    
      color = "#000000",
      size = 14,
      hjust = 0,
      face = "plain"
    ),
    axis.text.x = element_text(                                                             # texto do eixo x.
      color = "#000000",       
      size = 15,
      face = "plain",
      margin = unit(c(0.3, 0, 0.5, 0), "cm")  
    ),
    axis.text.y = element_markdown(                                                         # texto do eixo y.
      color = "#000000",    
      size = 15,
      face = "plain",
      family = "Arial", 
    ),
    axis.ticks.x = element_line(color = "#1F1F1F"),                                         # ticks do eixo x.
    axis.line.x = element_line(color = "#1F1F1F"),                                   # cor da linha do eixo x.
    axis.ticks.y = element_line(color = "#1F1F1F"),                             # padrões dos ticks do eixo x.
    axis.line.y = element_line(
      color = "#000000",                                                             # cor da linha do eixo x.
      size = 0.4,
    ),
    axis.title = element_text(                                                    # texto do título do eixo x.
      face = "plain",    
      size = 16,
      hjust = 0.5,
    ),
    legend.position = "none"                                                               # exclui a legenda.
  ) 

plot_dia_filmes                                                                         # visualiza o gráfico.






### Colocando o resultado em um gráfico de barras com os 20 paises -----------------------------------

### Manipulando os dados para obtenção dos 16 países com maiores quantidades de filmes lançados------
top_16_paises <- imdb |> 
  group_by(pais) |> 
  summarise(qtde = n()) |> 
  arrange(desc(qtde)) |> 
  slice_head(n=16)

### Palete de cores

cor <- c(                                                       # paleta  de cores para os elementos gráficos.
  "#c42847", "#ffad05", "#7d5ba6", "#00bbf9", "#2bc016",
  "#B2DDF7", "#B2DDF7", "#B2DDF7", "#B2DDF7", "#B2DDF7",
  "#B2DDF7", "#B2DDF7", "#B2DDF7", "#B2DDF7", "#B2DDF7",
  "#365F6D"
)

### Gráfico com os 16 países com maior número de filmes produzidos

plot_top_16_paises <- top_16_paises |> 
  ggplot(aes(y = fct_reorder(pais,      # reordenar a sequência dos países em ordem decrescente da quantidade.
                             qtde, 
                             .desc = FALSE
                             ),
             x = qtde, 
             label = qtde
             )
         ) +
  geom_col(fill = cor)+
  geom_label(size = 4.5,                                                                 # formatar os labels.
             alpha = 0,
             label.size = NA,    
             fontface = "plain",  
             color = "#000000",
             hjust = -0.3
             ) +
  geom_curve(aes(y = 14,                   # formatar curva para anotação " Relação dos países com mais.....".
                 x = 6000,  
                 xend = 15000,     
                 yend = 10
                 ),
             arrow = arrow(type = "closed",                                         # formata a seta da linha.
                           length = unit(0.02, "npc"
                                         )
                           ),
             curvature = -0.14,  
             color = "#000000" 
             ) +
  geom_curve(aes(y = 1,                             # formatar curva para anotação "Décima sexta posição....".
                 x = 2000,  
                 xend = 9500,     
                 yend = 2
                 ),
             arrow = arrow(type = "closed",                                         # formata a seta da linha.
                           length = unit(0.02,
                                         "npc") 
                           ),
             curvature = 0,                                                           # curvatura = 0 -- reta.
             color = "#000000"
             )+
  labs(                                                                            # define textos do gráfico.
    title = "Países com maior quantidade de lançamentos de filmes",
    subtitle = "Destaque para os cinco paises com maiores notas",
    x = "Quantidade de filmes lançados",
    y = "Paises"
  )+
  scale_x_continuous(                                                    # altera limites e quebras do eixo x.
    breaks = seq(0, 29000,10000),
    limits = c(0,29000)
  )+
  theme_classic()+                                                                 # define o tema do gráfico.
  theme(                                                  # Customizar o gráfico:
    panel.background = element_rect(fill = "#FFFFFF"),                            # altera o fundo do gráfico.
    plot.background = element_rect(fill = "#FFFFFF"),                  # altera o fundo da moldura retangular.
    legend.position = "none",                                                               # elimina legenda.
    plot.margin = unit(c(1, 1.5, 0.5, 1), "cm"),                          # distâncias do gráfico das margens.
    plot.title = element_text(                                          # define padrões do título do gráfico.
      size = 24,
      family = "Arial",
      margin = unit(c(0.3, 0, 0.5, -0,2), "cm")
    ), 
    plot.subtitle = element_text(                                    # define padrões do subtítulo do gráfico.
      size = 15,
      family = "Arial",
      margin = unit(c(0, 0, 0.5, 0), "cm")
    ),
    text = element_text(                                               # define padrões dos textos do gráfico.
      family = "Arial",    
      color = "#022859",
      size = 13,
    ),
    axis.text.x = element_text(                                           # define padrões do texto do eixo x.
      color = "#022859",       
      size = 14,
      family = "Arial",
      margin = unit(c(0.5, 0, 0.5, 0), "cm")    
    ),
    axis.text.y = element_markdown(                                       # define padrões do texto do eixo y.
      color = "#022859",    
      size = 14,
      family = "Arial", 
      margin = unit(c(0, 0.5, 0, 0.5), "cm")
    ),
    axis.ticks.x = element_line(color = "#1F1F1F"),                                         # ticks do eixo x.
    axis.line.x = element_line(color = "#1F1F1F"),                                   # cor da linha do eixo x.
    axis.ticks.y = element_line(color = "#1F1F1F"),                             # padrões dos ticks do eixo x.
    axis.line.y = element_line(
      color = "#000000",                                                             # cor da linha do eixo x.
      size = 0.4,
    ),
    axis.title = element_text(                                                    # texto do título do eixo x.
      face = "plain",    
      size = 15,
      hjust = 0.5,
    )
  )+    
  annotate( "text",
            x = 21000, y = 9, label = "Relação dos cinco países com mais lançamentos 
            registrados na base IMDB",
            color = "#000000", 
            size = 4,
            family = "Arial"
  )+
  annotate( "text",
            x = 14000, y = 2, label = "Décima sexta posição do Brasil",
            color = "#000000", 
            size = 4,
            family = "Arial"
  )

plot_top_16_paises

# os filmes brasileiros com maior ranking

imdb |> 
  filter(pais == "Brazil" & num_avaliacoes >= 10000) |> 
  summarise( titulo, 
             ano, 
             genero, 
             nota_imdb,
             num_avaliacoes)|>
  arrange(desc(nota_imdb)) |> 
  slice_head(n = 5)

# 5) Listar todas as moedas nas colunas orcamento e receita -------------------------------

moedas_orcamento <- imdb |> 
  select(orcamento) |>
  mutate(moeda = str_extract(string = orcamento,
                             pattern = "[\\w | \\$].* " )
         ) |> 
  distinct(moeda) |> 
  drop_na()

moedas_orcamento <- moedas_orcamento |> 
  mutate(moeda = str_trim(moeda)) |> 
  mutate(moeda = str_replace(moeda, pattern = "\\$",
                             replacement = "USD"))



## importar a tabela xls "siglas_moedas_paises"

tab_moedas <- readxl::read_xlsx("data-raw/siglas_moedas_paises.xlsx")


## juntar as tabelas moedas_receita e tab_moedas por meio da coluna moeda

moedas_orcamento_unificada<- left_join(x = moedas_orcamento,
                                       y = tab_moedas, 
                                       by = "moeda",
                                       copy = TRUE) |> 
  distinct(moeda, divisa) |> 
  mutate(divisa = str_to_title(divisa))
  
moedas_orcamento_unificada |> 
  kbl(
    align = "l",                                            # alinhamento do texto do cabeçalho.
    col.names = c("Moeda",                                           # define o nome das colunas.
                  "Divisa"),
    caption = "Moedas dos orçamentos dos filmes"
  ) |> 
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    html_font = "get_schwifty",
    font_size = 10,       
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, background = "#EDF6FD")
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                            # altera as configurações da tabela.
              bold = FALSE,
              # background = "#022859", 
              # color = "#FFFFFF",
              width = "5cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              # background = "#022859",
              # color = "#FFFFFF",
              width = "5cm"
  ) |> 
  footnote(general = "Base de dados IMDB (Internet Movie Database).",
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Fonte:",
  )


# Maiores lucros por gênero e maiores notas médias por gênero -----------------------------

# considerando apenas o valores em dólares

filmes_dolares <- imdb |>
  select(orcamento, receita, genero, titulo) |> 
  filter(str_detect(orcamento, pattern = "\\$.*"))

# eliminar o símbolo $ nos valores das colunas orçamento e receita

filmes_dolares <- filmes_dolares |> 
  mutate(orcamento = str_remove(orcamento, pattern = "\\$ "),
         receita = str_remove(receita, pattern = "\\$ "))


# alterar o tipo das variáveis orcamento e  receita

filmes_dolares$orcamento <- as.numeric(filmes_dolares$orcamento)
filmes_dolares$receita <- as.numeric(filmes_dolares$receita)

  
# calcular o lucro dos filmes

filmes_dolares <- filmes_dolares |> 
  mutate(
    lucro = receita - orcamento
  ) |>
  drop_na()


# usar a base imdb, precisamos fazer a separação da coluna genero

filmes_dolares <- filmes_dolares |> 
  separate(col = genero,
           into = c("genero1", "genero2", "genero3"),
           sep = ","
  )

# pivotar a base de wide para long

filmes_dolares <- filmes_dolares |> 
  pivot_longer(
    cols = c("genero1", "genero2", "genero3"),
    names_to = "tipos_generos",
    values_to = "generos",
    values_drop_na = TRUE
  ) |> 
  mutate(generos = str_trim(generos))

# maiores lucros por gênero

lucros_genero <- filmes_dolares |>
  group_by(generos) |>
  summarise(lucro_total = sum(lucro)) |> 
  arrange(desc(lucro_total)) |> 
  slice_head(n=10)


lucros_genero |> 
  kbl(
    align = "l",                                            # alinhamento do texto do cabeçalho.
    col.names = c("Gêneros",                                           # define o nome das colunas.
                  "Lucro Total (US$) "),
    caption = "Maiores lucros por gênero"
  ) |> 
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    html_font = "get_schwifty",
    font_size = 10,       
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, background = "#EDF6FD")
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                            # altera as configurações da tabela.
              bold = FALSE,
              # background = "#022859", 
              # color = "#FFFFFF",
              width = "5cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              # background = "#022859",
              # color = "#FFFFFF",
              width = "5cm"
  ) |> 
  footnote(general = "Base de dados IMDB (Internet Movie Database).",
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Fonte:",
  )


lucros_genero_filme <- filmes_dolares |>
  group_by(generos) |>
  summarise(lucro_total = sum(lucro)/n()) |> 
  arrange(desc(lucro_total)) |> 
  slice_head(n=10)

lucros_genero_filme |> 
  kbl(
    align = "l",                                            # alinhamento do texto do cabeçalho.
    col.names = c("Gêneros",                                           # define o nome das colunas.
                  "Lucro Total por Filme Produzido (US$)"),
    #caption = "Maiores lucros por filme produzido em cada gênero"
  ) |> 
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    html_font = "get_schwifty",
    font_size = 10,       
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, background = "#EDF6FD")
  ) |> 
  kable_classic_2() |> 
  column_spec(1,                                            # altera as configurações da tabela.
              bold = FALSE,
              # background = "#022859", 
              # color = "#FFFFFF",
              width = "5cm"
  ) |>      
  column_spec(2, 
              bold = FALSE,
              # background = "#022859",
              # color = "#FFFFFF",
              width = "5cm"
  ) |> 
  footnote(general = "Base de dados IMDB (Internet Movie Database).",
           footnote_as_chunk = TRUE,
           fixed_small_size = TRUE,
           general_title = "Fonte:",
  )



# Maiores notas medias por genero -----------------------------------------------------------------------

# Fazer o join de imdb e imdb_avaliacoes

imdb_join_aval <- left_join(imdb, imdb_avaliacoes, by = "id_filme")


