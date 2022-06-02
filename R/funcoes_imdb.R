
# Função tema dos gráficos

theme_imdb <- function() {
  theme(                                                          # customizar:
    panel.background = element_rect(fill = "#FFFFFF"),                                     # fundo do gráfico.
    plot.background = element_rect(fill = "#FFFFFF"),                           # fundo da moldura retangular.
    plot.margin = unit(c(1, 1, 1, 1), "cm"),                                          # distância das margens.
    plot.title = element_markdown(                                            # título do gráfico em markdown.
      size = 24,
      family = "",                                                                          # fonte do título.
      margin = unit(c(0, 0, 0.5, 0), "cm")                                                # margens do título.
    ), 
    plot.subtitle = element_text(                                                        #textos do subtitulo.
      size = 14,
      family = "",   
    ),
    text = element_text(                                                                  # textos do gráfico.
      family = "",    
      color = "#000000",
      size = 14
    ),
    axis.text.x = element_text(                                                             # texto do eixo x.
      color = "#000000",       
      size = 14,
      margin = unit(c(0.5, 0, 0.5, 0), "cm")  
    ),
    axis.text.y = element_text(                                                            # texto do eixo y.
      color = "#000000",    
      size = 14,
      family = "", 
      margin = unit(c(0, 0.5, 0, 0.5), "cm") 
    ),
    axis.ticks.x = element_line(color = "#000000"),                                         # ticks do eixo x.
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
    legend.position = "none",                                                              # exclui a legenda.
  ) 
}


# função formata valores em dólares

format_dolar <- function(valores, nsmall = 2) {
  valores |> 
    as.numeric() |> 
    format(nsmall = nsmall, decimal.mark = ".", big.mark = ",") |> 
    str_trim() 
}


