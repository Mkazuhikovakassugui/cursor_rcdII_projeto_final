
# Função tema dos gráficos
theme_imdb <- function() {
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
}



# Função kable_styling_imdb

tabela_estilo_imdb <- function() {
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    html_font = "",
    font_size = 10,       
    full_width = TRUE, 
    fixed_thead = list(enabled = TRUE, background = "#EDF6FD")
  )
}

