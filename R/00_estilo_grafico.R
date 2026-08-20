# ================================================================
# 00 - ESTILO GLOBAL DOS GRÁFICOS
# Projeto ENAMED 2025
# ================================================================
library(ggthemes)

# ----------------------------------------------------------------
# Pacotes
# ----------------------------------------------------------------

library(ggplot2)

if (!requireNamespace("ggthemes", quietly = TRUE)) {
  stop(
    "O pacote 'ggthemes' não está instalado. ",
    "Execute no Console: install.packages('ggthemes')"
  )
}


# ================================================================
# PALETA DE CORES
# ================================================================


# ----------------------------------------------------------------
# Redes de ensino
#
# As cores representam CATEGORIAS.
# Não significam melhor ou pior desempenho.
# ----------------------------------------------------------------

cores_rede <- c(
  "Pública"  = "#0072B2",
  "Privada"  = "#C97A00",
  "Especial" = "#7A5195"
)

# ----------------------------------------------------------------
# Desempenho
#
# As cores possuem significado SEMÂNTICO.
# ----------------------------------------------------------------

cores_desempenho <- c(
  "Bom"     = "#009E73",
  "Ruim"    = "#D55E00",
  "Neutro"  = "#6B7280"
)


# ----------------------------------------------------------------
# Proficiência
# ----------------------------------------------------------------

cores_proficiencia <- c("Proficiente"      = "#009E73",
                        "Abaixo do corte"  = "#D55E00")

# ----------------------------------------------------------------
# Concentração
# ----------------------------------------------------------------

cores_concentracao <- c(
  "IES" = "#6B7280",
  "IES no grupo" = "#6B7280",
  "Concluintes abaixo do corte" = "#D55E00"
)


# ----------------------------------------------------------------
# Quadrantes de desempenho
# ----------------------------------------------------------------

cores_quadrante <- c(
  "Média acima / dispersão abaixo" = "#009E73",
  "Média acima / dispersão acima"  = "#56B4E9",
  "Média abaixo / dispersão abaixo" = "#E69F00",
  "Média abaixo / dispersão acima" = "#D55E00"
)

# ----------------------------------------------------------------
# Situação das redes em relação às referências nacionais
# ----------------------------------------------------------------

cores_situacao_estado <- c(
  "Ambas melhores que a referência" = "#009E73",
  "Pública melhor / Privada pior"    = "#0072B2",
  "Pública pior / Privada melhor"    = "#C97A00",
  "Ambas piores que a referência"    = "#D55E00"
)


# ----------------------------------------------------------------
# Cores auxiliares
# ----------------------------------------------------------------

cor_referencia <- "#4B5563"

cor_grade <- "#E5E7EB"

cor_texto <- "#30343B"

cor_texto_secundario <- "#606770"

cor_fundo <- "#FFFFFF"


# ================================================================
# ESCALAS PARA REDE
# ================================================================


# ----------------------------------------------------------------
# Cor das redes
# Uso:
#   scale_color_rede()
# ----------------------------------------------------------------

scale_color_rede <- function(...) {
  scale_color_manual(values = cores_rede,
                     breaks = c("Pública", "Privada", "Especial"),
                     ...)
}


# ----------------------------------------------------------------
# Preenchimento das redes
# Uso:
#   scale_fill_rede()
# ----------------------------------------------------------------

scale_fill_rede <- function(...) {
  scale_fill_manual(values = cores_rede,
                    breaks = c("Pública", "Privada", "Especial"),
                    ...)
}


# ================================================================
# ESCALAS SEMÂNTICAS DE DESEMPENHO
# ================================================================


# ----------------------------------------------------------------
# Cor
# ----------------------------------------------------------------

scale_color_desempenho <- function(...) {
  scale_color_manual(values = cores_desempenho, ...)
}


# ----------------------------------------------------------------
# Preenchimento
# ----------------------------------------------------------------

scale_fill_desempenho <- function(...) {
  scale_fill_manual(values = cores_desempenho, ...)
}


# ----------------------------------------------------------------
# Preenchimento dos quadrantes
# ----------------------------------------------------------------

scale_fill_quadrante <- function(...) {
  scale_fill_manual(
    values = cores_quadrante,
    breaks = c(
      "Média acima / dispersão abaixo",
      "Média acima / dispersão acima",
      "Média abaixo / dispersão abaixo",
      "Média abaixo / dispersão acima"
    ),
    ...
  )
}

# ----------------------------------------------------------------
# Cor das situações estaduais
# ----------------------------------------------------------------

scale_color_situacao_estado <- function(...) {
  scale_color_manual(
    values = cores_situacao_estado,
    breaks = c(
      "Ambas melhores que a referência",
      "Pública melhor / Privada pior",
      "Pública pior / Privada melhor",
      "Ambas piores que a referência"
    ),
    ...
  )
}



# ================================================================
# ESCALAS DE PROFICIÊNCIA
# ================================================================


# ----------------------------------------------------------------
# Cor
# ----------------------------------------------------------------

scale_color_proficiencia <- function(...) {
  scale_color_manual(values = cores_proficiencia, ...)
}


# ----------------------------------------------------------------
# Preenchimento
# ----------------------------------------------------------------

scale_fill_proficiencia <- function(...) {
  scale_fill_manual(values = cores_proficiencia, ...)
}


# ----------------------------------------------------------------
# Preenchimento dos gráficos de concentração
# ----------------------------------------------------------------

scale_fill_concentracao <- function(...) {
  scale_fill_manual(values = cores_concentracao, ...)
}

# ================================================================
# TEMA GLOBAL
# ================================================================


tema_enamed <- function(base_size = 11,
                        base_family = "sans",
                        legenda = "right") {
  ggthemes::theme_few(base_size = base_size, base_family = base_family) +
    
    theme(
      # ----------------------------------------------------------
      # Fundo
      # ----------------------------------------------------------
      
      plot.background = element_rect(fill = cor_fundo, color = NA),
      
      panel.background = element_rect(fill = cor_fundo, color = NA),
      
      
      # ----------------------------------------------------------
      # Título
      # ----------------------------------------------------------
      
      plot.title = element_text(
        size = base_size * 1.30,
        face = "bold",
        color = cor_texto,
        margin = margin(b = 5)
      ),
      
      
      # ----------------------------------------------------------
      # Subtítulo
      # ----------------------------------------------------------
      
      plot.subtitle = element_text(
        size = base_size,
        color = cor_texto_secundario,
        margin = margin(b = 10)
      ),
      
      
      # ----------------------------------------------------------
      # Títulos dos eixos
      # ----------------------------------------------------------
      
      axis.title = element_text(size = base_size * 0.92, color = cor_texto),
      
      axis.title.x = element_text(margin = margin(t = 8)),
      
      axis.title.y = element_text(margin = margin(r = 8)),
      
      
      # ----------------------------------------------------------
      # Valores dos eixos
      # ----------------------------------------------------------
      
      axis.text = element_text(size = base_size * 0.82, color = cor_texto),
      
      
      # ----------------------------------------------------------
      # Grades
      # ----------------------------------------------------------
      
      panel.grid.major = element_line(color = cor_grade, linewidth = 0.35),
      
      panel.grid.minor = element_blank(),
      
      
      # ----------------------------------------------------------
      # Legenda
      # ----------------------------------------------------------
      
      legend.position = legenda,
      
      legend.title = element_text(
        size = base_size * 0.90,
        face = "bold",
        color = cor_texto
      ),
      
      legend.text = element_text(size = base_size * 0.85, color = cor_texto),
      
      legend.key = element_blank(),
      
      
      # ----------------------------------------------------------
      # Facetas
      # ----------------------------------------------------------
      
      strip.background = element_blank(),
      
      strip.text = element_text(
        size = base_size * 0.92,
        face = "bold",
        color = cor_texto
      ),
      
      
      # ----------------------------------------------------------
      # Fonte / legenda inferior
      # ----------------------------------------------------------
      
      plot.caption = element_text(
        size = base_size * 0.72,
        color = cor_texto_secundario,
        hjust = 0,
        margin = margin(t = 10)
      ),
      
      
      # ----------------------------------------------------------
      # Alinhamento
      # ----------------------------------------------------------
      
      plot.title.position = "plot",
      
      plot.caption.position = "plot",
      
      
      # ----------------------------------------------------------
      # Margens do gráfico
      # ----------------------------------------------------------
      
      plot.margin = margin(
        t = 10,
        r = 14,
        b = 10,
        l = 10
      )
    )
}


# ================================================================
# TEMA SEM LEGENDA
#
# Útil quando a informação já está explícita no próprio gráfico.
# ================================================================

tema_enamed_sem_legenda <- function(base_size = 11,
                                    base_family = "sans") {
  tema_enamed(base_size = base_size,
              base_family = base_family,
              legenda = "none")
}