# ----------------------------------------------------------------
# Objetos utilizados no relatório
# ----------------------------------------------------------------

# Este script deve ser executado após os scripts de análise 01 a 06.


# ----------------------------------------------------------------
# Funções de formatação
# ----------------------------------------------------------------

formata_decimal <- function(x) {
  format(
    round(x, 1),
    decimal.mark = ",",
    big.mark = ".",
    nsmall = 1,
    scientific = FALSE,
    trim = TRUE
  )
}

formata_inteiro <- function(x) {
  format(
    x,
    decimal.mark = ",",
    big.mark = ".",
    scientific = FALSE,
    trim = TRUE
  )
}


# ----------------------------------------------------------------
# Número de participantes
# ----------------------------------------------------------------

n_participantes <- nrow(enamed)

n_participantes_fmt <- formata_inteiro(n_participantes)


# ----------------------------------------------------------------
# Notas médias por rede
# ----------------------------------------------------------------

media_publica <- resumo_rede$media[resumo_rede$rede == "Pública"]

media_privada <- resumo_rede$media[resumo_rede$rede == "Privada"]

media_especial <- resumo_rede$media[resumo_rede$rede == "Especial"]


# Versões formatadas para o relatório

media_publica_fmt <- formata_decimal(media_publica)

media_privada_fmt <- formata_decimal(media_privada)

media_especial_fmt <- formata_decimal(media_especial)


# ----------------------------------------------------------------
# Diferença entre as médias pública e privada
# ----------------------------------------------------------------

diferenca_media_publica_privada <-
  media_publica - media_privada

diferenca_media_publica_privada_fmt <-
  formata_decimal(diferenca_media_publica_privada)


# ----------------------------------------------------------------
# Proficiência nacional
# ----------------------------------------------------------------

prof_nacional <-
  resumo_proficiencia_brasil$prop_proficientes

abaixo_corte_nacional <-
  resumo_proficiencia_brasil$prop_nao_proficientes


# Versões percentuais formatadas

prof_nacional_pct_fmt <-
  formata_decimal(100 * prof_nacional)

abaixo_corte_nacional_pct_fmt <-
  formata_decimal(100 * abaixo_corte_nacional)


# ----------------------------------------------------------------
# Proficiência por rede
# ----------------------------------------------------------------

prof_publica <-
  resumo_proficiencia_rede$prop_proficientes[resumo_proficiencia_rede$rede == "Pública"]

prof_privada <-
  resumo_proficiencia_rede$prop_proficientes[resumo_proficiencia_rede$rede == "Privada"]

prof_especial <-
  resumo_proficiencia_rede$prop_proficientes[resumo_proficiencia_rede$rede == "Especial"]


# Versões percentuais formatadas

prof_publica_pct_fmt <-
  formata_decimal(100 * prof_publica)

prof_privada_pct_fmt <-
  formata_decimal(100 * prof_privada)

prof_especial_pct_fmt <-
  formata_decimal(100 * prof_especial)

# ----------------------------------------------------------------
# Concentração dos concluintes abaixo do corte por rede
# ----------------------------------------------------------------

prop_ies_grupo_publica <-
  grafico_concentracao_rede$data %>%
  filter(rede == "Pública", metrica == "IES no grupo") %>%
  pull(proporcao)

prop_concluintes_grupo_publica <-
  grafico_concentracao_rede$data %>%
  filter(rede == "Pública", metrica == "Concluintes abaixo do corte") %>%
  pull(proporcao)


prop_ies_grupo_privada <-
  grafico_concentracao_rede$data %>%
  filter(rede == "Privada", metrica == "IES no grupo") %>%
  pull(proporcao)

prop_concluintes_grupo_privada <-
  grafico_concentracao_rede$data %>%
  filter(rede == "Privada", metrica == "Concluintes abaixo do corte") %>%
  pull(proporcao)


prop_ies_grupo_especial <-
  grafico_concentracao_rede$data %>%
  filter(rede == "Especial", metrica == "IES no grupo") %>%
  pull(proporcao)

prop_concluintes_grupo_especial <-
  grafico_concentracao_rede$data %>%
  filter(rede == "Especial", metrica == "Concluintes abaixo do corte") %>%
  pull(proporcao)


# ----------------------------------------------------------------
# Valores formatados
# ----------------------------------------------------------------

prop_ies_grupo_publica_pct_fmt <-
  formata_decimal(100 * prop_ies_grupo_publica)

prop_concluintes_grupo_publica_pct_fmt <-
  formata_decimal(100 * prop_concluintes_grupo_publica)

prop_ies_grupo_privada_pct_fmt <-
  formata_decimal(100 * prop_ies_grupo_privada)

prop_concluintes_grupo_privada_pct_fmt <-
  formata_decimal(100 * prop_concluintes_grupo_privada)

prop_ies_grupo_especial_pct_fmt <-
  formata_decimal(100 * prop_ies_grupo_especial)

prop_concluintes_grupo_especial_pct_fmt <-
  formata_decimal(100 * prop_concluintes_grupo_especial)