# ================================================================
# 04 - ANÁLISE DA DISTRIBUIÇÃO DO DESEMPENHO
# ================================================================

library(tidyverse)
library(ggplot2)

enamed <- readRDS("dados/processados/enamed.rds")

ies_enamed_identificadas <- readRDS("dados/processados/ies_enamed_identificadas.rds")


# ----------------------------------------------------------------
# 1. Distribuição nacional
# ----------------------------------------------------------------

# Base para comparação entre as redes pública e privada
enamed_rede <- enamed %>%
  filter(rede %in% c("Pública", "Privada"))
# Estatísticas da distribuição das notas por rede
distribuicao_brasil <- enamed_rede %>%
  group_by(rede) %>%
  summarise(
    n = n(),
    media = mean(NT_GER),
    p05 = quantile(NT_GER, .05),
    p10 = quantile(NT_GER, .1),
    p25 = quantile(NT_GER, .25),
    mediana = median(NT_GER),
    p75 = quantile(NT_GER, .75),
    p90 = quantile(NT_GER, .90),
    p95 = quantile(NT_GER, .95),
    desvio_padrao = sd(NT_GER),
    .groups = "drop"
  )

# Distribuição nacional das notas por rede
enamed_rede %>%
  ggplot(aes(x = NT_GER, color = rede, fill = rede)) +
  geom_density(linewidth = 0.5, alpha = 0.35) +
  labs(
    title = "Distribuição das Notas do ENAMED 2025",
    subtitle = "Comparação entre redes pública e privada",
    x = "Nota Geral",
    y = "Densidade",
    color = "Rede",
    fill = "Rede"
  ) +
  theme_minimal()


# Distribuição nacional em números absolutos de alunos
ggplot(enamed_rede, aes(x = NT_GER, fill = rede, color = rede)) +
  geom_histogram(binwidth = 2,
                 position = "identity",
                 alpha = 0.35) +
  labs(
    title = "Distribuição das notas do ENAMED 2025",
    subtitle = "Número de participantes por faixa de nota",
    x = "Nota geral",
    y = "Número de participantes",
    fill = "Rede",
    color = "Rede"
  ) +
  theme_minimal()

# Pontos de corte nacionais para a cauda inferior
cortes_baixo_desempenho <- enamed_rede %>%
  summarise(
    p05_nacional = quantile(NT_GER, 0.05),
    p10_nacional = quantile(NT_GER, 0.10),
    p25_nacional = quantile(NT_GER, 0.25)
  )
# Número e proporção de alunos abaixo dos cortes nacionais
baixo_desempenho_rede <- enamed_rede %>%
  group_by(rede) %>%
  summarise(
    n = n(),
    n_abaixo_p05 = sum(NT_GER <= cortes_baixo_desempenho$p05_nacional),
    prop_abaixo_p05 = mean(NT_GER <= cortes_baixo_desempenho$p05_nacional),
    n_abaixo_p10 = sum(NT_GER <= cortes_baixo_desempenho$p10_nacional),
    prop_abaixo_p10 = mean(NT_GER <= cortes_baixo_desempenho$p10_nacional),
    n_abaixo_p25 = sum(NT_GER <= cortes_baixo_desempenho$p25_nacional),
    prop_abaixo_p25 = mean(NT_GER <= cortes_baixo_desempenho$p25_nacional),
    .groups = "drop"
  )

cortes_referencia <- distribuicao_brasil %>%
  select(rede, p05, p10, p25)

cortes_publica <- cortes_referencia %>%
  filter(rede == "Pública")

cortes_privada <- cortes_referencia %>%
  filter(rede == "Privada")

# Comparação cruzada dos percentis de referência das duas redes
comparacao_cortes_cruzados <- enamed_rede %>%
  group_by(rede) %>%
  summarise(
    prop_abaixo_p05_publica = mean(NT_GER <= cortes_publica$p05),
    prop_abaixo_p10_publica = mean(NT_GER <= cortes_publica$p10),
    prop_abaixo_p25_publica = mean(NT_GER <= cortes_publica$p25),
    
    prop_abaixo_p05_privada = mean(NT_GER <= cortes_privada$p05),
    prop_abaixo_p10_privada = mean(NT_GER <= cortes_privada$p10),
    prop_abaixo_p25_privada = mean(NT_GER <= cortes_privada$p25),
    
    .groups = "drop"
  )

comparacao_cortes_grafico <- comparacao_cortes_cruzados %>%
  pivot_longer(cols = -rede,
               names_to = "corte_referencia",
               values_to = "proporcao")

comparacao_cortes_grafico <- comparacao_cortes_grafico %>%
  separate_wider_delim(
    corte_referencia,
    delim = "_",
    names = c("prop", "abaixo", "percentil_referencia", "rede_referencia")
  ) %>%
  select(rede, percentil_referencia, rede_referencia, proporcao) %>%
  mutate(
    rede_referencia = case_when(
      rede_referencia == "publica" ~ "Pública",
      rede_referencia == "privada" ~ "Privada"
    )
  )

comparacao_cortes_grafico %>%
  ggplot(aes(x = percentil_referencia, y = proporcao, fill = rede)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::percent(proporcao, accuracy = 0.1)),
            position = position_dodge(width = 0.9),
            vjust = -0.3) +
  facet_wrap( ~ rede_referencia, labeller = labeller(
    rede_referencia = c("Privada" = "Referência: rede privada", "Pública" = "Referência: rede pública")
  )) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "Participantes abaixo dos percentis de referência",
    subtitle = "Comparação entre rede pública e privada no ENAMED 2025",
    x = "Percentil da rede de referência",
    y = "Proporção de participantes",
    fill = "Rede dos participantes"
  ) +
  theme_minimal()


# ----------------------------------------------------------------
# 2. Distribuição por estado
# ----------------------------------------------------------------

# Pública x Privada por UF
# diferenças de média
# medianas
# caudas da distribuição


# ----------------------------------------------------------------
# 3. Alunos de baixo desempenho
# ----------------------------------------------------------------

# definição do critério
# proporção por rede
# proporção por UF
# proporção por curso
# proporção por IES


# ----------------------------------------------------------------
# 4. Distribuição por IES
# ----------------------------------------------------------------

# instituições com maior/menor desempenho
# cauda inferior
# tamanho da IES
# heterogeneidade entre cursos