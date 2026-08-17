# ================================================================
# 04 - ANÁLISE DA DISTRIBUIÇÃO DO DESEMPENHO
# ================================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)

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

# Proporção abaixo do P10 nacional da rede pública por UF e rede
baixo_desempenho_estado <- enamed_rede %>%
  group_by(UF, rede) %>%
  summarise(
    n = n(),
    n_abaixo_p10_publica = sum(NT_GER <= cortes_publica$p10),
    prop_abaixo_p10_publica = mean(NT_GER <= cortes_publica$p10),
    .groups = "drop"
  )

comparacao_baixo_estado <- baixo_desempenho_estado %>%
  select(UF, rede, n, prop_abaixo_p10_publica) %>%
  pivot_wider(names_from = rede,
              values_from = c(n, prop_abaixo_p10_publica))

comparacao_redes_estado <- comparacao_baixo_estado %>%
  drop_na(prop_abaixo_p10_publica_Privada,
          prop_abaixo_p10_publica_Pública) %>%
  mutate(
    diferenca_pp =
      prop_abaixo_p10_publica_Privada -
      prop_abaixo_p10_publica_Pública,
    
    razao_prevalencia =
      prop_abaixo_p10_publica_Privada /
      prop_abaixo_p10_publica_Pública,
    n_menor_rede =
      pmin(n_Privada, n_Pública)
  ) %>%
  arrange(desc(diferenca_pp))

# Referências nacionais para o corte no P10 da rede pública
ref_p10_publica <- comparacao_cortes_cruzados %>%
  filter(rede == "Pública") %>%
  pull(prop_abaixo_p10_publica)

ref_p10_privada <- comparacao_cortes_cruzados %>%
  filter(rede == "Privada") %>%
  pull(prop_abaixo_p10_publica)

comparacao_redes_estado <- comparacao_redes_estado %>%
  mutate(
    situacao = case_when(
      prop_abaixo_p10_publica_Pública <= ref_p10_publica &
        prop_abaixo_p10_publica_Privada <= ref_p10_privada ~
        "Ambas melhores que a referência",
      
      prop_abaixo_p10_publica_Pública <= ref_p10_publica &
        prop_abaixo_p10_publica_Privada > ref_p10_privada ~
        "Pública melhor / Privada pior",
      
      prop_abaixo_p10_publica_Pública > ref_p10_publica &
        prop_abaixo_p10_publica_Privada <= ref_p10_privada ~
        "Pública pior / Privada melhor",
      
      TRUE ~
        "Ambas piores que a referência"
    )
  )
ggplot(comparacao_redes_estado, aes(x = reorder(UF, diferenca_pp), y = diferenca_pp)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = scales::percent(diferenca_pp, accuracy = 0.1)),
            hjust = -0.1,
            size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     expand = expansion(mult = c(0, .12))) +
  labs(
    title = "Diferença de Proporção de baixo desempenho por estado",
    subtitle = "Rede privada menos rede pública - corte no p10 nacional da rede pública",
    x = "UF",
    y = "Diferença em pontos Percentuais",
    caption = "Valores positivos indicam maior proporção de participantes da rede privada abaixo do P10 nacional da rede pública."
  ) + theme_minimal()

ggplot(
  comparacao_redes_estado,
  aes(x = prop_abaixo_p10_publica_Pública, y = prop_abaixo_p10_publica_Privada)
) +
  geom_point(aes(color = situacao), size = 2) +
  geom_label_repel(aes(label = UF),
                   size = 3,
                   min.segment.length = 0) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed") +
  geom_vline(xintercept = ref_p10_publica, linetype = "dashed") +
  geom_hline(yintercept = ref_p10_privada, linetype = "dashed") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  coord_equal(xlim = c(0, 0.60), ylim = c(0, 0.60)) +
  labs(
    title = "Participantes abaixo do P10 da rede pública nacional por UF",
    subtitle = paste(
      "Comparação das redes pública e privada com suas respectivas",
      "proporções nacionais, usando a mesma nota de corte"
    ),
    x = "Rede Pública na UF \n(% abaixo da P10 público nacional)",
    y = "Rede Privada na UF \n(% abaixo da P10 público nacional)",
    color = "Desempenho em relação\nà referência nacional",
    caption = paste(
      "Linha diagonal: igualdade entre as redes pública e privada na UF.",
      "Linhas vertical e horizontal: referências nacionais das respectivas redes.",
      "Menores proporções indicam menor concentração de participantes abaixo do corte."
    )
  ) +
  annotate(
    "label",
    x = 0.43,
    y = 0.455,
    label = "Pública melhor",
    angle = 45,
    size = 3,
    linewidth = 0
  ) +
  annotate(
    "label",
    x = 0.455,
    y = 0.43,
    label = "Privada melhor",
    angle = 45,
    size = 3,
    linewidth = 0
  )+
theme_minimal()

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