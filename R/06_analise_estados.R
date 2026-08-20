# ================================================================
# 06 - ANÁLISE POR ESTADO
# ================================================================


library(tidyverse)

source("R/00_estilo_grafico.R")

enamed  <- readRDS("dados/processados/enamed.rds")
cadastro_ies <- readRDS("dados/processados/cadastro_ies.rds")


resumo_uf_rede <- enamed %>%
  filter(rede %in% c("Pública", "Privada")) %>%
  group_by(rede, UF) %>%
  summarise(
    n_participantes = n(),
    n_ies = n_distinct(CO_IES),
    
    media = mean(NT_GER),
    p50 = quantile(NT_GER, .5),
    
    n_proficientes = sum(NT_GER >= 60),
    prop_proficientes = mean(NT_GER >= 60),
    
    n_nao_proficientes = sum(NT_GER < 60),
    prop_nao_proficientes = mean(NT_GER < 60),
    .groups = "drop"
    
  )

resumo_uf_rede_comparavel <- resumo_uf_rede %>%
  group_by(UF) %>%
  filter(n_distinct(rede) == 2) %>%
  ungroup()



comparacao_uf_rede <- resumo_uf_rede_comparavel %>%
  select(UF,
         rede,
         n_participantes,
         media,
         prop_proficientes,
         prop_nao_proficientes) %>%
  pivot_wider(
    names_from = rede,
    values_from = c(
      n_participantes,
      media,
      prop_proficientes,
      prop_nao_proficientes
    ),
    names_sep = "_"
  ) %>%
  mutate(
    diferenca_nao_proficiencia =
      prop_nao_proficientes_Privada - prop_nao_proficientes_Pública,
    
    diferenca_media =
      media_Pública - media_Privada,
    
    diferenca_proficiencia =
      prop_proficientes_Pública -
      prop_proficientes_Privada
  )

referencia_max_privada <- resumo_uf_rede_comparavel %>%
  filter(rede == "Privada") %>%
  summarise(max_privada = max(prop_proficientes)) %>%
  pull(max_privada)

comparacao_uf_rede %>%
  ggplot(aes(y = reorder(UF, diferenca_proficiencia))) +
  
  geom_segment(
    aes(
      x = prop_proficientes_Privada,
      xend = prop_proficientes_Pública,
      yend = reorder(UF, diferenca_proficiencia)
    ),
    color = cor_referencia,
    linewidth = 0.7,
    alpha = .6
  ) +
  
  geom_vline(
    xintercept = referencia_max_privada,
    color = cor_referencia,
    linetype = "dashed",
    linewidth = .7
  ) +
  
  geom_point(
    aes(
      x = prop_proficientes_Privada,
      color = "Privada"
    ),
    size = 2.8
  ) +
  
  geom_point(
    aes(
      x = prop_proficientes_Pública,
      color = "Pública"
    ),
    size = 2.8
  ) +
  
  scale_color_rede() +
  
  scale_x_continuous(
    labels = scales::label_percent(1),
    breaks = seq(0, 1, .1),
    limits = c(0, 1)
  ) +
  
  labs(
    title = "Proficiência dos concluintes por estado e rede",
    subtitle = paste0(
      "Comparação entre redes pública e privada nas 25 UFs ",
      "em que ambas estão presentes — ENAMED 2025"
    ),
    x = "Proporção de concluintes proficientes",
    y = "UF",
    color = "Rede",
    caption = paste0(
      "Proficiência definida como nota geral ≥ 60. ",
      "Linha tracejada: maior proporção de proficiência ",
      "observada na rede privada."
    )
  ) +
  
  tema_enamed()

# ----------------------------------------------------------------
# Comparação entre ponderação por concluintes e peso igual por IES
# ----------------------------------------------------------------

resumo_ies_uf_rede <- enamed %>%
  filter(rede %in% c("Pública", "Privada")) %>%
  group_by(UF, rede, CO_IES) %>%
  summarise(
    n_participantes = n(),
    prop_proficientes_ies = mean(NT_GER >= 60),
    .groups = "drop"
  )


resumo_uf_rede_peso_igual <- resumo_ies_uf_rede %>%
  group_by(UF, rede) %>%
  summarise(
    n_ies = n(),
    prop_proficientes_peso_igual =
      mean(prop_proficientes_ies),
    .groups = "drop"
  )


comparacao_ponderacao_uf <- resumo_uf_rede %>%
  select(UF, rede, n_participantes, prop_proficientes) %>%
  left_join(resumo_uf_rede_peso_igual,
            by = c("UF", "rede"),
            relationship = "one-to-one") %>%
  mutate(efeito_ponderacao =
           prop_proficientes -
           prop_proficientes_peso_igual)


resumo_ies_uf_rede <- resumo_ies_uf_rede %>%
  group_by(UF, rede) %>%
  mutate(prop_participantes_rede =
           n_participantes / sum(n_participantes)) %>%
  ungroup()


ranking_uf_rede <- resumo_uf_rede %>%
  filter(rede %in% c("Pública", "Privada")) %>%
  arrange(rede, desc(prop_proficientes)) %>%
  group_by(rede) %>%
  mutate(posicao = row_number()) %>%
  ungroup()


# ----------------------------------------------------------------
# Ranking das IES dentro de um estado selecionado
# ----------------------------------------------------------------

uf_escolhida <- "SP"

resumo_ies_estado <- enamed %>%
  filter(rede %in% c("Pública", "Privada")) %>%
  group_by(UF, CO_IES, rede) %>%
  summarise(
    n_participantes = n(),
    media = mean(NT_GER),
    
    n_proficientes = sum(NT_GER >= 60),
    prop_proficientes = mean(NT_GER >= 60),
    
    n_nao_proficientes = sum(NT_GER < 60),
    prop_nao_proficientes = mean(NT_GER < 60),
    
    .groups = "drop"
  ) %>%
  left_join(
    cadastro_ies %>%
      select(CO_IES, NO_IES, SG_IES),
    by = "CO_IES",
    relationship = "many-to-one"
  )


ranking_ies_estado <- resumo_ies_estado %>%
  filter(UF == uf_escolhida) %>%
  group_by(SG_IES) %>%
  mutate(rotulo_ies = case_when(is.na(SG_IES) ~ NO_IES, n() > 1 ~ NO_IES, TRUE ~ SG_IES)) %>%
  ungroup()

n_ies_ranking <- nrow(ranking_ies_estado)

n_exibir <- min(20, floor(n_ies_ranking / 2))


# Maiores proporções de proficiência

ranking_ies_estado_top20 <- ranking_ies_estado %>%
  arrange(desc(prop_proficientes), desc(n_participantes)) %>%
  slice_head(n = n_exibir) %>%
  mutate(
    rotulo_proficiencia = paste0(
      scales::percent(prop_proficientes, accuracy = .1),
      " (",
      n_proficientes,
      "/",
      n_participantes,
      ")"
    )
  )


# Maiores proporções abaixo do corte

ranking_ies_estado_down20 <- ranking_ies_estado %>%
  arrange(desc(prop_nao_proficientes), desc(n_participantes)) %>%
  slice_head(n = n_exibir) %>%
  mutate(
    rotulo_nao_proficiencia = paste0(
      scales::percent(prop_nao_proficientes, accuracy = .1),
      " (",
      n_nao_proficientes,
      "/",
      n_participantes,
      ")"
    )
  )


ranking_ies_estado_top20 %>%
  ggplot(
    aes(
      x = reorder(rotulo_ies, prop_proficientes),
      y = prop_proficientes,
      fill = rede
    )
  ) +
  
  geom_col(
    width = .72
  ) +
  
  geom_text(
    aes(label = rotulo_proficiencia),
    hjust = 1.05,
    size = 3
  ) +
  
  scale_fill_rede() +
  
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, .25),
    limits = c(0, 1),
    expand = expansion(mult = c(0, .02))
  ) +
  
  coord_flip() +
  
  labs(
    title = paste0(
      nrow(ranking_ies_estado_top20),
      " IES com maior proporção de concluintes proficientes — ",
      uf_escolhida
    ),
    subtitle = "ENAMED 2025 — corte de proficiência: nota ≥ 60",
    x = "Instituição de Ensino Superior",
    y = "Proporção de concluintes proficientes",
    fill = "Rede",
    caption = "Rótulos: proficientes / participantes válidos."
  ) +
  
  tema_enamed()
ranking_ies_estado_down20 %>%
  ggplot(
    aes(
      x = reorder(rotulo_ies, prop_nao_proficientes),
      y = prop_nao_proficientes,
      fill = rede
    )
  ) +
  
  geom_col(
    width = .72
  ) +
  
  geom_text(
    aes(label = rotulo_nao_proficiencia),
    hjust = 1.05,
    size = 3
  ) +
  
  scale_fill_rede() +
  
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, .25),
    limits = c(0, 1),
    expand = expansion(mult = c(0, .02))
  ) +
  
  coord_flip() +
  
  labs(
    title = paste0(
      nrow(ranking_ies_estado_down20),
      " IES com maior proporção de concluintes abaixo do corte — ",
      uf_escolhida
    ),
    subtitle = "ENAMED 2025 — corte de proficiência: nota ≥ 60",
    x = "Instituição de Ensino Superior",
    y = "Proporção de concluintes abaixo do corte",
    fill = "Rede",
    caption = "Rótulos: abaixo do corte / participantes válidos."
  ) +
  
  tema_enamed()