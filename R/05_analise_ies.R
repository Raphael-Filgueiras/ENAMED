










# ================================================================
# 05 - ANÁLISE POR INSTITUIÇÃO DE ENSINO SUPERIOR
# ================================================================
library(tidyverse)

library(tidyverse)

source("R/00_estilo_grafico.R")

enamed <- readRDS("dados/processados/enamed.rds")

cadastro_ies <- readRDS("dados/processados/cadastro_ies.rds")

enamed_ies <- enamed %>%
  left_join(cadastro_ies, by = "CO_IES", relationship = "many-to-one")

# ----------------------------------------------------------------
# Validação do cadastro das IES
# ----------------------------------------------------------------


if (nrow(enamed_ies) != nrow(enamed)) {
  stop("ERRO: o join com cadastro_ies alterou o número de linhas da base.")
}

if (sum(is.na(enamed_ies$NO_IES)) > 0) {
  warning("Existem IES do ENAMED sem identificação no cadastro_ies.")
}

if (n_distinct(enamed_ies$CO_IES) != n_distinct(enamed$CO_IES)) {
  stop("ERRO: o número de IES mudou após o join com cadastro_ies.")
}


resumo_ies <- enamed_ies %>%
  group_by(CO_IES, NO_IES, SG_IES, rede) %>%
  summarise(
    n_alunos = n(),
    n_cursos = n_distinct(CO_CURSO),
    
    media = mean(NT_GER),
    
    p05 = quantile(NT_GER, .05),
    p10 = quantile(NT_GER, .1),
    p25 = quantile(NT_GER, .25),
    p50 = quantile(NT_GER, .5),
    p75 = quantile(NT_GER, .75),
    p90 = quantile(NT_GER, .9),
    p95 = quantile(NT_GER, .95),
    desvio_padrao = sd(NT_GER),
    
    n_proficientes = sum(NT_GER >= 60),
    prop_proficientes = mean(NT_GER >= 60),
    
    n_nao_proficientes = sum(NT_GER < 60),
    prop_nao_proficientes = mean(NT_GER < 60),
    
    .groups = "drop"
  )

if (any(resumo_ies$n_proficientes +
        resumo_ies$n_nao_proficientes !=
        resumo_ies$n_alunos)) {
  stop("ERRO: proficientes e não proficientes não somam o total de alunos.")
}

resumo_ies <- resumo_ies %>%
  group_by(SG_IES) %>%
  mutate(rotulo_ies = case_when(is.na(SG_IES) ~ NO_IES, n() > 1 ~ NO_IES, TRUE ~ SG_IES)) %>%
  ungroup()

resumo_proficiencia_brasil <- enamed_ies %>%
  summarise(
    n_alunos = n(),
    n_proficientes = sum(NT_GER >= 60),
    prop_proficientes = mean(NT_GER >= 60),
    n_nao_proficientes = sum(NT_GER < 60),
    prop_nao_proficientes = mean(NT_GER < 60)
  )

resumo_proficiencia_rede <- enamed_ies %>%
  group_by(rede) %>%
  summarise(
    n_alunos = n(),
    n_proficientes = sum(NT_GER >= 60),
    prop_proficientes = mean(NT_GER >= 60),
    n_nao_proficientes = sum(NT_GER < 60),
    prop_nao_proficientes = mean(NT_GER < 60),
    .groups = "drop"
  )

referencia_proficiencia_rede <- resumo_proficiencia_rede %>%
  select(rede, prop_nao_proficientes_rede = prop_nao_proficientes)

resumo_ies <- resumo_ies %>%
  left_join(referencia_proficiencia_rede,
            by = "rede",
            relationship = "many-to-one") %>%
  mutate(diferenca_rede = prop_nao_proficientes - prop_nao_proficientes_rede) %>%
  mutate(
    amplitude_interquartil = p75 - p25,
    distancia_p10_corte = p10 - 60,
    distancia_p50_corte = p50 - 60
  )

painel_ies <- resumo_ies %>%
  select(
    CO_IES,
    NO_IES,
    SG_IES,
    rotulo_ies,
    rede,
    
    n_alunos,
    n_cursos,
    
    media,
    p10,
    p25,
    p50,
    p75,
    p90,
    desvio_padrao,
    amplitude_interquartil,
    
    distancia_p10_corte,
    distancia_p50_corte,
    
    n_nao_proficientes,
    prop_nao_proficientes,
    
    prop_nao_proficientes_rede,
    diferenca_rede
  )

painel_ies %>%
  ggplot(aes(x = media, y = prop_nao_proficientes)) +
  geom_point() +
  theme_minimal()

painel_ies %>%
  ggplot(aes(x = media, y = prop_nao_proficientes)) +
  geom_point(aes(size = n_alunos, color = rede), alpha = .7) +
  labs(
    title = "Desempenho das IES no ENAMED 2025",
    subtitle = "Média da nota e proporção de participantes abaixo do corte de proficiência",
    x = "Nota média da IES",
    y = "Participantes não proficientes",
    color = "Rede",
    size = "Participantes válidos"
  ) +
  theme_minimal()
# ----------------------------------------------------------------
# Seleção das métricas principais
# ----------------------------------------------------------------

# Avalia a redundância entre métricas de desempenho e dispersão.
# Média, P10, P50 e proporção de não proficientes apresentam
# forte correlação entre si, enquanto o desvio-padrão acrescenta
# uma dimensão distinta relacionada à heterogeneidade das notas.
matriz_correlacao <- painel_ies %>%
  select(media,
         p10,
         p50,
         prop_nao_proficientes,
         desvio_padrao,
         amplitude_interquartil) %>%
  cor()
round(matriz_correlacao, 3)

# Painel principal para comparação entre IES.
# Mantém:
# - desempenho geral: média;
# - cauda inferior: P10;
# - heterogeneidade: desvio-padrão;
# - proficiência: quantidade e proporção abaixo de 60;
# - contexto: tamanho, número de cursos e referência da própria rede.
painel_ies_reduzido <- painel_ies %>%
  select(
    CO_IES,
    NO_IES,
    SG_IES,
    rotulo_ies,
    rede,
    n_alunos,
    n_cursos,
    media,
    desvio_padrao,
    p10,
    n_nao_proficientes,
    prop_nao_proficientes,
    diferenca_rede
  )

referencia_ies <- painel_ies_reduzido %>%
  summarise(mediana_media = median(media),
            mediana_dp = median(desvio_padrao))



# painel_ies_reduzido %>%
#   ggplot(aes(x = media, y = desvio_padrao)) +
#   geom_point(aes(size = n_alunos, color = rede), alpha = .7) +
#   theme_minimal()

# ----------------------------------------------------------------
# Ranking das IES por proporção de participantes proficientes
# ----------------------------------------------------------------
ranking_proficiencia <- resumo_ies %>%
  arrange(desc(prop_proficientes), desc(n_alunos)) %>%
  mutate(
    rotulo_proficiencia = paste0(
      scales::percent(prop_proficientes, accuracy = .1),
      " (",
      n_proficientes,
      "/",
      n_alunos,
      ")"
    ),
    rotulo_nao_proficiencia = paste0(
      scales::percent(prop_nao_proficientes, accuracy = .1),
      " (",
      n_nao_proficientes,
      "/",
      n_alunos,
      ")"
    )
  )
# 20 maiores proporções de proficiência
ranking_proficiencia_top20 <- ranking_proficiencia %>%
  slice_head(n = 20)

# 20 menores proporções de proficiência
ranking_proficiencia_down20 <- ranking_proficiencia %>%
  slice_tail(n = 20)

ies_destaque <- bind_rows(
  painel_ies_reduzido %>% slice_max(media, n = 5),
  painel_ies_reduzido %>% slice_min(media, n = 5),
  painel_ies_reduzido %>% slice_max(desvio_padrao, n = 5),
  painel_ies_reduzido %>% slice_min(desvio_padrao, n = 5)
) %>%
  distinct(CO_IES, .keep_all = TRUE)

grafico_top_ies_proficiencia <- ranking_proficiencia_top20 %>%
  ggplot(aes(
    x = reorder(rotulo_ies, prop_proficientes),
    y = prop_proficientes,
    fill = rede
  )) +
  
  geom_col(width = .72) +
  
  geom_text(aes(label = rotulo_proficiencia),
            hjust = 1.05,
            size = 3) +
  
  scale_fill_rede() +
  
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, .25),
    limits = c(0, 1),
    expand = expansion(mult = c(0, .02))
  ) +
  
  coord_flip() +
  
  labs(
    title = "20 IES com maior proporção de participantes proficientes",
    subtitle = "ENAMED 2025 — corte de proficiência: nota ≥ 60",
    x = "Instituição de Ensino Superior",
    y = "Proporção de participantes proficientes",
    fill = "Rede",
    caption = "Rótulos: proficientes / participantes válidos."
  ) +
  
  tema_enamed()

grafico_bottom_ies_proficiencia <- ranking_proficiencia_down20 %>%
  ggplot(aes(
    x = reorder(rotulo_ies, prop_nao_proficientes),
    y = prop_nao_proficientes,
    fill = rede
  )) +
  
  geom_col(width = .72) +
  
  geom_text(aes(label = rotulo_nao_proficiencia),
            hjust = 1.05,
            size = 3) +
  
  scale_fill_rede() +
  
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, .25),
    limits = c(0, 1),
    expand = expansion(mult = c(0, .02))
  ) +
  
  coord_flip() +
  
  labs(
    title = "20 IES com maior proporção de participantes abaixo do corte",
    subtitle = "ENAMED 2025 — corte de proficiência: nota ≥ 60",
    x = "Instituição de Ensino Superior",
    y = "Proporção de participantes abaixo do corte",
    fill = "Rede",
    caption = "Rótulos: abaixo do corte / participantes válidos."
  ) +
  tema_enamed()

grafico_media_dispersao_ies <- painel_ies_reduzido %>%
  ggplot(aes(x = media, y = desvio_padrao)) +
  
  geom_point(aes(size = n_alunos, color = rede), alpha = .6) +
  
  geom_vline(
    xintercept = referencia_ies$mediana_media,
    color = cor_referencia,
    linetype = "dashed",
    linewidth = .7
  ) +
  
  geom_hline(
    yintercept = referencia_ies$mediana_dp,
    color = cor_referencia,
    linetype = "dashed",
    linewidth = .7
  ) +
  
  ggrepel::geom_text_repel(
    data = ies_destaque,
    aes(label = rotulo_ies),
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  
  scale_color_rede() +
  
  labs(
    title = "Desempenho e heterogeneidade das IES no ENAMED 2025",
    subtitle = "Linhas tracejadas representam as medianas entre as IES",
    x = "Nota média da IES",
    y = "Desvio-padrão das notas",
    color = "Rede",
    size = "Participantes válidos"
  ) +
  
  tema_enamed()



painel_ies_quadrantes <- painel_ies_reduzido %>%
  mutate(
    quadrante = case_when(
      media >= referencia_ies$mediana_media &
        desvio_padrao < referencia_ies$mediana_dp ~
        "Média acima / dispersão abaixo",
      
      media >= referencia_ies$mediana_media &
        desvio_padrao >= referencia_ies$mediana_dp ~
        "Média acima / dispersão acima",
      
      media < referencia_ies$mediana_media &
        desvio_padrao < referencia_ies$mediana_dp ~
        "Média abaixo / dispersão abaixo",
      
      media < referencia_ies$mediana_media &
        desvio_padrao >= referencia_ies$mediana_dp ~
        "Média abaixo / dispersão acima"
    )
  )

resumo_quadrantes_rede <- painel_ies_quadrantes %>%
  count(rede, quadrante) %>%
  group_by(rede) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

grafico_quadrantes_ies <- resumo_quadrantes_rede %>%
  ggplot(aes(x = rede, y = prop, fill = quadrante)) +
  
  geom_col(width = .72) +
  
  geom_text(aes(label = scales::percent(prop, accuracy = .1)),
            position = position_stack(vjust = .5),
            size = 3) +
  
  scale_fill_quadrante() +
  
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, .2),
    limits = c(0, 1),
    expand = expansion(mult = c(0, .01))
  ) +
  
  labs(
    title = "Distribuição das IES nos quadrantes de desempenho",
    subtitle = paste0(
      "Classificação relativa à mediana das médias e ",
      "dos desvios-padrão das IES"
    ),
    x = "Rede",
    y = "Proporção de IES",
    fill = "Quadrante"
  ) +
  
  tema_enamed()

# ----------------------------------------------------------------
# Ranking das IES por número de participantes não proficientes
# ----------------------------------------------------------------

ranking_n_nao_proficientes <- resumo_ies %>%
  arrange(desc(n_nao_proficientes), desc(prop_nao_proficientes)) %>%
  mutate(
    rotulo_nao_proficientes = paste0(
      n_nao_proficientes,
      "/",
      n_alunos,
      " (",
      scales::percent(prop_nao_proficientes, accuracy = .1),
      ")"
    )
  )

ranking_n_nao_proficientes_top20 <- ranking_n_nao_proficientes %>%
  slice_head(n = 20)

grafico_ies_nao_proficientes_absoluto <- ranking_n_nao_proficientes_top20 %>%
  ggplot(aes(
    x = reorder(rotulo_ies, n_nao_proficientes),
    y = n_nao_proficientes,
    fill = rede
  )) +
  
  geom_col(width = .72) +
  
  geom_text(aes(label = rotulo_nao_proficientes),
            hjust = 1.05,
            size = 3) +
  
  scale_fill_rede() +
  
  scale_y_continuous(expand = expansion(mult = c(0, .02))) +
  
  coord_flip() +
  
  labs(
    title = "20 IES com maior número de participantes abaixo do corte",
    subtitle = "ENAMED 2025 — corte de proficiência: nota ≥ 60",
    x = "Instituição de Ensino Superior",
    y = "Número de participantes abaixo do corte",
    fill = "Rede",
    caption = paste0(
      "Rótulos: participantes abaixo do corte / participantes válidos ",
      "(proporção abaixo do corte)."
    )
  ) +
  
  tema_enamed()

referencia_nao_proficiencia <- painel_ies_reduzido %>%
  summarise(
    mediana_prop_nao_proficientes = median(prop_nao_proficientes),
    mediana_n_nao_proficientes = median(n_nao_proficientes)
  )


ies_destaque_nao_proficiencia <- bind_rows(
  painel_ies_reduzido %>%
    slice_max(prop_nao_proficientes, n = 5),
  
  painel_ies_reduzido %>%
    slice_max(n_nao_proficientes, n = 5)
) %>%
  distinct(CO_IES, .keep_all = TRUE)

grafico_prop_nao_proficientes_tamanho <- painel_ies_reduzido %>%
  ggplot(aes(x = prop_nao_proficientes, y = n_nao_proficientes, color = rede)) +
  
  geom_point(alpha = .7, size = 2.2) +
  
  geom_vline(
    xintercept =
      referencia_nao_proficiencia$mediana_prop_nao_proficientes,
    color = cor_referencia,
    linetype = "dashed",
    linewidth = .7
  ) +
  
  geom_hline(
    yintercept =
      referencia_nao_proficiencia$mediana_n_nao_proficientes,
    color = cor_referencia,
    linetype = "dashed",
    linewidth = .7
  ) +
  
  ggrepel::geom_text_repel(
    data = ies_destaque_nao_proficiencia,
    aes(label = rotulo_ies),
    show.legend = FALSE,
    min.segment.length = 0
  ) +
  
  scale_color_rede() +
  
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, .1),
    limits = c(0, 1)
  ) +
  
  labs(
    title = "Frequência e número de concluintes abaixo do corte de proficiência",
    subtitle = "ENAMED 2025 — linhas tracejadas representam as medianas entre as IES",
    x = "Proporção de concluintes abaixo do corte de proficiência",
    y = "Número de concluintes abaixo do corte de proficiência",
    color = "Rede"
  ) +
  
  tema_enamed()

painel_ies_volume <- painel_ies_reduzido %>%
  mutate(
    grupo_volume = case_when(
      prop_nao_proficientes <
        referencia_nao_proficiencia$mediana_prop_nao_proficientes &
        n_nao_proficientes <
        referencia_nao_proficiencia$mediana_n_nao_proficientes ~
        "Proporção abaixo / número abaixo",
      
      prop_nao_proficientes <
        referencia_nao_proficiencia$mediana_prop_nao_proficientes &
        n_nao_proficientes >=
        referencia_nao_proficiencia$mediana_n_nao_proficientes ~
        "Proporção abaixo / número acima",
      
      prop_nao_proficientes >=
        referencia_nao_proficiencia$mediana_prop_nao_proficientes &
        n_nao_proficientes <
        referencia_nao_proficiencia$mediana_n_nao_proficientes ~
        "Proporção acima / número abaixo",
      
      prop_nao_proficientes >=
        referencia_nao_proficiencia$mediana_prop_nao_proficientes &
        n_nao_proficientes >=
        referencia_nao_proficiencia$mediana_n_nao_proficientes ~
        "Proporção acima / número acima"
    )
  )

resumo_volume_nao_proficiencia <- painel_ies_volume %>%
  group_by(grupo_volume) %>%
  summarise(
    n_ies = n(),
    n_participantes = sum(n_alunos),
    n_nao_proficientes = sum(n_nao_proficientes),
    .groups = "drop"
  ) %>%
  mutate(
    prop_ies = n_ies / sum(n_ies),
    prop_nao_proficientes =
      n_nao_proficientes / sum(n_nao_proficientes),
    taxa_nao_proficiencia =
      n_nao_proficientes / n_participantes
  )

comparacao_concentracao <- resumo_volume_nao_proficiencia %>%
  select(grupo_volume, prop_ies, prop_nao_proficientes) %>%
  pivot_longer(
    cols = c(prop_ies, prop_nao_proficientes),
    names_to = "metrica",
    values_to = "proporcao"
  ) %>%
  mutate(
    # Nomes simplificados para exibição no gráfico
    grupo_rotulo = case_when(
      grupo_volume == "Proporção acima / número acima" ~
        "Alta taxa e alto número abaixo do corte",
      
      grupo_volume == "Proporção acima / número abaixo" ~
        "Alta taxa e baixo número abaixo do corte",
      
      grupo_volume == "Proporção abaixo / número acima" ~
        "Baixa taxa e alto número abaixo do corte",
      
      grupo_volume == "Proporção abaixo / número abaixo" ~
        "Baixa taxa e baixo número abaixo do corte"
    ),
    
    metrica = case_when(
      metrica == "prop_ies" ~
        "IES",
      
      metrica == "prop_nao_proficientes" ~
        "Concluintes abaixo do corte"
    )
  )


grafico_concentracao_nao_proficientes <- comparacao_concentracao %>%
  ggplot(aes(x = grupo_rotulo, y = proporcao, fill = metrica)) +
  
  geom_col(position = position_dodge(width = .9), width = .78) +
  
  geom_text(
    aes(label = scales::percent(proporcao, accuracy = .1)),
    position = position_dodge(width = .9),
    hjust = -.1,
    size = 3
  ) +
  
  scale_fill_concentracao() +
  
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, .8, .2),
    limits = c(0, .8),
    expand = expansion(mult = c(0, .02))
  ) +
  
  coord_flip() +
  
  labs(
    title = "Onde se concentram os concluintes abaixo do corte de proficiência?",
    subtitle = "Participação das IES e dos concluintes abaixo do corte em cada grupo",
    x = NULL,
    y = "Participação no total",
    fill = NULL,
    caption = paste0(
      "Alta taxa = ≥ 28,6% abaixo do corte; ",
      "alto número = ≥ 30 concluintes abaixo do corte."
    )
  ) +
  
  tema_enamed()
# ----------------------------------------------------------------
# Concentração dos concluintes abaixo do corte por rede
# ----------------------------------------------------------------

# Analisa a composição do grupo de IES que está simultaneamente
# acima da mediana na proporção e no número absoluto de concluintes
# abaixo do corte de proficiência.

concentracao_rede <- painel_ies_volume %>%
  filter(grupo_volume == "Proporção acima / número acima") %>%
  group_by(rede) %>%
  summarise(
    n_ies = n(),
    n_participantes = sum(n_alunos),
    n_nao_proficientes = sum(n_nao_proficientes),
    .groups = "drop"
  ) %>%
  mutate(
    prop_ies_grupo = n_ies / sum(n_ies),
    prop_nao_proficientes_grupo =
      n_nao_proficientes / sum(n_nao_proficientes)
  )

resumo_concentracao_rede <- painel_ies_volume %>%
  group_by(rede) %>%
  summarise(
    n_ies_total = n(),
    
    n_ies_grupo = sum(grupo_volume == "Proporção acima / número acima"),
    
    n_nao_proficientes_total =
      sum(n_nao_proficientes),
    
    n_nao_proficientes_grupo =
      sum(
        if_else(
          grupo_volume == "Proporção acima / número acima",
          n_nao_proficientes,
          0L
        )
      ),
    
    .groups = "drop"
  ) %>%
  mutate(
    prop_ies_no_grupo =
      n_ies_grupo / n_ies_total,
    
    prop_nao_proficientes_no_grupo =
      n_nao_proficientes_grupo /
      n_nao_proficientes_total
  )

comparacao_concentracao_rede <- resumo_concentracao_rede %>%
  select(rede, prop_ies_no_grupo, prop_nao_proficientes_no_grupo) %>%
  pivot_longer(
    cols = c(prop_ies_no_grupo, prop_nao_proficientes_no_grupo),
    names_to = "metrica",
    values_to = "proporcao"
  ) %>%
  mutate(
    metrica = case_when(
      metrica == "prop_ies_no_grupo" ~
        "IES no grupo",
      
      metrica == "prop_nao_proficientes_no_grupo" ~
        "Concluintes abaixo do corte"
    )
  )

grafico_concentracao_rede <- comparacao_concentracao_rede %>%
  ggplot(aes(x = rede, y = proporcao, fill = metrica)) +
  
  geom_col(position = position_dodge(width = .9), width = .72) +
  
  geom_text(
    aes(label = scales::percent(proporcao, accuracy = .1)),
    position = position_dodge(width = .9),
    vjust = -.3,
    size = 3
  ) +
  
  scale_fill_concentracao() +
  
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, .2),
    limits = c(0, 1),
    expand = expansion(mult = c(0, .03))
  ) +
  
  labs(
    title = "Concentração dos concluintes abaixo do corte dentro de cada rede",
    subtitle = "IES com alta taxa e alto número de concluintes abaixo do corte",
    x = "Rede",
    y = "Participação dentro da rede",
    fill = NULL,
    caption = paste0(
      "Alta taxa = ≥ 28,6% abaixo do corte; ",
      "alto número = ≥ 30 concluintes abaixo do corte."
    )
  ) +
  
  tema_enamed()
# ----------------------------------------------------------------
# Concentração acumulada dos concluintes abaixo do corte
# ----------------------------------------------------------------

concentracao_acumulada <- resumo_ies %>%
  arrange(desc(n_nao_proficientes)) %>%
  mutate(
    ordem = row_number(),
    prop_ies_acumulada = ordem / n(),
    n_nao_proficientes_acumulado =
      cumsum(n_nao_proficientes),
    prop_nao_proficientes_acumulada =
      n_nao_proficientes_acumulado /
      sum(n_nao_proficientes)
  )
pontos_concentracao <- tibble(alvo = c(.10, .20, .30, .50)) %>%
  mutate(ordem = ceiling(alvo * nrow(concentracao_acumulada))) %>%
  left_join(
    concentracao_acumulada %>%
      select(
        ordem,
        prop_ies_acumulada,
        n_nao_proficientes_acumulado,
        prop_nao_proficientes_acumulada
      ),
    by = "ordem"
  )

# ----------------------------------------------------------------
# Comparação das distribuições de notas entre IES e uma rede
# ----------------------------------------------------------------

siglas_ies_escolhidas <- c("FSM", "UFF", "UERJ")

# Preencher somente quando alguma sigla for ambígua.
# Caso contrário, deixar c().
codigos_desambiguacao <- c(UNESC = 1559)

rede_referencia <- "Pública"


# Cadastro das IES disponíveis para seleção

cadastro_selecao_ies <- enamed_ies %>%
  distinct(CO_IES, NO_IES, SG_IES, rede)


# Valida a rede de referência

if (!rede_referencia %in% unique(enamed_ies$rede)) {
  stop(paste0("ERRO: rede de referência não encontrada: ", rede_referencia))
}


# Verifica se todas as siglas informadas existem

siglas_nao_encontradas <- setdiff(siglas_ies_escolhidas, cadastro_selecao_ies$SG_IES)

if (length(siglas_nao_encontradas) > 0) {
  stop(paste0(
    "ERRO: sigla(s) não encontrada(s): ",
    paste(siglas_nao_encontradas, collapse = ", ")
  ))
}


# Localiza todas as IES correspondentes às siglas escolhidas

ies_candidatas <- cadastro_selecao_ies %>%
  filter(SG_IES %in% siglas_ies_escolhidas)


# Identifica siglas que pertencem a mais de uma IES

siglas_ambiguas <- ies_candidatas %>%
  count(SG_IES) %>%
  filter(n > 1) %>%
  pull(SG_IES)


# Verifica se foi informado CO_IES para todas as siglas ambíguas

siglas_sem_codigo <- setdiff(siglas_ambiguas, names(codigos_desambiguacao))

if (length(siglas_sem_codigo) > 0) {
  stop(
    paste0(
      "ERRO: a(s) sigla(s) ",
      paste(siglas_sem_codigo, collapse = ", "),
      " identifica(m) mais de uma IES. ",
      "Informe o CO_IES em codigos_desambiguacao."
    )
  )
}


# Aplica o CO_IES somente às siglas ambíguas

if (length(siglas_ambiguas) > 0) {
  ies_escolhidas <- ies_candidatas %>%
    filter(!SG_IES %in% siglas_ambiguas |
             CO_IES ==
             unname(codigos_desambiguacao[SG_IES]))
  
} else {
  ies_escolhidas <- ies_candidatas
}


# Verifica se os códigos informados são válidos

siglas_codigo_invalido <- setdiff(siglas_ambiguas, ies_escolhidas$SG_IES)

if (length(siglas_codigo_invalido) > 0) {
  stop(paste0(
    "ERRO: CO_IES inválido para a(s) sigla(s): ",
    paste(siglas_codigo_invalido, collapse = ", ")
  ))
}


# Cria os nomes que aparecerão no gráfico

ies_escolhidas <- ies_escolhidas %>%
  mutate(grupo = if_else(
    SG_IES %in% siglas_ambiguas,
    paste0(SG_IES, " (", CO_IES, ")"),
    SG_IES
  ))


# Códigos das IES escolhidas que pertencem à rede de referência

codigos_excluir_referencia <- ies_escolhidas %>%
  filter(rede == rede_referencia) %>%
  pull(CO_IES)


# Define o nome da curva da rede de referência

if (length(codigos_excluir_referencia) > 0) {
  rotulo_referencia <- paste0("Demais IES da rede ", rede_referencia)
  
} else {
  rotulo_referencia <- paste0("IES da rede ", rede_referencia)
}


# Dados das IES escolhidas

dados_ies_escolhidas <- enamed_ies %>%
  inner_join(ies_escolhidas %>%
               select(CO_IES, grupo),
             by = "CO_IES",
             relationship = "many-to-one") %>%
  transmute(NT_GER, grupo)


# Dados da rede de referência

dados_rede_referencia <- enamed_ies %>%
  filter(rede == rede_referencia,
         !CO_IES %in% codigos_excluir_referencia) %>%
  transmute(NT_GER, grupo = rotulo_referencia)


# Junta as IES selecionadas com a rede de referência

dados_densidade_comparacao <- bind_rows(dados_ies_escolhidas, dados_rede_referencia)


pontos_corte_densidade <- dados_densidade_comparacao %>%
  group_by(grupo) %>%
  summarise(prop_proficientes = mean(NT_GER >= 60),
            .groups = "drop")

grupos_ies <- ies_escolhidas$grupo

cores_ies_comparacao <- grDevices::hcl.colors(n = length(grupos_ies), palette = "Dark 3")

names(cores_ies_comparacao) <- grupos_ies

cores_comparacao <- c(cores_ies_comparacao,
                      setNames(cor_referencia, rotulo_referencia))

# As curvas de densidade são normalizadas para área total igual a 1.
# A altura representa concentração relativa das notas,
# e não o número absoluto de participantes.

grafico_cdf_complementar_ies <- dados_densidade_comparacao %>%
  ggplot(aes(x = NT_GER, color = grupo)) +
  
  stat_ecdf(aes(y = after_stat(1 - y)), linewidth = 1) +
  
  geom_vline(
    xintercept = 60,
    color = cor_referencia,
    linetype = "dashed",
    linewidth = .7
  ) +
  
  geom_point(
    data = pontos_corte_densidade,
    aes(x = 60, y = prop_proficientes, color = grupo),
    size = 2.8,
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = cores_comparacao) +
  
  scale_x_continuous(breaks = seq(20, 100, 10)) +
  
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, .10),
    limits = c(0, 1)
  ) +
  
  labs(
    title = "Proporção de concluintes acima de cada nota",
    subtitle = paste0("Comparação com a rede ", rede_referencia, " — ENAMED 2025"),
    x = "Nota geral",
    y = "Proporção de concluintes acima da nota",
    color = NULL,
    caption = "Linha tracejada: corte de proficiência = 60."
  ) +
  
  tema_enamed()
# As curvas de densidade são normalizadas para área total igual a 1.
# A altura representa concentração relativa das notas,
# e não o número absoluto de participantes.

grafico_densidade_ies <- dados_densidade_comparacao %>%
  ggplot(aes(x = NT_GER, color = grupo, fill = grupo)) +
  
  geom_density(alpha = .12, linewidth = 1) +
  
  geom_vline(
    xintercept = 60,
    color = cor_referencia,
    linetype = "dashed",
    linewidth = .7
  ) +
  
  scale_color_manual(values = cores_comparacao) +
  
  scale_fill_manual(values = cores_comparacao) +
  
  scale_x_continuous(breaks = seq(20, 100, 10)) +
  
  labs(
    title = "Distribuição das notas das IES selecionadas",
    subtitle = paste0("Comparação com a rede ", rede_referencia, " — ENAMED 2025"),
    x = "Nota geral",
    y = "Densidade",
    color = NULL,
    fill = NULL,
    caption = paste0(
      "Curvas de densidade normalizadas para área total igual a 1. ",
      "Linha tracejada: corte de proficiência = 60."
    )
  ) +
  
  tema_enamed()