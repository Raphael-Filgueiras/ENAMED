
# ================================================================
# 05 - ANÁLISE POR INSTITUIÇÃO DE ENSINO SUPERIOR
# ================================================================
library(tidyverse)

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

ranking_proficiencia_top20 %>%
  ggplot(aes(
    x = reorder(SG_IES, prop_proficientes),
    y = prop_proficientes,
    fill = rede
  )) +
  geom_col() +
  geom_text(aes(label = rotulo_proficiencia),
            hjust = 1.05,
            size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "20 IES com maior proporção de participantes proficientes",
    subtitle = "ENAMED 2025 — corte de proficiência: nota ≥ 60",
    x = "Instituição de Ensino Superior",
    y = "Proporção de participantes proficientes",
    fill = "Rede",
    caption = "Entre parênteses: proficientes / participantes válidos."
  ) +
  theme_minimal()

ranking_proficiencia_down20 %>%
  ggplot(aes(
    x = reorder(SG_IES, prop_nao_proficientes),
    y = prop_nao_proficientes,
    fill = rede
  )) +
  geom_col() +
  geom_text(aes(label = rotulo_nao_proficiencia),
            hjust = 1.05,
            size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "20 IES com maior proporção de participantes não proficientes",
    subtitle = "ENAMED 2025 — corte de proficiência: nota ≥ 60",
    x = "Instituição de Ensino Superior",
    y = "Proporção de participantes não proficientes",
    fill = "Rede",
    caption = "Entre parênteses: não proficientes / participantes válidos."
  ) +
  theme_minimal()

painel_ies_reduzido %>%
  ggplot(aes(x = media, y = desvio_padrao)) +
  geom_point(aes(size = n_alunos, color = rede), alpha = .7) +
  geom_vline(xintercept = referencia_ies$mediana_media, linetype = "dashed") +
  geom_hline(yintercept = referencia_ies$mediana_dp, linetype = "dashed") +
  ggrepel::geom_text_repel(data = ies_destaque,
                           aes(label = SG_IES),
                           min.segment.length = 0) +
  
  labs(
    title = "Desempenho e heterogeneidade das IES no ENAMED 2025",
    subtitle = "Linhas tracejadas representam as medianas entre as IES",
    x = "Nota média da IES",
    y = "Desvio-padrão das notas",
    color = "Rede",
    size = "Participantes válidos"
  ) +
  theme_minimal()




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

resumo_quadrantes_rede %>%
  ggplot(aes(x = rede, y = prop, fill = quadrante)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop, accuracy = .1)),
            position = position_stack(vjust = .5),
            size = 3) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "Distribuição das IES nos quadrantes de desempenho",
    subtitle = "Classificação relativa à mediana das médias e dos desvios-padrão das IES",
    x = "Rede",
    y = "Proporção de IES",
    fill = "Quadrante"
  ) +
  theme_minimal()


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

ranking_n_nao_proficientes_top20 %>%
  ggplot(aes(
    x = reorder(SG_IES, n_nao_proficientes),
    y = n_nao_proficientes,
    fill = rede
  )) +
  geom_col() +
  geom_text(aes(label = rotulo_nao_proficientes),
            hjust = 1.05,
            size = 3) +
  coord_flip() +
  labs(
    title = "20 IES com maior número de participantes não proficientes",
    subtitle = "ENAMED 2025 — corte de proficiência: nota ≥ 60",
    x = "Instituição de Ensino Superior",
    y = "Número de participantes não proficientes",
    caption = "Rótulos: não proficientes / participantes válidos (proporção de não proficientes)."
  )

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

painel_ies_reduzido %>%
  ggplot(aes(x = prop_nao_proficientes, y = n_nao_proficientes, color = rede)) +
  geom_point(alpha = .7) +
  geom_vline(xintercept =
               referencia_nao_proficiencia$mediana_prop_nao_proficientes,
             linetype = "dashed") +
  geom_hline(yintercept =
               referencia_nao_proficiencia$mediana_n_nao_proficientes,
             linetype = "dashed") +
  ggrepel::geom_text_repel(data = ies_destaque_nao_proficiencia, aes(label = SG_IES), show.legend = FALSE) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "Frequência e número de concluintes abaixo do corte de proficiência",
    subtitle = "ENAMED 2025 — linhas tracejadas representam as medianas entre as IES",
    x = "Proporção de concluintes abaixo do corte de proficiência",
    y = "Número de concluintes abaixo do corte de proficiência",
    color = "Rede"
  ) +
  theme_minimal()

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


comparacao_concentracao %>%
  ggplot(aes(x = grupo_rotulo, y = proporcao, fill = metrica)) +
  geom_col(position = position_dodge(width = .9)) +
  geom_text(
    aes(label = scales::percent(proporcao, accuracy = .1)),
    position = position_dodge(width = .9),
    hjust = -.1,
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, .8, .2),
    limits = c(0, .8)
  ) +
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
  theme_minimal()
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

comparacao_concentracao_rede %>%
  ggplot(aes(x = rede, y = proporcao, fill = metrica)) +
  geom_col(position = position_dodge(width = .9)) +
  geom_text(
    aes(label = scales::percent(proporcao, accuracy = .1)),
    position = position_dodge(width = .9),
    vjust = -.3,
    size = 3
  ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0, 1)) +
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
  theme_minimal()

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
