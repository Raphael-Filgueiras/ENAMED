










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
  summarise(
    mediana_media = median(media),
    mediana_dp = median(desvio_padrao)
  )



painel_ies_reduzido %>%
  ggplot(aes(x = media, y = desvio_padrao)) +
  geom_point(aes(size = n_alunos, color = rede), alpha = .7) +
  theme_minimal()

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
  ggplot(aes(x=media,y=desvio_padrao))+
  geom_point(aes(size = n_alunos,color=rede),alpha=.7)+
  geom_vline(xintercept = referencia_ies$mediana_media,
             linetype="dashed")+
  geom_hline(yintercept = referencia_ies$mediana_dp,
             linetype="dashed")+
  labs(
    title = "Desempenho e heterogeneidade das IES no ENAMED 2025",
    subtitle = "Linhas tracejadas representam as medianas entre as IES",
    x = "Nota média da IES",
    y = "Desvio-padrão das notas",
    color = "Rede",
    size = "Participantes válidos"
  ) +
  theme_minimal()



