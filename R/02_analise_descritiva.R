library(tidyverse)
source("R/00_estilo_grafico.R")
enamed <- read_rds("dados/processados/enamed.rds")
# Resumo de desempenho por rede administrativa
resumo_rede <- enamed %>%
  group_by(rede) %>%
  summarise(
    n = n(),
    media = mean(NT_GER),
    mediana = median(NT_GER),
    q1 = quantile(NT_GER, .25),
    q3 = quantile(NT_GER, .75),
    desvio_padrao = sd(NT_GER),
    minimo = min(NT_GER),
    maximo =  max(NT_GER),
    .groups = "drop"
  )

# Estatísticas com cursos agrupados por ESTADO e Rede
resumo_estado_rede <- enamed %>%
  group_by(UF, rede) %>%
  summarise(
    n = n(),
    media = mean(NT_GER),
    mediana = median(NT_GER),
    q1 = quantile(NT_GER, .25),
    q3 = quantile(NT_GER, .75),
    desvio_padrao = sd(NT_GER),
    minimo = min(NT_GER),
    maximo =  max(NT_GER),
    .groups = "drop"
  )
# Comparação entre as médias das redes pública e privada por UF
comparacao_estado_rede <- resumo_estado_rede %>%
  filter(rede %in% c("Pública", "Privada")) %>%
  select(UF, rede, media) %>%
  pivot_wider(names_from = rede, values_from = media) %>%
  mutate(diferenca = Pública - Privada) %>%
  drop_na(diferenca) %>%
  arrange(desc(diferenca))
#Contagem de alunos na rede pública , ou privada
contagem_estado_rede <- resumo_estado_rede %>%
  filter(rede %in% c("Pública", "Privada")) %>%
  select(UF, rede, n) %>%
  pivot_wider(names_from = rede,
              values_from = n,
              names_prefix = "n_")
#Comparando quem tem o menor número de alunos entre publica e privada
comparacao_estado_rede <- comparacao_estado_rede %>%
  left_join(contagem_estado_rede, by = "UF", relationship = "one-to-one") %>%
  mutate(n_menor_rede = pmin(n_Pública, n_Privada))

# Desvio-padrão das notas em cada rede por estado
desvio_estado_rede <- resumo_estado_rede %>%
  filter(rede %in% c("Pública", "Privada")) %>%
  select(UF, rede, desvio_padrao) %>%
  pivot_wider(names_from = rede,
              values_from = desvio_padrao,
              names_prefix = "d_")
comparacao_estado_rede <- comparacao_estado_rede %>%
  left_join(desvio_estado_rede, by = "UF", relationship = "one-to-one")


#Gáfico de barras comparando a difernça entre as médias das Pulbicas e privadas.
comparacao_estado_rede %>%
  ggplot(aes(
    x = reorder(UF, diferenca),
    y = diferenca,
    fill = if_else(diferenca >= 0, "Pública", "Privada")
  )) +
  
  geom_col(width = .72) +
  
  geom_hline(
    yintercept = 0,
    color = cor_referencia,
    linetype = "dashed",
    linewidth = .7
  ) +
  
  geom_text(aes(
    label = round(diferenca, 1),
    hjust = if_else(diferenca >= 0, -.15, 1.15)
  ), size = 3) +
  
  coord_flip() +
  
  scale_fill_rede() +
  
  scale_y_continuous(expand = expansion(mult = c(.12, .12))) +
  
  labs(
    title = "Diferença da nota média do ENAMED 2025",
    subtitle = "Rede pública menos rede privada, por UF",
    x = "UF",
    y = "Diferença na nota média",
    fill = "Rede com maior\nnota média",
    caption = paste0(
      "Valores positivos indicam média maior na rede pública; ",
      "valores negativos indicam média maior na rede privada."
    )
  ) +
  
  tema_enamed()
#Cursos agrupados por código de Curso , instiuição de ensino , e Estado .
resumo_curso <- enamed %>%
  group_by(CO_CURSO, CO_IES, UF, rede) %>%
  summarise(
    n_alunos = n(),
    media_curso = mean(NT_GER),
    mediana_curso = median(NT_GER),
    desvio_curso = sd(NT_GER),
    .groups = "drop"
  )
# Distribuição do número de participantes válidos por curso e por rede
tamanho_curso_rede <- resumo_curso %>%
  group_by(rede) %>%
  summarise(
    n_cursos = n(),
    media_alunos = mean(n_alunos),
    mediana_alunos = median(n_alunos),
    q1 = quantile(n_alunos, 0.25),
    q3 = quantile(n_alunos, 0.75),
    minimo_alunos = min(n_alunos),
    maximo_alunos = max(n_alunos),
    .groups = "drop"
  )

# Desempenho das redes dando o mesmo peso a cada curso
resumo_rede_curso <- resumo_curso %>%
  group_by(rede) %>%
  summarise(
    n_cursos = n(),
    media = mean(media_curso),
    mediana = median(mediana_curso),
    desvio_padrão = sd(media_curso),
    .groups = "drop"
  )

# Comparação das médias por aluno e por curso
comparacao_nivel <- resumo_rede %>%
  select(rede, media_aluno = media, mediana_aluno = mediana) %>%
  left_join(
    resumo_rede_curso %>%
      select(rede, media_curso = media, mediana_curso = mediana),
    by = "rede",
    relationship = "one-to-one"
  ) %>%
  mutate(diferenca_ponderacao = media_curso - media_aluno)


resumo_ies <- enamed %>%
  group_by(CO_IES, rede) %>%
  summarise(
    n_alunos = n(),
    n_cursos = n_distinct(CO_CURSO),
    media_ies = mean(NT_GER),
    mediana_ies = median(NT_GER),
    desvio_ies = sd(NT_GER),
    .groups = "drop"
  )
# Variação das médias entre cursos pertencentes à mesma IES
variacao_cursos_ies <- resumo_curso %>%
  group_by(CO_IES, rede) %>%
  filter(n() > 1) %>%
  summarise(
    n_cursos = n(),
    menor_media = min(media_curso),
    maior_media = max(media_curso),
    amplitude = maior_media - menor_media,
    desvio_entre_cursos = sd(media_curso),
    .groups = "drop"
  ) %>%
  arrange(desc(amplitude))

# Desempenho da IES ponderado pelo número de participantes
resumo_ies_aluno <- enamed %>%
  group_by(CO_IES, rede) %>%
  summarise(
    n_alunos = n(),
    n_cursos = n_distinct(CO_CURSO),
    media_ies_aluno = mean(NT_GER),
    .groups = "drop"
  )

# Desempenho da IES dando o mesmo peso a cada curso
resumo_ies_curso <- resumo_curso %>%
  group_by(CO_IES, rede) %>%
  summarise(
    n_cursos = n(),
    media_ies_curso = mean(media_curso),
    .groups = "drop"
  )

comparacao_ponderacao_ies <- resumo_ies_aluno %>%
  left_join(
    resumo_ies_curso,
    by = c("CO_IES", "rede", "n_cursos"),
    relationship = "one-to-one"
  ) %>%
  mutate(diferenca_ponderacao = media_ies_curso - media_ies_aluno)

# Desempenho das redes dando o mesmo peso a cada IES
resumo_rede_ies <- resumo_ies_aluno %>%
  group_by(rede) %>%
  summarise(
    n_ies = n(),
    media = mean(media_ies_aluno),
    mediana = median(media_ies_aluno),
    desvio_padrao = sd(media_ies_aluno),
    .groups = "drop"
  )


#comparação entre tres níveis de granularidade na ponderação:Aluno,Curso,IES
comparacao_tres_niveis <- bind_rows(
  resumo_rede %>%
    select(rede, media, mediana) %>%
    mutate(nivel = "Aluno"),
  
  resumo_rede_curso %>%
    select(rede, media, mediana) %>%
    mutate(nivel = "Curso"),
  
  resumo_rede_ies %>%
    select(rede, media, mediana) %>%
    mutate(nivel = "IES")
)
#comparação entre tres níveis de granularidade na ponderação:Aluno,Curso,IES
#filtrando somente públicas e privadas
comparacao_tres_niveis <- comparacao_tres_niveis %>%
  filter(rede %in% c("Pública", "Privada")) %>%
  select(nivel, rede, media, mediana) %>%
  pivot_wider(names_from = rede,
              values_from = c(media, mediana)) %>%
  mutate(
    diferenca_media = media_Pública - media_Privada,
    diferenca_mediana = mediana_Pública - mediana_Privada
  )

# Desempenho de cada IES dentro de cada UF
resumo_ies_uf <- enamed %>%
  group_by(CO_IES, UF, rede) %>%
  summarise(
    n_alunos = n(),
    n_cursos = n_distinct(CO_CURSO),
    media_ies = mean(NT_GER),
    mediana_ies = median(NT_GER),
    .groups = "drop"
  )



# Desempenho por estado e rede dando peso igual a cada IES
resumo_estado_rede_ies <- resumo_ies_uf %>%
  group_by(UF, rede) %>%
  summarise(
    n_ies = n(),
    media = mean(media_ies),
    mediana = median(media_ies),
    desvio_padrao = sd(media_ies),
    .groups = "drop"
  )

comparacao_estado_rede_ies <- resumo_estado_rede_ies %>%
  filter(rede %in% c("Pública", "Privada")) %>%
  select(UF, rede, media, n_ies) %>%
  pivot_wider(names_from = rede, values_from = c(media, n_ies)) %>%
  mutate(diferenca = media_Pública - media_Privada) %>%
  drop_na(diferenca) %>%
  arrange(desc(diferenca))


# Comparação da diferença pública-privada por aluno e por IES
comparacao_ponderacao_estado <- comparacao_estado_rede %>%
  select(UF, diferenca_aluno = diferenca) %>%
  inner_join(
    comparacao_estado_rede_ies %>%
      select(UF, diferenca_ies = diferenca, n_ies_Pública, n_ies_Privada),
    by = "UF",
    relationship = "one-to-one"
  ) %>%
  mutate(mudanca_ponderacao = diferenca_ies - diferenca_aluno)