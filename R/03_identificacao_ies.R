library(tidyverse)

url_censo_2024 <- "https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_2024.zip"

arquivo_censo_2024 <- "dados/brutos/microdados_censo_superior_2024.zip"

if (!file.exists(arquivo_censo_2024)) {
  download.file(
    url_censo_2024,
    destfile = arquivo_censo_2024,
    
    mode = "wb",
    method = "wininet"
  )
  
}

conteudo_censo <- unzip(arquivo_censo_2024, list = TRUE)

arquivo_ies_censo <- conteudo_censo %>%
  filter(str_detect(Name, "MICRODADOS_ED_SUP_IES_2024\\.CSV$"))

caminho_ies_censo <- unzip(
  arquivo_censo_2024,
  files = arquivo_ies_censo$Name,
  exdir = tempdir(),
  junkpaths = TRUE
)

ies_censo <- read_delim(
  caminho_ies_censo,
  delim = ";",
  locale = locale(encoding = "ISO-8859-1"),
  show_col_types = FALSE
)


cadastro_ies <- ies_censo %>%
  select(
    CO_IES,
    NO_IES,
    SG_IES,
    SG_UF_IES,
    NO_MUNICIPIO_IES,
    TP_ORGANIZACAO_ACADEMICA,
    TP_REDE,
    TP_CATEGORIA_ADMINISTRATIVA
  )

enamed <- readRDS("dados/processados/enamed.rds")

ies_enamed <- enamed %>%  distinct(CO_IES)


ies_enamed %>% anti_join(cadastro_ies, by = "CO_IES")

ies_enamed_identificadas <- ies_enamed %>%
  left_join(cadastro_ies, by = "CO_IES", relationship = "one-to-one")


saveRDS(ies_enamed_identificadas,file = "dados/processados/ies_enamed_identificadas.rds")