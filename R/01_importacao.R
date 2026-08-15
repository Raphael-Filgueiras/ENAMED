library(tidyverse)


# Script para importacao
#caminho para o arquivo compactado do ENAMED
arquivo_zip <- "dados/brutos/microdados_enamed_2025.zip"
#verifica se o arquivo existe
file.exists(arquivo_zip)
#lista o conteúdo do arquivo ZIP sem descompactar
conteudo_zip <- unzip(arquivo_zip, list = TRUE)
#Mostra os primeiros itens do ZIP
head(conteudo_zip)

#Filtrando arquivos que contem a palavra Enade

arquivos_enade <- conteudo_zip[grepl("/Enade/", conteudo_zip$Name), ]

#Extraindo somente os arquivos .txt

arquivos_txt <- arquivos_enade[grepl("\\.txt$", arquivos_enade$Name), ]
#Atribui a variável arquivos_txt uma versão modificada na ordem sequencial crescente.

arquivos_txt <- arquivos_txt %>%
  mutate(num_arquivo = parse_number(str_extract(Name, "arq\\d+"))) %>%
  arrange(num_arquivo)
#Localiza o dicionário de variáveis dentro do ZIP, e remove arquivos temporários

arquivo_dicionario <- conteudo_zip %>%
  filter(
    str_detect(
      Name,
      ".*Dicionário_arquivos_variáveis_microdados_Enamed_2025\\.xlsx$"
    ),!str_detect(basename(Name), "^~\\$")
  )
#Extraindo somente xls dos arquivos zipados.

caminho_dicionario <- unzip(
  arquivo_zip,
  files = arquivo_dicionario$Name,
  exdir = tempdir(),
  junkpaths = TRUE
)

#Lê a aba que teoricamente descreve os arquivos do ENADE

descricao_arquivos <- readxl::read_excel(caminho_dicionario, sheet = "ARQUIVOS - ENADE")

#Filtrando somente os arquivos necessários para análise de notas vs publicas e privadas.

arquivos_interesse <- descricao_arquivos %>%
  filter(
    `Nome do arquivo` %in% c("microdados_enade_2025_arq1", "microdados_enade_2025_arq3")
  )

# Lendo dicionário de variáveis do ENAMED e #Preenchendo NA's importados
# com os últimos valores das variáveis.
dicionario_variaveis <- readxl::read_excel(caminho_dicionario, sheet = "DICIONÁRIO DE VARIÁVEIS - ENADE", skip = 1) %>%
  fill(NOME, .direction = "down")
# Seleciona os arquivos arq1 e arq3
arquivos_microdados <- arquivos_txt %>%
  filter(num_arquivo %in% c(1, 3))

# Extrai arq1 e arq3 para a pasta temporária da sessão

caminhos_microdados <- unzip(
  arquivo_zip,
  files = arquivos_microdados$Name,
  exdir = tempdir(),
  junkpaths = TRUE
)

# Define o caminho de cada arquivo
caminho_arq1 <- caminhos_microdados[1]
caminho_arq3 <- caminhos_microdados[2]
# Importa os dados de caracterização dos cursos e das IES
arq1 <- readr::read_delim(caminho_arq1, delim = ";", show_col_types = FALSE)
# Importa os dados de desempenho dos participantes
arq3 <- readr::read_delim(
  caminho_arq3,
  delim = ";",
  na = c(".", ""),
  col_types = cols(
    DS_VT_GAB_OBJ = col_character(),
    DS_VT_ESC_OBJ = col_character(),
    DS_VT_ACE_OBJ = col_character(),
    .default = col_guess()
  ),
  show_col_types = FALSE
)
# Identifica participantes classificados como presentes, mas sem nota
casos_inconsistentes <- arq3 %>%
  filter(TP_PRES == 555, is.na(NT_GER))
# Remove os registros inconsistentes, mantendo ausentes e eliminados
arq3_limpo <- arq3 %>%
  filter(!(TP_PRES == 555 & is.na(NT_GER)))

# Mantendo somente os presentes e com nota válida.

arq3_validos <- arq3_limpo %>%
  filter(TP_PRES == 555)

curso_info <- arq1 %>%
  select(
    CO_CURSO,
    CO_IES,
    CO_CATEGAD,
    CO_ORGACAD,
    CO_MUNIC_CURSO,
    CO_UF_CURSO,
    CO_REGIAO_CURSO
  ) %>%
  distinct()

enamed <- arq3_validos %>%
  left_join(curso_info, by = "CO_CURSO", relationship = "many-to-one")
