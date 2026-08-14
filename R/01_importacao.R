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

#extraindo somente os arquivos .txt
arquivos_txt <- arquivos_enade[grepl("\\.txt$", arquivos_enade$Name), ]
#Atribui a variável arquivos_txt uma versão modificada na ordem sequencial crescente.
arquivos_txt <- arquivos_txt %>%
  mutate(num_arquivo = parse_number(str_extract(Name, "arq\\d+"))) %>%
  arrange(num_arquivo)
