  # Script para importacao
#caminho para o arquivo compactado do ENAMED
arquivo_zip <- "dados/brutos/microdados_enamed_2025.zip"
#verifica se o arquivo existe
file.exists(arquivo_zip)
#lista o conteúdo do arquivo ZIP sem descompactar
conteudo_zip <- unzip(arquivo_zip,list=TRUE)
#Mostra os primeiros itens do ZIP
head(conteudo_zip)

#Filtrando arquivos que contem a palavra Enade

arquivos_enade <- conteudo_zip[grepl("/Enade/",conteudo_zip$Name),]

#extraindo somente os arquivos .txt
arquivos_txt <- arquivos_enade[grepl("\\.txt$",arquivos_enade$Name),]

#Ordenando os arquivos através da numeração

# Extrai o número que aparece após "arq" e antes de ".txt"
num_arquivo <- sub(".*arq([0-9]+)\\.txt$","\\1",arquivos_txt$Name)

#converte string da variável num_arquivo para inteiro

num_arquivo <- as.integer(num_arquivo)
#ordem para colocar os números em ordem crescente. 
ordem <- order(num_arquivo)
# Reordena as linhas da tabela
arquivos_txt <- arquivos_txt[ordem,] 
