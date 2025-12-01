library(microdatasus)
library(dplyr)
library(janitor)
library(data.table)
library(stringr)
library(tidyr)
library(data.table)
library(future)
library(future.apply)

# Baixando todos os dados necessários (com paralelização) ---------------------
## Criando o planejamento dos futures
plan(multisession)

## Criando uma função que baixa todos os dados necessários para um certo ano
processa_ano <- function(ano) {
  # Carrega os pacotes dentro da worker
  library(microdatasus)
  library(dplyr)
  library(data.table)
  library(stringr)
  
  message("Processando ano ", ano)
  
  # Baixando os dados do SIH-RD para o dado ano
  procedimentos_parto <- c("0310010039", "0310010047", "0310010055", "0411010026", "0411010034", "0411010042")
  
  df_sih_rd <- microdatasus::fetch_datasus(
    year_start = ano,
    year_end = ano,
    month_start = 1,
    month_end = 12,
    information_system = "SIH-RD",
    vars = c("ANO_CMPT", "DT_INTER", "CNES", "MUNIC_MOV", "SEXO", "IDADE", "PROC_REA")
  ) |>
    mutate(ano_dt_inter = substr(DT_INTER, 1, 4)) |>
    filter(PROC_REA %in% procedimentos_parto)

  list(
    df_sih_rd = df_sih_rd
  )
}

## Criando um vetor com os anos a serem baixados
anos <- 2018:2023

## Baixando todos os dados
resultados <- future_lapply(anos, processa_ano)

## Separando-os em objetos diferentes
df_sih_rd <- rbindlist(lapply(resultados, `[[`, "df_sih_rd"), fill = TRUE)

## Criando uma função para salvar os arquivos
salva_csv_gz <- function(df, nome) {
  path <- paste0("data-raw/extracao-dos-dados/databases/", nome, "_2018_2023.csv.gz")
  write.csv(df, gzfile(path), row.names = FALSE)
}

## Salvando os arquivos
salva_csv_gz(df_sih_rd, "df_sih_rd")

# Tabela de correspondência entre código da UF, sigla e região
ufs <- tibble::tribble(
  ~cod_uf, ~uf,                ~regiao,
  "11",    "Rondônia",         "Norte",
  "12",    "Acre",             "Norte",
  "13",    "Amazonas",         "Norte",
  "14",    "Roraima",          "Norte",
  "15",    "Pará",             "Norte",
  "16",    "Amapá",            "Norte",
  "17",    "Tocantins",        "Norte",
  "21",    "Maranhão",         "Nordeste",
  "22",    "Piauí",            "Nordeste",
  "23",    "Ceará",            "Nordeste",
  "24",    "Rio Grande do Norte", "Nordeste",
  "25",    "Paraíba",          "Nordeste",
  "26",    "Pernambuco",       "Nordeste",
  "27",    "Alagoas",          "Nordeste",
  "28",    "Sergipe",          "Nordeste",
  "29",    "Bahia",            "Nordeste",
  "31",    "Minas Gerais",     "Sudeste",
  "32",    "Espírito Santo",   "Sudeste",
  "33",    "Rio de Janeiro",   "Sudeste",
  "35",    "São Paulo",        "Sudeste",
  "41",    "Paraná",           "Sul",
  "42",    "Santa Catarina",   "Sul",
  "43",    "Rio Grande do Sul","Sul",
  "50",    "Mato Grosso do Sul","Centro-Oeste",
  "51",    "Mato Grosso",      "Centro-Oeste",
  "52",    "Goiás",            "Centro-Oeste",
  "53",    "Distrito Federal", "Centro-Oeste"
)

# Define a ordem desejada das regiões
ordem_regioes <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")

# Junta com info de UF/região e organiza
df_partos_sih1 <- df_sih_rd |>
  dplyr::mutate(
    cod_uf = stringr::str_sub(as.character(MUNIC_MOV), 1, 2)
  ) |>
  dplyr::left_join(ufs, by = "cod_uf") |>
  dplyr::group_by(uf, regiao) |>
  dplyr::summarise(total_de_partos = dplyr::n(), .groups = "drop") |>
  dplyr::mutate(regiao = factor(regiao, levels = ordem_regioes)) |>
  dplyr::arrange(regiao, uf)

df_nv_sinasc1 <- read.csv("data-raw/csv/df_indicadores_sinasc_2018_2023.csv") |>
  filter(tipo %in% c("publico", "misto")) |>
  dplyr::group_by(uf, regiao) |>
  dplyr::summarise(total_de_nascidos_vivos = sum(total_de_nascidos_vivos), .groups = "drop") |>
  dplyr::mutate(regiao = factor(regiao, levels = ordem_regioes)) |>
  dplyr::arrange(regiao, uf)





df_partos_sih2 <- df_sih_rd |>
  dplyr::mutate(
    cod_uf = stringr::str_sub(as.character(MUNIC_MOV), 1, 2)
  ) |>
  dplyr::left_join(ufs, by = "cod_uf") |>
group_by(uf) |>
summarise(total_de_partos = n(), .groups = "drop") |>
rename(cnes = CNES, codmunnasc = MUNIC_MOV, ano = ANO_CMPT) |>
arrange(cnes, codmunnasc, ano)




# Obtendo o número de partos por ano por CNES
df_partos_sih2 <- df_sih_rd |>
  dplyr::mutate(
    cod_uf = stringr::str_sub(as.character(MUNIC_MOV), 1, 2)
  ) |>
  dplyr::left_join(ufs, by = "cod_uf") |>
  rename(cnes = CNES, codmunnasc = MUNIC_MOV, ano = ANO_CMPT) |>
  mutate(cnes = as.numeric(cnes), codmunnasc = as.numeric(codmunnasc), ano = as.numeric(ano)) |>
  group_by(cnes, codmunnasc, ano, uf, regiao) |>
  summarise(total_de_partos = n(), .groups = "drop")


# Lendo a base contendo as variáveis do SINASC, filtrando por hospitais públicos ou mistos e obtendo o total de n.v. por ano por CNES
df_nv_sinasc2 <- read.csv("data-raw/csv/df_indicadores_sinasc_2018_2023.csv") |>
  filter(tipo %in% c("publico", "misto")) |>
  group_by(cnes, codmunnasc, ano, uf, regiao) |>
  summarise(total_de_nascidos_vivos = sum(total_de_nascidos_vivos), .groups = "drop")

df_juncao <- inner_join(df_partos_sih2, df_nv_sinasc2) |>
  filter(total_de_nascidos_vivos >= total_de_partos) |>
  group_by(uf, regiao) |>
  summarise(
    total_de_nascidos_vivos = sum(total_de_nascidos_vivos), 
    total_de_partos = sum(total_de_partos),
    .groups = "drop"
  ) |>
  dplyr::mutate(regiao = factor(regiao, levels = ordem_regioes)) |>
  dplyr::arrange(regiao, uf) |>
  # group_by(regiao) |>
  # summarise(
  #   total_de_nascidos_vivos = sum(total_de_nascidos_vivos), 
  #   total_de_partos = sum(total_de_partos),
  #   .groups = "drop"
  # ) |>
  mutate(porc = total_de_partos/total_de_nascidos_vivos)

