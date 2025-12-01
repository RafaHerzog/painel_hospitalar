library(dplyr)
library(tidyr)
library(janitor)
library(data.table)

# Preparando uma base auxiliar de CNES ------------------------------------
df_cnes_lt <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_lt_2018_2023.csv.gz")
df_sinasc_nasc_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sinasc_nasc_em_hospital_2018_2023.csv.gz")

## Criando uma base com o número de nascidos vivos por mês em cada CNES
df_total_nascidos <- df_sinasc_nasc_em_hospital |>
  group_by(CODESTAB, CODMUNNASC, mes, ano) |>
  summarise(total_de_nascidos_vivos = n(), .groups = "drop") |>
  rename(CNES = CODESTAB, CODUFMUN = CODMUNNASC)

## Baixando uma planilha que contém os nomes dos estabelecimentos
df_nomes_fantasia <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/CNES/cnes_estabelecimentos_csv.zip") |>
  select(CNES = CO_CNES, CODUFMUN = CO_IBGE, nome_fantasia = NO_FANTASIA)

## Criando uma base contendo apenas os estabelecimentos com média de nascidos vivos/ano de 100 ou mais
df_cnes_100_nv <- df_total_nascidos |>
  group_by(CNES, CODUFMUN, ano) |>
  summarise(total_de_nascidos_vivos = sum(total_de_nascidos_vivos), .groups = "drop") |>
  group_by(CNES, CODUFMUN) |>
  summarise(total_de_nascidos_vivos = mean(total_de_nascidos_vivos), .groups = "drop") |>
  filter(total_de_nascidos_vivos >= 100) |>
  mutate(
    categoria_nv = case_when(
      total_de_nascidos_vivos < 500 ~ "nv_menos_500",
      total_de_nascidos_vivos >= 500 & total_de_nascidos_vivos < 1000 ~ "nv_500_a_999",
      total_de_nascidos_vivos >= 1000 ~ "nv_1000_mais"
    )
  )

## Filtrando, na base do CNES, apenas pelos estabelecimentos que apresentaram média de nascidos vivos/ano >= 100
df_cnes_lt_filtrado <- left_join(
  semi_join(df_total_nascidos, df_cnes_100_nv |> select(CNES, CODUFMUN)),
  df_cnes_lt,
  join_by(CNES, CODUFMUN, mes, ano)
) |>
  left_join(df_cnes_100_nv |> select(CNES, CODUFMUN, categoria_nv)) |>
  left_join(df_nomes_fantasia) |>
  filter(!is.na(nome_fantasia)) |>
  mutate(
    # Se o tipo é NA, então o estabelecimento não aparece no SIH/SUS, apesar de ter partos. Logo, ele é privado
    tipo = ifelse(
      is.na(tipo),
      "privado",
      tipo
    )
  ) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(
    categoria_leitos_utin = case_when(
      leitos_uti_neonatal < 4 ~ "leitos_utin_menos_4",
      leitos_uti_neonatal >= 4 ~ "leitos_utin_4_mais"
    )
  ) |>
  mutate(
    categoria_porte = paste0(categoria_nv, "_", categoria_leitos_utin)
  )

## Adicionando informações referentes ao município em que o estabelecimento se localiza
### Lendo uma base auxiliar de municípios
df_aux_municipios <- read.csv("data-raw/extracao-dos-dados/databases/tabela_aux_municipios.csv")

### Juntando as duas bases
df_cnes_aux <- left_join(
  df_cnes_lt_filtrado |> mutate(CODUFMUN = as.numeric(CODUFMUN)),
  df_aux_municipios,
  by = join_by(CODUFMUN == codmun)
) |>
  clean_names() |>
  select(cnes, codufmun, nome_fantasia, municipio:macro_r_saude, mes, ano, tipo, categoria_porte, leitos_obstetricos:leitos_uti_neonatal)

### Salvando a base auxiliar
write.csv(df_cnes_aux, "data-raw/csv/df_cnes_aux.csv", row.names = FALSE)
write.csv(df_cnes_aux, "data-raw/extracao-dos-dados/databases/df_cnes_aux.csv", row.names = FALSE)
