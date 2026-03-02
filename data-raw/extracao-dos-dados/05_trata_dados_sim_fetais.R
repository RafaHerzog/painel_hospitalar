library(dplyr)
library(janitor)
library(data.table)
library(stringr)
library(tidyr)
library(readxl)

# Para os indicadores de número de óbitos, taxa de mortalidade e distribuição dos óbitos por peso ou momento do óbito -----
## Lendo o arquivo com os óbitos fetais em hospital no período de 2018-2023
df_obitos_fetais_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sim_obito_fetal_em_hospital_2018_2023.csv.gz")

## Lendo o arquivo com os nascimentos em hospital no período de 2018-2023
df_sinasc_nasc_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sinasc_nasc_em_hospital_2018_2023.csv.gz")

## Lendo o arquivo com a base auxiliar de CNES
df_cnes_aux <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_aux.csv") |>
  select(cnes:nome_fantasia, mes, ano)

## Para os dados do SINASC, criando variáveis de nascidos vivos por faixa de peso
df_indicadores_sinasc <- df_sinasc_nasc_em_hospital |>
  clean_names() |>
  mutate(
    # Garantindo que as variáveis são do tipo correto
    peso = as.numeric(peso)
  ) |>
  mutate(
    total_de_nascidos_vivos = 1,
    nv_peso_menos_1000 = if_else(
      peso < 1000, 1, 0, missing = 0
    ),
    nv_peso_1000_1499 = if_else(
      (peso >= 1000 & peso < 1500), 1, 0, missing = 0
    ),
    nv_peso_1500_2499 = if_else(
      (peso >= 1500 & peso < 2500), 1, 0, missing = 0
    ),
    nv_peso_2500_mais = if_else(
      peso >= 2500, 1, 0, missing = 0
    )
  ) |>
  group_by(codestab, codmunnasc, mes, ano) |>
  summarise_at(vars(starts_with("total_") | starts_with("nv")), sum) |>
  ungroup()

## Para os dados do SIM, criando variáveis de óbitos fetais por faixa de peso e momento do óbito
df_indicadores_fetais <- df_obitos_fetais_em_hospital |>
  clean_names() |>
  mutate(
    # Garantindo que as variáveis são do tipo correto
    peso = as.numeric(peso),
    obitoparto = as.numeric(obitoparto)
  ) |>
  # Criando as variáveis necessárias para o cálculo dos indicadores
  mutate(
    obitos_fetais_todos_todos = 1,

    # Número de óbitos fetais por peso e momento do óbito
    ## Todos os momentos do óbito
    obitos_fetais_menos_1000_todos = if_else(
      peso < 1000, 1, 0, missing = 0
    ),
    obitos_fetais_1000_1499_todos = if_else(
      peso >= 1000 & peso < 1500, 1, 0, missing = 0
    ),
    obitos_fetais_1500_2499_todos = if_else(
      peso >= 1500 & peso < 2500, 1, 0, missing = 0
    ),
    obitos_fetais_2500_mais_todos = if_else(
      peso >= 2500, 1, 0, missing = 0
    ),

    ## Apenas antes do parto
    obitos_fetais_todos_antes = if_else(
      obitoparto == 1, 1, 0, missing = 0
    ),
    obitos_fetais_menos_1000_antes = if_else(
      peso < 1000 & obitoparto == 1, 1, 0, missing = 0
    ),
    obitos_fetais_1000_1499_antes = if_else(
      peso >= 1000 & peso < 1500 & obitoparto == 1, 1, 0, missing = 0
    ),
    obitos_fetais_1500_2499_antes = if_else(
      peso >= 1500 & peso < 2500 & obitoparto == 1, 1, 0, missing = 0
    ),
    obitos_fetais_2500_mais_antes = if_else(
      peso >= 2500 & obitoparto == 1, 1, 0, missing = 0
    ),

    ## Apenas durante o parto
    obitos_fetais_todos_durante = if_else(
      obitoparto == 2, 1, 0, missing = 0
    ),
    obitos_fetais_menos_1000_durante = if_else(
      peso < 1000 & obitoparto == 2, 1, 0, missing = 0
    ),
    obitos_fetais_1000_1499_durante = if_else(
      peso >= 1000 & peso < 1500 & obitoparto == 2, 1, 0, missing = 0
    ),
    obitos_fetais_1500_2499_durante = if_else(
      peso >= 1500 & peso < 2500 & obitoparto == 2, 1, 0, missing = 0
    ),
    obitos_fetais_2500_mais_durante = if_else(
      peso >= 2500 & obitoparto == 2, 1, 0, missing = 0
    )
  ) |>
  group_by(codestab, codmunocor, mes, ano) |>
  summarise_at(vars(starts_with("obitos")), sum) |>
  ungroup() |>
  # Juntando com a base auxiliar de CNES
  right_join(df_cnes_aux, by = join_by(codestab == cnes, codmunocor == codufmun, mes, ano)) |>
  # Juntando com a base contendo as variáveis do SINASC
  left_join(df_indicadores_sinasc, by = join_by(codestab, codmunocor == codmunnasc, mes, ano)) |>
  rename(cnes = codestab) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  dplyr::select(
    cnes, codmunocor, nome_fantasia, mes, ano,
    total_de_nascidos_vivos,
    starts_with("nv"),
    starts_with("obitos")
  ) |>
  arrange(cnes, codmunocor, mes, ano)

## Salvando a base final
write.csv(df_indicadores_fetais, "data-raw/csv/df_indicadores_fetais_2018_2023.csv", row.names = FALSE)


# Para os indicadores de causas evitáveis ---------------------------------------
## Lendo o arquivo com os óbitos fetais em hospital no período de 2018-2023
df_obitos_fetais_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sim_obito_fetal_em_hospital_2018_2023.csv.gz")

## Lendo o arquivo com a base auxiliar de CNES
df_cnes_aux <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_aux.csv") |>
  select(cnes:nome_fantasia, mes, ano)

## Definindo os vetores de CIDs
df_cids_evitaveis <- read_excel("data-raw/extracao-dos-dados/databases/evitabilidade_fetal.xlsx", sheet = "Fetal") |>
  dplyr::rename(nome = LBE_FETAL, cid = CID)

lista_cids_evitaveis <- list(
  imunoprevencao = df_cids_evitaveis |> dplyr::filter(nome == "Imunoprevenção") |> dplyr::pull(cid),
  gestacao = df_cids_evitaveis |> dplyr::filter(nome == "Reduzíveis por adequada atenção à mulher na gestação") |> dplyr::pull(cid),
  parto = df_cids_evitaveis |> dplyr::filter(nome == "Reduzíveis por adequada atenção à mulher no parto") |> dplyr::pull(cid),
  mal_definidas = df_cids_evitaveis |> dplyr::filter(nome == "Causas de morte mal-definidas") |> dplyr::pull(cid),
  nao_aplica = df_cids_evitaveis |> dplyr::filter(nome == "Não se aplicam ao óbito fetal") |> dplyr::pull(cid)
)

## Criando uma função para categorizar CIDs de acordo com os grupos de causas evitáveis
cria_grupo_evitavel <- function(data, lista_cids, prefixo, filtro_obitoparto = NULL) {
  data <- janitor::clean_names(data)

  if (!is.null(filtro_obitoparto)) {
    if (is.na(filtro_obitoparto)) {
      data <- dplyr::filter(data, is.na(obitoparto) | obitoparto == 9)
    } else {
      data <- dplyr::filter(data, obitoparto == filtro_obitoparto)
    }
  }

  data |>
    dplyr::mutate(
      causabas2 = substr(causabas, 1, 3),
      faixa_de_peso = dplyr::case_when(
        is.na(peso) ~ "sem_informacao",
        peso < 1000 ~ "menor_1000",
        peso < 1500 ~ "1000_a_1499",
        peso < 2500 ~ "1500_a_2499",
        peso >= 2500 ~ "2500_mais"
      ),
      grupo_cid = dplyr::case_when(
        causabas %in% lista_cids$imunoprevencao | causabas2 %in% lista_cids$imunoprevencao ~ paste0(prefixo, "_imunoprevencao"),
        causabas %in% lista_cids$gestacao | causabas2 %in% lista_cids$gestacao ~ paste0(prefixo, "_mulher_gestacao"),
        causabas %in% lista_cids$parto | causabas2 %in% lista_cids$parto ~ paste0(prefixo, "_mulher_parto"),
        causabas %in% lista_cids$nao_aplica | causabas2 %in% lista_cids$nao_aplica ~ paste0(prefixo, "_nao_aplica"),
        causabas %in% lista_cids$mal_definidas | causabas2 %in% lista_cids$mal_definidas ~ paste0(prefixo, "_mal_definidas"),
        TRUE ~ paste0(prefixo, "_demais_causas")
      )
    ) |>
    dplyr::select(codmunocor, codestab, mes, ano, grupo_cid, faixa_de_peso) |>
    dplyr::mutate(obitos = 1L) |>
    dplyr::group_by(across(!obitos)) |>
    dplyr::summarise(obitos = sum(obitos), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = c(grupo_cid, faixa_de_peso),
      values_from = obitos,
      values_fill = 0,
      names_sort = TRUE
    ) |>
    dplyr::right_join(df_cnes_aux, by = join_by(codestab == cnes, codmunocor == codufmun, mes, ano)) |>
    dplyr::mutate(across(everything(), ~ tidyr::replace_na(.x, 0))) |>
    dplyr::arrange(codmunocor)
}

## Criando uma função para adicionar colunas de totais
adiciona_totais_evitaveis <- function(df, prefixo) {
  grupos <- c("imunoprevencao", "mulher_gestacao", "mulher_parto", "nao_aplica", "mal_definidas", "demais_causas")

  for (grupo in grupos) {
    df <- df |>
      dplyr::mutate(
        !!paste0(prefixo, "_", grupo, "_total") := rowSums(dplyr::across(dplyr::starts_with(paste0(prefixo, "_", grupo))), na.rm = TRUE)
      )
  }

  df <- df |>
    dplyr::mutate(
      !!paste0("obitos_fetais_totais") := rowSums(dplyr::across(dplyr::starts_with(prefixo) & dplyr::ends_with("_total")), na.rm = TRUE)
    )

  return(df)
}

## Criando os dataframes
df_evitaveis_fetal_todos <- cria_grupo_evitavel(df_obitos_fetais_em_hospital, lista_cids_evitaveis, "evitaveis_fetal") |> adiciona_totais_evitaveis("evitaveis_fetal")
df_evitaveis_fetal_antes <- cria_grupo_evitavel(df_obitos_fetais_em_hospital, lista_cids_evitaveis, "evitaveis_fetal_antes", filtro_obitoparto = 1)
df_evitaveis_fetal_durante <- cria_grupo_evitavel(df_obitos_fetais_em_hospital, lista_cids_evitaveis, "evitaveis_fetal_durante", filtro_obitoparto = 2)
df_evitaveis_fetal_sem_info_parto <- cria_grupo_evitavel(df_obitos_fetais_em_hospital, lista_cids_evitaveis, "evitaveis_fetal_sem_info_parto", filtro_obitoparto = NA)

## Unindo todos os dataframes em um só
df_bloco7_fetais_evitaveis <- list(
  df_evitaveis_fetal_todos,
  df_evitaveis_fetal_antes,
  df_evitaveis_fetal_durante,
  df_evitaveis_fetal_sem_info_parto
) |>
  purrr::reduce(dplyr::full_join) |>
  select(
    codmunocor, codestab, nome_fantasia, mes, ano,
    starts_with("evitaveis_"),
    "obitos_fetais_totais"
  )

## Salvando a base final
write.csv(df_bloco7_fetais_evitaveis, "data-raw/csv/df_causas_evitaveis_fetais_2018_2023.csv", row.names = FALSE)


# Para os indicadores de grupos de causas ---------------------------------------
## Lendo o arquivo com os óbitos fetais em hospital no período de 2018-2023
df_obitos_fetais_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sim_obito_fetal_em_hospital_2018_2023.csv.gz")

## Lendo o arquivo com a base auxiliar de CNES
df_cnes_aux <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_aux.csv") |>
  select(cnes:nome_fantasia, mes, ano)

## Definindo os vetores de CIDs
### Criando vetores com as cids de cada grupo
grupos_prematuridade <- c("P07", "P220", "P25", "P26", "P52", "P77")


grupos_infeccoes <- c("P35", "P36", "P37", "P38", "P39", "A40", "A41", "P23",
                      "J12", "J13", "J14", "J15", "J16", "J17", "J18", "A00", "A01",
                      "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A33",
                      "A50", "B20", "B21", "B22", "B23", "B24", "G00", "G03", "G04")

grupos_asfixia <- c("P017", "P020", "P021", "P024", "P025", "P026", "P03",
                    "P10", "P11", "P12", "P13", "P14", "P15", "P20", "P21", "P24")

grupos_respiratorias <- c("P221", "P228", "P229", "P28")

grupos_gravidez <- c("P00", "P010", "P011", "P012", "P013", "P014", "P015", "P016",
                     "P018", "P019", "P022", "P023", "P027", "P028", "P029", "P04",
                     "P05", "P964")

grupos_afeccoes_perinatal <- c("P969")

grupos_anomalias <- c(paste0("Q", sprintf("%02d", 0:99)))

grupos_mal_definidas <- c(paste0("R", sprintf("%02d", 0:99)))

### Definindo os grupos como lista nomeada
lista_cids_causas_principais <- list(
  prematuridade = grupos_prematuridade,
  infeccoes = grupos_infeccoes,
  asfixia = grupos_asfixia,
  respiratorias = grupos_respiratorias,
  gravidez = grupos_gravidez,
  afeccoes_perinatal = grupos_afeccoes_perinatal,
  anomalias = grupos_anomalias,
  mal_definidas = grupos_mal_definidas
)

## Criando uma função para categorizar CIDs de acordo com os grupos de causas principais
cria_grupo_causa <- function(data, lista_cids, prefixo, filtro_obitoparto = NULL) {
  data <- janitor::clean_names(data)

  if (!is.null(filtro_obitoparto)) {
    if (is.na(filtro_obitoparto)) {
      data <- dplyr::filter(data, is.na(obitoparto) | obitoparto == 9)
    } else {
      data <- dplyr::filter(data, obitoparto == filtro_obitoparto)
    }
  }

  data |>
    dplyr::mutate(
      causabas2 = substr(causabas, 1, 3),
      faixa_de_peso = dplyr::case_when(
        is.na(peso) ~ "sem_informacao",
        peso < 1000 ~ "menor_1000",
        peso < 1500 ~ "1000_a_1499",
        peso < 2500 ~ "1500_a_2499",
        peso >= 2500 ~ "2500_mais"
      ),
      grupo_cid = dplyr::case_when(
        causabas %in% lista_cids$prematuridade | causabas2 %in% lista_cids$prematuridade ~ paste0(prefixo, "_prematuridade"),
        causabas %in% lista_cids$infeccoes | causabas2 %in% lista_cids$infeccoes ~ paste0(prefixo, "_infeccoes"),
        causabas %in% lista_cids$asfixia | causabas2 %in% lista_cids$asfixia ~ paste0(prefixo, "_asfixia"),
        causabas %in% lista_cids$respiratorias | causabas2 %in% lista_cids$respiratorias ~ paste0(prefixo, "_respiratorias"),
        causabas %in% lista_cids$gravidez | causabas2 %in% lista_cids$gravidez ~ paste0(prefixo, "_gravidez"),
        causabas %in% lista_cids$afeccoes_perinatal | causabas2 %in% lista_cids$afeccoes_perinatal ~ paste0(prefixo, "_afeccoes_perinatal"),
        causabas %in% lista_cids$anomalias | causabas2 %in% lista_cids$anomalias ~ paste0(prefixo, "_anomalias"),
        causabas %in% lista_cids$mal_definidas | causabas2 %in% lista_cids$mal_definidas ~ paste0(prefixo, "_mal_definidas"),
        TRUE ~ paste0(prefixo, "_demais_causas")
      )
    ) |>
    dplyr::select(codmunocor, codestab, mes, ano, grupo_cid, faixa_de_peso) |>
    dplyr::mutate(obitos = 1L) |>
    dplyr::group_by(across(!obitos)) |>
    dplyr::summarise(obitos = sum(obitos), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = c(grupo_cid, faixa_de_peso),
      values_from = obitos,
      values_fill = 0,
      names_sort = TRUE
    ) |>
    dplyr::right_join(df_cnes_aux, by = join_by(codestab == cnes, codmunocor == codufmun, mes, ano)) |>
    dplyr::mutate(across(everything(), ~ tidyr::replace_na(.x, 0))) |>
    dplyr::arrange(codmunocor)
}

## Criando uma função para adicionar colunas de totais
adiciona_totais_causa <- function(df, prefixo) {
  grupos <- c("prematuridade", "infeccoes", "asfixia", "respiratorias", "gravidez",
              "afeccoes_perinatal", "anomalias", "mal_definidas", "demais_causas")

  for (grupo in grupos) {
    df <- df |>
      dplyr::mutate(
        !!paste0(prefixo, "_", grupo, "_total") := rowSums(dplyr::across(dplyr::starts_with(paste0(prefixo, "_", grupo))), na.rm = TRUE)
      )
  }

  df <- df |>
    dplyr::mutate(
      !!paste0("obitos_fetais_totais") := rowSums(dplyr::across(dplyr::starts_with(prefixo) & dplyr::ends_with("_total")), na.rm = TRUE)
    )

  return(df)
}

## Gerando os dataframes
df_principais_fetal_todos <- cria_grupo_causa(df_obitos_fetais_em_hospital, lista_cids_causas_principais, "principais_fetal") |> adiciona_totais_causa("principais_fetal")
df_principais_fetal_antes <- cria_grupo_causa(df_obitos_fetais_em_hospital, lista_cids_causas_principais, "principais_fetal_antes", filtro_obitoparto = 1)
df_principais_fetal_durante <- cria_grupo_causa(df_obitos_fetais_em_hospital, lista_cids_causas_principais, "principais_fetal_durante", filtro_obitoparto = 2)
df_principais_fetal_sem_info_parto <- cria_grupo_causa(df_obitos_fetais_em_hospital, lista_cids_causas_principais, "principais_fetal_sem_info_parto", filtro_obitoparto = NA)

## Unindo todos os dataframes em um só
df_bloco7_principais_fetal <- list(
  df_principais_fetal_todos,
  df_principais_fetal_antes,
  df_principais_fetal_durante,
  df_principais_fetal_sem_info_parto
) |>
  purrr::reduce(dplyr::full_join) |>
  select(
    codmunocor, codestab, nome_fantasia, mes, ano,
    starts_with("principais_"),
    "obitos_fetais_totais"
  )

## Salvando a base final
write.csv(df_bloco7_principais_fetal, "data-raw/csv/df_causas_principais_fetais_2018_2023.csv", row.names = FALSE)
