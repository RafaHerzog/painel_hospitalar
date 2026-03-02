library(dplyr)
library(janitor)
library(data.table)
library(stringr)
library(tidyr)
library(readxl)

# Para os indicadores de número de óbitos, taxa de mortalidade e distribuição dos óbitos por peso ou momento do óbito -----
## Lendo o arquivo já tratado com os indicadores de óbitos fetais
df_indicadores_fetais <- fread("data-raw/csv/df_indicadores_fetais_2018_2023.csv")

## Lendo o arquivo já tratado com os indicadores de óbitos neonatais
df_indicadores_neonatais <- fread("data-raw/csv/df_indicadores_neonatais_2018_2023.csv")

## Juntando as duas bases
df_indicadores_fetais_neonatais <- full_join(df_indicadores_fetais, df_indicadores_neonatais)

## Criando variáveis de óbitos perinatais por faixa de peso e momento do óbito
df_indicadores_perinatais <- df_indicadores_fetais_neonatais |>
  # Criando as variáveis necessárias para o cálculo dos indicadores
  mutate(
    obitos_perinatais_todos_todos = obitos_fetais_todos_todos + obitos_neonatais_todos_0_a_6_dias,

    # Número de óbitos neonatais por peso e momento do óbito
    ## Todos os momentos do óbito
    obitos_perinatais_menos_1000_todos = obitos_fetais_menos_1000_todos + obitos_neonatais_menos_1000_0_a_6_dias,
    obitos_perinatais_1000_1499_todos = obitos_fetais_1000_1499_todos + obitos_neonatais_1000_1499_0_a_6_dias,
    obitos_perinatais_1500_2499_todos = obitos_fetais_1500_2499_todos + obitos_neonatais_1500_2499_0_a_6_dias,
    obitos_perinatais_2500_mais_todos = obitos_fetais_2500_mais_todos + obitos_neonatais_2500_mais_0_a_6_dias,

    ## Apenas óbitos antes do parto
    obitos_perinatais_todos_antes = obitos_fetais_todos_antes,
    obitos_perinatais_menos_1000_antes = obitos_fetais_menos_1000_antes,
    obitos_perinatais_1000_1499_antes = obitos_fetais_1000_1499_antes,
    obitos_perinatais_1500_2499_antes = obitos_fetais_1500_2499_antes,
    obitos_perinatais_2500_mais_antes = obitos_fetais_2500_mais_antes,

    obitos_perinatais_todos_durante = obitos_fetais_todos_durante,
    obitos_perinatais_menos_1000_durante = obitos_fetais_menos_1000_durante,
    obitos_perinatais_1000_1499_durante = obitos_fetais_1000_1499_durante,
    obitos_perinatais_1500_2499_durante = obitos_fetais_1500_2499_durante,
    obitos_perinatais_2500_mais_durante = obitos_fetais_2500_mais_durante,

    obitos_perinatais_todos_0_dias = obitos_neonatais_todos_0_dias,
    obitos_perinatais_menos_1000_0_dias = obitos_neonatais_menos_1000_0_dias,
    obitos_perinatais_1000_1499_0_dias = obitos_neonatais_1000_1499_0_dias,
    obitos_perinatais_1500_2499_0_dias = obitos_neonatais_1500_2499_0_dias,
    obitos_perinatais_2500_mais_0_dias = obitos_neonatais_2500_mais_0_dias,

    obitos_perinatais_todos_1_a_6_dias = obitos_neonatais_todos_1_a_6_dias,
    obitos_perinatais_menos_1000_1_a_6_dias = obitos_neonatais_menos_1000_1_a_6_dias,
    obitos_perinatais_1000_1499_1_a_6_dias = obitos_neonatais_1000_1499_1_a_6_dias,
    obitos_perinatais_1500_2499_1_a_6_dias = obitos_neonatais_1500_2499_1_a_6_dias,
    obitos_perinatais_2500_mais_1_a_6_dias = obitos_neonatais_2500_mais_1_a_6_dias
  ) |>
  dplyr::select(
    cnes, codmunocor, mes, ano,
    total_de_nascidos_vivos,
    starts_with("nv"),
    starts_with("obitos_perinatais")
  )

## Salvando a base final
write.csv(df_indicadores_perinatais, "data-raw/csv/df_indicadores_perinatais_2018_2023.csv", row.names = FALSE)


# Para os indicadores de causas evitáveis ---------------------------------------
## Lendo o arquivo com os óbitos fetais em hospital no período de 2018-2023
df_obitos_fetais_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sim_obito_fetal_em_hospital_2018_2023.csv.gz") |>
  select(CODESTAB, CODMUNOCOR, mes, ano, PESO, CAUSABAS, OBITOPARTO, IDADE)

## Lendo o arquivo com os óbitos neonatais em hospital no período de 2018-2023 (e filtrando por IDADE <= 206)
df_obitos_neonatais_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sim_obito_neonatal_em_hospital_2018_2023.csv.gz") |>
  filter(as.numeric(IDADE) <= 206) |>
  select(CODESTAB, CODMUNOCOR, mes, ano, PESO, CAUSABAS, OBITOPARTO, IDADE)

## Lendo o arquivo com a base auxiliar de CNES
df_cnes_aux <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_aux.csv") |>
  select(cnes:nome_fantasia, mes, ano)

## Definindo os vetores de CIDs
### Criando vetores com as cids de cada grupo de causas evitáveis
imunoprevencao <- c(
  "A17", "A19", "A33", "A35", "A36", "A37", "A80", "B05", "B06",
  "B16", "B260", "G000", "P350", "P353"
)

mulher_gestacao <- c(
  "A50", sprintf("B2%d", 0:4), "P022", "P023", "P027", "P028",
  "P029", "P00", "P04", "P01", "P05", "P07", "P220", "P26",
  "P52", "P550", "P551", "P558", "P559", "P56", "P57", "P77"
)

evitaveis_parto <- c(
  "P020", "P021", "P024", "P025", "P026", "P03", "P08", sprintf("P1%d", 0:5),
  "P20", "P21", "P24"
)

recem_nascido <- c(
  "P221", "P228", "P229", "P23", "P25", "P27", "P28",
  sprintf("P3%d", 51:53), sprintf("P3%d", 58:59), sprintf("P3%d", 6:9), sprintf("P5%d", 0:1), sprintf("P5%d", 3:4), "P58", "P59",
  sprintf("P7%d", 0:4), "P60", "P61",  sprintf("P7%d", 5:6), "P78",
  sprintf("P8%d", 0:3),  sprintf("P9%d", 0:4),
  sprintf("P9%d", 60:68)
)

acoes_diagnostico <- c(
  "A15", "A16", "A18", sprintf("G0%d", 0:4), sprintf("J0%d", 0:6),
  sprintf("J1%d", 2:8), sprintf("J1%d", 2:8), sprintf("J2%d", 0:2),
  "J384", sprintf("J4%d", 0:2), sprintf("J4%d", 5:7), sprintf("J6%d", 8:9),
  sprintf("A7%d", 0:4), "A30", "A31", "A32", "A38", "A39", "A40", "A41",
  "A46", "A49", "E030", "E031", sprintf("E1%d", 0:4), "E700", "E730",
  "G40", "G41", "Q90", "N390", sprintf("I0%d", 0:9)
)

acoes_saude <- c(
  sprintf("A0%d", 0:9), sprintf("A2%d", 0:8), sprintf("A9%d", 0:9),
  sprintf("A7%d", 5:9), "A82", sprintf("B5%d", 0:9), sprintf("B6%d", 0:4),
  sprintf("B6%d", 5:9), sprintf("B7%d", 0:9), sprintf("B8%d", 0:3),
  "B99", sprintf("D5%d", 0:3), sprintf("E4%d", 0:9), sprintf("E5%d", 0:9),
  sprintf("E6%d", 0:4), "E86", c(sprintf("V%02d", 1:99)), sprintf("X4%d", 0:4),
  sprintf("X4%d", 5:9), "R95", c(sprintf("W%02d", 0:19)), sprintf("X0%d", 0:9),
  sprintf("X3%d", 0:9), c(sprintf("W%02d", 65:74)), c(sprintf("W%02d", 75:84)),
  c(sprintf("W%02d", 85:99)), c(sprintf("X%02d", 85:99)),
  c(sprintf("Y%02d", 00:09)), c(sprintf("Y%02d", 10:34)), c(sprintf("W%02d", 20:49)),
  c(sprintf("Y%02d", 60:69)), c(sprintf("Y%02d", 83:84)), c(sprintf("Y%02d", 40:59))
)

mal_definidas <- c(
  c(sprintf("R%02d", 00:94)), c(sprintf("R%02d", 96:99)),
  "P95", "P969"
)

### Definindo os grupos como lista nomeada
lista_cids_evitaveis <- list(
  imunoprevencao = imunoprevencao,
  mulher_gestacao = mulher_gestacao,
  evitaveis_parto = evitaveis_parto,
  recem_nascido = recem_nascido,
  acoes_diagnostico = acoes_diagnostico,
  acoes_saude = acoes_saude,
  mal_definidas = mal_definidas
)

rm(imunoprevencao, mulher_gestacao, evitaveis_parto, recem_nascido, acoes_diagnostico, acoes_saude, mal_definidas)

## Criando uma função para categorizar CIDs de acordo com os grupos de causas evitáveis
cria_grupo_evitavel <- function(data, cids, prefixo, filtro_obitoparto = NULL, idade_start = NULL, idade_end = NULL) {
  data <- janitor::clean_names(data)

  if (!is.null(filtro_obitoparto)) {
    if (is.na(filtro_obitoparto)) {
      data <- dplyr::filter(data, is.na(obitoparto) | obitoparto == 9)
    } else {
      data <- dplyr::filter(data, obitoparto == filtro_obitoparto)
    }
  }

  if (!is.null(idade_start) & !is.null(idade_end)) {
    if (!is.null(idade_start)) {
      data <- dplyr::filter(data, idade >= idade_start & idade <= idade_end)
    } else {
      data <- dplyr::filter(data, idade <= idade_end)
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
        causabas %in% cids$imunoprevencao | causabas2 %in% cids$imunoprevencao ~ paste0(prefixo, "_imunoprevencao"),
        causabas %in% cids$mulher_gestacao | causabas2 %in% cids$mulher_gestacao ~ paste0(prefixo, "_mulher_gestacao"),
        causabas %in% cids$evitaveis_parto | causabas2 %in% cids$evitaveis_parto ~ paste0(prefixo, "_mulher_parto"),
        causabas %in% cids$recem_nascido | causabas2 %in% cids$recem_nascido ~ paste0(prefixo, "_recem_nascido"),
        causabas %in% cids$acoes_diagnostico | causabas2 %in% cids$acoes_diagnostico ~ paste0(prefixo, "_acoes_diagnostico"),
        causabas %in% cids$acoes_saude | causabas2 %in% cids$acoes_saude ~ paste0(prefixo, "_acoes_saude"),
        causabas %in% cids$mal_definidas | causabas2 %in% cids$mal_definidas ~ paste0(prefixo, "_mal_definidas"),
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
  grupos <- c("imunoprevencao", "mulher_gestacao", "mulher_parto", "recem_nascido", "acoes_diagnostico", "acoes_saude", "mal_definidas", "demais_causas")

  for (grupo in grupos) {
    df <- df |>
      dplyr::mutate(
        !!paste0(prefixo, "_", grupo, "_total") := rowSums(dplyr::across(dplyr::starts_with(paste0(prefixo, "_", grupo))), na.rm = TRUE)
      )
  }

  df <- df |>
    dplyr::mutate(
      !!paste0("obitos_perinatais_totais") := rowSums(dplyr::across(dplyr::starts_with(prefixo) & dplyr::ends_with("_total")), na.rm = TRUE)
    )

  return(df)
}

## Criando os dataframes consolidados
df_evitaveis_perinatal_todos <- cria_grupo_evitavel(rbind(df_obitos_fetais_em_hospital, df_obitos_neonatais_em_hospital), lista_cids_evitaveis, "evitaveis_perinatal") |>
  adiciona_totais_evitaveis("evitaveis_perinatal")
df_evitaveis_perinatal_antes <- cria_grupo_evitavel(df_obitos_fetais_em_hospital, lista_cids_evitaveis, "evitaveis_perinatal_antes", filtro_obitoparto = 1)
df_evitaveis_perinatal_durante <- cria_grupo_evitavel(df_obitos_fetais_em_hospital, lista_cids_evitaveis, "evitaveis_perinatal_durante", filtro_obitoparto = 2)
df_evitaveis_perinatal_sem_info_parto <- cria_grupo_evitavel(df_obitos_fetais_em_hospital, lista_cids_evitaveis, "evitaveis_perinatal_sem_info_parto", filtro_obitoparto = NA)
df_evitaveis_perinatal_0_dias <- cria_grupo_evitavel(df_obitos_neonatais_em_hospital, lista_cids_evitaveis, "evitaveis_perinatal_0_dias", idade_end = 200)
df_evitaveis_perinatal_1_a_6_dias <- cria_grupo_evitavel(df_obitos_neonatais_em_hospital, lista_cids_evitaveis, "evitaveis_perinatal_1_a_6_dias", idade_start = 201, idade_end = 206)

## Unindo todos os dataframes em um só
df_bloco7_perinatal_evitaveis <- list(
  df_evitaveis_perinatal_todos,
  df_evitaveis_perinatal_antes,
  df_evitaveis_perinatal_durante,
  df_evitaveis_perinatal_sem_info_parto,
  df_evitaveis_perinatal_0_dias,
  df_evitaveis_perinatal_1_a_6_dias
) |>
  purrr::reduce(dplyr::full_join) |>
  select(
    codmunocor, codestab, mes, ano,
    starts_with("evitaveis"),
    "obitos_perinatais_totais"
  )

## Salvando a base final
write.csv(df_bloco7_perinatal_evitaveis, "data-raw/csv/df_causas_evitaveis_perinatais_2018_2023.csv", row.names = FALSE)


# Para os indicadores de grupos de causas ---------------------------------------
## Lendo o arquivo com os óbitos fetais em hospital no período de 2018-2023
df_obitos_fetais_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sim_obito_fetal_em_hospital_2018_2023.csv.gz") |>
  select(CODESTAB, CODMUNOCOR, mes, ano, PESO, CAUSABAS, OBITOPARTO, IDADE)

## Lendo o arquivo com os óbitos neonatais em hospital no período de 2018-2023 (e filtrando por IDADE <= 206)
df_obitos_neonatais_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sim_obito_neonatal_em_hospital_2018_2023.csv.gz") |>
  filter(as.numeric(IDADE) <= 206) |>
  select(CODESTAB, CODMUNOCOR, mes, ano, PESO, CAUSABAS, OBITOPARTO, IDADE)

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

rm(
  grupos_prematuridade, grupos_infeccoes, grupos_asfixia, grupos_respiratorias,
  grupos_gravidez, grupos_afeccoes_perinatal, grupos_anomalias, grupos_mal_definidas
)

## Criando uma função para categorizar CIDs de acordo com os grupos de causas principais
cria_grupo_causa <- function(data, lista_cids, prefixo, filtro_obitoparto = NULL, idade_start = NULL, idade_end = NULL) {
  data <- janitor::clean_names(data)

  if (!is.null(filtro_obitoparto)) {
    if (is.na(filtro_obitoparto)) {
      data <- dplyr::filter(data, is.na(obitoparto) | obitoparto == 9)
    } else {
      data <- dplyr::filter(data, obitoparto == filtro_obitoparto)
    }
  }

  if (!is.null(idade_start) & !is.null(idade_end)) {
    if (!is.null(idade_start)) {
      data <- dplyr::filter(data, idade >= idade_start & idade <= idade_end)
    } else {
      data <- dplyr::filter(data, idade <= idade_end)
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
      !!paste0("obitos_perinatais_totais") := rowSums(dplyr::across(dplyr::starts_with(prefixo) & dplyr::ends_with("_total")), na.rm = TRUE)
    )

  return(df)
}

# Gerando os dataframes
df_principais_perinatal_todos <- cria_grupo_causa(rbind(df_obitos_fetais_em_hospital, df_obitos_neonatais_em_hospital), lista_cids_causas_principais, "principais_perinatal") |>
  adiciona_totais_causa("principais_perinatal")
df_principais_perinatal_antes <- cria_grupo_causa(df_obitos_fetais_em_hospital, lista_cids_causas_principais, "principais_perinatal_antes", filtro_obitoparto = 1)
df_principais_perinatal_durante <- cria_grupo_causa(df_obitos_fetais_em_hospital, lista_cids_causas_principais, "principais_perinatal_durante", filtro_obitoparto = 2)
df_principais_perinatal_sem_info_parto <- cria_grupo_causa(df_obitos_fetais_em_hospital, lista_cids_causas_principais, "principais_perinatal_sem_info_parto", filtro_obitoparto = NA)
df_principais_perinatal_0_dias <- cria_grupo_causa(df_obitos_neonatais_em_hospital, lista_cids_causas_principais, "principais_perinatal_0_dias", idade_end = 200)
df_principais_perinatal_1_a_6_dias <- cria_grupo_causa(df_obitos_neonatais_em_hospital, lista_cids_causas_principais, "principais_perinatal_1_a_6_dias", idade_start = 201, idade_end = 206)

# Unindo os dataframes
df_bloco7_principais_perinatal <- list(
  df_principais_perinatal_todos,
  df_principais_perinatal_antes,
  df_principais_perinatal_durante,
  df_principais_perinatal_sem_info_parto,
  df_principais_perinatal_0_dias,
  df_principais_perinatal_1_a_6_dias
) |>
  purrr::reduce(dplyr::full_join) |>
  select(
    codmunocor, codestab, mes, ano,
    starts_with("principais_"),
    "obitos_perinatais_totais"
  )

## Salvando a base final
write.csv(df_bloco7_principais_perinatal, "data-raw/csv/df_causas_principais_perinatais_2018_2023.csv", row.names = FALSE)
