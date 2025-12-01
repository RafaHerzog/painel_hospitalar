library(dplyr)
library(janitor)
library(data.table)
library(stringr)
library(tidyr)
library(future)
library(future.apply)

# Baixando dados do SIM de 2016-2017 (para o cálculo das médias móveis) -------
## Criando o planejamento dos futures
plan(multisession)

## Criando uma função que baixa os dados necessários para um certo ano
processa_ano <- function(ano) {
  # Carrega os pacotes dentro da worker
  library(microdatasus)
  library(dplyr)
  library(data.table)
  library(stringr)

  # Criando uma função para criar as coluna de "ano" e "mes" em bases do SIM e SINASC
  extrai_mes <- function(data, n = 4) {
    as.numeric(substr(data, 3, 4))
  }

  extrai_ano <- function(data, n = 4) {
    as.numeric(substr(data, nchar(data) - n + 1, nchar(data)))
  }

  # Criando uma função genérica para baixar dados pelo microdatasus
  baixa_dados <- function(ano, sistema, local_col, data_col) {
    microdatasus::fetch_datasus(
      year_start = ano,
      year_end = ano,
      information_system = sistema
    ) |>
      dplyr::filter(.data[[local_col]] == "1") |>
      dplyr::mutate(
        mes = extrai_mes(.data[[data_col]]),
        ano = extrai_ano(.data[[data_col]])
      )
  }

  message("Processando ano ", ano)

  # Baixando os dados do SIM-DOMAT
  list(
    domat = baixa_dados(ano, "SIM-DOMAT", "LOCOCOR", "DTOBITO")
  )
}

## Criando um vetor com os anos a serem baixados
anos <- 2016:2017

## Baixando todos os dados
resultados <- future_lapply(anos, processa_ano)

## Separando-os em objetos diferentes
df_domat_mm <- rbindlist(lapply(resultados, `[[`, "domat"), fill = TRUE)

## Manipulando a base auxiliar de CNES
df_cnes_aux_mm <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_aux.csv") |>
  group_by(cnes, codufmun, nome_fantasia, municipio, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude) |>
  summarise(
    tipo = first(tipo),
    categoria_porte = first(categoria_porte)
  ) |>
  ungroup()

## Criando variáveis necessárias para o cálculo das médias móveis
df_indicadores_sim_mm <- df_domat_mm |>
  clean_names() |>
  filter(codestab %in% df_cnes_aux_mm$cnes) |>
  # Criando as variáveis necessárias para o cálculo dos indicadores
  mutate(
    codestab = as.numeric(codestab),
    codmunocor = as.numeric(codmunocor),

    obitos_maternos_totais = 1,

    # Óbitos maternos diretos
    obitos_maternos_diretos = if_else(
      !((causabas >= "B200" & causabas <= "B249") |
          (causabas >= "O100" & causabas <= "O109") |
          ((causabas >= "O240" & causabas != "O244") & causabas <= "O259") |
          (causabas == "O94") |
          (causabas >= "O980" & causabas <= "O999")) & !(causabas == "O95"),
      1, 0, missing = 0
    ),

    # Óbitos maternos diretos por causas específicas
    obitos_maternos_diretos_aborto = if_else(
      obitos_maternos_diretos == 1 & (causabas >= "O030"  &  causabas <= "O079"),
      1, 0, missing = 0
    ),
    obitos_maternos_diretos_hipertensao = if_else(
      obitos_maternos_diretos == 1 & (causabas == "O11" | (causabas >= "O13" & causabas <= "O16")),
      1, 0, missing = 0
    ),
    obitos_maternos_diretos_hemorragia = if_else(
      obitos_maternos_diretos == 1 & ((causabas >= "O200" & causabas <= "O209") |
                                        (causabas >= "O440" & causabas <= "O469") |
                                        (causabas >= "O670" & causabas <= "O679") |
                                        (causabas >= "O710" & causabas <= "O711") |
                                        (causabas >= "O720" & causabas <= "O723")),
      1, 0, missing = 0
    ),
    obitos_maternos_diretos_infec_puerperal = if_else(
      obitos_maternos_diretos == 1 & (causabas >= "O85" & causabas <= "O868"),
      1, 0, missing = 0
    ),

    # Óbitos maternos indiretos
    obitos_maternos_indiretos = if_else(
      (causabas >= "B200" & causabas <= "B249") |
        (causabas >= "O100" & causabas <= "O109") |
        ((causabas >= "O240" & causabas != "O244") & causabas <= "O259") |
        (causabas == "O94") |
        (causabas >= "O980" & causabas <= "O999"),
      1, 0, missing = 0
    ),

    # Óbitos maternos indiretos por causas específicas
    obitos_maternos_indiretos_aids = if_else(
      obitos_maternos_indiretos == 1 & (causabas >= "B200" & causabas <= "B249"),
      1, 0, missing = 0
    ),
    obitos_maternos_indiretos_circulatorio = if_else(obitos_maternos_indiretos == 1 & causabas == "O994", 1, 0, missing = 0),
    obitos_maternos_indiretos_respiratorio = if_else(obitos_maternos_indiretos == 1 & causabas == "O995", 1, 0, missing = 0),
    obitos_maternos_indiretos_infecciosas = if_else(
      obitos_maternos_indiretos == 1 & (causabas >= "O980" & causabas <= "O989"),
      1, 0, missing = 0
    ),

    # Óbitos maternos por causas não especificadas
    obitos_maternos_nao_especificados = if_else(causabas == "O95", 1, 0, missing = 0)
  ) |>
  group_by(codestab, codmunocor, mes, ano) |>
  summarise_at(vars(starts_with("obitos")), sum) |>
  ungroup() |>
  # Juntando com a base auxiliar de CNES
  left_join(df_cnes_aux_mm, by = join_by(codestab == cnes, codmunocor == codufmun)) |>
  rename(cnes = codestab) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  arrange(cnes, codmunocor, nome_fantasia, mes, ano)


# Preparando a base de indicadores do SIM-DOMAT para o painel -------------------
## Lendo o arquivo com os óbitos maternos em hospital no período de 2018-2023
df_sim_obitos_maternos_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sim_obito_materno_em_hospital_2018_2023.csv.gz")

## Lendo o arquivo com os nascimentos em hospital no período de 2018-2023
df_sinasc_nasc_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sinasc_nasc_em_hospital_2018_2023.csv.gz")

## Lendo a base auxiliar de CNES
df_cnes_aux <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_aux.csv")

## Para os dados do SINASC, criando a variável com o total de nascidos vivos
df_indicadores_sinasc <- df_sinasc_nasc_em_hospital |>
  clean_names() |>
  mutate(
    total_de_nascidos_vivos = 1
  ) |>
  group_by(codestab, codmunnasc, mes, ano) |>
  summarise_at(vars(starts_with("total_")), sum) |>
  ungroup()

## Para os dados do SIM, fazendo as manipulações necessárias e criando as variáveis de interesse
df_indicadores_maternos <- df_sim_obitos_maternos_em_hospital |>
  clean_names() |>
  # Criando as variáveis necessárias para o cálculo dos indicadores
  mutate(
    obitos_maternos_totais = 1,

    # Óbitos maternos diretos
    obitos_maternos_diretos = if_else(
      !((causabas >= "B200" & causabas <= "B249") |
          (causabas >= "O100" & causabas <= "O109") |
          ((causabas >= "O240" & causabas != "O244") & causabas <= "O259") |
          (causabas == "O94") |
          (causabas >= "O980" & causabas <= "O999")) & !(causabas == "O95"),
      1, 0, missing = 0
    ),

    # Óbitos maternos diretos por causas específicas
    obitos_maternos_diretos_aborto = if_else(
      obitos_maternos_diretos == 1 & (causabas >= "O030"  &  causabas <= "O079"),
      1, 0, missing = 0
    ),
    obitos_maternos_diretos_hipertensao = if_else(
      obitos_maternos_diretos == 1 & (causabas == "O11" | (causabas >= "O13" & causabas <= "O16")),
      1, 0, missing = 0
    ),
    obitos_maternos_diretos_hemorragia = if_else(
      obitos_maternos_diretos == 1 & ((causabas >= "O200" & causabas <= "O209") |
                                   (causabas >= "O440" & causabas <= "O469") |
                                   (causabas >= "O670" & causabas <= "O679") |
                                   (causabas >= "O710" & causabas <= "O711") |
                                   (causabas >= "O720" & causabas <= "O723")),
      1, 0, missing = 0
    ),
    obitos_maternos_diretos_infec_puerperal = if_else(
      obitos_maternos_diretos == 1 & (causabas >= "O85" & causabas <= "O868"),
      1, 0, missing = 0
    ),

    # Óbitos maternos indiretos
    obitos_maternos_indiretos = if_else(
      (causabas >= "B200" & causabas <= "B249") |
        (causabas >= "O100" & causabas <= "O109") |
        ((causabas >= "O240" & causabas != "O244") & causabas <= "O259") |
        (causabas == "O94") |
        (causabas >= "O980" & causabas <= "O999"),
      1, 0, missing = 0
    ),

    # Óbitos maternos indiretos por causas específicas
    obitos_maternos_indiretos_aids = if_else(
      obitos_maternos_indiretos == 1 & (causabas >= "B200" & causabas <= "B249"),
      1, 0, missing = 0
    ),
    obitos_maternos_indiretos_circulatorio = if_else(obitos_maternos_indiretos == 1 & causabas == "O994", 1, 0, missing = 0),
    obitos_maternos_indiretos_respiratorio = if_else(obitos_maternos_indiretos == 1 & causabas == "O995", 1, 0, missing = 0),
    obitos_maternos_indiretos_infecciosas = if_else(
      obitos_maternos_indiretos == 1 & (causabas >= "O980" & causabas <= "O989"),
      1, 0, missing = 0
    ),

    # Óbitos maternos por causas não especificadas
    obitos_maternos_nao_especificados = if_else(causabas == "O95", 1, 0, missing = 0)
  ) |>
  group_by(codestab, codmunocor, mes, ano) |>
  summarise_at(vars(starts_with("obitos")), sum) |>
  ungroup()

## Juntando com a base auxiliar de CNES
df_indicadores_maternos_final <- left_join(
  df_cnes_aux,
  df_indicadores_maternos,
  by = join_by(cnes == codestab, codufmun == codmunocor, mes, ano)
) |>
  rename(codmunocor = codufmun) |>
  # Juntando com a base contendo as variáveis do SINASC
  left_join(df_indicadores_sinasc, by = join_by(cnes == codestab, codmunocor == codmunnasc, mes, ano)) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  bind_rows(df_indicadores_sim_mm) |>
  arrange(cnes, codmunocor, mes, ano)

## Salvando a base final
write.csv(df_indicadores_maternos_final, "data-raw/csv/df_indicadores_mortalidade_materna_2018_2023.csv", row.names = FALSE)
