# Carregando os pacotes necessários
library(dplyr)
library(tidyr)
library(janitor)
library(microdatasus)
library(data.table)
library(future)
library(future.apply)

# Baixando os dados do SINASC (com paralelização) -------------------------
## Criando o planejamento dos futures
plan(multisession)

## Criando uma função que baixa os dados do SINASC para um certo ano
processa_ano <- function(ano, variaveis) {

  message("Processando ano ", ano)

  # Carregando os pacotes dentro da worker
  library(microdatasus)
  library(dplyr)
  library(data.table)
  library(stringr)

  # Baixando os dados do SINASC para o dado ano
  df_sinasc_aux <- fetch_datasus(
    year_start = ano,
    year_end = ano,
    vars = c("CODESTAB", "CODMUNNASC", "DTNASC", variaveis),
    information_system = "SINASC"
  )

  ## Criando a variável de ano e mês e transformando todas as variáveis em numéricas
  df_sinasc <- df_sinasc_aux |>
    mutate(
      mes = as.numeric(substr(DTNASC, 3, 4)),
      ano = as.numeric(substr(DTNASC, nchar(DTNASC) - 3, nchar(DTNASC))),
      .keep = "unused",
      .after = "CODMUNNASC"
    ) |>
    clean_names() |>
    mutate_if(is.character, as.numeric)

  ## Retornando o dataframe tratado
  df_sinasc
}

## Criando um vetor com as variáveis de interesse
variaveis <- c(
  "IDADEMAE", "RACACORMAE", "ESCMAE", "ESTCIVMAE", "QTDPARTCES",
  "QTDPARTNOR", "PARTO", "GESTACAO", "SEMAGESTAC", "PESO", "TPROBSON",
  "CONSPRENAT", "MESPRENAT", "IDANOMAL", "CODANOMAL", "STCESPARTO", "STTRABPART",
  "GRAVIDEZ", "TPNASCASSI", "APGAR5"
)

## Criando um vetor com os anos a serem baixados
anos <- 2018:2023

## Baixando todos os dados
resultados <- future_lapply(anos, processa_ano, variaveis)

## Obtendo a base final
df_sinasc <- rbindlist(resultados, fill = TRUE)


# Checando quais os possíveis valores incompletos para cada variável ------
## Perfil sociodemográfico das mulheres atendidas
### Para IDADEMAE
sort(unique(df_sinasc$idademae), na.last = FALSE)  # NA e 99

### Para RACACORMAE
sort(unique(df_sinasc$racacormae), na.last = FALSE)  # NA

### Para ESCMAE
sort(unique(df_sinasc$escmae), na.last = FALSE)  # NA e 9

### Para ESTCIVMAE
sort(unique(df_sinasc$estcivmae), na.last = FALSE)  # NA e 9


## Perfil obstétrico das mulheres atendidas
### Para QTDPARTNOR
sort(unique(df_sinasc$qtdpartnor), na.last = FALSE)  # NA e 99
table(df_sinasc$qtdpartnor)

### Para QTDPARTCES
sort(unique(df_sinasc$qtdpartces), na.last = FALSE)  # NA e 99
table(df_sinasc$qtdpartces)

### Para GRAVIDEZ
sort(unique(df_sinasc$gravidez), na.last = FALSE)  # NA e 9

### Para MESPRENAT
sort(unique(df_sinasc$mesprenat), na.last = FALSE)  # NA e 99

### Para CONSPRENAT
sort(unique(df_sinasc$consprenat), na.last = FALSE)  # NA e 99
table(df_sinasc$consprenat, useNA = "ifany")

### Para GESTACAO
sort(unique(df_sinasc$gestacao), na.last = FALSE)  # NA e 9

### Para SEMAGESTAC
sort(unique(df_sinasc$semagestac), na.last = FALSE)  # NA


## Indicadores da assistência ao parto
### Para PARTO
sort(unique(df_sinasc$parto), na.last = FALSE)  # NA e 9

## Para TPROBSON
sort(unique(df_sinasc$tprobson), na.last = FALSE)  # 11

## Para STCESPARTO
sort(unique(df_sinasc$stcesparto), na.last = FALSE)  # NA e 9

## Para STTRABPART
sort(unique(df_sinasc$sttrabpart), na.last = FALSE)  # NA e 9

## Para TPNASCASSI
sort(unique(df_sinasc$tpnascassi), na.last = FALSE)  # NA e 9


## Perfil do nascimento
### Para PESO
sort(unique(df_sinasc$peso), na.last = FALSE)  # NA
sort(unique(df_sinasc$peso), decreasing = TRUE)

### Para IDANOMAL
sort(unique(df_sinasc$idanomal), na.last = FALSE)  # NA e 9

### Para APGAR5
sort(unique(df_sinasc$apgar5), na.last = FALSE)  # NA e 99


# Criando as variáveis de incompletude ------------------------------------
## Definindo uma lista de valores que representam informação ignorada" para cada variável
codigos_ignorados <- list(
  idademae = c(NA, 99),
  racacormae = c(NA),
  escmae = c(NA, 9),
  estcivmae = c(NA, 9),
  qtdpartnor = c(NA, 99),
  qtdpartces = c(NA, 99),
  gravidez = c(NA, 9),
  mesprenat = c(NA, 99),
  consprenat = c(NA, 99),
  gestacao = c(NA, 9),
  semagestac = c(NA),
  parto = c(NA, 9),
  tprobson = c(11),
  stcesparto = c(NA, 9),
  sttrabpart = c(NA, 9),
  tpnascassi = c(NA, 9),
  idanomal = c(NA, 9),
  apgar5 = c(NA, 99),
  peso = c(NA)
)

## Lendo a base auxiliar de CNES
df_cnes_aux <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_aux.csv")

## Criando as variáveis
df_incompletude_sinasc <- df_sinasc |>
  mutate(
    total_de_nascidos_vivos = 1,
    nv_sem_anomalia_2500g_mais = if_else(
      peso >= 2500 &
        ((idanomal == 2) | ((idanomal == '' | is.na(idanomal)) & (codanomal == '' | is.na(codanomal)))),
      1, 0 , missing = 0
    ),
    # AQUI ESTÁ A ADAPTAÇÃO:
    across(
      .cols = all_of(names(codigos_ignorados)),
      .fns = ~ {
        # 1. Verifica se o valor está na lista de ignorados ou é NA
        condicao_ignorado <- .x %in% codigos_ignorados[[cur_column()]] | is.na(.x)

        # # 2. Se for a coluna idademae, adiciona a regra do > 55
        # if (cur_column() == "idademae") {
        #   condicao_ignorado <- condicao_ignorado | (.x > 55)
        # }

        if_else(condicao_ignorado, 1, 0)
      },
      .names = "{.col}_sinasc_incompletos"
    )
  ) |>
  mutate(
    # Uniões de algumas variáveis usando pmax (se qualquer uma for 1, o resultado é 1)
    parto_qtdpartces_sinasc_incompletos = pmax(parto_sinasc_incompletos, qtdpartces_sinasc_incompletos),

    qtdpartces_qtdpartnor_sinasc_incompletos = pmax(qtdpartces_sinasc_incompletos, qtdpartnor_sinasc_incompletos),

    semagestac_consprenat_sinasc_incompletos = pmax(semagestac_sinasc_incompletos, consprenat_sinasc_incompletos),

    tprobson_sttrabpart_sinasc_incompletos = pmax(tprobson_sinasc_incompletos, sttrabpart_sinasc_incompletos),

    tprobson_stcesparto_sinasc_incompletos = pmax(tprobson_sinasc_incompletos, stcesparto_sinasc_incompletos),

    tprobson_parto_sinasc_incompletos = pmax(tprobson_sinasc_incompletos, parto_sinasc_incompletos),

    tprobson_sttrabpart_parto_sinasc_incompletos = pmax(tprobson_sinasc_incompletos, sttrabpart_sinasc_incompletos, parto_sinasc_incompletos),

    tprobson_stcesparto_parto_sinasc_incompletos = pmax(tprobson_sinasc_incompletos, stcesparto_sinasc_incompletos, parto_sinasc_incompletos),

    apgar_peso_idanomal_sinasc_incompletos = pmax(apgar5_sinasc_incompletos, peso_sinasc_incompletos, idanomal_sinasc_incompletos)
  ) |>
  group_by(codestab, codmunnasc, mes, ano) |>
  summarise(
    total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
    nv_sem_anomalia_2500g_mais = sum(nv_sem_anomalia_2500g_mais),
    across(ends_with("_sinasc_incompletos"), sum),
    .groups = "drop"
  ) |>
  right_join(df_cnes_aux, by = c("codestab" = "cnes", "codmunnasc" = "codufmun", "mes", "ano")) |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) |>
  arrange(codestab, codmunnasc, ano, mes)


## Exportando os dados
write.csv(df_incompletude_sinasc, 'data-raw/csv/df_incompletude_sinasc_2018-2023.csv', row.names = FALSE)
