# Carregando os pacotes necessários
library(dplyr)
library(tidyr)
library(janitor)
library(microdatasus)
library(data.table)
library(future)
library(future.apply)
library(purrr)

# Baixando os dados do SIM (com paralelização) -------------------------
## Criando o planejamento dos futures
plan(multisession)

## Criando uma função que baixa os dados do SIM para um certo ano
processa_ano <- function(ano, variaveis) {

  message("Processando ano ", ano)

  # Carregando os pacotes dentro da worker
  library(microdatasus)
  library(dplyr)
  library(data.table)
  library(stringr)

  # Baixando os dados do SIM para o dado ano
  df_sim_aux <- fetch_datasus(
    year_start = ano,
    year_end = ano,
    vars = c("CODESTAB", "CODMUNOCOR", "DTOBITO", variaveis),
    information_system = "SIM-DO"
  )

  # Baixando os dados do SIM-DOFET para o dado ano
  df_sim_dofet_aux <- fetch_datasus(
    year_start = ano,
    year_end = ano,
    vars = c("CODESTAB", "CODMUNOCOR", "DTOBITO", variaveis),
    information_system = "SIM-DOFET"
  )

  ## Criando a variável de ano e mês e transformando todas as variáveis em numéricas
  df_sim <- df_sim_aux |>
    mutate(
      mes = as.numeric(substr(DTOBITO, 3, 4)),
      ano = as.numeric(substr(DTOBITO, nchar(DTOBITO) - 3, nchar(DTOBITO))),
      .keep = "unused",
      .after = "CODMUNOCOR"
    ) |>
    clean_names() |>
    mutate_if(is.character, as.numeric)

  df_sim_dofet <- df_sim_dofet_aux |>
    mutate(
      mes = as.numeric(substr(DTOBITO, 3, 4)),
      ano = as.numeric(substr(DTOBITO, nchar(DTOBITO) - 3, nchar(DTOBITO))),
      .keep = "unused",
      .after = "CODMUNOCOR"
    ) |>
    clean_names() |>
    mutate_if(is.character, as.numeric)

  ## Retornando o dataframe tratado
  list(
    df_sim = df_sim,
    df_sim_dofet = df_sim_dofet
  )
}

## Criando um vetor com as variáveis de interesse
variaveis <- c(
  "IDADE", "TIPOBITO", "PESO", "SEMAGESTAC", "OBITOPARTO"
)

## Criando um vetor com os anos a serem baixados
anos <- 2018:2023

## Baixando todos os dados
resultados <- future_lapply(anos, processa_ano, variaveis)

## Obtendo as bases finais
df_sim <- rbindlist(lapply(resultados, `[[`, "df_sim"), fill = TRUE)
df_sim_dofet <- rbindlist(lapply(resultados, `[[`, "df_sim_dofet"), fill = TRUE)

# Checando quais os possíveis valores incompletos para cada variável ------
## Para SEMAGESTAC
sort(unique(df_sim$semagestac), na.last = FALSE)  # NA e 99
table(df_sim$semagestac, useNA = "ifany")

## Para PESO
sort(unique(df_sim$peso), na.last = FALSE)  # NA
sort(table(df_sim$peso, useNA = "ifany"), decreasing = TRUE)

## Para OBITOPARTO
sort(unique(df_sim$obitoparto), na.last = FALSE)  # NA e 9
sort(table(df_sim$obitoparto, useNA = "ifany"))


# Criando as variáveis de incompletude ------------------------------------
## Definindo uma lista de valores que representam informação ignorada" para cada variável
codigos_ignorados <- list(
  semagestac = c(NA, 99),
  peso = c(NA),
  obitoparto = c(NA, 9)
)

## Lendo a base auxiliar de CNES
df_cnes_aux <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_aux.csv")

## Criando uma base auxiliar com indicadoras de óbito fetal/neonatal
df_aux_incompletude_fetal <- df_sim_dofet |>
  mutate(
    total_de_obitos_fetais = if_else(tipobito == 1, 1, 0, missing = 0)
  )

df_aux_incompletude_neonatal <- df_sim |>
  mutate(
    total_de_obitos_neonatais = if_else(as.numeric(idade) < 228, 1, 0, missing = 0)
  )

## Criando as variáveis de incompletude
### Para os óbitos fetais
df_incompletude_fetal <- df_aux_incompletude_fetal |>
  filter(total_de_obitos_fetais == 1) |>
  mutate(
    across(
      .cols = all_of(names(codigos_ignorados)),
      .fns = ~ if_else(.x %in% codigos_ignorados[[cur_column()]] | is.na(.x), 1, 0),
      .names = "{.col}_sim_fetal_incompletos"
    )
  ) |>
  mutate(
    # Uniões de algumas variáveis usando pmax (se qualquer uma for 1, o resultado é 1)
    peso_semagestac_sim_fetal_incompletos = pmax(peso_sim_fetal_incompletos, semagestac_sim_fetal_incompletos),

    peso_semagestac_obitoparto_sim_fetal_incompletos = pmax(peso_sim_fetal_incompletos, semagestac_sim_fetal_incompletos, obitoparto_sim_fetal_incompletos)

  ) |>
  group_by(codestab, codmunocor, mes, ano) |>
  summarise(
    total_de_obitos_fetais = sum(total_de_obitos_fetais),
    across(ends_with("_sim_fetal_incompletos"), sum),
    .groups = "drop"
  ) |>
  right_join(df_cnes_aux, by = c("codestab" = "cnes", "codmunocor" = "codufmun", "mes", "ano")) |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) |>
  arrange(codestab, codmunocor, ano, mes)

### Para os óbitos neonatais
df_incompletude_neonatal <- df_aux_incompletude_neonatal |>
  filter(total_de_obitos_neonatais == 1) |>
  mutate(
    across(
      .cols = all_of(names(codigos_ignorados)),
      .fns = ~ if_else(.x %in% codigos_ignorados[[cur_column()]] | is.na(.x), 1, 0),
      .names = "{.col}_sim_neonatal_incompletos"
    )
  ) |>
  mutate(
    # Uniões de algumas variáveis usando pmax (se qualquer uma for 1, o resultado é 1)
    peso_semagestac_sim_neonatal_incompletos = pmax(peso_sim_neonatal_incompletos, semagestac_sim_neonatal_incompletos),

    peso_semagestac_obitoparto_sim_neonatal_incompletos = pmax(peso_sim_neonatal_incompletos, semagestac_sim_neonatal_incompletos, obitoparto_sim_neonatal_incompletos)

  ) |>
  group_by(codestab, codmunocor, mes, ano) |>
  summarise(
    total_de_obitos_neonatais = sum(total_de_obitos_neonatais),
    across(ends_with("_sim_neonatal_incompletos"), sum),
    .groups = "drop"
  ) |>
  right_join(df_cnes_aux, by = c("codestab" = "cnes", "codmunocor" = "codufmun", "mes", "ano")) |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) |>
  arrange(codestab, codmunocor, ano, mes)

### Para os óbitos perinatais
# 1. Preparando os dados Neonatais Precoces (0-6 dias)
df_aux_neonatal_precoce <- df_aux_incompletude_neonatal |>
  # Filtra apenas óbitos de 0 a 6 dias (conforme padrão SIM: 200 a 206)
  filter(as.numeric(idade) <= 206) |>
  mutate(
    across(
      .cols = all_of(names(codigos_ignorados)),
      .fns = ~ if_else(.x %in% codigos_ignorados[[cur_column()]] | is.na(.x), 1, 0),
      .names = "{.col}_sim_neonatal_precoce_incompletos"
    )
  ) |>
  mutate(
    peso_semagestac_sim_neonatal_precoce_incompletos = pmax(peso_sim_neonatal_precoce_incompletos, semagestac_sim_neonatal_precoce_incompletos),
    peso_semagestac_obitoparto_sim_neonatal_precoce_incompletos = pmax(peso_sim_neonatal_precoce_incompletos, semagestac_sim_neonatal_precoce_incompletos, obitoparto_sim_neonatal_precoce_incompletos)
  ) |>
  group_by(codestab, codmunocor, mes, ano) |>
  summarise(
    total_de_obitos_neonatais_precoces = sum(total_de_obitos_neonatais),
    across(ends_with("_sim_neonatal_precoce_incompletos"), sum),
    .groups = "drop"
  )

# 2. Unindo com os Fetais e calculando o Perinatal
df_incompletude_perinatal <- df_incompletude_fetal |>
  # Selecionamos as colunas de interesse do fetal (já resumidas por estabelecimento/mês/ano)
  select(codestab, codmunocor, mes, ano,
         total_de_obitos_fetais,
         ends_with("_sim_fetal_incompletos")) |>
  # Unimos com os neonatais precoces
  full_join(df_aux_neonatal_precoce, by = c("codestab", "codmunocor", "mes", "ano")) |>
  # Substituímos NAs por 0 para permitir a soma
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) |>
  # Calculamos as variáveis Perinatais
  mutate(
    total_de_obitos_perinatais = total_de_obitos_fetais + total_de_obitos_neonatais_precoces,

    # Incompletude por variável individual
    semagestac_sim_perinatal_incompletos = semagestac_sim_fetal_incompletos + semagestac_sim_neonatal_precoce_incompletos,
    peso_sim_perinatal_incompletos = peso_sim_fetal_incompletos + peso_sim_neonatal_precoce_incompletos,
    obitoparto_sim_perinatal_incompletos = obitoparto_sim_fetal_incompletos + obitoparto_sim_neonatal_precoce_incompletos,

    # Incompletude composta (Peso + Semagestac)
    peso_semagestac_sim_perinatal_incompletos = peso_semagestac_sim_fetal_incompletos + peso_semagestac_sim_neonatal_precoce_incompletos,

    # Incompletude composta (Peso + Semagestac + ObitoParto)
    peso_semagestac_obitoparto_sim_perinatal_incompletos = peso_semagestac_obitoparto_sim_fetal_incompletos + peso_semagestac_obitoparto_sim_neonatal_precoce_incompletos
  ) |>
  # Mantendo apenas as colunas perinatais finais e as chaves
  select(
    codestab, codmunocor, mes, ano,
    total_de_obitos_perinatais,
    ends_with("_sim_perinatal_incompletos")
  ) |>
  # Garante que todos os estabelecimentos do CNES original apareçam
  right_join(df_cnes_aux, by = c("codestab" = "cnes", "codmunocor" = "codufmun", "mes", "ano")) |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) |>
  arrange(codestab, codmunocor, ano, mes)

### Juntando todas as bases
df_incompletude_sim <- reduce(
  list(
    df_incompletude_fetal,
    df_incompletude_neonatal,
    df_incompletude_perinatal
  ),
  left_join
)

## Exportando os dados
write.csv(df_incompletude_sim, 'data-raw/csv/df_incompletude_sim_2018-2023.csv', row.names = FALSE)
