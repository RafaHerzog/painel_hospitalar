library(janitor)
library(RSQLite)
library(glue)
library(tidyr)
library(dplyr)
library(data.table)
library(readr)
library(readxl)
library(microdatasus)

# Para o indicador de condições ameaçadoras ------------------------------
## Lendo o arquivo com os nascimentos em hospital no período de 2018-2023
df_sinasc_nasc_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sinasc_nasc_em_hospital_2018_2023.csv.gz")

df_ameacadoras <- df_sinasc_nasc_em_hospital |>
  mutate(
    PESO = as.numeric(PESO),
    GESTACAO = as.numeric(GESTACAO),
    SEMAGESTAC = as.numeric(SEMAGESTAC),
    APGAR5 = as.numeric(APGAR5),
    .keep = "unused",
  ) |>
  mutate(
    total_de_nascidos_vivos = 1,
    nascidos_condicoes_ameacadoras = if_else(PESO < 1500 | (GESTACAO < 4 | SEMAGESTAC < 32) | APGAR5 < 7, 1, 0, missing = 0),
    .keep = "unused"
  ) |>
  clean_names() |>
  group_by(codmunnasc, codestab, mes, ano) |>
  summarise_at(vars(contains("nascidos")), sum) |>
  ungroup() |>
  rename(codmunocor = codmunnasc, cnes = codestab)

# Para os indicadores provenientes do SIH ---------------------------------
## Criando um vetor com os anos considerados
anos <- c(2018:2023)

## Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO")

procedimentos_parto <- c("0310010012", "0310010039", "0310010047",
                         "0310010055", "0411010026", "0411010034",
                         "0411010042")

## Baixando os dados do SIH-RD
for (estado in estados) {
  # Criando data.frames que guardarão as bases do estado
  df_sih_rd_menores_28_uf <- data.frame()
  df_sih_rd_partos_uf <- data.frame()

  for (ano in anos) {

    erro_rd <- TRUE
    while (erro_rd) {
      erro_rd <- tryCatch({
        # Baixando os dados do SIH-RD para o dado ano e UF
        df_sih_rd_aux <- fetch_datasus(
          year_start = ano,
          year_end = ano,
          uf = estado,
          month_start = 1,
          month_end = 12,
          information_system = "SIH-RD",
          timeout = 500,
          stop_on_error = TRUE,
          vars = c(
            "CNES", "CEP", "MUNIC_RES", "MUNIC_MOV", "MES_CMPT", "ANO_CMPT", "COD_IDADE", "IDADE", "NASC",
            "DT_INTER", "DT_SAIDA", "COBRANCA", "N_AIH", "DIAG_PRINC", "PROC_REA",
            "US_TOT", "UTI_MES_TO"
          )
        )

        # Criando um data.frame que contém apenas as internações de menores de 28 dias
        df_sih_rd_aux_menores_28 <- df_sih_rd_aux |>
          mutate(idade_dias = as.numeric(as.Date(DT_INTER, format = "%Y%m%d") - as.Date(NASC, format = "%Y%m%d"))) |>
          dplyr::filter(
            idade_dias < 28
          )

        # Criando um data.frame que contém apenas os partos
        df_sih_rd_aux_partos <- df_sih_rd_aux |>
          dplyr::filter(
            ((DIAG_PRINC >= "O32" & DIAG_PRINC <= "O36") | (DIAG_PRINC >= "O60" & DIAG_PRINC <= "O69") |
               (DIAG_PRINC >= "O75" & DIAG_PRINC < "O76") | (DIAG_PRINC >= "O80" & DIAG_PRINC <= "O84") |
               DIAG_PRINC == "P95") | (PROC_REA %in% procedimentos_parto)
          )

        erro_rd <- FALSE
      },
      warning = function(cond) return(TRUE)
      )
    }

    # Juntando com os dados dos anos anteriores para a dada UF
    df_sih_rd_menores_28_uf <- bind_rows(df_sih_rd_menores_28_uf, df_sih_rd_aux_menores_28)
    df_sih_rd_partos_uf <- bind_rows(df_sih_rd_partos_uf, df_sih_rd_aux_partos)


    # Limpando a memória
    rm(df_sih_rd_aux_menores_28,
       df_sih_rd_aux_partos)
    gc()
  }

  # Salvando as bases da UF
  write.csv2(
    df_sih_rd_menores_28_uf,
    gzfile(glue("data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_menores_28_dias_{anos[1]}_{anos[length(anos)]}.csv.gz")),
    row.names = FALSE
  )

  write.csv2(
    df_sih_rd_partos_uf,
    gzfile(glue("data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_partos_{anos[1]}_{anos[length(anos)]}.csv.gz")),
    row.names = FALSE
  )

  # Limpando a memória
  rm(df_sih_rd_uf)
  gc()
}

### Criando os data.frames que guardarão as bases finais
df_sih_rd_menores_28 <- data.frame()
df_sih_rd_partos <- data.frame()

for (estado in estados) {
  df_sih_rd_menores_28_aux <- fread(
    glue("data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_menores_28_dias_2018_2023.csv.gz"),
    sep = ";"
  )
  df_sih_rd_menores_28 <- bind_rows(df_sih_rd_menores_28, df_sih_rd_menores_28_aux)

  rm(df_sih_rd_menores_28_aux)
  gc()
}

for (estado in estados) {
  df_sih_rd_partos_aux <- fread(
    glue("data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_partos_2018_2023.csv.gz"),
    sep = ";"
  )
  df_sih_rd_partos <- bind_rows(df_sih_rd_partos, df_sih_rd_partos_aux)

  rm(df_sih_rd_partos_aux)
  gc()
}

## Salvando as bases completas
write.csv2(
  df_sih_rd_menores_28,
  glue("data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_menores_28_dias_{anos[1]}_{anos[length(anos)]}.csv"),
  row.names = FALSE
)

write.csv2(
  df_sih_rd_partos,
  glue("data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_partos_{anos[1]}_{anos[length(anos)]}.csv"),
  row.names = FALSE
)


## Para os numeradores dos indicadores (número de internações/internações em UTI em menores de 28 dias) ----
### Lendo a base auxiliar de CNES
df_cnes_aux <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_aux.csv") |>
  select(cnes:nome_fantasia, mes, ano)

### Lendo uma base com informações auxiliares dos municípios
df_infos_municipios <- read.csv("data-raw/extracao-dos-dados/databases_auxiliares/df_aux_municipios.csv") |>
  mutate_if(is.numeric, as.character)

### Rodando o algoritmo da Claudia na base completa de internações em menores de 28 dias
#### Criando um vetor que contém o diretório original do projeto
diretorio_original <- getwd()

#### Criando um vetor que contém o diretório das bases brutas do SIH-RD
diretorio_bases_brutas <- glue("{getwd()}/data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/SIH")

#### Mudando o diretório para a pasta que contém o algoritmo em C++
setwd("data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/")

#### Rodando o algoritmo em C++ na base de internações
system(glue("./processaih {diretorio_bases_brutas}/BR_sih_rd_menores_28_dias_2018_2023.csv"))

#### Voltando para o diretório original do projeto
setwd(diretorio_original)

#### Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
con <- dbConnect(SQLite(), "data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite")

#### Selecionando a tabela "aih" com todas as suas variáveis, ordenadas por AIHREF e DT_INTER
df_aih_internacoes_aux <- dbGetQuery(con, "select * from aih order by AIHREF, DT_INTER")
dbDisconnect(con)

### Adicionando variáveis que estão no SIH-RD, mas que não são devolvidas na base gerada pelo algoritmo
df_aih_internacoes <- left_join(
  df_aih_internacoes_aux,
  df_sih_rd_menores_28 |>
    select(CNES, MES_CMPT, ANO_CMPT, DT_INTER, DT_SAIDA, N_AIH, MUNIC_MOV, idade_dias) |>
    mutate_at(vars(c(CNES, DT_INTER, DT_SAIDA, N_AIH, MUNIC_MOV)), as.character)
)

### Passando os casos para o formato wide (cada linha corresponderá a uma pessoa única)
df_aih_internacoes_wide <- df_aih_internacoes |>
  mutate(
    DT_INTER = as.Date(DT_INTER, format = "%Y%m%d"),
    DT_SAIDA = as.Date(DT_SAIDA, format = "%Y%m%d"),
    NASC = as.Date(NASC, format = "%Y%m%d")
  ) |>
  group_by(AIHREF) |>  # Agrupando pelo N_AIH comum para todas as linhas de um episódio de cuidado completo
  summarise(
    MES_CMPT = last(MES_CMPT),
    ANO_CMPT = last(ANO_CMPT),  # Ano de processamento do SIH da última internação
    CNES = first(CNES),  # CNES do estabelecimento da primeira internação
    MUNIC_RES = first(MUNIC_RES),  # Município de residência da primeira internação
    MUNIC_MOV = first(MUNIC_MOV),  # Município do estabelecimento da primeira internação
    idade_dias = first(idade_dias),  # Idade, em dias, na data da primeira internação
    SOMA_UTI = sum(as.integer(UTI_MES_TO)),  # Total de dias na UTI
    PDIAG = first(DIAG_PRINC),  # Diagnóstico principal da primeira internação
    PPROC = first(PROC_REA),
    COBRANCA = last(COBRANCA)
  ) |>
  ungroup() |>
  select(mes = MES_CMPT, ano = ANO_CMPT, codmunres = MUNIC_RES, causabas = PDIAG, codmunocor = MUNIC_MOV, cnes = CNES, aihref = AIHREF, idade_dias, soma_uti_mes_to = SOMA_UTI, cobranca = COBRANCA) |>
  # Filtrando apenas pelos casos em que os municípios de residência e ocorrência são considerados no painel
  filter(codmunres %in% df_infos_municipios$codmunres & codmunocor %in% df_infos_municipios$codmunres) |>
  semi_join(df_cnes_aux |> mutate(cnes = as.character(cnes), codufmun = as.character(codufmun)), by = join_by(cnes, codmunocor == codufmun, mes, ano))

### Adicionando as indicadores de internação em UTI e de internação na macrorregião de residência
df_aih_internacoes_wide_macros <- df_aih_internacoes_wide |>
  left_join(df_infos_municipios |> select(codmunres, macro_r_saude_res = macro_r_saude), by = join_by(codmunres == codmunres)) |>
  left_join(df_infos_municipios |> select(codmunres, macro_r_saude_ocor = macro_r_saude), by = join_by(codmunocor == codmunres)) |>
  mutate(
    idade_cat = case_when(
      idade_dias == 0 ~ "0_dias",
      idade_dias >= 1 & idade_dias < 7 ~ "1_a_6_dias",
      idade_dias >= 7 & idade_dias <= 27 ~ "7_a_27_dias",
      TRUE ~ NA_character_
    ),
    indicadora_mesma_macro = ifelse(macro_r_saude_res == macro_r_saude_ocor, "na_macro", "fora_macro"),
    indicadora_uti = ifelse(soma_uti_mes_to > 0, "internado_uti", "nao_internado_uti")
  ) |>
  select(!c(aihref, idade_dias, soma_uti_mes_to, macro_r_saude_res, macro_r_saude_ocor))

### Passando a base para o formato wide (um município por linha) e criando as variáveis necessárias
df_aih_internacoes_wide_macros$codmunres <- as.numeric(df_aih_internacoes_wide_macros$codmunres)

df_temp <- df_aih_internacoes_wide_macros |>
  select(!c(indicadora_mesma_macro, codmunres, causabas)) |>
  mutate(
    cnes = as.numeric(cnes),
    codmunocor = as.numeric(codmunocor),
    cobranca = paste0("cobranca_", cobranca)
  ) |>
  group_by(across(everything())) |>
  summarise(num_internacoes = n(), .groups = "drop") |>
  pivot_wider(
    names_from = c(idade_cat, indicadora_uti, cobranca),
    values_from = num_internacoes,
    values_fill = 0,
    names_prefix = "internacoes_neonatais_"
  ) |>
  right_join(df_cnes_aux, by = join_by(cnes, codmunocor == codufmun, mes, ano)) |>
  arrange(cnes, codmunocor, mes, ano) |>
  mutate(across(-c(mes, ano, cnes, codmunocor), ~ replace_na(., 0)))

df_bloco7_sih_internacoes <- df_temp %>%
  mutate(
    # Para o indicador de internações geral, os nomes das variáveis seguem o padrão "internacoes_neonatais_local-do-parto_idade-do-bebe"
    internacoes_neonatais_0_dias = rowSums(select(., contains("0_dias")), na.rm = TRUE),
    internacoes_neonatais_1_a_6_dias = rowSums(select(., contains("1_a_6_dias")), na.rm = TRUE),
    internacoes_neonatais_7_a_27_dias = rowSums(select(., contains("7_a_27_dias")), na.rm = TRUE),
    total_internacoes_neonatais = internacoes_neonatais_0_dias + internacoes_neonatais_1_a_6_dias + internacoes_neonatais_7_a_27_dias,

    # Para o indicador de internações em UTI, os nomes das variáveis seguem o padrão "internacoes_neonatais_local-do-parto_idade-do-bebe_internado_uti"
    internacoes_neonatais_uti_0_dias = rowSums(select(., contains("0_dias_internado")), na.rm = TRUE),
    internacoes_neonatais_uti_1_a_6_dias = rowSums(select(., contains("1_a_6_dias_internado")), na.rm = TRUE),
    internacoes_neonatais_uti_7_a_27_dias = rowSums(select(., contains("7_a_27_dias_internado")), na.rm = TRUE),
    internacoes_neonatais_uti = internacoes_neonatais_uti_0_dias + internacoes_neonatais_uti_1_a_6_dias + internacoes_neonatais_uti_7_a_27_dias,

    # Para o indicador de motivo de alta
    internacoes_neonatais_tipo_de_saida_alta_0_dias = rowSums(select(., contains("0_dias") & contains("cobranca_1")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_alta_1_a_6_dias = rowSums(select(., contains("1_a_6_dias") & contains("cobranca_1")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_alta_7_a_27_dias = rowSums(select(., contains("7_a_27_dias") & contains("cobranca_1")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_alta =
      internacoes_neonatais_tipo_de_saida_alta_0_dias +
      internacoes_neonatais_tipo_de_saida_alta_1_a_6_dias +
      internacoes_neonatais_tipo_de_saida_alta_7_a_27_dias,

    internacoes_neonatais_tipo_de_saida_permanencia_0_dias = rowSums(select(., contains("0_dias") & contains("cobranca_2")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_permanencia_1_a_6_dias = rowSums(select(., contains("1_a_6_dias") & contains("cobranca_2")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_permanencia_7_a_27_dias = rowSums(select(., contains("7_a_27_dias") & contains("cobranca_2")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_permanencia =
      internacoes_neonatais_tipo_de_saida_permanencia_0_dias +
      internacoes_neonatais_tipo_de_saida_permanencia_1_a_6_dias +
      internacoes_neonatais_tipo_de_saida_permanencia_7_a_27_dias,

    internacoes_neonatais_tipo_de_saida_transferencia_0_dias = rowSums(select(., contains("0_dias") & contains("cobranca_3")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_transferencia_1_a_6_dias = rowSums(select(., contains("1_a_6_dias") & contains("cobranca_3")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_transferencia_7_a_27_dias = rowSums(select(., contains("7_a_27_dias") & contains("cobranca_3")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_transferencia =
      internacoes_neonatais_tipo_de_saida_transferencia_0_dias +
      internacoes_neonatais_tipo_de_saida_transferencia_1_a_6_dias +
      internacoes_neonatais_tipo_de_saida_transferencia_7_a_27_dias,

    internacoes_neonatais_tipo_de_saida_morte_0_dias = rowSums(select(., contains("0_dias") & contains("cobranca_4")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_morte_1_a_6_dias = rowSums(select(., contains("1_a_6_dias") & contains("cobranca_4")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_morte_7_a_27_dias = rowSums(select(., contains("7_a_27_dias") & contains("cobranca_4")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_morte =
      internacoes_neonatais_tipo_de_saida_morte_0_dias +
      internacoes_neonatais_tipo_de_saida_morte_1_a_6_dias +
      internacoes_neonatais_tipo_de_saida_morte_7_a_27_dias,

    internacoes_neonatais_tipo_de_saida_alta_administrativa_0_dias = rowSums(select(., contains("0_dias") & contains("cobranca_5")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_alta_administrativa_1_a_6_dias = rowSums(select(., contains("1_a_6_dias") & contains("cobranca_5")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_alta_administrativa_7_a_27_dias = rowSums(select(., contains("7_a_27_dias") & contains("cobranca_5")), na.rm = TRUE),
    internacoes_neonatais_tipo_de_saida_alta_administrativa =
      internacoes_neonatais_tipo_de_saida_alta_administrativa_0_dias +
      internacoes_neonatais_tipo_de_saida_alta_administrativa_1_a_6_dias +
      internacoes_neonatais_tipo_de_saida_alta_administrativa_7_a_27_dias
  ) |>
  select(mes, ano, cnes, codmunocor, internacoes_neonatais_0_dias:internacoes_neonatais_tipo_de_saida_alta_administrativa)

### Verificando se o total de internações equivale ao número de linhas das bases df_aih_wide/df_aih_wide_macros
sum(df_bloco7_sih_internacoes$internacoes_neonatais_0_dias, df_bloco7_sih_internacoes$internacoes_neonatais_1_a_6_dias, df_bloco7_sih_internacoes$internacoes_neonatais_7_a_27_dias) == nrow(df_aih_internacoes_wide_macros)

sum(df_bloco7_sih_internacoes$internacoes_neonatais_uti_0_dias, df_bloco7_sih_internacoes$internacoes_neonatais_uti_1_a_6_dias, df_bloco7_sih_internacoes$internacoes_neonatais_uti_7_a_27_dias) == nrow(df_aih_internacoes_wide[df_aih_internacoes_wide$soma_uti_mes_to > 0, ])


## Para o denominador dos indicadores (total de partos públicos) -----------
### Rodando o algoritmo da Claudia na base completa de partos
#### Mudando o diretório para a pasta que contém o algoritmo em C++
setwd("data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/")

#### Rodando o algoritmo em C++ na base de partos
system(glue("./processaih {diretorio_bases_brutas}/BR_sih_rd_partos_2018_2023.csv"))

#### Voltando para o diretório original do projeto
setwd(diretorio_original)

#### Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
con <- dbConnect(SQLite(), "data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite")

#### Selecionando a tabela "aih" com todas as suas variáveis, ordenadas por AIHREF e DT_INTER
df_aih_partos_aux <- dbGetQuery(con, "select * from aih order by AIHREF, DT_INTER")
dbDisconnect(con)

### Adicionando variáveis que estão no SIH-RD, mas que não são devolvidas na base gerada pelo algoritmo
df_aih_partos <- left_join(
  df_aih_partos_aux,
  df_sih_rd_partos |>
    select(CNES, MUNIC_MOV, MES_CMPT, ANO_CMPT, DT_INTER, DT_SAIDA, N_AIH) |>
    mutate_at(vars(c(CNES, MUNIC_MOV, DT_INTER, DT_SAIDA, N_AIH)), as.character)
)

### Passando as casos para o formato wide (cada linha corresponderá a uma pessoa única)
df_bloco7_sih_partos <- df_aih_partos |>
  group_by(AIHREF) |>  # Agrupando pelo N_AIH comum para todas as linhas de um episódio de cuidado completo
  summarise(
    MES_CMPT = last(MES_CMPT),
    ANO_CMPT = last(ANO_CMPT),  # Ano de processamento do SIH da última internação
    MUNIC_MOV = first(MUNIC_MOV),  # Município de ocorrência da primeira internação
    CNES = first(CNES)  # CNES da primeira internação
  ) |>
  ungroup() |>
  select(mes = MES_CMPT, ano = ANO_CMPT, cnes = CNES, codmunocor = MUNIC_MOV, aihref = AIHREF) |>
  mutate(nascidos_estabelecimentos_publicos_sih = 1) |>
  clean_names() |>
  group_by(cnes, codmunocor, mes, ano) |>
  # Criando a variável que contém o número nascimentos em hospitais públicos em cada município
  summarise(nascidos_estabelecimentos_publicos_sih = sum(nascidos_estabelecimentos_publicos_sih)) |>
  ungroup() |>
  # Juntando com a base auxiliar de CNES
  mutate(cnes = as.numeric(cnes), codmunocor = as.numeric(codmunocor)) |>
  right_join(df_cnes_aux, by = join_by(cnes, codmunocor == codufmun, mes, ano)) |>
  mutate(across(everything(), ~replace_na(., 0)))

### Removendo arquivos já utilizados e que são maiores que o limite de 100 mb
file.remove(c(
  "data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_menores_28_dias_2018_2023.csv",
  "data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_partos_2018_2023.csv",
  "data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/aihperm.csv",
  "data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/aihpermtransf.csv",
  "data-raw/extracao-dos-dados/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite"

))

## Juntando todas as bases
df_bloco7_morbidade_neonatal <- right_join(df_ameacadoras, df_cnes_aux, by = join_by(cnes, codmunocor == codufmun, mes, ano)) |>
  left_join(df_bloco7_sih_partos) |>
  left_join(df_bloco7_sih_internacoes) |>
  mutate(across(everything(), ~replace_na(., 0)))

write.csv(df_bloco7_morbidade_neonatal, 'data-raw/csv/df_indicadores_morbidade_neonatal_2018_2023.csv', row.names = FALSE)


############ Dados para a distribuição de internações neonatais
cids_internacoes_neonatais <- read_excel("data-raw/extracao-dos-dados/databases_auxiliares/cids_internacoes_neonatais.xlsx") |>
  select(causabas = `CID`,
         grupos = `CONSIDERAR ESTA  COLUNA -REVISÃO COM CINTIA E TATIANE EM 1/10/2024 Grupo da rede interagencial moficado para causa de internação neonatal (fluxograma na aba 'orientacoes')`)

for(i in unique(cids_internacoes_neonatais$grupos)){
  nome_variavel <- tolower(i)
  nome_variavel <- gsub(" ", "_", nome_variavel)
  assign(nome_variavel, filter(cids_internacoes_neonatais, grupos == i)$causabas)
}

df_internacoes_neonatais_totais <- df_aih_internacoes_wide |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  filter(!(causabas %in% excluir | causabas2 %in% excluir)) |>
  select(codmunocor, cnes, mes, ano) |>
  mutate(internacoes_neonatais_totais = 1) |>
  group_by(across(!internacoes_neonatais_totais)) |>
  summarise(internacoes_neonatais_totais = sum(internacoes_neonatais_totais)) |>
  ungroup()


internacoes_neonatais_grupos <- df_aih_internacoes_wide |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% infecções | causabas2 %in% infecções | causabas %in% infecção | causabas2 %in% infecção ~ "morbidade_neonatal_grupos_infeccoes",
      causabas %in% `afecções_respiratórias_do_recém-nascido` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` | causabas %in% `afecções_respiratórias_dos_recém-nascidos` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` ~ "morbidade_neonatal_grupos_afeccoes_respiratorias",
      causabas %in% fatores_maternos_relacionados_à_gravidez | causabas2 %in% fatores_maternos_relacionados_à_gravidez~ "morbidade_neonatal_grupos_fatores_maternos",
      causabas %in% `asfixia_/_hipóxia` | causabas2 %in% `asfixia_/_hipóxia` ~ "morbidade_neonatal_grupos_asfixia",
      causabas %in% prematuridade | causabas2 %in% prematuridade ~ "morbidade_neonatal_grupos_prematuridade",
      causabas %in% afecções_não_especificadas_do_período_perinatal | causabas %in% afecções_originais_no_período_perinatal| causabas %in% afecções_não_especificadas_originadas_no_período_perinatal |  causabas2 %in% afecções_não_especificadas_do_período_perinatal | causabas2 %in% afecções_originais_no_período_perinatal | causabas2 %in% causabas %in% afecções_não_especificadas_originadas_no_período_perinatal ~ "morbidade_neonatal_grupos_afeccoes_perinatal",
      causabas %in% transtornos_cardíacos_originados_no_período_perinatal | causabas2 %in% transtornos_cardíacos_originados_no_período_perinatal ~ "morbidade_neonatal_grupos_cardiacos_perinatal",
      causabas %in% icterícia_neonatal | causabas2 %in% icterícia_neonatal ~ "morbidade_neonatal_grupos_ictericia",
      causabas %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido`| causabas2 %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido` ~ "morbidade_neonatal_grupos_endocrinos",
      causabas %in% problemas_de_alimentação_do_rn | causabas2 %in% problemas_de_alimentação_do_rn ~ "morbidade_neonatal_grupos_alimentacao",
      causabas %in% má_formação_congênita | causabas2 %in% má_formação_congênita ~ "morbidade_neonatal_grupos_ma_formacao",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "morbidade_neonatal_grupos_mal_definidas",
      TRUE ~ "morbidade_neonatal_grupos_outros"
    )
  ) |>
  select(codmunocor, cnes, mes, ano, grupo_cid) |>
  mutate(internacoes_neonatais = 1) |>
  group_by(across(!internacoes_neonatais)) |>
  summarise(internacoes_neonatais = sum(internacoes_neonatais)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = internacoes_neonatais,
    values_fill = 0
  ) |>
  right_join(df_cnes_aux |> mutate(cnes = as.character(cnes), codufmun = as.character(codufmun)), by = join_by(cnes, codmunocor == codufmun, mes, ano)) |>
  mutate(across(everything(), ~replace_na(., 0)))  |>
  arrange(cnes, codmunocor, mes, ano)

internacoes_neonatais_grupos[is.na(internacoes_neonatais_grupos)] <- 0


internacoes_neonatais_grupos_0_dias <- df_aih_internacoes_wide |>
  filter(idade_dias == 0) |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% infecções | causabas2 %in% infecções | causabas %in% infecção | causabas2 %in% infecção ~ "morbidade_neonatal_grupos_0_dias_infeccoes",
      causabas %in% `afecções_respiratórias_do_recém-nascido` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` | causabas %in% `afecções_respiratórias_dos_recém-nascidos` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` ~ "morbidade_neonatal_grupos_0_dias_afeccoes_respiratorias",
      causabas %in% fatores_maternos_relacionados_à_gravidez | causabas2 %in% fatores_maternos_relacionados_à_gravidez~ "morbidade_neonatal_grupos_0_dias_fatores_maternos",
      causabas %in% `asfixia_/_hipóxia` | causabas2 %in% `asfixia_/_hipóxia` ~ "morbidade_neonatal_grupos_0_dias_asfixia",
      causabas %in% prematuridade | causabas2 %in% prematuridade ~ "morbidade_neonatal_grupos_0_dias_prematuridade",
      causabas %in% afecções_não_especificadas_do_período_perinatal | causabas %in% afecções_originais_no_período_perinatal| causabas %in% afecções_não_especificadas_originadas_no_período_perinatal |  causabas2 %in% afecções_não_especificadas_do_período_perinatal | causabas2 %in% afecções_originais_no_período_perinatal | causabas2 %in% causabas %in% afecções_não_especificadas_originadas_no_período_perinatal ~ "morbidade_neonatal_grupos_0_dias_afeccoes_perinatal",
      causabas %in% transtornos_cardíacos_originados_no_período_perinatal | causabas2 %in% transtornos_cardíacos_originados_no_período_perinatal ~ "morbidade_neonatal_grupos_0_dias_cardiacos_perinatal",
      causabas %in% icterícia_neonatal | causabas2 %in% icterícia_neonatal ~ "morbidade_neonatal_grupos_0_dias_ictericia",
      causabas %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido`| causabas2 %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido` ~ "morbidade_neonatal_grupos_0_dias_endocrinos",
      causabas %in% problemas_de_alimentação_do_rn | causabas2 %in% problemas_de_alimentação_do_rn ~ "morbidade_neonatal_grupos_0_dias_alimentacao",
      causabas %in% má_formação_congênita | causabas2 %in% má_formação_congênita ~ "morbidade_neonatal_grupos_0_dias_ma_formacao",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "morbidade_neonatal_grupos_0_dias_mal_definidas",
      TRUE ~ "morbidade_neonatal_grupos_0_dias_outros"
    )
  ) |>
  select(codmunocor, cnes, mes, ano, grupo_cid) |>
  mutate(internacoes_neonatais = 1) |>
  group_by(across(!internacoes_neonatais)) |>
  summarise(internacoes_neonatais = sum(internacoes_neonatais)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = internacoes_neonatais,
    values_fill = 0
  ) |>
  right_join(df_cnes_aux |> mutate(cnes = as.character(cnes), codufmun = as.character(codufmun)), by = join_by(cnes, codmunocor == codufmun, mes, ano)) |>
  mutate(across(everything(), ~replace_na(., 0)))  |>
  arrange(cnes, codmunocor, mes, ano)

internacoes_neonatais_grupos_0_dias[is.na(internacoes_neonatais_grupos_0_dias)] <- 0


internacoes_neonatais_grupos_7_27_dias <- df_aih_internacoes_wide |>
  filter(idade_dias >= 7 & idade_dias <= 27)|>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% infecções | causabas2 %in% infecções | causabas %in% infecção | causabas2 %in% infecção ~ "morbidade_neonatal_grupos_7_27_dias_infeccoes",
      causabas %in% `afecções_respiratórias_do_recém-nascido` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` | causabas %in% `afecções_respiratórias_dos_recém-nascidos` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` ~ "morbidade_neonatal_grupos_7_27_dias_afeccoes_respiratorias",
      causabas %in% fatores_maternos_relacionados_à_gravidez | causabas2 %in% fatores_maternos_relacionados_à_gravidez~ "morbidade_neonatal_grupos_7_27_dias_fatores_maternos",
      causabas %in% `asfixia_/_hipóxia` | causabas2 %in% `asfixia_/_hipóxia` ~ "morbidade_neonatal_grupos_7_27_dias_asfixia",
      causabas %in% prematuridade | causabas2 %in% prematuridade ~ "morbidade_neonatal_grupos_7_27_dias_prematuridade",
      causabas %in% afecções_não_especificadas_do_período_perinatal | causabas %in% afecções_originais_no_período_perinatal| causabas %in% afecções_não_especificadas_originadas_no_período_perinatal |  causabas2 %in% afecções_não_especificadas_do_período_perinatal | causabas2 %in% afecções_originais_no_período_perinatal | causabas2 %in% causabas %in% afecções_não_especificadas_originadas_no_período_perinatal ~ "morbidade_neonatal_grupos_7_27_dias_afeccoes_perinatal",
      causabas %in% transtornos_cardíacos_originados_no_período_perinatal | causabas2 %in% transtornos_cardíacos_originados_no_período_perinatal ~ "morbidade_neonatal_grupos_7_27_dias_cardiacos_perinatal",
      causabas %in% icterícia_neonatal | causabas2 %in% icterícia_neonatal ~ "morbidade_neonatal_grupos_7_27_dias_ictericia",
      causabas %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido`| causabas2 %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido` ~ "morbidade_neonatal_grupos_7_27_dias_endocrinos",
      causabas %in% problemas_de_alimentação_do_rn | causabas2 %in% problemas_de_alimentação_do_rn ~ "morbidade_neonatal_grupos_7_27_dias_alimentacao",
      causabas %in% má_formação_congênita | causabas2 %in% má_formação_congênita ~ "morbidade_neonatal_grupos_7_27_dias_ma_formacao",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "morbidade_neonatal_grupos_7_27_dias_mal_definidas",
      TRUE ~ "morbidade_neonatal_grupos_7_27_dias_outros"
    )
  ) |>
  select(codmunocor, cnes, mes, ano, grupo_cid) |>
  mutate(internacoes_neonatais = 1) |>
  group_by(across(!internacoes_neonatais)) |>
  summarise(internacoes_neonatais = sum(internacoes_neonatais)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = internacoes_neonatais,
    values_fill = 0
  ) |>
  right_join(df_cnes_aux |> mutate(cnes = as.character(cnes), codufmun = as.character(codufmun)), by = join_by(cnes, codmunocor == codufmun, mes, ano)) |>
  mutate(across(everything(), ~replace_na(., 0)))  |>
  arrange(cnes, codmunocor, mes, ano)

internacoes_neonatais_grupos_7_27_dias[is.na(internacoes_neonatais_grupos_7_27_dias)] <- 0

internacoes_neonatais_grupos_1_6_dias <- df_aih_internacoes_wide |>
  filter(idade_dias >= 1 & idade_dias <= 6)|>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% infecções | causabas2 %in% infecções | causabas %in% infecção | causabas2 %in% infecção ~ "morbidade_neonatal_grupos_1_6_dias_infeccoes",
      causabas %in% `afecções_respiratórias_do_recém-nascido` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` | causabas %in% `afecções_respiratórias_dos_recém-nascidos` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` ~ "morbidade_neonatal_grupos_1_6_dias_afeccoes_respiratorias",
      causabas %in% fatores_maternos_relacionados_à_gravidez | causabas2 %in% fatores_maternos_relacionados_à_gravidez~ "morbidade_neonatal_grupos_1_6_dias_fatores_maternos",
      causabas %in% `asfixia_/_hipóxia` | causabas2 %in% `asfixia_/_hipóxia` ~ "morbidade_neonatal_grupos_1_6_dias_asfixia",
      causabas %in% prematuridade | causabas2 %in% prematuridade ~ "morbidade_neonatal_grupos_1_6_dias_prematuridade",
      causabas %in% afecções_não_especificadas_do_período_perinatal | causabas %in% afecções_originais_no_período_perinatal| causabas %in% afecções_não_especificadas_originadas_no_período_perinatal |  causabas2 %in% afecções_não_especificadas_do_período_perinatal | causabas2 %in% afecções_originais_no_período_perinatal | causabas2 %in% causabas %in% afecções_não_especificadas_originadas_no_período_perinatal ~ "morbidade_neonatal_grupos_1_6_dias_afeccoes_perinatal",
      causabas %in% transtornos_cardíacos_originados_no_período_perinatal | causabas2 %in% transtornos_cardíacos_originados_no_período_perinatal ~ "morbidade_neonatal_grupos_1_6_dias_cardiacos_perinatal",
      causabas %in% icterícia_neonatal | causabas2 %in% icterícia_neonatal ~ "morbidade_neonatal_grupos_1_6_dias_ictericia",
      causabas %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido`| causabas2 %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido` ~ "morbidade_neonatal_grupos_1_6_dias_endocrinos",
      causabas %in% problemas_de_alimentação_do_rn | causabas2 %in% problemas_de_alimentação_do_rn ~ "morbidade_neonatal_grupos_1_6_dias_alimentacao",
      causabas %in% má_formação_congênita | causabas2 %in% má_formação_congênita ~ "morbidade_neonatal_grupos_1_6_dias_ma_formacao",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "morbidade_neonatal_grupos_1_6_dias_mal_definidas",
      TRUE ~ "morbidade_neonatal_grupos_1_6_dias_outros"
    )
  ) |>
  select(codmunocor, cnes, mes, ano, grupo_cid) |>
  mutate(internacoes_neonatais = 1) |>
  group_by(across(!internacoes_neonatais)) |>
  summarise(internacoes_neonatais = sum(internacoes_neonatais)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = internacoes_neonatais,
    values_fill = 0
  ) |>
  right_join(df_cnes_aux |> mutate(cnes = as.character(cnes), codufmun = as.character(codufmun)), by = join_by(cnes, codmunocor == codufmun, mes, ano)) |>
  mutate(across(everything(), ~replace_na(., 0)))  |>
  arrange(cnes, codmunocor, mes, ano)

internacoes_neonatais_grupos_1_6_dias[is.na(internacoes_neonatais_grupos_1_6_dias)] <- 0

############ Juntandos os dados para a aba de morbidade
df_distribuicao_morbidade <- left_join(internacoes_neonatais_grupos, internacoes_neonatais_grupos_0_dias) |>
  left_join(internacoes_neonatais_grupos_1_6_dias) |>
  left_join(internacoes_neonatais_grupos_7_27_dias) |>
  left_join(df_internacoes_neonatais_totais) |>
  dplyr::select(
    cnes, codmunocor, mes, ano,
    internacoes_neonatais_totais,
    starts_with("morbidade")
  ) |>
  arrange(cnes, codmunocor, mes, ano)

write.csv(df_distribuicao_morbidade, "data-raw/csv/df_causas_principais_morbidade_neonatal_2018_2023.csv", row.names = FALSE)


