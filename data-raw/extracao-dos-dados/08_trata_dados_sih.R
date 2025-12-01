### OBS: esse script requer arquivos gerados ao rodar os scripts da pasta "morbidade"

library(dplyr)
library(janitor)
library(data.table)
library(stringr)
library(tidyr)
library(glue)
library(purrr)
library(furrr)

# Preparando a base de indicadores do SIH para o painel -----------------------
## Montando a base de internações obstétricas nacional
### Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO"
             )

estados_nomes <- c(
  "AC" = "Acre",
  "AL" = "Alagoas",
  "AP" = "Amapá",
  "AM" = "Amazonas",
  "BA" = "Bahia",
  "CE" = "Ceará",
  "DF" = "Distrito Federal",
  "ES" = "Espírito Santo",
  "GO" = "Goiás",
  "MA" = "Maranhão",
  "MT" = "Mato Grosso",
  "MS" = "Mato Grosso do Sul",
  "MG" = "Minas Gerais",
  "PA" = "Pará",
  "PB" = "Paraíba",
  "PR" = "Paraná",
  "PE" = "Pernambuco",
  "PI" = "Piauí",
  "RJ" = "Rio de Janeiro",
  "RN" = "Rio Grande do Norte",
  "RS" = "Rio Grande do Sul",
  "RO" = "Rondônia",
  "RR" = "Roraima",
  "SC" = "Santa Catarina",
  "SP" = "São Paulo",
  "SE" = "Sergipe",
  "TO" = "Tocantins"
)

### Lendo a base auxiliar de CNES
df_cnes_aux <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_aux.csv")

### Criando um vetor com as variáveis de diagnóstico de interesse
variaveis_diagnostico <- c(
  "DIAG_PRINC", "DIAG_SECUN", "DIAGSEC1", "DIAGSEC2", "DIAGSEC3", "DIAGSEC4",
  "DIAGSEC5", "DIAGSEC6", "DIAGSEC7", "DIAGSEC8", "DIAGSEC9", "CID_MORTE",
  "CID_ASSO", "CID_NOTIF"
) |>
  make_clean_names()

### Lendo as bases estaduais (pós algoritmo de cuidado) e passando-as para o formato wide
plan(multisession)
options(future.rng.onMisuse = "ignore")

processa_estado <- function(estado) {
  uf_nome <- estados_nomes[estado]

  # Lendo o arquivo no formato long
  output_dir_long <- "data-raw/extracao-dos-dados/morbidade/databases/01_sih_rd/02_arquivos_tratados_long"
  df_sih_long_uf <- fread(glue("{output_dir_long}/{estado}_sih_rd_tratado_long_2018_2023.csv"))

  # Passando-o para o formato wide
  df_sih_wide_uf <- df_sih_long_uf |>
    mutate(
      DT_INTER = as.Date(DT_INTER, format = "%Y%m%d"),
      DT_SAIDA = as.Date(DT_SAIDA, format = "%Y%m%d"),
      NASC = as.Date(NASC, format = "%Y%m%d")
    ) |>
    group_by(AIHREF) |>
    summarise(
      ano = last(ANO_CMPT),
      cnes = first(CNES),
      codmunocor = first(MUNIC_MOV),
      soma_uti = sum(as.integer(UTI_MES_TO)),
      diag_princ = first(DIAG_PRINC),
      diag_secun = first(DIAG_SECUN),
      diagsec1 = first(DIAGSEC1),
      diagsec2 = first(DIAGSEC2),
      diagsec3 = first(DIAGSEC3),
      diagsec4 = first(DIAGSEC4),
      diagsec5 = first(DIAGSEC5),
      diagsec6 = first(DIAGSEC6),
      diagsec7 = first(DIAGSEC7),
      diagsec8 = first(DIAGSEC8),
      diagsec9 = first(DIAGSEC9),
      cid_morte = last(CID_MORTE),
      cid_asso = first(CID_ASSO),
      cid_notif = first(CID_NOTIF),
      proc_rea = first(PROC_REA),
      cobranca = last(COBRANCA)
    ) |>
    ungroup() |>
    select(
      aihref = AIHREF,
      ano,
      cnes,
      codmunocor,
      soma_uti,
      diag_princ,
      diag_secun,
      starts_with("diagsec"),
      cid_morte,
      cid_asso,
      cid_notif,
      proc_rea,
      cobranca
    )

  # Salvando o arquivo no formato wide
  output_dir_wide <- "data-raw/extracao-dos-dados/morbidade/databases/01_sih_rd/03_arquivos_tratados_wide"
  if (!dir.exists(output_dir_wide)) {dir.create(output_dir_wide)}

  write.csv(df_sih_wide_uf, glue("{output_dir_wide}/{estado}_sih_tratado_wide_2018_2023.csv"), row.names = FALSE)

  # Criando a variável de internações para parto com acompanhante a partir do SIH-SP
  output_dir_sih_sp <- "data-raw/extracao-dos-dados/morbidade/databases/02_sih_sp"
  df_indicadores_sih_sp <- fread(glue("{output_dir_sih_sp}/{estado}_sih_sp_filtrado_2018_2023.csv.gz")) |>
    left_join(df_sih_long_uf, by = join_by("SP_NAIH" == "N_AIH")) |>
    mutate(
      internacoes_motivo_parto = if_else(
        FLAG == "<init>" &
          (
            (
              (DIAG_PRINC >= "O32" & DIAG_PRINC <= "O36") |
                (DIAG_PRINC >= "O60" & DIAG_PRINC <= "O69") |
                (DIAG_PRINC >= "O80" & DIAG_PRINC <= "O84") |
                (DIAG_PRINC == "P95")
            ) |
              (!(DIAG_PRINC >= "O00" & DIAG_PRINC <= "O08") & (PROC_REA %in% c(310010012, 310010039, 310010047, 310010055, 411010026, 411010034, 411010042)))
          ),
        1, 0, missing = 0
      ),
      internacoes_parto_acompanhante = if_else(SP_ATOPROF == 802010032, 1, 0, missing = 0)
    ) |>
    group_by(AIHREF) |>
    summarise(
      internacoes_motivo_parto = first(internacoes_motivo_parto),
      internacoes_parto_acompanhante = sum(internacoes_parto_acompanhante)
    ) |>
    ungroup() |>
    mutate(
      internacoes_parto_acompanhante = if_else(
        internacoes_motivo_parto == 1 & internacoes_parto_acompanhante >= 1,
        1, 0, missing = 0
      ),
      .keep = "unused"
    )

  # Criando as variáveis de interesse a partir do SIH-RD
  df_sih_wide_uf <- fread(glue("{output_dir_wide}/{estado}_sih_tratado_wide_2018_2023.csv"))

  df_indicadores_sih_uf <- df_sih_wide_uf |>
    dplyr::mutate(across(all_of(variaveis_diagnostico), as.character)) |>
    mutate(
      proc_rea = as.numeric(proc_rea),
      cobranca = as.numeric(cobranca)
    ) |>
    mutate(
      total_internacoes = 1,

      internacoes_motivo_aborto = if_else(diag_princ >= "O00" & diag_princ <= "O08", 1, 0, missing = 0),
      internacoes_motivo_parto = if_else(
        (
          (diag_princ >= "O32" & diag_princ <= "O36") |
            (diag_princ >= "O60" & diag_princ <= "O69") |
            (diag_princ >= "O80" & diag_princ <= "O84") |
            (diag_princ == "P95")
        ) |
          (!(diag_princ >= "O00" & diag_princ <= "O08") & (proc_rea %in% c(310010012, 310010039, 310010047, 310010055, 411010026, 411010034, 411010042))),
        1, 0, missing = 0
      ),
      internacoes_motivo_intercorrencia_puerperio = if_else(
        ((diag_princ >= "O70" & diag_princ <= "O73") |
          (diag_princ >= "O85" & diag_princ <= "O94") |
          proc_rea == 303100010) &
          !((
            (diag_princ >= "O32" & diag_princ <= "O36") |
              (diag_princ >= "O60" & diag_princ <= "O69") |
              (diag_princ >= "O80" & diag_princ <= "O84") |
              (diag_princ == "P95")
          ) | proc_rea %in% c(310010012, 310010039, 310010047, 310010055, 411010026, 411010034, 411010042)),
        1, 0, missing = 0
      ),
      internacoes_motivo_intercorrencia_gravidez = if_else(
        (
          ((diag_princ >= "O10" & diag_princ <= "O28") |
             diag_princ %in% c("O30", "O31") |
             (diag_princ >= "O40" & diag_princ <= "O48")) |
            (proc_rea %in% c(303100044, 303100028, 303100036))
        ) &
          (internacoes_motivo_aborto == 0 & internacoes_motivo_parto == 0 & internacoes_motivo_intercorrencia_puerperio == 0),
        1, 0, missing = 0
      ),
      internacoes_motivo_intercorrencia_gravidez = if_else(
        internacoes_motivo_intercorrencia_gravidez == 0 & internacoes_motivo_aborto == 0 & internacoes_motivo_parto == 0 & internacoes_motivo_intercorrencia_puerperio == 0,
        1, internacoes_motivo_intercorrencia_gravidez
      ),
      internacoes_tipo_de_alta_alta = if_else(cobranca == 1, 1, 0, missing = 0),
      internacoes_tipo_de_alta_permanencia = if_else(cobranca == 2, 1, 0, missing = 0),
      internacoes_tipo_de_alta_transferencia = if_else(cobranca == 3, 1, 0, missing = 0),
      internacoes_tipo_de_alta_morte = if_else(cobranca == 4, 1, 0, missing = 0),
      internacoes_tipo_de_alta_administrativa = if_else(cobranca == 5, 1, 0, missing = 0),

      internacoes_amiu = if_else(proc_rea == 409060070 & internacoes_motivo_aborto == 1, 1, 0, missing = 0),
      internacoes_curetagem = if_else(proc_rea == 411020013 & internacoes_motivo_aborto == 1, 1, 0, missing = 0),

      internacoes_aborto_legal = fifelse(
        pmap_lgl(across(all_of(variaveis_diagnostico)), ~ any(startsWith(c(...), "O04"), na.rm = TRUE)),
        1L, 0L
      )
    ) |>
    left_join(df_indicadores_sih_sp, by = join_by("aihref" == "AIHREF")) |>
    group_by(cnes, codmunocor, ano) |>
    summarise(across(starts_with("total") | starts_with("internacoes"), sum), .groups = "drop") |>
    right_join(df_cnes_aux |> filter(uf == uf_nome), by = c("cnes", "codmunocor" = "codufmun", "ano")) |>
    mutate(across(everything(), ~replace_na(., 0))) |>
    dplyr::select(
      cnes, codmunocor, ano, categoria_porte, tipo, municipio, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
      total_internacoes,
      starts_with("internacoes")
    ) |>
    arrange(cnes, codmunocor, ano)

  write.csv(df_indicadores_sih_uf, glue("{output_dir_wide}/{estado}_df_indicadores_sih_2018_2023.csv"), row.names = FALSE)

  return(invisible(TRUE))
}

### Executa em paralelo
invisible(future_map(estados, processa_estado))

### Unindo as bases estaduais
df_indicadores_sih <- data.frame()

for (estado in estados) {
  # Lendo a base do dado estado já no formato wide
  df_indicadores_sih_uf <- fread(glue("data-raw/extracao-dos-dados/morbidade/databases/01_sih_rd/03_arquivos_tratados_wide/{estado}_df_indicadores_sih_2018_2023.csv"))

  # Juntando com o restante dos dados
  df_indicadores_sih <- bind_rows(df_indicadores_sih, df_indicadores_sih_uf)
}

## Salvando a base final
write.csv(df_indicadores_sih |> arrange(cnes, codmunocor), "data-raw/csv/df_indicadores_internacoes_obstetricas_2018_2023.csv", row.names = FALSE)



