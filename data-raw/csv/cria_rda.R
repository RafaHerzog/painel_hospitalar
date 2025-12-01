# Lendo as bases com os indicadores -------------------------------------------
df_cnes_aux <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_aux.csv") |>
  dplyr::mutate(
    ano_mes = as.Date(sprintf("%04d-%02d-01", ano, mes)),
    .after = "ano"
  )

## Lendo a tabela que contém informações dos indicadores
tabela_indicadores <- read.csv("data-raw/csv/tabela_indicadores_painel_hospitalar.csv")

## Criando vetores com as possibilidades de indicadores a serem escolhidos
indicadores_perfil_sociodemografico <- tabela_indicadores |>
  dplyr::filter(bloco == "perfil_sociodemografico") |>
  dplyr::select(indicador_principal, nome_abreviado_indicador, categoria_sub_indicador, sub_indicador) |>
  unique()

indicadores_perfil_obstetrico <- tabela_indicadores |>
  dplyr::filter(bloco == "perfil_obstetrico") |>
  dplyr::select(indicador_principal, nome_abreviado_indicador, categoria_sub_indicador, sub_indicador) |>
  unique()

## Criando os dataframes/vetores contendo as escolhas de regiões de estados, regiões de saúde, municípios e hospitais
estados_choices <- df_cnes_aux |>
  dplyr::pull(uf) |>
  unique()

r_saude_choices <- df_cnes_aux |>
  dplyr::select(uf, r_saude) |>
  unique()

municipios_choices <- df_cnes_aux |>
  dplyr::select(uf, r_saude, municipio) |>
  unique()

hospitais_choices <- df_cnes_aux |>
  dplyr::select(uf, r_saude, municipio, cnes, nome_fantasia) |>
  unique()

## Lendo a base contendo as variáveis do SINASC
df_indicadores_sinasc <- data.table::fread("data-raw/csv/df_indicadores_sinasc_2018_2023.csv") |>
  dplyr::left_join(df_cnes_aux |> dplyr::rename(codmunnasc = codufmun))

## Lendo a base contendo as variáveis de óbitos maternos
df_indicadores_mortalidade_materna <- data.table::fread("data-raw/csv/df_indicadores_mortalidade_materna_2018_2023.csv") |>
  dplyr::left_join(df_cnes_aux |> dplyr::rename(codmunocor = codufmun))

df_indicadores_morbidade_materna <- data.table::fread("data-raw/csv/df_indicadores_morbidade_materna_2018_2023.csv") |>
  dplyr::left_join(df_cnes_aux |> dplyr::rename(codmunocor = codufmun))

df_indicadores_internacoes_obstetricas <- dplyr::left_join(
  df_cnes_aux |> dplyr::rename(codmunocor = codufmun),
  data.table::fread("data-raw/csv/df_indicadores_internacoes_obstetricas_2018_2023.csv") |>
    dplyr::select(!c(tipo, qtd_leitos))
) |>
  mutate(across(everything(), ~replace_na(., 0)))

## Lendo as bases contendo as variáveis de óbitos fetais
df_indicadores_fetais <- data.table::fread("data-raw/csv/df_indicadores_fetais_2018_2023.csv") |>
  dplyr::left_join(df_cnes_aux |> dplyr::rename(codmunocor = codufmun))
df_evitaveis_fetais <- data.table::fread("data-raw/csv/df_causas_evitaveis_fetais_2018_2023.csv") |>
  dplyr::rename(cnes = codestab) |>
  dplyr::left_join(df_cnes_aux)
df_principais_fetais <- data.table::fread("data-raw/csv/df_causas_principais_fetais_2018_2023.csv") |>
  dplyr::rename(cnes = codestab) |>
  dplyr::left_join(df_cnes_aux)

## Lendo a base contendo as variáveis de óbitos neonatais
df_indicadores_neonatais <- data.table::fread("data-raw/csv/df_indicadores_neonatais_2018_2023.csv") |>
  dplyr::left_join(df_cnes_aux |> dplyr::rename(codmunocor = codufmun))
df_evitaveis_neonatais <- data.table::fread("data-raw/csv/df_causas_evitaveis_neonatais_2018_2023.csv") |>
  dplyr::rename(cnes = codestab) |>
  dplyr::left_join(df_cnes_aux)
df_principais_neonatais <- data.table::fread("data-raw/csv/df_causas_principais_neonatais_2018_2023.csv") |>
  dplyr::rename(cnes = codestab) |>
  dplyr::left_join(df_cnes_aux)

## Lendo a base contendo as variáveis de óbitos perinatais
df_indicadores_perinatais <- data.table::fread("data-raw/csv/df_indicadores_perinatais_2018_2023.csv") |>
  dplyr::left_join(df_cnes_aux |> dplyr::rename(codmunocor = codufmun))
df_evitaveis_perinatais <- data.table::fread("data-raw/csv/df_causas_evitaveis_perinatais_2018_2023.csv") |>
  dplyr::rename(cnes = codestab) |>
  dplyr::left_join(df_cnes_aux)
df_principais_perinatais <- data.table::fread("data-raw/csv/df_causas_principais_perinatais_2018_2023.csv") |>
  dplyr::rename(cnes = codestab) |>
  dplyr::left_join(df_cnes_aux)

## Lendo a base contendo as variáveis de morbidade neonatal
df_indicadores_morbidade_neonatal <- data.table::fread("data-raw/csv/df_indicadores_morbidade_neonatal_2018_2023.csv") |>
  dplyr::left_join(df_cnes_aux)
df_principais_morbidade_neonatal <- data.table::fread("data-raw/csv/df_causas_principais_morbidade_neonatal_2018_2023.csv") |>
  dplyr::left_join(df_cnes_aux)

# Criando dataframes específicos de cada página ------------------------------
## Para a visão geral
df_visao_geral <- purrr::reduce(
  list(
    df_cnes_aux,
    df_indicadores_sinasc |> dplyr::select(cnes:ano, total_de_nascidos_vivos, nvm_cesarea, nv_prematuros),
    df_indicadores_mortalidade_materna |> dplyr::select(cnes:ano, obitos_maternos_totais),
    df_indicadores_morbidade_materna |> dplyr::select(cnes:ano, casos_mmg),
    df_indicadores_fetais |> dplyr::select(cnes, codmunocor, nome_fantasia, municipio, uf, regiao, cod_r_saude, r_saude, mes, ano, obitos_fetais_todos_todos),
    df_indicadores_neonatais |> dplyr::select(cnes:ano, obitos_neonatais_todos_todos)
  ),
  dplyr::left_join
)

## Para o bloco de "Perfil sociodemográfico das mulheres atendidas"
df_perfil_sociodemografico <- df_indicadores_sinasc |>
  dplyr::select(
    cnes:ano,
    categoria_porte,
    tipo:macro_r_saude,
    total_de_nascidos_vivos,
    starts_with("nvm_idade"),
    starts_with("nvm_racacor"),
    starts_with("nvm_escolaridade"),
    starts_with("nvm_estciv")
  )

## Para o bloco de "Perfil obstétrico das mulheres atendidas"
df_perfil_obstetrico <- df_indicadores_sinasc |>
  dplyr::select(
    cnes:ano,
    categoria_porte,
    tipo:macro_r_saude,
    total_de_nascidos_vivos,
    starts_with("nvm_qtdpart"),
    starts_with("nvm_qtdpartces"),
    starts_with("nvm_parto"),
    starts_with("nvm_gestacao"),
    nvm_trab_parto_induzido,
    starts_with("nvm_inicio_prenat"),
    starts_with("nvm_num_consultas_prenat_adequado")
  )

## Para o bloco 3
bloco3 <- df_indicadores_sinasc |>
  dplyr::select(
    cnes:ano,
    qtd_leitos_nascidos,
    tipo:macro_r_saude,
    total_de_nascidos_vivos,
    starts_with("nvm_inicio_prenat"),
    starts_with("nvm_num_consultas_prenat_adequado")
  )

## Para o bloco 4
bloco4 <- df_indicadores_sinasc |>
  dplyr::select(
    cnes:ano,
    qtd_leitos_nascidos,
    tipo:macro_r_saude,
    total_de_nascidos_vivos,
    starts_with("nvm_robson"),
    starts_with("nvm_cesarea"),
    nv_parto_vaginal,
    starts_with("nv_assist")
  )

## Para o bloco 5
bloco5 <- df_indicadores_sinasc |>
  dplyr::select(
    cnes:ano,
    qtd_leitos_nascidos,
    tipo:macro_r_saude,
    total_de_nascidos_vivos,
    starts_with("nv_ig"),
    starts_with("nv_prematuros"),
    starts_with("nv_peso"),
    starts_with("nv_baixo_peso"),
    "nv_asfixia_2500g_mais",
    starts_with("nv_anomalia")
  )

### Para a tabela de anomalias
tabela_anomalias_prioritarias <- data.table::fread("data-raw/csv/df_tabela_anomalias_prioritarias_2018_2023.csv")

## Para o bloco 6
bloco6 <- list(
  df_indicadores_mortalidade_materna,
  df_indicadores_morbidade_materna,
  df_indicadores_internacoes_obstetricas
) |>
  purrr::reduce(dplyr::full_join)

## Para o bloco 7
### Para óbitos fetais
bloco7_fetal <- df_indicadores_fetais
bloco7_evitaveis_fetal <- df_evitaveis_fetais
bloco7_principais_fetal <- df_principais_fetais

### Para óbitos perinatais
bloco7_perinatal <- df_indicadores_perinatais
bloco7_evitaveis_perinatal <- df_evitaveis_perinatais
bloco7_principais_perinatal <- df_principais_perinatais

### Para óbitos neonatais
bloco7_neonatal <- df_indicadores_neonatais
bloco7_evitaveis_neonatal <- df_evitaveis_neonatais
bloco7_principais_neonatal <- df_principais_neonatais

### Para morbidade neonatal
bloco7_morbidade_neonatal <- df_indicadores_morbidade_neonatal
bloco7_principais_morbidade_neonatal <- df_principais_morbidade_neonatal


# Criando os RDAs, que são carregados ao dar o load_all -----------------------
df_cnes_aux <- df_cnes_aux |>
  dplyr::select(cnes:macro_r_saude, tipo) |>
  dplyr::group_by_at(dplyr::vars(!tipo)) |>
  dplyr::summarise(
    tipo = dplyr::last(tipo),
    .groups = "drop"
  )

usethis::use_data(df_cnes_aux, overwrite = TRUE)
usethis::use_data(tabela_indicadores, overwrite = TRUE)
usethis::use_data(indicadores_perfil_sociodemografico, overwrite = TRUE)
usethis::use_data(indicadores_perfil_obstetrico, overwrite = TRUE)
usethis::use_data(estados_choices, overwrite = TRUE)
usethis::use_data(r_saude_choices, overwrite = TRUE)
usethis::use_data(municipios_choices, overwrite = TRUE)
usethis::use_data(hospitais_choices, overwrite = TRUE)
usethis::use_data(df_visao_geral, overwrite = TRUE)
usethis::use_data(df_perfil_sociodemografico, overwrite = TRUE)
usethis::use_data(df_perfil_obstetrico, overwrite = TRUE)
usethis::use_data(bloco3, overwrite = TRUE)
usethis::use_data(bloco4, overwrite = TRUE)
usethis::use_data(bloco5, overwrite = TRUE)
usethis::use_data(tabela_anomalias_prioritarias, overwrite = TRUE)
usethis::use_data(bloco6, overwrite = TRUE)
usethis::use_data(bloco7_fetal, overwrite = TRUE)
usethis::use_data(bloco7_evitaveis_fetal, overwrite = TRUE)
usethis::use_data(bloco7_principais_fetal, overwrite = TRUE)
usethis::use_data(bloco7_perinatal, overwrite = TRUE)
usethis::use_data(bloco7_evitaveis_perinatal, overwrite = TRUE)
usethis::use_data(bloco7_principais_perinatal, overwrite = TRUE)
usethis::use_data(bloco7_neonatal, overwrite = TRUE)
usethis::use_data(bloco7_evitaveis_neonatal, overwrite = TRUE)
usethis::use_data(bloco7_principais_neonatal, overwrite = TRUE)
usethis::use_data(bloco7_morbidade_neonatal, overwrite = TRUE)
usethis::use_data(bloco7_principais_morbidade_neonatal, overwrite = TRUE)
