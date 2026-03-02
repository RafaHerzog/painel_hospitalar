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
  dplyr::select(sub_bloco, sub_bloco_por_extenso, indicador_principal, nome_abreviado_indicador, categoria_sub_indicador, sub_indicador) |>
  unique()

indicadores_perfil_obstetrico <- tabela_indicadores |>
  dplyr::filter(bloco == "perfil_obstetrico") |>
  dplyr::select(sub_bloco, sub_bloco_por_extenso, indicador_principal, nome_abreviado_indicador, categoria_sub_indicador, sub_indicador) |>
  unique()

indicadores_indicadores_assistenciais <- tabela_indicadores |>
  dplyr::filter(bloco == "indicadores_assistenciais") |>
  dplyr::select(sub_bloco, sub_bloco_por_extenso, indicador_principal, nome_abreviado_indicador, categoria_sub_indicador, sub_indicador) |>
  unique()

indicadores_indicadores_assistencia_ao_parto <- tabela_indicadores |>
  dplyr::filter(bloco == "indicadores_assistencia_ao_parto") |>
  dplyr::select(sub_bloco, sub_bloco_por_extenso, indicador_principal, nome_abreviado_indicador, categoria_sub_indicador, sub_indicador) |>
  unique()

indicadores_perfil_dos_nascimentos <- tabela_indicadores |>
  dplyr::filter(bloco == "perfil_dos_nascimentos") |>
  dplyr::select(sub_bloco, sub_bloco_por_extenso, indicador_principal, nome_abreviado_indicador, categoria_sub_indicador, sub_indicador) |>
  unique()

indicadores_morbimortalidade_materna <- tabela_indicadores |>
  dplyr::filter(bloco == "morbimortalidade_materna") |>
  dplyr::select(sub_bloco, sub_bloco_por_extenso, indicador_principal, nome_abreviado_indicador, categoria_sub_indicador, sub_indicador) |>
  unique()

indicadores_morbimortalidade_perinatal <- tabela_indicadores |>
  dplyr::filter(bloco == "morbimortalidade_perinatal") |>
  dplyr::select(sub_bloco, sub_bloco_por_extenso, indicador_principal, nome_abreviado_indicador, categoria_sub_indicador, sub_indicador) |>
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


## Leitura das bases
### Criando uma função mestre para carregar e processar arquivos
processa_arquivos <- function(nome_arquivo, cnes_col = NULL) {

  # 1. Construção do caminho de forma robusta
  path <- here::here("data-raw", "csv", paste0(nome_arquivo, "_2018_2023.csv"))

  # 2. Leitura e limpeza inicial
  dados <- data.table::fread(path) |>
    dplyr::select(!any_of("categoria_porte"))

  # 3. Tratamento específico para colunas de CNES/Estabelecimento
  if (!is.null(cnes_col)) {
    dados <- dados |> dplyr::rename(cnes = all_of(cnes_col))
  }

  # 4. Join padronizado
  df_cnes_aux |>
    dplyr::rename(codmunocor = codufmun) |>
    dplyr::left_join(dados)
}

### Base contendo as variáveis do SINASC
df_indicadores_sinasc <- processa_arquivos("df_indicadores_sinasc")

### Bases contendo as variáveis de morbimortalidade materna
df_indicadores_mortalidade_materna <- processa_arquivos("df_indicadores_mortalidade_materna")
df_indicadores_morbidade_materna <- processa_arquivos("df_indicadores_morbidade_materna")
df_indicadores_internacoes_obstetricas <- processa_arquivos("df_indicadores_internacoes_obstetricas")

### Bases contendo as variáveis de mortalidade fetal
df_indicadores_fetais <- processa_arquivos("df_indicadores_fetais")
df_evitaveis_fetais <- processa_arquivos("df_causas_evitaveis_fetais", cnes_col = "codestab")
df_principais_fetais <- processa_arquivos("df_causas_principais_fetais", cnes_col = "codestab")

### Bases contendo as variáveis de mortalidade neonatal
df_indicadores_neonatais <- processa_arquivos("df_indicadores_neonatais")
df_evitaveis_neonatais <- processa_arquivos("df_causas_evitaveis_neonatais", cnes_col = "codestab")
df_principais_neonatais <- processa_arquivos("df_causas_principais_neonatais", cnes_col = "codestab")

### Bases contendo as variáveis de mortalidade perinatal
df_indicadores_perinatais <- processa_arquivos("df_indicadores_perinatais")
df_evitaveis_perinatais <- processa_arquivos("df_causas_evitaveis_perinatais", cnes_col = "codestab")
df_principais_perinatais <- processa_arquivos("df_causas_principais_perinatais", cnes_col = "codestab")

### Bases contendo as variáveis de morbidade neonatal
df_indicadores_morbidade_neonatal <- processa_arquivos("df_indicadores_morbidade_neonatal")
df_principais_morbidade_neonatal <- processa_arquivos("df_causas_principais_morbidade_neonatal")


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
df_perfil_sociodemografico <- purrr::reduce(
  list(
    df_indicadores_sinasc,
    df_indicadores_internacoes_obstetricas
  ),
  dplyr::left_join
) |>
  dplyr::select(
    cnes:ano,
    tipo:categoria_porte,
    total_de_nascidos_vivos,
    dplyr::starts_with("nvm_idade"),
    dplyr::starts_with("nvm_racacor"),
    dplyr::starts_with("nvm_escolaridade"),
    dplyr::starts_with("nvm_estciv"),
    internacoes_obstetricas_motivo_aborto,
    dplyr::starts_with("internacoes_aborto")
  )

## Para o bloco de "Perfil obstétrico das mulheres atendidas"
df_perfil_obstetrico <- df_indicadores_sinasc |>
  dplyr::select(
    cnes:ano,
    tipo:categoria_porte,
    total_de_nascidos_vivos,
    dplyr::starts_with("nvm_qtdpart"),
    dplyr::starts_with("nvm_qtdpartces"),
    dplyr::starts_with("nvm_gestacao"),
    nvm_trab_parto_induzido,
    dplyr::starts_with("nvm_inicio_prenat"),
    dplyr::starts_with("nvm_num_consultas_prenat_adequado")
  )

## Para o bloco de "Indicadores assistenciais"
df_indicadores_assistenciais <- purrr::reduce(
  list(
    df_indicadores_sinasc |> dplyr::select(cnes:ano, tipo:categoria_porte, total_de_nascidos_vivos),
    df_indicadores_internacoes_obstetricas,
    df_indicadores_morbidade_neonatal
  ),
  dplyr::left_join
) |>
  dplyr::select(
    cnes:ano,
    tipo:categoria_porte,
    total_de_nascidos_vivos,
    total_internacoes_obstetricas,
    dplyr::starts_with("internacoes_obstetricas"),
    total_internacoes_neonatais,
    dplyr::starts_with("internacoes_neonatais") & !dplyr::ends_with("dias")
  )

## Para o bloco de "Indicadores da assistência ao parto"
df_indicadores_assistencia_ao_parto <- df_indicadores_sinasc |>
  dplyr::select(
    cnes:ano,
    tipo:categoria_porte,
    total_de_nascidos_vivos,
    dplyr::starts_with("nvm_parto"),
    nvm_trab_parto_induzido,
    dplyr::starts_with("nvm_robson"),
    dplyr::starts_with("nvm_cesarea_robson"),
    nvm_cesarea
  )

## Para o bloco de "Perfil dos nascimentos"
df_perfil_dos_nascimentos <- df_indicadores_sinasc |>
  dplyr::select(
    cnes:ano,
    tipo:categoria_porte,
    total_de_nascidos_vivos,
    dplyr::starts_with("nv_peso"),
    dplyr::starts_with("nv_baixo_peso"),
    dplyr::starts_with("nv_prematuros"),
    dplyr::starts_with("nv_ig"),
    dplyr::starts_with("nv_anomalia")
  )

## Para o bloco de "Morbimortalidade materna"
df_morbimortalidade_materna <- purrr::reduce(
  list(
    df_indicadores_mortalidade_materna,
    df_indicadores_morbidade_materna
  ),
  dplyr::left_join
) |>
  dplyr::select(
    cnes:ano,
    tipo:categoria_porte,
    total_de_nascidos_vivos,
    dplyr::starts_with("obitos_maternos"),
    total_internacoes,
    dplyr::starts_with("casos_mmg")
  )

## Para o bloco de "Morbimortalidade perinatal"
df_morbimortalidade_perinatal <- purrr::reduce(
  list(
    df_indicadores_fetais |> dplyr::select(cnes:ano, tipo:categoria_porte, total_de_nascidos_vivos, dplyr::contains("todos")),
    df_evitaveis_fetais |> dplyr::select(cnes:ano, tipo:categoria_porte, obitos_fetais_totais, dplyr::contains("total")),
    df_principais_fetais |> dplyr::select(cnes:ano, tipo:categoria_porte, obitos_fetais_totais, dplyr::contains("total")),
    df_indicadores_perinatais |> dplyr::select(cnes:ano, tipo:categoria_porte, total_de_nascidos_vivos, dplyr::contains("todos")),
    df_evitaveis_perinatais |> dplyr::select(cnes:ano, tipo:categoria_porte, obitos_perinatais_totais, dplyr::contains("total")),
    df_principais_perinatais |> dplyr::select(cnes:ano, tipo:categoria_porte, obitos_perinatais_totais, dplyr::contains("total")),
    df_indicadores_neonatais |> dplyr::select(cnes:ano, tipo:categoria_porte, total_de_nascidos_vivos, dplyr::contains("todos")),
    df_evitaveis_neonatais |> dplyr::select(cnes:ano, tipo:categoria_porte, obitos_neonatais_totais, dplyr::contains("total")),
    df_principais_neonatais |> dplyr::select(cnes:ano, tipo:categoria_porte, obitos_neonatais_totais, dplyr::contains("total"))
  ),
  dplyr::left_join
) |>
  dplyr::select(
    cnes:ano,
    tipo:categoria_porte,
    tipo:macro_r_saude,
    total_de_nascidos_vivos,
    obitos_fetais_todos_todos,
    obitos_fetais_totais,
    dplyr::starts_with("obitos_fetais"),
    dplyr::starts_with("evitaveis_fetal"),
    dplyr::starts_with("principais_fetal"),
    obitos_perinatais_todos_todos,
    obitos_perinatais_totais,
    dplyr::starts_with("obitos_perinatais"),
    dplyr::starts_with("evitaveis_perinatal"),
    dplyr::starts_with("principais_perinatal"),
    obitos_neonatais_todos_todos,
    obitos_neonatais_totais,
    dplyr::starts_with("obitos_neonatais"),
    dplyr::starts_with("evitaveis_neonatal"),
    dplyr::starts_with("principais_neonatal")
  )

# ## Para o bloco 4
# bloco4 <- df_indicadores_sinasc |>
#   dplyr::select(
#     cnes:ano,
#     qtd_leitos_nascidos,
#     tipo:macro_r_saude,
#     total_de_nascidos_vivos,
#     dplyr::starts_with("nvm_robson"),
#     dplyr::starts_with("nvm_cesarea"),
#     nv_parto_vaginal,
#     dplyr::starts_with("nv_assist")
#   )
# ## Para o bloco 7
# ### Para óbitos fetais
# bloco7_fetal <- df_indicadores_fetais
# bloco7_evitaveis_fetal <- df_evitaveis_fetais
# bloco7_principais_fetal <- df_principais_fetais
#
# ### Para óbitos perinatais
# bloco7_perinatal <- df_indicadores_perinatais
# bloco7_evitaveis_perinatal <- df_evitaveis_perinatais
# bloco7_principais_perinatal <- df_principais_perinatais
#
# ### Para óbitos neonatais
# bloco7_neonatal <- df_indicadores_neonatais
# bloco7_evitaveis_neonatal <- df_evitaveis_neonatais
# bloco7_principais_neonatal <- df_principais_neonatais
#
# ### Para morbidade neonatal
# bloco7_morbidade_neonatal <- df_indicadores_morbidade_neonatal
# bloco7_principais_morbidade_neonatal <- df_principais_morbidade_neonatal


# Criando o dataframe de incompletude -------------------------------------
## Lendo o arquivo com a incompletude do SINASC
df_incompletude_sinasc <- read.csv("data-raw/csv/df_incompletude_sinasc_2018-2023.csv")

## Lendo o arquivo com a incompletude do SIM
df_incompletude_sim <- read.csv("data-raw/csv/df_incompletude_sim_2018-2023.csv")

## Lendo o arquivo com a incompletude provisória do SIH
df_incompletude_sih <- read.csv("data-raw/csv/df_indicadores_internacoes_obstetricas_2018_2023.csv") |>
  dplyr::select(
    cnes:cod_macro_r_saude,
    total_internacoes_obstetricas,
    internacoes_obstetricas_motivo_aborto,
    ends_with("incompletos")
  )

## Juntando todos os dataframes
df_incompletude <- purrr::reduce(
  list(
    df_incompletude_sinasc,
    df_incompletude_sim,
    df_incompletude_sih
  ),
  dplyr::left_join
)


# Criando os RDAs, que são carregados ao dar o load_all -----------------------
df_cnes_aux <- df_cnes_aux |>
  dplyr::select(cnes:macro_r_saude, tipo, categoria_porte) |>
  dplyr::group_by_at(dplyr::vars(!c(tipo, categoria_porte))) |>
  dplyr::summarise(
    categoria_porte = dplyr::last(categoria_porte),
    tipo = dplyr::last(tipo),
    .groups = "drop"
  )

usethis::use_data(df_cnes_aux, overwrite = TRUE)
usethis::use_data(tabela_indicadores, overwrite = TRUE)
usethis::use_data(indicadores_perfil_sociodemografico, overwrite = TRUE)
usethis::use_data(indicadores_perfil_obstetrico, overwrite = TRUE)
usethis::use_data(indicadores_indicadores_assistenciais, overwrite = TRUE)
usethis::use_data(indicadores_indicadores_assistencia_ao_parto, overwrite = TRUE)
usethis::use_data(indicadores_perfil_dos_nascimentos, overwrite = TRUE)
usethis::use_data(indicadores_morbimortalidade_materna, overwrite = TRUE)
usethis::use_data(indicadores_morbimortalidade_perinatal, overwrite = TRUE)
usethis::use_data(estados_choices, overwrite = TRUE)
usethis::use_data(r_saude_choices, overwrite = TRUE)
usethis::use_data(municipios_choices, overwrite = TRUE)
usethis::use_data(hospitais_choices, overwrite = TRUE)
usethis::use_data(df_visao_geral, overwrite = TRUE)
usethis::use_data(df_perfil_sociodemografico, overwrite = TRUE)
usethis::use_data(df_perfil_obstetrico, overwrite = TRUE)
usethis::use_data(df_indicadores_assistenciais, overwrite = TRUE)
usethis::use_data(df_indicadores_assistencia_ao_parto, overwrite = TRUE)
usethis::use_data(df_perfil_dos_nascimentos, overwrite = TRUE)
usethis::use_data(df_morbimortalidade_materna, overwrite = TRUE)
usethis::use_data(df_morbimortalidade_perinatal, overwrite = TRUE)
usethis::use_data(df_incompletude, overwrite = TRUE)
