library(dplyr)
library(janitor)
library(data.table)
library(stringr)
library(tidyr)

# Preparando a base de indicadores do SINASC para o painel ----------------------
## Lendo o arquivo com os nascimentos em hospital no período de 2018-2023
df_sinasc_nasc_em_hospital <- fread("data-raw/extracao-dos-dados/databases/df_sinasc_nasc_em_hospital_2018_2023.csv.gz")

## Separando a coluna CODANOMAL em várias colunas, colocando um código de anomalia por coluna
### Fazendo isso pois, se um indivíduo nasceu com mais de uma anomalia, os códigos ficam todos juntos
unique(df_sinasc_nasc_em_hospital$CODANOMAL)

### Convertendo para data.table (para que as operações fiquem mais eficientes)
dt <- as.data.table(df_sinasc_nasc_em_hospital)

### Extraindo os códigos como lista e convertendo para string separada por vírgula
dt[, anomalias := sapply(str_extract_all(CODANOMAL, "Q[0-9X]{2,3}"), paste, collapse = ",")]

### Separando em 5 colunas (o número máximo de anomalias em um único registro foi de 5)
dt[, paste0("anomalia_", 1:5) := tstrsplit(anomalias, ",", fixed = TRUE)]

### Removendo a coluna temporária
dt[, anomalias := NULL]

### Convertendo de volta para um dataframe
df_sinasc_nasc_em_hospital <- as.data.frame(dt)

### Removendo o dataframe convertido e limpando a memória
rm(dt)
gc()

### Criando um vetor contendo os códigos das anomalias prioritárias para vigilância
codigos_anomalias_prioritarias <- c('Q000', 'Q001', 'Q002',
                                    'Q01X','Q01', 'Q010', 'Q011', 'Q012', 'Q013', 'Q014', 'Q015', 'Q016', 'Q017', 'Q018', 'Q019',
                                    'Q05X','Q05', 'Q050', 'Q051', 'Q052', 'Q053', 'Q054', 'Q055', 'Q056', 'Q057', 'Q058', 'Q059',
                                    'Q02X', 'Q02', 'Q020', 'Q021', 'Q022', 'Q023', 'Q024', 'Q025', 'Q026', 'Q027', 'Q028', 'Q029',
                                    'Q20X', 'Q20', 'Q200', 'Q201', 'Q202', 'Q203', 'Q204', 'Q205', 'Q206', 'Q207', 'Q208', 'Q209',
                                    'Q21X', 'Q21', 'Q210', 'Q211', 'Q212', 'Q213', 'Q214', 'Q215', 'Q216', 'Q217', 'Q218', 'Q219',
                                    'Q22X', 'Q22', 'Q220', 'Q221', 'Q222', 'Q223', 'Q224', 'Q225', 'Q226', 'Q227', 'Q228', 'Q229',
                                    'Q23X', 'Q23', 'Q230', 'Q231', 'Q232', 'Q233', 'Q234', 'Q235', 'Q236', 'Q237', 'Q238', 'Q239',
                                    'Q24X', 'Q24', 'Q240', 'Q241', 'Q242', 'Q243', 'Q244', 'Q245', 'Q246', 'Q247', 'Q248', 'Q249',
                                    'Q25X', 'Q25', 'Q250', 'Q251', 'Q252', 'Q253', 'Q254', 'Q255', 'Q256', 'Q257', 'Q258', 'Q259',
                                    'Q26X', 'Q26', 'Q260', 'Q261', 'Q262', 'Q263', 'Q264', 'Q265', 'Q266', 'Q267', 'Q268', 'Q269',
                                    'Q27X', 'Q27', 'Q270', 'Q271', 'Q272', 'Q273', 'Q274', 'Q275', 'Q276', 'Q277', 'Q278', 'Q279',
                                    'Q28X', 'Q28', 'Q280', 'Q281', 'Q282', 'Q283', 'Q284', 'Q285', 'Q286', 'Q287', 'Q288', 'Q289',
                                    'Q35X', 'Q35', 'Q350', 'Q351', 'Q352', 'Q353', 'Q354', 'Q355', 'Q356', 'Q357', 'Q358', 'Q359',
                                    'Q36X', 'Q36', 'Q360', 'Q361', 'Q362', 'Q363', 'Q364', 'Q365', 'Q366', 'Q367', 'Q368', 'Q369',
                                    'Q37X', 'Q37', 'Q370', 'Q371', 'Q372', 'Q373', 'Q374', 'Q375', 'Q376', 'Q377', 'Q378', 'Q379',
                                    'Q54X', 'Q54', 'Q540', 'Q541', 'Q542', 'Q543', 'Q544', 'Q545', 'Q546', 'Q547', 'Q548', 'Q549',
                                    'Q56X', 'Q56', 'Q560', 'Q561', 'Q562', 'Q563', 'Q564', 'Q565', 'Q566', 'Q567', 'Q568', 'Q569',
                                    'Q66X', 'Q66', 'Q660', 'Q661', 'Q662', 'Q663', 'Q664', 'Q665', 'Q666', 'Q667', 'Q668', 'Q669',
                                    'Q69X', 'Q69', 'Q690', 'Q691', 'Q692', 'Q693', 'Q694', 'Q695', 'Q696', 'Q697', 'Q698', 'Q699',
                                    'Q71X', 'Q71', 'Q710', 'Q711', 'Q712', 'Q713', 'Q714', 'Q715', 'Q716', 'Q717', 'Q718', 'Q719',
                                    'Q72X', 'Q72', 'Q720', 'Q721', 'Q722', 'Q723', 'Q724', 'Q725', 'Q726', 'Q727', 'Q728', 'Q729',
                                    'Q73X', 'Q73', 'Q730', 'Q731', 'Q732', 'Q733', 'Q734', 'Q735', 'Q736', 'Q737', 'Q738', 'Q739',
                                    'Q743', 'Q792', 'Q793',
                                    'Q90X', 'Q90', 'Q900', 'Q901', 'Q902', 'Q903', 'Q904', 'Q905', 'Q906', 'Q907', 'Q908', 'Q909')

## Lendo uma base auxiliar de municípios, que utilizaremos para criar variáveis necessárias para o cálculo de alguns indicadores
df_aux_municipios <- read.csv("data-raw/extracao-dos-dados/databases/tabela_aux_municipios.csv") |>
  select(codmun, cod_macro_r_saude)

## Fazendo as manipulações necessárias e criando as variáveis de interesse
df_indicadores_sinasc <- df_sinasc_nasc_em_hospital |>
  clean_names() |>
  # Juntando com a base auxiliar de municípios para obter a macrorregião de saúde do mun. de nascimento
  left_join(df_aux_municipios |> rename(cod_macro_r_saude_nasc = cod_macro_r_saude), by = join_by(codmunnasc == codmun)) |>
  # Juntando com a base auxiliar de municípios para obter a macrorregião de saúde do mun. de residência
  left_join(df_aux_municipios |> rename(cod_macro_r_saude_res = cod_macro_r_saude), by = join_by(codmunres == codmun)) |>
  mutate(
    # Garantindo que as variáveis são do tipo correto
    idademae = as.numeric(idademae),
    racacormae = as.numeric(racacormae),
    escmae = as.numeric(escmae),
    estcivmae = as.numeric(estcivmae),
    qtdpartnor = as.numeric(qtdpartnor),
    qtdpartces = as.numeric(qtdpartces),
    gravidez = as.numeric(gravidez),
    mesprenat = as.numeric(mesprenat),
    semagestac = as.numeric(semagestac),
    consprenat = as.numeric(consprenat),
    peso = as.numeric(peso),
    apgar5 = as.numeric(apgar5),
    idanomal = as.numeric(idanomal),
    tprobson = as.numeric(tprobson),
    parto = as.numeric(parto),
    stcesparto = as.numeric(stcesparto),
    sttrabpart = as.numeric(sttrabpart),
    tpnascassi = as.numeric(tpnascassi)
  ) |>
  # Criando as variáveis necessárias para o cálculo dos indicadores
  mutate(
    total_de_nascidos_vivos = 1,
    # Idade da mãe
    nvm_idade_10_a_14_anos = if_else(idademae >= 10 & idademae <= 14, 1, 0, missing = 0),
    nvm_idade_15_a_19_anos = if_else(idademae >= 15 & idademae <= 19, 1, 0, missing = 0),
    nvm_idade_20_a_34_anos = if_else(idademae >= 20 & idademae <= 34, 1, 0, missing = 0),
    nvm_idade_35_a_39_anos = if_else(idademae >= 35 & idademae <= 39, 1, 0, missing = 0),
    nvm_idade_40_mais_anos = if_else(idademae >= 40 & idademae < 99, 1, 0, missing = 0),

    # Raça/cor da mãe
    nvm_racacor_branca = if_else(racacormae == 1, 1, 0, missing = 0),
    nvm_racacor_preta = if_else(racacormae == 2, 1, 0, missing = 0),
    nvm_racacor_parda = if_else(racacormae == 4, 1, 0, missing = 0),
    nvm_racacor_amarela = if_else(racacormae == 3, 1, 0, missing = 0),
    nvm_racacor_indigena = if_else(racacormae == 5, 1, 0, missing = 0),

    # Escolaridade da mãe
    nvm_escolaridade_3_menos_anos = if_else(escmae == 1 | escmae == 2, 1, 0, missing = 0),
    nvm_escolaridade_4_a_7_anos = if_else(escmae == 3, 1, 0, missing = 0),
    nvm_escolaridade_8_a_11_anos = if_else(escmae == 4, 1, 0, missing = 0),
    nvm_escolaridade_11_mais_anos = if_else(escmae == 5, 1, 0, missing = 0),

    # Situação conjugal
    nvm_estciv_casada_uniao = if_else(estcivmae %in% c(2, 5), 1, 0, missing = 0),
    nvm_estciv_solteira_viuva_separada = if_else(estcivmae %in% c(1, 3, 4), 1, 0, missing = 0),

    # Município de residência
    nvm_munres_mesmo_muni_hospital = if_else(codmunres == codmunnasc, 1, 0, missing = 0),
    nvm_munres_mesma_macro_hospital = if_else(cod_macro_r_saude_res == cod_macro_r_saude_nasc, 1, 0, missing = 0),
    nvm_munres_fora_macro_hospital = if_else(cod_macro_r_saude_res != cod_macro_r_saude_nasc, 1, 0, missing = 0),
    nvm_munres_fora_uf_hospital = if_else(substr(codmunres, 1, 2) != substr(codmunnasc, 1, 2), 1, 0, missing = 0),

    # Número de partos anteriores
    nvm_qtdpart_0_partos_anteriores = if_else(rowSums(cbind(qtdpartnor, qtdpartces), na.rm = TRUE) == 0, 1, 0, missing = 0),
    nvm_qtdpart_1_partos_anteriores = if_else(rowSums(cbind(qtdpartnor, qtdpartces), na.rm = TRUE) == 1, 1, 0, missing = 0),
    nvm_qtdpart_2_partos_anteriores = if_else(rowSums(cbind(qtdpartnor, qtdpartces), na.rm = TRUE) == 2, 1, 0, missing = 0),
    nvm_qtdpart_3_mais_partos_anteriores = if_else(rowSums(cbind(qtdpartnor, qtdpartces), na.rm = TRUE) >= 3, 1, 0, missing = 0),

    # Cesariana anterior
    nvm_qtdpartces_0_cesareas_anteriores = if_else(qtdpartces == 0, 1, 0, missing = 0),
    nvm_qtdpartces_1_cesareas_anteriores = if_else(qtdpartces == 1, 1, 0, missing = 0),
    nvm_qtdpartces_2_mais_cesareas_anteriores = if_else(qtdpartces >= 2, 1, 0, missing = 0),

    # Tipo de parto
    nvm_parto_vaginal = if_else(parto == 1, 1, 0, missing = 0),
    nvm_parto_cesariana_anteparto = if_else(parto == 2 & stcesparto == 1, 1, 0, missing = 0),
    nvm_parto_cesariana_intraparto = if_else(parto == 2 & stcesparto %in% c(2, 9), 1, 0, missing = 0),

    # Tipo de gestação
    nvm_gestacao_unica = if_else(gravidez == 1, 1, 0, missing = 0),
    nvm_gestacao_multipla = if_else(gravidez %in% c(2, 3), 1, 0, missing = 0),

    # Parto vaginal pós cesariana
    nvm_parto_vaginal_pos_cesarea = if_else(parto == 1 & qtdpartces > 0, 1, 0, missing = 0),

    # Trabalho de parto induzido
    nvm_trab_parto_induzido = if_else(sttrabpart == 1, 1, 0, missing = 0),

    # Início do pré-natal
    nvm_inicio_prenat_12_menos_semanas = if_else(mesprenat >= 1 & mesprenat <= 3, 1, 0, missing = 0),
    nvm_inicio_prenat_13_a_20_semanas = if_else(mesprenat >= 4 & mesprenat <= 5, 1, 0, missing = 0),
    nvm_inicio_prenat_mais_20_semanas = if_else(mesprenat >= 6 & mesprenat <= 9, 1, 0, missing = 0),

    # Nº adequado de consultas de pré-natal
    nvm_num_consultas_prenat_adequado = if_else(
      ((semagestac < 20 & consprenat >= 1) |
         (semagestac >= 20 & semagestac < 26 & consprenat >= 2) |
         (semagestac >= 26 & semagestac < 30 & consprenat >= 3) |
         (semagestac >= 30 & semagestac < 34 & consprenat >= 4) |
         (semagestac >= 34 & semagestac < 36 & consprenat >= 5) |
         (semagestac >= 36 & semagestac < 38 & consprenat >= 6) |
         (semagestac >= 38 & semagestac < 40 & consprenat >= 7) |
         (semagestac >= 40 & semagestac < 99 & consprenat >= 8 & consprenat < 99)),
      1, 0, missing = 0
    ),

    # Duração da gestação
    nv_ig_menos_37_semanas = if_else(semagestac < 37, 1, 0, missing = 0),
    nv_ig_37_a_38_semanas = if_else(semagestac >= 37 & semagestac <= 38, 1, 0, missing = 0),
    nv_ig_39_a_40_semanas = if_else(semagestac >= 39 & semagestac <= 40, 1, 0, missing = 0),
    nv_ig_41_semanas = if_else(semagestac == 41, 1, 0, missing = 0),
    nv_ig_42_mais_semanas = if_else(semagestac >= 42 & semagestac < 99, 1, 0, missing = 0),

    # Tipo de prematuridade
    nv_prematuros = if_else(gestacao < 5, 1, 0, missing = 0),
    nv_prematuros_menos_28_semanas = if_else(semagestac < 28, 1, 0, missing = 0),
    nv_prematuros_28_a_32_semanas = if_else(semagestac >= 28 & semagestac <= 32, 1, 0, missing = 0),
    nv_prematuros_33_a_34_semanas = if_else(semagestac %in% c(33, 34), 1, 0, missing = 0),
    nv_prematuros_35_a_36_semanas = if_else(semagestac %in% c(35, 36), 1, 0, missing = 0),

    # Peso ao nascer
    nv_peso_menor_2500 = if_else(peso < 2500, 1, 0, missing = 0),
    nv_peso_2500_a_4499 = if_else(peso >= 2500 & peso < 4500, 1, 0, missing = 0),
    nv_peso_4500_mais = if_else(peso >= 4500, 1, 0, missing = 0),

    # Tipo de baixo peso
    nv_baixo_peso = if_else(peso < 2500, 1, 0, missing = 0),
    nv_baixo_peso_menor_1000 = if_else(peso < 1000, 1, 0, missing = 0),
    nv_baixo_peso_1000_a_1499 = if_else(peso >= 1000 & peso < 1500, 1, 0, missing = 0),
    nv_baixo_peso_1500_a_2499 = if_else(peso >= 1500 & peso < 2500, 1, 0, missing = 0),

    # Asfixia
    nv_asfixia_sem_anomalia_2500g_mais = if_else(
      apgar5 < 7 & peso >= 2500 &
        ((idanomal == 2) | ((idanomal == '' | is.na(idanomal)) & (codanomal == '' | is.na(codanomal)))),
      1, 0 , missing = 0
    ),
    nv_sem_anomalia_2500g_mais = if_else(
      peso >= 2500 &
        ((idanomal == 2) | ((idanomal == '' | is.na(idanomal)) & (codanomal == '' | is.na(codanomal)))),
      1, 0 , missing = 0
    ),

    # Anomalias congênitas
    nv_anomalia = if_else((idanomal == 1 | (!is.na(codanomal) & codanomal != "")), 1, 0, missing = 0),

    # Grupos de Robson
    nvm_robson_1 = if_else(tprobson == 1, 1, 0, missing = 0),
    nvm_robson_2 = if_else(tprobson == 2, 1, 0, missing = 0),
    nvm_robson_2a = if_else(tprobson == 2 & sttrabpart == 1, 1, 0, missing = 0),
    nvm_robson_2b = if_else(tprobson == 2 & stcesparto == 1, 1, 0, missing = 0),
    nvm_robson_3 = if_else(tprobson == 3, 1, 0, missing = 0),
    nvm_robson_4 = if_else(tprobson == 4, 1, 0, missing = 0),
    nvm_robson_4a = if_else(tprobson == 4 & sttrabpart == 1, 1, 0, missing = 0),
    nvm_robson_4b = if_else(tprobson == 4 & stcesparto == 1, 1, 0, missing = 0),
    nvm_robson_5 = if_else(tprobson == 5, 1, 0, missing = 0),
    nvm_robson_6_a_9 = if_else(tprobson >= 6 & tprobson <= 9, 1, 0, missing = 0),
    nvm_robson_10 = if_else(tprobson == 10, 1, 0, missing = 0),
    nvm_cesarea = if_else(parto == 2, 1, 0, missing = 0),
    nvm_cesarea_robson_1 = if_else(tprobson == 1 & parto == 2, 1, 0, missing = 0),
    nvm_cesarea_robson_2 = if_else(tprobson == 2 & parto == 2, 1, 0, missing = 0),
    nvm_cesarea_robson_2a = if_else(tprobson == 2 & parto == 2 & sttrabpart == 1, 1, 0, missing = 0),
    nvm_cesarea_robson_2b = if_else(tprobson == 2 & parto == 2 & stcesparto == 1, 1, 0, missing = 0),
    nvm_cesarea_robson_3 = if_else(tprobson == 3 & parto == 2, 1, 0, missing = 0),
    nvm_cesarea_robson_4 = if_else(tprobson == 4 & parto == 2, 1, 0, missing = 0),
    nvm_cesarea_robson_4a = if_else(tprobson == 4 & parto == 2 & sttrabpart == 1, 1, 0, missing = 0),
    nvm_cesarea_robson_4b = if_else(tprobson == 4 & parto == 2 & stcesparto == 1, 1, 0, missing = 0),
    nvm_cesarea_robson_5 = if_else(tprobson == 5 & parto == 2, 1, 0, missing = 0),
    nvm_cesarea_robson_6_a_9 = if_else(tprobson >= 6 & tprobson <= 9 & parto == 2, 1, 0, missing = 0),
    nvm_cesarea_robson_10 = if_else(tprobson == 10 & parto == 2, 1, 0, missing = 0),

    # Profissional que assistiu o parto
    nv_parto_vaginal = if_else(parto == 1, 1, 0, missing = 0),
    nv_assist_medico = if_else(parto == 1 & tpnascassi == 1, 1, 0, missing = 0),
    nv_assist_enf_obs = if_else(parto == 1 & tpnascassi == 2, 1, 0, missing = 0),
    nv_assist_parteira = if_else(parto == 1 & tpnascassi == 3, 1, 0, missing = 0),
    nv_assist_outros = if_else(parto == 1 & tpnascassi == 4, 1, 0, missing = 0)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    # Anomalias congênitas prioritárias para vigilância
    nv_anomalia_prioritaria = ifelse(
      any(c(anomalia_1, anomalia_2, anomalia_3, anomalia_4, anomalia_5) %in% codigos_anomalias_prioritarias),
      1,
      0
    )
  ) |>
  ungroup() |>
  group_by(codestab, codmunnasc, mes, ano) |>
  summarise_at(vars(starts_with("total_") | starts_with("nv")), sum) |>
  ungroup()

## Juntando com a base auxiliar de CNES
df_cnes_aux <- read.csv("data-raw/extracao-dos-dados/databases/df_cnes_aux.csv") |>
  select(cnes:nome_fantasia, mes, ano)

df_indicadores_sinasc_final <- left_join(
  df_cnes_aux,
  df_indicadores_sinasc,
  by = join_by(cnes == codestab, codufmun == codmunnasc, mes, ano)
) |>
  rename(codmunnasc = codufmun) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  arrange(cnes, codmunnasc, mes, ano)

## Salvando a base final
write.csv(df_indicadores_sinasc_final, "data-raw/csv/df_indicadores_sinasc_2018_2023.csv", row.names = FALSE)


# # Criando uma base separada para a tabela de anomalias prioritárias -----------
# ## Criando um dataframe com o total de nascidos vivos
# df_anomalias <- df_sinasc_nasc_em_hospital |>
#   group_by(CODESTAB, CODMUNNASC, nome_fantasia, mes, ano, anomalia_1, anomalia_2, anomalia_3, anomalia_4, anomalia_5) |>
#   summarise(total_de_nascidos_vivos = n()) |>
#   clean_names() |>
#   left_join(
#     df_cnes_aux,
#     by = join_by(codestab == cnes, codmunnasc == codufmun, mes, ano)
#   ) |>
#   arrange(codestab, codmunnasc, nome_fantasia, mes, ano) |>
#   filter(!is.na(municipio))
#
# ## Filtrando pelas linhas em que ocorre uma anomalia prioritária
# df_anomalias_prioritarias <- df_anomalias |>
#   filter(str_detect(anomalia_1, paste(codigos_anomalias_prioritarias, collapse = "|")) |
#            str_detect(anomalia_2, paste(codigos_anomalias_prioritarias, collapse = "|")) |
#            str_detect(anomalia_3, paste(codigos_anomalias_prioritarias, collapse = "|")) |
#            str_detect(anomalia_4, paste(codigos_anomalias_prioritarias, collapse = "|")) |
#            str_detect(anomalia_5, paste(codigos_anomalias_prioritarias, collapse = "|")))
#
# ## Passando a base para o formato long
# df_anomalias_prioritarias_long <- df_anomalias_prioritarias |>
#   gather(coluna_anomalia, codigo_anomalia, anomalia_1:anomalia_5) |>
#   filter(str_detect(codigo_anomalia, paste0("^(", paste(codigos_anomalias_prioritarias, collapse = "|"), ")")))
#
# ## Transformando alguns códigos de anomalias
# codigos_a_transformar <- c("Q01", "Q05", "Q02", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28", "Q35", "Q36", "Q37", "Q54", "Q56", "Q66", "Q69", "Q71", "Q72", "Q73", "Q90")
#
# df_anomalias_prioritarias_long$codigo_cid <- ifelse(
#   substr(df_anomalias_prioritarias_long$codigo_anomalia, 1, 3) %in% codigos_a_transformar,
#   substr(df_anomalias_prioritarias_long$codigo_anomalia, 1, 3),
#   df_anomalias_prioritarias_long$codigo_anomalia
# )
#
# ## Criando vetores com os códigos para cada grupo de anomalias congênitas
# defeitos_de_tubo_neural <- c('Q000', 'Q001', 'Q002',
#                              'Q01X','Q01', 'Q010', 'Q011', 'Q012', 'Q013', 'Q014', 'Q015', 'Q016', 'Q017', 'Q018', 'Q019',
#                              'Q05X','Q05','Q050', 'Q051', 'Q052', 'Q053', 'Q054', 'Q055', 'Q056', 'Q057', 'Q058', 'Q059')
# microcefalia <- c('Q02X','Q02','Q020', 'Q021', 'Q022', 'Q023', 'Q024', 'Q025', 'Q026', 'Q027', 'Q028', 'Q029')
# cardiopatias_congenitas <- c(  'Q20X','Q20','Q200', 'Q201', 'Q202', 'Q203', 'Q204', 'Q205', 'Q206', 'Q207', 'Q208', 'Q209',
#                                'Q21X','Q21','Q210', 'Q211', 'Q212', 'Q213', 'Q214', 'Q215', 'Q216', 'Q217', 'Q218', 'Q219',
#                                'Q22X','Q22','Q220', 'Q221', 'Q222', 'Q223', 'Q224', 'Q225', 'Q226', 'Q227', 'Q228', 'Q229',
#                                'Q23X','Q23','Q230', 'Q231', 'Q232', 'Q233', 'Q234', 'Q235', 'Q236', 'Q237', 'Q238', 'Q239',
#                                'Q24X','Q24','Q240', 'Q241', 'Q242', 'Q243', 'Q244', 'Q245', 'Q246', 'Q247', 'Q248', 'Q249',
#                                'Q25X','Q25','Q250', 'Q251', 'Q252', 'Q253', 'Q254', 'Q255', 'Q256', 'Q257', 'Q258', 'Q259',
#                                'Q26X','Q26','Q260', 'Q261', 'Q262', 'Q263', 'Q264', 'Q265', 'Q266', 'Q267', 'Q268', 'Q269',
#                                'Q27X','Q27','Q270', 'Q271', 'Q272', 'Q273', 'Q274', 'Q275', 'Q276', 'Q277', 'Q278', 'Q279',
#                                'Q28X','Q28','Q280', 'Q281', 'Q282', 'Q283', 'Q284', 'Q285', 'Q286', 'Q287', 'Q288', 'Q289')
# fendas_orais <- c('Q35X','Q35','Q350', 'Q351', 'Q352', 'Q353', 'Q354', 'Q355', 'Q356', 'Q357', 'Q358', 'Q359',
#                   'Q36X','Q36','Q360', 'Q361', 'Q362', 'Q363', 'Q364', 'Q365', 'Q366', 'Q367', 'Q368', 'Q369',
#                   'Q37X','Q37','Q370', 'Q371', 'Q372', 'Q373', 'Q374', 'Q375', 'Q376', 'Q377', 'Q378', 'Q379')
# defeitos_de_orgaos_genitais <- c('Q54X','Q54','Q540', 'Q541', 'Q542', 'Q543', 'Q544', 'Q545', 'Q546', 'Q547', 'Q548', 'Q549',
#                                  'Q56X','Q56','Q560', 'Q561', 'Q562', 'Q563', 'Q564', 'Q565', 'Q566', 'Q567', 'Q568', 'Q569')
# defeitos_de_membros <- c('Q66X','Q66', 'Q660', 'Q661', 'Q662', 'Q663', 'Q664', 'Q665', 'Q666', 'Q667', 'Q668', 'Q669',
#                          'Q69X','Q69', 'Q690', 'Q691', 'Q692', 'Q693', 'Q694', 'Q695', 'Q696', 'Q697', 'Q698', 'Q699',
#                          'Q71X','Q71', 'Q710', 'Q711', 'Q712', 'Q713', 'Q714', 'Q715', 'Q716', 'Q717', 'Q718', 'Q719',
#                          'Q72X','Q72', 'Q720', 'Q721', 'Q722', 'Q723', 'Q724', 'Q725', 'Q726', 'Q727', 'Q728', 'Q729',
#                          'Q73X','Q73', 'Q730', 'Q731', 'Q732', 'Q733', 'Q734', 'Q735', 'Q736', 'Q737', 'Q738', 'Q739',
#                          'Q743')
# defeitos_de_parede_abdominal <- c('Q792', 'Q793')
# sindrome_de_down <- c('Q90X','Q90','Q900', 'Q901', 'Q902', 'Q903', 'Q904', 'Q905', 'Q906', 'Q907', 'Q908', 'Q909')
#
# ## Criando uma função que retorna a descrição do grupo de anomalias com base em seu código
# map_grupo <- function(codigo_anomalia) {
#   # Converter para maiúsculas para tornar a correspondência sem distinção entre maiúsculas e minúsculas
#   if (substr(codigo_anomalia, 1, 4) %in% defeitos_de_tubo_neural) {
#     return('Defeitos de tubo neural')
#   } else if (substr(codigo_anomalia, 1, 4) %in% microcefalia) {
#     return('Microcefalia')
#   } else if (substr(codigo_anomalia, 1, 4) %in% cardiopatias_congenitas) {
#     return('Cardiopatias congênitas')
#   } else if (substr(codigo_anomalia, 1, 4) %in% fendas_orais) {
#     return('Fendas orais')
#   } else if (substr(codigo_anomalia, 1, 4) %in% defeitos_de_orgaos_genitais) {
#     return('Defeitos de órgãos genitais')
#   } else if (substr(codigo_anomalia, 1, 4) %in% defeitos_de_membros) {
#     return('Defeitos de membros')
#   } else if (substr(codigo_anomalia, 1, 4) %in% defeitos_de_parede_abdominal) {
#     return('Defeitos da parede abdominal')
#   } else if (substr(codigo_anomalia, 1, 4) %in% sindrome_de_down) {
#     return('Síndrome de Down')
#   } else {
#     return(NA)
#   }
# }
#
# ## Aplicando essa função ao dataframe long
# df_anomalias_prioritarias_long$grupo_de_anomalias_congenitas <- sapply(df_anomalias_prioritarias_long$codigo_anomalia, map_grupo)
#
# ## Para adicionar a descrição da anomalia: criando vetores para cada tipo de anomalia
# Anencefalia <- c('Q000')
# Craniorraquisquise <- c('Q001')
# Iniencefalia <- c('Q002')
# Encefalocele <- c('Q01X','Q010', 'Q011', 'Q012', 'Q013', 'Q014', 'Q015', 'Q016', 'Q017', 'Q018', 'Q019')
# Espinha_bifida <- c('Q05X', 'Q050', 'Q051', 'Q052', 'Q053', 'Q054', 'Q055', 'Q056', 'Q057', 'Q058', 'Q059')
# Microcefalia <- c('Q02X','Q02','Q020', 'Q021', 'Q022', 'Q023', 'Q024', 'Q025', 'Q026', 'Q027', 'Q028', 'Q029')
# Malformacoes_congenitas_das_camaras_e_das_comunicacoes_cardiacas <- c('Q200', 'Q201', 'Q202', 'Q203', 'Q204', 'Q205', 'Q206', 'Q207', 'Q208', 'Q209')
# Malformacoes_congenitas_dos_septos_cardiacos <- c('Q21X','Q210', 'Q211', 'Q212', 'Q213', 'Q214', 'Q215', 'Q216', 'Q217', 'Q218', 'Q219')
# Malformacoes_congenitas_das_valvas_pulmonar_e_tricuspide <- c('Q220', 'Q221', 'Q222', 'Q223', 'Q224', 'Q225', 'Q226', 'Q227', 'Q228', 'Q229')
# Malformacoes_congenitas_das_valvas_aortica_e_mitral <- c('Q230', 'Q231', 'Q232', 'Q233', 'Q234', 'Q235', 'Q236', 'Q237', 'Q238', 'Q239')
# Outras_malformacoes_congenitas_do_coracao <- c('Q24X','Q240', 'Q241', 'Q242', 'Q243', 'Q244', 'Q245', 'Q246', 'Q247', 'Q248', 'Q249')
# Malformacoes_congenitas_das_grandes_arterias <- c('Q250', 'Q251', 'Q252', 'Q253', 'Q254', 'Q255', 'Q256', 'Q257', 'Q258', 'Q259')
# Malformacoes_congenitas_das_grandes_veias <- c('Q260', 'Q261', 'Q262', 'Q263', 'Q264', 'Q265', 'Q266', 'Q267', 'Q268', 'Q269')
# Outras_malformacoes_congenitas_do_sistema_vascular_periferico <- c('Q270', 'Q271', 'Q272', 'Q273', 'Q274', 'Q275', 'Q276', 'Q277', 'Q278', 'Q279')
# Outras_malformacoes_congenitas_do_aparelho_circulatorio <- c('Q280', 'Q281', 'Q282', 'Q283', 'Q284', 'Q285', 'Q286', 'Q287', 'Q288', 'Q289')
# Fenda_palatina <- c('Q35X', 'Q350', 'Q351', 'Q352', 'Q353', 'Q354', 'Q355', 'Q356', 'Q357', 'Q358', 'Q359')
# Fenda_labial <- c('Q360', 'Q361', 'Q362', 'Q363', 'Q364', 'Q365', 'Q366', 'Q367', 'Q368', 'Q369')
# Fenda_labial_com_fenda_palatina <- c('Q37X', 'Q370', 'Q371', 'Q372', 'Q373', 'Q374', 'Q375', 'Q376', 'Q377', 'Q378', 'Q379')
# Hipospadia <- c('Q540', 'Q541', 'Q542', 'Q543', 'Q544', 'Q545', 'Q546', 'Q547', 'Q548', 'Q549')
# Sexo_indeterminado_e_pseudo_hermafroditismo <- c('Q560', 'Q561', 'Q562', 'Q563', 'Q564', 'Q565', 'Q566', 'Q567', 'Q568', 'Q569')
# Deformidades_congenitas_do_pe <- c('Q66X','Q660', 'Q661', 'Q662', 'Q663', 'Q664', 'Q665', 'Q666', 'Q667', 'Q668', 'Q669')
# Polidactilia <- c('Q69X','Q690', 'Q691', 'Q692', 'Q693', 'Q694', 'Q695', 'Q696', 'Q697', 'Q698', 'Q699')
# Defeitos_por_reducao_do_membro_superior <- c('Q710', 'Q711', 'Q712', 'Q713', 'Q714', 'Q715', 'Q716', 'Q717', 'Q718', 'Q719')
# Defeitos_por_reducao_do_membro_inferior <- c('Q720', 'Q721', 'Q722', 'Q723', 'Q724', 'Q725', 'Q726', 'Q727', 'Q728', 'Q729')
# Defeitos_por_reducao_de_membro_nao_especificado <- c('Q730', 'Q731', 'Q732', 'Q733', 'Q734', 'Q735', 'Q736', 'Q737', 'Q738', 'Q739')
# Artrogripose_congenita_multipla <- c('Q743')
# Enxofalia <- c('Q792')
# Gastrite <- c('Q793')
# Sindrome_de_down <- c('Q900', 'Q901', 'Q902', 'Q903', 'Q904', 'Q905', 'Q906', 'Q907', 'Q908', 'Q909')
#
# ## Criando uma função que retorna a descrição da anomalia com base em seu código
# obter_descricao <- function(codigo_anomalia) {
#   if (codigo_anomalia %in% Anencefalia) {
#     return("Anencefalia")
#   } else if (codigo_anomalia %in% Craniorraquisquise) {
#     return("Craniorraquisquise")
#   } else if (codigo_anomalia %in% Iniencefalia) {
#     return("Iniencefalia")
#   } else if (codigo_anomalia %in% Encefalocele) {
#     return("Encefalocele")
#   } else if (codigo_anomalia %in% Espinha_bifida) {
#     return("Espinha bifida")
#   } else if (codigo_anomalia %in% Microcefalia) {
#     return("Microcefalia")
#   } else if (codigo_anomalia %in% Malformacoes_congenitas_das_camaras_e_das_comunicacoes_cardiacas) {
#     return("Malformações congênitas das câmaras e das comunicações cardíacas")
#   } else if (codigo_anomalia %in% Malformacoes_congenitas_dos_septos_cardiacos) {
#     return("Malformações congênitas dos septos cardíacos")
#   } else if (codigo_anomalia %in% Malformacoes_congenitas_das_valvas_pulmonar_e_tricuspide) {
#     return("Malformações congênitas das valvas pulmonar e tricúspide")
#   } else if (codigo_anomalia %in% Malformacoes_congenitas_das_valvas_aortica_e_mitral) {
#     return("Malformações congênitas das valvas aórtica e mitral")
#   } else if (codigo_anomalia %in% Outras_malformacoes_congenitas_do_coracao) {
#     return("Outras malformações congênitas do coração")
#   } else if (codigo_anomalia %in% Malformacoes_congenitas_das_grandes_arterias) {
#     return("Malformações congênitas das grandes artérias")
#   } else if (codigo_anomalia %in% Malformacoes_congenitas_das_grandes_veias) {
#     return("Malformações congênitas das grandes veias")
#   } else if (codigo_anomalia %in% Outras_malformacoes_congenitas_do_sistema_vascular_periferico) {
#     return("Outras malformações congênitas do sistema vascular periférico")
#   } else if (codigo_anomalia %in% Outras_malformacoes_congenitas_do_aparelho_circulatorio) {
#     return("Outras malformações congênitas do aparelho circulatório")
#   } else if (codigo_anomalia %in% Fenda_palatina) {
#     return("Fenda palatina")
#   } else if (codigo_anomalia %in% Fenda_labial) {
#     return("Fenda labial")
#   } else if (codigo_anomalia %in% Fenda_labial_com_fenda_palatina) {
#     return("Fenda labial com fenda palatina")
#   } else if (codigo_anomalia %in% Hipospadia) {
#     return("Hipospadia")
#   } else if (codigo_anomalia %in% Sexo_indeterminado_e_pseudo_hermafroditismo) {
#     return("Sexo indeterminado e pseudo-hermafroditismo")
#   } else if (codigo_anomalia %in% Deformidades_congenitas_do_pe) {
#     return("Deformidades congênitas do pé")
#   } else if (codigo_anomalia %in% Polidactilia) {
#     return("Polidactilia")
#   } else if (codigo_anomalia %in% Defeitos_por_reducao_do_membro_superior) {
#     return("Defeitos por redução do membro superior")
#   } else if (codigo_anomalia %in% Defeitos_por_reducao_do_membro_inferior) {
#     return("Defeitos por redução do membro inferior")
#   } else if (codigo_anomalia %in% Defeitos_por_reducao_de_membro_nao_especificado) {
#     return("Defeitos por redução de membro não especificado")
#   } else if (codigo_anomalia %in% Artrogripose_congenita_multipla) {
#     return("Artrogripose congênita múltipla")
#   } else if (codigo_anomalia %in% Enxofalia) {
#     return("Exonfalia")
#   } else if (codigo_anomalia %in% Gastrite) {
#     return("Gastrosquise")
#   } else if (codigo_anomalia %in% Sindrome_de_down) {
#     return("Síndrome de Down")
#   } else {
#     return("Outra anomalia")
#   }
# }
#
# ## Aplicando essa função no dataframe long
# df_anomalias_prioritarias_long$descricao <- sapply(df_anomalias_prioritarias_long$codigo_anomalia, obter_descricao)
#
# ## Salvando a base final
# write.csv(df_anomalias_prioritarias_long |> rename(nv_anomalia = total_de_nascidos_vivos), "data-raw/csv/df_tabela_anomalias_prioritarias_2018_2023.csv", row.names = FALSE)


