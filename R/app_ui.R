#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    # Your application UI logic
    includeCSS("inst/app/www/global/custom.css"),
    tags$head(tags$script(src = "funcoes_javascript.js")),
    bs4Dash::bs4DashPage(
      help = NULL,
      dark = NULL,
      title = "Painel Hospitalar",
      bs4Dash::bs4DashNavbar(
        fixed = TRUE,
        status = "primary",
        skin = "light",
        title = HTML("<b>&nbsp; Painel Hospitalar</b>"),
        span(
          class = "navbar-brand-fake fonte-titulos",
          tags$img(
            style = "height: 40px; margin-right: 8px; vertical-align: middle;",
            src = "www/logos/logo-oobr-curto.png",
          ),
          span(
            style = "display: inline-block; transform: translateY(5%);",
            "Painel Hospitalar"
          )
        ),

        bs4Dash::navbarMenu(
          id = "navmenu",

          # bs4Dash::navbarTab(
          #   tabName = "home",
          #   text = HTML("<span class = 'fonte-titulos-sobre'> Home </span>")
          # ),

          # Para a aba de "Visão geral"
          bs4Dash::navbarTab(
            tabName = "visao_geral",
            text = HTML("<span class = 'fonte-titulos-sobre'> Visão Geral </span>")
          ),

          # Para a aba de "Indicadores"
          bs4Dash::navbarTab(
            text = HTML("<span class = 'fonte-titulos-sobre'> Indicadores </span>"),
            tabName = "indicadores",

            ## Para os indicadores do grupo "Perfil sociodemográfico das mulheres atendidas"
            bs4Dash::navbarTab(
              tabName = "indicadores",
              text = span(
                span(class = "fonte-grande", "Perfil sociodemográfico das mulheres atendidas")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores",
                text = span(class = "fonte-grande", "Para parto"),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_nvm_idade",
                  text = span(class = "fonte-grande", "Porcentagem de nascidos vivos por idade da mãe")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_nvm_racacor",
                  text = span(class = "fonte-grande", "Porcentagem de nascidos vivos por raça/cor da mãe")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_nvm_escolaridade",
                  text = span(class = "fonte-grande", "Porcentagem de nascidos vivos por escolaridade da mãe")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_nvm_estciv",
                  text = span(class = "fonte-grande", "Porcentagem de nascidos vivos por situação conjugal da mãe")
                )
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores",
                text = span(class = "fonte-grande", "Para aborto"),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_internacoes_aborto_idade",
                  text = span(class = "fonte-grande", "Porcentagem de internações por aborto por idade da mulher")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_internacoes_aborto_racacor",
                  text = span(class = "fonte-grande", "Porcentagem de internações por aborto por raça/cor da mulher")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_internacoes_aborto_instrucao",
                  text = span(class = "fonte-grande", "Porcentagem de internações por aborto por grau de instrução da mulher")
                )
              )
            ),

            ## Para os indicadores do grupo "Perfil obstétrico das mulheres atendidas"
            bs4Dash::navbarTab(
              tabName = "indicadores",
              text = span(
                span(class = "fonte-grande", "Perfil obstétrico das mulheres atendidas")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_qtdpart",
                text = span(class = "fonte-grande", "Porcentagem de nascidos vivos por número de partos anteriores da mãe")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_qtdpartces",
                text = span(class = "fonte-grande", "Porcentagem de nascidos vivos por cesarianas anteriores da mãe")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_gestacao",
                text = span(class = "fonte-grande", "Porcentagem de nascidos vivos por tipo de gestação")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_inicio_prenat",
                text = span(class = "fonte-grande", "Porcentagem de nascidos vivos segundo idade gestacional no início do pré-natal")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_num_consultas_prenat_adequado",
                text = span(class = "fonte-grande", "Porcentagem de nascidos vivos com número de consultas pré-natal adequado")
              )
            ),

            ## Para os indicadores do grupo "Indicadores assistenciais"
            bs4Dash::navbarTab(
              tabName = "indicadores",
              text = span(
                span(class = "fonte-grande", "Indicadores assistenciais")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores",
                text = span(class = "fonte-grande", "Internações obstétricas"),
                bs4Dash::navbarTab(
                  tabName = "indicadores-total_internacoes_obstetricas",
                  text = span(class = "fonte-grande", "Número de internações obstétricas")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_internacoes_obstetricas_motivo",
                  text = span(class = "fonte-grande", "Porcentagem de internações obstétricas por motivos específicos")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_internacoes_obstetricas_saida",
                  text = span(class = "fonte-grande", "Porcentagem de internações obstétricas por tipo de saída")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-total_internacoes_obstetricas_aborto_legal",
                  text = span(class = "fonte-grande", "Número de internações obstétricas por aborto legal")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_internacoes_obstetricas_amiu",
                  text = span(class = "fonte-grande", "Porcentagem de AMIU dentre procedimentos de esvaziamento uterino")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_internacoes_obstetricas_parto_acompanhante",
                  text = span(class = "fonte-grande", "Porcentagem de internações para parto com presença de acompanhante")
                )
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores",
                text = span(class = "fonte-grande", "Internações neonatais"),
                bs4Dash::navbarTab(
                  tabName = "indicadores-total_internacoes_neonatais",
                  text = span(class = "fonte-grande", "Número de internações neonatais")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_internacoes_neonatais_uti",
                  text = span(class = "fonte-grande", "Porcentagem de internações neonatais em UTI neonatal")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_internacoes_neonatais_saida",
                  text = span(class = "fonte-grande", "Porcentagem de internações neonatais por tipo de saída")
                )
              )
            ),

            ## Para os indicadores do grupo "Indicadores da assistência ao parto"
            bs4Dash::navbarTab(
              tabName = "indicadores",
              text = span(
                span(class = "fonte-grande", "Indicadores da assistência ao parto")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_parto",
                text = span(class = "fonte-grande", "Porcentagem de nascidos vivos por tipo de parto")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_parto_vaginal_pos_cesarea",
                text = span(class = "fonte-grande", "Porcentagem de nascidos vivos por partos vaginais pós-cesariana")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_trab_parto_induzido",
                text = span(class = "fonte-grande", "Porcentagem de nascidos vivos com trabalho de parto induzido")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_robson",
                text = span(class = "fonte-grande", "Porcentagem de nascidos vivos por grupo de Robson")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_cesarea_robson",
                text = span(class = "fonte-grande", "Porcentagem de cesarianas por grupo de Robson")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_contrib_cesarea_robson",
                text = span(class = "fonte-grande", "Contrib. de cada grupo de Robson para a taxa global de cesarianas")
              )
            ),

            ## Para os indicadores do grupo "Perfil dos nascimentos"
            bs4Dash::navbarTab(
              tabName = "indicadores",
              text = span(
                span(class = "fonte-grande", "Perfil dos nascimentos")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nv_baixo_peso",
                text = span(class = "fonte-grande", "Porcentagem de baixo peso ao nascer")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-distribuicao_nv_baixo_peso",
                text = span(class = "fonte-grande", "Distribuição percentual do baixo peso ao nascer")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nv_prematuros",
                text = span(class = "fonte-grande", "Porcentagem de nascimentos prematuros")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-distribuicao_nv_prematuros",
                text = span(class = "fonte-grande", "Distribuição percentual da prematuridade")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nv_termo_precoce",
                text = span(class = "fonte-grande", "Porcentagem de nascimentos termo precoce")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nv_termo_pleno",
                text = span(class = "fonte-grande", "Porcentagem de nascimentos termo pleno")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nv_pos_termo",
                text = span(class = "fonte-grande", "Porcentagem de nascimentos pós-termo")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nv_asfixia_sem_anomalia_2500g_mais",
                text = span(class = "fonte-grande", "Porcentagem de nascidos vivos com asfixia dentre os nascidos vivos sem anomalias e com peso ≥ 2500 g")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nv_anomalia",
                text = span(class = "fonte-grande", "Porcentagem de nascidos vivos com anomalias congênitas")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nv_anomalia_prioritaria",
                text = span(class = "fonte-grande", "Porcentagem de nascidos vivos com anomalias congênitas prioritárias para vigilância")
              )
            ),

            ## Para os indicadores do grupo "Morbimortalidade materna"
            bs4Dash::navbarTab(
              tabName = "indicadores",
              text = span(
                span(class = "fonte-grande", "Morbimortalidade materna")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores",
                text = span(class = "fonte-grande", "Mortalidade"),
                bs4Dash::navbarTab(
                  tabName = "indicadores-obitos_maternos_totais",
                  text = span(class = "fonte-grande", "Número de óbitos maternos")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-rmm",
                  text = span(class = "fonte-grande", "Razão de mortalidade materna")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_obitos_maternos_diretos",
                  text = span(class = "fonte-grande", "Porcentagem de óbitos maternos por causas obstétricas diretas")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_obitos_maternos_diretos_causas",
                  text = span(class = "fonte-grande", "Distribuição percentual das causas de óbitos maternos diretos")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_obitos_maternos_indiretos",
                  text = span(class = "fonte-grande", "Porcentagem de óbitos maternos por causas obstétricas indiretas")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_obitos_maternos_indiretos_causas",
                  text = span(class = "fonte-grande", "Distribuição percentual das causas de óbitos maternos indiretos")
                )
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores",
                text = span(class = "fonte-grande", "Morbidade (no SUS)"),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_mmg",
                  text = span(class = "fonte-grande", "Porcentagem de casos de morbidade materna grave")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_mmg_causas",
                  text = span(class = "fonte-grande", "Porcentagem de casos de morbidade materna grave por causas específicas")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_mmg_uti",
                  text = span(class = "fonte-grande", "Porcentagem de casos de morbidade materna grave com internação em UTI")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_mmg_tempo",
                  text = span(class = "fonte-grande", "Porcentagem de casos de morbidade materna grave com tempo de permanência prolongado")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_mmg_transfusao",
                  text = span(class = "fonte-grande", "Porcentagem de casos de morbidade materna grave com transfusão sanguínea")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-porc_mmg_histerectomia",
                  text = span(class = "fonte-grande", "Porcentagem de casos de morbidade materna grave com histerectomia")
                )
              )
            ),

            ## Para os indicadores do grupo "Morbimortalidade perinatal"
            bs4Dash::navbarTab(
              tabName = "indicadores",
              text = span(
                span(class = "fonte-grande", "Morbimortalidade perinatal")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores",
                text = span(class = "fonte-grande", "Mortalidade fetal"),
                bs4Dash::navbarTab(
                  tabName = "indicadores-obitos_fetais_totais",
                  text = span(class = "fonte-grande", "Número de óbitos fetais")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-taxa_de_mortalidade_fetal",
                  text = span(class = "fonte-grande", "Taxa de mortalidade fetal")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-distribuicao_obitos_fetais_momento",
                  text = span(class = "fonte-grande", "Distribuição percentual dos óbitos fetais por momento do óbito")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-distribuicao_obitos_fetais_peso",
                  text = span(class = "fonte-grande", "Distribuição percentual dos óbitos fetais por faixa de peso")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-distribuicao_obitos_fetais_principais",
                  text = span(class = "fonte-grande", "Distribuição percentual dos óbitos fetais por grupo de causas")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-distribuicao_obitos_fetais_evitaveis",
                  text = span(class = "fonte-grande", "Distribuição percentual dos óbitos fetais por grupo de causas evitáveis")
                )
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores",
                text = span(class = "fonte-grande", "Mortalidade perinatal"),
                bs4Dash::navbarTab(
                  tabName = "indicadores-obitos_perinatais_totais",
                  text = span(class = "fonte-grande", "Número de óbitos perinatais")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-taxa_de_mortalidade_perinatal",
                  text = span(class = "fonte-grande", "Taxa de mortalidade perinatal")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-distribuicao_obitos_perinatais_momento",
                  text = span(class = "fonte-grande", "Distribuição percentual dos óbitos perinatais por momento do óbito")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-distribuicao_obitos_perinatais_peso",
                  text = span(class = "fonte-grande", "Distribuição percentual dos óbitos perinatais por faixa de peso")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-distribuicao_obitos_perinatais_principais",
                  text = span(class = "fonte-grande", "Distribuição percentual dos óbitos perinatais por grupo de causas")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-distribuicao_obitos_perinatais_evitaveis",
                  text = span(class = "fonte-grande", "Distribuição percentual dos óbitos perinatais por grupo de causas evitáveis")
                )
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores",
                text = span(class = "fonte-grande", "Mortalidade neonatal"),
                bs4Dash::navbarTab(
                  tabName = "indicadores-obitos_neonatais_totais",
                  text = span(class = "fonte-grande", "Número de óbitos neonatais")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-taxa_de_mortalidade_neonatal",
                  text = span(class = "fonte-grande", "Taxa de mortalidade neonatal")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-taxa_de_mortalidade_neonatal_0_a_6_dias",
                  text = span(class = "fonte-grande", "Taxa de mortalidade neonatal de 0 a 6 dias de vida")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-taxa_de_mortalidade_neonatal_7_a_27_dias",
                  text = span(class = "fonte-grande", "Taxa de mortalidade neonatal de 7 a 27 dias de vida")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-distribuicao_obitos_neonatais_momento",
                  text = span(class = "fonte-grande", "Distribuição percentual dos óbitos neonatais por momento do óbito")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-distribuicao_obitos_neonatais_peso",
                  text = span(class = "fonte-grande", "Distribuição percentual dos óbitos neonatais por faixa de peso")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-distribuicao_obitos_neonatais_principais",
                  text = span(class = "fonte-grande", "Distribuição percentual dos óbitos neonatais por grupo de causas")
                ),
                bs4Dash::navbarTab(
                  tabName = "indicadores-distribuicao_obitos_neonatais_evitaveis",
                  text = span(class = "fonte-grande", "Distribuição percentual dos óbitos neonatais por grupo de causas evitáveis")
                )
              )
            )
          )
        ),

        span(
          tags$img(
            class = "navbar-img",
            src = "www/logos/logo-oobr-curto.png",
          )
        )
      ),
      bs4Dash::bs4DashSidebar(
        width = "300px",
        status = "navy",
        skin = "light",
        collapsed = TRUE,
        minified = FALSE,
        bs4Dash::bs4SidebarMenu(
          id = "abas",
          # bs4Dash::bs4SidebarMenuItem(
          #   text = HTML("<b class = 'fonte-grande'>Home</b>"),
          #   tabName = "home",
          #   icon = icon("house")
          # ),
          bs4Dash::bs4SidebarMenuItem(
            tabName = "visao_geral",
            text = HTML("<b>Visão geral</b>"),
            icon = icon("sitemap")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<span class = 'fonte-grande'>Indicadores</span>"),
            tabName = "indicadores",
            icon = icon("person-dress")
          )
        )
      ),
      bs4Dash::bs4DashBody(
        div(
          class = "div-placeholder",
          conditionalPanel(
            condition = "input.abas == 'home'",
            span(class = "fonte-titulos", tags$b("Home")),
            style = "display: none;"
          ),
          conditionalPanel(
            condition = "input.abas == 'visao_geral'",
            span(style = "color: #EAF5FF;",  class = "fonte-titulos", "Visão geral do estabelecimento"),
            shinyjs::hidden(
              div(
                id = "div_titulo_visao_geral",
                style = "display: flex; position: absolute; top: 18%;",
                h2(
                  tags$b(HTML("Visão geral do estabelecimento"), htmlOutput("titulo_visao_geral", inline = TRUE)),
                  class = "fonte-titulos"
                )
              )
            ),
            style = "display: none;"
          ),
          conditionalPanel(
            condition = "input.abas == 'indicadores'",
            span(style = "color: #EAF5FF;",  class = "fonte-titulos", "Indicadores"),
            shinyjs::hidden(
              div(
                id = "div_titulo_indicadores",
                style = "display: flex; position: absolute; top: 18%;",
                h2(
                  tags$b(htmlOutput("titulo_indicadores", inline = TRUE)),
                  class = "fonte-titulos"
                )
              )
            ),
            style = "display: none;"
          ),
          # Nova linha para os chips de filtros
          fluidRow(
            div(
              id = "filtros_ativos_chips", # O ID do seu output
              class = "shiny-html-output filter-container", # Classe obrigatória do Shiny + sua classe de estilo
              style = "min-height: 47px;", # Reserva o espaço vertical para evitar o pulo

              # Estes chips de esqueleto aparecem apenas no "milissegundo" do loading inicial
              tags$div(class = "filter-chip skeleton-chip", style = "width: 158px;"),
              tags$div(class = "filter-chip skeleton-chip", style = "width: 203px;"),
              tags$div(class = "filter-chip skeleton-chip", style = "width: 465px;"),
              tags$div(class = "filter-chip skeleton-chip", style = "width: 206px;")
            )
          )
        ),
        fluidRow(
          div(
            class = "col-12 col-xl-3",
            div(class = "div-corta-card"),
            conditionalPanel(
              condition = "input.abas != 'home'",
              class = "conditional-sticky",
              style = "display: none; margin-top: -16px;",
              # Caixa de filtros
              bs4Dash::bs4Card(
                width = 12,
                id = "card-filters",
                title = HTML("<b class = 'fonte-muito-grande''>&nbsp;Filtros globais</b>"),
                icon = icon("filter"),
                status = "primary",
                solidHeader = TRUE,
                collapsible = FALSE,
                div(
                  class = "row g-2 align-items-end fonte-semi-grande",

                  # Filtros do grupo "Localização e temporalidade"
                  accordionSection(
                    id = "localizacao",
                    title = "Localização e temporalidade",
                    content_ui = div(
                      style = "padding-top: 10px; padding-bottom: 10px;",
                      fluidRow(
                        div(
                          class = "col-12",
                          selectizeInput(
                            inputId = "input_nivel",
                            label = HTML("<span class = 'fonte-semi-grande'>Nível de análise</span>"),
                            options = list(
                              dropdownParent = 'body',
                              onDelete = I('function(value) {return false;}'),
                              inputClass = "sem-cursor"
                            ),
                            choices = c(
                              #"UF" = "uf",
                              #"Macrorregião de saúde" = "macro_r_saude",
                              #"Região de saúde" = "r_saude",
                              #"Município" = "municipio",
                              "Estabelecimento" = "estabelecimento"
                            ),
                            selected = "estabelecimento",
                            width = "95%"
                          )
                        ),
                        div(
                          style = "width: 100%",
                          div(
                            class = "custom-guideline-box",
                            div(
                              class = "col-12",
                              style = "display: flex; align-items: anchor-center; gap: 6px;",
                              selectizeInput(
                                inputId = "input_uf",
                                label = HTML("<span class = 'fonte-semi-grande'>UF</span>"),
                                options = list(
                                  placeholder = "Selecione uma UF",
                                  dropdownParent = 'body'
                                ),
                                choices = sort(estados_choices),
                                selected = "Rio de Janeiro",
                                width = "98%"
                              ),
                              span(
                                "!",
                                class = "exclamacao-inputs",
                                id = "exclamacao_uf"
                              )
                            ),
                            shinyjs::hidden(
                              div(
                                id = "conditional_macro_r_saude",
                                class = "col-12",
                                style = "display: flex; align-items: anchor-center; gap: 6px;",
                                selectizeInput(
                                  inputId = "input_macro_r_saude",
                                  label = HTML("<span class = 'fonte-semi-grande'>Macrorregião de saúde</span>"),
                                  options = list(
                                    placeholder = "Selecione uma macrorregião de saúde",
                                    dropdownParent = 'body'
                                  ),
                                  choices = "MACRORREGIAO I",
                                  width = "98%"
                                ),
                                span(
                                  "!",
                                  class = "exclamacao-inputs",
                                  id = "exclamacao_macro_r_saude"
                                )
                              )
                            ),
                            shinyjs::hidden(
                              div(
                                id = "conditional_r_saude",
                                class = "col-12",
                                style = "display: flex; align-items: anchor-center; gap: 6px;",
                                selectizeInput(
                                  inputId = "input_r_saude",
                                  label = HTML("<span class = 'fonte-semi-grande'>Região de saúde</span>"),
                                  options = list(
                                    placeholder = "Selecione uma região de saúde",
                                    dropdownParent = 'body'
                                  ),
                                  choices = "Baia Da Ilha Grande",
                                  width = "98%"
                                ),
                                span(
                                  "!",
                                  class = "exclamacao-inputs",
                                  id = "exclamacao_r_saude"
                                )
                              )
                            ),
                            div(
                              id = "conditional_municipio",
                              class = "col-12",
                              style = "display: flex; align-items: anchor-center; gap: 6px;",
                              selectizeInput(
                                inputId = "input_municipio",
                                label = HTML("<span class = 'fonte-semi-grande'>Município</span>"),
                                options = list(
                                  placeholder = "Selecione um município",
                                  dropdownParent = 'body'
                                ),
                                choices = "Angra dos Reis",
                                width = "98%"
                              ),
                              span(
                                "!",
                                class = "exclamacao-inputs",
                                id = "exclamacao_municipio"
                              )
                            ),
                            shinyjs::hidden(
                              div(
                                id = "conditional_hospital_multiplo",
                                class = "col-12",
                                style = "display: flex; align-items: anchor-center; gap: 6px;",
                                shinyWidgets::virtualSelectInput(
                                  inputId = "input_hospital_multiplo",
                                  label = HTML("<span class = 'fonte-semi-grande'>Estabelecimento(s)</span>"),
                                  choices = list(
                                    `Públicos` = "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR",
                                    `Mistos` = NULL,
                                    `Privados` = "HOSPITAL UNIMED VOLTA REDONDA UNIDADE LITORAL SUL"
                                  ),
                                  multiple = TRUE,
                                  width = "95%",
                                  dropboxWrapper = "body",
                                  noOfDisplayValues = 1,
                                  optionsCount = 250,
                                  showValueAsTags = TRUE,
                                  focusSelectedOptionOnOpen = FALSE
                                ),
                                span(
                                  "!",
                                  class = "exclamacao-inputs",
                                  id = "exclamacao_hospital_multiplo"
                                )
                              )
                            ),
                            div(
                              id = "conditional_hospital_unico",
                              class = "col-12",
                              style = "display: flex; align-items: anchor-center; gap: 6px;",
                              shinyWidgets::virtualSelectInput(
                                inputId = "input_hospital_unico",
                                label = HTML("<span class = 'fonte-semi-grande'>Estabelecimento</span>"),
                                choices = list(
                                  `Públicos` = "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR",
                                  `Mistos` = NULL,
                                  `Privados` = "HOSPITAL UNIMED VOLTA REDONDA UNIDADE LITORAL SUL"
                                ),
                                selected = "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR",
                                multiple = FALSE,
                                disableSelectAll = TRUE,
                                width = "95%",
                                dropboxWrapper = "body",
                                optionsCount = 250,
                                focusSelectedOptionOnOpen = FALSE,
                                placeholder = "Selecione um estabelecimento",
                                search = TRUE
                              ),
                              span(
                                "!",
                                class = "exclamacao-inputs",
                                id = "exclamacao_hospital_unico"
                              )
                            )
                          )
                        ),
                        div(
                          class = "col-12",
                          style = "padding-top: 16px",
                          shinyWidgets::prettyRadioButtons(
                            inputId = "input_tipo_periodo",
                            label = htmltools::tagList(
                              span(HTML("Período de análise"), class = "fonte-semi-grande")
                            ),
                            choices = c(
                              "Últimos 12 meses" = "12_meses",
                              "Todo o período disponível" = "todo_periodo",
                              "Outro" = "outro"
                            ),
                            icon = icon("check", style = "background-color: #0A1E3C;"),
                            animation = "rotate"
                          )
                        ),
                        shinyjs::hidden(
                          div(
                            id = "conditional_periodo",
                            style = "width: 99%;",
                            div(
                              class = "custom-guideline-box",
                              style = "padding-left: 15px;",
                              shinyWidgets::airMonthpickerInput(
                                inputId = "input_periodo",
                                label = HTML("<span class = 'fonte-semi-grande'>Período customizado</span>"),
                                value = c("2023-01", "2023-12"),
                                minDate = "2018-01",
                                maxDate = "2023-12",
                                view = "months",
                                language = "pt-BR",
                                dateFormat = "MMMM, yyyy",
                                range = TRUE,
                                width = "100%"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),

                  # Filtros do grupo "Indicador selecionado"
                  shinyjs::hidden(
                    div(
                      id = "conditional_indicador",
                      style = "width: 100%;",
                      accordionSection(
                        id = "indicador",
                        title = "Indicador selecionado",
                        content_ui = div(
                          style = "padding-top: 10px; padding-bottom: 10px;",
                          fluidRow(
                            div(
                              class = "col-12",
                              style = "display: flex; align-items: anchor-center; gap: 6px;",
                              selectizeInput(
                                inputId = "input_bloco",
                                label = HTML("<span class = 'fonte-semi-grande'>Bloco de indicadores</span>"),
                                options = list(
                                  placeholder = "Selecione um bloco de indicadores",
                                  dropdownParent = 'body',
                                  onDelete = I('function(value) {return false;}'),
                                  inputClass = "sem-cursor"
                                ),
                                choices = setNames(
                                  unique(tabela_indicadores$bloco),
                                  unique(tabela_indicadores$bloco_por_extenso)
                                ),
                                width = "98%"
                              ),
                              span(
                                "!",
                                class = "exclamacao-inputs",
                                id = "exclamacao_bloco"
                              )
                            ),
                            div(
                              style = "width: 100%",
                              div(
                                class = "custom-guideline-box",
                                shinyjs::hidden(
                                  div(
                                    id = "conditional_sub_bloco",
                                    class = "col-12",
                                    style = "display: flex; align-items: anchor-center; gap: 6px;",
                                    selectizeInput(
                                      inputId = "input_sub_bloco",
                                      label = HTML("<span class = 'fonte-semi-grande'>Sub-bloco de indicadores</span>"),
                                      options = list(
                                        placeholder = "Selecione um sub-bloco de indicadores",
                                        dropdownParent = 'body',
                                        onDelete = I('function(value) {return false;}'),
                                        inputClass = "sem-cursor"
                                      ),
                                      choices = NULL,
                                      width = "98%"
                                    ),
                                    span(
                                      "!",
                                      class = "exclamacao-inputs",
                                      id = "exclamacao_sub_bloco"
                                    )
                                  )
                                ),
                                div(
                                  class = "col-12",
                                  style = "display: flex; align-items: anchor-center; gap: 6px;",
                                  selectizeInput(
                                    inputId = "input_indicador",
                                    label = HTML("<span class = 'fonte-semi-grande'>Indicador</span>"),
                                    options = list(
                                      placeholder = "Selecione um indicador",
                                      dropdownParent = 'body',
                                      onDelete = I('function(value) {return false;}'),
                                      inputClass = "sem-cursor"
                                    ),
                                    choices = NULL,
                                    width = "98%"
                                  ),
                                  span(
                                    "!",
                                    class = "exclamacao-inputs",
                                    id = "exclamacao_indicador"
                                  )
                                )
                              )
                            ),
                            shinyjs::hidden(
                              div(
                                id = "conditional_desejo_visualizar",
                                class = "col-12",
                                style = "padding-top: 16px",
                                shinyWidgets::prettyRadioButtons(
                                  inputId = "input_desejo_visualizar",
                                  label = htmltools::tagList(
                                    span(HTML("Desejo visualizar:"), class = "fonte-semi-grande")
                                  ),
                                  choices = c(
                                    "Todas as categorias simultaneamente" = "agrupado",
                                    "Uma categoria individual" = "individual"
                                  ),
                                  icon = icon("check", style = "background-color: #0A1E3C;"),
                                  animation = "rotate"
                                )
                              ),
                              shinyjs::hidden(
                                div(
                                  id = "conditional_categoria",
                                  style = "width: 100%;",
                                  div(
                                    class = "custom-guideline-box",
                                    div(
                                      class = "col-12",
                                      style = "display: flex; align-items: anchor-center; gap: 6px;",
                                      selectizeInput(
                                        inputId = "input_categoria",
                                        label = HTML("<span class = 'fonte-semi-grande'>Idade da mãe</span>"),
                                        options = list(
                                          placeholder = "Selecione uma categoria",
                                          dropdownParent = 'body',
                                          onDelete = I('function(value) {return false;}'),
                                          inputClass = "sem-cursor"
                                        ),
                                        choices = NULL,
                                        width = "98%"
                                      ),
                                      span(
                                        "!",
                                        class = "exclamacao-inputs",
                                        id = "exclamacao_categoria"
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                # UI do footer
                footer = div(
                  class = "d-flex",
                  style = "justify-content: center; gap: 8%; align-items: center;",

                  actionButton(
                    class = "nav-btn btn-redefinir fonte-grande",
                    inputId = "btn_redefinir",
                    label = "Redefinir",
                    icon = icon("undo")
                  ),

                  div(
                    style = "position: relative; display: inline-block;",
                    actionButton(
                      class = "nav-btn btn-atualizar fonte-grande",
                      inputId = "btn_atualizar",
                      label = "Atualizar",
                      icon = icon("sync")
                    ),
                    # Badge circular com exclamação
                    span(
                      "!",
                      id = "aviso_exclamacao",
                      style = "
                        display: flex;
                        align-items: center;
                        justify-content: center;
                        color: #fff;
                        background-color: #32A0FF;
                        border-radius: 50%;
                        width: 18px;        /* menor largura */
                        height: 18px;       /* menor altura */
                        font-size: 12px;    /* tamanho da exclamação */
                        font-weight: bold;
                        position: absolute;
                        top: 50%;
                        right: -30px;       /* ajustar posição horizontal se necessário */
                        transform: translateY(-50%);
                        opacity: 0;
                        transition: opacity 0.3s ease;
                      "
                    )

                  )
                )

              )
            )
          ),
          div(
            class = "col-12 col-xl-9",
            conditionalPanel(
              condition = "input.abas != 'home'",
              div(
                class = "row-visualizations",
                bs4Dash::bs4TabItems(
                  bs4Dash::bs4TabItem(
                    tabName = "visao_geral",
                    mod_visao_geral_ui("visao_geral_1")
                  ),
                  bs4Dash::bs4TabItem(
                    tabName = "indicadores",
                    mod_indicadores_ui("indicadores_1")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          div(
            class = "col-12",
            conditionalPanel(
              condition = "input.abas == 'visao_geral' | input.abas == 'indicadores'",
              div(
                style = "padding-bottom: 60px; padding-top: 80px;",
                div(
                  style = "display: flex; justify-content: center; margin: auto;",
                  p(class = "fonte-titulos-sobre",
                    style = "text-align: justify; margin-bottom: 36px; font-weight: 600;",
                    "Navegue pelos blocos de indicadores"
                  )
                ),
                div(
                  class = "div-botoes-blocos",
                  tags$a(
                    id = "btn_perfil_sociodemografico",
                    class = "circle-btn",
                    href = "#",
                    # Mandamos o NOME DO BLOCO exato para o R
                    onclick = "Shiny.setInputValue('clique_bloco_home', 'perfil_sociodemografico', {priority: 'event'});",
                    div(class = "circle-icon", icon("person-dress")),
                    div(class = "circle-label fonte-muito-grande", "Perfil sociodemográfico das mulheres atendidas")
                  ),
                  tags$a(
                    id = "btn_perfil_obstetrico",
                    class = "circle-btn",
                    href = "#",
                    onclick = "Shiny.setInputValue('clique_bloco_home', 'perfil_obstetrico', {priority: 'event'});",
                    div(class = "circle-icon", icon("person-breastfeeding")),
                    div(class = "circle-label fonte-muito-grande", "Perfil obstétrico das mulheres atendidas")
                  ),
                  tags$a(
                    id = "btn_indicadores_assistenciais",
                    class = "circle-btn",
                    href = "#",
                    onclick = "Shiny.setInputValue('clique_bloco_home', 'indicadores_assistenciais', {priority: 'event'});",
                    div(class = "circle-icon", icon("stethoscope")),
                    div(class = "circle-label fonte-muito-grande", "Indicadores assistenciais")
                  ),
                  tags$a(
                    id = "btn_indicadores_assistencia_ao_parto",
                    class = "circle-btn",
                    href = "#",
                    onclick = "Shiny.setInputValue('clique_bloco_home', 'indicadores_assistencia_ao_parto', {priority: 'event'});",
                    div(class = "circle-icon", icon("stethoscope")), # Troque o ícone se quiser
                    div(class = "circle-label fonte-muito-grande", "Indicadores da assistência ao parto")
                  ),
                  tags$a(
                    id = "btn_perfil_dos_nascimentos",
                    class = "circle-btn",
                    href = "#",
                    onclick = "Shiny.setInputValue('clique_bloco_home', 'perfil_dos_nascimentos', {priority: 'event'});",
                    div(class = "circle-icon", icon("baby-carriage")),
                    div(class = "circle-label fonte-muito-grande", "Perfil dos nascimentos")
                  ),
                  tags$a(
                    id = "btn_morbimortalidade_materna",
                    class = "circle-btn",
                    href = "#",
                    onclick = "Shiny.setInputValue('clique_bloco_home', 'morbimortalidade_materna', {priority: 'event'});",
                    div(class = "circle-icon", icon("heart-pulse")),
                    div(class = "circle-label fonte-muito-grande", "Morbimortalidade materna")
                  ),
                  tags$a(
                    id = "btn_morbimortalidade_perinatal",
                    class = "circle-btn",
                    href = "#",
                    onclick = "Shiny.setInputValue('clique_bloco_home', 'morbimortalidade_perinatal', {priority: 'event'});",
                    div(class = "circle-icon", icon("baby")),
                    div(class = "circle-label fonte-muito-grande", "Morbimortalidade perinatal")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Painel Hospitalar"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
