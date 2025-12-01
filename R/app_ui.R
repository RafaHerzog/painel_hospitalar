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

          bs4Dash::navbarTab(
            tabName = "visao_geral",
            text = HTML("<span class = 'fonte-titulos-sobre'> Visão Geral </span>")
          ),

          bs4Dash::navbarTab(
            text = HTML("<span class = 'fonte-titulos-sobre'> Indicadores </span>"),
            tabName = "indicadores",
            bs4Dash::navbarTab(
              tabName = "indicadores",
              text = span(
                span(class = "fonte-muito-grande", "Perfil sociodemográfico das mulheres atendidas")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_idade",
                text = span(class = "fonte-muito-grande", "Percentual de nascidos vivos por idade da mãe")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_racacor",
                text = span(class = "fonte-muito-grande", "Percentual de nascidos vivos por raça/cor da mãe")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_escolaridade",
                text = span(class = "fonte-muito-grande", "Percentual de nascidos vivos por escolaridade da mãe")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_estciv",
                text = span(class = "fonte-muito-grande", "Percentual de nascidos vivos por estado civil da mãe")
              )
            ),

            bs4Dash::navbarTab(
              tabName = "indicadores",
              text = span(
                span(class = "fonte-muito-grande", "Perfil obstétrico das mulheres atendidas")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_qtdpart",
                text = span(class = "fonte-muito-grande", "Porcentagem de nascidos vivos por número de partos anteriores da mãe")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_qtdpartces",
                text = span(class = "fonte-muito-grande", "Porcentagem de nascidos vivos por cesarianas anteriores da mãe")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_parto",
                text = span(class = "fonte-muito-grande", "Porcentagem de nascidos vivos por tipo de parto")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_gestacao",
                text = span(class = "fonte-muito-grande", "Porcentagem de nascidos vivos por tipo de gestação")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_trab_parto_induzido",
                text = span(class = "fonte-muito-grande", "Porcentagem de nascidos vivos com trabalho de parto induzido")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_inicio_prenat",
                text = span(class = "fonte-muito-grande", "Porcentagem de nascidos vivos por tempo até o início do pré-natal")
              ),
              bs4Dash::navbarTab(
                tabName = "indicadores-porc_nvm_num_consultas_prenat_adequado",
                text = span(class = "fonte-muito-grande", "Porcentagem de nascidos vivos com número de consultas pré-natal adequado")
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
            span(style = "padding-left: 0.4em;",  class = "fonte-destaque-caixas", tags$b("Home")),
            style = "display: none;"
          ),
          conditionalPanel(
            condition = "input.abas == 'visao_geral'",
            span(style = "padding-left: 0.4em; color: #EAF5FF;",  class = "fonte-destaque-caixas", "Visão geral"),
            shinyjs::hidden(
              div(
                id = "div_titulo_visao_geral",
                style = "display: flex; position: absolute; top: 25%;",
                h2(
                  tags$b(HTML("Visão geral"), htmlOutput("titulo_visao_geral", inline = TRUE)),
                  class = "fonte-destaque-caixas",
                  style = "padding-left: 0.4em; padding-right: 0.4em;"
                )
              )
            ),
            style = "display: none;"
          ),
          conditionalPanel(
            condition = "input.abas == 'indicadores'",
            span(style = "padding-left: 0.4em; color: #EAF5FF;",  class = "fonte-destaque-caixas", "Indicadores"),
            shinyjs::hidden(
              div(
                id = "div_titulo_indicadores",
                style = "display: flex; position: absolute; top: 25%;",
                h2(
                  tags$b(HTML("Indicadores"), htmlOutput("titulo_indicadores", inline = TRUE)),
                  class = "fonte-destaque-caixas",
                  style = "padding-left: 0.4em; padding-right: 0.4em;"
                )
              )
            ),
            style = "display: none;"
          )
        ),
        fluidRow(
          div(
            class = "col-12 col-xl-3",
            div(class = "div-corta-card"),
            conditionalPanel(
              condition = "input.abas != 'home'",
              class = "conditional-sticky",
              style = "display: none;",
              # Caixa de filtros
              bs4Dash::bs4Card(
                width = 12,
                id = "card-filters",
                title = HTML("<b class = 'fonte-subtitulos-sobre''>&nbsp;Filtros</b>"),
                icon = icon("filter"),
                status = "primary",
                solidHeader = TRUE,
                collapsible = FALSE,
                div(
                  class = "row g-2 align-items-end fonte-grande",

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
                            label = HTML("<span class = 'fonte-grande'>Nível de análise</span>"),
                            options = list(
                              dropdownParent = 'body',
                              inputClass = "sem-cursor"
                            ),
                            choices = c(
                              "UF" = "uf",
                              "Macrorregião de saúde" = "macro_r_saude",
                              "Região de saúde" = "r_saude",
                              "Município" = "municipio",
                              "Estabelecimento" = "estabelecimento"
                            ),
                            selected = "estabelecimento"
                          )
                        ),
                        div(
                          style = "width: 100%",
                          div(
                            class = "custom-guideline-box",
                            div(
                              class = "col-12",
                              selectizeInput(
                                inputId = "input_uf",
                                label = HTML("<span class = 'fonte-grande'>UF</span>"),
                                options = list(placeholder = "Selecione uma UF", dropdownParent = 'body'),
                                choices = sort(estados_choices),
                                selected = "Rio de Janeiro",
                                width = "100%"
                              )
                            ),
                            shinyjs::hidden(
                              div(
                                id = "conditional_macro_r_saude",
                                class = "col-12",
                                selectizeInput(
                                  inputId = "input_macro_r_saude",
                                  label = HTML("<span class = 'fonte-grande'>Macrorregião de saúde</span>"),
                                  options = list(placeholder = "Selecione uma macrorregião de saúde", dropdownParent = 'body'),
                                  choices = "MACRORREGIAO I",
                                  width = "100%"
                                )
                              )
                            ),
                            shinyjs::hidden(
                              div(
                                id = "conditional_r_saude",
                                class = "col-12",
                                selectizeInput(
                                  inputId = "input_r_saude",
                                  label = HTML("<span class = 'fonte-grande'>Região de saúde</span>"),
                                  options = list(placeholder = "Selecione uma região de saúde", dropdownParent = 'body'),
                                  choices = "Baia Da Ilha Grande",
                                  width = "100%"
                                )
                              )
                            ),
                            div(
                              id = "conditional_municipio",
                              class = "col-12",
                              selectizeInput(
                                inputId = "input_municipio",
                                label = HTML("<span class = 'fonte-grande'>Município</span>"),
                                options = list(placeholder = "Selecione um município", dropdownParent = 'body'),
                                choices = "Angra dos Reis",
                                width = "100%"
                              )
                            ),
                            shinyjs::hidden(
                              div(
                                id = "conditional_hospital_multiplo",
                                class = "col-12",
                                shinyWidgets::virtualSelectInput(
                                  inputId = "input_hospital_multiplo",
                                  label = HTML("<span class = 'fonte-grande'>Estabelecimento(s)</span>"),
                                  choices = list(
                                    `Públicos` = "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR",
                                    `Mistos` = NULL,
                                    `Privados` = "HOSPITAL UNIMED VOLTA REDONDA UNIDADE LITORAL SUL",
                                    `Sem classificação` = NULL
                                  ),
                                  multiple = TRUE,
                                  width = "99%",
                                  dropboxWrapper = "body",
                                  noOfDisplayValues = 1,
                                  optionsCount = 250,
                                  showValueAsTags = TRUE,
                                  focusSelectedOptionOnOpen = FALSE
                                )
                              )
                            ),
                            div(
                              id = "conditional_hospital_unico",
                              class = "col-12",
                              shinyWidgets::virtualSelectInput(
                                inputId = "input_hospital_unico",
                                label = HTML("<span class = 'fonte-grande'>Estabelecimento</span>"),
                                choices = list(
                                  `Públicos` = "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR",
                                  `Mistos` = NULL,
                                  `Privados` = "HOSPITAL UNIMED VOLTA REDONDA UNIDADE LITORAL SUL",
                                  `Sem classificação` = NULL
                                ),
                                selected = "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR",
                                multiple = FALSE,
                                disableSelectAll = TRUE,
                                width = "99%",
                                dropboxWrapper = "body",
                                optionsCount = 250,
                                focusSelectedOptionOnOpen = FALSE
                              )
                            )
                          )
                        ),
                        div(
                          class = "col-12",
                          shinyWidgets::airMonthpickerInput(
                            inputId = "input_periodo",
                            label = HTML("<span class = 'fonte-grande'>Período de análise</span>"),
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
                  ),

                  # Filtros do grupo "Filtros de visualização"
                  shinyjs::hidden(
                    div(
                      id = "conditional_indicador",
                      style = "width: 100%;",
                      accordionSection(
                        id = "indicador",
                        title = "Filtros de visualização",
                        content_ui = div(
                          style = "padding-top: 10px; padding-bottom: 10px;",
                          fluidRow(
                            div(
                              class = "col-12",
                              selectizeInput(
                                inputId = "input_bloco",
                                label = HTML("<span class = 'fonte-grande'>Bloco de indicadores</span>"),
                                options = list(
                                  placeholder = "Selecione um bloco de indicadores",
                                  dropdownParent = 'body',
                                  inputClass = "sem-cursor"
                                ),
                                choices = setNames(
                                  unique(tabela_indicadores$bloco),
                                  unique(tabela_indicadores$bloco_por_extenso)
                                ),
                                width = "100%"
                              )
                            ),
                            div(
                              class = "col-12",
                              selectizeInput(
                                inputId = "input_indicador",
                                label = HTML("<span class = 'fonte-grande'>Indicador</span>"),
                                options = list(
                                  placeholder = "Selecione um indicador",
                                  dropdownParent = 'body',
                                  inputClass = "sem-cursor"
                                ),
                                choices = setNames(
                                  indicadores_perfil_sociodemografico$nome_abreviado_indicador,
                                  indicadores_perfil_sociodemografico$indicador_principal
                                ),
                                width = "100%"
                              )
                            ),
                            shinyjs::hidden(
                              div(
                                id = "conditional_desejo_visualizar",
                                class = "col-12",
                                shinyWidgets::prettyRadioButtons(
                                  inputId = "input_desejo_visualizar",
                                  label = htmltools::tagList(
                                    span(HTML("Desejo visualizar:"), class = "fonte-grande")
                                  ),
                                  choices = c(
                                    "Uma categoria individual" = "individual",
                                    "Todas as categorias simultaneamente" = "agrupado"
                                  ),
                                  icon = icon("check", style = "background-color: #0A1E3C;"),
                                  animation = "rotate"
                                )
                              ),
                              shinyjs::hidden(
                                div(
                                  id = "conditional_categoria",
                                  style = "width: 99%;",
                                  div(
                                    class = "custom-guideline-box",
                                    selectizeInput(
                                      inputId = "input_categoria",
                                      label = HTML("<span class = 'fonte-grande'>Idade da mãe</span>"),
                                      options = list(placeholder = "Selecione uma categoria", inputClass = "sem-cursor"),
                                      choices = setNames(
                                        indicadores_perfil_sociodemografico$sub_indicador,
                                        indicadores_perfil_sociodemografico$categoria_sub_indicador
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
              condition = "input.abas == 'visao_geral'",
              div(
                style = "padding-bottom: 20px; padding-top: 60px;",
                div(
                  style = "display: flex; justify-content: center; margin: auto;",
                  p(class = "fonte-titulos-sobre",
                    style = "text-align: justify; margin-bottom: 24px; font-weight: 600;",
                    "Navegue pelos blocos de indicadores"
                  )
                ),
                div(
                  class = "div-botoes-cancer",
                  tags$a(
                    id = "btn_perfil_sociodemografico",
                    class = "circle-btn circle-btn-style1",
                    href = "#",
                    #onclick = "openTab('indicadores_perfil_sociodemografico');",
                    onclick = "
                      openTab('indicadores');
                      Shiny.setInputValue('input_bloco', 'perfil_sociodemografico', {priority: 'event'});
                      Shiny.setInputValue('btn_atualizar', Math.random(), {priority: 'event'});
                    ",
                    div(class = "circle-icon", icon("person-dress")),
                    div(class = "circle-label fonte-muito-grande", "Perfil sociodemográfico das mulheres atendidas")
                  ),
                  tags$a(
                    id = "btn_perfil_obstetrico",
                    class = "circle-btn circle-btn-style1",
                    href = "#",
                    #onclick = "openTab('indicadores_perfil_obstetrico');",
                    onclick = "
                      openTab('indicadores');
                      Shiny.setInputValue('input_bloco', 'perfil_obstetrico', {priority: 'event'});
                      Shiny.setInputValue('btn_atualizar', Math.random(), {priority: 'event'});
                    ",
                    div(class = "circle-icon", icon("person-breastfeeding")),
                    div(class = "circle-label fonte-muito-grande", "Perfil obstétrico das mulheres atendidas")
                  ),
                  tags$a(
                    id = "btn_indicadores_assistenciais",
                    class = "circle-btn circle-btn-style1 disabled-btn",
                    href = "#",
                    #onclick = "openTab('indicadores_indicadores_assistenciais');",
                    onclick = "openTab('indicadores');",
                    div(class = "circle-icon", icon("stethoscope")),
                    div(class = "circle-label fonte-muito-grande", "Indicadores assistenciais")
                  ),
                  tags$a(
                    id = "btn_indicadores_assistencia_ao_parto",
                    class = "circle-btn circle-btn-style1 disabled-btn",
                    href = "#",
                    #onclick = "openTab('indicadores_indicadores_assistencia_ao_parto');",
                    onclick = "openTab('indicadores');",
                    div(class = "circle-icon", icon("stethoscope")),
                    div(class = "circle-label fonte-muito-grande", "Indicadores da assistência ao parto")
                  ),
                  tags$a(
                    id = "btn_perfil_dos_nascimentos",
                    class = "circle-btn circle-btn-style1 disabled-btn",
                    href = "#",
                    #onclick = "openTab('indicadores_perfil_dos_nascimentos');",
                    onclick = "openTab('indicadores');",
                    div(class = "circle-icon", icon("baby-carriage")),
                    div(class = "circle-label fonte-muito-grande", "Perfil dos nascimentos")
                  ),
                  tags$a(
                    id = "btn_morbimortalidade_materna",
                    class = "circle-btn circle-btn-style1 disabled-btn",
                    href = "#",
                    #onclick = "openTab('indicadores_morbimortalidade_materna');",
                    onclick = "openTab('indicadores');",
                    div(class = "circle-icon", icon("heart-pulse")),
                    div(class = "circle-label fonte-muito-grande", "Morbimortalidade materna")
                  ),
                  tags$a(
                    id = "btn_morbimortalidade_perinatal",
                    class = "circle-btn circle-btn-style1 disabled-btn",
                    href = "#",
                    #onclick = "openTab('indicadores_morbimortalidade_perinatal');",
                    onclick = "openTab('indicadores');",
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
