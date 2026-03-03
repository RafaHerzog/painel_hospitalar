#' indicadores UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
html_skeleton_calculo <- function() {
  div(
    class = "skeleton-card-wrapper skel-container-grid",
    div(
      class = "value-box-custom",
      # Ícone (flex: 0 0 50px)
      div(class = "skeleton-box skel-icon-circle"),
      # Textos (flex: 1)
      div(
        class = "value-text",
        div(class = "skeleton-box", style = "height: 12px; width: 30%; margin-bottom: 12px;"), # Título "CÁLCULO"
        div(class = "skeleton-box", style = "height: 20px; width: 100%; margin-bottom: 8px; border-radius: 2px;"), # Texto 1
        div(class = "skeleton-box", style = "height: 20px; width: 90%; margin-bottom: 8px; border-radius: 2px;"),  # Texto 2
        div(class = "skeleton-box", style = "height: 20px; width: 60%; border-radius: 2px;")   # Texto 3
      )
    )
  )
}

html_skeleton_fonte <- function() {
  div(
    class = "skeleton-card-wrapper skel-container-grid",
    div(
      class = "value-box-custom",
      # Ícone
      div(class = "skeleton-box skel-icon-circle"),
      # Textos
      div(
        class = "value-text",
        div(class = "skeleton-box", style = "height: 12px; width: 30%; margin-bottom: 12px;"), # Título "FONTE"
        div(class = "skeleton-box", style = "height: 28px; width: 50%; margin-bottom: 10px; border-radius: 4px;"), # Texto Fonte (Maior)
        div(class = "skeleton-box", style = "height: 14px; width: 40%; border-radius: 2px;")   # Texto Variável (Menor)
      )
    )
  )
}

html_skeleton_completude <- function() {
  div(
    class = "skeleton-card-wrapper skel-container-grid",
    div(
      style = "width: 100%; display: flex; flex-direction: column;",
      div(class = "skeleton-box", style = "height: 12px; width: 60%; margin-bottom: 20px;"), # Título
      div(class = "skeleton-box", style = "height: 32px; width: 45%; margin-bottom: 12px; border-radius: 4px;"), # Status
      div(class = "skeleton-box", style = "height: 14px; width: 55%; margin-bottom: 20px;"), # Texto descritivo
      div(class = "skeleton-box", style = "height: 45px; width: 100%; margin-bottom: 25px; border-radius: 4px;"), # Gráfico
      div(class = "skeleton-box", style = "height: 35px; width: 100%; border-radius: 4px;") # Botão
    )
  )
}


# Esqueleto para o Gráfico de Barras / Linhas
html_skeleton_grafico <- function() {
  div(
    class = "skeleton-card-wrapper skel-container-grafico",

    # Topo: Simula o título e o botão de filtros
    div(
      style = "display: flex; justify-content: space-between; align-items: center; width: 100%; margin-bottom: 30px;",
      div(class = "skeleton-box", style = "height: 25px; width: 40%;"), # Título grande
      div(class = "skeleton-box", style = "height: 20px; width: 80px; border-radius: 15px;") # Botão de filtro fantasma
    ),

    # Eixo Y (Simulação)
    div(
      style = "display: flex; height: 100%; width: 100%; padding-left: 10px;",
      div(
        style = "display: flex; flex-direction: column; justify-content: space-between; height: 80%; width: 30px; margin-right: 15px;",
        div(class = "skeleton-box", style = "height: 10px; width: 100%;"),
        div(class = "skeleton-box", style = "height: 10px; width: 100%;"),
        div(class = "skeleton-box", style = "height: 10px; width: 100%;"),
        div(class = "skeleton-box", style = "height: 10px; width: 100%;")
      ),

      # Área do Gráfico (Simulando 5 colunas com alturas variadas)
      div(
        style = "display: flex; justify-content: space-around; align-items: flex-end; flex: 1; height: 80%; border-bottom: 2px solid #f6f7f8; padding-bottom: 5px;",
        div(class = "skeleton-box", style = "height: 40%; width: 12%; border-radius: 4px 4px 0 0;"),
        div(class = "skeleton-box", style = "height: 70%; width: 12%; border-radius: 4px 4px 0 0;"),
        div(class = "skeleton-box", style = "height: 90%; width: 12%; border-radius: 4px 4px 0 0;"),
        div(class = "skeleton-box", style = "height: 55%; width: 12%; border-radius: 4px 4px 0 0;"),
        div(class = "skeleton-box", style = "height: 30%; width: 12%; border-radius: 4px 4px 0 0;")
      )
    )
  )
}
mod_indicadores_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      div(
        class = "col-12",
        bs4Dash::bs4Card(
          width = 12,
          title = HTML("<b class = 'fonte-muito-grande''>&nbsp;Sobre o indicador</b>"),
          status = "white",
          solidHeader = TRUE,
          collapsible = FALSE,
          fluidRow(
            # Caixa 1: Cálculo
            div(
              class = "col-12 col-xl-4",
              div(
                style = "position: relative; width: 100%; min-height: 240px;", # Ajustado para bater com seu CSS real
                html_skeleton_calculo(), # Novo skeleton específico
                bs4Dash::valueBoxOutput(ns("vb_calculo"), width = "100%")
              )
            ),

            # Caixa 2: Fonte
            div(
              class = "col-12 col-xl-4",
              div(
                style = "position: relative; width: 100%; min-height: 240px;",
                html_skeleton_fonte(), # Novo skeleton específico
                bs4Dash::valueBoxOutput(ns("vb_fonte"), width = "100%")
              )
            ),

            # Caixa 3: Completude
            div(
              class = "col-12 col-xl-4",
              div(
                style = "position: relative; width: 100%; min-height: 240px;",
                html_skeleton_completude(),
                uiOutput(ns("vb_completude"))
              )
            )
          )
        )
      )
    ),
    fluidRow(
      id = ns("linha_graficos_individuais"),
      style = "padding-bottom: 42px; padding-top: 36px;",
      div(
        class = "col-12 col-xl-6",
        bs4Dash::bs4Card(
          width = 12,
          title = tagList(
            HTML("<b class = 'fonte-muito-grande''>&nbsp;Evolução do indicador ao longo do período</b>"),
            tags$div(
              class = "div-botao-filtros",
              style = "position: absolute; top: 8px; right: 20px;",
              tags$button(
                class = "filter-icon fonte-media",
                icon("filter", style = "background: none;")
              ),
              tags$b(
                class = "fonte-grande",
                "Filtros"
              ),
              tags$div(
                class = "filter-dropdown",
                div(
                  span(tags$b("Restringir a comparação a hospitais:"), class = "fonte-semi-grande"),
                  div(
                    style = "padding-left: 7.5px; padding-top: 4px; padding-bottom: 0.9rem;",
                    shinyWidgets::prettySwitch(
                      inputId = ns("input_linhas_restringir_natureza"),
                      label = "De mesma natureza",
                      status = "success",
                      fill = TRUE,
                      value = TRUE
                    ),
                    shinyWidgets::prettySwitch(
                      inputId = ns("input_linhas_restringir_categoria"),
                      label = "De mesma categoria",
                      status = "success",
                      fill = TRUE,
                      value = TRUE
                    )
                  )
                ),
                checkboxGroupInput(
                  inputId = ns("input_linhas_comparacoes"),
                  label = div(
                    class = "fonte-semi-grande",
                    style = "margin-bottom: 1px;",
                    tags$b("Linhas de comparação no gráfico:"),
                    br(),
                    tags$span(class = "fonte-aviso", style = "font-style: italic;", "Selecione até três opções")
                  ),
                  choiceNames = lapply(
                    c(
                      "Média nacional",
                      "UF",
                      "Macrorregião de saúde",
                      "Região de saúde",
                      "Município"#,
                      # "Hospitais públicos",
                      # "Hospitais mistos",
                      # "Hospitais privados"
                    ),
                    function(txt) tags$span(class = "fonte-media", txt)
                  ),
                  choiceValues = c("pais", "uf", "macro_rsaude", "rsaude", "municipio"),
                  selected = c("pais", "uf")
                )
              )
            )
          ),
          status = "white",
          solidHeader = TRUE,
          collapsible = FALSE,
          shinycssloaders::withSpinner(highcharter::highchartOutput(ns("line_hc"), height = 500))
        )
      ),
      div(
        class = "col-12 col-xl-6",
        bs4Dash::bs4Card(
          width = 12,
          title = tagList(
            HTML("<b class = 'fonte-muito-grande''>&nbsp;Comparação com outros estabelecimentos</b>"),
            tags$div(
              class = "div-botao-filtros",
              style = "position: absolute; top: 8px; right: 20px;",
              tags$button(
                class = "filter-icon fonte-media",
                icon("filter", style = "background: none;")
              ),
              tags$b(
                class = "fonte-grande",
                "Filtros"
              ),
              tags$div(
                class = "filter-dropdown",
                div(
                  span(tags$b("Restringir a comparação a hospitais:"), class = "fonte-semi-grande"),
                  div(
                    style = "padding-left: 7.5px; padding-top: 4px; padding-bottom: 0.9rem;",
                    shinyWidgets::prettySwitch(
                      inputId = ns("input_scatter_restringir_natureza"),
                      label = "De mesma natureza",
                      status = "success",
                      fill = TRUE,
                      value = TRUE
                    ),
                    shinyWidgets::prettySwitch(
                      inputId = ns("input_scatter_restringir_categoria"),
                      label = "De mesma categoria",
                      status = "success",
                      fill = TRUE,
                      value = TRUE
                    )
                  )
                ),
                div(
                  style = "padding-bottom: 0.9rem;",
                  shinyWidgets::prettyRadioButtons(
                    inputId = ns("input_scatter_estabelecimentos"),
                    label = div(
                      class = "fonte-semi-grande",
                      style = "margin-bottom: 1px;",
                      tags$b("Mostrar estabelecimentos:"),
                      br()
                    ),
                    choices = c(
                      "Do município" = "municipio",
                      "Da região de saúde" = "rsaude",
                      "Da macrorregião de saúde" = "macro_rsaude",
                      "Da UF" = "uf",
                      "Da região do país" = "regiao",
                      "De todo o país" = "pais"
                    ),
                    selected = "uf",
                    icon = icon("check", style = "background-color: #0A1E3C;"),
                    animation = "rotate"
                  )
                ),
                shinyWidgets::prettyRadioButtons(
                  inputId = ns("input_scatter_periodo"),
                  label = div(class = "fonte-semi-grande", tags$b("Calcular o indicador para:")),,
                  choices = c(
                    "O último mês selecionado" = "mes",
                    "Todo o período" = "periodo"
                  ),
                  icon = icon("check", style = "background-color: #0A1E3C; color: white;"),
                  animation = "rotate"
                )
              )
            )
          ),
          status = "white",
          solidHeader = TRUE,
          collapsible = FALSE,
          shinycssloaders::withSpinner(highcharter::highchartOutput(ns("scatter_hc"), height = 500))
        )
      )
    ),
    shinyjs::hidden(
      fluidRow(
        id = ns("linha_grafico_agrupado"),
        style = "padding-bottom: 42px; padding-top: 36px;",
        div(
          class = "col-12",
          bs4Dash::bs4Card(
            width = 12,
            title = tagList(
              HTML("<b class = 'fonte-muito-grande''>&nbsp;Evolução do indicador ao longo do período</b>"),
              tags$div(
                class = "div-botao-filtros",
                style = "position: absolute; top: 8px; right: 20px;",
                tags$button(
                  class = "filter-icon fonte-media",
                  icon("filter", style = "background: none;")
                ),
                tags$b(
                  class = "fonte-grande",
                  "Filtros"
                ),
                tags$div(
                  class = "filter-dropdown",
                  style = "width: 320px;",
                  div(
                    span(tags$b("Restringir a comparação a hospitais:"), class = "fonte-semi-grande"),
                    div(
                      style = "padding-left: 7.5px; padding-top: 4px; padding-bottom: 0.9rem;",
                      shinyWidgets::prettySwitch(
                        inputId = ns("input_barras_restringir_natureza"),
                        label = "De mesma natureza",
                        status = "success",
                        fill = TRUE,
                        value = TRUE
                      ),
                      shinyWidgets::prettySwitch(
                        inputId = ns("input_barras_restringir_categoria"),
                        label = "De mesma categoria",
                        status = "success",
                        fill = TRUE,
                        value = TRUE
                      )
                    )
                  ),
                  div(
                    style = "padding-bottom: 0.9rem;",
                    shinyWidgets::prettyRadioButtons(
                      inputId = ns("input_barras_comparacao_principal"),
                      label = div(
                        class = "fonte-semi-grande",
                        style = "margin-bottom: 1px;",
                        tags$b("Comparação principal:"),
                        br(),
                        tags$span(class = "fonte-aviso", style = "font-style: italic;", "Uma barra adicional será gerada para este nível")
                      ),
                      choices = c(
                        "Média nacional" = "pais",
                        "UF" = "uf",
                        "Macrorregião de saúde" = "macro_rsaude",
                        "Região de saúde" = "rsaude",
                        "Município" = "municipio"
                      ),
                      icon = icon("check", style = "background-color: #0A1E3C;"),
                      animation = "rotate"
                    )
                  ),
                  div(
                    style = "padding-bottom: 0.9rem;",
                    checkboxGroupInput(
                      inputId = ns("input_barras_comparacoes_adicionais"),
                      label = div(
                        class = "fonte-semi-grande",
                        style = "margin-bottom: 1px;",
                        tags$b("Comparações adicionais:"),
                        br(),
                        tags$span(class = "fonte-aviso", style = "font-style: italic;", "Passe o mouse por cima das barras para visualizá-las")
                      ),
                      choiceNames = lapply(
                        c(
                          "Média nacional",
                          "UF",
                          "Macrorregião de saúde",
                          "Região de saúde",
                          "Município"
                        ),
                        function(txt) tags$span(class = "fonte-media", txt)
                      ),
                      choiceValues = c("pais", "uf", "macro_rsaude", "rsaude", "municipio"),
                      selected = c("uf")
                    )
                  )
                )
              )
            ),
            status = "white",
            solidHeader = TRUE,
            collapsible = FALSE,
            div(
              style = "position: relative; width: 100%; min-height: 500px;", # 500px é o tamanho do seu hc
              html_skeleton_grafico(), # Esqueleto logo após a div
              highcharter::highchartOutput(ns("barras_hc"), height = 500)
            )
          )
        )
      )
    )
  )
}

#' indicadores Server Functions
#'
#' @noRd
mod_indicadores_server <- function(id, filtros){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Criando dataframes com as informações do indicador -------------------
    df_infos_indicador <- reactive({
      if (filtros()$input_desejo_visualizar == "individual") {
        tabela_indicadores |>
          dplyr::filter(sub_indicador == filtros()$input_sub_indicador)
      } else {
        tabela_indicadores |>
          dplyr::filter(nome_abreviado_indicador == filtros()$input_indicador) |>
          dplyr::mutate(categoria_sub_indicador = forcats::fct_inorder(categoria_sub_indicador))
      }
    })

    df_indicadores <- reactive({
      if (filtros()$input_bloco == "morbimortalidade_perinatal") {
        get(paste0("df_", unique(df_infos_indicador()$sub_bloco)))
      } else {
        get(paste0("df_", filtros()$input_bloco))
      }
    })

    df_incompletude_filtrado <- reactive({
      periodo <- filtros()$input_periodo
      data_inicio <- as.Date(periodo[1])
      data_fim <- as.Date(periodo[2])

      df_incompletude |>
        dplyr::filter(nome_fantasia == filtros()$input_hospital) |>
        dplyr::mutate(data_aux = as.Date(paste0(ano, "-", sprintf("%02d", mes), "-01"))) |>
        dplyr::filter(data_aux >= data_inicio & data_aux <= data_fim)
    })


    # Criando as caixinhas de "Sobre o indicador" --------------------------
    ## Cálculo
    output$vb_calculo <- bs4Dash::renderValueBox({

      calculo_por_extenso <- if (filtros()$input_desejo_visualizar == "individual") {
        df_infos_indicador()$calculo_por_extenso_sub_indicador
      } else {
        df_infos_indicador()$calculo_por_extenso_indicador_principal[1]
      }

      hospitalValueBox(
        "Cálculo",
        calculo_por_extenso,
        "divide",
        class_fonte_conteudo = "fonte-titulos-sobre"
      )
    })

    ## Fonte
    output$vb_fonte <- bs4Dash::renderValueBox({
      hospitalValueBox(
        "Fonte",
        {
          info <- df_infos_indicador() |>
            dplyr::slice(1)

          div(
           div(
             df_infos_indicador()$fonte[1]
           ),
           div(
             class = "completude-variaveis fonte-grande",
             HTML(formatar_lista_variaveis(info$variaveis_incompletude))
            )
          )
        },
        "database"
      )
    })

    ## Qualidade da informação
    # 2. Cálculo Dinâmico com extração automática do denominador
    calculos_completude <- reactive({
      df <- df_incompletude_filtrado()
      info <- df_infos_indicador() |> dplyr::slice(1)

      # Se não tem incompletude no metadado, não calcula
      if (isFALSE(info$tem_incompletude)) {
        return(list(agregado = NA, mensal = NULL, tem_info = FALSE))
      }

      formula_str <- info$calculo_incompletude

      # EXTRAÇÃO DO DENOMINADOR: Pegamos o código entre o "/" e o "*"
      # Regex: Busca o que está após '/ ' e antes de ' *'
      denom_code <- stringr::str_extract(formula_str, "(?<=/\\s).*(?=\\s\\*)")

      # Verificamos se o denominador acumulado do período é zero
      denominador <- eval(parse(text = denom_code), envir = df)

      if (is.na(denominador) || denominador == 0) {
        return(list(agregado = NA, mensal = NULL, denominador = 0, tem_info = TRUE))
      }

      # Cálculo Agregado Total
      inc <- eval(parse(text = formula_str), envir = df)

      # Cálculo Mensal com verificação de denominador por mês
      df_mensal <- df |>
        dplyr::group_by(data_aux) |>
        dplyr::summarise(
          # Calculamos o percentual e o valor do denominador para cada mês
          pct_inc = eval(parse(text = formula_str)),
          denominador_mes = eval(parse(text = denom_code)),
          .groups = "drop"
        ) |>
        # Proteção interna para não gerar erro no mutate se pct_inc não existir
        dplyr::mutate(
          pct = dplyr::if_else(denominador_mes > 0, round(100 - pct_inc, 1), NA_real_)
        )

      list(
        agregado = round(100 - inc, 1),
        mensal = df_mensal,
        denominador = denominador,
        tem_info = TRUE
      )
    })

    # 3. Lógica de Categorias (Ajustada para o novo fluxo)
    meta_info <- reactive({
      res <- calculos_completude()
      info_meta <- df_infos_indicador() |> dplyr::slice(1)

      if (isFALSE(info_meta$tem_incompletude) || is.na(res$agregado)) {
        return(list(label = "Não se aplica", class = "status-na", color = "#0A1E3C"))
      }

      if (res$agregado < 90) {
        list(label = "Inadequada", class = "status-inadequada", color = "#f59e0b")
      } else if (res$agregado < 95) {
        list(label = "Boa", class = "status-boa", color = "#10b981")
      } else {
        list(label = "Excelente", class = "status-excelente", color = "#059669")
      }
    })

    # --- OUTPUTS ---
    output$bullet_completude <- highcharter::renderHighchart({
      res <- calculos_completude()
      info <- meta_info()

      # Se for NA, a barra fica em 0 e as faixas ficam neutras
      valor_plot <- if(is.na(res$agregado)) 0 else res$agregado
      opacidade_banda <- if(is.na(res$agregado)) 0.03 else 0.08

      highcharter::highchart() |>
        highcharter::hc_chart(
          type = "bar",
          # O segredo: Dizer ao Highcharts para herdar o tamanho do pai!
          width = NULL,
          height = NULL,
          style = list(width = "100%", height = "100%"),
          backgroundColor = "transparent",
          margin = c(5, 0, 25, 0)
        ) |>
        highcharter::hc_xAxis(visible = FALSE) |>
        highcharter::hc_yAxis(
          min = 0, max = 100, title = list(text = NULL),
          tickPositions = c(0, 90, 95, 100),
          showLastLabel = FALSE,
          labels = list(
            format = "{value}",
            style = list(
              color = "#0A1E3C50",
              fontWeight = "normal"
            ),
            y = 15
          ),
          gridLineWidth = 0,
          # plotBands = list(
          #   list(from = 0, to = 90, color = sprintf("rgba(245, 158, 11, %f)", opacidade_banda)),
          #   list(from = 90, to = 95, color = sprintf("rgba(16, 185, 129, %f)", opacidade_banda)),
          #   list(from = 95, to = 100, color = sprintf("rgba(5, 150, 105, %f)", opacidade_banda))
          # ),
          plotBands = list(
            list(from = 0, to = 100, color = "#f1f5f9")
          ),
          plotLines = list(
            list(value = 90, color = "#0A1E3C50", width = 1.5, zIndex = 5, dashStyle = "Dash"),
            list(value = 95, color = "#0A1E3C50", width = 1.5, zIndex = 5, dashStyle = "Dash")
          )
        ) |>
        highcharter::hc_tooltip(
          useHTML = TRUE,      # Renderização nativa Highcharts (SVG)
          headerFormat = "",    # Remove o título automático
          outside = TRUE,       # Essencial para não ser cortado em gráficos pequenos (65px)
          shared = FALSE,
          style = list(color = "#0A1E3C"), # Azul escuro para sobriedade
          padding = 10,         # Respiro interno "Premium"
          # pointFormat foca apenas no dado principal (o valor da completude)
          pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> <b>{point.y}%</b> de completude<br/>",

          # footerFormat renderiza a legenda técnica com estilo reduzido
          footerFormat = paste0(
            "<div style=\"font-size: 0.85em; color: #0A1E3Caa; line-height: 1.4; margin-top: 8px; border-top: 1px solid #0A1E3C20; padding-top: 5px;\">",
            "<b>NÍVEIS DE COMPLETUDE:</b><br/>",
            "\u2022 Excelente: acima de 95%<br/>",
            "\u2022 Boa: entre 90% e 95%<br/>",
            "\u2022 Inadequada: abaixo de 90%",
            "</div>"
          )
        ) |>
        highcharter::hc_add_series(
          data = valor_plot,
          color = info$color,
          name = "Completude",
          showInLegend = FALSE,
          zIndex = 3
        ) |>
        highcharter::hc_plotOptions(series = list(borderWidth = 0, pointWidth = 14, borderRadius = 4, grouping = FALSE))
    })

    # Modal e Série Histórica
    observeEvent(input$btn_detalhes, {
      showModal(modalDialog(
        title = HTML(glue::glue("
          <div style = 'color: #0A1E3C;'>
            <div class = 'fonte-muito-grande' style='font-weight: 800;'>Evolução da completude da informação</div>
            <div class = 'fonte-grande' style='opacity: 0.8;'>Hospital selecionado: {filtros()$input_hospital}</div>
          </div>
        ")),
        size = "l",
        easyClose = TRUE,
        highcharter::highchartOutput(ns("grafico_linha")),
        footer = modalButton("Fechar")
      ))
    })

    output$grafico_linha <- highcharter::renderHighchart({
      res <- calculos_completude()
      info <- df_infos_indicador() |> dplyr::slice(1)

      if (is.null(res$mensal)) {
        return(highcharter::highchart() |> highcharter::hc_title(text = "Sem dados para o período"))
      }

      # Definição do título
      titulo_dinamico <- gerar_titulo_modal(info$variaveis_incompletude)

      # Nota de rodapé com concordância gramatical
      texto_vars <- formatar_vars_caption(info$variaveis_incompletude)
      caption_texto <- paste0("Nota: Foram considerados registros incompletos de ", texto_vars,
                              " aqueles com valores ", info$valores_invalidos, ".")

      highcharter::highchart() |>
        highcharter::hc_add_dependency("modules/series-label.js") |>
        highcharter::hc_chart(type = "line") |>
        highcharter::hc_title(
          text = titulo_dinamico
        ) |>
        highcharter::hc_caption(
          text = caption_texto
        ) |>
        highcharter::hc_xAxis(
          categories = format(res$mensal$data_aux, "%b/%y")
        ) |>
        highcharter::hc_yAxis(
          max = 100,
          title = list(text = "% de completude"),
          gridLineDashStyle = "Dot",
          labels = list(format = "{value}%")
        ) |>
        # 3. Série Principal: Hospital Selecionado
        highcharter::hc_add_series(
          name = "Hospital selecionado",
          data = res$mensal$pct,
          color = "#2c115f",
          lineWidth = 3
        ) |>
        # 1. Série de Referência: Excelente (95%)
        highcharter::hc_add_series(
          name = "Referência: Excelente (95%)",
          data = rep(95, nrow(res$mensal)),
          color = "#b73779",
          dashStyle = "ShortDash",
          opacity = 0.5,
          marker = list(enabled = FALSE),
          enableMouseTracking = FALSE # Para não poluir a tooltip
        ) |>
        # 2. Série de Referência: Bom (90%)
        highcharter::hc_add_series(
          name = "Referência: Bom (90%)",
          data = rep(90, nrow(res$mensal)),
          color = "#fc8961",
          dashStyle = "ShortDash",
          opacity = 0.5,
          marker = list(enabled = FALSE),
          enableMouseTracking = FALSE
        ) |>
        highcharter::hc_tooltip(
          shared = TRUE,
          crosshairs = TRUE,
          padding = 12,
          borderRadius = 8,
          backgroundColor = "rgba(255, 255, 255, 0.95)",
          pointFormat = "<span style='color:{point.color}'>\u25CF</span> {series.name}: <b>{point.y}%</b><br/>"
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            label = list(enabled = TRUE),
            allowPointSelect = TRUE
          )
        )
    })


    output$vb_completude <- renderUI({
      res <- calculos_completude()
      info_meta <- df_infos_indicador() |> dplyr::slice(1)
      info_cat <- meta_info()

      # 1. Definição da Mensagem Dinâmica baseada no denominador (ou numerador quando for o caso)
      if (info_meta$tipo_do_indicador == "numero") {
        denom_num <- info_meta$nome_numerador
      } else {
        denom_num <- info_meta$nome_denominador
      }
      denom_num_limpo <- limpar_nome_denominador(denom_num)
      msg_vazio <- paste0("Não foram registrados ", denom_num_limpo, " no período")

      # 2. Lógica Condicional de Exibição
      if (isFALSE(info_meta$tem_incompletude)) {
        # --- CASO: INDICADOR SEM INCOMPLETUDE MAPEADA ---
        status_label <- "Não se aplica"
        status_class <- "status-na"
        status_icon  <- NULL
        desc_html    <- ""
        conteudo_central <- div(
          style = "margin-top: 10px; min-height: 65px; display: flex; align-items: center;",
          div(class = "completude-desc fonte-media", info_meta$texto_incompletude)
        )
        exibir_botao <- FALSE
      } else {
        # --- CASO: POSSUI INCOMPLETUDE ---
        status_label <- info_cat$label
        status_class <- info_cat$class
        status_icon  <- info_cat$icon

        # Se o denominador for zero, mostra a mensagem dinâmica de vazio
        if (is.na(res$agregado)) {
          desc_html <- div(class = "completude-desc fonte-media", msg_vazio)
          conteudo_central <- div(style = "height: 65px;") # Espaço em branco no lugar do gráfico
          exibir_botao <- FALSE
        } else {
          desc_html <- div(class = "completude-desc fonte-media",
                           HTML(paste0("<b>", res$agregado, "%</b> dos casos preenchidos")))
          conteudo_central <- highcharter::highchartOutput(ns("bullet_completude"), height = "65px")
          exibir_botao <- TRUE
        }
      }

      # 3. Construção do Card Final
      bs4Dash::bs4Card(
        id = ns("card-completude"),
        width = 12, closable = FALSE, collapsible = FALSE, headerBorder = FALSE,
        class = "premium-card",
        div(
          class = "completude-box",
          # Usa Grid para forçar o limite de largura
          style = "display: grid; grid-template-columns: minmax(0, 1fr); width: 100%;",

          div( # Container interno para agrupar o texto
            span(class = "label-caps fonte-media", "Completude da Informação"),
            div(class = paste("completude-status fonte-destaque-caixas", status_class),
                status_label,
                if(!is.null(status_icon)) icon(status_icon, style = "font-size: 22px;")
            ),
            desc_html
          ),

          # O Gráfico
          div(
            id = ns("hc_completude_wrapper"),
            style = "width: 100%; height: 65px; margin-top: 5px; position: relative;",
            conteudo_central
          ),

          # Botão (Se houver)
          if (exibir_botao) {
            div(style = "width: 100%; display: flex; justify-content: flex-start;",
                actionButton(ns("btn_detalhes"),
                             HTML('Explorar série histórica <i class="fas fa-arrow-right"></i>'),
                             class = "btn-detalhes-completude fonte-pequena"))
          }
        )
      )
    })



    # Controle dos gráficos a serem mostrados ---------------------------------
    observe({
      if (filtros()$input_desejo_visualizar == "individual") {
        shinyjs::show(id = "linha_graficos_individuais", anim = TRUE, animType = "slide", time = 0.3)
        shinyjs::hide(id = "linha_grafico_agrupado", anim = FALSE)
      } else {
        shinyjs::show(id = "linha_grafico_agrupado", anim = TRUE, animType = "slide", time = 0.3)
        shinyjs::hide(id = "linha_graficos_individuais", anim = FALSE)
      }
    })


    # Para o gráfico de barras (evolução do indicador ao longo do período) ----
    ## Calculando o indicador para cada nível de análise possível -------------
    ### Recalculando as bases apenas quando algum input de interesse muda
    base_barras <- eventReactive(c(input$input_barras_restringir_natureza, input$input_barras_restringir_categoria, filtros()), {

      req(filtros()$input_desejo_visualizar == "agrupado")

      # 1. Configuração e extração de parâmetros
      ## Definindo os parâmetros iniciais a partir dos inputs
      df_base <- df_indicadores()
      hosp <- filtros()$input_hospital
      uf_sel <- filtros()$input_uf
      muni <- filtros()$input_municipio
      periodo <- filtros()$input_periodo

      # Capturamos os nomes das colunas existentes na base para validação
      colunas_na_base <- names(df_base)


      # 2. Lógica de adaptação dinâmica de fórmulas (PESO e MOMENTO)
      ##' Função auxiliar para expandir colunas "todos" para a soma dos filtros selecionados
      ##' @param formula_str A string original
      ##' @param info_row A linha do df_infos_indicador contendo as flags de filtro
      ##' @param colunas_validas Vetor com os nomes das colunas que realmente existem no df
      ajustar_formula_filtros <- function(formula_str, info_row, colunas_validas) {
        if (is.na(formula_str) || formula_str == "") return(formula_str)

        pattern <- "(obitos|principais|evitaveis|nascidos_vivos)_[a-z0-9_]+"

        stringr::str_replace_all(formula_str, pattern, function(match) {
          partes <- stringr::str_split(match, "_")[[1]]
          n <- length(partes)

          # CASO 1: Nascidos Vivos (Trata apenas PESO)
          if (stringr::str_starts(match, "nascidos_vivos")) {
            peso_orig <- partes[n]
            prefixo_base <- "nascidos_vivos"

            pesos_expandidos <- if (isTRUE(info_row$filtro_peso) && peso_orig == "todos") {
              filtros()$input_faixas_de_peso
            } else {
              peso_orig
            }

            novas_colunas <- paste(prefixo_base, pesos_expandidos, sep = "_")

            # Validação: Se a coluna não existe, vira 0
            novas_colunas <- ifelse(novas_colunas %in% colunas_validas, novas_colunas, "0")

            return(paste0("(", paste(novas_colunas, collapse = " + "), ")"))
          }

          # CASO 2: Óbitos (Trata PESO e MOMENTO)
          momento_orig <- partes[n]
          peso_orig <- partes[n-1]
          prefixo_base <- paste(partes[1:(n-2)], collapse = "_")

          tipo_obito <- dplyr::case_when(
            stringr::str_detect(match, "fetal|fetais") ~ "fetal",
            stringr::str_detect(match, "neonat") ~ "neonatal",
            stringr::str_detect(match, "perinat") ~ "perinatal",
            TRUE ~ "fetal"
          )

          momentos_validos_tipo <- switch(tipo_obito,
                                          "fetal" = c("sem_informacao", "antes", "durante"),
                                          "neonatal" = c("0_dias", "1_a_6_dias", "7_a_27_dias"),
                                          "perinatal" = c("sem_informacao", "antes", "durante", "0_dias", "1_a_6_dias")
          )

          pesos_expandidos <- if (isTRUE(info_row$filtro_peso) && peso_orig == "todos") {
            filtros()$input_faixas_de_peso
          } else {
            peso_orig
          }

          momentos_expandidos <- if (isTRUE(info_row$filtro_momento) && momento_orig == "todos") {
            intersect(filtros()$input_momentos_do_obito, momentos_validos_tipo)
          } else {
            momento_orig
          }

          if (length(momentos_expandidos) == 0) return("0")

          combinacoes <- expand.grid(p = pesos_expandidos, m = momentos_expandidos, stringsAsFactors = FALSE)
          novas_colunas <- paste(prefixo_base, combinacoes$p, combinacoes$m, sep = "_")

          # VALIDAÇÃO CRUCIAL: Se a coluna não existe na base, substituímos por "0" na string
          novas_colunas <- ifelse(novas_colunas %in% colunas_validas, novas_colunas, "0")

          # Retornamos a soma das colunas expandidas (ou zeros) entre parênteses
          paste0("(", paste(novas_colunas, collapse = " + "), ")")
        })
      }

      # Aplicamos a adaptação passando o vetor colunas_na_base
      df_infos_local <- df_infos_indicador() |>
        dplyr::rowwise() |>
        dplyr::mutate(
          calculo = ajustar_formula_filtros(calculo, dplyr::pick(everything()), colunas_na_base),
          numerador = ajustar_formula_filtros(numerador, dplyr::pick(everything()), colunas_na_base),
          denominador = ajustar_formula_filtros(denominador, dplyr::pick(everything()), colunas_na_base)
        ) |>
        dplyr::ungroup()

      # A partir daqui, y_exprs usará fórmulas seguras (Ex: "sum((0 + col_existente))")
      y_exprs <- df_infos_local$calculo


      # 3. Filtros temporais e geográficos
      ## Lógica para determinar a granularidade temporal
      data_inicio <- as.Date(periodo[1])
      data_fim <- as.Date(periodo[2])

      ## Calcula a diferença em meses
      diff_meses <- (as.numeric(format(data_fim, "%Y")) - as.numeric(format(data_inicio, "%Y"))) * 12 +
        (as.numeric(format(data_fim, "%m")) - as.numeric(format(data_inicio, "%m")))

      ## Lógica para definir os filtros de natureza
      naturezas <- if (isTRUE(input$input_barras_restringir_natureza)) {
        filtros()$input_natureza
      } else {
        if (df_infos_indicador()$fonte[1] == "SIH" & filtros()$input_natureza == "privado") {
          c("publico", "misto")
        } else {
          unique(df_cnes_aux$tipo)
        }
      }

      ## Lógica para definir os filtros de categoria
      categorias <- if (isTRUE(input$input_barras_restringir_categoria)) {
        filtros()$input_categoria
      } else {
        unique(df_cnes_aux$categoria_porte)
      }

      ## Preparação da base geral (tratamento e filtros)
      df_base <- df_base |>
        dplyr::mutate(
          # Criamos a data completa primeiro para filtrar o período corretamente
          data_aux = as.Date(paste0(ano, "-", sprintf("%02d", mes), "-01"))
        ) |>
        dplyr::filter(
          data_aux >= data_inicio & data_aux <= data_fim
        ) |>
        dplyr::mutate(
          # Se a diferença for > 12 meses, data_ref vira apenas o ANO (ex: 2023-01-01)
          # Caso contrário, mantém a data mensal normal
          data_ref = if (diff_meses > 12) {
            as.Date(paste0(ano, "-01-01"))
          } else {
            data_aux
          }
        ) |>
        dplyr::select(-data_aux)


      # 4. Definição da função de cálculo dinâmico
      ##' Calcula métricas dinamicamente baseado nas fórmulas de df_infos_indicador
      ##' @param dataset O dataframe filtrado para o recorte geográfico
      ##' @param nome_coluna_valor O nome que a coluna de resultado deve ter (ex: 'br_porc')
      calcular_metricas <- function(dataset, nome_coluna_valor) {
        # 1. Extração de fórmulas já adaptadas
        formulas_numeradores <- df_infos_local$numerador
        formula_denominador  <- df_infos_local$denominador[1]
        soma_numeradores_str <- paste(formulas_numeradores, collapse = " + ")

        formula_sem_info_str <- glue::glue(
          "round( pmax(0, {formula_denominador} - ({soma_numeradores_str})) / {formula_denominador} * 100, 1)"
        )

        # 2. Categorias existentes
        lista_resultados <- lapply(seq_len(nrow(df_infos_local)), function(i) {
          dataset |>
            dplyr::group_by(data_ref) |>
            dplyr::summarise(valor = !!rlang::parse_expr(df_infos_local$calculo[i]), .groups = "drop") |>
            dplyr::mutate(categoria_sub_indicador = as.character(df_infos_local$categoria_sub_indicador[i]))
        })

        # 3. Categoria "Sem informação"
        res_sem_info <- dataset |>
          dplyr::group_by(data_ref) |>
          dplyr::summarise(valor = !!rlang::parse_expr(formula_sem_info_str), .groups = "drop") |>
          dplyr::mutate(categoria_sub_indicador = "Sem informação")

        dplyr::bind_rows(dplyr::bind_rows(lista_resultados), res_sem_info) |>
          dplyr::rename(!!nome_coluna_valor := valor)
      }


      # 5. Criação das bases para cada nível de análise
      ## Nível 1: Hospital selecionado
      df_sel <- df_base |>
        dplyr::filter(
          nome_fantasia %in% hosp,
          uf == uf_sel
        )

      ## Extração segura das referências geográficas do hospital selecionado
      ## Se df_sel estiver vazio (sem dados no período), usamos NA para evitar erros
      if (nrow(df_sel) > 0) {
        macro_ref <- df_sel$macro_r_saude[1]
        r_saude_ref <- df_sel$r_saude[1]
      } else {
        macro_ref <- NA_character_
        r_saude_ref <- NA_character_
      }

      df_outros <- df_base |>
        dplyr::filter(tipo %in% naturezas, categoria_porte %in% categorias)

      ## Nível 2: Município
      df_muni <- df_outros |>
        dplyr::filter(municipio == muni, uf == uf_sel)

      ## Nível 3: Região de saúde
      df_rsaude <- df_outros |>
        dplyr::filter(r_saude == r_saude_ref, uf == uf_sel)

      ## Nível 4: Macrorregião de saúde
      df_macro <- df_outros |>
        dplyr::filter(macro_r_saude == macro_ref, uf == uf_sel)

      ## Nível 5: UF
      df_uf <- df_outros |>
        dplyr::filter(uf == uf_sel)

      ## Nível 6: Brasil
      df_br <- df_outros


      # 6. Execução dos cálculos
      ## Calculando para o hospital selecionado e adicionando a classe identificadora
      res_hosp <- calcular_metricas(df_sel, "porc") |>
        dplyr::mutate(class = "Hospital selecionado")

      ## Calculando para os níveis comparativos
      res_muni <- calcular_metricas(df_muni, "municipio_porc")
      res_rsaude <- calcular_metricas(df_rsaude, "r_saude_porc")
      res_macro <- calcular_metricas(df_macro, "macro_r_saude_porc")
      res_uf <- calcular_metricas(df_uf, "uf_porc")
      res_br <- calcular_metricas(df_br, "br_porc")


      # 7. Unificação final
      df_final <- res_br |>
        dplyr::left_join(res_hosp, by = c("data_ref", "categoria_sub_indicador")) |>
        dplyr::left_join(res_uf, by = c("data_ref", "categoria_sub_indicador")) |>
        dplyr::left_join(res_macro, by = c("data_ref", "categoria_sub_indicador")) |>
        dplyr::left_join(res_rsaude, by = c("data_ref", "categoria_sub_indicador")) |>
        dplyr::left_join(res_muni, by = c("data_ref", "categoria_sub_indicador"))

      # Pegamos os nomes das categorias da planilha (como texto)
      categorias_planilha <- as.character(unique(df_infos_indicador()$categoria_sub_indicador))

      # Verificamos se há dados faltantes de verdade
      tem_dados_faltantes <- any(df_final$porc[df_final$categoria_sub_indicador == "Sem informação"] > 0, na.rm = TRUE) ||
        any(df_final$br_porc[df_final$categoria_sub_indicador == "Sem informação"] > 0, na.rm = TRUE)

      if (tem_dados_faltantes) {
        # "Sem informação" por último
        niveis_finais <- c(categorias_planilha, "Sem informação")
      } else {
        niveis_finais <- categorias_planilha
        df_final <- df_final |> dplyr::filter(categoria_sub_indicador != "Sem informação")
      }

      df_final <- df_final |>
        dplyr::mutate(
          # Primeiro converte para texto (limpa NAs fantasmas), depois para fator com a ordem certa
          categoria_sub_indicador = as.character(categoria_sub_indicador),
          categoria_sub_indicador = factor(categoria_sub_indicador, levels = niveis_finais)
        ) |>
        dplyr::filter(!is.na(categoria_sub_indicador)) |> # Remove qualquer resíduo
        dplyr::arrange(categoria_sub_indicador)

      df_final <- df_final |>
        dplyr::mutate(
          # Criamos um ID seguro para o Highcharts (ex: "cat_branca")
          cat_id = janitor::make_clean_names(as.character(categoria_sub_indicador)),

          hosp_line = dplyr::if_else(
            is.na(porc), "",
            paste0("<br> &emsp;Hospital selecionado: ", format(round(porc, 1), nsmall = 1), "%")
          ),
          #porc = tidyr::replace_na(porc, 0),
          class = tidyr::replace_na(class, "Hospital selecionado")
        )

    }, ignoreNULL = FALSE, ignoreInit = FALSE)


    ## Plotando o gráfico ---------------------------------------------------
    output$barras_hc <- highcharter::renderHighchart({

      req(filtros()$input_desejo_visualizar == "agrupado")

      if (df_infos_indicador()$fonte[1] == "SIH" & filtros()$input_natureza == "privado") {
        return(
          highcharter::highchart() |>
            highcharter::hc_title(
              text = "<span class='fonte-grande' style='color:#888; font-weight: 500;'>⚠ Indicadores do SIH não estão disponíveis para hospitais privados.</span>",
              useHTML = TRUE,
              align = "center",
              verticalAlign = "middle"
            )

        )
      }


      # 1. Configuração e extração de parâmetros
      ## Definindo os parâmetros iniciais a partir dos inputs
      info <- df_infos_indicador()
      periodo <- filtros()$input_periodo

      ## Cálculo da granularidade de tempo
      data_inicio <- as.Date(periodo[1])
      data_fim <- as.Date(periodo[2])
      diff_meses <- (as.numeric(format(data_fim, "%Y")) - as.numeric(format(data_inicio, "%Y"))) * 12 +
        (as.numeric(format(data_fim, "%m")) - as.numeric(format(data_inicio, "%m")))

      is_anual <- diff_meses > 12
      label_eixo_x <- if (is_anual) "Ano" else "Mês"
      formato_data <- if (is_anual) "%Y" else "%b/%y"

      ## Definindo os inputs de controle
      restringir_natureza <- input$input_barras_restringir_natureza
      restringir_categoria <- input$input_barras_restringir_categoria

      ## Definindo as seleções de comparação
      comp_principal <- input$input_barras_comparacao_principal
      comps_adicionais <- input$input_barras_comparacoes_adicionais

      # 2. Mapeamento (input -> coluna do DF -> rótulo legível)
      ## Criando um dicionário para traduzir o input em nome de coluna
      dict_cols <- c(
        "pais" = "br_porc",
        "uf" = "uf_porc",
        "macro_rsaude" = "macro_r_saude_porc",
        "rsaude" = "r_saude_porc",
        "municipio" = "municipio_porc"
      )

      ## Criando um dicionário para traduzir o input em rótulo para a tooltip/legenda
      dict_labels <- c(
        "pais" = "Média nacional",
        "uf" = "UF",
        "macro_rsaude" = "Macrorregião de saúde",
        "rsaude" = "Região de saúde",
        "municipio" = "Município"
      )

      ## Identificando a coluna e o nome da comparação principal
      col_principal <- dict_cols[[comp_principal]]
      lbl_principal <- dict_labels[[comp_principal]]

      # 3. Construção dinâmica da tooltip
      ## Passo A: Montar o trecho das comparações adicionais
      ## Itera sobre os inputs adicionais e cria linhas HTML: "<br> Label: {point.coluna}%"
      txt_comps_adicionais <- ""

      if (!is.null(comps_adicionais) && length(comps_adicionais) > 0) {
        comps_extras <- setdiff(comps_adicionais, comp_principal)

        if (length(comps_extras) > 0) {
          txt_comps_adicionais <- paste0(
            vapply(comps_extras, function(k) {
              # Lógica: se a coluna existir (não for null/NA), mostra o valor. Senão, N/A
              glue::glue("<br> &emsp;{dict_labels[[k]]}: {{#if point.{dict_cols[[k]]}}} {{point.{dict_cols[[k]]}:.1f}}% {{else}} N/A {{/if}}")
            }, character(1)),
            collapse = ""
          )
        }
      }

      ## Adicionando a mensagem de filtro ativo
      tooltip_alerta_filtro <- if (restringir_natureza | restringir_categoria) {
        glue::glue("<br><br><span style='font-size: 0.85em; color: #888888; font-style: italic;'>
                   FILTRO ATIVO: comparações restringidas a hospitais de mesma {ifelse(restringir_natureza, 'natureza', '')}{ifelse(restringir_natureza & restringir_categoria, ' e ', '')}{ifelse(restringir_categoria, 'categoria', '')}
                   </span>")
      } else {
        ""
      }

      ## Passo B: Montar a tooltip da série 1 (hospital selecionado)
      ## Mostra: Hospital (destaque) + Comp Principal + Comps Adicionais
      tooltip_hosp <- glue::glue(
        "<div style='line-height: 1.2em;'>
          <span style = 'color: {{series.color}}'> &#9679 </span> <b>{{series.name}} ({{point.class}}):</b> {{point.y:.1f}}%
          <br>
          <span style='font-size: 0.95em; opacity: 0.95'>
            &emsp;{lbl_principal}: {{point.{col_principal}:.1f}}%{txt_comps_adicionais}
          </span>
          {tooltip_alerta_filtro}
        </div>"
      )

      ## Passo C: Montar a tooltip da Série 2 (comparação principal)
      ## Mostra: Comp Principal (destaque) + Hospital + Comps Adicionais
      tooltip_comp <- glue::glue(
        "<div style='line-height: 1.2em;'>
          <span style = 'color: {{series.color}}'> &#9679 </span> <b>{{series.name}} ({lbl_principal}):</b> {{point.y:.1f}}%
          <span style='font-size: 0.95em; opacity: 0.95'>
            {{point.hosp_line}}{txt_comps_adicionais}
          </span>
          {tooltip_alerta_filtro}
        </div>"
      )

      # 4. Elementos textuais e formatação do gráfico
      y_suffix <- if (!is.null(info$tipo_do_indicador) && any(info$tipo_do_indicador == "porcentagem")) "%" else ""

      ano_ini <- format(as.Date(periodo[1]), "%Y")
      ano_fim <- format(as.Date(periodo[2]), "%Y")

      subtitulo <- if (ano_ini == ano_fim) {
        paste0(stringr::str_to_sentence(format(as.Date(periodo[1]), "%B", locale = "pt_BR.UTF-8")), " a ", format(as.Date(periodo[2]), "%B de %Y", locale = "pt_BR.UTF-8"))
      } else {
        paste0(stringr::str_to_sentence(format(as.Date(periodo[1]), "%B de %Y", locale = "pt_BR.UTF-8")), " a ", format(as.Date(periodo[2]), "%B de %Y", locale = "pt_BR.UTF-8"))
      }

      texto_extra <- if (restringir_natureza | restringir_categoria) {
        glue::glue("FILTRO ATIVO: comparações restringidas a hospitais de mesma {ifelse(restringir_natureza, 'natureza', '')}{ifelse(restringir_natureza & restringir_categoria, ' e ', '')}{ifelse(restringir_categoria, 'categoria', '')}")
      } else {
        ''
      }

      # Definição dinâmica de cores
      categorias_ativas <- levels(droplevels(base_barras()$categoria_sub_indicador))
      n_cats <- length(categorias_ativas)

      if ("Sem informação" %in% categorias_ativas) {
        # Cor cinza para "Sem informação", restante magma
        cores <- c(viridis::magma(n_cats + 1, direction = 1)[-c(1, n_cats + 1)], "#D3D3D3")
      } else {
        cores <- viridis::magma(n_cats + 2, direction = 1)[-c(1, n_cats + 2)]

      }

      # 5. Plotagem
      hc <- highcharter::highchart() |>
        highcharter::hc_title(text = info$indicador_principal[1], align = "left") |>
        highcharter::hc_subtitle(text = subtitulo, align = "left") |>
        highcharter::hc_caption(text = texto_extra, align = "left") |>
        highcharter::hc_xAxis(
          type = "datetime",
          title = list(text = label_eixo_x), # DINÂMICO: "Ano" ou "Mês"
          labels = list(
            useHTML = TRUE,
            y = 15,
            formatter = highcharter::JS(glue::glue("
              function() {{
                // Formata a data dinamicamente baseada no R
                var dataLabel = Highcharts.dateFormat('{formato_data}', this.value);

                return '<div style=\"text-align: center; line-height: 1.2;\">' +
                       '<span>H. &nbsp;&nbsp; C.</span>' +
                       '<br/>' +
                       '<span>' + dataLabel + '</span>' +
                       '</div>';
              }}
            "))
          )
        ) |>
        highcharter::hc_yAxis(
          title = list(text = y_suffix),
          labels = list(format = "{value}%"),
          max = 100
        )

      # 6. Adicionando as Séries via Loop
      # niveis_finais contém a ordem correta (incluindo "Sem informação")
      niveis_finais <- levels(base_barras()$categoria_sub_indicador)

      for (i in seq_along(niveis_finais)) {
        cat_nome <- niveis_finais[i]
        dados_cat <- base_barras() |> dplyr::filter(categoria_sub_indicador == cat_nome)
        cor_cat <- cores[i]
        id_cat <- dados_cat$cat_id[1] # Nosso ID limpo

        # PARTE A: Série do Hospital (A que aparece na legenda)
        hc <- hc |> highcharter::hc_add_series(
          data = dados_cat,
          type = "column",
          highcharter::hcaes(x = data_ref, y = porc),
          name = cat_nome,
          id = id_cat, # ID da série para o vínculo
          color = cor_cat,
          stack = 0,
          showInLegend = TRUE,
          tooltip = list(pointFormat = tooltip_hosp)
        )

        # PARTE B: Série de Comparação (Vinculada à primeira)
        hc <- hc |> highcharter::hc_add_series(
          data = dados_cat,
          type = "column",
          highcharter::hcaes(x = data_ref, y = !!rlang::sym(col_principal), hosp_line = hosp_line),
          name = cat_nome,
          linkedTo = id_cat, # VÍNCULO REAL AQUI
          color = cor_cat,
          stack = 1,
          opacity = 1, # Deixamos 1 e controlamos o visual no plotOptions
          showInLegend = FALSE,
          tooltip = list(pointFormat = tooltip_comp)
        )
      }

      # 7. Configuração Global de Destaque
      hc |>
        highcharter::hc_plotOptions(
          series = list(
            stacking = "percent",
            connectNulls = FALSE,
            borderWidth = 0.5,
            states = list(
              inactive = list(opacity = 0.15), # Apaga o que não for a categoria selecionada
              hover = list(enabled = TRUE, brightness = 0.1)
            )
          ),
          column = list(
            groupPadding = 0.25,
            pointPadding = 0.05
          )
        ) |>
        highcharter::hc_legend(
          title = list(text = info$label_input_categoria[1])
        )
    })


    # Para o gráfico de linhas (evolução do indicador ao longo do período) ----
    ## Guardará a seleção anterior
    prev_sel <- reactiveVal(character())

    ## Limite de 3 seleções, revertendo o último acréscimo
    observeEvent(input$input_linhas_comparacoes, {
      new <- input$input_linhas_comparacoes
      if (is.null(new)) new <- character(0)
      old <- prev_sel()

      # Houve acréscimo?
      added <- setdiff(new, old)

      if (length(new) > 3 && length(added) > 0) {
        # Reverte exatamente o último clique (volta ao estado anterior)
        updateCheckboxGroupInput(
          session,
          "input_linhas_comparacoes",
          selected = old
        )
        showNotification("Selecione no máximo três grupos de estabelecimentos.", type = "message", duration = 3)
        return(invisible(NULL))
      }

      # Se não excedeu (ou foi remoção), aceita e memoriza
      prev_sel(new)
    }, ignoreInit = FALSE)


    ## Calculando o indicador para cada nível de análise possível -------------
    ### Criando um ambiente reativo global para cache das bases
    bases_medias <- reactiveValues()

    ### Recalculando as bases apenas quando filtros() muda
    observeEvent(c(input$input_linhas_restringir_natureza, input$input_linhas_restringir_categoria, filtros()), {

      req(filtros()$input_desejo_visualizar == "individual")

      # 1. Configuração e extração de parâmetros
      df_base <- df_indicadores()
      hosp <- filtros()$input_hospital
      uf_sel <- filtros()$input_uf
      muni <- filtros()$input_municipio
      periodo <- filtros()$input_periodo


      # 2. Lógica de adaptação dinâmica da fórmula (PESO e MOMENTO)
      ##' Função auxiliar para expandir colunas "todos"
      ajustar_formula_filtros <- function(formula_str, info_row) {
        if (is.na(formula_str) || formula_str == "") return(formula_str)

        # Regex expandido para incluir nascidos_vivos
        pattern <- "(obitos|principais|evitaveis|nascidos_vivos)_[a-z0-9_]+"

        stringr::str_replace_all(formula_str, pattern, function(match) {
          partes <- stringr::str_split(match, "_")[[1]]
          n <- length(partes)

          # CASO 1: Nascidos Vivos (Trata apenas PESO)
          if (stringr::str_starts(match, "nascidos_vivos")) {
            peso_orig <- partes[n] # O peso é o último segmento (ex: "todos")
            prefixo_base <- "nascidos_vivos"

            pesos_expandidos <- if (isTRUE(info_row$filtro_peso) && peso_orig == "todos") {
              filtros()$input_faixas_de_peso
            } else {
              peso_orig
            }

            novas_colunas <- paste(prefixo_base, pesos_expandidos, sep = "_")
            return(paste0("(", paste(novas_colunas, collapse = " + "), ")"))
          }

          # CASO 2: Óbitos (Trata PESO e MOMENTO)
          # Por padrão, os dois últimos segmentos são sempre PESO e MOMENTO
          momento_orig <- partes[n]
          peso_orig <- partes[n-1]
          prefixo_base <- paste(partes[1:(n-2)], collapse = "_")

          tipo_obito <- dplyr::case_when(
            stringr::str_detect(match, "fetal|fetais") ~ "fetal",
            stringr::str_detect(match, "neonat") ~ "neonatal",
            stringr::str_detect(match, "perinat") ~ "perinatal",
            TRUE ~ "fetal"
          )

          momentos_validos_tipo <- switch(tipo_obito,
                                          "fetal" = c("sem_informacao", "antes", "durante"),
                                          "neonatal" = c("0_dias", "1_a_6_dias", "7_a_27_dias"),
                                          "perinatal" = c("sem_informacao", "antes", "durante", "0_dias", "1_a_6_dias")
          )

          pesos_expandidos <- if (isTRUE(info_row$filtro_peso) && peso_orig == "todos") {
            filtros()$input_faixas_de_peso
          } else {
            peso_orig
          }

          momentos_expandidos <- if (isTRUE(info_row$filtro_momento) && momento_orig == "todos") {
            intersect(filtros()$input_momentos_do_obito, momentos_validos_tipo)
          } else {
            momento_orig
          }

          if (length(momentos_expandidos) == 0) return("0")

          combinacoes <- expand.grid(p = pesos_expandidos, m = momentos_expandidos, stringsAsFactors = FALSE)
          novas_colunas <- paste(prefixo_base, combinacoes$p, combinacoes$m, sep = "_")

          return(paste0("(", paste(novas_colunas, collapse = " + "), ")"))
        })
      }

      ## Adaptamos a fórmula de cálculo baseada nos filtros de peso e momento
      ## Usamos a primeira linha do df_infos_indicador pois o desejo é "individual"
      y_expr <- ajustar_formula_filtros(df_infos_indicador()$calculo[1], df_infos_indicador()[1, ])


      # 3. Filtros de Natureza e Categoria
      naturezas <- if (input$input_linhas_restringir_natureza == TRUE) {
        filtros()$input_natureza
      } else {
        if (df_infos_indicador()$fonte[1] == "SIH" & filtros()$input_natureza == "privado") {
          c("publico", "misto")
        } else {
          unique(df_cnes_aux$tipo)
        }
      }

      categorias <- if (input$input_linhas_restringir_categoria == TRUE) {
        filtros()$input_categoria
      } else {
        unique(df_cnes_aux$categoria_porte)
      }


      # 4. Preparação da base e cálculo por nível
      df_base <- df_base |>
        dplyr::mutate(data_ref = as.Date(paste0(ano, "-", sprintf("%02d", mes), "-01"))) |>
        dplyr::filter(
          data_ref >= as.Date(periodo[1]) & data_ref <= as.Date(periodo[2])
        )

      df_sel <- dplyr::filter(
        df_base,
        nome_fantasia %in% hosp,
        uf == uf_sel
      )

      ## Cálculo para o hospital selecionado
      df_sel_line <- df_sel |>
        dplyr::group_by(data_ref) |>
        dplyr::summarise(y = mean(base::eval(base::parse(text = y_expr))), .groups = "drop")

      ## Salvar hospital
      bases_medias$hospital <- df_sel_line |>
        dplyr::transmute(x = data_ref, y = y)

      ## Função auxiliar de média
      calc_media <- function(filtro) {
        df_base |>
          dplyr::filter(tipo %in% naturezas, categoria_porte %in% categorias) |>
          filtro() |>
          dplyr::group_by(data_ref) |>
          dplyr::summarise(y = mean(base::eval(base::parse(text = y_expr))), .groups = "drop")
      }

      ## Salvar médias para os níveis comparativos
      bases_medias$municipio <- calc_media(function(d) dplyr::filter(d, municipio == muni & uf == uf_sel))
      bases_medias$macro_rsaude <- calc_media(function(d) dplyr::filter(d, macro_r_saude == unique(df_sel$macro_r_saude)[1] & uf == uf_sel))
      bases_medias$rsaude <- calc_media(function(d) dplyr::filter(d, r_saude == unique(df_sel$r_saude)[1] & uf == uf_sel))
      bases_medias$uf <- calc_media(function(d) dplyr::filter(d, .data$uf == uf_sel))
      bases_medias$pais <- calc_media(function(d) d)  # média nacional (sem filtro geográfico)
    }, ignoreNULL = FALSE, ignoreInit = FALSE)


    ## Plotando o gráfico ---------------------------------------------------
    output$line_hc <- highcharter::renderHighchart({

      req(filtros()$input_desejo_visualizar == "individual")

      if (df_infos_indicador()$fonte[1] == "SIH" & filtros()$input_natureza == "privado") {
        return(
          highcharter::highchart() |>
            highcharter::hc_title(
              text = "<span class='fonte-grande' style='color:#888; font-weight: 500;'>⚠ Indicadores do SIH não estão disponíveis para hospitais privados.</span>",
              useHTML = TRUE,
              align = "center",
              verticalAlign = "middle"
            )

        )
      }

      info <- df_infos_indicador()
      comps <- input$input_linhas_comparacoes
      periodo <- filtros()$input_periodo
      restringir_natureza <- input$input_linhas_restringir_natureza
      restringir_categoria <- input$input_linhas_restringir_categoria

      # Sufixo para tooltip
      y_suffix <- if (!is.null(info$tipo_do_indicador) && info$tipo_do_indicador == "porcentagem") "%" else ""

      # Subtítulo dinâmico
      ano_ini <- format(as.Date(periodo[1]), "%Y")
      ano_fim <- format(as.Date(periodo[2]), "%Y")
      subtitulo <- if (ano_ini == ano_fim) {
        paste0(
          stringr::str_to_sentence(format(as.Date(periodo[1]), "%B", locale = "pt_BR.UTF-8")),
          " a ",
          format(as.Date(periodo[2]), "%B de %Y", locale = "pt_BR.UTF-8")
        )
      } else {
        paste0(
          stringr::str_to_sentence(format(as.Date(periodo[1]), "%B de %Y", locale = "pt_BR.UTF-8")),
          " a ",
          format(as.Date(periodo[2]), "%B de %Y", locale = "pt_BR.UTF-8")
        )
      }

      texto_extra <- if (restringir_natureza | restringir_categoria) {
        glue::glue(
          "FILTRO ATIVO: comparações restringidas a hospitais de mesma {ifelse(restringir_natureza, 'natureza', '')}{ifelse(restringir_natureza & restringir_categoria, ' e ', '')}{ifelse(restringir_categoria, 'categoria', '')}"
        )
      } else {
        ''
      }

      ## STRING DO FILTRO PARA O GRÁFICO DE LINHAS (NOVO)
      # No gráfico de linhas a tooltip é 'shared = TRUE', então o ideal é
      # colocar o alerta no rodapé da tooltip, usando o 'footerFormat'
      tooltip_alerta_filtro_linha <- if (restringir_natureza | restringir_categoria) {
        glue::glue("<br><span style='font-size: 0.85em; color: #888888; font-style: italic;'>
                   FILTRO ATIVO: comparações restringidas a hospitais de mesma {ifelse(restringir_natureza, 'natureza', '')}{ifelse(restringir_natureza & restringir_categoria, ' e ', '')}{ifelse(restringir_categoria, 'categoria', '')}
                   </span>")
      } else {
        ""
      }

      hc <- highcharter::highchart() |>
        highcharter::hc_add_dependency("modules/series-label.js") |>
        highcharter::hc_chart(type = "line") |>
        highcharter::hc_title(
          text = info$sub_indicador,
          align = "left",
          minScale = 0.9,
          style = list(transition = "transform 0.4s ease")
        ) |>
        highcharter::hc_subtitle(text = subtitulo, align = "left") |>
        highcharter::hc_caption(
          text = texto_extra,
          align = "left"
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            label = list(enabled = TRUE),
            allowPointSelect = TRUE
          )
        ) |>
        highcharter::hc_xAxis(type = "datetime", title = list(text = "Mês")) |>
        highcharter::hc_yAxis(title = list(text = y_suffix), ceiling = ifelse(y_suffix == "%", 100, "undefined")) |>
        highcharter::hc_tooltip(
          shared = TRUE,
          crosshairs = TRUE,
          useHTML = TRUE,
          backgroundColor = "rgb(255, 255, 255, 1)",
          style = list(
            opacity = 1
          ),
          pointFormat = paste0(
            "<span style='color:{point.color}'>&#9679;</span> ",
            "<b>{series.name}:</b> {point.y}", y_suffix, "<br>"
          ),
          footerFormat = tooltip_alerta_filtro_linha
        )

      # Linha do hospital sempre
      hc <- hc |> highcharter::hc_add_series(
        name = ifelse(length(filtros()$input_hospital) == 1, "Hospital selecionado", "Hospitais selecionados"),
        data = highcharter::list_parse2(req(bases_medias$hospital)),
        zIndex = 5
      )

      # Tabela de dashStyles conforme categoria
      dash_styles <- c(
        municipio = "ShortDot",
        macro_rsaude = "ShortDashDotDot",
        rsaude    = "ShortDash",
        uf        = "ShortDash",
        # publicos  = "ShortDashDot",
        # privados  = "ShortDashDotDot",
        # mistos    = "ShortDashDot",
        pais      = "ShortDot"
      )

      # Adicionar linhas ativas com estilos corretos
      if (filtros()$input_nivel == "estabelecimento") {
        comps <- comps
      } else if (filtros()$input_nivel == "uf") {
        comps <- "pais"
      } else {
        comps <- c("pais", "uf")
      }

      for (categ in comps) {
        nome_legenda <- switch(categ,
                               "municipio" = "Média do município",
                               "macro_rsaude" = "Média da macrorregião de saúde",
                               "rsaude"    = "Média da região de saúde",
                               "uf"        = "Média da UF",
                               # "publicos"  = "Média dos hospitais públicos",
                               # "privados"  = "Média dos hospitais privados",
                               # "mistos"    = "Média dos hospitais mistos",
                               "pais"      = "Média nacional"
        )

        hc <- hc |> highcharter::hc_add_series(
          name = nome_legenda,
          data = highcharter::list_parse2(req(bases_medias[[categ]])),
          dashStyle = dash_styles[[categ]],
          zIndex = 1
        )
      }

      # Aplicar paleta de cores
      cores <- c("#0A1E3C", "rgba(50,160,255,0.6)", "rgba(65,190,60,0.6)", "rgb(250, 200, 15, 0.6)")
      hc |> highcharter::hc_colors(cores)
    })


    # Para o scatterplot (comparação com outros estabelecimentos) ------------
    ## Calculando o indicador para cada categoria possível -------------------
    ### Criando um ambiente reativo global para cache das bases
    bases_scatter <- reactiveValues(
      mes_sel = NULL, mes_others = NULL,
      periodo_sel = NULL, periodo_others = NULL,
      periodo = NULL
    )

    ## Recalculando as bases apenas quando filtros() mudar
    observeEvent(filtros(), {
      bases_scatter$mes_sel <- NULL
      bases_scatter$mes_others <- NULL
      bases_scatter$periodo_sel <- NULL
      bases_scatter$periodo_others <- NULL
      bases_scatter$periodo <- as.Date(filtros()$input_periodo)
    }, ignoreInit = FALSE, priority = 10)


    ### Calcula bases conforme modo selecionado (só se não existir ainda)
    observeEvent(list(filtros(), input$input_scatter_periodo), {

      req(filtros()$input_desejo_visualizar == "individual")

      modo <- match.arg(input$input_scatter_periodo, c("mes", "periodo"))
      periodo <- as.Date(filtros()$input_periodo)

      # Se já existe cache para o modo atual, não recalcula
      if (modo == "mes" && !is.null(bases_scatter$mes_sel)) return(invisible())
      if (modo == "periodo" && !is.null(bases_scatter$periodo_sel)) return(invisible())

      # 1. Lógica de adaptação dinâmica da fórmula (PESO e MOMENTO)

      ##' Função auxiliar para expandir colunas "todos"
      ajustar_formula_filtros <- function(formula_str, info_row) {
        if (is.na(formula_str) || formula_str == "") return(formula_str)

        pattern <- "(obitos|principais|evitaveis|nascidos_vivos)_[a-z0-9_]+"

        stringr::str_replace_all(formula_str, pattern, function(match) {
          partes <- stringr::str_split(match, "_")[[1]]
          n <- length(partes)

          # CASO 1: Nascidos Vivos
          if (stringr::str_starts(match, "nascidos_vivos")) {
            peso_orig <- partes[n]
            prefixo_base <- "nascidos_vivos"

            pesos_expandidos <- if (isTRUE(info_row$filtro_peso) && peso_orig == "todos") {
              filtros()$input_faixas_de_peso
            } else {
              peso_orig
            }

            novas_colunas <- paste(prefixo_base, pesos_expandidos, sep = "_")
            return(paste0("(", paste(novas_colunas, collapse = " + "), ")"))
          }

          # CASO 2: Óbitos
          momento_orig <- partes[n]
          peso_orig <- partes[n-1]
          prefixo_base <- paste(partes[1:(n-2)], collapse = "_")

          tipo_obito <- dplyr::case_when(
            stringr::str_detect(match, "fetal|fetais") ~ "fetal",
            stringr::str_detect(match, "neonat") ~ "neonatal",
            stringr::str_detect(match, "perinat") ~ "perinatal",
            TRUE ~ "fetal"
          )

          momentos_validos_tipo <- switch(tipo_obito,
                                          "fetal" = c("sem_informacao", "antes", "durante"),
                                          "neonatal" = c("0_dias", "1_a_6_dias", "7_a_27_dias"),
                                          "perinatal" = c("sem_informacao", "antes", "durante", "0_dias", "1_a_6_dias")
          )

          pesos_expandidos <- if (isTRUE(info_row$filtro_peso) && peso_orig == "todos") {
            filtros()$input_faixas_de_peso
          } else {
            peso_orig
          }

          momentos_expandidos <- if (isTRUE(info_row$filtro_momento) && momento_orig == "todos") {
            intersect(filtros()$input_momentos_do_obito, momentos_validos_tipo)
          } else {
            momento_orig
          }

          if (length(momentos_expandidos) == 0) return("0")

          combinacoes <- expand.grid(p = pesos_expandidos, m = momentos_expandidos, stringsAsFactors = FALSE)
          novas_colunas <- paste(prefixo_base, combinacoes$p, combinacoes$m, sep = "_")

          return(paste0("(", paste(novas_colunas, collapse = " + "), ")"))
        })
      }

      info <- df_infos_indicador()
      # Adaptamos as fórmulas de numerador (denominador) e cálculo antes de prosseguir
      # Usamos as flags da primeira linha (individual)
      x_expr_raw <- ajustar_formula_filtros(info$denominador[1], info[1, ])
      y_expr_raw <- ajustar_formula_filtros(info$calculo[1], info[1, ])

      # 2. Configuração de parâmetros e bases
      hosp <- filtros()$input_hospital
      uf_in <- filtros()$input_uf
      muni_in <- filtros()$input_municipio
      rsaude <- filtros()$input_r_saude
      macro_rsaude <- filtros()$input_macro_r_saude
      regiao <- filtros()$input_regiao_pais

      df_base <- df_indicadores() |>
        dplyr::mutate(
          data_ref = as.Date(paste0(ano, "-", sprintf("%02d", mes), "-01"))
        )

      if (modo == "mes") {
        df_use <- df_base |>
          dplyr::filter(data_ref == periodo[2])
      } else {
        df_use <- df_base |>
          dplyr::filter(data_ref >= periodo[1], data_ref <= periodo[2]) |>
          dplyr::group_by(cnes, nome_fantasia, municipio, r_saude, macro_r_saude, uf, regiao) |>
          dplyr::summarise(
            tipo = dplyr::last(tipo),
            categoria_porte = dplyr::last(categoria_porte),
            dplyr::across(
              .cols = where(is.numeric),
              .fns = ~ sum(.x, na.rm = TRUE)
            ),
            .groups = "drop"
          )
      }

      # Seleção direta do hospital
      df_sel <- dplyr::filter(
        df_use,
        nome_fantasia %in% hosp,
        uf == uf_in
      )
      df_others <- dplyr::filter(
        df_use,
        !(nome_fantasia %in% hosp & uf == uf_in)
      )

      # 3. Preparação das expressões para o Scatterplot
      # O segredo aqui é:
      # 1. Primeiro expandimos a fórmula (já feito em x_expr_raw e y_expr_raw)
      # 2. Depois removemos o sum() para que o cálculo funcione por linha/hospital

      limpar_sum <- function(expr) {
        # Remove sum(...) mas mantém o conteúdo interno
        # Ex: sum((col1 + col2)) vira (col1 + col2)
        stringr::str_replace_all(expr, "sum\\(([^\\)]+)\\)", "\\1")
      }

      x_expr_final <- limpar_sum(x_expr_raw)
      y_expr_final <- limpar_sum(y_expr_raw)

      # Pré-compila expressões para cálculo
      x_expr_parsed <- base::parse(text = x_expr_final)
      y_expr_parsed <- base::parse(text = y_expr_final)

      x_eval <- function(df) base::eval(x_expr_parsed, envir = df)
      y_eval <- function(df) base::eval(y_expr_parsed, envir = df)

      # 4. Montagem dos data.frames finais
      prep_points <- function(df) {
        if (nrow(df) == 0) return(df)
        x_vals <- x_eval(df)
        y_vals <- y_eval(df)
        dplyr::mutate(
          df,
          x = x_vals,
          y = y_vals,
          name = stringr::str_squish(base::toupper(nome_fantasia))
        ) |>
          dplyr::select(x, y, name, tipo, categoria_porte, municipio, r_saude, macro_r_saude, uf, regiao)
      }

      df_sel_ready <- prep_points(df_sel)
      df_others_ready <- prep_points(df_others)

      # Atualiza cache
      if (modo == "mes") {
        bases_scatter$mes_sel <- df_sel_ready
        bases_scatter$mes_others <- df_others_ready
      } else {
        bases_scatter$periodo_sel <- df_sel_ready
        bases_scatter$periodo_others <- df_others_ready
      }

    }, ignoreInit = FALSE, ignoreNULL = FALSE)


    ## Plotando o gráfico ---------------------------------------------------
    output$scatter_hc <- highcharter::renderHighchart({

      req(filtros()$input_desejo_visualizar == "individual")

      if (df_infos_indicador()$fonte[1] == "SIH" & filtros()$input_natureza == "privado") {
        return(
          highcharter::highchart() |>
            highcharter::hc_title(
              text = "<span class='fonte-grande' style='color:#888; font-weight: 500;'>⚠ Indicadores do SIH não estão disponíveis para hospitais privados.</span>",
              useHTML = TRUE,
              align = "center",
              verticalAlign = "middle"
            )

        )
      }

      info <- df_infos_indicador()
      grupos <- if (filtros()$input_nivel == "estabelecimento") {
        input$input_scatter_estabelecimentos
      } else {
        NULL
      }
      modo <- input$input_scatter_periodo
      periodo <- as.Date(filtros()$input_periodo)
      restringir_natureza <- input$input_scatter_restringir_natureza
      restringir_categoria <- input$input_scatter_restringir_categoria

      ## Lógica para definir os filtros de natureza
      naturezas <- if (isTRUE(restringir_natureza)) {
        filtros()$input_natureza
      } else {
        if (df_infos_indicador()$fonte[1] == "SIH" & filtros()$input_natureza == "privado") {
          c("publico", "misto")
        } else {
          unique(df_cnes_aux$tipo)
        }
      }

      ## Lógica para definir os filtros de categoria
      categorias <- if (isTRUE(restringir_categoria)) {
        filtros()$input_categoria
      } else {
        unique(df_cnes_aux$categoria_porte)
      }

      # Escolher bases conforme o modo
      if (modo == "mes") {
        df_sel <- req(bases_scatter$mes_sel)
        df_others <- req(bases_scatter$mes_others)
      } else {
        df_sel <- req(bases_scatter$periodo_sel)
        df_others <- req(bases_scatter$periodo_others)
      }

      # Subtítulo
      subtitulo <- if (modo == "mes") {
        paste0(
          stringr::str_to_title(base::format(periodo[2], "%B")),
          " de ", base::format(periodo[2], "%Y")
        )
      } else {
        ano_ini <- format(periodo[1], "%Y")
        ano_fim <- format(periodo[2], "%Y")
        if (ano_ini == ano_fim) {
          paste0(
            stringr::str_to_sentence(format(periodo[1], "%B", locale = "pt_BR.UTF-8")),
            " a ",
            format(periodo[2], "%B de %Y", locale = "pt_BR.UTF-8")
          )
        } else {
          paste0(
            stringr::str_to_sentence(format(periodo[1], "%B de %Y", locale = "pt_BR.UTF-8")),
            " a ",
            format(periodo[2], "%B de %Y", locale = "pt_BR.UTF-8")
          )
        }
      }

      # Sufixo do eixo/tooltip
      y_suffix <- if (!is.null(info$tipo_do_indicador) && info$tipo_do_indicador == "porcentagem") "%" else ""

      # Texto sobre as restrições da comparação
      texto_extra <- if (restringir_natureza | restringir_categoria) {
        glue::glue("FILTRO ATIVO: comparações restringidas a hospitais de mesma {ifelse(restringir_natureza, 'natureza', '')}{ifelse(restringir_natureza & restringir_categoria, ' e ', '')}{ifelse(restringir_categoria, 'categoria', '')}")
      } else {
        ''
      }

      # Filtra "Outros" pelos grupos selecionados
      df_others_plot <- df_others
      if (!is.null(grupos) && length(grupos) > 0 && nrow(df_others_plot) > 0) {
        df_others_plot <- df_others_plot |>
          dplyr::filter(
            if (input$input_scatter_estabelecimentos == "regiao")
              regiao == filtros()$input_regiao_pais
            else if (input$input_scatter_estabelecimentos == "uf")
              uf == filtros()$input_uf
            else if (input$input_scatter_estabelecimentos == "macro_rsaude")
              macro_r_saude == filtros()$input_macro_r_saude & uf == filtros()$input_uf
            else if (input$input_scatter_estabelecimentos == "rsaude")
              r_saude == filtros()$input_r_saude & uf == filtros()$input_uf
            else if (input$input_scatter_estabelecimentos == "municipio")
              municipio == filtros()$input_municipio & uf == filtros()$input_uf
            else
              TRUE
          ) |>
          dplyr::filter(
            tipo %in% naturezas,
            categoria_porte %in% categorias
          )
      }

      # Define o nome da série dos outros hospitais
      dict_labels <- c(
        "pais" = "Outros hospitais do país",
        "regiao" = "Outros hospitais da região do país",
        "uf" = "Outros hospitais da UF",
        "macro_rsaude" = "Outros hospitais da macrorregião de saúde",
        "rsaude" = "Outros hospitais da região de saúde",
        "municipio" = "Outros hospitais do município"
      )

      nome_serie_outros <- dict_labels[[input$input_scatter_estabelecimentos]]

      # Prepara os dados para o gráfico
      data_others <- highcharter::list_parse(df_others_plot)
      data_sel <- highcharter::list_parse(df_sel)

      # Cria o gráfico
      highcharter::highchart() |>
        highcharter::hc_chart(type = "scatter") |>
        highcharter::hc_title(
          text = paste(info$sub_indicador, "versus", tolower(info$nome_denominador)),
          align = "left",
          minScale = 0.9,
          style = list(transition = "transform 0.4s ease")
        ) |>
        highcharter::hc_subtitle(text = subtitulo, align = "left") |>
        highcharter::hc_caption(
          text = texto_extra,
          align = "left"
        ) |>
        highcharter::hc_xAxis(title = list(text = info$nome_denominador)) |>
        highcharter::hc_yAxis(title = list(text = y_suffix), ceiling = ifelse(y_suffix == "%", 100, "undefined")) |>
        highcharter::hc_tooltip(
          useHTML = TRUE,
          backgroundColor = "rgb(255, 255, 255, 1)",
          style = list(
            opacity = 1
          ),
          headerFormat = "{point.name} ({point.municipio}, {point.uf})",
          pointFormat = paste0(
            "<br><br><span style='color:{point.color}'>&#9679;</span> ",
            "<b>", info$sub_indicador, ":</b> {point.y}", y_suffix, "<br>",
            "<span style='padding-left: 11px'>", info$nome_denominador, ": {point.x}"
          )
        ) |>
        # 1) Outros hospitais
        highcharter::hc_add_series(
          name = nome_serie_outros,
          data = data_others,
          color = "#32A0FF",
          marker = list(radius = 3, symbol = "circle"),
          showInLegend = TRUE
        ) |>
        # 2) Hospital selecionado
        highcharter::hc_add_series(
          name = ifelse(length(filtros()$input_hospital) == 1, "Hospital selecionado", "Hospitais selecionados"),
          data = data_sel,
          color = "#0A1E3C",
          marker = list(radius = ifelse(length(filtros()$input_hospital) == 1, 6, 5), symbol = "diamond"),
          showInLegend = TRUE
        )
    })


  })
}

## To be copied in the UI
# mod_indicadores_ui("indicadores_1")

## To be copied in the server
# mod_indicadores_server("indicadores_1")
