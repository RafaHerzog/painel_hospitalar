#' indicadores UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indicadores_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      div(
        style = "padding-top: 10px",
        class = "col-12",
        bs4Dash::bs4Card(
          width = 12,
          title = HTML("<b class = 'fonte-muito-grande''>&nbsp;Sobre o indicador</b>"),
          status = "white",
          solidHeader = TRUE,
          collapsible = FALSE,
          fluidRow(
            div(
              class = "col-12 col-xl-4",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_calculo"), width = "100%"), proxy.height = 190)
            ),
            div(
              class = "col-12 col-xl-4",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_fonte"), width = "100%"), proxy.height = 190)
            ),
            div(
              class = "col-12 col-xl-4",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_qualidade"), width = "100%"), proxy.height = 190)
            )
          )
        )
      )
    ),
    fluidRow(
      style = "padding-bottom: 42px; padding-top: 36px;",
      div(
        class = "col-12 col-xl-6",
        bs4Dash::bs4Card(
          width = 12,
          title = tagList(
            HTML("<b class = 'fonte-muito-grande''>&nbsp;Evolução do indicador ao longo do período</b>"),
            tags$div(
              style = "position: absolute; top: 12px; right: 25px;",
              tags$button(
                class = "filter-icon fonte-grande custom-tooltip",
                `data-tooltip` = "Filtros do gráfico",
                icon("filter")
              ),
              tags$div(
                class = "filter-dropdown",
                checkboxGroupInput(
                  inputId = ns("input_linhas_estabelecimentos"),
                  label = div(
                    class = "fonte-grande",
                    style = "margin-bottom: 1px;",
                    tags$b("Comparar com estabelecimentos:"),
                    br(),
                    tags$span(class = "fonte-aviso", style = "font-style: italic;", "Selecione até três opções")
                  ),
                  choiceNames = NULL,
                  choiceValues = NULL,
                  selected = NULL
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
              style = "position: absolute; top: 12px; right: 25px;",
              tags$button(
                class = "filter-icon fonte-grande custom-tooltip",
                `data-tooltip` = "Filtros do gráfico",
                icon("filter")
              ),
              tags$div(
                class = "filter-dropdown",
                checkboxGroupInput(
                  inputId = ns("input_scatter_estabelecimentos"),
                  label = div(class = "fonte-grande", tags$b("Mostrar estabelecimentos:")),
                  choiceNames = lapply(
                    c(
                      "Do município",
                      "Da região de saúde, mas fora do município",
                      "Da UF, mas fora da região de saúde",
                      "Da região do país, mas fora da UF",
                      "De outras regiões"
                    ),
                    function(txt) tags$span(class = "fonte-semi-grande", txt)
                  ),
                  choiceValues = c("municipio", "rsaude", "uf", "regiao", "outros"),
                  selected = c("municipio", "rsaude", "uf")
                ),
                shinyWidgets::prettyRadioButtons(
                  inputId = ns("input_scatter_periodo"),
                  label = div(class = "fonte-grande", tags$b("Calcular o indicador para:")),,
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
    )
  )
}

#' indicadores Server Functions
#'
#' @noRd
mod_indicadores_server <- function(id, filtros){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # Criando um dataframe com as informações do indicador --------------------
    df_infos_indicador <- reactive({
      tabela_indicadores |>
        dplyr::filter(sub_indicador == filtros()$input_indicador)
    })

    df_indicadores <- reactive({
      get(paste0("df_", filtros()$input_bloco))
    })


    # Criando as caixinhas de "Sobre o indicador" ----------------------------------------
    ## Cálculo
    output$vb_calculo <- bs4Dash::renderValueBox({
      hospitalValueBox(
        "Cálculo",
        "A fazer",
        "divide"
      )
    })

    ## Fonte
    output$vb_fonte <- bs4Dash::renderValueBox({
      hospitalValueBox(
        "Fonte",
        "A fazer",
        "database"
      )
    })

    ## Qualidade da informação
    output$vb_qualidade <- bs4Dash::renderValueBox({
      hospitalValueBox(
        "Qualidade da informação",
        "A fazer",
        "list-check"
      )
    })


    # Criando o scatterplot (comparação com outros estabelecimentos) ----------
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
    }, ignoreInit = TRUE, priority = 10)


    ### Calcula bases conforme modo selecionado (só se não existir ainda)
    observeEvent(list(filtros(), input$input_scatter_periodo), {

      modo <- match.arg(input$input_scatter_periodo, c("mes", "periodo"))
      periodo <- as.Date(filtros()$input_periodo)

      # Se já existe cache para o modo atual, não recalcula
      if (modo == "mes" && !is.null(bases_scatter$mes_sel)) return(invisible())
      if (modo == "periodo" && !is.null(bases_scatter$periodo_sel)) return(invisible())

      info <- df_infos_indicador()
      x_expr <- info$denominador
      y_expr <- info$calculo

      hosp <- filtros()$input_hospital
      uf_in <- filtros()$input_uf
      muni_in <- filtros()$input_municipio
      rsaude <- filtros()$input_r_saude
      regiao <- filtros()$input_regiao_pais

      # Base com data_ref e classificação de grupo_estab
      df_base <- df_indicadores() |>
        dplyr::mutate(
          data_ref = as.Date(paste0(ano, "-", sprintf("%02d", mes), "-01")),
          grupo_estab = dplyr::case_when(
            municipio == muni_in ~ "municipio",
            r_saude == rsaude ~ "rsaude",
            uf == uf_in ~ "uf",
            regiao == regiao ~ "regiao",
            TRUE ~ "outros"
          )
        )

      if (modo == "mes") {
        df_use <- df_base |>
          dplyr::filter(data_ref == periodo[2])
      } else {
        df_use <- df_base |>
          dplyr::filter(data_ref >= periodo[1], data_ref <= periodo[2]) |>
          dplyr::group_by(cnes, nome_fantasia, municipio, uf, grupo_estab) |>
          dplyr::summarise(
            dplyr::across(
              .cols = where(is.numeric),
              .fns = ~ sum(.x, na.rm = TRUE)
            ),
            .groups = "drop"
          )
      }

      # Seleção direta do hospital (sem normalização)
      df_sel <- dplyr::filter(
        df_use,
        nome_fantasia %in% hosp,
        uf == uf_in
      )
      df_others <- dplyr::filter(
        df_use,
        !(nome_fantasia %in% hosp & uf == uf_in))

      # Pré-compila expressões para cálculo (evita parse repetido)
      x_expr_parsed <- base::parse(text = gsub("sum\\(([^\\)]+)\\)", "\\1", x_expr))
      y_expr_parsed <- base::parse(text = gsub("sum\\(([^\\)]+)\\)", "\\1", y_expr))

      x_eval <- function(df) base::eval(x_expr_parsed, envir = df)
      y_eval <- function(df) base::eval(y_expr_parsed, envir = df)

      # Monta data.frames finais
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
          dplyr::select(x, y, name, municipio, uf, grupo_estab)
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
    }, ignoreInit = TRUE, ignoreNULL = FALSE)


    # Plotando o gráfico ---------------------------------------------------
    output$scatter_hc <- highcharter::renderHighchart({
      info <- df_infos_indicador()
      grupos <- if (filtros()$input_nivel == "estabelecimento") {
        input$input_scatter_estabelecimentos
      } else {
        NULL
      }
      modo <- input$input_scatter_periodo
      periodo <- as.Date(filtros()$input_periodo)

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

      # Filtra "Outros" pelos grupos selecionados
      df_others_plot <- df_others
      if (!is.null(grupos) && length(grupos) > 0 && nrow(df_others_plot) > 0) {
        df_others_plot <- dplyr::filter(df_others_plot, grupo_estab %in% grupos)
      }

      data_others <- highcharter::list_parse(df_others_plot)
      data_sel <- highcharter::list_parse(df_sel)

      highcharter::highchart() |>
        highcharter::hc_chart(type = "scatter") |>
        highcharter::hc_title(text = paste(info$sub_indicador, "versus", tolower(info$nome_denominador)), align = "left") |>
        highcharter::hc_subtitle(text = subtitulo, align = "left") |>
        highcharter::hc_xAxis(title = list(text = info$nome_denominador)) |>
        highcharter::hc_yAxis(title = list(text = y_suffix), max = 100) |>
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
            "<span style='padding-left: 10px'>", info$nome_denominador, ": {point.x}"
          )
        ) |>
        # 1) Outros hospitais
        highcharter::hc_add_series(
          name = "Outros hospitais",
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



    # Para o gráfico de linhas (evolução do indicador ao longo do período) ----
    ## Definindo as escolhas do input -----------------------------------------
    observe({
      if (filtros()$input_nivel == "estabelecimento") {
        if (filtros()$input_natureza == "publico") {
          updateCheckboxGroupInput(
            session,
            inputId = "input_linhas_estabelecimentos",
            choiceNames = lapply(
              c(
                "De todo o país",
                "Da UF",
                "Da macrorregião de saúde",
                "Da região de saúde",
                "Do município",
                "De mesma natureza (pública)",
                "De natureza mista",
                "De natureza privada"
              ),
              function(txt) tags$span(class = "fonte-semi-grande", txt)
            ),
            choiceValues = c("pais", "uf", "macro_rsaude", "rsaude", "municipio", "publicos", "mistos", "privados"),
            selected = c("pais", "uf")
          )
        } else if (filtros()$input_natureza == "misto") {
          updateCheckboxGroupInput(
            session,
            inputId = "input_linhas_estabelecimentos",
            choiceNames = lapply(
              c(
                "De todo o país",
                "Da UF",
                "Da macrorregião de saúde",
                "Da região de saúde",
                "Do município",
                "De mesma natureza (mista)",
                "De natureza pública",
                "De natureza privada"
              ),
              function(txt) tags$span(class = "fonte-semi-grande", txt)
            ),
            choiceValues = c("pais", "uf", "macro_rsaude", "rsaude", "municipio", "mistos", "publicos", "privados"),
            selected = c("pais", "uf")
          )
        } else {
          updateCheckboxGroupInput(
            session,
            inputId = "input_linhas_estabelecimentos",
            choiceNames = lapply(
              c(
                "De todo o país",
                "Da UF",
                "Da macrorregião de saúde",
                "Da região de saúde",
                "Do município",
                "De mesma natureza (privada)",
                "De natureza pública",
                "De natureza mista"
              ),
              function(txt) tags$span(class = "fonte-semi-grande", txt)
            ),
            choiceValues = c("pais", "uf", "macro_rsaude", "rsaude", "municipio", "privados", "publicos", "mistos"),
            selected = c("pais", "uf")
          )
        }
      }
    })

    ## Guardará a seleção anterior
    prev_sel <- reactiveVal(character())

    ## Limite de 3 seleções, revertendo o último acréscimo
    observeEvent(input$input_linhas_estabelecimentos, {
      new <- input$input_linhas_estabelecimentos
      if (is.null(new)) new <- character(0)
      old <- prev_sel()

      # Houve acréscimo?
      added <- setdiff(new, old)

      if (length(new) > 3 && length(added) > 0) {
        # Reverte exatamente o último clique (volta ao estado anterior)
        updateCheckboxGroupInput(
          session,
          "input_linhas_estabelecimentos",
          selected = old
        )
        showNotification("Selecione no máximo três grupos de estabelecimentos.", type = "message", duration = 3)
        return(invisible(NULL))
      }

      # Se não excedeu (ou foi remoção), aceita e memoriza
      prev_sel(new)
    }, ignoreInit = TRUE)


    ## Calculando o indicador para cada categoria possível -------------------
    ### Criando um ambiente reativo global para cache das bases
    bases_medias <- reactiveValues()

    ### Recalculando as bases apenas quando filtros() muda
    observeEvent(filtros(), {
      df_base <- df_indicadores()
      hosp    <- filtros()$input_hospital
      uf_sel  <- filtros()$input_uf
      muni    <- filtros()$input_municipio
      periodo <- filtros()$input_periodo
      y_expr  <- df_infos_indicador()$calculo

      df_base <- df_base |>
        dplyr::mutate(data_ref = as.Date(paste0(ano, "-", sprintf("%02d", mes), "-01"))) |>
        dplyr::filter(data_ref >= as.Date(periodo[1]) & data_ref <= as.Date(periodo[2]))

      df_sel <- dplyr::filter(
        df_base,
        nome_fantasia %in% hosp,
        uf == uf_sel
      )

      df_sel_line <- df_sel |>
        dplyr::group_by(data_ref) |>
        dplyr::summarise(y = mean(base::eval(base::parse(text = y_expr))), .groups = "drop")

      # salvar hospital
      bases_medias$hospital <- df_sel_line |>
        dplyr::transmute(x = data_ref, y = y)

      # Função auxiliar de média
      calc_media <- function(filtro) {
        df_base |>
          filtro() |>
          dplyr::group_by(data_ref) |>
          dplyr::summarise(y = mean(base::eval(base::parse(text = y_expr))), .groups = "drop")
      }

      # salvar médias
      bases_medias$municipio <- calc_media(function(d) dplyr::filter(d, municipio == muni & uf == uf_sel))
      bases_medias$macro_rsaude <- calc_media(function(d) dplyr::filter(d, macro_r_saude == unique(df_sel$macro_r_saude)[1] & uf == uf_sel))
      bases_medias$rsaude       <- calc_media(function(d) dplyr::filter(d, r_saude == unique(df_sel$r_saude)[1] & uf == uf_sel))
      bases_medias$uf            <- calc_media(function(d) dplyr::filter(d, .data$uf == uf_sel))
      bases_medias$publicos      <- calc_media(function(d) dplyr::filter(d, tipo == "publico"))
      bases_medias$privados      <- calc_media(function(d) dplyr::filter(d, tipo == "privado"))
      bases_medias$mistos        <- calc_media(function(d) dplyr::filter(d, tipo == "misto"))
      bases_medias$pais          <- calc_media(function(d) d)  # média nacional (sem filtro)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)


    ## Plotando o gráfico ---------------------------------------------------
    output$line_hc <- highcharter::renderHighchart({
      info     <- df_infos_indicador()
      comps    <- input$input_linhas_estabelecimentos
      periodo  <- filtros()$input_periodo

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

      hc <- highcharter::highchart() |>
        highcharter::hc_chart(type = "line") |>
        highcharter::hc_title(text = info$sub_indicador, align = "left") |>
        highcharter::hc_subtitle(text = subtitulo, align = "left") |>
        highcharter::hc_xAxis(type = "datetime", title = list(text = "Mês")) |>
        highcharter::hc_yAxis(title = list(text = y_suffix)) |>
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
          )
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
        publicos  = "ShortDashDot",
        privados  = "ShortDashDotDot",
        mistos    = "ShortDashDot",
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
                               "municipio" = "Média dos hospitais do município",
                               "macro_rsaude" = "Média dos hospitais da macrorregião de saúde",
                               "rsaude"    = "Média dos hospitais da região de saúde",
                               "uf"        = "Média dos hospitais da UF",
                               "publicos"  = "Média dos hospitais públicos",
                               "privados"  = "Média dos hospitais privados",
                               "mistos"    = "Média dos hospitais mistos",
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
      cores <- c("#0A1E3C", "rgba(50,160,255,0.7)", "rgba(65,190,60,0.7)", "rgb(250, 200, 15, 1)")
      hc |> highcharter::hc_colors(cores)
    })


  })
}

## To be copied in the UI
# mod_indicadores_ui("indicadores_1")

## To be copied in the server
# mod_indicadores_server("indicadores_1")
