#' visao_geral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visao_geral_ui <- function(id) {
  ns <- NS(id)
  options(
    spinner.color = "grey",
    spinner.color.background = "#ffffff",
    spinner.size = 0.5,
    spinner.type = 6
  )
  tagList(
    # Linha contendo os três cards de apresentação do hospital
    fluidRow(
      div(
        class = "col-12",
        bs4Dash::bs4Card(
          width = 12,
          title = HTML("<b class = 'fonte-muito-grande''>&nbsp;Informações do estabelecimento</b>"),
          status = "white",
          solidHeader = TRUE,
          collapsible = FALSE,
          fluidRow(
            div(
              class = "col-12 col-xl-4",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_hospital"), width = "100%"), proxy.height = 235)
            ),
            div(
              class = "col-12 col-xl-4",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_natureza"), width = "100%"), proxy.height = 235)
            ),
            div(
              class = "col-12 col-xl-4",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_categoria"), width = "100%"), proxy.height = 235)
            )
          )
        )
      )
    ),
    # Linha contendo outras informações
    fluidRow(
      ## Coluna da esquerda: resumo da estrutura do hospital
      div(
        class = "col-12",
        style = "padding-top: 36px",
        bs4Dash::bs4Card(
          width = 12,
          title = HTML("<b class = 'fonte-muito-grande''>&nbsp;Resumo da estrutura</b>"),
          status = "white",
          solidHeader = TRUE,
          collapsible = FALSE,
          div(
            class = "col-12",
            tags$b(class = "fonte-muito-grande", HTML("Leitos")),
            tags$hr(style = "margin-top: 0.5rem")
          ),
          div(
            class = "col-12",
            uiOutput(ns("mes1")),
          ),
          fluidRow(
            div(
              class = "col-12 col-lg-6 col-xl-3",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_leitos_obstetricos"), width = "100%"), proxy.height = 233)
            ),
            div(
              class = "col-12 col-lg-6 col-xl-3",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_leitos_obstetricos_sus"), width = "100%"), proxy.height = 233)
            ),
            div(
              class = "col-12 col-lg-6 col-xl-3",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_leitos_uti_adulto"), width = "100%"), proxy.height = 233)
            ),
            div(
              class = "col-12 col-lg-6 col-xl-3",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_leitos_uti_neonatal"), width = "100%"), proxy.height = 233)
            )
          )
        )
      ),
      ## Coluna direita: Resumo dos dados de atendimento
      div(
        class = "col-12",
        style = "padding-top: 36px",
        bs4Dash::bs4Card(
          width = 12,
          title = HTML("<b class = 'fonte-muito-grande''>&nbsp;Resumo dos dados de atendimento</b>"),
          status = "white",
          solidHeader = TRUE,
          collapsible = FALSE,
          div(
            class = "col-12",
            tags$b(class = "fonte-muito-grande", HTML("Nascimentos")),
            tags$hr(style = "margin-top: 0.5rem")
          ),
          div(
            class = "col-12",
            uiOutput(ns("mes2")),
          ),
          fluidRow(
            div(
              class = "col-12 col-xl-4",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_total_nascidos"), width = "100%"), proxy.height = 233)
            ),
            div(
              class = "col-12 col-xl-4",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_prop_cesarianas"), width = "100%"), proxy.height = 233)
            ),
            div(
              class = "col-12 col-xl-4",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_prematuros"), width = "100%"), proxy.height = 233)
            )
          ),
          div(
            class = "col-12",
            style = "padding-top: 24px;",
            tags$b(class = "fonte-muito-grande", HTML("Mortalidade e morbidade materna")),
            tags$hr(style = "margin-top: 0.5rem")
          ),
          div(
            class = "col-12",
            uiOutput(ns("mes3")),
          ),
          fluidRow(
            div(
              class = "col-12 col-xl-4 offset-xl-2",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_obitos_maternos"), width = "100%"), proxy.height = 233)
            ),
            div(
              class = "col-12 col-xl-4",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_casos_mmg"), width = "100%"), proxy.height = 233)
            )
          ),
          div(
            class = "col-12",
            style = "padding-top: 24px;",
            tags$b(class = "fonte-muito-grande", HTML("Mortalidade perinatal")),
            tags$hr(style = "margin-top: 0.5rem")
          ),
          div(
            class = "col-12",
            uiOutput(ns("mes4")),
          ),
          fluidRow(
            div(
              class = "col-12 col-xl-4 offset-xl-2",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_obitos_fetais"), width = "100%"), proxy.height = 233)
            ),
            div(
              class = "col-12 col-xl-4",
              shinycssloaders::withSpinner(bs4Dash::valueBoxOutput(ns("vb_obitos_neonatais"), width = "100%"), proxy.height = 233)
            )
          )
        )
      )
    )
  )
}

#' visao_geral Server Functions
#'
#' @noRd
mod_visao_geral_server <- function(id, filtros){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$mes1 <- output$mes2 <- output$mes3 <- output$mes4 <- renderUI({
        p(
          class = "fonte-grande",
          style = "font-style: italic;",
          paste0(
            stringr::str_to_title(base::format(as.Date(filtros()$input_periodo[2]), "%B")),
            " de ",
            base::format(as.Date(filtros()$input_periodo[2]), "%Y")
          )
        )
    })

    # Criando um dicionário de UFs e suas siglas
    ufs <- c(
      "Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM",
      "Bahia" = "BA", "Ceará" = "CE", "Distrito Federal" = "DF",
      "Espírito Santo" = "ES", "Goiás" = "GO", "Maranhão" = "MA",
      "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
      "Pará" = "PA", "Paraíba" = "PB", "Paraná" = "PR", "Pernambuco" = "PE",
      "Piauí" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
      "Rio Grande do Sul" = "RS", "Rondônia" = "RO", "Roraima" = "RR",
      "Santa Catarina" = "SC", "São Paulo" = "SP", "Sergipe" = "SE",
      "Tocantins" = "TO"
    )

    # Criando um dataframe com os dados do hospital escolhido
    df_hospital_filtrado <- reactive({
      nivel <- filtros()$input_nivel

      base_filtrada <- dplyr::filter(
        df_visao_geral,
        ano_mes >= filtros()$input_periodo[1],
        ano_mes <= filtros()$input_periodo[2],
        uf == filtros()$input_uf
      )

      if (nivel == "estabelecimento") {
        base_filtrada <- base_filtrada |>
          dplyr::filter(
            municipio == filtros()$input_municipio,
            nome_fantasia == filtros()$input_hospital
          )
      } else if (nivel == "macro_r_saude") {
        base_filtrada <- base_filtrada |>
          dplyr::filter(
            macro_r_saude == filtros()$input_macro_r_saude,
            nome_fantasia %in% filtros()$input_hospital
          )
      } else if (nivel == "r_saude") {
        base_filtrada <- base_filtrada |>
          dplyr::filter(
            r_saude == filtros()$input_r_saude,
            nome_fantasia %in% filtros()$input_hospital
          )
      } else if (nivel == "municipio") {
        base_filtrada <- base_filtrada |>
          dplyr::filter(
            municipio == filtros()$input_municipio,
            nome_fantasia %in% filtros()$input_hospital
          )
      } else if (nivel == "uf") {
        base_filtrada <- base_filtrada |>
          dplyr::filter(
            nome_fantasia %in% filtros()$input_hospital
          )
      }

      if (length(filtros()$input_hospital) == 1) {
        base_filtrada |>
          dplyr::arrange(ano_mes)
      } else {
        # sumariza por mês
        base_filtrada |>
          dplyr::group_by(ano_mes) |>
          dplyr::summarise(
            dplyr::across(
              c(
                leitos_obstetricos,
                leitos_obstetricos_sus,
                leitos_uti,
                leitos_uti_neonatal,
                codmunnasc,
                total_de_nascidos_vivos,
                nvm_cesarea,
                nv_prematuros,
                codmunocor,
                obitos_maternos_totais,
                casos_mmg,
                obitos_fetais_todos_todos,
                obitos_neonatais_todos_todos
              ),
              sum,
              na.rm = TRUE
            ),
            .groups = "drop"
          ) |>
          dplyr::arrange(ano_mes)
      }
    })


    # Caixas da linha principal
    output$vb_hospital <- bs4Dash::renderValueBox({
      uf_nome  <- filtros()$input_uf
      uf_sigla <- ufs[uf_nome] %||% uf_nome

      # define nome
      nome <- if (length(filtros()$input_hospital) == 1) {
        filtros()$input_hospital
      } else {
        paste(length(filtros()$input_hospital), "hospitais selecionados")
      }

      # define local conforme o nível
      nivel <- filtros()$input_nivel

      local <- dplyr::case_when(
        nivel == "uf" ~ filtros()$input_uf,
        nivel == "macro_r_saude" ~ glue::glue("{filtros()$input_macro_r_saude}, {uf_sigla}"),
        nivel == "r_saude" ~ glue::glue("{filtros()$input_r_saude}, {uf_sigla}"),
        nivel %in% c("municipio", "estabelecimento") ~ glue::glue("{filtros()$input_municipio}, {uf_sigla}"),
        TRUE ~ uf_sigla
      )

      hospitalValueBoxHospital(
        nome  = nome,
        local = local,
        icone = "hospital"
      )
    })


    output$vb_natureza <- bs4Dash::renderValueBox({
      # mapeia único valor para rótulo
      map_label <- function(x) dplyr::case_when(
        x == "publico" ~ "Público",
        x == "privado" ~ "Privado",
        x == "misto" ~ "Misto",
        x == "sem_classificacao" ~ "Sem classificação",
        TRUE ~ NA_character_
      )

      if (length(filtros()$input_natureza) == 1) {
        natureza_out <- map_label(filtros()$input_natureza)

      } else {
        # múltiplos: gráfico de barras compacto
        labs <- vapply(filtros()$input_natureza, map_label, character(1))
        df_plot <- as.data.frame(table(fator = labs), stringsAsFactors = FALSE)
        names(df_plot) <- c("categoria", "freq")
        df_plot <- df_plot[order(-df_plot$freq, df_plot$categoria), ]

        natureza_out <- highcharter::hchart(
          df_plot,
          type = "column",
          highcharter::hcaes(x = categoria, y = freq),
          name = "Natureza",
          color = "#32A0FF"
        ) |>
          highcharter::hc_size(height = 110) |>
          highcharter::hc_xAxis(
            categories = df_plot$categoria,
            visible = TRUE,
            title = list(text = NULL),
            tickWidth = 0,
            lineWidth = 0,
            labels = list(
              style = list(color = "#0A1E3C"),
              rotation = 0,
              y = 15
            )
          ) |>
          highcharter::hc_yAxis(visible = FALSE) |>
          highcharter::hc_legend(enabled = FALSE) |>
          highcharter::hc_credits(enabled = FALSE) |>
          highcharter::hc_plotOptions(
            column = list(
              borderWidth = 0,
              pointPadding = 0.05,
              groupPadding = 0.05,
              dataLabels = list(
                enabled = TRUE,
                style = list(color = "#0A1E3C"),
                format = "{point.y}",
                inside = FALSE
              )
            )
          ) |>
          highcharter::hc_chart(
            backgroundColor = "transparent",
            margin = c(0, 0, 18, 0),
            spacing = c(0, 0, 0, 0)
          ) |>
          highcharter::hc_tooltip(
            useHTML = TRUE,
            headerFormat = "{series.name}<br/>",
            pointFormat = "<span style='color:{point.color}'>&#9679;</span> <b>{point.categoria}:</b> {point.y}"
          )


      }

      hospitalValueBox(
        titulo = "Natureza",
        conteudo = natureza_out,
        icone = "coins"
      )
    })


    output$vb_categoria <- bs4Dash::renderValueBox({
      # nomes completos
      map_label_completo <- function(x) dplyr::case_when(
        x == "nv_menos_500_leitos_utin_menos_4" ~ "Menos de 500 nascidos vivos por ano e menos de 4 leitos de UTI neonatal",
        x == "nv_menos_500_leitos_utin_4_mais"  ~ "Menos de 500 nascidos vivos por ano e 4 ou mais leitos de UTI neonatal",
        x == "nv_500_a_999_leitos_utin_menos_4" ~ "De 500 a 999 nascidos vivos por ano e menos de 4 leitos de UTI neonatal",
        x == "nv_500_a_999_leitos_utin_4_mais"  ~ "De 500 a 999 nascidos vivos por ano e 4 ou mais leitos de UTI neonatal",
        x == "nv_1000_a_1999_leitos_utin_menos_4" ~ "De 1000 a 1999 nascidos vivos por ano e menos de 4 leitos de UTI neonatal",
        x == "nv_1000_a_1999_leitos_utin_4_mais"  ~ "De 1000 a 1999 nascidos vivos por ano e 4 ou mais leitos de UTI neonatal",
        x == "nv_2000_mais_leitos_utin_menos_4" ~ "2.000 ou mais nascidos vivos por ano e menos de 4 leitos de UTI neonatal",
        x == "nv_2000_mais_leitos_utin_4_mais"  ~ "2.000 ou mais nascidos vivos por ano e 4 ou mais leitos de UTI neonatal",
        TRUE ~ NA_character_
      )

      # nomes abreviados
      map_label_curto <- function(x) dplyr::case_when(
        x == "nv_menos_500_leitos_utin_menos_4" ~ "< 500 n.v. e < 4 leitos de UTIN",
        x == "nv_menos_500_leitos_utin_4_mais"  ~ "< 500 n.v. e ≥ 4 leitos de UTIN",
        x == "nv_500_a_999_leitos_utin_menos_4" ~ "500–999 n.v. e < 4 leitos de UTIN",
        x == "nv_500_a_999_leitos_utin_4_mais"  ~ "500–999 n.v. e ≥ 4 leitos de UTIN",
        x == "nv_1000_a_1999_leitos_utin_menos_4" ~ "1000–1999 n.v. e < 4 leitos de UTIN",
        x == "nv_1000_a_1999_leitos_utin_4_mais"  ~ "1000–1999 n.v. e ≥ 4 leitos de UTIN",
        x == "nv_2000_mais_leitos_utin_menos_4" ~ "≥ 2000 n.v. e < 4 leitos de UTIN",
        x == "nv_2000_mais_leitos_utin_4_mais"  ~ "≥ 2000 n.v. e ≥ 4 leitos de UTIN",
        TRUE ~ NA_character_
      )

      if (length(filtros()$input_categoria) == 1) {
        # texto completo
        categoria_out <- map_label_completo(filtros()$input_categoria)

      } else {
        # gráfico com nomes curtos
        labs <- vapply(filtros()$input_categoria, map_label_curto, character(1))
        df_plot <- as.data.frame(table(fator = labs), stringsAsFactors = FALSE)
        names(df_plot) <- c("categoria", "freq")
        df_plot <- df_plot[order(-df_plot$freq, df_plot$categoria), ]

        categoria_out <- highcharter::hchart(
          df_plot,
          type = "column",
          highcharter::hcaes(x = categoria, y = freq),
          name = "Categoria",
          color = "#32A0FF"
        ) |>
          highcharter::hc_size(height = 110) |>
          highcharter::hc_xAxis(
            categories = df_plot$categoria,
            visible = TRUE,
            title = list(text = NULL),
            tickWidth = 0,
            lineWidth = 0,
            labels = list(
              style = list(color = "#0A1E3C"),
              rotation = 0,
              y = 15
            )
          ) |>
          highcharter::hc_yAxis(visible = FALSE) |>
          highcharter::hc_legend(enabled = FALSE) |>
          highcharter::hc_credits(enabled = FALSE) |>
          highcharter::hc_plotOptions(
            column = list(
              borderWidth = 0,
              pointPadding = 0.05,
              groupPadding = 0.05,
              dataLabels = list(
                enabled = TRUE,
                style = list(color = "#0A1E3C"),
                format = "{point.y}",
                inside = FALSE
              )
            )
          ) |>
          highcharter::hc_chart(
            backgroundColor = "transparent",
            margin = c(0, 0, 18, 0),
            spacing = c(0, 0, 0, 0)
          ) |>
          highcharter::hc_tooltip(
            useHTML = TRUE,
            headerFormat = "{series.name}<br/>",
            pointFormat = "<span style='color:{point.color}'>&#9679;</span> <b>{point.categoria}:</b> {point.y}"
          )
      }

      hospitalValueBox(
        titulo = "Categoria",
        conteudo = categoria_out,
        icone = "star",
        class_fonte_conteudo = "fonte-titulos-sobre"
      )
    })



    # Caixas de "Resumo da estrutura" -----------------------------------------
    ## Leitos obstétricos -----------------------------------------------------
    output$vb_leitos_obstetricos <- bs4Dash::renderValueBox({
      df <- df_hospital_filtrado()

      valor <- tail(df$leitos_obstetricos, 1)

      hc <- highcharter::hchart(
        df,
        "line",
        highcharter::hcaes(x = ano_mes, y = leitos_obstetricos),
        name = "Leitos Obstétricos (Total)"
      ) |>
        highcharter::hc_size(height = 80) |>
        highcharter::hc_credits(enabled = FALSE) |>
        highcharter::hc_add_theme(hc_theme_sparkline_vb()) |>
        highcharter::hc_xAxis(
          title = list(text = NULL, enabled = FALSE)
        )

      valueBoxSpark(
        value = format(valor, big.mark = ".", decimal.mark = ","),
        title = "Leitos Obstétricos (Total)",
        sparkobj = hc,
        subtitle = get_subtitle_variacao(df, "leitos_obstetricos"),
        icon = icon("bed"),
        href = NULL
      )
    })

    ## Leitos obstétricos SUS -------------------------------------------------
    output$vb_leitos_obstetricos_sus <- bs4Dash::renderValueBox({
      df <- df_hospital_filtrado()

      valor <- tail(df$leitos_obstetricos_sus, 1)

      hc <- highcharter::hchart(
        df,
        "line",
        highcharter::hcaes(x = ano_mes, y = leitos_obstetricos_sus),
        name = "Leitos Obstétricos (SUS)"
      ) |>
        highcharter::hc_size(height = 80) |>
        highcharter::hc_credits(enabled = FALSE) |>
        highcharter::hc_add_theme(hc_theme_sparkline_vb()) |>
        highcharter::hc_xAxis(
          title = list(text = NULL, enabled = FALSE)
        )

      valueBoxSpark(
        value = format(valor, big.mark = ".", decimal.mark = ","),
        title = "Leitos Obstétricos (SUS)",
        sparkobj = hc,
        subtitle = get_subtitle_variacao(df, "leitos_obstetricos_sus"),
        icon = icon("bed"),
        href = NULL
      )
    })

    ## Leitos de UTI Adulto ---------------------------------------------------
    output$vb_leitos_uti_adulto <- bs4Dash::renderValueBox({
      df <- df_hospital_filtrado()

      valor <- tail(df$leitos_uti, 1)

      hc <- highcharter::hchart(
        df,
        "line",
        highcharter::hcaes(x = ano_mes, y = leitos_uti),
        name = "Leitos de UTI Adulto"
      ) |>
        highcharter::hc_size(height = 80) |>
        highcharter::hc_credits(enabled = FALSE) |>
        highcharter::hc_add_theme(hc_theme_sparkline_vb()) |>
        highcharter::hc_xAxis(
          title = list(text = NULL, enabled = FALSE)
        )

      valueBoxSpark(
        value = format(valor, big.mark = ".", decimal.mark = ","),
        title = "Leitos de UTI Adulto",
        sparkobj = hc,
        subtitle = get_subtitle_variacao(df, "leitos_uti"),
        icon = icon("procedures"),
        href = NULL
      )
    })

    ## Leitos de UTI Neonatal -------------------------------------------------
    output$vb_leitos_uti_neonatal <- bs4Dash::renderValueBox({
      df <- df_hospital_filtrado()

      valor <- tail(df$leitos_uti_neonatal, 1)

      hc <- highcharter::hchart(
        df,
        "line",
        highcharter::hcaes(x = ano_mes, y = leitos_uti_neonatal),
        name = "Leitos de UTI Neonatal"
      ) |>
        highcharter::hc_size(height = 80) |>
        highcharter::hc_credits(enabled = FALSE) |>
        highcharter::hc_add_theme(hc_theme_sparkline_vb()) |>
        highcharter::hc_xAxis(
          title = list(text = NULL, enabled = FALSE)
        )

      valueBoxSpark(
        value = format(valor, big.mark = ".", decimal.mark = ","),
        title = "Leitos de UTI Neonatal",
        sparkobj = hc,
        subtitle = get_subtitle_variacao(df, "leitos_uti_neonatal"),
        icon = icon("baby"),
        href = NULL
      )
    })


    # Caixas de "Resumo dos dados de atendimento" -----------------------------
    ## Nascidos vivos ---------------------------------------------------------
    output$vb_total_nascidos <- bs4Dash::renderValueBox({
      df <- df_hospital_filtrado()

      valor <- tail(df$total_de_nascidos_vivos, 1)

      hc <- highcharter::hchart(
        df,
        "line",
        highcharter::hcaes(x = ano_mes, y = total_de_nascidos_vivos),
        name = "Nascidos vivos"
      ) |>
        highcharter::hc_size(height = 80) |>
        highcharter::hc_credits(enabled = FALSE) |>
        highcharter::hc_add_theme(hc_theme_sparkline_vb()) |>
        highcharter::hc_xAxis(
          title = list(text = NULL, enabled = FALSE)
        )

      valueBoxSpark(
        value = format(valor, big.mark = ".", decimal.mark = ","),
        title = "Nascidos vivos",
        sparkobj = hc,
        subtitle = get_subtitle_variacao(df, "total_de_nascidos_vivos"),
        icon = icon("baby-carriage"),
        href = NULL
      )
    })

    ## Percentual de cesarianas -----------------------------------------------
    output$vb_prop_cesarianas <- bs4Dash::renderValueBox({
      df <- df_hospital_filtrado() |>
        dplyr::mutate(
          porc_cesarianas = round(nvm_cesarea / total_de_nascidos_vivos * 100, 1)
        )

      valor <- tail(df$porc_cesarianas, 1)

      hc <- highcharter::hchart(
        df,
        "line",
        highcharter::hcaes(x = ano_mes, y = porc_cesarianas),
        name = "% de cesarianas"
      ) |>
        highcharter::hc_size(height = 80) |>
        highcharter::hc_credits(enabled = FALSE) |>
        highcharter::hc_add_theme(hc_theme_sparkline_vb()) |>
        highcharter::hc_xAxis(
          title = list(text = NULL, enabled = FALSE)
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%")

      valueBoxSpark(
        value = paste0(format(valor, big.mark = ".", decimal.mark = ","), "%"),
        title = "% de cesarianas",
        sparkobj = hc,
        subtitle = get_subtitle_variacao(df, "porc_cesarianas"),
        icon = icon("person-breastfeeding"),
        href = NULL
      )
    })

    ## Percentual de partos prematuros ----------------------------------------
    output$vb_prematuros <- bs4Dash::renderValueBox({
      df <- df_hospital_filtrado() |>
        dplyr::mutate(
          porc_prematuros = round(nv_prematuros / total_de_nascidos_vivos * 100, 1)
        )

      valor <- tail(df$porc_prematuros, 1)

      hc <- highcharter::hchart(
        df,
        "line",
        highcharter::hcaes(x = ano_mes, y = porc_prematuros),
        name = "% de partos prematuros"
      ) |>
        highcharter::hc_size(height = 80) |>
        highcharter::hc_credits(enabled = FALSE) |>
        highcharter::hc_add_theme(hc_theme_sparkline_vb()) |>
        highcharter::hc_xAxis(
          title = list(text = NULL, enabled = FALSE)
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%")

      valueBoxSpark(
        value = paste0(format(valor, big.mark = ".", decimal.mark = ","), "%"),
        title = "% de partos prematuros",
        sparkobj = hc,
        subtitle = get_subtitle_variacao(df, "porc_prematuros"),
        icon = icon("baby"),
        href = NULL
      )
    })

    ## Óbitos maternos --------------------------------------------------------
    output$vb_obitos_maternos <- bs4Dash::renderValueBox({
      df <- df_hospital_filtrado()

      valor <- tail(df$obitos_maternos_totais, 1)

      hc <- highcharter::hchart(
        df,
        "line",
        highcharter::hcaes(x = ano_mes, y = obitos_maternos_totais),
        name = "Óbitos maternos"
      ) |>
        highcharter::hc_size(height = 80) |>
        highcharter::hc_credits(enabled = FALSE) |>
        highcharter::hc_add_theme(hc_theme_sparkline_vb()) |>
        highcharter::hc_xAxis(
          title = list(text = NULL, enabled = FALSE)
        )

      valueBoxSpark(
        value = format(valor, big.mark = ".", decimal.mark = ","),
        title = "Óbitos maternos",
        sparkobj = hc,
        subtitle = get_subtitle_variacao(df, "obitos_maternos_totais"),
        icon = icon("procedures"),
        href = NULL
      )
    })


    ## Casos de MMG -----------------------------------------------------------
    output$vb_casos_mmg <- bs4Dash::renderValueBox({
      df <- df_hospital_filtrado()

      valor <- tail(df$casos_mmg, 1)

      if (filtros()$input_natureza == "privado") {
        hc <- highcharter::highchart() |>
          highcharter::hc_title(
            text = "<span class='fonte-grande' style='color:#0A1E3C; font-weight: 400;'>Não disponível para hospitais privados</span>",
            useHTML = TRUE,
            align = "left",
            verticalAlign = "middle"
          ) |>
          highcharter::hc_size(height = 80) |>
          highcharter::hc_credits(enabled = FALSE) |>
          highcharter::hc_add_theme(hc_theme_sparkline_vb()) |>
          highcharter::hc_xAxis(
            title = list(text = NULL, enabled = FALSE)
          )
      } else {
        hc <- highcharter::hchart(
          df,
          "line",
          highcharter::hcaes(x = ano_mes, y = casos_mmg),
          name = "Casos de MMG"
        ) |>
          highcharter::hc_size(height = 80) |>
          highcharter::hc_credits(enabled = FALSE) |>
          highcharter::hc_add_theme(hc_theme_sparkline_vb()) |>
          highcharter::hc_xAxis(
            title = list(text = NULL, enabled = FALSE)
          )
      }

      valueBoxSpark(
        value = format(valor, big.mark = ".", decimal.mark = ","),
        title = "Casos de MMG",
        sparkobj = hc,
        subtitle = get_subtitle_variacao(df, "casos_mmg"),
        icon = icon("heartbeat"),
        href = NULL,
        natureza = filtros()$input_natureza
      )
    })

    ## Óbitos fetais ----------------------------------------------------------
    output$vb_obitos_fetais <- bs4Dash::renderValueBox({
      df <- df_hospital_filtrado()

      valor <- tail(df$obitos_fetais_todos_todos, 1)

      hc <- highcharter::hchart(
        df,
        "line",
        highcharter::hcaes(x = ano_mes, y = obitos_fetais_todos_todos),
        name = "Óbitos fetais"
      ) |>
        highcharter::hc_size(height = 80) |>
        highcharter::hc_credits(enabled = FALSE) |>
        highcharter::hc_add_theme(hc_theme_sparkline_vb()) |>
        highcharter::hc_xAxis(
          title = list(text = NULL, enabled = FALSE)
        )

      valueBoxSpark(
        value = format(valor, big.mark = ".", decimal.mark = ","),
        title = "Óbitos fetais",
        sparkobj = hc,
        subtitle = get_subtitle_variacao(df, "obitos_fetais_todos_todos"),
        icon = icon("baby"),
        href = NULL
      )
    })

    ## Óbitos neonatais -------------------------------------------------------
    output$vb_obitos_neonatais <- bs4Dash::renderValueBox({
      df <- df_hospital_filtrado()

      valor <- tail(df$obitos_neonatais_todos_todos, 1)

      hc <- highcharter::hchart(
        df,
        "line",
        highcharter::hcaes(x = ano_mes, y = obitos_neonatais_todos_todos),
        name = "Óbitos neonatais"
      ) |>
        highcharter::hc_size(height = 80) |>
        highcharter::hc_credits(enabled = FALSE) |>
        highcharter::hc_add_theme(hc_theme_sparkline_vb()) |>
        highcharter::hc_xAxis(
          title = list(text = NULL, enabled = FALSE)
        )

      valueBoxSpark(
        value = format(valor, big.mark = ".", decimal.mark = ","),
        title = "Óbitos neonatais",
        sparkobj = hc,
        subtitle = get_subtitle_variacao(df, "obitos_neonatais_todos_todos"),
        icon = icon("baby"),
        href = NULL
      )
    })
  })
}

## To be copied in the UI
# mod_visao_geral_ui("visao_geral_1")

## To be copied in the server
# mod_visao_geral_server("visao_geral_1")
