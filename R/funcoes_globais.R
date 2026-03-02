#' @exportS3Method pkg::generic
# Definindo a função que cria os acordeões na caixa de filtros
accordionSection <- function(id, title, content_ui, default_open = TRUE, disabled = FALSE) {
  # classes iniciais
  header_class <- paste(
    if (default_open) "accordion-header-container" else "accordion-header-container closed",
    if (disabled) "disabled" else NULL
  )
  icon_class   <- if (default_open) "accordion-icon" else "accordion-icon closed"
  body_style   <- if (default_open) "" else "display:none;"
  body_class   <- paste("accordion-body col-12", if (disabled) "disabled" else NULL)

  htmltools::div(
    class = "accordion-section col-12",

    # Cabeçalho
    htmltools::tags$div(
      id = paste0(id, "-header-container"),
      class = header_class,
      # # Sempre adiciona o onclick
      # onclick = sprintf("
      #   (function(){
      #     var ic = document.getElementById('%s-icon');
      #     var container = document.getElementById('%s-header-container');
      #     var isClosed = ic.classList.contains('closed');
      #     ic.classList.toggle('closed');
      #     container.classList.toggle('closed');
      #     Shiny.setInputValue('%s_click', isClosed ? 'open' : 'closed', {priority:'event'});
      #   })();
      # ", id, id, id),

      # Título + ícone
      htmltools::tags$div(
        id = paste0(id, "-header"),
        class = "accordion-header",
        htmltools::tags$span(
          id = paste0(id, "-icon"),
          class = icon_class,
          htmltools::HTML("<i class='fa-solid fa-chevron-up chev'></i>")
        ),
        htmltools::tags$b(title, class = "fonte-grande")
      ),

      # Linha
      hr(style = "margin-top: 0.5rem; margin-bottom: 1.5rem")
    ),

    # Corpo
    shiny::div(
      id = paste0(id, "-body"),
      class = body_class,
      style = body_style,
      content_ui
    )
  )
}

# helper: aceita texto OU um htmlwidget
hospitalValueBox <- function(titulo, conteudo, icone, class_fonte_conteudo = "fonte-destaque-caixas") {
  is_widget <- inherits(conteudo, "htmlwidget")

  value_content <- if (is_widget) {
    tags$div(class = "vb-conteudo-grafico", conteudo)
  } else {
    tags$div(class = glue::glue("value-content {class_fonte_conteudo}"), conteudo)
  }

  outer_class <- if (is_widget) "value-box-custom has-widget" else "value-box-custom"
  titulo_class <- if (is_widget) "value-title fonte-titulos-sobre has-widget" else "value-title fonte-media"

  bs4Dash::valueBox(
    value = tags$div(
      class = outer_class,
      # Ícone INLINE só quando NÃO há gráfico
      if (!is_widget) icon(icone, class = "icone-valuebox"),
      tags$div(
        class = "value-text",
        tags$div(class = titulo_class, titulo),
        value_content
      ),
      # Ícone FLUTUANTE só quando HÁ gráfico
      if (is_widget) tags$div(class = "icone-flutuante", icon(icone))
    ),
    subtitle = NULL,
    color = "white",
    width = NULL
  )
}




hospitalValueBoxHospital <- function(nome, local, icone) {
  bs4Dash::valueBox(
    value = tags$div(
      class = "value-box-custom",
      icon(icone, class = "icone-valuebox"),
      tags$div(
        class = "value-text",
        tags$div(class = "value-content fonte-nome-hospital", nome),
        tags$div(class = "value-content fonte-titulos-sobre", style = "font-weight: 500", local)
      )
    ),
    subtitle = NULL,
    color = "white",
    width = NULL
  )
}


valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL,
                          icon = NULL, color = "aqua", width = 4, href = NULL, natureza = "publico"){

  shinydashboard:::validateColor(color)

  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")

  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    # bs3 pull-right
    # bs4 float-right
    class = "pull-right float-right"
  )

  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title, class = "fonte-grande", style = "color: #0A1E3C;"),
      if (!is.null(sparkobj)) info_icon,
      if (natureza != "privado") {
        h3(value, class = "fonte-destaque-caixas", style = "color: #0A1E3C; font-weight: bold;")
      } else {
        h3(value, class = "fonte-destaque-caixas", style = "pointer-events: none; color: #FCFCFD; font-weight: bold;")
      },
      if (!is.null(sparkobj)) sparkobj,
      if (natureza != "privado") {
        p(subtitle, style = "margin-top: 0.5rem; margin-bottom: 0rem;", class = "fonte-media")
      } else {
        p(subtitle, style = "pointer-events: none; color: #FCFCFD; margin-top: 0.5rem; margin-bottom: 0rem;", class = "fonte-media")
      }
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-small icon", icon, style = "z-index; 0")
  )

  if (!is.null(href))
    boxContent <- a(href = href, boxContent)

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}

hc_theme_sparkline_vb <- function(...) {

  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 6,
      spacingRight = 6,
      spacingBottom = 6,
      spacingLeft = 6,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = TRUE,
      lineColor = "#0A1E3C",         # cor da linha do eixo
      tickColor = "#0A1E3C",
      tickPixelInterval = 120,
      endOnTick = FALSE,
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE,
      startOnTick = FALSE
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = "hsl(216deg 71% 14% / 100%)",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )

  theme <- structure(theme, class = "hc_theme")

  if (length(list(...)) > 0) {
    theme <- highcharter::hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }

  theme
}

# Função para criar subtitle com variação do último mês
get_subtitle_variacao <- function(df, coluna) {
  # Ordena por ano_mes
  df <- df |> dplyr::arrange(ano_mes)

  # Últimos dois meses disponíveis
  ultimos <- tail(df[[coluna]], 2)

  if (length(ultimos) < 2 || any(is.na(ultimos))) {
    var_text <- "Não é possível calcular a variação"
    var_icon <- ""
  } else if (ultimos[1] == 0) {
    # Se o mês anterior for 0
    if (ultimos[2] == 0) {
      var_icon <- HTML("&rarr;")
      var_text <- "Sem variação em relação ao mês anterior"
    } else {
      var_icon <- ""
      var_text <- "Não é possível calcular a variação"
    }
  } else {
    perc_diff <- (ultimos[2] - ultimos[1]) / ultimos[1] * 100

    if (perc_diff > 0) {
      var_icon <- HTML("&uarr;")
      var_text <- paste0(round(perc_diff, 1), "% de aumento em relação ao mês anterior")
    } else if (perc_diff < 0) {
      var_icon <- HTML("&darr;")
      var_text <- paste0(round(abs(perc_diff), 1), "% de redução em relação ao mês anterior")
    } else {
      var_icon <- HTML("&rarr;")
      var_text <- "Sem variação em relação ao mês anterior"
    }
  }

  tagList(var_icon, " ", var_text)
}


# Função para criar HTML de um chip de filtro
create_filter_chip <- function(label, value, input_id, placeholder = "Todos", icon_name = "times") {
  # Verifica se o valor está vazio
  vazio <- is.null(value) || value == "" || length(value) == 0

  # Define o texto que será exibido
  texto_exibido <- if (vazio) placeholder else value

  # Colapsa vetores (múltiplos hospitais)
  if (length(texto_exibido) > 1) texto_exibido <- paste0(texto_exibido, collapse = ", ")

  tags$div(
    # Adiciona a classe 'chip-vazio' se não houver seleção
    class = paste("filter-chip", if (vazio) "chip-vazio"),
    tags$span(
      class = "fonte-media",
      tags$b(class = "filter-chip-label", paste0(label, ":")),
      texto_exibido
    ),
    # Ícone dinâmico com base no argumento icon_name
    if (!vazio) {
      tags$span(
        class = "filter-chip-remove",
        onclick = sprintf("Shiny.setInputValue('remover_filtro_id', '%s', {priority: 'event'})", input_id),
        icon(icon_name)
      )
    }
  )
}


# Função 1: Para a Value Box (Título interno do card)
formatar_lista_variaveis <- function(string_vars) {
  if (is.na(string_vars) || string_vars == "") return("")

  # Regex: Divide por vírgula OU pela palavra " e " (com espaços em volta)
  vars <- stringr::str_split(string_vars, ",\\s*|\\s+e\\s+")[[1]] |>
    stringr::str_trim() |>
    Filter(f = function(x) x != "") # Remove entradas vazias

  n <- length(vars)

  if (n == 1) {
    return(paste0("Variável ", string_vars))
  } else {
    # Reconstrói a lista padronizada: X, Y e Z
    texto <- paste(vars[1:(n-1)], collapse = ", ")
    texto <- paste0(texto, " e ", string_vars[n])
    return(paste0("Variáveis ", string_vars))
  }
}

# Função 2: Para o Título do Gráfico no Modal
gerar_titulo_modal <- function(string_vars) {
  if (is.na(string_vars) || string_vars == "") return("Completude da Informação")

  vars <- stringr::str_split(string_vars, ",\\s*|\\s+e\\s+")[[1]] |>
    stringr::str_trim() |>
    Filter(f = function(x) x != "")

  n <- length(vars)

  # Reconstrói a lista padronizada para o negrito
  lista_formatada <- if (n == 1) {
    vars
  } else {
    paste0(paste(vars[1:(n-1)], collapse = ", "), " e ", vars[n])
  }

  # Ajuste fino de concordância
  prefixo <- if (n == 1) "com a variável" else "com as variáveis"
  sufixo  <- if (n == 1) "adequadamente preenchida" else "adequadamente preenchidas"

  return(paste0("Porcentagem de declarações de nascidos vivos ", prefixo, " <b>", lista_formatada, "</b> ", sufixo))
}

# Função 3: Para a Nota de Rodapé (Caption)
formatar_vars_caption <- function(string_vars) {
  if (is.na(string_vars) || string_vars == "") return("variáveis")

  vars <- stringr::str_split(string_vars, ",\\s*|\\s+e\\s+")[[1]] |>
    stringr::str_trim() |>
    Filter(f = function(x) x != "")

  n <- length(vars)

  # Mantemos "registros de" para ambos, pois concorda com o plural oculto ou explícito
  prefixo <- ""

  lista <- if (n == 1) {
    vars
  } else {
    paste0(paste(vars[1:(n-1)], collapse = ", "), " e ", vars[n])
  }

  return(paste0(prefixo, lista))
}


# Limpa o nome do denominador para a mensagem (ex: "Total de nascidos vivos" -> "nascidos vivos")
limpar_nome_denominador <- function(nome) {
  if (is.na(nome) || nome == "") return("registros")
  nome_limpo <- stringr::str_to_lower(nome)
  nome_limpo <- stringr::str_remove(nome_limpo, "total de ")
  return(nome_limpo)
}
