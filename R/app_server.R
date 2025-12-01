#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  base::Sys.setlocale("LC_TIME", "pt_BR.UTF-8")
  options(scipen = 999)

  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$decimalPoint <- ","
  hcoptslang$thosandsSep <- "."
  options(highcharter.lang = hcoptslang)


  # Módulos principais ------------------------------------------------------
  mod_visao_geral_server("visao_geral_1", filtros = filtros)
  mod_indicadores_server("indicadores_1", filtros = filtros)

  # Outputs do título -------------------------------------------------------
  #output$titulo_visao_geral <- output$titulo_perfil_sociodemografico <- renderUI(glue::glue("({input$input_hospital})"))
  output$titulo_indicadores <- renderUI(glue::glue("({tabela_indicadores$indicador_principal[which(tabela_indicadores$nome_abreviado_indicador == input$input_indicador)][1]})"))


  # Controle das sanfonas ---------------------------------------------------
  shiny::observeEvent(input$localizacao_click, {
    if (identical(input$localizacao_click, "closed")) {
      shinyjs::hide("localizacao-body", anim = TRUE, animType = "slide", time = 0.35)
    } else {
      shinyjs::show("localizacao-body", anim = TRUE, animType = "slide", time = 0.35)
    }
  }, ignoreNULL = TRUE)

  shiny::observeEvent(input$indicador_click, {
    if (identical(input$indicador_click, "closed")) {
      shinyjs::hide("indicador-body", anim = TRUE, animType = "slide", time = 0.35)
    } else {
      shinyjs::show("indicador-body", anim = TRUE, animType = "slide", time = 0.35)
      shinyjs::runjs("
        var card = $('#card-filters .card-body');
        card.stop().animate({ scrollTop: card.prop('scrollHeight') }, 400);
      ")
    }
  }, ignoreNULL = TRUE)


  # Atualizando as opções dos inputs ----------------------------------------
  # --- Atualiza inputs conforme UF ---
  observeEvent(input$input_uf, {
    updateSelectizeInput(
      session, "input_macro_r_saude",
      choices = sort(unique(df_cnes_aux$macro_r_saude[df_cnes_aux$uf == input$input_uf])),
      server = FALSE
    )
    updateSelectizeInput(
      session, "input_r_saude",
      choices = sort(unique(df_cnes_aux$r_saude[df_cnes_aux$uf == input$input_uf])),
      server = FALSE
    )
    updateSelectizeInput(
      session, "input_municipio",
      choices = sort(unique(df_cnes_aux$municipio[df_cnes_aux$uf == input$input_uf])),
      server = FALSE
    )
  })

  # --- Atualiza inputs conforme macro_r_saude ---
  observeEvent(input$input_macro_r_saude, {
    req(input$nivel)
    if (input$nivel == "macro_r_saude") {
      updateSelectizeInput(
        session, "input_municipio",
        choices = sort(unique(df_cnes_aux$municipio[
          df_cnes_aux$uf == input$input_uf &
            df_cnes_aux$macro_r_saude == input$macro_r_saude
        ])),
        server = FALSE
      )
    }
  })

  # --- Atualiza inputs conforme r_saude ---
  observeEvent(input$input_r_saude, {
    req(input$nivel)
    if (input$nivel == "macro_r_saude") {
      updateSelectizeInput(
        session, "input_municipio",
        choices = sort(unique(df_cnes_aux$municipio[
          df_cnes_aux$uf == input$input_uf &
            df_cnes_aux$r_saude == input$input_r_saude
        ])),
        server = FALSE
      )
    }
  })

  # flags
  from_navbar <- reactiveVal(FALSE)
  indicador_atual_nav <- reactiveVal(NULL)
  awaiting_nav_categoria <- reactiveVal(FALSE)

  # quando a navbar muda indicador/categoria
  observeEvent(input$navmenu, {
    indicador_selecionado <- sub(".*-(.*)$", "\\1", input$navmenu)

    # só reage se o indicador for diferente do atual
    if (!is.null(indicador_selecionado) &&
        indicador_selecionado %in% tabela_indicadores$nome_abreviado_indicador &&
        indicador_selecionado != input$input_indicador) {

      indicador_atual_nav(indicador_selecionado)
      awaiting_nav_categoria(TRUE)

      bloco_selecionado <- unique(tabela_indicadores$bloco[which(tabela_indicadores$nome_abreviado_indicador == indicador_selecionado)])
      indicadores_bloco_selecionado <- get(paste0("indicadores_", bloco_selecionado))

      updateSelectInput(session, "input_bloco", selected = bloco_selecionado)

      updateSelectInput(
        session,
        "input_indicador",
        choices = setNames(
          indicadores_bloco_selecionado$nome_abreviado_indicador,
          indicadores_bloco_selecionado$indicador_principal
        ),
        selected = indicador_selecionado
      )

      updateSelectizeInput(
        session,
        "input_categoria",
        label = tabela_indicadores$label_input_categoria[
          tabela_indicadores$nome_abreviado_indicador == indicador_selecionado
        ][1],
        choices = setNames(
          tabela_indicadores$sub_indicador[
            tabela_indicadores$nome_abreviado_indicador == indicador_selecionado
          ],
          tabela_indicadores$categoria_sub_indicador[
            tabela_indicadores$nome_abreviado_indicador == indicador_selecionado
          ]
        ),
        server = FALSE
      )
    }
  }, priority = 1000)

  # só dispara quando o input_categoria muda após uma atualização da navbar
  observeEvent(input$input_categoria, {
    if (isTRUE(awaiting_nav_categoria()) &&
        identical(input$input_indicador, indicador_atual_nav())) {
      from_navbar(TRUE)
    }

    awaiting_nav_categoria(FALSE)
    indicador_atual_nav(NULL)
  }, ignoreInit = TRUE, priority = 999)


  # --- atualiza bloco e indicadores só quando NÃO vem da navbar ---
  observeEvent(input$input_bloco, {
    if (isTRUE(from_navbar())) return(NULL)

    updateSelectInput(session, "input_bloco", selected = input$input_bloco)

    indicadores_bloco_selecionado <- get(paste0("indicadores_", input$input_bloco))

    updateSelectInput(
      session,
      "input_indicador",
      choices = setNames(
        indicadores_bloco_selecionado$nome_abreviado_indicador,
        indicadores_bloco_selecionado$indicador_principal
      )
    )
  }, priority = 998)

  observeEvent(input$input_indicador, {
    if (isTRUE(from_navbar())) return(NULL)

    updateSelectizeInput(
      session,
      "input_categoria",
      label = tabela_indicadores$label_input_categoria[
        tabela_indicadores$nome_abreviado_indicador == input$input_indicador
      ][1],
      choices = setNames(
        tabela_indicadores$sub_indicador[
          tabela_indicadores$nome_abreviado_indicador == input$input_indicador
        ],
        tabela_indicadores$categoria_sub_indicador[
          tabela_indicadores$nome_abreviado_indicador == input$input_indicador
        ]
      ),
      server = FALSE
    )
  })

  observe({
    if (input$input_municipio == "Angra dos Reis" & input$input_nivel == "estabelecimento") {

      shinyWidgets::updateVirtualSelect(
        inputId = "input_hospital_unico",
        choices = list(
          `Públicos` = "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR",
          `Mistos`   = NULL,
          `Privados` = "HOSPITAL UNIMED VOLTA REDONDA UNIDADE LITORAL SUL",
          `Sem classificação` = NULL
        ),
        selected = "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR"
      )
    } else {

      if (input$input_municipio == "Angra dos Reis" & input$input_nivel == "municipio") {
        shinyWidgets::updateVirtualSelect(
          inputId = "input_hospital_multiplo",
          choices = list(
            `Públicos` = "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR",
            `Mistos`   = NULL,
            `Privados` = "HOSPITAL UNIMED VOLTA REDONDA UNIDADE LITORAL SUL",
            `Sem classificação` = NULL
          ),
          selected = c("HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR", "HOSPITAL UNIMED VOLTA REDONDA UNIDADE LITORAL SUL")
        )
      } else {
        if (input$input_nivel == "estabelecimento") {
          publico <- sort(unique(df_cnes_aux$nome_fantasia[
            df_cnes_aux$uf == input$input_uf &
              df_cnes_aux$municipio == input$input_municipio &
              df_cnes_aux$tipo == "publico"
          ]))
          misto <- sort(unique(df_cnes_aux$nome_fantasia[
            df_cnes_aux$uf == input$input_uf &
              df_cnes_aux$municipio == input$input_municipio &
              df_cnes_aux$tipo == "misto"
          ]))
          privado <- sort(unique(df_cnes_aux$nome_fantasia[
            df_cnes_aux$uf == input$input_uf &
              df_cnes_aux$municipio == input$input_municipio &
              df_cnes_aux$tipo == "privado"
          ]))
          sem_classificacao <- sort(unique(df_cnes_aux$nome_fantasia[
            df_cnes_aux$uf == input$input_uf &
              df_cnes_aux$municipio == input$input_municipio &
              df_cnes_aux$tipo == "sem_classificacao"
          ]))
        } else {
          # colunas de referência por nível
          nivel_col <- switch(
            input$input_nivel,
            "uf" = c("uf"),
            "macro_r_saude" = c("uf", "macro_r_saude"),
            "r_saude" = c("uf", "r_saude"),
            "municipio" = c("uf", "municipio")
          )

          # constrói expressão lógica
          expr <- rlang::expr(TRUE)
          for (col in nivel_col) {
            valor <- input[[paste0("input_", col)]]
            expr <- rlang::expr(!!expr & !!rlang::sym(col) == !!valor)
          }

          # aplica filtro
          df_filtrado <- df_cnes_aux |> dplyr::filter(!!expr, ano_mes == "2023-12-01")

          publico <- sort(unique(df_filtrado$nome_fantasia[df_filtrado$tipo == "publico"]))
          misto <- sort(unique(df_filtrado$nome_fantasia[df_filtrado$tipo == "misto"]))
          privado <- sort(unique(df_filtrado$nome_fantasia[df_filtrado$tipo == "privado"]))
          sem_classificacao <- sort(unique(df_filtrado$nome_fantasia[df_filtrado$tipo == "sem_classificacao"]))
        }

        # monta lista
        choices_list <- list(
          `Públicos` = publico,
          `Mistos`   = misto,
          `Privados` = privado,
          `Sem classificação` = sem_classificacao
        )

        # define seleção inicial na ordem de prioridade
        selected <- NULL
        if (length(publico) > 0) {
          selected <- publico
        }

        if (length(misto) > 0) {
          selected <- c(selected, misto)
        }

        if (length(privado) > 0) {
          selected <- c(selected, privado)
        }

        if (length(sem_classificacao) > 0) {
          selected <- c(selected, sem_classificacao)
        }

        shinyWidgets::updateVirtualSelect(
          inputId = "input_hospital_unico",
          choices = choices_list,
          selected = selected[1]
        )

        shinyWidgets::updateVirtualSelect(
          inputId = "input_hospital_multiplo",
          choices = choices_list,
          selected = selected
        )
      }
    }
  })


  # Adicionando animações a "conditional panels" ----------------------------
  observeEvent(input$abas, {
    if (input$abas == "visao_geral") {
      shinyjs::show("div_titulo_visao_geral", anim = TRUE, animType = "fade", time = 0.3)
    } else {
      shinyjs::hide("div_titulo_visao_geral", anim = TRUE, animType = "fade", time = 0.3)
    }
  })

  observeEvent(input$abas, {
    if (input$abas == "indicadores") {
      shinyjs::show("div_titulo_indicadores", anim = TRUE, animType = "fade", time = 0.3)
    } else {
      shinyjs::hide("div_titulo_indicadores", anim = TRUE, animType = "fade", time = 0.3)
    }
  })

  observeEvent(input$abas, {
    if (startsWith(input$abas, "indicadores")) {
      shinyjs::show("conditional_indicador", anim = TRUE, animType = "slide", time = 0.4)
      shinyjs::runjs("
      var card = $('#card-filters .card-body');
      card.stop().animate({ scrollTop: card.prop('scrollHeight') }, 400);
    ")
    } else {
      shinyjs::hide("conditional_indicador", anim = TRUE, animType = "slide", time = 0.4)
    }
  })

  observeEvent(input$input_indicador, {
    if (nrow(tabela_indicadores[which(tabela_indicadores$nome_abreviado_indicador == input$input_indicador), ]) > 1) {
      shinyjs::show("conditional_desejo_visualizar", anim = TRUE, animType = "slide", time = 0.3)

      if (input$input_desejo_visualizar == "individual") {
        shinyjs::show("conditional_categoria", anim = TRUE, animType = "slide", time = 0.3)
      } else {
        shinyjs::hide("conditional_categoria", anim = TRUE, animType = "slide", time = 0.3)
      }
    } else {
      shinyjs::hide("conditional_categoria", anim = FALSE)
      shinyjs::hide("conditional_desejo_visualizar", anim = TRUE, animType = "slide", time = 0.3)
    }
  }, priority = 2000)

  observeEvent(input$input_nivel, {
    if (input$input_nivel == "estabelecimento") {
      shinyjs::hide("conditional_hospital_multiplo", anim = TRUE, animType = "slide", time = 0.3)
      shinyjs::show("conditional_hospital_unico", anim = TRUE, animType = "slide", time = 0.3)
    } else {
      shinyjs::hide("conditional_hospital_unico", anim = TRUE, animType = "slide", time = 0.3)
      shinyjs::show("conditional_hospital_multiplo", anim = TRUE, animType = "slide", time = 0.3)
    }

    if (input$input_nivel %in% c("estabelecimento", "municipio")) {
      shinyjs::show("conditional_municipio", anim = TRUE, animType = "slide", time = 0.3)
    } else {
      shinyjs::hide("conditional_municipio", anim = TRUE, animType = "slide", time = 0.3)
    }

    if (input$input_nivel == "macro_r_saude") {
      shinyjs::show("conditional_macro_r_saude", anim = TRUE, animType = "slide", time = 0.3)
    } else {
      shinyjs::hide("conditional_macro_r_saude", anim = TRUE, animType = "slide", time = 0.3)
    }

    if (input$input_nivel == "r_saude") {
      shinyjs::show("conditional_r_saude", anim = TRUE, animType = "slide", time = 0.3)
    } else {
      shinyjs::hide("conditional_r_saude", anim = TRUE, animType = "slide", time = 0.3)
    }
  })


  observeEvent(input$abas, {
    shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'});")
  })


  # Garantindo que apenas valores válidos sejam transmitidos ---------------------
  ## Criando uma lista com os nomes dos inputs a serem validados
  campos <- c(
    "input_nivel",
    "input_uf",
    "input_macro_r_saude",
    "input_r_saude",
    "input_municipio",
    "input_hospital_unico",
    "input_hospital_multiplo",
    "input_periodo",
    "input_bloco",
    "input_indicador",
    "input_categoria"
  )

  ## Criando uma lista de reactiveVals nomeados dinamicamente
  valid_inputs <- setNames(purrr::map(campos, ~ reactiveVal()), campos)

  ## Criando os observers para atualizar os reactiveVals quando o input for válido
  purrr::imap(valid_inputs, function(rv, nome) {
    observe({
      valor <- input[[nome]]

      if (isTruthy(valor)) rv(valor)
    })
  })

  observeEvent(input$btn_redefinir, {

    # Valores default
    default_nivel <- "estabelecimento"
    default_uf <- "Rio de Janeiro"
    #default_r_saude   <- "Alto Acre"  # descomentaria se tiver
    default_municipio <- "Angra dos Reis"
    default_hospital  <- "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR"
    default_periodo   <- c("2023-01-01", "2023-12-01")

    # Atualiza inputs visuais
    updateSelectizeInput(session, "input_nivel", selected = default_nivel)
    updateSelectizeInput(session, "input_uf", selected = default_uf)
    #updateSelectizeInput(session, "input_r_saude", selected = default_r_saude)
    updateSelectizeInput(session, "input_municipio", selected = default_municipio)
    shinyWidgets::updateVirtualSelect("input_hospital_unico", selected = default_hospital, session = session)
    shinyWidgets::updateAirDateInput(session, "input_periodo", value = default_periodo)

    # Atualiza os reactiveVals
    valid_inputs$input_uf(default_uf)
    #valid_inputs$input_r_saude(default_r_saude)
    valid_inputs$input_municipio(default_municipio)
    valid_inputs$input_hospital_unico(default_hospital)
    valid_inputs$input_periodo(default_periodo)
  })

  # Inputs que disparam o aviso
  inputs_relevantes <- reactive({
    list(
      input$input_hospital_unico,
      input$input_hospital_multiplo,
      input$input_uf,
      input$input_municipio,
      input$input_periodo,
      input$input_bloco,
      input$input_indicador,
      input$input_categoria
    )
  })

  # Contador de execuções
  contador_execucao <- reactiveVal(0)

  observeEvent(inputs_relevantes(), {
    contador_execucao(contador_execucao() + 1)
    # só mostra o aviso a partir da segunda execução (ou maior)
    if (!isTRUE(from_navbar()) && contador_execucao() >= 3) {
      shinyjs::runjs("document.getElementById('aviso_exclamacao').style.opacity = 1;")
    }
  }, ignoreInit = TRUE)



  # Esconder exclamação ao atualizar
  observeEvent(input$btn_atualizar, {
    shinyjs::runjs("document.getElementById('aviso_exclamacao').style.opacity = 0;")
  })


  # Transmitindo os inputs --------------------------------------------------
  trigger_filtros <- reactiveVal(0)

  observeEvent(input$btn_atualizar, {
    # aguarda atualização dos inputs antes de recalcular filtros
    shinyjs::delay(100, trigger_filtros(isolate(trigger_filtros()) + 1))
  })

  filtros <- eventReactive(
    list(trigger_filtros(), from_navbar()),
    {
      from_navbar(FALSE)

      # garante que período tenha 2 valores válidos
      periodo_valido <- valid_inputs$input_periodo()
      if (is.null(periodo_valido) || length(periodo_valido) < 2) {
        periodo_valido <- c("2023-01-01", "2023-12-01")
      }

      # define o nível atual (ou padrão)
      nivel_atual <- valid_inputs$input_nivel() %||% "estabelecimento"

      # define a UF atual
      uf_atual <- valid_inputs$input_uf() %||% "Rio de Janeiro"

      # define município atual
      municipio_atual <- valid_inputs$input_municipio() %||% "Angra dos Reis"

      # define hospital atual
      hospital_unico <- valid_inputs$input_hospital_unico() %||% "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR"
      hospital_multiplo <- valid_inputs$input_hospital_multiplo() %||% "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR"

      # natureza (segura contra NULL e vetores vazios)
      if (!is.null(nivel_atual) && nivel_atual == "estabelecimento") {
        out_nat <- tail(df_cnes_aux$tipo[
          df_cnes_aux$uf == uf_atual &
            df_cnes_aux$municipio == municipio_atual &
            df_cnes_aux$nome_fantasia == hospital_unico
        ], 1)
        input_natureza <- if (length(out_nat) == 0 || is.na(out_nat)) "publico" else out_nat
      } else {
        if (is.null(periodo_valido) || length(periodo_valido) < 2) {
          input_natureza <- "publico"
        } else {
          out_nat <- df_cnes_aux |>
            dplyr::filter(
              uf == uf_atual &
                ano_mes >= periodo_valido[1] &
                ano_mes <= periodo_valido[2] &
                nome_fantasia %in% hospital_multiplo
            ) |>
            dplyr::group_by(cnes, codufmun) |>
            dplyr::summarise(tipo = dplyr::last(tipo), .groups = "drop") |>
            dplyr::pull(tipo)
          input_natureza <- if (length(out_nat) == 0 || all(is.na(out_nat))) "publico" else out_nat
        }
      }

      # categoria (segura contra NULL e vetores vazios)
      if (!is.null(nivel_atual) && nivel_atual == "estabelecimento") {
        out_cat <- tail(df_cnes_aux$categoria_porte[
          df_cnes_aux$uf == uf_atual &
            df_cnes_aux$municipio == municipio_atual &
            df_cnes_aux$nome_fantasia == hospital_unico
        ], 1)
        input_categoria <- if (length(out_cat) == 0 || is.na(out_cat)) "nv_1000_mais_leitos_utin_4_mais" else out_cat
      } else {
        if (is.null(periodo_valido) || length(periodo_valido) < 2) {
          input_categoria <- "nv_1000_mais_leitos_utin_4_mais"
        } else {
          out_cat <- df_cnes_aux |>
            dplyr::filter(
              uf == uf_atual &
                ano_mes >= periodo_valido[1] &
                ano_mes <= periodo_valido[2] &
                nome_fantasia %in% hospital_multiplo
            ) |>
            dplyr::group_by(cnes, codufmun) |>
            dplyr::summarise(categoria_porte = dplyr::last(categoria_porte), .groups = "drop") |>
            dplyr::pull(categoria_porte)
          input_categoria <- if (length(out_cat) == 0 || all(is.na(out_cat))) "nv_1000_mais_leitos_utin_4_mais" else out_cat
        }
      }

      # constrói lista final
      list(
        input_nivel = nivel_atual,
        input_regiao_pais = unique(df_cnes_aux$regiao[df_cnes_aux$uf == uf_atual])[1] %||% "Sudeste",
        input_uf = uf_atual,
        input_macro_r_saude = valid_inputs$input_macro_r_saude() %||% "MACRORREGIAO I",
        input_r_saude = valid_inputs$input_r_saude() %||% "Baia Da Ilha Grande",
        input_municipio = municipio_atual,
        input_hospital = if (!is.null(nivel_atual) && nivel_atual == "estabelecimento") hospital_unico else hospital_multiplo,
        input_natureza = input_natureza,
        input_categoria = input_categoria,
        input_periodo = periodo_valido,
        input_bloco = input$input_bloco %||% "perfil_sociodemografico",
        input_indicador = input$input_categoria %||% "Porcentagem de nascidos vivos de mães com idade de 10 a 14 anos"
      )
    },
    ignoreNULL = FALSE
  )
}
