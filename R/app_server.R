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
  mod_visao_geral_server("visao_geral_1", filtros = filtros_visao_geral)
  mod_indicadores_server("indicadores_1", filtros = filtros_indicadores)

  # Outputs do título -------------------------------------------------------
  output$titulo_visao_geral <- renderUI(glue::glue("({input$input_hospital})"))
  output$titulo_indicadores <- renderUI(glue::glue("{tabela_indicadores$indicador_principal[which(tabela_indicadores$nome_abreviado_indicador == filtros_indicadores()$input_indicador)][1]}"))


  # # Controle das sanfonas ---------------------------------------------------
  # shiny::observeEvent(input$localizacao_click, {
  #   if (identical(input$localizacao_click, "closed")) {
  #     shinyjs::hide("localizacao-body", anim = TRUE, animType = "slide", time = 0.35)
  #   } else {
  #     shinyjs::show("localizacao-body", anim = TRUE, animType = "slide", time = 0.35)
  #   }
  # }, ignoreNULL = TRUE)
  #
  # shiny::observeEvent(input$indicador_click, {
  #   if (identical(input$indicador_click, "closed")) {
  #     shinyjs::hide("indicador-body", anim = TRUE, animType = "slide", time = 0.35)
  #   } else {
  #     shinyjs::show("indicador-body", anim = TRUE, animType = "slide", time = 0.35)
  #     shinyjs::runjs("
  #       var card = $('#card-filters .card-body');
  #       card.stop().animate({ scrollTop: card.prop('scrollHeight') }, 350);
  #     ")
  #   }
  # }, ignoreNULL = TRUE)


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

      sub_bloco_selecionado <- unique(tabela_indicadores$sub_bloco[which(tabela_indicadores$nome_abreviado_indicador == indicador_selecionado)])
      indicadores_bloco_selecionado <- get(paste0("indicadores_", bloco_selecionado))

      updateSelectInput(session, "input_bloco", selected = bloco_selecionado)

      updateSelectInput(
        session,
        "input_sub_bloco",
        choices = setNames(
          unique(indicadores_bloco_selecionado$sub_bloco),
          unique(indicadores_bloco_selecionado$sub_bloco_por_extenso)
        ),
        selected = sub_bloco_selecionado
      )

      updateSelectInput(
        session,
        "input_indicador",
        choices = setNames(
          indicadores_bloco_selecionado$nome_abreviado_indicador[which(indicadores_bloco_selecionado$sub_bloco == sub_bloco_selecionado)],
          indicadores_bloco_selecionado$indicador_principal[which(indicadores_bloco_selecionado$sub_bloco == sub_bloco_selecionado)]
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


  # --- atualiza bloco e indicadores só quando NÃO vem da navbar ou Modal ---
  observeEvent(input$input_bloco, {

    # AQUI ESTÁ A PROTEÇÃO NOVA:
    if (isTRUE(from_navbar()) || isTRUE(awaiting_nav_categoria())) return(NULL)

    updateSelectInput(session, "input_bloco", selected = input$input_bloco)

    if (!isTruthy(input$input_bloco)) {
      updateSelectInput(session, "input_sub_bloco", choices = NULL)
      return()
    }

    sub_blocos_bloco_selecionado <- get(paste0("indicadores_", input$input_bloco)) |>
      dplyr::select(sub_bloco, sub_bloco_por_extenso) |>
      unique()

    updateSelectInput(
      session, "input_sub_bloco",
      choices = setNames(
        sub_blocos_bloco_selecionado$sub_bloco,
        sub_blocos_bloco_selecionado$sub_bloco_por_extenso
      )
    )

  }, ignoreInit = TRUE, priority = 998)

  observeEvent(input$input_sub_bloco, {

    # AQUI ESTÁ A PROTEÇÃO NOVA:
    if (isTRUE(from_navbar()) || isTRUE(awaiting_nav_categoria())) return(NULL)

    if (!isTruthy(input$input_sub_bloco)) {
      updateSelectInput(session, "input_indicador", choices = NULL)
      return()
    }

    indicadores_bloco_selecionado <- get(paste0("indicadores_", input$input_bloco)) |>
      dplyr::filter(sub_bloco == input$input_sub_bloco)

    updateSelectInput(
      session, "input_indicador",
      choices = setNames(
        indicadores_bloco_selecionado$nome_abreviado_indicador,
        indicadores_bloco_selecionado$indicador_principal
      )
    )
  }, ignoreInit = TRUE, priority = 997)

  observeEvent(input$input_indicador, {

    # AQUI ESTÁ A PROTEÇÃO NOVA:
    if (isTRUE(from_navbar()) || isTRUE(awaiting_nav_categoria())) return(NULL)

    if (!isTruthy(input$input_indicador)) {
      updateSelectInput(session, "input_categoria", choices = NULL)
      return()
    }

    updateSelectizeInput(
      session, "input_categoria",
      label = tabela_indicadores$label_input_categoria[tabela_indicadores$nome_abreviado_indicador == input$input_indicador][1],
      choices = setNames(
        tabela_indicadores$sub_indicador[tabela_indicadores$nome_abreviado_indicador == input$input_indicador],
        tabela_indicadores$categoria_sub_indicador[tabela_indicadores$nome_abreviado_indicador == input$input_indicador]
      ),
      server = FALSE
    )
  }, ignoreInit = TRUE, priority = 996)

  observe({
    if (input$input_municipio == "Angra dos Reis" & input$input_nivel == "estabelecimento") {

      shinyWidgets::updateVirtualSelect(
        inputId = "input_hospital_unico",
        choices = list(
          `Públicos` = "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR",
          `Mistos`   = NULL,
          `Privados` = "HOSPITAL UNIMED VOLTA REDONDA UNIDADE LITORAL SUL"
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
            `Privados` = "HOSPITAL UNIMED VOLTA REDONDA UNIDADE LITORAL SUL"
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
        }

        # monta lista
        choices_list <- list(
          `Públicos` = publico,
          `Mistos`   = misto,
          `Privados` = privado
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


  ## Para o input_periodo
  observeEvent(input$input_tipo_periodo, {
    if (input$input_tipo_periodo == "12_meses") {
      shinyWidgets::updateAirDateInput(session, "input_periodo", value = c("2023-01-01", "2023-12-01"))
    } else if (input$input_tipo_periodo == "todo_periodo") {
      shinyWidgets::updateAirDateInput(session, "input_periodo", value = c("2018-01-01", "2023-12-01"))
    }
  })


  # Adicionando animações a "conditional panels" ----------------------------
  observeEvent(input$abas, {
    if (input$abas == "visao_geral") {
      shinyjs::show("div_titulo_visao_geral", anim = TRUE, animType = "fade", time = 0.35)
    } else {
      shinyjs::hide("div_titulo_visao_geral", anim = TRUE, animType = "fade", time = 0.35)
    }
  })

  observeEvent(input$input_tipo_periodo, {
    if (input$input_tipo_periodo == "outro") {

      shinyjs::runjs("
        var card = $('#card-filters .card-body');
        var el = $('#conditional_periodo');

        if (card.length && el.length) {
          el.removeClass('shinyjs-hide');

          // 1. Abre invisível para medir o tamanho real
          el.css({visibility: 'hidden', display: 'block'});

          // 2. Compara a base do elemento com a base visível do card
          var elBottom = el.offset().top + el.outerHeight();
          var cardBottom = card.offset().top + card.innerHeight();
          var targetTop = card.scrollTop();

          // 3. Se a div vazar para fora do card, calcula apenas o necessário para revelá-la + 20px de respiro
          if (elBottom > cardBottom) {
            targetTop = card.scrollTop() + (elBottom - cardBottom) + 20;
          }

          // 4. Esconde novamente e dispara as animações
          el.css({visibility: 'visible', display: 'none'});

          el.fadeIn(350);
          card.stop().animate({ scrollTop: targetTop }, 350);
        }
      ")

    } else {
      shinyjs::runjs("$('#conditional_periodo').fadeOut(350);")
    }
  })

  observeEvent(input$abas, {
    if (input$abas == "indicadores") {
      shinyjs::show("div_titulo_indicadores", anim = TRUE, animType = "fade", time = 0.35)
    } else {
      shinyjs::hide("div_titulo_indicadores", anim = TRUE, animType = "fade", time = 0.35)
    }
  })

  observeEvent(input$abas, {
    if (startsWith(input$abas, "indicadores")) {

      shinyjs::runjs("
        var card = $('#card-filters .card-body');
        var el = $('#conditional_indicador');

        if (card.length && el.length) {
          el.removeClass('shinyjs-hide');

          // Calcula altura real para ver se o conteúdo vai vazar
          el.css({visibility: 'hidden', display: 'block'});
          var elBottom = el.offset().top + el.outerHeight();
          var cardBottom = card.offset().top + card.innerHeight();
          var targetTop = card.scrollTop();

          if (elBottom > cardBottom) {
            targetTop = card.scrollTop() + (elBottom - cardBottom) + 20;
          }

          // Prepara e dispara animação de SLIDE e Scroll juntos
          el.css({visibility: 'visible', display: 'none'});
          el.slideDown(350);
          card.stop().animate({ scrollTop: targetTop }, 350);
        }
      ")

    } else {
      shinyjs::runjs("$('#conditional_indicador').slideUp(350);")
    }
  })

  observeEvent(input$input_bloco, {

    blocos_com_sub_bloco <- tabela_indicadores |>
      dplyr::select(bloco, sub_bloco) |>
      unique() |>
      dplyr::group_by(bloco) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::filter(n > 1) |>
      dplyr::pull(bloco)

    if (input$input_bloco %in% c("perfil_sociodemografico", "indicadores_assistenciais", "morbimortalidade_materna", "morbimortalidade_perinatal")) {

      shinyjs::runjs("
        var card = $('#card-filters .card-body');
        var subBloco = $('#conditional_sub_bloco');

        // Captura todos os containers na ordem da cascata
        var indicadorContainer = $('#exclamacao_indicador').parent();
        var desejoVisualizar = $('#conditional_desejo_visualizar');
        var categoria = $('#conditional_categoria');

        if (card.length && subBloco.length && indicadorContainer.length) {
          if (!subBloco.is(':visible')) {
            subBloco.removeClass('shinyjs-hide');

            // Simula a abertura do sub-bloco
            subBloco.css({visibility: 'hidden', display: 'flex'});

            // 1. Define o alvo base como o Indicador
            var targetElement = indicadorContainer;

            // 2. Se 'Desejo Visualizar' estiver aberto e visível, ele vira o alvo
            if (desejoVisualizar.is(':visible') && desejoVisualizar.outerHeight() > 0) {
              targetElement = desejoVisualizar;
            }

            // 3. Se 'Categoria' estiver aberta e visível, ELA vira o alvo final
            if (categoria.is(':visible') && categoria.outerHeight() > 0) {
              targetElement = categoria;
            }

            // O SEGREDO: Medimos a base do elemento alvo que definimos acima!
            var elBottom = targetElement.offset().top + targetElement.outerHeight();
            var cardBottom = card.offset().top + card.innerHeight();
            var targetTop = card.scrollTop();

            // Calcula o scroll necessário
            if (elBottom > cardBottom) {
              targetTop = card.scrollTop() + (elBottom - cardBottom) + 20;
            }

            // Desfaz a simulação e prepara a animação real
            subBloco.css({visibility: 'visible', display: 'none'});

            subBloco.slideDown(350, function() {
              $(this).css('display', 'flex'); // Salva o flex no fim da animação
            });

            // O Card agora rola cirurgicamente até o último input aberto!
            card.stop().animate({ scrollTop: targetTop }, 350);
          }
        }
      ")

    } else {
      shinyjs::runjs("$('#conditional_sub_bloco').slideUp(350);")
    }
  })

  # Variáveis para guardar o histórico da navegação
  nav_history <- reactiveValues(prev = "", current = "")

  # Observer que atualiza o histórico toda vez que input$navmenu muda
  observeEvent(input$navmenu, {
    nav_history$prev <- nav_history$current # O que era atual vira anterior
    nav_history$current <- input$navmenu    # O novo input vira atual
  })

  observeEvent(c(input$input_indicador, input$input_desejo_visualizar), {
    prev_menu <- nav_history$prev
    should_scroll <- TRUE

    if (input$abas == "indicadores") {
      if (nrow(tabela_indicadores[which(tabela_indicadores$nome_abreviado_indicador == input$input_indicador), ]) > 1) {

        # Animação para "Desejo Visualizar"
        shinyjs::runjs("
            var card = $('#card-filters .card-body');
            var el = $('#conditional_desejo_visualizar');
            if (card.length && el.length) {
              // SÓ ANIMA SE ESTIVER ESCONDIDO
              if (!el.is(':visible')) {
                el.removeClass('shinyjs-hide');
                el.css({visibility: 'hidden', display: 'block'});
                var elBottom = el.offset().top + el.outerHeight();
                var cardBottom = card.offset().top + card.innerHeight();
                var targetTop = card.scrollTop();
                if (elBottom > cardBottom) {
                  targetTop = card.scrollTop() + (elBottom - cardBottom) + 20;
                }
                el.css({visibility: 'visible', display: 'none'});
                el.slideDown(350);
                card.stop().animate({ scrollTop: targetTop }, 350);
              }
            }
        ")

        if (input$input_desejo_visualizar == "individual") {

          # Animação para "Categoria"
          shinyjs::runjs("
              var card = $('#card-filters .card-body');
              var el = $('#conditional_categoria');
              if (card.length && el.length) {
                // SÓ ANIMA SE ESTIVER ESCONDIDO
                if (!el.is(':visible')) {
                  el.removeClass('shinyjs-hide');
                  el.css({visibility: 'hidden', display: 'block'});
                  var elBottom = el.offset().top + el.outerHeight();
                  var cardBottom = card.offset().top + card.innerHeight();
                  var targetTop = card.scrollTop();
                  if (elBottom > cardBottom) {
                    targetTop = card.scrollTop() + (elBottom - cardBottom) + 20;
                  }
                  el.css({visibility: 'visible', display: 'none'});
                  el.slideDown(350);
                  card.stop().animate({ scrollTop: targetTop }, 350);
                }
              }
          ")

        } else {
          shinyjs::runjs("$('#conditional_categoria').slideUp(350);")
        }
      } else {
        shinyjs::runjs("$('#conditional_categoria').slideUp(350);")
        shinyjs::runjs("$('#conditional_desejo_visualizar').slideUp(350);")
      }
    }
  }, priority = 994)


  observeEvent(input$input_nivel, {
    if (input$input_nivel == "estabelecimento") {
      shinyjs::hide("conditional_hospital_multiplo", anim = TRUE, animType = "slide", time = 0.35)
      shinyjs::show("conditional_hospital_unico", anim = TRUE, animType = "slide", time = 0.35)
    } else {
      shinyjs::hide("conditional_hospital_unico", anim = TRUE, animType = "slide", time = 0.35)
      shinyjs::show("conditional_hospital_multiplo", anim = TRUE, animType = "slide", time = 0.35)
    }

    if (input$input_nivel %in% c("estabelecimento", "municipio")) {
      shinyjs::show("conditional_municipio", anim = TRUE, animType = "slide", time = 0.35)
    } else {
      shinyjs::hide("conditional_municipio", anim = TRUE, animType = "slide", time = 0.35)
    }

    if (input$input_nivel == "macro_r_saude") {
      shinyjs::show("conditional_macro_r_saude", anim = TRUE, animType = "slide", time = 0.35)
    } else {
      shinyjs::hide("conditional_macro_r_saude", anim = TRUE, animType = "slide", time = 0.35)
    }

    if (input$input_nivel == "r_saude") {
      shinyjs::show("conditional_r_saude", anim = TRUE, animType = "slide", time = 0.35)
    } else {
      shinyjs::hide("conditional_r_saude", anim = TRUE, animType = "slide", time = 0.35)
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
    "input_tipo_periodo",
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
    default_tipo_periodo <- "12_meses"
    default_periodo <- c("2023-01-01", "2023-12-01")

    # Atualiza inputs visuais
    updateSelectizeInput(session, "input_nivel", selected = default_nivel)
    updateSelectizeInput(session, "input_uf", selected = default_uf)
    #updateSelectizeInput(session, "input_r_saude", selected = default_r_saude)
    updateSelectizeInput(session, "input_municipio", selected = default_municipio)
    shinyWidgets::updateVirtualSelect("input_hospital_unico", selected = default_hospital, session = session)
    shinyWidgets::updatePrettyRadioButtons(session, "input_tipo_periodo", selected = default_tipo_periodo)
    shinyWidgets::updateAirDateInput(session, "input_periodo", value = default_periodo)

    # Atualiza os reactiveVals
    valid_inputs$input_uf(default_uf)
    #valid_inputs$input_r_saude(default_r_saude)
    valid_inputs$input_municipio(default_municipio)
    valid_inputs$input_hospital_unico(default_hospital)
    valid_inputs$input_tipo_periodo(default_tipo_periodo)
    valid_inputs$input_periodo(default_periodo)
  })


  # Criando os chips de filtros ---------------------------------------------
  ## Criando o objeto reativo com os filtros "congelados" (inicia com os valores default da UI)
  filtros_congelados <- reactiveValues(
    uf = "Rio de Janeiro",
    municipio = "Angra dos Reis",
    hospital = "HOSPITAL MATERNIDADE DE ANGRA DOS REIS HMAR",
    tipo_periodo = "12_meses",
    periodo = c("2023-01-01", "2023-12-01"),
    bloco = "perfil_sociodemografico",
    bloco_label = "Perfil Sociodemográfico",
    sub_bloco = "",
    indicador = "",
    categoria = "",
    desejo_visualizar = "agrupado"
  )


  ## Lógica para atualizar ao clicar no botão "Atualizar"
  observeEvent(c(input$btn_atualizar, from_navbar()), {
    filtros_congelados$uf <- input$input_uf
    filtros_congelados$municipio <- input$input_municipio

    # Lógica de nível para o hospital
    filtros_congelados$hospital <- if (input$input_nivel == "estabelecimento") {
      input$input_hospital_unico
    } else {
      input$input_hospital_multiplo
    }

    filtros_congelados$id_hospital_ref <- if (input$input_nivel == "estabelecimento") "input_hospital_unico" else "input_hospital_multiplo"

    filtros_congelados$tipo_periodo <- input$input_tipo_periodo
    filtros_congelados$periodo <- input$input_periodo
    filtros_congelados$bloco <- input$input_bloco
    filtros_congelados$bloco_label <- unique(tabela_indicadores$bloco_por_extenso[which(tabela_indicadores$bloco == input$input_bloco)])
    filtros_congelados$sub_bloco <- input$input_sub_bloco
    filtros_congelados$indicador <- input$input_indicador
    filtros_congelados$categoria <- input$input_categoria
    filtros_congelados$desejo_visualizar <- input$input_desejo_visualizar

  }, priority = 994)


  observeEvent(c(input$btn_atualizar, from_navbar()), {

    # Remove o aviso de exclamação pois os chips agora condizem com os filtros
    shinyjs::runjs("document.getElementById('aviso_exclamacao').style.opacity = 0;")

  }, priority = 993)


  ## Criando o renderUI com os valores congelados
  output$filtros_ativos_chips <- renderUI({

    # 1. Tratamento do texto do período
    periodo_texto <- if (!is.null(filtros_congelados$periodo)) {
      format_data <- function(d) format(as.Date(paste0(d, "-01")), "%b/%y")
      paste(format_data(filtros_congelados$periodo[1]), "até", format_data(filtros_congelados$periodo[2]))
    } else {
      "jan/23 até dez/23"
    }

    # 2. Criamos a lista de chips que SEMPRE aparecem
    chips <- list(
      create_filter_chip("UF", filtros_congelados$uf, "input_uf", placeholder = "Nenhuma selecionada"),
      create_filter_chip("Município", filtros_congelados$municipio, "input_municipio", placeholder = "Nenhum selecionado"),
      create_filter_chip("Estabelecimento", filtros_congelados$hospital, filtros_congelados$id_hospital_ref, placeholder = "Nenhum selecionado"),
      create_filter_chip("Período", periodo_texto, "input_tipo_periodo", placeholder = "Todo o período", icon_name = "undo")
    )

    # 3. Adicionamos os chips de Indicadores apenas na aba correta
    if (identical(input$abas, "indicadores")) {
      chips_tecnicos <- list(
        create_filter_chip("Bloco", filtros_congelados$bloco_label, "input_bloco", placeholder = "Nenhum selecionado")
      )
      # Unimos as duas listas
      chips <- c(chips, chips_tecnicos)
    }

    # 4. Renderizamos o container final com a lista resultante
    tags$div(
      class = "filter-container",
      chips
    )
  })


  ## Lógica para resetar os dados ao clicar no "X" do Chip (com Cascata)
  observeEvent(input$remover_filtro_id, {
    id <- input$remover_filtro_id

    # --- 1. Funções Auxiliares para evitar repetição ---
    reset_ui_select <- function(ids) {
      lapply(ids, function(i) updateSelectizeInput(session, i, selected = ""))
    }

    reset_ui_virtual <- function(ids) {
      lapply(ids, function(i) shinyWidgets::updateVirtualSelect(inputId = i, selected = character(0)))
    }

    # --- 2. Lógica de Execução por Grupos ---

    # Grupo: Localização (UF > Município > Hospital)
    if (id %in% c("input_uf", "input_municipio", "input_hospital_unico", "input_hospital_multiplo")) {

      # Se resetar UF, limpa tudo abaixo
      if (id == "input_uf") {
        updateSelectizeInput(session, "input_uf", selected = "")
        filtros_congelados$uf <- ""
      }

      # Se resetar UF ou Município, limpa município e hospital
      if (id %in% c("input_uf", "input_municipio")) {
        updateSelectizeInput(session, "input_municipio", selected = "")
        filtros_congelados$municipio <- ""
      }

      # Sempre limpa o hospital se qualquer nível acima for tocado ou ele mesmo
      reset_ui_virtual(c("input_hospital_unico", "input_hospital_multiplo"))
      filtros_congelados$hospital <- character(0)

      # --- 3. Finalização Comum ---
      shinyjs::runjs("$('#card-filters .card-body').animate({ scrollTop: 0 }, 350);")
    }

    # Grupo: Bloco e Indicadores
    else if (id == "input_bloco") {
      # Reseta todos os inputs relacionados de uma vez
      reset_ui_select(c("input_bloco", "input_sub_bloco", "input_indicador", "input_categoria"))

      # Reseta os dados congelados
      filtros_congelados$bloco_label <- ""
      filtros_congelados$bloco <- ""
      filtros_congelados$sub_bloco <- ""
      filtros_congelados$indicador <- ""
      filtros_congelados$categoria <- ""

      # Ao invés de mandar pro fundo, rola a tela de volta para o topo do input de Bloco
      shinyjs::runjs("
          var el = document.getElementById('conditional_indicador');
          if(el) { el.scrollIntoView({behavior: 'smooth', block: 'start'}); }
      ")
    }

    # Grupo: Período
    else if (id == "input_tipo_periodo") {
      shinyWidgets::updatePrettyRadioButtons(session, "input_tipo_periodo", selected = "12_meses")
      filtros_congelados$periodo <- c("2023-01-01", "2023-12-01")

      shinyjs::runjs("$('#card-filters .card-body').animate({ scrollTop: 0 }, 350);")
    }

  })


  observe({
    # 1. Mapeamento de Inputs
    valores_atuais <- list(
      uf                = input$input_uf,
      municipio         = input$input_municipio,
      hospital          = if(input$input_nivel == "estabelecimento") input$input_hospital_unico else input$input_hospital_multiplo,
      bloco             = input$input_bloco,
      sub_bloco         = input$input_sub_bloco,
      indicador         = input$input_indicador,
      categoria         = input$input_categoria,
      tipo_periodo      = input$input_tipo_periodo,
      desejo_visualizar = input$input_desejo_visualizar
    )

    # 2. Definição Dinâmica de Campos Obrigatórios
    # UF, Município e Hospital são sempre obrigatórios
    obrigatorios <- c("uf", "municipio", "hospital")

    # Bloco, Indicador e Categoria só travam o botão na aba de indicadores
    if (identical(input$abas, "indicadores")) {
      obrigatorios <- c(obrigatorios, "bloco", "sub_bloco", "indicador", "categoria")
    }

    esta_vazio <- function(x) {
      if (is.null(x) || length(x) == 0) return(TRUE)
      all(x == "")
    }

    # Verifica vacuidade apenas para o que é obrigatório no momento
    status_vazio <- sapply(valores_atuais[obrigatorios], esta_vazio)
    algum_vazio  <- any(status_vazio)

    # 3. Controle do Botão
    if (algum_vazio) {
      shinyjs::disable("btn_atualizar")
      shinyjs::runjs("document.getElementById('aviso_exclamacao').style.opacity = 0;")
    } else {
      shinyjs::enable("btn_atualizar")
    }

    # 4. Atualização das Exclamações Individuais
    toggle_excl <- function(id, condicao) {
      shinyjs::runjs(sprintf("document.getElementById('%s').style.opacity = %s;", id, if(condicao) 1 else 0))
    }

    # Lista de todos os campos que possuem ícone de exclamação na UI
    todos_com_icone <- c("uf", "municipio", "hospital", "bloco", "sub_bloco", "indicador", "categoria")

    lapply(todos_com_icone, function(nome) {
      id_html <- if(nome == "hospital") "exclamacao_hospital_unico" else paste0("exclamacao_", nome)

      # Lógica: Só mostra exclamação se o campo for OBRIGATÓRIO agora E estiver vazio
      deve_mostrar <- (nome %in% obrigatorios) && esta_vazio(valores_atuais[[nome]])
      toggle_excl(id_html, deve_mostrar)
    })

    # 5. Comparação Global (Dinâmica por Aba)
    congelados_list <- reactiveValuesToList(filtros_congelados)

    # Definimos quais campos fazem sentido comparar em cada contexto
    # UF, Município, Hospital e Período sempre são comparados
    campos_comparar <- c("uf", "municipio", "hospital", "tipo_periodo", "desejo_visualizar")

    # Bloco e seus derivados só entram na comparação se estivermos na aba de indicadores
    if (identical(input$abas, "indicadores")) {
      campos_comparar <- c(campos_comparar, "bloco", "sub_bloco", "indicador", "categoria")
    }

    # Filtramos apenas os nomes que existem em ambos e que decidimos comparar agora
    nomes_para_teste <- intersect(names(valores_atuais), campos_comparar)

    # Comparamos as sub-listas
    # Se estiver fora de 'indicadores', ele vai ignorar se o bloco atual é diferente do congelado
    mudou_algo <- !identical(valores_atuais[nomes_para_teste], congelados_list[nomes_para_teste])

    # Só mostra o alerta de "Atualizar" se não houver erro impeditivo (algum_vazio)
    # Adicionamos req(input$abas) para garantir que a aba já carregou
    req(input$abas)
    diff_global <- !isTRUE(from_navbar()) && !algum_vazio && mudou_algo

    toggle_excl("aviso_exclamacao", diff_global)

  }, priority = 993)


  # -------------------------------------------------------------------------
  # Lógica de Navegação da Home (Modal PREMIUM)
  # -------------------------------------------------------------------------

  observeEvent(input$clique_bloco_home, {
    bloco_id <- input$clique_bloco_home
    nome_bloco_extenso <- unique(tabela_indicadores$bloco_por_extenso[tabela_indicadores$bloco == bloco_id])
    indicadores_do_bloco <- tabela_indicadores |>
      dplyr::filter(bloco == bloco_id) |>
      dplyr::select(nome_abreviado_indicador, indicador_principal) |>
      unique()

    opcoes <- setNames(indicadores_do_bloco$nome_abreviado_indicador, indicadores_do_bloco$indicador_principal)

    icone_bloco <- switch(bloco_id,
                          "perfil_sociodemografico" = "person-dress",
                          "perfil_obstetrico" = "person-breastfeeding",
                          "indicadores_assistenciais" = "stethoscope",
                          "indicadores_assistencia_ao_parto" = "stethoscope",
                          "perfil_dos_nascimentos" = "baby-carriage",
                          "morbimortalidade_materna" = "heart-pulse",
                          "morbimortalidade_perinatal" = "baby",
                          "list"
    )

    showModal(
      modalDialog(
        title = NULL, header = NULL, footer = NULL, size = "l",
        easyClose = TRUE, fade = TRUE,
        class = "premium-modal",

        div(
          # 1. Topo escuro com o ícone flutuante
          div(
            class = "pm-header",
            div(class = "pm-icon", icon(icone_bloco)),
            h3("Navegar para indicador", class = "pm-title"),
            p(nome_bloco_extenso, class = "pm-subtitle")
          ),
          div(
            class = "pm-body",
            span("Qual indicador você deseja analisar?", class = "pm-label"),
            selectizeInput(
              inputId = "input_indicador_modal",
              label = NULL,
              choices = opcoes,
              width = "100%",
              options = list(
                placeholder = "Selecione um indicador",
                dropdownParent = 'body',
                dropdownClass = "dropdown-modal-premium"
              )
            )
          ),
          div(
            class = "pm-footer",
            actionButton(
              inputId = "btn_fechar_modal",
              label = "Cancelar",
              class = "pm-btn-cancel fonte-grande",
              `data-dismiss` = "modal"
            ),
            actionButton(
              inputId = "btn_ir_para_painel",
              label = "Visualizar indicador",
              icon = icon("arrow-right"),
              class = "pm-btn-submit fonte-grande"
            )
          )
        )
      )
    )
  }, ignoreInit = TRUE)

  # -------------------------------------------------------------------------
  # Confirmação do Modal (LÓGICA DE CASCATA CORRIGIDA)
  # -------------------------------------------------------------------------
  observeEvent(input$btn_ir_para_painel, {
    req(input$input_indicador_modal)

    bloco_id <- input$clique_bloco_home
    indicador_id <- input$input_indicador_modal

    # 1. Fecha o modal e limpa a tela
    removeModal()
    shinyjs::runjs("$('body').css('overflow', '').removeClass('modal-open').css('padding-right', ''); $('.modal-backdrop').remove();")

    # 2. Ativa as Flags para bloquear a "limpeza automática" da barra lateral
    indicador_atual_nav(indicador_id)
    awaiting_nav_categoria(TRUE)

    # 3. Descobre o Sub-bloco correto do indicador escolhido
    sub_bloco_id <- tabela_indicadores$sub_bloco[which(tabela_indicadores$nome_abreviado_indicador == indicador_id)][1]
    indicadores_bloco_selecionado <- get(paste0("indicadores_", bloco_id))

    # 4. Atualiza TODA a cascata perfeitamente (Bloco > Sub-bloco > Indicador > Categoria)
    updateSelectInput(session, "input_bloco", selected = bloco_id)

    updateSelectInput(
      session, "input_sub_bloco",
      choices = setNames(
        unique(indicadores_bloco_selecionado$sub_bloco),
        unique(indicadores_bloco_selecionado$sub_bloco_por_extenso)
      ),
      selected = sub_bloco_id
    )

    updateSelectInput(
      session, "input_indicador",
      choices = setNames(
        indicadores_bloco_selecionado$nome_abreviado_indicador[which(indicadores_bloco_selecionado$sub_bloco == sub_bloco_id)],
        indicadores_bloco_selecionado$indicador_principal[which(indicadores_bloco_selecionado$sub_bloco == sub_bloco_id)]
      ),
      selected = indicador_id
    )

    updateSelectizeInput(
      session, "input_categoria",
      label = tabela_indicadores$label_input_categoria[tabela_indicadores$nome_abreviado_indicador == indicador_id][1],
      choices = setNames(
        tabela_indicadores$sub_indicador[tabela_indicadores$nome_abreviado_indicador == indicador_id],
        tabela_indicadores$categoria_sub_indicador[tabela_indicadores$nome_abreviado_indicador == indicador_id]
      ),
      server = FALSE
    )

    # 5. Muda fisicamente de aba e sobe a tela
    shinyjs::runjs("
      openTab('indicadores');
      window.scrollTo({top: 0, behavior: 'smooth'});
    ")

  }, ignoreInit = TRUE)


  # Transmitindo os inputs --------------------------------------------------
  trigger_filtros <- reactiveVal(0)

  observeEvent(input$btn_atualizar, {
    # Verifica se o clique veio dos botões circulares
    if (is.character(input$btn_atualizar) && input$btn_atualizar == "circle_btn") {

      # Veio dos botões circulares: APLICA DELAY
      shinyjs::delay(250, trigger_filtros(isolate(trigger_filtros()) + 1))

    } else {

      # Veio de qualquer outro lugar: EXECUTA IMEDIATAMENTE
      trigger_filtros(isolate(trigger_filtros()) + 1)

    }
  })

  filtros_visao_geral <- eventReactive(
    list(trigger_filtros()),
    {
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
        input_periodo = periodo_valido
      )
    },
    ignoreNULL = FALSE
  )

  filtros_indicadores <- eventReactive(
    list(trigger_filtros(), from_navbar()),
    {
      from_navbar(FALSE)

      req(input$input_categoria)

      # constrói lista final
      c(
        filtros_visao_geral(),
        list(
          input_desejo_visualizar = if (nrow(tabela_indicadores[which(tabela_indicadores$nome_abreviado_indicador == input$input_indicador), ]) == 1) {
            "individual"
          } else {
            input$input_desejo_visualizar
          },
          input_bloco = input$input_bloco,
          input_indicador = input$input_indicador,
          input_sub_indicador = input$input_categoria
        )
      )
    },
    ignoreNULL = FALSE
  )



}
