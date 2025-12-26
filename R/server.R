# ---- Server ----

server <- function(input, output, session) {

  # Helper: modal de OSRM offline com instruções

  mostrar_modal_osrm_offline <- function(url_tentada) {
    shiny::showModal(
      shiny::modalDialog(
        title = "Servidor OSRM nao esta rodando",
        shiny::tags$p(
          shiny::tags$strong("O servidor OSRM nao respondeu em:"),
          shiny::tags$code(url_tentada)
        ),
        shiny::tags$hr(),
        shiny::tags$p(shiny::tags$strong("Para iniciar o OSRM, execute no terminal:")),
        shiny::tags$pre(
          style = "background: #f5f5f5; padding: 10px; border-radius: 4px; overflow-x: auto;",
          "# Se voce usa containers individuais:\ndocker start osrm-car osrm-foot osrm-bike osrm-bus\n\n# Ou se usa docker-compose:\ncd ~/location_history/osrm\ndocker-compose up -d"
        ),
        shiny::tags$hr(),
        shiny::tags$p(shiny::tags$strong("Para verificar se esta rodando:")),
        shiny::tags$pre(
          style = "background: #f5f5f5; padding: 10px; border-radius: 4px;",
          "docker ps | grep osrm"
        ),
        shiny::tags$hr(),
        shiny::tags$p(
          shiny::tags$strong("Portas esperadas:"),
          shiny::tags$ul(
            shiny::tags$li("Carro: ", shiny::tags$code("localhost:5000")),
            shiny::tags$li("A pe: ", shiny::tags$code("localhost:5001")),
            shiny::tags$li("Bicicleta: ", shiny::tags$code("localhost:5002")),
            shiny::tags$li("Onibus: ", shiny::tags$code("localhost:5003"))
          )
        ),
        easyClose = TRUE,
        footer = shiny::modalButton("Entendi")
      )
    )
  }

  # Estado reativo
  pontos_temp <- shiny::reactiveVal(data.frame(lat = numeric(0), lng = numeric(0)))
  timeline <- shiny::reactiveVal(list())
  all_samples <- shiny::reactiveVal(list())
  edit_samples <- shiny::reactiveVal(list())  # Samples carregados para edição
  ignored_samples <- shiny::reactiveVal(character(0))  # IDs de samples ignorados
  rota_osrm <- shiny::reactiveVal(NULL)  # Geometria da rota OSRM (matriz lon/lat)
  samples_virtuais <- shiny::reactiveVal(list())  # Samples criados da rota OSRM (pontos intermediários)
  original_samples <- shiny::reactiveVal(list())  # Samples originais antes da edição
  original_timeline_item_id <- shiny::reactiveVal(NULL)  # timelineItemId original dos samples

  # ---- Location History de outras pessoas ----
  # PESSOAS_CONFIG é carregada no app.R (variável global)
  pessoas_config <- tryCatch(
    get("PESSOAS_CONFIG", envir = globalenv()),
    error = function(e) list()
  )
  location_history_cache <- shiny::reactiveVal(list())  # Cache dos dados carregados

  # Gera toggles dinamicamente baseado na configuração
  output$toggles_pessoas <- shiny::renderUI({
    if (length(pessoas_config) == 0) return(NULL)

    checkboxes <- lapply(pessoas_config, function(p) {
      shiny::checkboxInput(
        inputId = paste0("lh_", p$id),
        label   = shiny::span(
          shiny::icon("map-marker-alt"),
          p$label,
          style = paste0("color:", p$cor, "; font-weight: 500;")
        ),
        value   = FALSE
      )
    })

    shiny::tagList(
      shiny::tags$small(
        shiny::tags$strong("Location History:"),
        style = "color: #666;"
      ),
      checkboxes,
      shiny::hr()
    )
  })

  # Carrega dados de Location History quando necessário
  carregar_lh_pessoa <- function(pessoa_id) {
    cache <- location_history_cache()
    if (!is.null(cache[[pessoa_id]])) {
      return(cache[[pessoa_id]])
    }

    # Encontra config da pessoa
    cfg <- NULL
    for (p in pessoas_config) {
      if (p$id == pessoa_id) {
        cfg <- p
        break
      }
    }
    if (is.null(cfg)) return(NULL)

    # Mostra notificação de carregamento
    shiny::showNotification(
      paste0("Carregando Location History de ", cfg$label, "..."),
      id = "lh_loading", type = "message", duration = NULL
    )

    # Carrega o arquivo
    dados <- carregar_location_history(cfg$arquivo)

    shiny::removeNotification("lh_loading")

    if (!is.null(dados)) {
      cache[[pessoa_id]] <- dados
      location_history_cache(cache)
      shiny::showNotification(
        paste0(cfg$label, ": ", nrow(dados), " registros carregados"),
        type = "message", duration = 3
      )
    } else {
      shiny::showNotification(
        paste0("Falha ao carregar dados de ", cfg$label),
        type = "error"
      )
    }

    dados
  }

  # Atualiza mapa quando data ou toggles mudam
  shiny::observe({
    data_atual <- input$data_trabalho
    if (is.null(data_atual)) return()

    # Limpa camadas anteriores
    proxy <- leaflet::leafletProxy("map")
    for (p in pessoas_config) {
      proxy <- proxy %>% leaflet::clearGroup(paste0("lh_", p$id))
    }

    # Desenha cada pessoa ativada
    for (p in pessoas_config) {
      toggle_id <- paste0("lh_", p$id)
      if (!isTRUE(input[[toggle_id]])) next

      # Usa cache RDS se disponível, senão carrega JSON completo
      if (cache_existe(p$id)) {
        df <- carregar_lh_por_data(p$id, data_atual)
      } else {
        dados <- carregar_lh_pessoa(p$id)
        if (is.null(dados)) next
        df <- filtrar_location_history_por_data(dados, data_atual)
      }

      if (is.null(df) || nrow(df) == 0) next

      group_name <- paste0("lh_", p$id)

      # Desenha visitas como círculos
      visitas <- df[df$tipo == "visit", ]
      if (nrow(visitas) > 0) {
        for (i in seq_len(nrow(visitas))) {
          v <- visitas[i, ]
          data_hora_ini <- formatar_data_hora_lh(v$start_time)
          data_hora_fim <- formatar_data_hora_lh(v$end_time)

          proxy <- proxy %>%
            leaflet::addCircleMarkers(
              lng = v$lon, lat = v$lat,
              radius = 10,
              color = p$cor, fillColor = p$cor, fillOpacity = 0.6,
              stroke = TRUE, weight = 2,
              group = group_name,
              label = paste0(p$label, ": ", data_hora_ini, " - ", data_hora_fim)
            )
        }
      }

      # Desenha atividades como linhas com indicadores de direção
      atividades <- df[df$tipo == "activity", ]
      if (nrow(atividades) > 0) {
        for (i in seq_len(nrow(atividades))) {
          a <- atividades[i, ]
          data_hora_ini <- formatar_data_hora_lh(a$start_time)
          data_hora_fim <- formatar_data_hora_lh(a$end_time)
          label_texto <- paste0(p$label, ": ", data_hora_ini, " -> ", data_hora_fim)

          # Linha do trajeto
          proxy <- proxy %>%
            leaflet::addPolylines(
              lng = c(a$lon, a$lon_end),
              lat = c(a$lat, a$lat_end),
              color = p$cor, weight = 3, opacity = 0.7,
              group = group_name,
              label = label_texto
            )

          # Marcador de início (bolinha pequena verde)
          proxy <- proxy %>%
            leaflet::addCircleMarkers(
              lng = a$lon, lat = a$lat,
              radius = 4,
              color = "green", fillColor = "green", fillOpacity = 1,
              stroke = FALSE,
              group = group_name,
              label = paste0("Início: ", data_hora_ini)
            )

          # Marcador de fim (bolinha pequena vermelha)
          proxy <- proxy %>%
            leaflet::addCircleMarkers(
              lng = a$lon_end, lat = a$lat_end,
              radius = 4,
              color = "red", fillColor = "red", fillOpacity = 1,
              stroke = FALSE,
              group = group_name,
              label = paste0("Fim: ", data_hora_fim)
            )
        }
      }
    }
  })

  # Formata automaticamente inputs de hora (só quando parece completo)
  auto_format_time <- function(id) {
    shiny::observeEvent(input[[id]], {
      val <- input[[id]]
      if (is.null(val) || !nzchar(val)) return()

      # SÓ FORMATA se já digitou pelo menos 3 caracteres (evita formatar enquanto digita)
      if (nchar(gsub("\\D", "", val)) < 3) return()

      fmt <- formatar_hora(val)
      if (!is.na(fmt) && !identical(fmt, val)) {
        shiny::updateTextInput(session, id, value = fmt)
      }
    }, ignoreInit = TRUE)
  }

  # Campos de hora que devem ser auto-formatados
  auto_format_time("visita_hora_inicio")
  auto_format_time("visita_hora_fim")
  auto_format_time("manual_hora_inicio")
  auto_format_time("manual_hora_fim")
  auto_format_time("import_hora_inicio")
  auto_format_time("import_hora_fim")
  auto_format_time("osrm_hora_inicio")
  auto_format_time("osrm_hora_fim")
  auto_format_time("edit_hora_inicio")
  auto_format_time("edit_hora_fim")
  auto_format_time("edit_samples_hora_inicio")
  auto_format_time("edit_samples_hora_fim")


  # Retorna o fim local da ÚLTIMA atividade da timeline (ou NULL se não der pra calcular)
  get_last_end_local <- function() {
    tl <- timeline()
    if (length(tl) == 0) return(NULL)

    # pega os endDate como POSIXct (UTC), ignorando itens quebrados
    ends_num <- vapply(tl, function(it) {
      date_str <- it$endDate$date %||% NA_character_
      ts <- parse_timestamp_utc(date_str)
      if (is.null(ts) || is.na(ts[1])) {
        NA_real_
      } else {
        as.numeric(ts[1])  # segundos desde epoch
      }
    }, numeric(1))

    # se todo mundo deu NA, não tem como descobrir último fim
    if (all(is.na(ends_num))) return(NULL)

    # pega o índice do MAIOR endDate válido
    idx <- which.max(ends_num)
    if (length(idx) == 0 || is.na(idx) || idx < 1) return(NULL)

    # reconstrói o POSIXct em UTC
    end_utc <- as.POSIXct(ends_num[idx], origin = "1970-01-01", tz = "UTC")

    end_utc
  }


  # Quando a timeline mudar, sugerir próximo início = fim anterior + 1 min
  shiny::observeEvent(timeline(), {
    last_utc <- get_last_end_local()  # retorna em UTC
    if (is.null(last_utc)) return()

    # Converte para timezone do sistema do usuário
    last_local <- lubridate::with_tz(last_utc, tzone = Sys.timezone())

    next_local <- last_local + 60  # +1 minuto

    data_next <- as.Date(next_local)
    hora_next <- format(next_local, "%H:%M:%S")

    # visita
    shiny::updateDateInput(session, "visita_data_inicio", value = data_next)
    shiny::updateTextInput(session, "visita_hora_inicio", value = hora_next)

    # rota manual
    shiny::updateDateInput(session, "manual_data_inicio", value = data_next)
    shiny::updateTextInput(session, "manual_hora_inicio", value = hora_next)

    # importar
    shiny::updateDateInput(session, "import_data_inicio", value = data_next)
    shiny::updateTextInput(session, "import_hora_inicio", value = hora_next)
  })


  # Quando a data principal mudar, replica para os outros campos de data
  shiny::observeEvent(input$data_trabalho, {
    d <- input$data_trabalho

    shiny::updateDateInput(session, "visita_data_inicio", value = d)
    shiny::updateDateInput(session, "visita_data_fim",    value = d)

    shiny::updateDateInput(session, "manual_data_inicio", value = d)
    shiny::updateDateInput(session, "manual_data_fim",    value = d)

    shiny::updateDateInput(session, "import_data_inicio", value = d)
    shiny::updateDateInput(session, "import_data_fim",    value = d)

    shiny::updateDateInput(session, "edit_samples_data_inicio", value = d)
    shiny::updateDateInput(session, "edit_samples_data_fim",    value = d)
  })


  # ---- Mapa base ----
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles(group = "OSM") |>
      leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
      leaflet::addLayersControl(
        baseGroups = c("CartoDB", "OSM", "Satélite"),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) |>
      leaflet::setView(lng = -43.2, lat = -22.9, zoom = 11)
  })

  # Helper: atualiza pontos no mapa
  atualizar_pontos_mapa <- function(auto_zoom = TRUE) {
    pts <- pontos_temp()
    proxy <- leaflet::leafletProxy("map")
    proxy <- proxy %>% leaflet::clearGroup("waypoints")
    if (nrow(pts) > 0) {
      proxy <- proxy %>%
        leaflet::addCircleMarkers(
          data = pts,
          lng = ~lng, lat = ~lat,
          radius = 6,
          color = "red",
          fillColor = "red",
          fillOpacity = 0.8,
          label = ~as.character(seq_len(nrow(pts))),
          group = "waypoints"
        )

      # fitBounds APENAS se auto_zoom = TRUE
      if (auto_zoom) {
        proxy <- proxy %>%
          leaflet::fitBounds(
            lng1 = min(pts$lng), lat1 = min(pts$lat),
            lng2 = max(pts$lng), lat2 = max(pts$lat)
          )
      }
    }
  }

  # Clique no mapa
  shiny::observeEvent(input$map_click, {
    click <- input$map_click
    if (is.null(click)) return()

    modo <- input$modo

    # Ignora cliques no modo Editar Samples (usa marcadores arrastáveis)
    if (modo == "Editar Samples") return()

    # Ignora cliques no modo Importar Arquivo (usa arquivo carregado)
    if (modo == "Importar Arquivo") return()

    pts <- pontos_temp()

    if (modo == "Visita") {
      # visita usa só 1 ponto: substitui
      pts <- data.frame(lat = click$lat, lng = click$lng)
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = TRUE)
    } else {
      # OSRM ou Rota Manual: adiciona ponto
      pts <- rbind(pts, data.frame(lat = click$lat, lng = click$lng))
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = FALSE)
    }
  })

  # Limpar pontos (OSRM / manual) - SEM auto_zoom
  shiny::observeEvent(input$limpar_pontos_osrm, {
    pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))
    atualizar_pontos_mapa(auto_zoom = FALSE)
  })
  shiny::observeEvent(input$limpar_pontos_manual, {
    pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))
    atualizar_pontos_mapa(auto_zoom = FALSE)
  })

  # Desfazer último ponto - SEM auto_zoom
  shiny::observeEvent(input$desfazer_ponto_osrm, {
    pts <- pontos_temp()
    if (nrow(pts) > 0) {
      pts <- pts[-nrow(pts), , drop = FALSE]
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = FALSE)
    }
  })
  shiny::observeEvent(input$desfazer_ponto_manual, {
    pts <- pontos_temp()
    if (nrow(pts) > 0) {
      pts <- pts[-nrow(pts), , drop = FALSE]
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = FALSE)
    }
  })

  # ---- Visita: auto-preencher coordenadas por nome frequente ----

  shiny::observeEvent(input$visita_nome, {
    nome <- input$visita_nome
    if (nome %in% names(LOCAIS_FREQUENTES)) {
      loc <- LOCAIS_FREQUENTES[[nome]]
      pts <- data.frame(lat = loc$lat, lng = loc$lon)
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = TRUE)
      leaflet::leafletProxy("map") %>%
        leaflet::addPopups(loc$lon, loc$lat, paste0("Local: ", nome), layerId = "visita_popup")
    } else {
      leaflet::leafletProxy("map") %>% leaflet::removePopup("visita_popup")
    }
  })

  # ---- Adicionar visita ----

  shiny::observeEvent(input$adicionar_visita, {
    pts <- pontos_temp()
    if (nrow(pts) != 1) {
      shiny::showNotification("Selecione exatamente 1 ponto no mapa para a visita.", type = "error")
      return()
    }
    lat <- pts$lat[1]
    lng <- pts$lng[1]

    tz <- tz_from_coords(lat, lng)

    val <- validar_intervalo(
      data_ini = input$visita_data_inicio,
      hora_ini = input$visita_hora_inicio,
      data_fim = input$visita_data_fim,
      hora_fim = input$visita_hora_fim,
      tz = tz
    )
    if (is.null(val)) {
      shiny::showNotification("Horário inválido para a visita.", type = "error")
      return()
    }

    # converte intervalo da visita para UTC para checar overlap
    inicio_utc <- lubridate::with_tz(val$inicio, "UTC")
    fim_utc    <- lubridate::with_tz(val$fim,    "UTC")

    if (has_overlap(timeline(), inicio_utc, fim_utc)) {
      shiny::showNotification("Este intervalo se sobrepõe a outra atividade existente.", type = "error")
      return()
    }

    visita <- criar_timeline_item_visit(
      lat = lat,
      lon = lng,
      inicio_local = val$inicio,
      fim_local = val$fim,
      nome = if (nzchar(input$visita_nome)) input$visita_nome else NULL
    )


    item <- visita$item
    samples_novos <- visita$samples

    # descrição
    hora_ini_local <- format(val$inicio, "%H:%M")
    hora_fim_local <- format(val$fim, "%H:%M")
    nome_visita <- item$place$name
    item$tipo <- "visita"
    item$descricao <- sprintf("📍 %s (%s - %s)", nome_visita, hora_ini_local, hora_fim_local)

    # Atualiza timeline e samples
    tl <- timeline()
    tl[[length(tl) + 1L]] <- item
    timeline(tl)

    s <- all_samples()
    all_samples(c(s, samples_novos))

    # fitBounds no ponto da visita
    leaflet::leafletProxy("map") %>%
      leaflet::fitBounds(lng, lat, lng, lat)

    shiny::showNotification("Visita adicionada.", type = "message")
  })

  # ---- Rota manual ----

  shiny::observeEvent(input$adicionar_manual, {
    pts <- pontos_temp()
    if (nrow(pts) < 2) {
      shiny::showNotification("Defina pelo menos 2 pontos no mapa para a rota manual.", type = "error")
      return()
    }

    lat1 <- pts$lat[1]
    lng1 <- pts$lng[1]
    tz <- tz_from_coords(lat1, lng1)

    val <- validar_intervalo(
      data_ini = input$manual_data_inicio,
      hora_ini = input$manual_hora_inicio,
      data_fim = input$manual_data_fim,
      hora_fim = input$manual_hora_fim,
      tz = tz
    )
    if (is.null(val)) {
      shiny::showNotification("Horário inválido para a rota manual.", type = "error")
      return()
    }

    # Gera timestamps UTC equiespaçados
    inicio_utc <- lubridate::with_tz(val$inicio, "UTC")
    fim_utc    <- lubridate::with_tz(val$fim,    "UTC")

    if (has_overlap(timeline(), inicio_utc, fim_utc)) {
      shiny::showNotification("Este intervalo se sobrepõe a outra atividade existente.", type = "error")
      return()
    }


    # Interpola coords para pelo menos 100 pontos
    coords <- as.matrix(pts[, c("lng", "lat")])
    n0 <- nrow(coords)
    n_target <- max(n0, 100L)
    ts_seq <- seq(inicio_utc, fim_utc, length.out = n_target)

    t_interp <- seq(0, 1, length.out = n0)
    t_new <- seq(0, 1, length.out = n_target)

    lon_new <- suppressWarnings(approx(
      x = t_interp,
      y = as.numeric(coords[, 1]),
      xout = t_new,
      ties = "ordered"
    )$y)

    lat_new <- suppressWarnings(approx(
      x = t_interp,
      y = as.numeric(coords[, 2]),
      xout = t_new,
      ties = "ordered"
    )$y)


    coords_new <- cbind(lon_new, lat_new)

    samples_novos <- criar_locomotion_samples(
      coords         = coords_new,
      timestamps_utc = ts_seq,
      accuracy       = 0.01,
      force_single_tz = TRUE,
      moving_state   = "moving"
    )
    sample_ids <- vapply(samples_novos, `[[`, "", "sampleId")

    item <- criar_timeline_item_path(
      timestamps_utc = ts_seq,
      coords = coords_new,
      sample_ids = sample_ids,
      tipo = "rota_manual",
      descricao = sprintf("Rota manual (%.1f km)",
                          geosphere::distVincentyEllipsoid(
                            coords_new[1, 2:1], coords_new[n_target, 2:1]
                          ) / 1000)
    )

    tl <- timeline()
    tl[[length(tl) + 1L]] <- item
    timeline(tl)

    s <- all_samples()
    all_samples(c(s, samples_novos))

    # Desenha rota e dá zoom
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("rota_atual") %>%
      leaflet::addPolylines(
        lng = coords_new[, 1],
        lat = coords_new[, 2],
        color = "red",
        weight = 4,
        opacity = 0.8,
        group = "rota_atual"
      ) %>%
      leaflet::fitBounds(
        lng1 = min(coords_new[, 1]),
        lat1 = min(coords_new[, 2]),
        lng2 = max(coords_new[, 1]),
        lat2 = max(coords_new[, 2])
      )

    shiny::showNotification("Rota manual adicionada.", type = "message")
  })

  # ---- OSRM: calcular rota ----

  shiny::observeEvent(input$calcular_osrm, {
    pts <- pontos_temp()
    if (nrow(pts) < 2) {
      shiny::showNotification("Defina pelo menos 2 pontos no mapa para a rota OSRM.", type = "error")
      return()
    }

    perfil <- input$osrm_perfil
    base_url <- OSRM_SERVERS[[perfil]]

    if (is.null(base_url) || !check_osrm_server(base_url)) {
      mostrar_modal_osrm_offline(base_url %||% "nao configurado")
      return()
    }

    rota <- calcular_rota_osrm(pts, perfil = perfil)
    if (is.null(rota)) {
      shiny::showNotification("Falha ao calcular rota OSRM.", type = "error")
      return()
    }

    # Sugere horário com base na última atividade (se houver)
    last_utc <- get_last_end_local()  # retorna em UTC
    if (!is.null(last_utc)) {
      last_local <- lubridate::with_tz(last_utc, tzone = Sys.timezone())
      inicia_sugerido <- last_local + 60  # +1 minuto
    } else {
      data_ref <- input$data_trabalho
      inicia_sugerido <- lubridate::ymd_hms(
        paste(data_ref, "08:00:00"),
        tz = Sys.timezone()
      )
    }
    fim_sugerido <- inicia_sugerido + rota$duration_s


    shiny::showModal(
      shiny::modalDialog(
        title = "Horários da rota OSRM",
        shiny::dateInput("osrm_data_inicio", "Data início", as.Date(inicia_sugerido)),
        shiny::textInput("osrm_hora_inicio", "Hora início", format(inicia_sugerido, "%H:%M:%S")),
        shiny::dateInput("osrm_data_fim", "Data fim", as.Date(fim_sugerido)),
        shiny::textInput("osrm_hora_fim", "Hora fim", format(fim_sugerido, "%H:%M:%S")),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton("recalcular_osrm_fim", "Recalcular chegada"),
          shiny::actionButton("confirmar_rota_osrm", "Confirmar", class = "btn-primary")
        )
      )
    )

    auto_format_time("osrm_hora_inicio")
    auto_format_time("osrm_hora_fim")

    # Guarda rota em atributo da sessão
    session$userData$ultima_rota_osrm <- rota
    session$userData$ultima_rota_pts <- pts
  })

  shiny::observeEvent(input$recalcular_osrm_fim, {
    rota <- session$userData$ultima_rota_osrm
    if (is.null(rota)) {
      shiny::showNotification("Nenhuma rota OSRM calculada para recalcular o horário de chegada.", type = "error")
      return()
    }

    coords <- rota$coords
    if (is.data.frame(coords)) coords <- as.matrix(coords)
    if (!is.matrix(coords) || nrow(coords) < 1 || ncol(coords) < 2) {
      shiny::showNotification("Coordenadas da rota OSRM estão em formato inesperado.", type = "error")
      return()
    }

    lat1 <- as.numeric(coords[1, 2])
    lng1 <- as.numeric(coords[1, 1])
    if (is.na(lat1) || is.na(lng1)) {
      shiny::showNotification("Primeiro ponto da rota OSRM está inválido.", type = "error")
      return()
    }

    tz <- tz_from_coords(lat1, lng1)

    # hora de partida (formata de novo pra garantir)
    hora_ini_fmt <- formatar_hora(input$osrm_hora_inicio)
    if (is.na(hora_ini_fmt)) {
      shiny::showNotification("Hora de partida inválida.", type = "error")
      return()
    }

    inicio_local <- suppressWarnings(
      lubridate::ymd_hms(
        paste(input$osrm_data_inicio, hora_ini_fmt),
        tz = tz
      )
    )
    if (is.na(inicio_local)) {
      shiny::showNotification("Não consegui interpretar a data/hora de partida.", type = "error")
      return()
    }

    # duração em segundos vinda do OSRM
    dur_s <- as.numeric(rota$duration_s)
    if (is.na(dur_s)) {
      shiny::showNotification("Duração da rota OSRM inválida.", type = "error")
      return()
    }

    fim_local <- inicio_local + dur_s

    shiny::updateDateInput(session, "osrm_data_fim", value = as.Date(fim_local))
    shiny::updateTextInput(session, "osrm_hora_fim", value = format(fim_local, "%H:%M:%S"))
  })


  shiny::observeEvent(input$confirmar_rota_osrm, {
    tryCatch(
      {
        shiny::removeModal()

        rota <- session$userData$ultima_rota_osrm
        if (is.null(rota)) {
          shiny::showNotification("Nenhuma rota OSRM calculada.", type = "error")
          return()
        }

        coords <- rota$coords
        if (is.data.frame(coords)) {
          coords <- as.matrix(coords)
        }
        if (!is.matrix(coords) || ncol(coords) < 2) {
          shiny::showNotification("Coordenadas da rota OSRM estão em formato inesperado.", type = "error")
          return()
        }
        coords <- suppressWarnings(apply(coords, 2, as.numeric))
        if (!is.matrix(coords)) {
          coords <- matrix(coords, ncol = 2)
        }

        n0 <- nrow(coords)
        if (is.null(n0) || is.na(n0) || n0 < 2) {
          shiny::showNotification("Rota OSRM inválida (poucos pontos).", type = "error")
          return()
        }

        lat1 <- coords[1, 2]
        lng1 <- coords[1, 1]

        if (is.na(lat1) || is.na(lng1)) {
          shiny::showNotification("Primeiro ponto da rota OSRM está inválido.", type = "error")
          return()
        }

        tz <- tz_from_coords(lat1, lng1)

        hora_ini_fmt <- formatar_hora(input$osrm_hora_inicio)
        hora_fim_fmt <- formatar_hora(input$osrm_hora_fim)

        if (is.na(hora_ini_fmt) || is.na(hora_fim_fmt)) {
          shiny::showNotification("Horas inválidas para rota OSRM.", type = "error")
          return()
        }

        val <- validar_intervalo(
          data_ini = input$osrm_data_inicio,
          hora_ini = hora_ini_fmt,
          data_fim = input$osrm_data_fim,
          hora_fim = hora_fim_fmt,
          tz = tz
        )
        if (is.null(val)) {
          shiny::showNotification("Horário inválido para rota OSRM.", type = "error")
          return()
        }

        if (!inherits(val$inicio, "POSIXt") || !inherits(val$fim, "POSIXt")) {
          shiny::showNotification("Intervalo de horários da rota OSRM não pôde ser interpretado.", type = "error")
          return()
        }

        inicio_utc <- lubridate::with_tz(val$inicio, "UTC")
        fim_utc    <- lubridate::with_tz(val$fim,    "UTC")

        if (is.na(inicio_utc) || is.na(fim_utc) || fim_utc <= inicio_utc) {
          shiny::showNotification("Intervalo da rota OSRM inválido (fim antes ou igual ao início).", type = "error")
          return()
        }

        if (has_overlap(timeline(), inicio_utc, fim_utc)) {
          shiny::showNotification("Este intervalo se sobrepõe a outra atividade existente.", type = "error")
          return()
        }

        n_target <- max(n0, 100L)
        ts_seq <- seq(from = inicio_utc, to = fim_utc, length.out = n_target)
        if (any(is.na(ts_seq))) {
          shiny::showNotification("Falha ao gerar sequência de tempos para rota OSRM.", type = "error")
          return()
        }

        t_interp <- seq(0, 1, length.out = n0)
        t_new    <- seq(0, 1, length.out = n_target)

        lon_new <- suppressWarnings(approx(
          x = t_interp,
          y = coords[, 1],
          xout = t_new,
          ties = "ordered"
        )$y)

        lat_new <- suppressWarnings(approx(
          x = t_interp,
          y = coords[, 2],
          xout = t_new,
          ties = "ordered"
        )$y)

        if (any(is.na(lon_new)) || any(is.na(lat_new))) {
          shiny::showNotification("Falha ao interpolar coordenadas da rota OSRM.", type = "error")
          return()
        }

        coords_new <- cbind(lon_new, lat_new)

        samples_novos <- criar_locomotion_samples(
          coords         = coords_new,
          timestamps_utc = ts_seq,
          accuracy       = 0.01,
          force_single_tz = TRUE,
          moving_state   = "moving"
        )
        sample_ids <- vapply(samples_novos, `[[`, "", "sampleId")

        item <- criar_timeline_item_path(
          timestamps_utc = ts_seq,
          coords         = coords_new,
          sample_ids     = sample_ids,
          tipo           = "rota_osrm",
          descricao      = "Rota OSRM"
        )

        tl <- timeline()
        tl[[length(tl) + 1L]] <- item
        timeline(tl)

        s <- all_samples()
        all_samples(c(s, samples_novos))

        leaflet::leafletProxy("map") %>%
          leaflet::clearGroup("rota_atual") %>%
          leaflet::addPolylines(
            lng   = coords_new[, 1],
            lat   = coords_new[, 2],
            color = "blue",
            weight = 4,
            group = "rota_atual"
          ) %>%
          leaflet::fitBounds(
            lng1 = min(coords_new[, 1]),
            lat1 = min(coords_new[, 2]),
            lng2 = max(coords_new[, 1]),
            lat2 = max(coords_new[, 2])
          )

        shiny::showNotification("Rota OSRM adicionada.", type = "message")
      },
      error = function(e) {
        shiny::showNotification(
          paste("Erro ao confirmar rota OSRM:", e$message),
          type = "error",
          duration = NULL
        )
      }
    )
  })



  # ---- Importar arquivo (GeoJSON/GPX/KML/GPKG) ----

  geometria_importada <- shiny::reactiveVal(NULL)
  fr24_enriquecido <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(input$arquivo_geo, {
    shiny::req(input$arquivo_geo)
    arquivo <- input$arquivo_geo$datapath
    ext <- tools::file_ext(arquivo)
    ext <- tolower(ext)

    leaflet::leafletProxy("map") %>% leaflet::clearGroup("importado")

    if (ext == "kml") {
      geo_rich <- parse_kml_rich(arquivo)
      if (!is.null(geo_rich) && "timestamp" %in% names(geo_rich)) {
        geometria_importada(geo_rich)
        fr24_enriquecido(TRUE)

        ts_gmt <- parse_timestamp_utc(geo_rich$timestamp)
        ord <- order(ts_gmt)
        ts_gmt <- ts_gmt[ord]

        coords <- cbind(
          geo_rich$lon[ord],
          geo_rich$lat[ord]
        )
        colnames(coords) <- c("lon", "lat")


        if (!is.null(ts_gmt) && !all(is.na(ts_gmt))) {
          ini <- ts_gmt[1]
          fim <- ts_gmt[length(ts_gmt)]
          shiny::updateDateInput(session, "import_data_inicio", value = as.Date(ini))
          shiny::updateTextInput(session, "import_hora_inicio", value = format(lubridate::with_tz(ini, Sys.timezone()), "%H:%M"))
          shiny::updateDateInput(session, "import_data_fim", value = as.Date(fim))
          shiny::updateTextInput(session, "import_hora_fim", value = format(lubridate::with_tz(fim, Sys.timezone()), "%H:%M"))
          shiny::showNotification("Horários preenchidos com base no arquivo KML (UTC -> horário local do seu sistema).", type = "message")
        }

        leaflet::leafletProxy("map") %>%
          leaflet::addPolylines(
            lng = coords[, 1],
            lat = coords[, 2],
            color = "darkblue",
            weight = 3,
            group = "importado"
          ) %>%
          leaflet::addCircleMarkers(
            lng = coords[1, 1], lat = coords[1, 2],
            radius = 8, color = "green", fillColor = "green", fillOpacity = 1,
            group = "importado", label = "Início"
          ) %>%
          leaflet::addCircleMarkers(
            lng = coords[nrow(coords), 1], lat = coords[nrow(coords), 2],
            radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
            group = "importado", label = "Fim"
          ) %>%
          leaflet::fitBounds(
            lng1 = min(coords[, 1]), lat1 = min(coords[, 2]),
            lng2 = max(coords[, 1]), lat2 = max(coords[, 2])
          )

        shiny::showNotification("KML enriquecido (FR24) carregado. Verde = início, Vermelho = fim.", type = "message")
        return()
      }
    }

    # Caso normal: lê com sf
    geo <- tryCatch(
      sf::st_read(arquivo, quiet = TRUE),
      error = function(e) NULL
    )
    if (is.null(geo)) {
      shiny::showNotification("Não consegui ler o arquivo como camada geográfica.", type = "error")
      return()
    }

    fr24_enriquecido(FALSE)

    if (!sf::st_is_longlat(geo)) {
      geo <- sf::st_transform(geo, 4326)
    }

    if (any(sf::st_geometry_type(geo) %in% c("MULTILINESTRING"))) {
      geo <- sf::st_cast(geo, "LINESTRING")
    }

    geometria_importada(geo)

    bbox <- sf::st_bbox(geo)

    # Extrai primeiro e último ponto para marcadores de direção
    coords_all <- sf::st_coordinates(geo)
    pt_inicio <- coords_all[1, c("X", "Y")]
    pt_fim <- coords_all[nrow(coords_all), c("X", "Y")]

    leaflet::leafletProxy("map") %>%
      leaflet::addPolylines(
        data = geo,
        color = "purple",
        weight = 3,
        group = "importado"
      ) %>%
      leaflet::addCircleMarkers(
        lng = pt_inicio["X"], lat = pt_inicio["Y"],
        radius = 8, color = "green", fillColor = "green", fillOpacity = 1,
        group = "importado", label = "Início"
      ) %>%
      leaflet::addCircleMarkers(
        lng = pt_fim["X"], lat = pt_fim["Y"],
        radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
        group = "importado", label = "Fim"
      ) %>%
      leaflet::fitBounds(
        lng1 = bbox["xmin"], lat1 = bbox["ymin"],
        lng2 = bbox["xmax"], lat2 = bbox["ymax"]
      )

    shiny::showNotification("Arquivo geográfico importado. Verde = início, Vermelho = fim.", type = "message")
  })

  shiny::observeEvent(input$direcao_arquivo, {
    geo <- geometria_importada()
    if (is.null(geo)) return()

    leaflet::leafletProxy("map") %>% leaflet::clearGroup("importado")

    if (isTRUE(fr24_enriquecido())) {
      coords <- cbind(geo$lon, geo$lat)
      if (input$direcao_arquivo == "inverter") {
        coords <- coords[nrow(coords):1, , drop = FALSE]
      }
      leaflet::leafletProxy("map") %>%
        leaflet::addPolylines(
          lng = coords[, 1],
          lat = coords[, 2],
          color = "darkblue",
          weight = 3,
          group = "importado"
        ) %>%
        leaflet::addCircleMarkers(
          lng = coords[1, 1], lat = coords[1, 2],
          radius = 8, color = "green", fillColor = "green", fillOpacity = 1,
          group = "importado", label = "Início"
        ) %>%
        leaflet::addCircleMarkers(
          lng = coords[nrow(coords), 1], lat = coords[nrow(coords), 2],
          radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
          group = "importado", label = "Fim"
        ) %>%
        leaflet::fitBounds(
          lng1 = min(coords[, 1]), lat1 = min(coords[, 2]),
          lng2 = max(coords[, 1]), lat2 = max(coords[, 2])
        )
    } else {
      bbox <- sf::st_bbox(geo)
      coords_all <- sf::st_coordinates(geo)

      # Considera inversão para marcadores
      if (input$direcao_arquivo == "inverter") {
        pt_inicio <- coords_all[nrow(coords_all), c("X", "Y")]
        pt_fim <- coords_all[1, c("X", "Y")]
      } else {
        pt_inicio <- coords_all[1, c("X", "Y")]
        pt_fim <- coords_all[nrow(coords_all), c("X", "Y")]
      }

      leaflet::leafletProxy("map") %>%
        leaflet::addPolylines(
          data = geo,
          color = "purple",
          weight = 3,
          group = "importado"
        ) %>%
        leaflet::addCircleMarkers(
          lng = pt_inicio["X"], lat = pt_inicio["Y"],
          radius = 8, color = "green", fillColor = "green", fillOpacity = 1,
          group = "importado", label = "Início"
        ) %>%
        leaflet::addCircleMarkers(
          lng = pt_fim["X"], lat = pt_fim["Y"],
          radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
          group = "importado", label = "Fim"
        ) %>%
        leaflet::fitBounds(
          lng1 = bbox["xmin"], lat1 = bbox["ymin"],
          lng2 = bbox["xmax"], lat2 = bbox["ymax"]
        )
    }
  })

  shiny::observeEvent(input$adicionar_import, {
    geo <- geometria_importada()
    if (is.null(geo)) {
      shiny::showNotification("Nenhum arquivo importado.", type = "error")
      return()
    }

    # -------------------------------------------------------------------
    # IMPORTAÇÃO ESPECIAL: trilha de voo FR24 já enriquecida
    # -------------------------------------------------------------------
    if (isTRUE(fr24_enriquecido())) {
      traj <- tryCatch(
        limpar_trajeto_fr24(
          df      = geo,
          ts_col  = "timestamp",
          lon_col = "lon",
          lat_col = "lat"
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Erro ao processar trilha do FR24:", e$message),
            type = "error",
            duration = NULL
          )
          return(NULL)
        }
      )

      if (is.null(traj)) return()

      ts_utc <- traj$ts_utc
      coords <- traj$coords

      distancia_km <- trajeto_distancia_km(coords)

      if (distancia_km <= 0 || distancia_km > 30000) {
        shiny::showNotification(
          sprintf(
            "Distância absurda detectada para o voo (%.1f km). Algo está errado com a trilha.",
            distancia_km
          ),
          type = "error",
          duration = NULL
        )
        return()
      }

      lat1 <- coords[1, "lat"]
      lon1 <- coords[1, "lon"]
      tz   <- tz_from_coords(lat1, lon1)
      sec_gmt <- as.integer(lubridate::with_tz(ts_utc[1], tz) - ts_utc[1])

      n_samples <- length(ts_utc)

      altitude_vec <- if ("altitude_m" %in% names(geo)) {
        alt <- geo$altitude_m
        alt_ts <- parse_timestamp_utc(geo$timestamp)
        ok <- !is.na(alt_ts) & !is.na(alt)
        if (any(ok)) {
          ord <- order(alt_ts[ok])
          alt[ok][ord]
        } else {
          NULL
        }
      } else {
        NULL
      }

      speed_vec <- if ("speed_mps" %in% names(geo)) {
        spd <- geo$speed_mps
        spd_ts <- parse_timestamp_utc(geo$timestamp)
        ok <- !is.na(spd_ts) & !is.na(spd)
        if (any(ok)) {
          ord <- order(spd_ts[ok])
          spd[ok][ord]
        } else {
          NULL
        }
      } else {
        NULL
      }

      heading_vec <- if ("heading" %in% names(geo)) {
        hdg <- geo$heading
        hdg_ts <- parse_timestamp_utc(geo$timestamp)
        ok <- !is.na(hdg_ts) & !is.na(hdg)
        if (any(ok)) {
          ord <- order(hdg_ts[ok])
          hdg[ok][ord]
        } else {
          NULL
        }
      } else {
        NULL
      }

      samples_novos <- criar_locomotion_samples(
        coords          = coords,
        timestamps_utc  = ts_utc,
        accuracy        = 5,
        altitude        = altitude_vec,
        speed           = speed_vec,
        heading         = heading_vec,
        force_single_tz = TRUE,
        moving_state    = "moving"
      )

      sample_ids <- vapply(samples_novos, `[[`, "", "sampleId")

      item <- criar_timeline_item_path(
        timestamps_utc = ts_utc,
        coords         = coords,
        sample_ids     = sample_ids,
        tipo           = "voo_fr24",
        descricao      = sprintf("Voo FR24 (%.1f km)", distancia_km),
        activity_type  = "airplane"
      )

      tl <- timeline()
      tl[[length(tl) + 1L]] <- item
      timeline(tl)

      s <- all_samples()
      all_samples(c(s, samples_novos))

      leaflet::leafletProxy("map") %>%
        leaflet::addPolylines(
          lng   = coords[, "lon"],
          lat   = coords[, "lat"],
          color = "purple",
          weight = 4,
          group = "rota_atual"
        ) %>%
        leaflet::fitBounds(
          lng1 = min(coords[, "lon"], na.rm = TRUE),
          lat1 = min(coords[, "lat"], na.rm = TRUE),
          lng2 = max(coords[, "lon"], na.rm = TRUE),
          lat2 = max(coords[, "lat"], na.rm = TRUE)
        )

      shiny::showNotification("Voo FR24 adicionado à timeline.", type = "message")
      return()
    }


    # Caso padrão (sf): timestamps vêm do formulário (horário local)
    if (!inherits(geo, "sf")) {
      shiny::showNotification("Geometria importada inválida.", type = "error")
      return()
    }

    coords_list <- sf::st_coordinates(geo)
    if (nrow(coords_list) < 2) {
      shiny::showNotification("Geometria muito curta para rota.", type = "error")
      return()
    }

    if (input$direcao_arquivo == "inverter") {
      coords_list <- coords_list[nrow(coords_list):1, , drop = FALSE]
    }

    lat1 <- coords_list[1, "Y"]
    lng1 <- coords_list[1, "X"]
    tz_origem <- tz_from_coords(lat1, lng1)

    latn <- coords_list[nrow(coords_list), "Y"]
    lngn <- coords_list[nrow(coords_list), "X"]
    tz_destino <- tz_from_coords(latn, lngn)
    if (is.na(tz_destino)) tz_destino <- tz_origem

    val <- validar_intervalo(
      data_ini = input$import_data_inicio,
      hora_ini = input$import_hora_inicio,
      data_fim = input$import_data_fim,
      hora_fim = input$import_hora_fim,
      tz = tz_origem
    )
    if (is.null(val)) {
      shiny::showNotification("Horário inválido para rota importada.", type = "error")
      return()
    }

    inicio_utc <- lubridate::with_tz(val$inicio, "UTC")
    fim_utc    <- lubridate::with_tz(val$fim,    "UTC")

    if (has_overlap(timeline(), inicio_utc, fim_utc)) {
      shiny::showNotification("Este intervalo se sobrepõe a outra atividade existente.", type = "error")
      return()
    }

    n0 <- nrow(coords_list)
    n_target <- max(n0, 100L)

    ts_seq <- seq(inicio_utc, fim_utc, length.out = n_target)

    t0 <- seq(0, 1, length.out = n0)
    t_new <- seq(0, 1, length.out = n_target)

    lon_new <- approx(t0, coords_list[, "X"], xout = t_new)$y
    lat_new <- approx(t0, coords_list[, "Y"], xout = t_new)$y

    coords_new <- cbind(lon_new, lat_new)

    samples_novos <- criar_locomotion_samples(
      coords         = coords_new,
      timestamps_utc = ts_seq,
      accuracy       = 0.01,
      force_single_tz = FALSE,
      moving_state   = "moving"
    )
    sample_ids <- vapply(samples_novos, `[[`, "", "sampleId")

    item <- criar_timeline_item_path(
      timestamps_utc = ts_seq,
      coords = coords_new,
      sample_ids = sample_ids,
      tipo = "rota_importada",
      descricao = "Rota importada de arquivo"
    )

    tl <- timeline()
    tl[[length(tl) + 1L]] <- item
    timeline(tl)

    s <- all_samples()
    all_samples(c(s, samples_novos))

    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("rota_atual") %>%
      leaflet::addPolylines(
        lng = coords_new[, 1],
        lat = coords_new[, 2],
        color = "purple",
        weight = 3,
        group = "rota_atual"
      ) %>%
      leaflet::fitBounds(
        lng1 = min(coords_new[, 1]), lat1 = min(coords_new[, 2]),
        lng2 = max(coords_new[, 1]), lat2 = max(coords_new[, 2])
      )

    shiny::showNotification("Rota do arquivo adicionada.", type = "message")
  })

  # ---- Timeline UI ----

  output$timeline_list <- shiny::renderUI({
    tl <- timeline()
    if (length(tl) == 0) {
      return(shiny::tags$p("Nenhum item na timeline ainda."))
    }

    ord <- order(vapply(tl, function(it) it$startDate$date, character(1)))
    tl_ord <- tl[ord]

    shiny::tagList(
      lapply(seq_along(tl_ord), function(i) {
        it   <- tl_ord[[i]]
        tipo <- it$tipo %||% if (isTRUE(it$.isVisit)) "visita" else "rota"
        desc <- it$descricao %||% tipo

        ts_ini_utc <- parse_timestamp_utc(it$startDate$date)
        sec_ini    <- it$startDate$secondsFromGMT %||% 0
        ts_ini_loc <- ts_ini_utc + sec_ini

        ts_fim_utc <- parse_timestamp_utc(it$endDate$date)
        sec_fim    <- it$endDate$secondsFromGMT %||% sec_ini
        ts_fim_loc <- ts_fim_utc + sec_fim

        ini_str <- format(ts_ini_loc, "%Y-%m-%d %H:%M")
        fim_str <- format(ts_fim_loc, "%Y-%m-%d %H:%M")

        shiny::div(
          class = "card mb-2 p-2",
          shiny::strong(sprintf("[%s] %s", tipo, desc)),
          shiny::br(),
          shiny::tags$small(paste0(ini_str, " -> ", fim_str)),
          shiny::br(),
          shiny::actionButton(
            paste0("editar_", it$.internalId),
            "Editar",
            class = "btn-sm btn-warning"
          ),
          shiny::actionButton(
            paste0("deletar_", it$.internalId),
            "Excluir",
            class = "btn-sm btn-danger"
          )
        )
      })
    )

  })

  # ---- Editar / deletar items ----

  # Deletar
  shiny::observe({
    tl <- timeline()
    if (length(tl) == 0) return()

    ids <- vapply(tl, `[[`, "", ".internalId")

    lapply(ids, function(id) {
      shiny::observeEvent(input[[paste0("deletar_", id)]], {
        tl_atual <- timeline()
        idx <- which(vapply(tl_atual, `[[`, "", ".internalId") == id)
        if (length(idx)) {
          it <- tl_atual[[idx]]
          tl_atual <- tl_atual[-idx]

          if (!is.null(it$samples)) {
            s <- all_samples()
            keep <- !vapply(s, function(x) x$sampleId %in% it$samples, logical(1))
            all_samples(s[keep])
          }

          timeline(tl_atual)

          leaflet::leafletProxy("map") %>%
            leaflet::clearGroup("rota_atual") %>%
            leaflet::clearGroup("importado") %>%
            leaflet::clearGroup("waypoints")

          pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))

          shiny::showNotification("Item removido da timeline.", type = "message")
        }
      }, ignoreInit = TRUE)

    })
  })

  # Editar
  shiny::observe({
    tl <- timeline()
    if (length(tl) == 0) return()

    ids <- vapply(tl, `[[`, "", ".internalId")

    lapply(ids, function(id) {
      shiny::observeEvent(input[[paste0("editar_", id)]], {
        tl_atual <- timeline()
        idx <- which(vapply(tl_atual, `[[`, "", ".internalId") == id)
        if (!length(idx)) return()
        it <- tl_atual[[idx]]

        ts_utc <- parse_timestamp_utc(it$startDate$date)
        sec <- it$startDate$secondsFromGMT %||% 0
        ts_local <- ts_utc + sec

        tsf_utc <- parse_timestamp_utc(it$endDate$date)
        secf <- it$endDate$secondsFromGMT %||% sec
        tsf_local <- tsf_utc + secf

        shiny::showModal(
          shiny::modalDialog(
            title = "Editar item",
            shiny::dateInput("edit_data_inicio", "Data início", as.Date(ts_local)),
            shiny::textInput("edit_hora_inicio", "Hora início", format(ts_local, "%H:%M")),
            shiny::dateInput("edit_data_fim", "Data fim", as.Date(tsf_local)),
            shiny::textInput("edit_hora_fim", "Hora fim", format(tsf_local, "%H:%M")),
            footer = shiny::tagList(
              shiny::modalButton("Cancelar"),
              shiny::actionButton("salvar_edicao", "Salvar", class = "btn-primary")
            )
          )
        )

        session$userData$item_editando <- id
      }, ignoreInit = TRUE)
    })
  })

  shiny::observeEvent(input$salvar_edicao, {
    shiny::removeModal()
    id <- session$userData$item_editando
    if (is.null(id)) return()

    tl <- timeline()
    idx <- which(vapply(tl, `[[`, "", ".internalId") == id)
    if (!length(idx)) return()
    it <- tl[[idx]]

    start_sec <- it$startDate$secondsFromGMT %||% 0
    end_sec   <- it$endDate$secondsFromGMT %||% start_sec

    hora_ini_fmt <- formatar_hora(input$edit_hora_inicio)
    hora_fim_fmt <- formatar_hora(input$edit_hora_fim)

    local_ini <- lubridate::ymd_hms(paste(input$edit_data_inicio, hora_ini_fmt), tz = "UTC")
    local_fim <- lubridate::ymd_hms(paste(input$edit_data_fim, hora_fim_fmt), tz = "UTC")

    if (is.na(local_ini) || is.na(local_fim)) {
      shiny::showNotification("Horários inválidos.", type = "error")
      return()
    }

    novo_ini_utc <- local_ini - start_sec
    novo_fim_utc <- local_fim  - end_sec

    if (novo_fim_utc <= novo_ini_utc) {
      shiny::showNotification("O fim deve ser depois do início.", type = "error")
      return()
    }

    if (has_overlap(tl, novo_ini_utc, novo_fim_utc, ignore_id = id)) {
      shiny::showNotification("Este intervalo se sobrepõe a outra atividade existente.", type = "error")
      return()
    }


    it$startDate$date <- format(novo_ini_utc, "%Y-%m-%dT%H:%M:%SZ")
    it$endDate$date   <- format(novo_fim_utc, "%Y-%m-%dT%H:%M:%SZ")

    if (!is.null(it$samples) && length(it$samples)) {
      s <- all_samples()
      sample_ids <- it$samples
      n <- length(sample_ids)
      novos_ts <- seq(novo_ini_utc, novo_fim_utc, length.out = n)

      for (i in seq_len(n)) {
        sid <- sample_ids[i]
        idx_s <- which(vapply(s, `[[`, "", "sampleId") == sid)
        if (length(idx_s)) {
          ts_utc <- novos_ts[i]
          lat <- s[[idx_s]]$location$latitude
          lon <- s[[idx_s]]$location$longitude
          tz <- tz_from_coords(lat, lon)
          if (is.na(tz)) tz <- "UTC"
          sec <- seconds_from_gmt(ts_utc, tz)

          s[[idx_s]]$date <- format(ts_utc, "%Y-%m-%dT%H:%M:%SZ")
          s[[idx_s]]$location$timestamp <- s[[idx_s]]$date
          s[[idx_s]]$secondsFromGMT <- sec
        }
      }
      all_samples(s)
    }

    tl[[idx]] <- it
    timeline(tl)
    shiny::showNotification("Item atualizado.", type = "message")
  })

  # ---- Modo Editar Samples ----

  # Helper: renderiza samples no mapa como marcadores arrastáveis
  renderizar_edit_samples <- function() {
    samples <- edit_samples()
    virtuais <- samples_virtuais()
    ignored <- ignored_samples()
    proxy <- leaflet::leafletProxy("map")
    proxy <- proxy %>%
      leaflet::clearGroup("edit_samples") %>%
      leaflet::clearGroup("edit_virtuais") %>%
      leaflet::clearGroup("edit_rota")

    if (length(samples) == 0) return()

    # Filtra samples com location válida
    samples <- Filter(function(s) {
      !is.null(s$location) &&
      !is.null(s$location$latitude) &&
      !is.null(s$location$longitude) &&
      length(s$location$latitude) > 0 &&
      length(s$location$longitude) > 0
    }, samples)

    if (length(samples) == 0) return()

    # Extrai coordenadas
    lats <- vapply(samples, function(s) as.numeric(s$location$latitude)[1], numeric(1))
    lngs <- vapply(samples, function(s) as.numeric(s$location$longitude)[1], numeric(1))
    ids <- vapply(samples, function(s) {
      id <- s$sampleId
      if (is.null(id) || length(id) == 0) return(paste0("unknown_", runif(1)))
      as.character(id)[1]
    }, character(1))

    # Verifica se há rota OSRM armazenada
    rota <- rota_osrm()
    if (!is.null(rota) && is.matrix(rota) && nrow(rota) >= 2) {
      # Desenha a rota OSRM completa (verde)
      proxy <- proxy %>%
        leaflet::addPolylines(
          lng = rota[, 1],  # lon
          lat = rota[, 2],  # lat
          color = "#28a745",
          weight = 4,
          opacity = 0.8,
          group = "edit_rota"
        )
    } else {
      # Sem rota OSRM: desenha linha conectando os samples
      proxy <- proxy %>%
        leaflet::addPolylines(
          lng = lngs,
          lat = lats,
          color = "blue",
          weight = 2,
          opacity = 0.6,
          group = "edit_rota"
        )
    }

    # ---- SAMPLES VIRTUAIS (pontos laranjas menores) ----
    if (length(virtuais) > 0) {
      # Filtra virtuais com location válida
      virtuais <- Filter(function(s) {
        !is.null(s$location) &&
        !is.null(s$location$latitude) &&
        !is.null(s$location$longitude) &&
        length(s$location$latitude) > 0 &&
        length(s$location$longitude) > 0
      }, virtuais)
    }
    if (length(virtuais) > 0) {
      lats_v <- vapply(virtuais, function(s) as.numeric(s$location$latitude)[1], numeric(1))
      lngs_v <- vapply(virtuais, function(s) as.numeric(s$location$longitude)[1], numeric(1))

      proxy <- proxy %>%
        leaflet::addCircleMarkers(
          lng = lngs_v,
          lat = lats_v,
          radius = 4,
          color = "#ff8c00",  # laranja
          fillColor = "#ff8c00",
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "edit_virtuais"
        )
    }

    # ---- SAMPLES ARC (alfinetes arrastáveis) ----
    for (i in seq_along(samples)) {
      is_ignored <- ids[i] %in% ignored
      proxy <- proxy %>%
        leaflet::addMarkers(
          lng = lngs[i],
          lat = lats[i],
          layerId = ids[i],
          group = "edit_samples",
          options = leaflet::markerOptions(draggable = TRUE)
        )
    }

    # Zoom para os samples (inclui virtuais se existirem)
    all_lats <- lats
    all_lngs <- lngs
    if (length(virtuais) > 0) {
      all_lats <- c(all_lats, vapply(virtuais, function(s) as.numeric(s$location$latitude)[1], numeric(1)))
      all_lngs <- c(all_lngs, vapply(virtuais, function(s) as.numeric(s$location$longitude)[1], numeric(1)))
    }

    proxy %>%
      leaflet::fitBounds(
        lng1 = min(all_lngs), lat1 = min(all_lats),
        lng2 = max(all_lngs), lat2 = max(all_lats)
      )

    # Adiciona handler de clique direito via JS
    session$sendCustomMessage("setup_context_menu", list())
  }

  # Carregar samples existentes
  shiny::observeEvent(input$carregar_samples, {
    hora_ini_fmt <- formatar_hora(input$edit_samples_hora_inicio)
    hora_fim_fmt <- formatar_hora(input$edit_samples_hora_fim)

    if (is.na(hora_ini_fmt) || is.na(hora_fim_fmt)) {
      shiny::showNotification("Horários inválidos.", type = "error")
      return()
    }

    # Converte para UTC (assume timezone local do sistema)
    inicio_local <- lubridate::ymd_hms(
      paste(input$edit_samples_data_inicio, hora_ini_fmt),
      tz = Sys.timezone()
    )
    fim_local <- lubridate::ymd_hms(
      paste(input$edit_samples_data_fim, hora_fim_fmt),
      tz = Sys.timezone()
    )

    if (is.na(inicio_local) || is.na(fim_local)) {
      shiny::showNotification("Data/hora inválida.", type = "error")
      return()
    }

    inicio_utc <- lubridate::with_tz(inicio_local, "UTC")
    fim_utc <- lubridate::with_tz(fim_local, "UTC")

    samples_raw <- carregar_samples_intervalo(inicio_utc, fim_utc)

    if (length(samples_raw) == 0) {
      shiny::showNotification("Nenhum sample encontrado no intervalo.", type = "warning")
      return()
    }

    # Filtra samples com location válida
    samples <- Filter(function(s) {
      !is.null(s$location) &&
      !is.null(s$location$latitude) &&
      !is.null(s$location$longitude) &&
      length(s$location$latitude) > 0 &&
      length(s$location$longitude) > 0
    }, samples_raw)

    n_invalidos <- length(samples_raw) - length(samples)
    if (n_invalidos > 0) {
      shiny::showNotification(
        sprintf("%d samples ignorados (sem coordenadas válidas).", n_invalidos),
        type = "warning"
      )
    }

    if (length(samples) == 0) {
      shiny::showNotification("Nenhum sample com coordenadas válidas.", type = "error")
      return()
    }

    original_samples(samples)  # Guarda cópia dos samples originais para exportação
    # Guarda timelineItemId original (se existir)
    if (length(samples) > 0 && !is.null(samples[[1]]$timelineItemId)) {
      original_timeline_item_id(samples[[1]]$timelineItemId)
    } else {
      original_timeline_item_id(NULL)
    }
    edit_samples(samples)
    rota_osrm(NULL)  # Limpa rota OSRM ao carregar novos samples
    samples_virtuais(list())  # Limpa samples virtuais
    renderizar_edit_samples()

    shiny::showNotification(
      sprintf("%d samples carregados.", length(samples)),
      type = "message"
    )
  })

  # Captura arraste de marcador
  shiny::observeEvent(input$map_marker_dragend, {
    event <- input$map_marker_dragend
    if (is.null(event)) return()

    sample_id <- event$id
    new_lat <- event$lat
    new_lng <- event$lng

    samples <- edit_samples()
    if (length(samples) == 0) return()

    # Encontra o sample pelo ID
    idx <- which(vapply(samples, function(s) {
      id <- s$sampleId
      if (is.null(id) || length(id) == 0) return("")
      as.character(id)[1]
    }, character(1)) == sample_id)
    if (length(idx) == 0) return()

    # Atualiza coordenadas
    samples[[idx]]$location$latitude <- new_lat
    samples[[idx]]$location$longitude <- new_lng

    edit_samples(samples)
    rota_osrm(NULL)  # Limpa rota OSRM ao mover marcador manualmente
    samples_virtuais(list())  # Limpa samples virtuais

    # Redesenha a linha (sem redesenhar os marcadores para manter drag)
    # Filtra samples com location válida
    samples_validos <- Filter(function(s) {
      !is.null(s$location) &&
      !is.null(s$location$latitude) &&
      !is.null(s$location$longitude) &&
      length(s$location$latitude) > 0 &&
      length(s$location$longitude) > 0
    }, samples)
    if (length(samples_validos) == 0) return()

    lats <- vapply(samples_validos, function(s) as.numeric(s$location$latitude)[1], numeric(1))
    lngs <- vapply(samples_validos, function(s) as.numeric(s$location$longitude)[1], numeric(1))

    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("edit_rota") %>%
      leaflet::clearGroup("edit_virtuais") %>%
      leaflet::addPolylines(
        lng = lngs,
        lat = lats,
        color = "blue",
        weight = 2,
        opacity = 0.6,
        group = "edit_rota"
      )
  })

  # Snap-to-road OSRM
  shiny::observeEvent(input$snap_to_road, {
    # Remove notificações anteriores
    shiny::removeNotification("snap_progress")

    tryCatch({
      shiny::showNotification("1/7 Iniciando...", id = "snap_progress", duration = NULL, type = "message")

      samples <- edit_samples()
      ignored <- ignored_samples()

      if (length(samples) == 0) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification("Nenhum sample carregado. Clique em 'Carregar' primeiro.", type = "error")
        return()
      }

      shiny::showNotification(sprintf("2/7 Filtrando %d samples...", length(samples)), id = "snap_progress", duration = NULL, type = "message")

      # Filtra samples ignorados para o snap-to-road
      ids <- character(length(samples))
      for (j in seq_along(samples)) {
        val <- samples[[j]]$sampleId
        if (is.null(val)) val <- paste0("sample_", j)
        if (is.list(val)) val <- unlist(val)[1]
        ids[j] <- as.character(val)
      }
      samples_ativos <- samples[!ids %in% ignored]

      if (length(samples_ativos) < 2) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification("Carregue pelo menos 2 samples (nao ignorados) para snap-to-road.", type = "error")
        return()
      }

      perfil <- input$edit_osrm_perfil
      base_url <- OSRM_SERVERS[[perfil]]

      if (is.null(base_url)) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification(sprintf("Perfil OSRM '%s' nao configurado.", perfil), type = "error")
        return()
      }

      shiny::showNotification("3/7 Verificando OSRM...", id = "snap_progress", duration = NULL, type = "message")

      if (!check_osrm_server(base_url)) {
        shiny::removeNotification("snap_progress")
        mostrar_modal_osrm_offline(base_url)
        return()
      }

      shiny::showNotification("4/7 Extraindo coordenadas...", id = "snap_progress", duration = NULL, type = "message")

      # Extrai coordenadas dos samples ativos (com conversão defensiva)
      n_ativos <- length(samples_ativos)
      lats <- numeric(n_ativos)
      lngs <- numeric(n_ativos)
      for (j in seq_len(n_ativos)) {
        lat_val <- samples_ativos[[j]]$location$latitude
        lng_val <- samples_ativos[[j]]$location$longitude
        if (is.list(lat_val)) lat_val <- unlist(lat_val)
        if (is.list(lng_val)) lng_val <- unlist(lng_val)
        lats[j] <- as.numeric(lat_val)[1]
        lngs[j] <- as.numeric(lng_val)[1]
      }

      # Limita a 100 waypoints (chunking se necessário)
      n <- length(samples_ativos)
      if (n > 100) {
        shiny::showNotification(
          sprintf("Muitos samples (%d). Usando apenas os primeiros 100 para OSRM.", n),
          type = "warning"
        )
        lats <- lats[1:100]
        lngs <- lngs[1:100]
        samples_ativos <- samples_ativos[1:100]
        n <- 100
      }

      shiny::showNotification(sprintf("5/7 Chamando OSRM com %d pontos...", n), id = "snap_progress", duration = NULL, type = "message")

      # Chama OSRM
      pts <- data.frame(lat = lats, lng = lngs)
      rota <- calcular_rota_osrm(pts, perfil = perfil)

      if (is.null(rota)) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification(
          "Falha ao calcular rota OSRM. O servidor pode nao ter conseguido rotear entre os pontos.",
          type = "error"
        )
        return()
      }

      if (is.null(rota$coords) || nrow(rota$coords) < 2) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification("Rota OSRM retornou sem coordenadas validas.", type = "error")
        return()
      }

      shiny::showNotification("6/7 Interpolando samples...", id = "snap_progress", duration = NULL, type = "message")

      coords_osrm <- rota$coords

      # Converte para matriz numérica de forma defensiva
      if (is.data.frame(coords_osrm)) {
        coords_osrm <- as.matrix(coords_osrm)
      }
      if (is.list(coords_osrm) && !is.matrix(coords_osrm)) {
        # Se ainda é lista, converte manualmente
        coords_osrm <- do.call(rbind, lapply(coords_osrm, function(x) as.numeric(unlist(x))))
      }
      # Garante que é matriz numérica
      storage.mode(coords_osrm) <- "double"

      # Calcula distância acumulada na rota OSRM
      n_osrm <- nrow(coords_osrm)
      if (is.null(n_osrm) || n_osrm < 2) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification("Rota OSRM muito curta ou invalida.", type = "error")
        return()
      }

      # Armazena a geometria da rota OSRM para exibição
      rota_osrm(coords_osrm)

      # Filtra pontos com mudança de direção (>2 graus) ou distância significativa
      # Critérios: mudança angular OU distância > 50m do último ponto incluído
      pontos_relevantes <- 1L  # sempre inclui o primeiro
      ultimo_incluido <- 1L

      for (i in 2:(n_osrm - 1)) {
        incluir <- FALSE

        # Critério 1: mudança de direção > 2 graus
        b1 <- geosphere::bearing(
          c(coords_osrm[i - 1, 1], coords_osrm[i - 1, 2]),
          c(coords_osrm[i, 1], coords_osrm[i, 2])
        )
        b2 <- geosphere::bearing(
          c(coords_osrm[i, 1], coords_osrm[i, 2]),
          c(coords_osrm[i + 1, 1], coords_osrm[i + 1, 2])
        )
        diff_ang <- abs((b2 - b1 + 180) %% 360 - 180)
        if (diff_ang > 2) {
          incluir <- TRUE
        }

        # Critério 2: distância > 50m do último ponto incluído
        if (!incluir) {
          dist_ultimo <- geosphere::distHaversine(
            c(coords_osrm[ultimo_incluido, 1], coords_osrm[ultimo_incluido, 2]),
            c(coords_osrm[i, 1], coords_osrm[i, 2])
          )
          if (dist_ultimo > 50) {
            incluir <- TRUE
          }
        }

        if (incluir) {
          pontos_relevantes <- c(pontos_relevantes, i)
          ultimo_incluido <- i
        }
      }
      pontos_relevantes <- c(pontos_relevantes, n_osrm)  # sempre inclui o último

      # Coordenadas filtradas para samples virtuais
      coords_filtradas <- coords_osrm[pontos_relevantes, , drop = FALSE]
      n_filtrados <- nrow(coords_filtradas)

      dist_acum <- numeric(n_osrm)
      dist_acum[1] <- 0
      for (i in 2:n_osrm) {
        p1 <- c(coords_osrm[i - 1, 1], coords_osrm[i - 1, 2])
        p2 <- c(coords_osrm[i, 1], coords_osrm[i, 2])
        dist_acum[i] <- dist_acum[i - 1] + geosphere::distHaversine(p1, p2)
      }
      dist_total <- dist_acum[n_osrm]

      # Para cada sample ativo, interpola posição na rota OSRM
      # usando proporção temporal - extrair timestamps com for loop
      ts_vec <- numeric(n)
      for (j in seq_len(n)) {
        date_val <- samples_ativos[[j]]$date
        if (is.list(date_val)) date_val <- unlist(date_val)[1]
        ts_vec[j] <- as.numeric(as.POSIXct(as.character(date_val), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
      }

      t_min <- min(ts_vec)
      t_max <- max(ts_vec)
      t_range <- t_max - t_min
      if (t_range <= 0) t_range <- 1

      for (i in seq_len(n)) {
        # Proporção temporal (0 a 1)
        prop <- (ts_vec[i] - t_min) / t_range

        # Distância alvo na rota OSRM
        dist_alvo <- prop * dist_total

        # Encontra segmento na rota OSRM
        seg_idx <- max(1, sum(dist_acum <= dist_alvo))
        if (seg_idx >= n_osrm) seg_idx <- n_osrm - 1

        # Interpola dentro do segmento
        d1 <- dist_acum[seg_idx]
        d2 <- dist_acum[seg_idx + 1]
        seg_len <- d2 - d1
        if (seg_len <= 0) seg_len <- 1

        frac <- (dist_alvo - d1) / seg_len
        frac <- max(0, min(1, frac))

        new_lon <- coords_osrm[seg_idx, 1] + frac * (coords_osrm[seg_idx + 1, 1] - coords_osrm[seg_idx, 1])
        new_lat <- coords_osrm[seg_idx, 2] + frac * (coords_osrm[seg_idx + 1, 2] - coords_osrm[seg_idx, 2])

        samples_ativos[[i]]$location$longitude <- new_lon
        samples_ativos[[i]]$location$latitude <- new_lat
      }

      shiny::showNotification("7/7 Finalizando...", id = "snap_progress", duration = NULL, type = "message")

      # Recalcula bearings dos samples ativos - usa for loop
      coords_new <- matrix(0, nrow = length(samples_ativos), ncol = 2)
      for (j in seq_along(samples_ativos)) {
        coords_new[j, 1] <- as.numeric(samples_ativos[[j]]$location$longitude)
        coords_new[j, 2] <- as.numeric(samples_ativos[[j]]$location$latitude)
      }
      bearings <- calcular_bearings(coords_new)

      for (i in seq_along(samples_ativos)) {
        samples_ativos[[i]]$location$course <- bearings[i]
      }

      # Atualiza samples_ativos de volta na lista completa
      ids_ativos <- character(length(samples_ativos))
      for (j in seq_along(samples_ativos)) {
        val <- samples_ativos[[j]]$sampleId
        if (is.null(val)) val <- paste0("sample_", j)
        if (is.list(val)) val <- unlist(val)[1]
        ids_ativos[j] <- as.character(val)
      }

      for (i in seq_along(samples)) {
        sid <- samples[[i]]$sampleId
        if (is.null(sid)) next
        if (is.list(sid)) sid <- unlist(sid)[1]
        sid <- as.character(sid)
        idx_ativo <- which(ids_ativos == sid)
        if (length(idx_ativo) == 1) {
          samples[[i]] <- samples_ativos[[idx_ativo]]
        }
      }

      # ---- CRIA SAMPLES VIRTUAIS da rota OSRM ----
      # Calcula distância acumulada para os pontos filtrados
      dist_acum_filtrados <- numeric(n_filtrados)
      dist_acum_filtrados[1] <- 0
      for (i in 2:n_filtrados) {
        # Usa índices dos pontos originais para calcular distância
        idx_orig <- pontos_relevantes[i]
        dist_acum_filtrados[i] <- dist_acum[idx_orig]
      }

      # Pega o timelineItemId do primeiro sample (para os virtuais)
      ti_id_virtual <- NULL
      if (length(samples_ativos) > 0 && !is.null(samples_ativos[[1]]$timelineItemId)) {
        ti_id_virtual <- samples_ativos[[1]]$timelineItemId
      }

      # Cria samples virtuais interpolando timestamps
      novos_virtuais <- list()
      for (i in seq_len(n_filtrados)) {
        # Proporção baseada na distância
        prop <- if (dist_total > 0) dist_acum_filtrados[i] / dist_total else 0

        # Interpola timestamp
        ts_virtual <- t_min + prop * t_range
        ts_utc <- as.POSIXct(ts_virtual, origin = "1970-01-01", tz = "UTC")
        date_str <- format(ts_utc, "%Y-%m-%dT%H:%M:%SZ")

        # Coordenadas
        lon_v <- coords_filtradas[i, 1]
        lat_v <- coords_filtradas[i, 2]

        # Calcula bearing para o próximo ponto
        course_v <- 0
        if (i < n_filtrados) {
          course_v <- geosphere::bearing(
            c(coords_filtradas[i, 1], coords_filtradas[i, 2]),
            c(coords_filtradas[i + 1, 1], coords_filtradas[i + 1, 2])
          )
          if (is.na(course_v)) course_v <- 0
          if (course_v < 0) course_v <- course_v + 360
        } else if (i > 1) {
          # Último ponto: usa bearing do anterior
          course_v <- geosphere::bearing(
            c(coords_filtradas[i - 1, 1], coords_filtradas[i - 1, 2]),
            c(coords_filtradas[i, 1], coords_filtradas[i, 2])
          )
          if (is.na(course_v)) course_v <- 0
          if (course_v < 0) course_v <- course_v + 360
        }

        # Timezone
        tz_v <- tz_from_coords(lat_v, lon_v)
        if (is.na(tz_v)) tz_v <- "UTC"
        sec_gmt_v <- seconds_from_gmt(ts_utc, tz_v)

        # Cria sample virtual
        sample_v <- list(
          sampleId = uuid::UUIDgenerate(use.time = TRUE),
          date = date_str,
          secondsFromGMT = sec_gmt_v,
          lastSaved = date_str,
          movingState = "moving",
          recordingState = "recording",
          stepHz = 0,
          courseVariance = 1,
          xyAcceleration = 0.0,
          zAcceleration = 0.0,
          location = list(
            timestamp = date_str,
            latitude = lat_v,
            longitude = lon_v,
            altitude = 0,
            horizontalAccuracy = 0.01,
            verticalAccuracy = 10,
            speed = 0,
            course = course_v
          ),
          .virtual = TRUE  # Marca como virtual para diferenciar
        )

        if (!is.null(ti_id_virtual)) {
          sample_v$timelineItemId <- ti_id_virtual
        }

        novos_virtuais[[length(novos_virtuais) + 1L]] <- sample_v
      }

      # Armazena samples virtuais
      samples_virtuais(novos_virtuais)

      edit_samples(samples)
      renderizar_edit_samples()

      shiny::removeNotification("snap_progress")
      shiny::showNotification(
        sprintf("Snap-to-road concluido! %d samples Arc + %d virtuais (%.1f km).",
                n, length(novos_virtuais), dist_total / 1000),
        type = "message"
      )
    }, error = function(e) {
      shiny::removeNotification("snap_progress")
      shiny::showNotification(
        paste("Erro no snap-to-road:", e$message),
        type = "error",
        duration = NULL
      )
    })
  })

  # ---- Handlers do menu de contexto ----

  # Propriedades: mostra modal com informações do sample
  shiny::observeEvent(input$ctx_propriedades, {
    req(input$ctx_propriedades)
    sample_id <- input$ctx_propriedades$id
    samples <- edit_samples()

    idx <- which(vapply(samples, function(s) s$sampleId, character(1)) == sample_id)
    if (length(idx) == 0) return()

    s <- samples[[idx]]
    ts_utc <- as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ts_local <- ts_utc + (s$secondsFromGMT %||% 0)

    lat <- s$location$latitude
    lon <- s$location$longitude
    speed <- s$location$speed %||% NA
    course <- s$location$course %||% NA
    alt <- s$location$altitude %||% NA

    shiny::showModal(
      shiny::modalDialog(
        title = "Propriedades do Sample",
        shiny::tags$table(
          class = "table table-condensed",
          shiny::tags$tr(shiny::tags$td(shiny::strong("ID:")), shiny::tags$td(sample_id)),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Data/Hora (UTC):")), shiny::tags$td(format(ts_utc, "%Y-%m-%d %H:%M:%S"))),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Data/Hora (Local):")), shiny::tags$td(format(ts_local, "%Y-%m-%d %H:%M:%S"))),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Latitude:")), shiny::tags$td(sprintf("%.8f", lat))),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Longitude:")), shiny::tags$td(sprintf("%.8f", lon))),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Velocidade:")), shiny::tags$td(if (is.na(speed)) "-" else sprintf("%.1f km/h", speed * 3.6))),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Curso:")), shiny::tags$td(if (is.na(course)) "-" else sprintf("%.1f graus", course))),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Altitude:")), shiny::tags$td(if (is.na(alt)) "-" else sprintf("%.1f m", alt)))
        ),
        easyClose = TRUE,
        footer = shiny::modalButton("Fechar")
      )
    )
  })

  # Ignorar: toggle sample ignorado
  shiny::observeEvent(input$ctx_ignorar, {
    req(input$ctx_ignorar)
    sample_id <- input$ctx_ignorar$id
    ignored <- ignored_samples()

    if (sample_id %in% ignored) {
      # Remove da lista de ignorados
      ignored <- ignored[ignored != sample_id]
      shiny::showNotification("Sample incluido novamente.", type = "message")
    } else {
      # Adiciona à lista de ignorados
      ignored <- c(ignored, sample_id)
      shiny::showNotification("Sample ignorado.", type = "message")
    }

    ignored_samples(ignored)
    renderizar_edit_samples()
  })

  # Descartar: remove sample da lista
  shiny::observeEvent(input$ctx_descartar, {
    req(input$ctx_descartar)
    sample_id <- input$ctx_descartar$id
    samples <- edit_samples()

    idx <- which(vapply(samples, function(s) s$sampleId, character(1)) == sample_id)
    if (length(idx) == 0) return()

    samples <- samples[-idx]
    edit_samples(samples)

    # Remove também da lista de ignorados se estiver
    ignored <- ignored_samples()
    ignored <- ignored[ignored != sample_id]
    ignored_samples(ignored)

    renderizar_edit_samples()
    shiny::showNotification("Sample descartado.", type = "message")
  })

  # Inserir: cria novo sample após o selecionado
  shiny::observeEvent(input$ctx_inserir, {
    req(input$ctx_inserir)
    sample_id <- input$ctx_inserir$id
    samples <- edit_samples()

    idx <- which(vapply(samples, function(s) s$sampleId, character(1)) == sample_id)
    if (length(idx) == 0) return()

    # Se for o último sample, não pode inserir após
    if (idx == length(samples)) {
      shiny::showNotification("Nao e possivel inserir apos o ultimo sample.", type = "warning")
      return()
    }

    s_atual <- samples[[idx]]
    s_prox <- samples[[idx + 1]]

    # Calcula posição e timestamp médios
    lat_novo <- (s_atual$location$latitude + s_prox$location$latitude) / 2
    lon_novo <- (s_atual$location$longitude + s_prox$location$longitude) / 2

    ts_atual <- as.POSIXct(s_atual$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ts_prox <- as.POSIXct(s_prox$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ts_novo <- ts_atual + as.numeric(difftime(ts_prox, ts_atual, units = "secs")) / 2

    # Cria novo sample copiando estrutura do atual
    novo_sample <- s_atual
    novo_sample$sampleId <- uuid::UUIDgenerate(use.time = TRUE)
    novo_sample$date <- format(ts_novo, "%Y-%m-%dT%H:%M:%SZ")
    novo_sample$location$latitude <- lat_novo
    novo_sample$location$longitude <- lon_novo
    novo_sample$location$timestamp <- novo_sample$date
    novo_sample$lastSaved <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    # Insere na posição correta
    if (idx == length(samples)) {
      samples <- c(samples, list(novo_sample))
    } else {
      samples <- c(samples[1:idx], list(novo_sample), samples[(idx + 1):length(samples)])
    }

    edit_samples(samples)
    renderizar_edit_samples()
    shiny::showNotification("Novo sample inserido.", type = "message")
  })

  # ---- Acoes em massa (selecao) ----

  # Ignorar selecionados
  shiny::observeEvent(input$ignorar_selecionados, {
    selected <- input$selected_samples
    if (is.null(selected) || length(selected) == 0) {
      shiny::showNotification("Nenhum sample selecionado.", type = "warning")
      return()
    }

    ignored <- ignored_samples()
    novos <- setdiff(selected, ignored)
    ignored <- c(ignored, novos)
    ignored_samples(ignored)

    # Limpa selecao
    session$sendCustomMessage("clear_selection", list())

    renderizar_edit_samples()
    shiny::showNotification(sprintf("%d samples ignorados.", length(novos)), type = "message")
  })

  # Descartar selecionados
  shiny::observeEvent(input$descartar_selecionados, {
    selected <- input$selected_samples
    if (is.null(selected) || length(selected) == 0) {
      shiny::showNotification("Nenhum sample selecionado.", type = "warning")
      return()
    }

    samples <- edit_samples()
    ids <- vapply(samples, function(s) s$sampleId, character(1))
    samples <- samples[!ids %in% selected]
    edit_samples(samples)

    # Remove da lista de ignorados também
    ignored <- ignored_samples()
    ignored <- ignored[!ignored %in% selected]
    ignored_samples(ignored)

    # Limpa selecao
    session$sendCustomMessage("clear_selection", list())

    renderizar_edit_samples()
    shiny::showNotification(sprintf("%d samples descartados.", length(selected)), type = "message")
  })

  # Download samples corrigidos
  output$download_edit_arc <- shiny::downloadHandler(
    filename = function() {
      paste0("arc_edit_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      all_edit_samples <- edit_samples()
      ignored <- ignored_samples()

      if (length(all_edit_samples) == 0) {
        shiny::showNotification("Nenhum sample para exportar.", type = "error")
        return()
      }

      # Filtra samples ignorados
      ids <- vapply(all_edit_samples, function(s) s$sampleId, character(1))
      samples <- all_edit_samples[!ids %in% ignored]

      if (length(samples) == 0) {
        shiny::showNotification("Todos os samples foram ignorados.", type = "error")
        return()
      }

      tmpdir <- tempfile("arc_edit_")
      dir.create(tmpdir)

      # Cria estrutura de diretórios - SÓ LocomotionSample
      import_dir <- file.path(tmpdir, "Import")
      sample_dir <- file.path(import_dir, "LocomotionSample")
      dir.create(sample_dir, recursive = TRUE)

      current_timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

      # Obtém timelineItemId original (será mantido!)
      ti_id <- original_timeline_item_id()

      # Obtém samples virtuais (criados pelo snap-to-road)
      virtuais <- samples_virtuais()

      # ---- SAMPLES NOVOS (samples.json) ----
      # Usa os samples VIRTUAIS se existirem, senão usa os editados
      samples_novos <- list()

      if (length(virtuais) > 0) {
        # Exporta samples virtuais (já têm novos IDs e timelineItemId)
        for (i in seq_along(virtuais)) {
          s <- virtuais[[i]]
          # Remove campo .virtual (interno)
          s$.virtual <- NULL
          s$lastSaved <- current_timestamp
          samples_novos[[length(samples_novos) + 1L]] <- s
        }
      } else {
        # Fallback: exporta samples editados com novos IDs
        for (i in seq_along(samples)) {
          s <- samples[[i]]
          s$sampleId <- uuid::UUIDgenerate(use.time = TRUE)
          s$lastSaved <- current_timestamp
          # Mantém timelineItemId original
          samples_novos[[length(samples_novos) + 1L]] <- s
        }
      }

      samples_json <- file.path(sample_dir, "samples.json")
      jsonlite::write_json(samples_novos, samples_json, auto_unbox = TRUE, pretty = TRUE)

      # ---- SAMPLES PARA DELETAR (samples_apagar.json) ----
      # Apenas os samples originais do intervalo carregado (não todo o TI)
      originais <- original_samples()
      samples_apagar <- list()
      for (i in seq_along(originais)) {
        s <- originais[[i]]
        if (!(s$sampleId %in% ignored)) {
          s$deleted <- TRUE
          s$lastSaved <- current_timestamp
          samples_apagar[[length(samples_apagar) + 1L]] <- s
        }
      }
      apagar_json <- file.path(sample_dir, "samples_apagar.json")
      jsonlite::write_json(samples_apagar, apagar_json, auto_unbox = TRUE, pretty = TRUE)

      owd <- setwd(tmpdir)
      on.exit(setwd(owd), add = TRUE)

      zip::zipr(zipfile = file, files = "Import")

      n_novos <- length(samples_novos)
      n_apagar <- length(samples_apagar)

      msg <- sprintf("%d samples novos exportados, %d marcados p/ deletar",
                     n_novos, n_apagar)
      if (length(virtuais) > 0) {
        msg <- paste0(msg, " (virtuais da rota OSRM)")
      }
      shiny::showNotification(msg, type = "message")
    }
  )

  # ---- Limpar tudo ----

  shiny::observeEvent(input$limpar_tudo, {
    timeline(list())
    all_samples(list())
    edit_samples(list())
    original_samples(list())
    original_timeline_item_id(NULL)
    ignored_samples(character(0))
    rota_osrm(NULL)
    samples_virtuais(list())
    pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("waypoints") %>%
      leaflet::clearGroup("rota_atual") %>%
      leaflet::clearGroup("importado") %>%
      leaflet::clearGroup("edit_samples") %>%
      leaflet::clearGroup("edit_virtuais") %>%
      leaflet::clearGroup("edit_rota")
    shiny::showNotification("Timeline, samples e mapa limpos.", type = "message")
  })

  # ---- Download Arc ----

  output$download_arc <- shiny::downloadHandler(
    filename = function() {
      paste0("arc_import_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      tmpdir <- tempfile("arc_import_")
      dir.create(tmpdir)
      tl <- timeline()
      s <- all_samples()

      if (length(tl) == 0 || length(s) == 0) {
        shiny::showNotification("Não há itens/samples para exportar.", type = "error")
        return()
      }

      exportar_arc_json(tl, s, tmpdir,
                        data_trabalho = input$data_trabalho)

      owd <- setwd(tmpdir)
      on.exit(setwd(owd), add = TRUE)

      zip::zipr(zipfile = file, files = "Import")
    }
  )

  # ---- Download GPX ----

  output$download_gpx <- shiny::downloadHandler(
    filename = function() {
      paste0("samples_", format(Sys.Date(), "%Y%m%d"), ".gpx")
    },
    content = function(file) {
      s <- all_samples()
      if (length(s) == 0) {
        shiny::showNotification("Nenhum sample para exportar.", type = "error")
        return()
      }

      gpx_header <- '<?xml version="1.0" encoding="UTF-8"?>\n<gpx version="1.1" creator="ArcTimelineBuilder" xmlns="http://www.topografix.com/GPX/1/1">\n  <trk>\n    <name>Arc Samples</name>\n    <trkseg>\n'
      gpx_footer <- '    </trkseg>\n  </trk>\n</gpx>\n'

      trkpts <- vapply(s, function(x) {
        lat <- x$location$latitude
        lon <- x$location$longitude
        time <- x$date
        sprintf('      <trkpt lat="%.8f" lon="%.8f"><time>%s</time></trkpt>\n', lat, lon, time)
      }, character(1))

      cat(gpx_header, file = file)
      cat(trkpts, file = file, append = TRUE)
      cat(gpx_footer, file = file, append = TRUE)
    }
  )
}
