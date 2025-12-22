# ---- Server ----

server <- function(input, output, session) {

  # Estado reativo
  pontos_temp <- shiny::reactiveVal(data.frame(lat = numeric(0), lng = numeric(0)))
  timeline <- shiny::reactiveVal(list())
  all_samples <- shiny::reactiveVal(list())

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
      shiny::showModal(
        shiny::modalDialog(
          title = "Servidor OSRM indisponível",
          shiny::p("Não consegui me conectar ao servidor OSRM local."),
          shiny::tags$ul(
            shiny::tags$li("Verifique se os containers Docker do OSRM estão rodando."),
            shiny::tags$li("Se você usa containers chamados ", shiny::code("osrm-car osrm-foot osrm-bike osrm-bus"), ":"),
            shiny::tags$li(shiny::code("docker start osrm-car osrm-foot osrm-bike osrm-bus")),
            shiny::tags$li("Se você usa docker-compose, algo como:"),
            shiny::tags$li(shiny::code("docker-compose -f ~/location_history/osrm/docker-compose.yml up -d"))
          ),
          easyClose = TRUE
        )
      )
      return()
    }

    rota <- calcular_rota_osrm(pts, perfil = perfil)
    if (is.null(rota)) {
      shiny::showNotification("Falha ao calcular rota OSRM.", type = "error")
      return()
    }

    # Sugere horário com base na última atividade (se houver)
    last_local <- get_last_end_local()
    if (!is.null(last_local)) {
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
          leaflet::fitBounds(
            lng1 = min(coords[, 1]), lat1 = min(coords[, 2]),
            lng2 = max(coords[, 1]), lat2 = max(coords[, 2])
          )

        shiny::showNotification("KML enriquecido (FR24) carregado.", type = "message")
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
    leaflet::leafletProxy("map") %>%
      leaflet::addPolylines(
        data = geo,
        color = "purple",
        weight = 3,
        group = "importado"
      ) %>%
      leaflet::fitBounds(
        lng1 = bbox["xmin"], lat1 = bbox["ymin"],
        lng2 = bbox["xmax"], lat2 = bbox["ymax"]
      )

    shiny::showNotification("Arquivo geográfico importado.", type = "message")
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
        leaflet::fitBounds(
          lng1 = min(coords[, 1]), lat1 = min(coords[, 2]),
          lng2 = max(coords[, 1]), lat2 = max(coords[, 2])
        )
    } else {
      bbox <- sf::st_bbox(geo)
      leaflet::leafletProxy("map") %>%
        leaflet::addPolylines(
          data = geo,
          color = "purple",
          weight = 3,
          group = "importado"
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

  # ---- Limpar tudo ----

  shiny::observeEvent(input$limpar_tudo, {
    timeline(list())
    all_samples(list())
    pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("waypoints") %>%
      leaflet::clearGroup("rota_atual") %>%
      leaflet::clearGroup("importado")
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
