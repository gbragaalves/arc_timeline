# ---- UI ----

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      .shiny-notification {
        top: 60px;
        left: 50%;
        transform: translateX(-50%);
      }

      /* Cruzinha no mapa Leaflet */
      .leaflet-container {
        cursor: crosshair !important;
      }
    "))
  ),

  shiny::titlePanel("Arc Timeline Builder 2.0"),

  shiny::fluidRow(
    # Coluna esquerda - controles (width = 2)
    shiny::column(
      width = 2,
      shiny::dateInput("data_trabalho", "Data principal", Sys.Date()),
      shiny::radioButtons(
        "modo",
        "Modo de edição",
        choices = c("OSRM", "Visita", "Rota Manual", "Importar Arquivo"),
        selected = "OSRM"
      ),
      shiny::hr(),
      # OSRM
      shiny::conditionalPanel(
        "input.modo == 'OSRM'",
        shiny::selectInput(
          "osrm_perfil",
          "Perfil OSRM",
          choices = c("Carro" = "car", "A pé" = "foot", "Bicicleta" = "bike", "Ônibus" = "bus"),
          selected = "car"
        ),
        shiny::actionButton("desfazer_ponto_osrm", "Desfazer último ponto"),
        shiny::actionButton("limpar_pontos_osrm", "Limpar pontos"),
        shiny::br(), shiny::br(),
        shiny::actionButton("calcular_osrm", "Calcular rota OSRM", class = "btn-primary")
      ),
      # Visita
      shiny::conditionalPanel(
        "input.modo == 'Visita'",
        shiny::textInput("visita_nome", "Nome da visita", ""),
        shiny::tags$small("Clique no mapa para marcar o local (um ponto)."),
        shiny::dateInput("visita_data_inicio", "Data de entrada", Sys.Date()),
        shiny::textInput("visita_hora_inicio", "Hora de entrada", "08:00"),
        shiny::dateInput("visita_data_fim", "Data de saída", Sys.Date()),
        shiny::textInput("visita_hora_fim", "Hora de saída", "09:00"),
        shiny::br(),
        shiny::actionButton("adicionar_visita", "Adicionar visita", class = "btn-success")
      ),
      # Rota manual
      shiny::conditionalPanel(
        "input.modo == 'Rota Manual'",
        shiny::tags$small("Clique no mapa para definir os pontos da rota."),
        shiny::dateInput("manual_data_inicio", "Data início", Sys.Date()),
        shiny::textInput("manual_hora_inicio", "Hora início", "08:00"),
        shiny::dateInput("manual_data_fim", "Data fim", Sys.Date()),
        shiny::textInput("manual_hora_fim", "Hora fim", "09:00"),
        shiny::br(),
        shiny::actionButton("desfazer_ponto_manual", "Desfazer último ponto"),
        shiny::actionButton("limpar_pontos_manual", "Limpar pontos"),
        shiny::br(), shiny::br(),
        shiny::actionButton("adicionar_manual", "Adicionar rota manual", class = "btn-success")
      ),
      # Importar arquivo
      shiny::conditionalPanel(
        "input.modo == 'Importar Arquivo'",
        shiny::fileInput(
          "arquivo_geo", "Arquivo Geo (GeoJSON/GPX/KML/GPKG)",
          accept = c(".geojson", ".json", ".gpx", ".kml", ".gpkg")
        ),
        shiny::radioButtons(
          "direcao_arquivo",
          "Direção",
          choices = c("Normal" = "normal", "Inverter" = "inverter"),
          selected = "normal",
          inline = TRUE
        ),
        shiny::dateInput("import_data_inicio", "Data início", Sys.Date()),
        shiny::textInput("import_hora_inicio", "Hora início", "08:00"),
        shiny::dateInput("import_data_fim", "Data fim", Sys.Date()),
        shiny::textInput("import_hora_fim", "Hora fim", "09:00"),
        shiny::br(),
        shiny::actionButton("adicionar_import", "Adicionar rota do arquivo", class = "btn-success")
      ),
      shiny::hr(),
      shiny::actionButton("limpar_tudo", "Limpar tudo", class = "btn-danger")
    ),

    # Coluna central - mapa (width = 8)
    shiny::column(
      width = 8,
      leaflet::leafletOutput("map", height = "700px")
    ),

    # Coluna direita - timeline (width = 2)
    shiny::column(
      width = 2,
      shiny::h4("Timeline do dia"),
      shiny::uiOutput("timeline_list"),
      shiny::hr(),
      shiny::downloadButton("download_arc", "Baixar pacote Arc (zip)", class = "btn-block"),
      shiny::br(),
      shiny::downloadButton("download_gpx", "Baixar GPX (todos samples)", class = "btn-block")
    )
  )
)
