#############################
# Arc Timeline Builder 2.0
# Versão reorganizada / robusta
#############################

# ---- Pacotes ----
library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(sf)
library(dplyr)
library(lubridate)
library(uuid)
library(lutz)
library(xml2)
library(stringr)
library(purrr)
library(geosphere)  # ADICIONADO: necessário para distHaversine e distVincentyEllipsoid
library(zip)        # ADICIONADO: necessário para zip::zipr

# ---- Constantes ----

# Carrega configurações locais (dados privados)
source("config_local.R", local = FALSE)

# Servidores OSRM locais (ajuste portas se necessário)
OSRM_SERVERS <- list(
  car  = "http://127.0.0.1:5000",
  foot = "http://127.0.0.1:5001",
  bike = "http://127.0.0.1:5002",
  bus  = "http://127.0.0.1:5003"
)

`%||%` <- function(x, y) if (!is.null(x)) x else y

# ---- Helpers de tempo / timezone ----

formatar_hora <- function(hora_str) {
  if (is.null(hora_str) || is.na(hora_str)) return(NA_character_)
  hora_str <- trimws(hora_str)
  if (hora_str == "") return(NA_character_)
  
  # Se já está no formato H:M, HH:MM, HH:MM:SS etc.
  if (grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", hora_str)) {
    parts <- strsplit(hora_str, ":", fixed = TRUE)[[1]]
    h <- as.integer(parts[1])
    m <- as.integer(parts[2])
    s <- if (length(parts) >= 3) as.integer(parts[3]) else 0L
  } else {
    # Pega só dígitos
    d <- gsub("\\D", "", hora_str)
    if (nchar(d) == 0) return(NA_character_)
    
    if (nchar(d) <= 2) {
      # "4" -> 4:00:00
      h <- as.integer(d)
      m <- 0L
      s <- 0L
    } else if (nchar(d) == 3) {
      # "400" -> 4:00:00
      h <- as.integer(substr(d, 1, 1))
      m <- as.integer(substr(d, 2, 3))
      s <- 0L
    } else {
      # "0400", "1230", "123045" -> usa só os 4 primeiros
      d <- substr(d, 1, 4)
      h <- as.integer(substr(d, 1, 2))
      m <- as.integer(substr(d, 3, 4))
      s <- 0L
    }
  }
  
  if (is.na(h) || is.na(m) || is.na(s)) return(NA_character_)
  
  # Garante faixas válidas
  h <- max(0L, min(23L, h))
  m <- max(0L, min(59L, m))
  s <- max(0L, min(59L, s))
  
  sprintf("%02d:%02d:%02d", h, m, s)
}

# Limpa e ordena trilha de voo FR24 --------------------------------------
limpar_trajeto_fr24 <- function(df,
                                ts_col  = "timestamp",
                                lon_col = "longitude",
                                lat_col = "latitude") {
  # Garante colunas básicas
  if (!all(c(ts_col, lon_col, lat_col) %in% names(df))) {
    stop("Data frame do FR24 não tem as colunas esperadas: ",
         ts_col, ", ", lon_col, ", ", lat_col)
  }
  
  ts_chr <- as.character(df[[ts_col]])
  ts_utc <- parse_timestamp_utc(ts_chr)
  
  lon <- suppressWarnings(as.numeric(df[[lon_col]]))
  lat <- suppressWarnings(as.numeric(df[[lat_col]]))
  
  # remove linhas quebradas
  ok <- !is.na(ts_utc) & !is.na(lon) & !is.na(lat)
  ts_utc <- ts_utc[ok]
  lon    <- lon[ok]
  lat    <- lat[ok]
  
  if (length(ts_utc) < 2) {
    stop("Trilha FR24 tem menos de 2 pontos válidos.")
  }
  
  # ordena por tempo
  ord    <- order(ts_utc)
  ts_utc <- ts_utc[ord]
  lon    <- lon[ord]
  lat    <- lat[ord]
  
  # remove duplicados exatos consecutivos
  dup <- c(FALSE,
           ts_utc[-1] == ts_utc[-length(ts_utc)] &
             lon[-1]  == lon[-length(lon)] &
             lat[-1]  == lat[-length(lat)])
  ts_utc <- ts_utc[!dup]
  lon    <- lon[!dup]
  lat    <- lat[!dup]
  
  coords <- cbind(lon, lat)
  colnames(coords) <- c("lon", "lat")
  
  list(
    ts_utc = ts_utc,
    coords = coords
  )
}

# Distância de um trajeto em km (pares consecutivos) ----------------------
trajeto_distancia_km <- function(coords) {
  n <- nrow(coords)
  if (is.null(n) || n < 2) return(0)
  
  dists_m <- geosphere::distHaversine(
    coords[-n, , drop = FALSE],
    coords[-1, , drop = FALSE]
  )
  sum(dists_m) / 1000
}


# Parse genérico de timestamps ISO 8601 em UTC
parse_timestamp_utc <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  ts <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE))
  if (all(is.na(ts))) {
    ts <- suppressWarnings(
      lubridate::parse_date_time(
        x,
        orders = c("Ymd HMSz", "Ymd HMS", "Ymd HMz", "Ymd HM", "Ymd HMSOSz"),
        tz = "UTC"
      )
    )
  }
  ts
}

tz_from_coords <- local({
  cache <- new.env(parent = emptyenv())
  
  function(lat, lon) {
    # Arredonda pra evitar chaves demais pra pontos quase iguais
    key <- paste0(round(lat, 4), ":", round(lon, 4))
    if (exists(key, envir = cache, inherits = FALSE)) {
      return(get(key, envir = cache))
    }
    
    tz <- tryCatch(
      lutz::tz_lookup_coords(lat, lon, method = "fast"),
      error = function(e) NA_character_
    )
    
    if (is.na(tz) || length(tz) == 0) {
      tz <- "UTC"
    } else {
      tz <- tz[1]
    }
    
    assign(key, tz, envir = cache)
    tz
  }
})


# Calcula secondsFromGMT para um instante UTC em um timezone
seconds_from_gmt <- function(ts_utc, tz) {
  local_time <- with_tz(ts_utc, tzone = tz)
  # formato +HHMM / -HHMM
  off_str <- format(local_time, "%z")
  # "+0300" -> 3 horas -> 10800 seg
  sign <- ifelse(substr(off_str, 1, 1) == "-", -1L, 1L)
  h <- as.integer(substr(off_str, 2, 3))
  m <- as.integer(substr(off_str, 4, 5))
  sign * (h * 3600L + m * 60L)
}

# Valida intervalo (data/hora local) num fuso
validar_intervalo <- function(data_ini, hora_ini, data_fim, hora_fim, tz) {
  hora_ini_fmt <- formatar_hora(hora_ini)
  hora_fim_fmt <- formatar_hora(hora_fim)
  if (is.na(hora_ini_fmt) || is.na(hora_fim_fmt)) return(NULL)
  
  inicio <- suppressWarnings(
    lubridate::ymd_hms(
      paste(data_ini, hora_ini_fmt),
      tz = tz,
      quiet = TRUE
    )
  )
  
  fim <- suppressWarnings(
    lubridate::ymd_hms(
      paste(data_fim, hora_fim_fmt),
      tz = tz,
      quiet = TRUE
    )
  )
  
  
  if (is.na(inicio) || is.na(fim) || fim <= inicio) return(NULL)
  
  list(inicio = inicio, fim = fim)
}

# ---- Helpers Arc: samples & timeline items ----

# Cria LocomotionSamples a partir de coords e timestamps UTC
# coords: matriz n x 2 (lon, lat); timestamps_utc: POSIXct UTC
criar_locomotion_samples <- function(coords,
                                     timestamps_utc,
                                     altitude = NULL,
                                     speed = NULL,
                                     heading = NULL,
                                     accuracy = 0.01,  # Reduzido para respeitar snap-to-road
                                     force_single_tz = FALSE,
                                     moving_state = "moving") {
  
  n <- nrow(coords)
  if (length(timestamps_utc) != n) stop("coords e timestamps_utc com comprimentos diferentes.")
  
  if (is.null(altitude)) altitude <- rep(NA_real_, n)
  if (is.null(speed))    speed    <- rep(NA_real_, n)
  if (is.null(heading))  heading  <- rep(NA_real_, n)
  
  # Timezone por ponto (ou único para todos)
  if (force_single_tz) {
    tz_all <- rep(tz_from_coords(coords[1, 2], coords[1, 1]), n)
  } else {
    tz_all <- vapply(
      seq_len(n),
      function(i) tz_from_coords(coords[i, 2], coords[i, 1]),
      character(1)
    )
  }
  
  samples <- vector("list", n)
  
  for (i in seq_len(n)) {
    ts_utc <- timestamps_utc[i]
    tz_i   <- tz_all[i]
    if (is.na(tz_i)) tz_i <- "UTC"
    
    sec_gmt <- seconds_from_gmt(ts_utc, tz_i)
    
    # Formato UTC em string para Arc
    date_str <- format(ts_utc, "%Y-%m-%dT%H:%M:%SZ")
    
    samples[[i]] <- list(
      sampleId       = UUIDgenerate(use.time = TRUE),
      date           = date_str,
      secondsFromGMT = sec_gmt,
      lastSaved      = date_str,
      movingState    = moving_state,
      recordingState = "recording",
      stepHz         = 0,
      courseVariance = 1,
      xyAcceleration = 0.0,
      zAcceleration  = 0.0,
      location = list(
        timestamp          = date_str,
        latitude           = coords[i, 2],
        longitude          = coords[i, 1],
        altitude           = ifelse(is.na(altitude[i]), 0, unname(altitude[i])),
        horizontalAccuracy = accuracy,
        verticalAccuracy   = 10,
        speed              = ifelse(is.na(speed[i]), 0, unname(speed[i])),
        course             = ifelse(is.na(heading[i]), 0, unname(heading[i]))
      )
      # timelineItemId será preenchido depois em exportar_arc_json()
    )
    
  }
  
  samples
}

# Cria TimelineItem de visita (Trabalha com horário LOCAL do lugar)
criar_timeline_item_visit <- function(lat, lon, inicio_local, fim_local, nome = NULL) {
  tz <- tz_from_coords(lat, lon)
  if (is.na(tz)) tz <- "UTC"
  
  inicio_utc <- with_tz(inicio_local, tzone = "UTC")
  fim_utc    <- with_tz(fim_local,    tzone = "UTC")
  
  sec_ini <- seconds_from_gmt(inicio_utc, tz)
  sec_fim <- seconds_from_gmt(fim_utc,    tz)
  
  start_date_str <- format(inicio_utc, "%Y-%m-%dT%H:%M:%SZ")
  end_date_str   <- format(fim_utc,    "%Y-%m-%dT%H:%M:%SZ")
  
  # Samples estacionários espalhados no intervalo
  n_samp <- max(3L, min(20L, as.integer(as.numeric(difftime(fim_utc, inicio_utc, units = "mins")) / 5)))
  if (is.na(n_samp) || n_samp < 1) n_samp <- 3L
  
  ts_seq <- seq(inicio_utc, fim_utc, length.out = n_samp)
  coords <- cbind(rep(lon, n_samp), rep(lat, n_samp))
  
  samples <- criar_locomotion_samples(
    coords         = coords,
    timestamps_utc = ts_seq,
    altitude       = rep(NA_real_, n_samp),
    moving_state   = "stationary",
    accuracy       = 0.05,  # Visitas: um pouco mais de tolerância
    force_single_tz = TRUE
  )
  
  sample_ids <- vapply(samples, `[[`, "", "sampleId")
  
  item_id <- UUIDgenerate(use.time = TRUE)
  
  item <- list(
    .internalId = item_id,
    .isVisit = TRUE,
    activityType = "stay",
    startDate = list(
      date = start_date_str,
      secondsFromGMT = sec_ini
    ),
    endDate = list(
      date = end_date_str,
      secondsFromGMT = sec_fim
    ),
    place = list(
      center = list(
        latitude = lat,
        longitude = lon
      ),
      radius = 25,
      secondsFromGMT = sec_ini,
      name = nome %||% "Visita"
    ),
    samples = sample_ids
  )
  
  list(item = item, samples = samples)
}

# Cria TimelineItem de trajeto (path) a partir de timestamps UTC + coords
criar_timeline_item_path <- function(timestamps_utc,
                                     coords,
                                     sample_ids,
                                     tipo = "rota",
                                     descricao = NULL,
                                     activity_type = "transport") {
  
  n <- length(timestamps_utc)
  if (n == 0) stop("Sem timestamps para criar path.")
  if (nrow(coords) != n) stop("timestamps e coords com comprimentos diferentes.")
  
  # Timezones da origem e destino
  tz_ini <- tz_from_coords(coords[1, 2], coords[1, 1])
  tz_fim <- tz_from_coords(coords[n, 2], coords[n, 1])
  if (is.na(tz_ini)) tz_ini <- "UTC"
  if (is.na(tz_fim)) tz_fim <- tz_ini
  
  ini_utc <- timestamps_utc[1]
  fim_utc <- timestamps_utc[n]
  
  sec_ini <- seconds_from_gmt(ini_utc, tz_ini)
  sec_fim <- seconds_from_gmt(fim_utc, tz_fim)
  
  item_id <- UUIDgenerate(use.time = TRUE)
  
  item <- list(
    .internalId = item_id,
    .isVisit = FALSE,
    activityType = activity_type,  # CORREÇÃO: usa o parâmetro em vez de hardcoded "transport"
    startDate = list(
      date = format(ini_utc, "%Y-%m-%dT%H:%M:%SZ"),
      secondsFromGMT = sec_ini
    ),
    endDate = list(
      date = format(fim_utc, "%Y-%m-%dT%H:%M:%SZ"),
      secondsFromGMT = sec_fim
    ),
    samples = sample_ids,
    tipo = tipo,
    descricao = descricao %||% "Trajeto"
  )
  
  item
}

# ---- Helpers: parser KML enriquecido (FlightRadar24) ----

parse_kml_rich <- function(kml_file) {
  kml <- tryCatch(xml2::read_xml(kml_file), error = function(e) NULL)
  if (is.null(kml)) return(NULL)
  
  ns <- xml2::xml_ns(kml)
  placemarks <- xml2::xml_find_all(kml, ".//d1:Placemark", ns)
  if (length(placemarks) == 0) return(NULL)
  
  out <- lapply(placemarks, function(pm) {
    point <- xml2::xml_find_first(pm, ".//d1:Point", ns)
    if (inherits(point, "xml_missing")) return(NULL)
    
    coord_node <- xml2::xml_find_first(point, ".//d1:coordinates", ns)
    if (inherits(coord_node, "xml_missing")) return(NULL)
    
    coord_str <- xml2::xml_text(coord_node)
    coords <- strsplit(trimws(coord_str), ",")[[1]]
    if (length(coords) < 2) return(NULL)
    
    lon <- as.numeric(coords[1])
    lat <- as.numeric(coords[2])
    
    altitude_m <- NA_real_
    if (length(coords) >= 3) {
      alt_raw <- suppressWarnings(as.numeric(coords[3]))
      if (!is.na(alt_raw)) altitude_m <- alt_raw
    }
    
    desc_node <- xml2::xml_find_first(pm, ".//d1:description", ns)
    desc <- if (!inherits(desc_node, "xml_missing")) xml2::xml_text(desc_node) else ""
    
    # altitude em pés -> metros
    alt_ft <- stringr::str_match(desc, "Altitude[: ]+([0-9.,]+) ?ft")[, 2]
    if (!is.na(alt_ft)) {
      alt_ft_num <- as.numeric(gsub(",", "", alt_ft))
      if (!is.na(alt_ft_num)) altitude_m <- alt_ft_num * 0.3048
    }
    
    # speed em nós -> m/s
    speed_mps <- NA_real_
    spd_knots <- stringr::str_match(desc, "Speed[: ]+([0-9.,]+) ?kt")[, 2]
    if (!is.na(spd_knots)) {
      spd_knots_num <- as.numeric(gsub(",", "", spd_knots))
      if (!is.na(spd_knots_num)) speed_mps <- spd_knots_num * 0.514444
    }
    
    # heading
    heading <- NA_real_
    hdg <- stringr::str_match(desc, "Heading[: ]+([0-9.,]+) ?deg")[, 2]
    if (!is.na(hdg)) {
      heading_num <- as.numeric(gsub(",", "", hdg))
      if (!is.na(heading_num)) heading <- heading_num
    }
    
    when_node <- xml2::xml_find_first(pm, ".//d1:when", ns)
    timestamp <- if (!inherits(when_node, "xml_missing")) xml2::xml_text(when_node) else NA_character_
    
    data.frame(
      lon        = lon,
      lat        = lat,
      altitude_m = altitude_m,
      speed_mps  = speed_mps,
      heading    = heading,
      timestamp  = timestamp,
      stringsAsFactors = FALSE
    )
  })
  
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

# ---- Helpers: OSRM ----

check_osrm_server <- function(base_url) {
  # tenta uma request mínima, só pra ver se responde
  test_url <- paste0(base_url, "/route/v1/driving/0,0;0,0")
  res <- tryCatch(
    httr::GET(test_url, timeout(2)),
    error = function(e) NULL
  )
  !is.null(res)
}

calcular_rota_osrm <- function(pontos, perfil = c("car", "foot", "bike", "bus")) {
  perfil <- match.arg(perfil)
  base_url <- OSRM_SERVERS[[perfil]]
  if (is.null(base_url)) return(NULL)
  
  if (!check_osrm_server(base_url)) {
    return(NULL)
  }
  
  coords_str <- apply(pontos[, c("lng", "lat")], 1, paste, collapse = ",")
  url <- paste0(
    base_url, "/route/v1/driving/",
    paste(coords_str, collapse = ";"),
    "?overview=full&geometries=geojson"
  )
  
  res <- tryCatch(httr::GET(url, timeout(10)), error = function(e) NULL)
  if (is.null(res) || res$status_code >= 400) return(NULL)
  
  js <- httr::content(res, as = "parsed", encoding = "UTF-8")
  if (is.null(js$routes) || length(js$routes) == 0) return(NULL)
  
  route <- js$routes[[1]]
  coords <- route$geometry$coordinates
  coords_mat <- do.call(rbind, coords)
  colnames(coords_mat) <- c("lon", "lat")
  
  list(
    coords      = coords_mat,
    distance_m  = route$distance,
    duration_s  = route$duration
  )
}

# ---- Helpers: semanas + samples_apagar ----

# Dado um intervalo [ini_utc, fim_utc], gera nomes de arquivos semanais envolvidos
semanas_para_intervalo <- function(ini_utc, fim_utc) {
  if (ini_utc > fim_utc) return(character(0))
  dias <- seq(floor_date(ini_utc, "day"), ceiling_date(fim_utc, "day"), by = "1 day")
  unique(strftime(dias, "%G-W%V"))  # ex: "2025-W30"
}

# Gera lista de samples marcados como deleted com base nos itens novos
gerar_samples_apagar <- function(timeline_items, semana_dir = SEMANA_DIR) {
  if (length(timeline_items) == 0) return(list())
  
  # Intervalos de cada item
  intervalos <- lapply(timeline_items, function(it) {
    ini <- parse_timestamp_utc(it$startDate$date)
    fim <- parse_timestamp_utc(it$endDate$date)
    if (is.null(ini) || is.null(fim) || is.na(ini) || is.na(fim)) return(NULL)
    list(inicio = ini[1], fim = fim[1])
  })
  intervalos <- Filter(Negate(is.null), intervalos)
  if (length(intervalos) == 0) return(list())
  
  # Semanas envolvidas
  semanas <- unique(unlist(lapply(intervalos, function(iv) {
    semanas_para_intervalo(iv$inicio, iv$fim)
  })))
  
  if (length(semanas) == 0) return(list())
  
  samples_apagar <- list()
  current_timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  for (sem in semanas) {
    gz_path <- file.path(semana_dir, paste0(sem, ".json.gz"))
    if (!file.exists(gz_path)) next
    
    # lê array de samples como lista (igual ao apagador antigo)
    semana_samples <- tryCatch(
      jsonlite::fromJSON(gzfile(gz_path), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(semana_samples) || length(semana_samples) == 0) next
    
    for (s in semana_samples) {
      # Usa o campo date (igual ao apagador antigo)
      date_str <- s$date
      if (is.null(date_str)) next
      
      # Parse do timestamp
      ts <- tryCatch(
        as.POSIXct(date_str, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        error = function(e) NULL
      )
      if (is.null(ts) || is.na(ts)) next
      
      # Verifica se está em algum intervalo
      overlap <- any(vapply(intervalos, function(iv) {
        ts >= iv$inicio && ts <= iv$fim
      }, logical(1)))
      
      if (overlap) {
        # IMPORTANTE: Preserva TODOS os campos do sample original
        # e apenas marca como deleted (igual ao apagador antigo)
        s$deleted <- TRUE
        s$lastSaved <- current_timestamp
        samples_apagar[[length(samples_apagar) + 1L]] <- s
      }
    }
  }
  
  samples_apagar
}


# Verifica se [inicio_utc, fim_utc] se sobrepõe a algum item da timeline
# ignore_id: se não for NULL, ignora item com esse .internalId (útil na edição)
has_overlap <- function(timeline_items, inicio_utc, fim_utc, ignore_id = NULL) {
  if (length(timeline_items) == 0) return(FALSE)
  if (is.null(inicio_utc) || is.null(fim_utc) || is.na(inicio_utc) || is.na(fim_utc)) return(FALSE)
  
  any(vapply(timeline_items, function(it) {
    if (!is.null(ignore_id) && identical(it$.internalId, ignore_id)) return(FALSE)
    
    ex_ini <- parse_timestamp_utc(it$startDate$date)
    ex_fim <- parse_timestamp_utc(it$endDate$date)
    if (is.null(ex_ini) || is.null(ex_fim) || is.na(ex_ini[1]) || is.na(ex_fim[1])) return(FALSE)
    
    # overlap se intervalos realmente se cruzam (bordas encostando é OK)
    (inicio_utc < ex_fim[1]) && (fim_utc > ex_ini[1])
  }, logical(1)))
}

# ---- Exportador Arc JSON (Timeline + Samples + samples_apagar) ----

exportar_arc_json <- function(timeline_items, samples, output_dir, data_trabalho) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  import_dir <- file.path(output_dir, "Import")
  ti_dir     <- file.path(import_dir, "TimelineItem")
  ls_dir     <- file.path(import_dir, "LocomotionSample")
  
  dir.create(ti_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(ls_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (length(timeline_items) == 0 || length(samples) == 0) {
    return(invisible(NULL))
  }
  
  # index dos samples por sampleId
  sample_idx <- setNames(
    seq_along(samples),
    vapply(samples, `[[`, "", "sampleId")
  )
  
  now_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  
  # ---- TimelineItem: um arquivo por item ----
  for (i in seq_along(timeline_items)) {
    it <- timeline_items[[i]]
    
    # nosso id interno
    internal_id <- it$.internalId %||% UUIDgenerate(use.time = TRUE)
    
    # garante start/end
    start_date <- it$startDate
    end_date   <- it$endDate
    
    # garante activityType razoável
    act_type <- it$activityType %||% if (isTRUE(it$.isVisit)) "stay" else "transport"
    
    # Se for airplane, marca como manual para forçar o Arc a aceitar
    manual_type <- (act_type == "airplane")
    
    # monta o objeto no formato esperado pelo Arc
    arc_item <- list(
      itemId             = internal_id,
      isVisit            = isTRUE(it$.isVisit),
      startDate          = start_date,
      endDate            = end_date,
      lastSaved          = now_utc,
      activityType       = act_type,
      manualActivityType = manual_type,  # CORREÇÃO: TRUE para airplane
      altitude           = it$altitude %||% 0,
      stepCount          = 0,
      floorsAscended     = 0,
      floorsDescended    = 0
    )
    
    # se for visita e tiver place, podemos incluir
    if (isTRUE(it$.isVisit) && !is.null(it$place)) {
      arc_item$place <- it$place
    }
    
    # escreve um arquivo por TimelineItem
    ti_path <- file.path(ti_dir, paste0(internal_id, ".json"))
    jsonlite::write_json(
      arc_item,
      ti_path,
      auto_unbox = TRUE,
      pretty     = TRUE
    )
    
    # amarra os samples referenciados a este item
    if (!is.null(it$samples)) {
      for (sid in it$samples) {
        idx <- sample_idx[[sid]]
        if (!is.na(idx)) {
          samples[[idx]]$timelineItemId <- internal_id
        }
      }
    }
  }
  
  # ---- LocomotionSample: um arquivo único samples.json ----
  ls_path <- file.path(ls_dir, "samples.json")
  jsonlite::write_json(
    samples,
    ls_path,
    auto_unbox = TRUE,
    pretty     = TRUE
  )
  
  # ---- Samples a apagar (baseado nos arquivos semanais) ----
  
  apagar <- gerar_samples_apagar(timeline_items, semana_dir = SEMANA_DIR)
  
  if (length(apagar) > 0) {
    apagar_path <- file.path(ls_dir, "samples_apagar.json")
    jsonlite::write_json(
      apagar,
      apagar_path,
      auto_unbox = TRUE,
      pretty     = TRUE
    )
    message("✓ samples_apagar.json criado com ", length(apagar), " samples para deletar")
  } else {
    message("⚠ Nenhum sample encontrado nos arquivos semanais para deletar")
    message("   Arquivos procurados em: ", SEMANA_DIR)
  }
  
  invisible(NULL)
}


# ---- UI ----

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
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
  
  titlePanel("Arc Timeline Builder 2.0"),
  
  fluidRow(
    # Coluna esquerda - controles (width = 2)
    column(
      width = 2,
      dateInput("data_trabalho", "Data principal", Sys.Date()),
      radioButtons(
        "modo",
        "Modo de edição",
        choices = c("OSRM", "Visita", "Rota Manual", "Importar Arquivo"),
        selected = "OSRM"
      ),
      hr(),
      # OSRM
      conditionalPanel(
        "input.modo == 'OSRM'",
        selectInput(
          "osrm_perfil",
          "Perfil OSRM",
          choices = c("Carro" = "car", "A pé" = "foot", "Bicicleta" = "bike", "Ônibus" = "bus"),
          selected = "car"
        ),
        actionButton("desfazer_ponto_osrm", "Desfazer último ponto"),
        actionButton("limpar_pontos_osrm", "Limpar pontos"),
        br(), br(),
        actionButton("calcular_osrm", "Calcular rota OSRM", class = "btn-primary")
      ),
      # Visita
      conditionalPanel(
        "input.modo == 'Visita'",
        textInput("visita_nome", "Nome da visita", ""),
        tags$small("Clique no mapa para marcar o local (um ponto)."),
        dateInput("visita_data_inicio", "Data de entrada", Sys.Date()),
        textInput("visita_hora_inicio", "Hora de entrada", "08:00"),
        dateInput("visita_data_fim", "Data de saída", Sys.Date()),
        textInput("visita_hora_fim", "Hora de saída", "09:00"),
        br(),
        actionButton("adicionar_visita", "Adicionar visita", class = "btn-success")
      ),
      # Rota manual
      conditionalPanel(
        "input.modo == 'Rota Manual'",
        tags$small("Clique no mapa para definir os pontos da rota."),
        dateInput("manual_data_inicio", "Data início", Sys.Date()),
        textInput("manual_hora_inicio", "Hora início", "08:00"),
        dateInput("manual_data_fim", "Data fim", Sys.Date()),
        textInput("manual_hora_fim", "Hora fim", "09:00"),
        br(),
        actionButton("desfazer_ponto_manual", "Desfazer último ponto"),
        actionButton("limpar_pontos_manual", "Limpar pontos"),
        br(), br(),
        actionButton("adicionar_manual", "Adicionar rota manual", class = "btn-success")
      ),
      # Importar arquivo
      conditionalPanel(
        "input.modo == 'Importar Arquivo'",
        fileInput(
          "arquivo_geo", "Arquivo Geo (GeoJSON/GPX/KML/GPKG)",
          accept = c(".geojson", ".json", ".gpx", ".kml", ".gpkg")
        ),
        radioButtons(
          "direcao_arquivo",
          "Direção",
          choices = c("Normal" = "normal", "Inverter" = "inverter"),
          selected = "normal",
          inline = TRUE
        ),
        dateInput("import_data_inicio", "Data início", Sys.Date()),
        textInput("import_hora_inicio", "Hora início", "08:00"),
        dateInput("import_data_fim", "Data fim", Sys.Date()),
        textInput("import_hora_fim", "Hora fim", "09:00"),
        br(),
        actionButton("adicionar_import", "Adicionar rota do arquivo", class = "btn-success")
      ),
      hr(),
      actionButton("limpar_tudo", "Limpar tudo", class = "btn-danger")
    ),
    
    # Coluna central - mapa (AINDA MAIS LARGO: width = 8)
    column(
      width = 8,
      leafletOutput("map", height = "700px")
    ),
    
    # Coluna direita - timeline (COMPACTA: width = 2)
    column(
      width = 2,
      h4("Timeline do dia"),
      uiOutput("timeline_list"),
      hr(),
      # BOTÕES DE DOWNLOAD MOVIDOS PARA CÁ
      downloadButton("download_arc", "Baixar pacote Arc (zip)", class = "btn-block"),
      br(),
      downloadButton("download_gpx", "Baixar GPX (todos samples)", class = "btn-block")
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {
  
  # Estado reativo
  pontos_temp <- reactiveVal(data.frame(lat = numeric(0), lng = numeric(0)))
  timeline <- reactiveVal(list())
  all_samples <- reactiveVal(list())
  
  # Formata automaticamente inputs de hora (só quando parece completo)
  auto_format_time <- function(id) {
    observeEvent(input[[id]], {
      val <- input[[id]]
      if (is.null(val) || !nzchar(val)) return()
      
      # SÓ FORMATA se já digitou pelo menos 3 caracteres (evita formatar enquanto digita)
      # Exemplo: "1" -> não formata, "14" -> não formata, "143" -> formata para 14:30:00
      if (nchar(gsub("\\D", "", val)) < 3) return()
      
      fmt <- formatar_hora(val)
      if (!is.na(fmt) && !identical(fmt, val)) {
        updateTextInput(session, id, value = fmt)
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
    
    # CORREÇÃO: retorna em UTC, o observeEvent vai lidar com o timezone
    # (porque os campos de data/hora trabalham em "local" do navegador)
    end_utc
  }
  
  
  # Quando a timeline mudar, sugerir próximo início = fim anterior + 1 min
  observeEvent(timeline(), {
    last_utc <- get_last_end_local()  # retorna em UTC
    if (is.null(last_utc)) return()
    
    # Converte para timezone do sistema do usuário
    last_local <- with_tz(last_utc, tzone = Sys.timezone())
    
    next_local <- last_local + 60  # +1 minuto
    
    data_next <- as.Date(next_local)
    hora_next <- format(next_local, "%H:%M:%S")
    
    # visita
    updateDateInput(session, "visita_data_inicio", value = data_next)
    updateTextInput(session, "visita_hora_inicio", value = hora_next)
    
    # rota manual
    updateDateInput(session, "manual_data_inicio", value = data_next)
    updateTextInput(session, "manual_hora_inicio", value = hora_next)
    
    # importar
    updateDateInput(session, "import_data_inicio", value = data_next)
    updateTextInput(session, "import_hora_inicio", value = hora_next)
  })
  
  
  # Quando a data principal mudar, replica para os outros campos de data
  observeEvent(input$data_trabalho, {
    d <- input$data_trabalho
    
    updateDateInput(session, "visita_data_inicio", value = d)
    updateDateInput(session, "visita_data_fim",    value = d)
    
    updateDateInput(session, "manual_data_inicio", value = d)
    updateDateInput(session, "manual_data_fim",    value = d)
    
    updateDateInput(session, "import_data_inicio", value = d)
    updateDateInput(session, "import_data_fim",    value = d)
  })
  
  
  # ---- Mapa base ----
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles(group = "OSM") |>
      addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
      addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
      addLayersControl(
        baseGroups = c("CartoDB", "OSM", "Satélite"),
        options = layersControlOptions(collapsed = TRUE)
      ) |>
      setView(lng = -43.2, lat = -22.9, zoom = 11)
  })
  
  # Helper: atualiza pontos no mapa
  atualizar_pontos_mapa <- function(auto_zoom = TRUE) {
    pts <- pontos_temp()
    proxy <- leafletProxy("map")
    proxy <- proxy %>% clearGroup("waypoints")
    if (nrow(pts) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
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
          fitBounds(
            lng1 = min(pts$lng), lat1 = min(pts$lat),
            lng2 = max(pts$lng), lat2 = max(pts$lat)
          )
      }
    }
  }
  
  # Clique no mapa
  observeEvent(input$map_click, {
    click <- input$map_click
    if (is.null(click)) return()
    
    modo <- input$modo
    pts <- pontos_temp()
    
    if (modo == "Visita") {
      # visita usa só 1 ponto: substitui
      pts <- data.frame(lat = click$lat, lng = click$lng)
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = TRUE)  # Visita: faz zoom
    } else {
      # OSRM ou Rota Manual: adiciona ponto
      pts <- rbind(pts, data.frame(lat = click$lat, lng = click$lng))
      pontos_temp(pts)
      # NÃO faz auto_zoom para deixar você controlar o mapa
      atualizar_pontos_mapa(auto_zoom = FALSE)
    }
  })
  
  # Limpar pontos (OSRM / manual) - SEM auto_zoom
  observeEvent(input$limpar_pontos_osrm, {
    pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))
    atualizar_pontos_mapa(auto_zoom = FALSE)
  })
  observeEvent(input$limpar_pontos_manual, {
    pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))
    atualizar_pontos_mapa(auto_zoom = FALSE)
  })
  
  # Desfazer último ponto - SEM auto_zoom
  observeEvent(input$desfazer_ponto_osrm, {
    pts <- pontos_temp()
    if (nrow(pts) > 0) {
      pts <- pts[-nrow(pts), , drop = FALSE]
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = FALSE)
    }
  })
  observeEvent(input$desfazer_ponto_manual, {
    pts <- pontos_temp()
    if (nrow(pts) > 0) {
      pts <- pts[-nrow(pts), , drop = FALSE]
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = FALSE)
    }
  })
  
  # ---- Visita: auto-preencher coordenadas por nome frequente ----
  
  observeEvent(input$visita_nome, {
    nome <- input$visita_nome
    if (nome %in% names(LOCAIS_FREQUENTES)) {
      loc <- LOCAIS_FREQUENTES[[nome]]
      pts <- data.frame(lat = loc$lat, lng = loc$lon)
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = TRUE)  # Visita: COM zoom
      leafletProxy("map") %>%
        addPopups(loc$lon, loc$lat, paste0("Local: ", nome), layerId = "visita_popup")
    } else {
      leafletProxy("map") %>% removePopup("visita_popup")
    }
  })
  
  # ---- Adicionar visita ----
  
  observeEvent(input$adicionar_visita, {
    pts <- pontos_temp()
    if (nrow(pts) != 1) {
      showNotification("Selecione exatamente 1 ponto no mapa para a visita.", type = "error")
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
      showNotification("Horário inválido para a visita.", type = "error")
      return()
    }
    
    # converte intervalo da visita para UTC para checar overlap
    inicio_utc <- with_tz(val$inicio, "UTC")
    fim_utc    <- with_tz(val$fim,    "UTC")
    
    if (has_overlap(timeline(), inicio_utc, fim_utc)) {
      showNotification("Este intervalo se sobrepõe a outra atividade existente.", type = "error")
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
    leafletProxy("map") %>%
      fitBounds(lng, lat, lng, lat)
    
    showNotification("Visita adicionada.", type = "message")
  })
  
  # ---- Rota manual ----
  
  observeEvent(input$adicionar_manual, {
    pts <- pontos_temp()
    if (nrow(pts) < 2) {
      showNotification("Defina pelo menos 2 pontos no mapa para a rota manual.", type = "error")
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
      showNotification("Horário inválido para a rota manual.", type = "error")
      return()
    }
    
    # Gera timestamps UTC equiespaçados
    inicio_utc <- with_tz(val$inicio, "UTC")
    fim_utc    <- with_tz(val$fim,    "UTC")
    
    if (has_overlap(timeline(), inicio_utc, fim_utc)) {
      showNotification("Este intervalo se sobrepõe a outra atividade existente.", type = "error")
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
      accuracy       = 0.01,  # Bem preciso para snap-to-road
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
    leafletProxy("map") %>%
      clearGroup("rota_atual") %>%
      addPolylines(
        lng = coords_new[, 1],
        lat = coords_new[, 2],
        color = "red",
        weight = 4,
        opacity = 0.8,
        group = "rota_atual"
      ) %>%
      fitBounds(
        lng1 = min(coords_new[, 1]),
        lat1 = min(coords_new[, 2]),
        lng2 = max(coords_new[, 1]),
        lat2 = max(coords_new[, 2])
      )
    
    showNotification("Rota manual adicionada.", type = "message")
  })
  
  # ---- OSRM: calcular rota ----
  
  observeEvent(input$calcular_osrm, {
    pts <- pontos_temp()
    if (nrow(pts) < 2) {
      showNotification("Defina pelo menos 2 pontos no mapa para a rota OSRM.", type = "error")
      return()
    }
    
    perfil <- input$osrm_perfil
    base_url <- OSRM_SERVERS[[perfil]]
    
    if (is.null(base_url) || !check_osrm_server(base_url)) {
      showModal(
        modalDialog(
          title = "Servidor OSRM indisponível",
          p("Não consegui me conectar ao servidor OSRM local."),
          tags$ul(
            tags$li("Verifique se os containers Docker do OSRM estão rodando."),
            tags$li("Se você usa containers chamados ", code("osrm-car osrm-foot osrm-bike osrm-bus"), ":"),
            tags$li(code("docker start osrm-car osrm-foot osrm-bike osrm-bus")),
            tags$li("Se você usa docker-compose, algo como:"),
            tags$li(code("docker-compose -f ~/location_history/osrm/docker-compose.yml up -d"))
          ),
          easyClose = TRUE
        )
      )
      return()
    }
    
    rota <- calcular_rota_osrm(pts, perfil = perfil)
    if (is.null(rota)) {
      showNotification("Falha ao calcular rota OSRM.", type = "error")
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
    
    
    showModal(
      modalDialog(
        title = "Horários da rota OSRM",
        dateInput("osrm_data_inicio", "Data início", as.Date(inicia_sugerido)),
        textInput("osrm_hora_inicio", "Hora início", format(inicia_sugerido, "%H:%M:%S")),
        dateInput("osrm_data_fim", "Data fim", as.Date(fim_sugerido)),
        textInput("osrm_hora_fim", "Hora fim", format(fim_sugerido, "%H:%M:%S")),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("recalcular_osrm_fim", "Recalcular chegada"),
          actionButton("confirmar_rota_osrm", "Confirmar", class = "btn-primary")
        )
      )
    )
    
    auto_format_time("osrm_hora_inicio")
    auto_format_time("osrm_hora_fim")
    
    # Guarda rota em atributo da sessão
    session$userData$ultima_rota_osrm <- rota
    session$userData$ultima_rota_pts <- pts
  })
  
  observeEvent(input$recalcular_osrm_fim, {
    rota <- session$userData$ultima_rota_osrm
    if (is.null(rota)) {
      showNotification("Nenhuma rota OSRM calculada para recalcular o horário de chegada.", type = "error")
      return()
    }
    
    coords <- rota$coords
    if (is.data.frame(coords)) coords <- as.matrix(coords)
    if (!is.matrix(coords) || nrow(coords) < 1 || ncol(coords) < 2) {
      showNotification("Coordenadas da rota OSRM estão em formato inesperado.", type = "error")
      return()
    }
    
    lat1 <- as.numeric(coords[1, 2])
    lng1 <- as.numeric(coords[1, 1])
    if (is.na(lat1) || is.na(lng1)) {
      showNotification("Primeiro ponto da rota OSRM está inválido.", type = "error")
      return()
    }
    
    tz <- tz_from_coords(lat1, lng1)
    
    # hora de partida (formata de novo pra garantir)
    hora_ini_fmt <- formatar_hora(input$osrm_hora_inicio)
    if (is.na(hora_ini_fmt)) {
      showNotification("Hora de partida inválida.", type = "error")
      return()
    }
    
    inicio_local <- suppressWarnings(
      lubridate::ymd_hms(
        paste(input$osrm_data_inicio, hora_ini_fmt),
        tz = tz
      )
    )
    if (is.na(inicio_local)) {
      showNotification("Não consegui interpretar a data/hora de partida.", type = "error")
      return()
    }
    
    # duração em segundos vinda do OSRM
    dur_s <- as.numeric(rota$duration_s)
    if (is.na(dur_s)) {
      showNotification("Duração da rota OSRM inválida.", type = "error")
      return()
    }
    
    fim_local <- inicio_local + dur_s
    
    updateDateInput(session, "osrm_data_fim", value = as.Date(fim_local))
    updateTextInput(session, "osrm_hora_fim", value = format(fim_local, "%H:%M:%S"))
  })
  
  
  observeEvent(input$confirmar_rota_osrm, {
    tryCatch(
      {
        removeModal()
        
        rota <- session$userData$ultima_rota_osrm
        if (is.null(rota)) {
          showNotification("Nenhuma rota OSRM calculada.", type = "error")
          return()
        }
        
        coords <- rota$coords
        # Garante que coords é matriz numérica (lon, lat)
        if (is.data.frame(coords)) {
          coords <- as.matrix(coords)
        }
        if (!is.matrix(coords) || ncol(coords) < 2) {
          showNotification("Coordenadas da rota OSRM estão em formato inesperado.", type = "error")
          return()
        }
        # força numérico
        coords <- suppressWarnings(apply(coords, 2, as.numeric))
        if (!is.matrix(coords)) {
          coords <- matrix(coords, ncol = 2)
        }
        
        n0 <- nrow(coords)
        if (is.null(n0) || is.na(n0) || n0 < 2) {
          showNotification("Rota OSRM inválida (poucos pontos).", type = "error")
          return()
        }
        
        lat1 <- coords[1, 2]
        lng1 <- coords[1, 1]
        
        if (is.na(lat1) || is.na(lng1)) {
          showNotification("Primeiro ponto da rota OSRM está inválido.", type = "error")
          return()
        }
        
        tz <- tz_from_coords(lat1, lng1)
        
        # Garante que os horários do modal foram bem formatados
        hora_ini_fmt <- formatar_hora(input$osrm_hora_inicio)
        hora_fim_fmt <- formatar_hora(input$osrm_hora_fim)
        
        if (is.na(hora_ini_fmt) || is.na(hora_fim_fmt)) {
          showNotification("Horas inválidas para rota OSRM.", type = "error")
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
          showNotification("Horário inválido para rota OSRM.", type = "error")
          return()
        }
        
        # Garante que são POSIXct
        if (!inherits(val$inicio, "POSIXt") || !inherits(val$fim, "POSIXt")) {
          showNotification("Intervalo de horários da rota OSRM não pôde ser interpretado.", type = "error")
          return()
        }
        
        inicio_utc <- with_tz(val$inicio, "UTC")
        fim_utc    <- with_tz(val$fim,    "UTC")
        
        if (is.na(inicio_utc) || is.na(fim_utc) || fim_utc <= inicio_utc) {
          showNotification("Intervalo da rota OSRM inválido (fim antes ou igual ao início).", type = "error")
          return()
        }
        
        # Checa overlap antes de qualquer matemática pesada
        if (has_overlap(timeline(), inicio_utc, fim_utc)) {
          showNotification("Este intervalo se sobrepõe a outra atividade existente.", type = "error")
          return()
        }
        
        # Interpola coords para ter n pontos (mín 100) e gera sequência de tempos
        n_target <- max(n0, 100L)
        ts_seq <- seq(from = inicio_utc, to = fim_utc, length.out = n_target)
        if (any(is.na(ts_seq))) {
          showNotification("Falha ao gerar sequência de tempos para rota OSRM.", type = "error")
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
          showNotification("Falha ao interpolar coordenadas da rota OSRM.", type = "error")
          return()
        }
        
        coords_new <- cbind(lon_new, lat_new)
        
        # Cria samples e item
        samples_novos <- criar_locomotion_samples(
          coords         = coords_new,
          timestamps_utc = ts_seq,
          accuracy       = 0.01,  # Bem preciso para snap-to-road
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
        
        leafletProxy("map") %>%
          clearGroup("rota_atual") %>%
          addPolylines(
            lng   = coords_new[, 1],
            lat   = coords_new[, 2],
            color = "blue",
            weight = 4,
            group = "rota_atual"
          ) %>%
          fitBounds(
            lng1 = min(coords_new[, 1]),
            lat1 = min(coords_new[, 2]),
            lng2 = max(coords_new[, 1]),
            lat2 = max(coords_new[, 2])
          )
        
        showNotification("Rota OSRM adicionada.", type = "message")
      },
      error = function(e) {
        showNotification(
          paste("Erro ao confirmar rota OSRM:", e$message),
          type = "error",
          duration = NULL
        )
      }
    )
  })
  
  
  
  # ---- Importar arquivo (GeoJSON/GPX/KML/GPKG) ----
  
  geometria_importada <- reactiveVal(NULL)
  fr24_enriquecido <- reactiveVal(FALSE)
  
  observeEvent(input$arquivo_geo, {
    req(input$arquivo_geo)
    arquivo <- input$arquivo_geo$datapath
    ext <- tools::file_ext(arquivo)
    ext <- tolower(ext)
    
    leafletProxy("map") %>% clearGroup("importado")
    
    if (ext == "kml") {
      geo_rich <- parse_kml_rich(arquivo)
      if (!is.null(geo_rich) && "timestamp" %in% names(geo_rich)) {
        geometria_importada(geo_rich)
        fr24_enriquecido(TRUE)
        
        ts_gmt <- parse_timestamp_utc(geo_rich$timestamp)
        ord <- order(ts_gmt)
        ts_gmt <- ts_gmt[ord]
        
        # monta coords a partir do geo_rich, na MESMA ordem dos timestamps
        coords <- cbind(
          geo_rich$lon[ord],
          geo_rich$lat[ord]
        )
        colnames(coords) <- c("lon", "lat")
        
        
        # auto-preenche datas/horas com base no UTC
        if (!is.null(ts_gmt) && !all(is.na(ts_gmt))) {
          ini <- ts_gmt[1]
          fim <- ts_gmt[length(ts_gmt)]
          updateDateInput(session, "import_data_inicio", value = as.Date(ini))
          updateTextInput(session, "import_hora_inicio", value = format(with_tz(ini, Sys.timezone()), "%H:%M"))
          updateDateInput(session, "import_data_fim", value = as.Date(fim))
          updateTextInput(session, "import_hora_fim", value = format(with_tz(fim, Sys.timezone()), "%H:%M"))
          showNotification("Horários preenchidos com base no arquivo KML (UTC -> horário local do seu sistema).", type = "message")
        }
        
        leafletProxy("map") %>%
          addPolylines(
            lng = coords[, 1],
            lat = coords[, 2],
            color = "darkblue",
            weight = 3,
            group = "importado"
          ) %>%
          fitBounds(
            lng1 = min(coords[, 1]), lat1 = min(coords[, 2]),
            lng2 = max(coords[, 1]), lat2 = max(coords[, 2])
          )
        
        showNotification("KML enriquecido (FR24) carregado.", type = "message")
        return()
      }
    }
    
    # Caso normal: lê com sf
    geo <- tryCatch(
      sf::st_read(arquivo, quiet = TRUE),
      error = function(e) NULL
    )
    if (is.null(geo)) {
      showNotification("Não consegui ler o arquivo como camada geográfica.", type = "error")
      return()
    }
    
    fr24_enriquecido(FALSE)
    
    if (!sf::st_is_longlat(geo)) {
      geo <- sf::st_transform(geo, 4326)
    }
    
    # converte para LINESTRING se for multilinha
    if (any(sf::st_geometry_type(geo) %in% c("MULTILINESTRING"))) {
      geo <- sf::st_cast(geo, "LINESTRING")
    }
    
    geometria_importada(geo)
    
    bbox <- sf::st_bbox(geo)
    leafletProxy("map") %>%
      addPolylines(
        data = geo,
        color = "purple",
        weight = 3,
        group = "importado"
      ) %>%
      fitBounds(
        lng1 = bbox["xmin"], lat1 = bbox["ymin"],
        lng2 = bbox["xmax"], lat2 = bbox["ymax"]
      )
    
    showNotification("Arquivo geográfico importado.", type = "message")
  })
  
  observeEvent(input$direcao_arquivo, {
    geo <- geometria_importada()
    if (is.null(geo)) return()
    
    leafletProxy("map") %>% clearGroup("importado")
    
    if (isTRUE(fr24_enriquecido())) {
      coords <- cbind(geo$lon, geo$lat)
      if (input$direcao_arquivo == "inverter") {
        coords <- coords[nrow(coords):1, , drop = FALSE]
      }
      leafletProxy("map") %>%
        addPolylines(
          lng = coords[, 1],
          lat = coords[, 2],
          color = "darkblue",
          weight = 3,
          group = "importado"
        ) %>%
        fitBounds(
          lng1 = min(coords[, 1]), lat1 = min(coords[, 2]),
          lng2 = max(coords[, 1]), lat2 = max(coords[, 2])
        )
    } else {
      bbox <- sf::st_bbox(geo)
      leafletProxy("map") %>%
        addPolylines(
          data = geo,
          color = "purple",
          weight = 3,
          group = "importado"
        ) %>%
        fitBounds(
          lng1 = bbox["xmin"], lat1 = bbox["ymin"],
          lng2 = bbox["xmax"], lat2 = bbox["ymax"]
        )
    }
  })
  
  observeEvent(input$adicionar_import, {
    geo <- geometria_importada()
    if (is.null(geo)) {
      showNotification("Nenhum arquivo importado.", type = "error")
      return()
    }
    
    # -------------------------------------------------------------------
    # IMPORTAÇÃO ESPECIAL: trilha de voo FR24 já enriquecida
    # -------------------------------------------------------------------
    if (isTRUE(fr24_enriquecido())) {
      # CORREÇÃO: geo já é o objeto correto (foi geometria_importada())
      # Não existe dados_import()
      
      traj <- tryCatch(
        limpar_trajeto_fr24(
          df      = geo,
          ts_col  = "timestamp",
          lon_col = "lon",         # CORREÇÃO: parse_kml_rich retorna "lon" não "longitude"
          lat_col = "lat"          # CORREÇÃO: parse_kml_rich retorna "lat" não "latitude"
        ),
        error = function(e) {
          showNotification(
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
      
      # distância com base na trilha ordenada
      distancia_km <- trajeto_distancia_km(coords)
      
      # sanity check básico: 0 < dist < 30.000 km
      if (distancia_km <= 0 || distancia_km > 30000) {
        showNotification(
          sprintf(
            "Distância absurda detectada para o voo (%.1f km). Algo está errado com a trilha.",
            distancia_km
          ),
          type = "error",
          duration = NULL
        )
        return()
      }
      
      # secondsFromGMT: usa TZ do primeiro ponto para todos
      lat1 <- coords[1, "lat"]
      lon1 <- coords[1, "lon"]
      tz   <- tz_from_coords(lat1, lon1)
      # FR24 já está em UTC; secondsFromGMT = offset do local
      sec_gmt <- as.integer(lubridate::with_tz(ts_utc[1], tz) - ts_utc[1])
      
      # cria samples já em UTC, com secondsFromGMT consistente
      # CORREÇÃO: extrai altitude, speed e heading corretamente do geo após limpeza
      n_samples <- length(ts_utc)
      
      # Extrai dados do geo (que já foi limpo e ordenado em limpar_trajeto_fr24)
      altitude_vec <- if ("altitude_m" %in% names(geo)) {
        # Filtra e ordena igual aos timestamps
        alt <- geo$altitude_m
        alt_ts <- parse_timestamp_utc(geo$timestamp)
        ok <- !is.na(alt_ts) & !is.na(alt)
        if (any(ok)) {
          # Ordena pela mesma ordem dos timestamps
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
        accuracy        = 5,  # Voos: 5 metros (não precisa snap-to-road)
        altitude        = altitude_vec,
        speed           = speed_vec,
        heading         = heading_vec,
        force_single_tz = TRUE,
        moving_state    = "moving"
      )
      
      sample_ids <- vapply(samples_novos, `[[`, "", "sampleId")
      
      # timeline item tipo voo_fr24 com activityType = airplane
      item <- criar_timeline_item_path(
        timestamps_utc = ts_utc,
        coords         = coords,
        sample_ids     = sample_ids,
        tipo           = "voo_fr24",
        descricao      = sprintf("Voo FR24 (%.1f km)", distancia_km),
        activity_type  = "airplane"  # CORREÇÃO: define como airplane para voos
      )
      
      # adiciona à timeline
      tl <- timeline()
      tl[[length(tl) + 1L]] <- item
      timeline(tl)
      
      # anexa samples
      s <- all_samples()
      all_samples(c(s, samples_novos))
      
      # desenha no mapa e ajusta bounds
      leafletProxy("map") %>%
        addPolylines(
          lng   = coords[, "lon"],
          lat   = coords[, "lat"],
          color = "purple",
          weight = 4,
          group = "rota_atual"
        ) %>%
        fitBounds(
          lng1 = min(coords[, "lon"], na.rm = TRUE),
          lat1 = min(coords[, "lat"], na.rm = TRUE),
          lng2 = max(coords[, "lon"], na.rm = TRUE),
          lat2 = max(coords[, "lat"], na.rm = TRUE)
        )
      
      showNotification("Voo FR24 adicionado à timeline.", type = "message")
      return()
    }
    
    
    # Caso padrão (sf): timestamps vêm do formulário (horário local)
    if (!inherits(geo, "sf")) {
      showNotification("Geometria importada inválida.", type = "error")
      return()
    }
    
    coords_list <- sf::st_coordinates(geo)
    if (nrow(coords_list) < 2) {
      showNotification("Geometria muito curta para rota.", type = "error")
      return()
    }
    
    if (input$direcao_arquivo == "inverter") {
      coords_list <- coords_list[nrow(coords_list):1, , drop = FALSE]
    }
    
    # timezone da origem e destino
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
      showNotification("Horário inválido para rota importada.", type = "error")
      return()
    }
    
    inicio_utc <- with_tz(val$inicio, "UTC")
    fim_utc    <- with_tz(val$fim,    "UTC")
    
    if (has_overlap(timeline(), inicio_utc, fim_utc)) {
      showNotification("Este intervalo se sobrepõe a outra atividade existente.", type = "error")
      return()
    }
    
    # CORREÇÃO: faltava definir n0 e ts_seq
    n0 <- nrow(coords_list)
    n_target <- max(n0, 100L)
    
    # Gera sequência de timestamps
    ts_seq <- seq(inicio_utc, fim_utc, length.out = n_target)
    
    t0 <- seq(0, 1, length.out = n0)
    t_new <- seq(0, 1, length.out = n_target)
    
    # CORREÇÃO: coords_list tem colunas X e Y, não índices numéricos
    lon_new <- approx(t0, coords_list[, "X"], xout = t_new)$y
    lat_new <- approx(t0, coords_list[, "Y"], xout = t_new)$y
    
    coords_new <- cbind(lon_new, lat_new)
    
    samples_novos <- criar_locomotion_samples(
      coords         = coords_new,
      timestamps_utc = ts_seq,
      accuracy       = 0.01,  # Bem preciso para snap-to-road
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
    
    leafletProxy("map") %>%
      clearGroup("rota_atual") %>%
      addPolylines(
        lng = coords_new[, 1],
        lat = coords_new[, 2],
        color = "purple",
        weight = 3,
        group = "rota_atual"
      ) %>%
      fitBounds(
        lng1 = min(coords_new[, 1]), lat1 = min(coords_new[, 2]),
        lng2 = max(coords_new[, 1]), lat2 = max(coords_new[, 2])
      )
    
    showNotification("Rota do arquivo adicionada.", type = "message")
  })
  
  # ---- Timeline UI ----
  
  output$timeline_list <- renderUI({
    tl <- timeline()
    if (length(tl) == 0) {
      return(tags$p("Nenhum item na timeline ainda."))
    }
    
    # Ordena por startDate$date (UTC) mas exibe em local (do item)
    ord <- order(vapply(tl, function(it) it$startDate$date, character(1)))
    tl_ord <- tl[ord]
    
    # Constrói lista de cards
    tagList(
      lapply(seq_along(tl_ord), function(i) {
        it   <- tl_ord[[i]]
        tipo <- it$tipo %||% if (isTRUE(it$.isVisit)) "visita" else "rota"
        desc <- it$descricao %||% tipo
        
        # início local
        ts_ini_utc <- parse_timestamp_utc(it$startDate$date)
        sec_ini    <- it$startDate$secondsFromGMT %||% 0
        ts_ini_loc <- ts_ini_utc + sec_ini
        
        # fim local
        ts_fim_utc <- parse_timestamp_utc(it$endDate$date)
        sec_fim    <- it$endDate$secondsFromGMT %||% sec_ini
        ts_fim_loc <- ts_fim_utc + sec_fim
        
        # formata bonitinho (data + hora)
        ini_str <- format(ts_ini_loc, "%Y-%m-%d %H:%M")
        fim_str <- format(ts_fim_loc, "%Y-%m-%d %H:%M")
        
        div(
          class = "card mb-2 p-2",
          strong(sprintf("[%s] %s", tipo, desc)),
          br(),
          tags$small(paste0(ini_str, " → ", fim_str)),
          br(),
          actionButton(
            paste0("editar_", it$.internalId),
            "Editar",
            class = "btn-sm btn-warning"
          ),
          actionButton(
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
  observe({
    tl <- timeline()
    if (length(tl) == 0) return()
    
    ids <- vapply(tl, `[[`, "", ".internalId")
    
    lapply(ids, function(id) {
      observeEvent(input[[paste0("deletar_", id)]], {
        tl_atual <- timeline()
        idx <- which(vapply(tl_atual, `[[`, "", ".internalId") == id)
        if (length(idx)) {
          # item que está sendo removido
          it <- tl_atual[[idx]]
          tl_atual <- tl_atual[-idx]
          
          # remove samples associados
          if (!is.null(it$samples)) {
            s <- all_samples()
            keep <- !vapply(s, function(x) x$sampleId %in% it$samples, logical(1))
            all_samples(s[keep])
          }
          
          timeline(tl_atual)
          
          # Limpa TODOS os grupos relacionados a rotas ao deletar
          leafletProxy("map") %>%
            clearGroup("rota_atual") %>%
            clearGroup("importado") %>%
            clearGroup("waypoints")
          
          # Limpa também os pontos temporários (não precisa atualizar mapa)
          pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))
          
          showNotification("Item removido da timeline.", type = "message")
        }
      }, ignoreInit = TRUE)
      
    })
  })
  
  # Editar
  observe({
    tl <- timeline()
    if (length(tl) == 0) return()
    
    ids <- vapply(tl, `[[`, "", ".internalId")
    
    lapply(ids, function(id) {
      observeEvent(input[[paste0("editar_", id)]], {
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
        
        showModal(
          modalDialog(
            title = "Editar item",
            dateInput("edit_data_inicio", "Data início", as.Date(ts_local)),
            textInput("edit_hora_inicio", "Hora início", format(ts_local, "%H:%M")),
            dateInput("edit_data_fim", "Data fim", as.Date(tsf_local)),
            textInput("edit_hora_fim", "Hora fim", format(tsf_local, "%H:%M")),
            footer = tagList(
              modalButton("Cancelar"),
              actionButton("salvar_edicao", "Salvar", class = "btn-primary")
            )
          )
        )
        
        # guarda id em session
        session$userData$item_editando <- id
      }, ignoreInit = TRUE)
    })
  })
  
  observeEvent(input$salvar_edicao, {
    removeModal()
    id <- session$userData$item_editando
    if (is.null(id)) return()
    
    tl <- timeline()
    idx <- which(vapply(tl, `[[`, "", ".internalId") == id)
    if (!length(idx)) return()
    it <- tl[[idx]]
    
    # Reconstrói horário local com base no secondsFromGMT existente
    start_sec <- it$startDate$secondsFromGMT %||% 0
    end_sec   <- it$endDate$secondsFromGMT %||% start_sec
    
    hora_ini_fmt <- formatar_hora(input$edit_hora_inicio)
    hora_fim_fmt <- formatar_hora(input$edit_hora_fim)
    
    local_ini <- ymd_hms(paste(input$edit_data_inicio, hora_ini_fmt), tz = "UTC")
    local_fim <- ymd_hms(paste(input$edit_data_fim, hora_fim_fmt), tz = "UTC")
    
    if (is.na(local_ini) || is.na(local_fim)) {
      showNotification("Horários inválidos.", type = "error")
      return()
    }
    
    # local = utc + secondsFromGMT  =>  utc = local - secondsFromGMT
    novo_ini_utc <- local_ini - start_sec
    novo_fim_utc <- local_fim  - end_sec
    
    if (novo_fim_utc <= novo_ini_utc) {
      showNotification("O fim deve ser depois do início.", type = "error")
      return()
    }
    
    # Checa se a edição cria overlap com outros itens
    if (has_overlap(tl, novo_ini_utc, novo_fim_utc, ignore_id = id)) {
      showNotification("Este intervalo se sobrepõe a outra atividade existente.", type = "error")
      return()
    }
    
    
    it$startDate$date <- format(novo_ini_utc, "%Y-%m-%dT%H:%M:%SZ")
    it$endDate$date   <- format(novo_fim_utc, "%Y-%m-%dT%H:%M:%SZ")
    
    # Atualiza samples desse item (redistribui no novo intervalo)
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
          # recalcula secondsFromGMT com base na coordenada do sample
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
    showNotification("Item atualizado.", type = "message")
  })
  
  # ---- Limpar tudo ----
  
  observeEvent(input$limpar_tudo, {
    timeline(list())
    all_samples(list())
    pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))
    leafletProxy("map") %>%
      clearGroup("waypoints") %>%
      clearGroup("rota_atual") %>%
      clearGroup("importado")
    showNotification("Timeline, samples e mapa limpos.", type = "message")
  })
  
  # ---- Download Arc ----
  
  output$download_arc <- downloadHandler(
    filename = function() {
      paste0("arc_import_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      tmpdir <- tempfile("arc_import_")
      dir.create(tmpdir)
      tl <- timeline()
      s <- all_samples()
      
      if (length(tl) == 0 || length(s) == 0) {
        showNotification("Não há itens/samples para exportar.", type = "error")
        return()
      }
      
      # SEMPRE gera samples_apagar dos arquivos semanais
      exportar_arc_json(tl, s, tmpdir, 
                        data_trabalho = input$data_trabalho)
      
      owd <- setwd(tmpdir)
      on.exit(setwd(owd), add = TRUE)
      
      # compacta pasta Import
      zip::zipr(zipfile = file, files = "Import")
    }
  )
  
  # ---- Download GPX ----
  
  output$download_gpx <- downloadHandler(
    filename = function() {
      paste0("samples_", format(Sys.Date(), "%Y%m%d"), ".gpx")
    },
    content = function(file) {
      s <- all_samples()
      if (length(s) == 0) {
        showNotification("Nenhum sample para exportar.", type = "error")
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

shinyApp(ui, server)