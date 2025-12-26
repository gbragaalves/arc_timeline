# ---- Helpers: Parsers geográficos e cálculos ----

# Calcula bearing geodésico entre pontos consecutivos
# coords: matriz n x 2 (lon, lat)
# Retorna vetor de n bearings (0-360 graus)
calcular_bearings <- function(coords) {
  # Converte para matriz numérica se necessário
  if (is.data.frame(coords)) coords <- as.matrix(coords)
  if (!is.matrix(coords)) {
    coords <- matrix(as.numeric(unlist(coords)), ncol = 2, byrow = TRUE)
  }
  storage.mode(coords) <- "double"

  n <- nrow(coords)
  if (is.null(n) || n < 2) return(rep(0, max(1, n)))

  bearings <- numeric(n)

  for (i in seq_len(n - 1)) {
    p1 <- c(as.numeric(coords[i, 1]), as.numeric(coords[i, 2]))
    p2 <- c(as.numeric(coords[i + 1, 1]), as.numeric(coords[i + 1, 2]))
    bearings[i] <- geosphere::bearing(p1, p2)
  }

  # Último ponto: mantém o mesmo bearing do penúltimo
  bearings[n] <- bearings[n - 1]

  # Normaliza para 0-360 (bearing pode retornar negativos)
  bearings <- bearings %% 360

  bearings
}

# Limpa e ordena trilha de voo FR24
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

# Distância de um trajeto em km (pares consecutivos)
trajeto_distancia_km <- function(coords) {
  n <- nrow(coords)
  if (is.null(n) || n < 2) return(0)

  dists_m <- geosphere::distHaversine(
    coords[-n, , drop = FALSE],
    coords[-1, , drop = FALSE]
  )
  sum(dists_m) / 1000
}


# Parser KML enriquecido (FlightRadar24)
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


# ---- Location History do Google ----

# Carrega e parseia arquivo Location History do Google
# Retorna data.frame com: start_time, end_time, lat, lon, tipo (visit/activity)
carregar_location_history <- function(arquivo) {
  if (!file.exists(arquivo)) return(NULL)

  dados <- tryCatch(
    jsonlite::fromJSON(arquivo, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(dados) || length(dados) == 0) return(NULL)

  registros <- lapply(dados, function(item) {
    # Parseia ISO8601 com offset (ex: 2024-09-10T08:33:50.000-03:00)
    start_time <- tryCatch({
      ts <- lubridate::ymd_hms(item$startTime)
      lubridate::with_tz(ts, "UTC")
    }, error = function(e) NA)

    end_time <- tryCatch({
      ts <- lubridate::ymd_hms(item$endTime)
      lubridate::with_tz(ts, "UTC")
    }, error = function(e) NA)

    if (is.na(start_time) || is.na(end_time)) return(NULL)

    # Extrai coordenadas
    if (!is.null(item$visit)) {
      # É uma visita
      loc_str <- item$visit$topCandidate$placeLocation
      coords <- parse_geo_string(loc_str)
      if (is.null(coords)) return(NULL)

      data.frame(
        start_time = start_time,
        end_time   = end_time,
        lat        = coords$lat,
        lon        = coords$lon,
        lat_end    = coords$lat,
        lon_end    = coords$lon,
        tipo       = "visit",
        stringsAsFactors = FALSE
      )
    } else if (!is.null(item$activity)) {
      # É uma atividade (trajeto)
      start_coords <- parse_geo_string(item$activity$start)
      end_coords   <- parse_geo_string(item$activity$end)
      if (is.null(start_coords) || is.null(end_coords)) return(NULL)

      data.frame(
        start_time = start_time,
        end_time   = end_time,
        lat        = start_coords$lat,
        lon        = start_coords$lon,
        lat_end    = end_coords$lat,
        lon_end    = end_coords$lon,
        tipo       = "activity",
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  registros <- Filter(Negate(is.null), registros)
  if (length(registros) == 0) return(NULL)

  do.call(rbind, registros)
}

# Parseia string "geo:lat,lon" para lista(lat, lon)
parse_geo_string <- function(geo_str) {
  if (is.null(geo_str) || !grepl("^geo:", geo_str)) return(NULL)

  coords_str <- sub("^geo:", "", geo_str)
  parts <- strsplit(coords_str, ",")[[1]]
  if (length(parts) < 2) return(NULL)

  lat <- as.numeric(parts[1])
  lon <- as.numeric(parts[2])

  if (is.na(lat) || is.na(lon)) return(NULL)

  list(lat = lat, lon = lon)
}

# Filtra Location History por data (considera timezone local)
filtrar_location_history_por_data <- function(df, data_alvo, tz_local = Sys.timezone()) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Converte data_alvo para range de timestamps UTC
  inicio_local <- lubridate::ymd_hms(paste(data_alvo, "00:00:00"), tz = tz_local)
  fim_local    <- lubridate::ymd_hms(paste(data_alvo, "23:59:59"), tz = tz_local)

  inicio_utc <- lubridate::with_tz(inicio_local, "UTC")
  fim_utc    <- lubridate::with_tz(fim_local, "UTC")

  # Filtra registros que se sobrepõem ao dia
  df_filtrado <- df[
    (df$start_time <= fim_utc & df$end_time >= inicio_utc),
  ]

  if (nrow(df_filtrado) == 0) return(NULL)

  df_filtrado
}

# ---- Carregamento otimizado via cache RDS ----

CACHE_DIR <- "location_history/cache"

# Verifica se cache existe para uma pessoa
cache_existe <- function(pessoa_id) {
  indice_file <- file.path(CACHE_DIR, pessoa_id, "_indice.rds")
  file.exists(indice_file)
}

# Carrega dados de uma pessoa para uma data específica (via cache RDS)
carregar_lh_por_data <- function(pessoa_id, data_alvo, tz_local = Sys.timezone()) {
  pessoa_dir <- file.path(CACHE_DIR, pessoa_id)
  indice_file <- file.path(pessoa_dir, "_indice.rds")

  if (!file.exists(indice_file)) return(NULL)

  # Determina qual(is) mês(es) carregar
  # A data pode estar em UTC diferente do mês local
  inicio_local <- lubridate::ymd_hms(paste(data_alvo, "00:00:00"), tz = tz_local)
  fim_local    <- lubridate::ymd_hms(paste(data_alvo, "23:59:59"), tz = tz_local)

  inicio_utc <- lubridate::with_tz(inicio_local, "UTC")
  fim_utc    <- lubridate::with_tz(fim_local, "UTC")

  # Meses que podem conter dados do dia (considerando timezone)
  meses_necessarios <- unique(c(
    format(inicio_utc, "%Y-%m"),
    format(fim_utc, "%Y-%m")
  ))

  # Carrega apenas os RDS necessários
  dfs <- lapply(meses_necessarios, function(mes) {
    rds_file <- file.path(pessoa_dir, paste0(mes, ".rds"))
    if (file.exists(rds_file)) {
      readRDS(rds_file)
    } else {
      NULL
    }
  })

  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0) return(NULL)

  df <- do.call(rbind, dfs)

  # Filtra para o dia específico
  df_filtrado <- df[
    (df$start_time <= fim_utc & df$end_time >= inicio_utc),
  ]

  if (nrow(df_filtrado) == 0) return(NULL)

  df_filtrado
}

# Formata data/hora para tooltip (ex: "05/jun. 14:30")
formatar_data_hora_lh <- function(timestamp, tz_local = Sys.timezone()) {
  ts_local <- lubridate::with_tz(timestamp, tz_local)

  meses <- c("jan.", "fev.", "mar.", "abr.", "mai.", "jun.",
             "jul.", "ago.", "set.", "out.", "nov.", "dez.")

  dia <- format(ts_local, "%d")
  mes <- meses[as.integer(format(ts_local, "%m"))]
  hora <- format(ts_local, "%H:%M")

  paste0(dia, "/", mes, " ", hora)
}
