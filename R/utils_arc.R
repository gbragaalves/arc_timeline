# ---- Helpers Arc: samples & timeline items ----

# Operador auxiliar para null coalescing
`%||%` <- function(x, y) if (!is.null(x)) x else y

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
  if (is.null(heading))  heading  <- calcular_bearings(coords)

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
      sampleId       = uuid::UUIDgenerate(use.time = TRUE),
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

  inicio_utc <- lubridate::with_tz(inicio_local, tzone = "UTC")
  fim_utc    <- lubridate::with_tz(fim_local,    tzone = "UTC")

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

  item_id <- uuid::UUIDgenerate(use.time = TRUE)

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

  item_id <- uuid::UUIDgenerate(use.time = TRUE)

  item <- list(
    .internalId = item_id,
    .isVisit = FALSE,
    activityType = activity_type,
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
