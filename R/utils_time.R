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
  local_time <- lubridate::with_tz(ts_utc, tzone = tz)
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
