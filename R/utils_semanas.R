# ---- Helpers: semanas + samples_apagar + overlap ----

# Dado um intervalo [ini_utc, fim_utc], gera nomes de arquivos semanais envolvidos
semanas_para_intervalo <- function(ini_utc, fim_utc) {
  if (ini_utc > fim_utc) return(character(0))
  dias <- seq(lubridate::floor_date(ini_utc, "day"), lubridate::ceiling_date(fim_utc, "day"), by = "1 day")
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
