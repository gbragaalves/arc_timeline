#!/usr/bin/env Rscript
#############################
# Pré-processador de Location History
# Execute uma vez para converter JSON -> RDS particionado por mês
# Uso: Rscript preprocessar_location_history.R
#############################

library(jsonlite)
library(lubridate)

# Carrega configuração de pessoas
if (file.exists("R/config_pessoas.R")) {
  source("R/config_pessoas.R")
} else {
  stop("Arquivo R/config_pessoas.R não encontrado!")
}

# Diretório de cache
CACHE_DIR <- "location_history/cache"
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

# Parseia string "geo:lat,lon"
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

# Processa um arquivo JSON de Location History
processar_arquivo <- function(arquivo_json, pessoa_id) {
  cat("Processando:", arquivo_json, "\n")

  if (!file.exists(arquivo_json)) {
    cat("  ERRO: Arquivo não encontrado!\n")
    return(NULL)
  }

  cat("  Lendo JSON...\n")
  dados <- tryCatch(
    fromJSON(arquivo_json, simplifyVector = FALSE),
    error = function(e) {
      cat("  ERRO ao ler JSON:", e$message, "\n")
      NULL
    }
  )

  if (is.null(dados) || length(dados) == 0) {
    cat("  ERRO: Arquivo vazio ou inválido\n")
    return(NULL)
  }

  cat("  Parseando", length(dados), "registros...\n")

  registros <- lapply(seq_along(dados), function(i) {
    if (i %% 1000 == 0) cat("    ", i, "/", length(dados), "\r")

    item <- dados[[i]]

    start_time <- tryCatch({
      ts <- ymd_hms(item$startTime)
      with_tz(ts, "UTC")
    }, error = function(e) NA)

    end_time <- tryCatch({
      ts <- ymd_hms(item$endTime)
      with_tz(ts, "UTC")
    }, error = function(e) NA)

    if (is.na(start_time) || is.na(end_time)) return(NULL)

    if (!is.null(item$visit)) {
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
  cat("\n")

  registros <- Filter(Negate(is.null), registros)
  if (length(registros) == 0) {
    cat("  ERRO: Nenhum registro válido\n")
    return(NULL)
  }

  df <- do.call(rbind, registros)
  cat("  Total de registros válidos:", nrow(df), "\n")

  # Adiciona coluna de ano-mês para particionamento
  df$ano_mes <- format(df$start_time, "%Y-%m")

  # Particiona por mês
  meses <- unique(df$ano_mes)
  cat("  Salvando", length(meses), "arquivos RDS (um por mês)...\n")

  pessoa_dir <- file.path(CACHE_DIR, pessoa_id)
  dir.create(pessoa_dir, recursive = TRUE, showWarnings = FALSE)

  for (mes in meses) {
    df_mes <- df[df$ano_mes == mes, ]
    df_mes$ano_mes <- NULL  # Remove coluna auxiliar

    rds_file <- file.path(pessoa_dir, paste0(mes, ".rds"))
    saveRDS(df_mes, rds_file)
    cat("    ", mes, ":", nrow(df_mes), "registros\n")
  }

  # Salva índice com range de datas
  indice <- data.frame(
    mes = meses,
    registros = sapply(meses, function(m) sum(df$ano_mes == m)),
    stringsAsFactors = FALSE
  )
  saveRDS(indice, file.path(pessoa_dir, "_indice.rds"))

  cat("  Concluído!\n\n")
  TRUE
}

# Processa todas as pessoas configuradas
cat("=== Pré-processador de Location History ===\n\n")

for (p in PESSOAS_CONFIG) {
  cat("Pessoa:", p$label, "(", p$id, ")\n")
  processar_arquivo(p$arquivo, p$id)
}

cat("=== Pré-processamento concluído! ===\n")
cat("Cache salvo em:", CACHE_DIR, "\n")
