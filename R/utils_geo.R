# ---- Helpers: Parsers geográficos e cálculos ----

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
