# ---- Helpers: OSRM ----

# Servidores OSRM locais (ajuste portas se necessário)
OSRM_SERVERS <- list(
  car  = "http://127.0.0.1:5000",
  foot = "http://127.0.0.1:5001",
  bike = "http://127.0.0.1:5002",
  bus  = "http://127.0.0.1:5003"
)

check_osrm_server <- function(base_url) {
  # tenta uma request mínima, só pra ver se responde
  test_url <- paste0(base_url, "/route/v1/driving/0,0;0,0")
  res <- tryCatch(
    httr::GET(test_url, httr::timeout(2)),
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

  res <- tryCatch(httr::GET(url, httr::timeout(10)), error = function(e) NULL)
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
