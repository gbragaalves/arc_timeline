# ---- Helpers: OSRM ----

# Suaviza rota OSRM removendo pontos redundantes e aplicando simplificação
# coords: matriz n x 2 (lon, lat)
# tolerancia_m: distância mínima entre pontos consecutivos (metros)
# angulo_min: ângulo mínimo de mudança de direção para manter ponto (graus)
suavizar_rota_osrm <- function(coords, tolerancia_m = 5, angulo_min = 1) {
  if (!is.matrix(coords)) coords <- as.matrix(coords)
  n <- nrow(coords)
  if (n < 3) return(coords)

  # Passo 1: Remove pontos duplicados ou muito próximos
  manter <- rep(TRUE, n)
  for (i in 2:n) {
    dist <- geosphere::distHaversine(
      c(coords[i - 1, 1], coords[i - 1, 2]),
      c(coords[i, 1], coords[i, 2])
    )
    if (dist < tolerancia_m) {
      manter[i] <- FALSE
    }
  }
  # Sempre mantém primeiro e último
  manter[1] <- TRUE
  manter[n] <- TRUE
  coords <- coords[manter, , drop = FALSE]
  n <- nrow(coords)
  if (n < 3) return(coords)

  # Passo 2: Remove pontos em linha reta (sem mudança de direção significativa)
  manter <- rep(FALSE, n)
  manter[1] <- TRUE  # primeiro
  manter[n] <- TRUE  # último

  for (i in 2:(n - 1)) {
    # Bearing do segmento anterior
    b1 <- geosphere::bearing(
      c(coords[i - 1, 1], coords[i - 1, 2]),
      c(coords[i, 1], coords[i, 2])
    )
    # Bearing do segmento seguinte
    b2 <- geosphere::bearing(
      c(coords[i, 1], coords[i, 2]),
      c(coords[i + 1, 1], coords[i + 1, 2])
    )

    # Diferença angular (considerando wrap-around)
    diff_ang <- abs((b2 - b1 + 180) %% 360 - 180)

    # Mantém se mudança de direção > angulo_min
    if (diff_ang > angulo_min) {
      manter[i] <- TRUE
    }
  }

  coords <- coords[manter, , drop = FALSE]
  n <- nrow(coords)
  if (n < 2) return(coords)

  # Passo 3: Garante espaçamento mínimo entre pontos mantidos
  # (evita clusters de pontos muito próximos após filtragem por ângulo)
  final <- matrix(coords[1, ], nrow = 1)
  colnames(final) <- colnames(coords)

  for (i in 2:(n - 1)) {
    dist_ultimo <- geosphere::distHaversine(
      c(final[nrow(final), 1], final[nrow(final), 2]),
      c(coords[i, 1], coords[i, 2])
    )
    if (dist_ultimo >= tolerancia_m) {
      final <- rbind(final, coords[i, ])
    }
  }

  # Sempre inclui o último ponto
  final <- rbind(final, coords[n, ])

  final
}

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

  res <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
  if (is.null(res) || res$status_code >= 400) return(NULL)

  js <- httr::content(res, as = "parsed", encoding = "UTF-8")
  if (is.null(js$routes) || length(js$routes) == 0) return(NULL)

  route <- js$routes[[1]]
  coords <- route$geometry$coordinates

  # Converte lista de coordenadas para matriz numérica
  n_coords <- length(coords)
  coords_mat <- matrix(0, nrow = n_coords, ncol = 2)
  for (i in seq_len(n_coords)) {
    pt <- coords[[i]]
    if (is.list(pt)) pt <- unlist(pt)
    coords_mat[i, 1] <- as.numeric(pt[1])  # lon
    coords_mat[i, 2] <- as.numeric(pt[2])  # lat
  }
  colnames(coords_mat) <- c("lon", "lat")

  list(
    coords      = coords_mat,
    distance_m  = as.numeric(route$distance),
    duration_s  = as.numeric(route$duration)
  )
}
