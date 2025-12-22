#############################
# Arc Timeline Builder 2.0
# Versão modular
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
library(geosphere)
library(zip)

# ---- Configurações locais (dados privados) ----
source("config_local.R", local = FALSE)

# ---- Carregar módulos ----
# Ordem importante: módulos base primeiro, depois os que dependem deles

source("R/utils_time.R", local = FALSE)
source("R/utils_geo.R", local = FALSE)
source("R/utils_osrm.R", local = FALSE)
source("R/utils_arc.R", local = FALSE)
source("R/utils_semanas.R", local = FALSE)
source("R/export_arc.R", local = FALSE)
source("R/ui.R", local = FALSE)
source("R/server.R", local = FALSE)

# ---- Iniciar aplicação ----
shinyApp(ui, server)
