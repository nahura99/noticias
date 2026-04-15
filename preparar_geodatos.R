# ====================================================================
# Ejecutar UNA SOLA VEZ para pre-cachear los geodatos de Uruguay.
# Usa rnaturalearth (disponible en CRAN).
# El archivo data/geodatos.RData resultante se usa al iniciar la app.
# ====================================================================

if (!requireNamespace("sf",                 quietly = TRUE)) install.packages("sf")
if (!requireNamespace("dplyr",              quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("rnaturalearth",      quietly = TRUE)) install.packages("rnaturalearth")
if (!requireNamespace("rnaturalearthdata",  quietly = TRUE)) install.packages("rnaturalearthdata")

library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

sf_use_s2(FALSE)

message("Descargando mapa departamental de Uruguay...")
raw <- ne_states(country = "Uruguay", returnclass = "sf")

# Normalizar nombre a TitleCase en columna NAME_1
raw$NAME_1 <- tools::toTitleCase(tolower(raw$name))

# Correcciones de nombres para que coincidan con el diccionario de la app
raw$NAME_1 <- dplyr::recode(raw$NAME_1,
  "Cerro Largo"    = "Cerro Largo",
  "Rio Negro"      = "Rio Negro",
  "San Jose"       = "San Jose",
  "Treinta Y Tres" = "Treinta y Tres",
  "Tacuarembo"     = "Tacuarembo",
  "Paysandu"       = "Paysandu"
)

message("Procesando geometrias...")
uruguay_map <- raw %>%
  select(NAME_1, geometry) %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 0.0005, preserveTopology = TRUE)

message("Calculando centroides...")
cents  <- suppressWarnings(st_centroid(uruguay_map))
coords <- st_coordinates(cents)

uruguay_centroids <- data.frame(
  NAME_1 = uruguay_map$NAME_1,
  lng    = coords[, 1],
  lat    = coords[, 2],
  stringsAsFactors = FALSE
)

if (!dir.exists("data")) dir.create("data")

message("Guardando data/geodatos.RData...")
save(uruguay_map, uruguay_centroids, file = "data/geodatos.RData")
message("Listo. Departamentos incluidos:")
message(paste(" -", uruguay_map$NAME_1, collapse = "\n"))
