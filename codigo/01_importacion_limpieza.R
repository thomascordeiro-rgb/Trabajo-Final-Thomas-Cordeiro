# =============================================================================
# 01_importacion_limpieza.R
# Trabajo final M37 - Ciencia de Datos y Sistemas Complejos (UBA)
# Replicación y extensión de Minoiu y Reyes (2013), "A network analysis of
# global banking: 1978-2010", Journal of Financial Stability 9(2).
#
# Este script descarga y limpia los datos. Uso: ejecutar primero este script
# y después 02_redes_resultados.R, ambos desde la misma carpeta.
#
# Etapas:
#   0. Configuración
#   1. Descarga de las estadísticas bancarias locacionales (LBS) del BIS
#   2. Lectura y verificación de la base cruda
#   3. Limpieza
#   4. Clasificación de países (núcleo, declarantes)
#   5. Deflactor: IPC urbano de Estados Unidos
#   6. Guardado y resumen
#
# 
# =============================================================================

# -----------------------------------------------------------------------------
# Etapa 0. Configuración
# -----------------------------------------------------------------------------

# Carpeta de trabajo: la carpeta desde la que se ejecuta el script (en
# RStudio: Session > Set Working Directory > To Source File Location). Los
# datos y los resultados se guardan en las subcarpetas datos/ y resultados/,
# que se crean si no existen.
dir_base <- getwd()
dir_brutos     <- file.path(dir_base, "datos", "brutos")
dir_procesados <- file.path(dir_base, "datos", "procesados")
dir_tablas     <- file.path(dir_base, "resultados", "tablas")
dir.create(dir_brutos,     recursive = TRUE, showWarnings = FALSE)
dir.create(dir_procesados, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tablas,     recursive = TRUE, showWarnings = FALSE)

# TRUE: descarga de nuevo aunque ya exista el archivo crudo.
# FALSE: reutiliza el archivo guardado (resultados reproducibles).
forzar_descarga <- FALSE

# Consulta a la API SDMX del BIS (v1). Cada posición de la clave es una
# dimensión de la base (ver BIS, "LBS data structure definition"):

#   Q    frecuencia trimestral
#   F    flujos ajustados por tipo de cambio y quiebres (no stocks)
#   C    activos (claims)
#   A    todos los instrumentos
#   TO1  todas las monedas
#   A    todos los tipos de moneda
#   5J   todos los países de origen del banco (criterio de residencia)
#   A    todos los bancos declarantes
#   (vacío) todos los países declarantes
#   A    todos los sectores de la contraparte
#   (vacío) todos los países contraparte
#   N    posiciones transfronterizas

clave_lbs   <- "Q.F.C.A.TO1.A.5J.A..A..N"
periodo_ini <- "1977-Q1"
url_lbs <- paste0("https://stats.bis.org/api/v1/data/WS_LBS_D_PUB/",
                  clave_lbs, "?format=csv&startPeriod=", periodo_ini)
archivo_lbs <- file.path(dir_brutos, "bis_lbs_flujos_claims.csv")

# IPC urbano de EE.UU. (CPI-U, mensual, desestacionalizado), FRED.
url_ipc     <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL"
archivo_ipc <- file.path(dir_brutos, "fred_cpiaucsl.csv")
anio_base   <- 2009   # el paper expresa los flujos en USD constantes de 2009

# Núcleo del paper: los 15 declarantes con datos bilaterales continuos.
nucleo_paper <- c("AT", "BE", "CA", "DK", "FR", "DE", "IE", "IT",
                  "JP", "LU", "NL", "SE", "CH", "GB", "US")

options(timeout = 900)   # la descarga del BIS pesa unos 40 MB


# -----------------------------------------------------------------------------
# Etapa 1. Descarga
# -----------------------------------------------------------------------------

descargar <- function(url, destino, forzar) {
  if (file.exists(destino) && !forzar) {
    message("Ya existe, no se descarga: ", basename(destino))
    return(invisible(destino))
  }
  for (intento in 1:3) {
    ok <- tryCatch({
      download.file(url, destino, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      message("Intento ", intento, " fallido: ", conditionMessage(e))
      FALSE
    })
    if (ok) {
      message("Descargado: ", basename(destino))
      return(invisible(destino))
    }
    Sys.sleep(5)
  }
  stop("No se pudo descargar ", url,
       ". Alternativa: bajar el archivo a mano y guardarlo como ", destino)
}

descargar(url_lbs, archivo_lbs, forzar_descarga)
descargar(url_ipc, archivo_ipc, forzar_descarga)


# -----------------------------------------------------------------------------
# Etapa 2. Lectura y verificación de la base "cruda"
# -----------------------------------------------------------------------------

# Todo se lee como texto. na.strings = "" evita que el código de Namibia
# ("NA") se convierta en dato faltante.
lbs_crudo <- read.csv(archivo_lbs, colClasses = "character",
                      na.strings = "", check.names = FALSE)

cat("\nBase cruda:", nrow(lbs_crudo), "filas,", ncol(lbs_crudo), "columnas\n")

# La consulta fija todas las dimensiones salvo los dos países: cada una de
# esas columnas debe tener un único valor.
fijas <- c("FREQ", "L_MEASURE", "L_POSITION", "L_INSTR", "L_DENOM",
           "L_CURR_TYPE", "L_PARENT_CTY", "L_REP_BANK_TYPE",
           "L_CP_SECTOR", "L_POS_TYPE")
stopifnot(all(fijas %in% names(lbs_crudo)))
stopifnot(all(sapply(lbs_crudo[fijas], function(x) length(unique(x)) == 1)))

# Unidad: millones de USD (UNIT_MEASURE = USD, UNIT_MULT = 6).
stopifnot(all(lbs_crudo$UNIT_MEASURE == "USD"),
          all(lbs_crudo$UNIT_MULT == "6"))

cat("Estado de las observaciones (OBS_STATUS):\n")
print(table(lbs_crudo$OBS_STATUS, useNA = "ifany"))


# -----------------------------------------------------------------------------
# Etapa 3. Limpieza
# -----------------------------------------------------------------------------

lbs <- data.frame(
  declarante  = lbs_crudo$L_REP_CTY,
  contraparte = lbs_crudo$L_CP_COUNTRY,
  periodo     = lbs_crudo$TIME_PERIOD,
  flujo_musd  = suppressWarnings(as.numeric(lbs_crudo$OBS_VALUE)),
  estado_obs  = lbs_crudo$OBS_STATUS,
  stringsAsFactors = FALSE
)

# 3.1 Agregados del BIS. Los países tienen código ISO de dos letras; los
#     agregados (regiones, totales, "todos los declarantes") llevan un dígito.
es_pais <- function(x) grepl("^[A-Z]{2}$", x)
cat("\nDeclarantes agregados eliminados:",
    sort(unique(lbs$declarante[!es_pais(lbs$declarante)])), "\n")
cat("Contrapartes agregadas eliminadas:",
    sort(unique(lbs$contraparte[!es_pais(lbs$contraparte)])), "\n")
lbs <- lbs[es_pais(lbs$declarante) & es_pais(lbs$contraparte), ]

# 3.2 Observaciones sin valor numérico. Son valores suprimidos por
#     confidencialidad (OBS_STATUS = "Q"), no flujos nulos. Se registra su
#     peso por declarante antes de eliminarlas (se usa en la Etapa 4),
#     solo en los trimestres en que el declarante publica algún valor.
sin_valor <- is.na(lbs$flujo_musd)
cat("Filas sin valor eliminadas:", sum(sin_valor), "\n")
cat("Estado de las filas sin valor:\n")
print(table(lbs$estado_obs[sin_valor]))
clave_dp   <- paste(lbs$declarante, lbs$periodo)
activo     <- clave_dp %in% clave_dp[!sin_valor]
suprimidas <- aggregate(
  cbind(n_filas, n_suprimidas) ~ declarante,
  data = data.frame(declarante   = lbs$declarante[activo],
                    n_filas      = 1L,
                    n_suprimidas = as.integer(sin_valor[activo])),
  FUN = sum)
lbs <- lbs[!sin_valor, ]

# 3.3 Posiciones de un país consigo mismo.
propias <- lbs$declarante == lbs$contraparte
cat("Filas declarante = contraparte eliminadas:", sum(propias), "\n")
lbs <- lbs[!propias, ]

# 3.4 Un único valor por par de países y trimestre.
stopifnot(!anyDuplicated(lbs[c("declarante", "contraparte", "periodo")]))

# 3.5 Tiempo: "1978-Q1" -> año y trimestre.
stopifnot(all(grepl("^[0-9]{4}-Q[1-4]$", lbs$periodo)))
lbs$anio      <- as.integer(substr(lbs$periodo, 1, 4))
lbs$trimestre <- as.integer(substr(lbs$periodo, 7, 7))

lbs <- lbs[order(lbs$declarante, lbs$contraparte, lbs$anio, lbs$trimestre), ]
rownames(lbs) <- NULL


# -----------------------------------------------------------------------------
# Etapa 4. Clasificación de países
# -----------------------------------------------------------------------------

# 4.1 Cobertura de cada declarante: primer y último trimestre con datos y
#     si reporta en todos los años completos desde 1978.
anio_ultimo_completo <- max(lbs$anio[lbs$trimestre == 4])
anios_completos <- 1978:anio_ultimo_completo

cobertura <- do.call(rbind, lapply(split(lbs, lbs$declarante), function(d) {
  data.frame(
    declarante      = d$declarante[1],
    primer_periodo  = min(d$periodo),
    ultimo_periodo  = max(d$periodo),
    n_trimestres    = length(unique(d$periodo)),
    # trimestres sin ningún valor entre el primero y el último
    n_faltantes     = 4 * (max(d$anio) - min(d$anio)) + 4 -
                      min(d$trimestre[d$anio == min(d$anio)]) + 1 -
                      (4 - max(d$trimestre[d$anio == max(d$anio)])) -
                      length(unique(d$periodo)),
    n_contrapartes  = length(unique(d$contraparte)),
    continuo_1978   = all(anios_completos %in% d$anio),
    nucleo_paper    = d$declarante[1] %in% nucleo_paper
  )
}))
rownames(cobertura) <- NULL

# Porcentaje de observaciones suprimidas (calculado en 3.2).
cobertura <- merge(cobertura, suprimidas, by = "declarante")
cobertura$pct_suprimidas <- round(100 * cobertura$n_suprimidas /
                                    cobertura$n_filas, 1)
cobertura$n_filas <- NULL
cobertura$n_suprimidas <- NULL
cobertura <- cobertura[order(cobertura$primer_periodo, cobertura$declarante), ]

cat("\nCobertura de los declarantes:\n")
print(cobertura, row.names = FALSE)

declarantes_continuos <- cobertura$declarante[cobertura$continuo_1978]
cat("\nDeclarantes continuos desde 1978:", declarantes_continuos, "\n")
cat("Núcleo del paper sin serie continua en los datos públicos:",
    setdiff(nucleo_paper, declarantes_continuos), "\n")

# 4.2 Marcas por fila.
#  - nucleo_paper: el declarante pertenece al núcleo de 15 del paper.
#  - nucleo_continuo: el declarante reporta sin interrupción desde 1978.
#  - contraparte_declara: la contraparte también reporta ese trimestre
#    (necesario para la red núcleo-núcleo).
lbs$nucleo_paper    <- lbs$declarante %in% nucleo_paper
lbs$nucleo_continuo <- lbs$declarante %in% declarantes_continuos
declarantes_periodo <- unique(lbs[c("declarante", "periodo")])
lbs$contraparte_declara <- paste(lbs$contraparte, lbs$periodo) %in%
  paste(declarantes_periodo$declarante, declarantes_periodo$periodo)


# -----------------------------------------------------------------------------
# Etapa 5. Deflactor: IPC urbano de EE.UU., promedio anual, base 2009 = 1
# -----------------------------------------------------------------------------

ipc_crudo <- read.csv(archivo_ipc, colClasses = "character")
names(ipc_crudo) <- c("fecha", "ipc")   # FRED cambió el nombre de la fecha
ipc_crudo$anio <- as.integer(substr(ipc_crudo$fecha, 1, 4))
ipc_crudo$ipc  <- suppressWarnings(as.numeric(ipc_crudo$ipc))
ipc_crudo <- ipc_crudo[!is.na(ipc_crudo$ipc), ]

# Promedio anual de los meses disponibles; la columna "meses" indica cuántos
# (2025: 11 meses, falta octubre; 2026 no se usa en el análisis).
meses <- table(ipc_crudo$anio)
ipc <- aggregate(ipc ~ anio, data = ipc_crudo, FUN = mean)
ipc$meses <- as.integer(meses[as.character(ipc$anio)])
stopifnot(anio_base %in% ipc$anio)
ipc$deflactor <- ipc$ipc / ipc$ipc[ipc$anio == anio_base]
ipc <- ipc[ipc$anio >= min(lbs$anio), ]

stopifnot(all(unique(lbs$anio) %in% ipc$anio))
cat("\nDeflactor (base ", anio_base, " = 1), primeros y últimos años:\n", sep = "")
print(rbind(head(ipc, 3), tail(ipc, 3)), row.names = FALSE)


# -----------------------------------------------------------------------------
# Etapa 6. Guardado y resumen
# -----------------------------------------------------------------------------

saveRDS(lbs,       file.path(dir_procesados, "lbs_trimestral_limpio.rds"))
saveRDS(ipc,       file.path(dir_procesados, "ipc_eeuu_anual.rds"))
write.csv(cobertura, file.path(dir_procesados, "cobertura_declarantes.csv"),
          row.names = FALSE)
# Copia para el anexo del trabajo.
write.csv(cobertura, file.path(dir_tablas, "datos_cobertura_declarantes.csv"),
          row.names = FALSE)

cat("\nResumen de la base limpia\n",
    "  Filas:        ", nrow(lbs), "\n",
    "  Período:      ", min(lbs$periodo), "a", max(lbs$periodo), "\n",
    "  Declarantes:  ", length(unique(lbs$declarante)), "\n",
    "  Contrapartes: ", length(unique(lbs$contraparte)), "\n",
    "  Archivos en:  ", dir_procesados, "\n")
