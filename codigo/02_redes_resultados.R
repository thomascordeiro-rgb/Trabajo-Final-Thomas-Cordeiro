# =============================================================================
# 02_redes_resultados.R
# Trabajo final M37 - Ciencia de Datos y Sistemas Complejos (UBA)
# Replicación y extensión de Minoiu y Reyes (2013), "A network analysis of
# global banking: 1978-2010", Journal of Financial Stability 9(2).
#
# Construye las redes bancarias anuales a partir de la base limpia de
# 01_importacion_limpieza.R, replica los resultados del paper y los extiende.
# Uso: ejecutar después de 01_importacion_limpieza.R, desde la misma carpeta.
#
# Etapas:
#   0. Configuración y carga de datos
#   1. Panel anual en dólares constantes
#   2. Construcción de las redes
#   3. Indicadores por nodo: grado, fuerza y HHI
#   4. Coeficientes de clustering (Fagiolo, 2007)
#   5. Indicadores por red y Tabla 1 del paper
#   6. Figura 3 del paper: indicadores de red, 1978-2025
#   7. Estudio de eventos alrededor de las crisis (Tabla 5 y Figura 8)
#   8. Robustez: núcleo sin Países Bajos
#
# Salidas: resultados/tablas y resultados/figuras. Prefijos de los archivos:
#   rep_ = réplica del paper; rob_ = robustez.
#
#
# =============================================================================

# -----------------------------------------------------------------------------
# Etapa 0. Configuración y carga de datos
# -----------------------------------------------------------------------------

# Carpeta de trabajo: la carpeta desde la que se ejecuta el script (en
# RStudio: Session > Set Working Directory > To Source File Location). Los
# datos y los resultados se guardan en las subcarpetas datos/ y resultados/,
# que se crean si no existen.
dir_base <- getwd()
dir_brutos     <- file.path(dir_base, "datos", "brutos")
dir_procesados <- file.path(dir_base, "datos", "procesados")
dir_tablas     <- file.path(dir_base, "resultados", "tablas")
dir_figuras    <- file.path(dir_base, "resultados", "figuras")
dir.create(dir_tablas,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_figuras, recursive = TRUE, showWarnings = FALSE)

lbs       <- readRDS(file.path(dir_procesados, "lbs_trimestral_limpio.rds"))
ipc       <- readRDS(file.path(dir_procesados, "ipc_eeuu_anual.rds"))
cobertura <- read.csv(file.path(dir_procesados, "cobertura_declarantes.csv"))

# Núcleo: declarantes con datos continuos desde 1978 (12 países).
nucleo <- cobertura$declarante[cobertura$continuo_1978]

# Período: hasta el último año con los cuatro trimestres.
anios <- 1978:2025

redes <- c("completa", "nucleo_periferia", "nucleo_nucleo")

# Guarda una tabla en resultados/tablas.
guardar_tabla <- function(x, nombre) {
  write.csv(x, file.path(dir_tablas, nombre), row.names = FALSE)
}

cat("Núcleo (", length(nucleo), " países): ", paste(nucleo, collapse = " "),
    "\n", sep = "")


# -----------------------------------------------------------------------------
# Etapa 1. Panel anual en dólares constantes
# -----------------------------------------------------------------------------

# Para un conjunto de prestamistas (nuc) y un período:
#   1.1 se toman sus préstamos en esos años;
#   1.2 flujo anual de cada par = suma de los flujos trimestrales disponibles;
#   1.3 se pasa a dólares constantes de 2009 y a miles de millones;
#   1.4 peso del vínculo: los flujos netos negativos (repagos) pasan a cero.
armar_panel_anual <- function(nuc, periodo = anios) {
  x <- lbs[lbs$declarante %in% nuc & lbs$anio %in% periodo, ]
  suma <- aggregate(flujo_musd ~ declarante + contraparte + anio,
                    data = x, FUN = sum)
  n_trim <- aggregate(flujo_musd ~ declarante + contraparte + anio,
                      data = x, FUN = length)
  names(n_trim)[4] <- "n_trimestres"
  suma <- merge(suma, n_trim, by = c("declarante", "contraparte", "anio"))
  suma <- merge(suma, ipc[c("anio", "deflactor")], by = "anio")
  suma$flujo_real_mm <- suma$flujo_musd / suma$deflactor / 1000
  suma$peso <- pmax(suma$flujo_real_mm, 0)
  suma
}

anual <- armar_panel_anual(nucleo)

cat("\nPares declarante-contraparte-año:", nrow(anual), "\n")
cat("Proporción con flujo anual positivo:", round(mean(anual$peso > 0), 3), "\n")
cat("Pares con menos de 4 trimestres:", sum(anual$n_trimestres < 4), "\n")


# -----------------------------------------------------------------------------
# Etapa 2. Construcción de las redes
# -----------------------------------------------------------------------------

# 2.1 Nodos de cada año: el núcleo más todo país que aparece como
#     contraparte del núcleo ese año (con flujo positivo, nulo o negativo).
# 2.2 Enlaces: flujo anual positivo de un país del núcleo a otro país. Cada
#     enlace pertenece a la red núcleo-núcleo o a la núcleo-periferia; la red
#     completa es la unión de ambas.
armar_redes <- function(panel, nuc) {
  ar <- panel[panel$peso > 0, c("anio", "declarante", "contraparte", "peso")]
  names(ar) <- c("anio", "origen", "destino", "peso")
  ar$destino_nucleo <- ar$destino %in% nuc
  list(enlaces = ar, nucleo = nuc,
       nodos_anio = lapply(split(panel$contraparte, panel$anio),
                           function(x) sort(union(nuc, x))))
}

# Enlaces y nodos de una red en un año:
#   completa:          todos los nodos; enlaces del núcleo a todos.
#   nucleo_periferia:  todos los nodos; enlaces del núcleo a la periferia.
#   nucleo_nucleo:     solo el núcleo; enlaces dentro del núcleo.
red_anio <- function(R, red, a) {
  ar <- R$enlaces[R$enlaces$anio == a, ]
  nd <- R$nodos_anio[[as.character(a)]]
  if (red == "nucleo_periferia") ar <- ar[!ar$destino_nucleo, ]
  if (red == "nucleo_nucleo") {
    ar <- ar[ar$destino_nucleo, ]
    nd <- R$nucleo
  }
  list(enlaces = ar, nodos = nd)
}

R_base <- armar_redes(anual, nucleo)

cat("\nNodos por año (selección):\n")
print(sapply(R_base$nodos_anio, length)[as.character(
  c(1978, 1980, 1990, 2000, 2007, 2010, 2020, 2025))])


# -----------------------------------------------------------------------------
# Etapa 3. Indicadores por nodo: grado, fuerza y HHI
# -----------------------------------------------------------------------------

# Para cada nodo i:
#   grado de salida  = número de países a los que i presta
#   grado de entrada = número de países que le prestan a i
#   fuerza de salida / entrada = suma de los pesos de esos vínculos
#   HHI = suma de los cuadrados de las participaciones de cada socio en la
#         fuerza del nodo; vale 0 si el nodo no tiene vínculos (como en los
#         mínimos de la Tabla 1 del paper).
indicadores_nodo <- function(ar, nodos, nuc) {
  o <- factor(ar$origen,  levels = nodos)
  d <- factor(ar$destino, levels = nodos)
  s_sal <- tapply(ar$peso, o, sum); s_sal[is.na(s_sal)] <- 0
  s_ent <- tapply(ar$peso, d, sum); s_ent[is.na(s_ent)] <- 0
  part_sal <- ar$peso / s_sal[as.character(ar$origen)]
  part_ent <- ar$peso / s_ent[as.character(ar$destino)]
  h_sal <- tapply(part_sal^2, o, sum); h_sal[is.na(h_sal)] <- 0
  h_ent <- tapply(part_ent^2, d, sum); h_ent[is.na(h_ent)] <- 0
  data.frame(pais       = nodos,
             es_nucleo  = nodos %in% nuc,
             grado_sal  = as.vector(table(o)),
             grado_ent  = as.vector(table(d)),
             fuerza_sal = as.vector(s_sal),
             fuerza_ent = as.vector(s_ent),
             hhi_sal    = as.vector(h_sal),
             hhi_ent    = as.vector(h_ent))
}


# -----------------------------------------------------------------------------
# Etapa 4. Coeficientes de clustering (Fagiolo, 2007)
# -----------------------------------------------------------------------------

# A es la matriz de adyacencia (a_ij = 1 si i presta a j) y W la de pesos.
# Para cada nodo i, C_i = t_i / T_i (triángulos observados / posibles):
#   "de entrada": t_i = (A' A^2)_ii       T_i = d_ent (d_ent - 1)
#   "ciclo":      t_i = (A^3)_ii          T_i = d_ent d_sal - d_bil
# donde d_bil = (A^2)_ii es el número de socios recíprocos de i.
# Versión ponderada: el numerador usa M = W^(1/3) (raíz cúbica elemento por
# elemento) en lugar de A; con los pesos sin normalizar, no está acotada.
# Si T_i = 0 el coeficiente no está definido y se fija en 0; el paper no
# explicita esta convención, pero es coherente con los mínimos de 0 de su
# Tabla 1 (ver la comparación de la Etapa 5).
clustering_nodo <- function(A, W) {
  M  <- W^(1 / 3)
  A2 <- A %*% A
  M2 <- M %*% M
  d_ent <- colSums(A)
  d_sal <- rowSums(A)
  d_bil <- diag(A2)
  T_ent <- d_ent * (d_ent - 1)
  T_cic <- d_ent * d_sal - d_bil
  cociente <- function(t, T) ifelse(T > 0, t / T, 0)
  data.frame(cl_ent      = cociente(diag(t(A) %*% A2), T_ent),
             cl_cic      = cociente(diag(A2 %*% A),    T_cic),
             cl_ent_pond = cociente(diag(t(M) %*% M2), T_ent),
             cl_cic_pond = cociente(diag(M2 %*% M),    T_cic))
}

# 4.1 Validación en una red de 4 países, con triángulos contados a mano.
#     Enlaces: P1->P2, P2->P3, P3->P1, P1->P3, P1->P4, P2->P4, P3->P4.
#     "De entrada": P3 recibe de P1 y P2, y P1->P2 existe: 1 de 2 = 1/2.
#                   P4 recibe de P1, P2 y P3; de los 6 pares ordenados,
#                   4 tienen la flecha (P1->P2, P1->P3, P2->P3, P3->P1): 4/6.
#     "Ciclo":      P1: P1->P2->P3->P1, 1 de 2 posibles (3*1 - 1): 1/2.
#                   P2: P2->P3->P1->P2, 1 de 2: 1/2.
#                   P3: P3->P1->P2->P3, 1 de 3 (2*2 - 1): 1/3.
#     Con todos los pesos iguales a 8, M = 2 y cada triángulo aporta 8:
#     el clustering ponderado es 8 veces el binario.
p <- paste0("P", 1:4)
A_prueba <- matrix(0, 4, 4, dimnames = list(p, p))
A_prueba[cbind(c(1, 2, 3, 1, 1, 2, 3), c(2, 3, 1, 3, 4, 4, 4))] <- 1
prueba <- clustering_nodo(A_prueba, 8 * A_prueba)
stopifnot(isTRUE(all.equal(prueba$cl_ent, c(0, 0, 1/2, 4/6))),
          isTRUE(all.equal(prueba$cl_cic, c(1/2, 1/2, 1/3, 0))),
          isTRUE(all.equal(prueba$cl_ent_pond, 8 * prueba$cl_ent)),
          isTRUE(all.equal(prueba$cl_cic_pond, 8 * prueba$cl_cic)))
cat("\nValidación del clustering en la red de prueba: correcta\n")

# 4.2 Indicadores por nodo (Etapas 3 y 4) para cada red y año.
matriz <- function(ar, nodos, valores) {
  X <- matrix(0, length(nodos), length(nodos), dimnames = list(nodos, nodos))
  X[cbind(ar$origen, ar$destino)] <- valores
  X
}

calcular_ind_nodos <- function(R) {
  out <- do.call(rbind, lapply(redes, function(r) {
    do.call(rbind, lapply(anios, function(a) {
      x <- red_anio(R, r, a)
      cbind(red = r, anio = a,
            indicadores_nodo(x$enlaces, x$nodos, R$nucleo),
            clustering_nodo(matriz(x$enlaces, x$nodos, 1),
                            matriz(x$enlaces, x$nodos, x$enlaces$peso)))
    }))
  }))
  # Control: la suma de los grados (y de las fuerzas) de salida es igual a la
  # de entrada en cada red y año, porque cada enlace se cuenta de ambos lados.
  ctrl <- aggregate(cbind(grado_sal, grado_ent, fuerza_sal, fuerza_ent) ~
                      red + anio, data = out, FUN = sum)
  stopifnot(all(ctrl$grado_sal == ctrl$grado_ent),
            all(abs(ctrl$fuerza_sal - ctrl$fuerza_ent) < 1e-6))
  out
}

ind_nodos <- calcular_ind_nodos(R_base)


# -----------------------------------------------------------------------------
# Etapa 5. Indicadores por red y Tabla 1 del paper
# -----------------------------------------------------------------------------

# Población sobre la que se promedia cada indicador:
#   de entrada (grado, fuerza, HHI, clustering): todos los nodos (completa),
#     la periferia (nucleo_periferia) o el núcleo (nucleo_nucleo);
#   de salida y "ciclo": el núcleo, que es el único que presta.
poblacion_ent <- function(d, r) {
  switch(r, completa = d, nucleo_periferia = d[!d$es_nucleo, ],
         nucleo_nucleo = d)
}

# 5.1 Indicadores de red por año.
#   Conectividad = vínculos observados / posibles:
#     completa:         núcleo x (nodos - 1)
#     nucleo_periferia: núcleo x periferia
#     nucleo_nucleo:    núcleo x (núcleo - 1)
calcular_ind_red <- function(ind, nuc) {
  nn <- length(nuc)
  do.call(rbind, lapply(redes, function(r) {
    do.call(rbind, lapply(anios, function(a) {
      d     <- ind[ind$red == r & ind$anio == a, ]
      d_ent <- poblacion_ent(d, r)
      d_sal <- d[d$es_nucleo, ]
      nt <- nrow(d)
      posibles <- switch(r, completa = nn * (nt - 1),
                         nucleo_periferia = nn * (nt - nn),
                         nucleo_nucleo = nn * (nn - 1))
      data.frame(red = r, anio = a, n_nodos = nt,
                 n_enlaces    = sum(d$grado_sal),
                 conectividad = sum(d$grado_sal) / posibles,
                 flujo_total  = sum(d$fuerza_sal),
                 fuerza_sal   = mean(d_sal$fuerza_sal),
                 hhi_ent      = mean(d_ent$hhi_ent),
                 hhi_sal      = mean(d_sal$hhi_sal),
                 cl_ent       = mean(d_ent$cl_ent),
                 cl_ent_pond  = mean(d_ent$cl_ent_pond),
                 cl_cic       = mean(d_sal$cl_cic),
                 cl_cic_pond  = mean(d_sal$cl_cic_pond))
    }))
  }))
}

ind_red <- calcular_ind_red(ind_nodos, nucleo)

# 5.2 Tabla 1: estadísticas descriptivas en 1980 y 2007 (años del paper) y en
#     2019 (antes de la pandemia) y 2025 (fin de la extensión). El clustering se
#     reporta donde lo reporta el paper: "de entrada" en las redes completa y
#     núcleo-núcleo; "ciclo" en la núcleo-núcleo.
describir <- function(x) {
  c(media = mean(x), mediana = median(x), desvio = sd(x),
    min = min(x), max = max(x))
}
vars_ent <- list(completa         = c("grado_ent", "fuerza_ent", "hhi_ent", "cl_ent"),
                 nucleo_periferia = c("grado_ent", "fuerza_ent", "hhi_ent"),
                 nucleo_nucleo    = c("grado_ent", "fuerza_ent", "hhi_ent", "cl_ent"))
vars_sal <- list(completa         = c("grado_sal", "fuerza_sal", "hhi_sal"),
                 nucleo_periferia = c("grado_sal", "fuerza_sal", "hhi_sal"),
                 nucleo_nucleo    = c("grado_sal", "fuerza_sal", "hhi_sal", "cl_cic"))

calcular_tabla1 <- function(ind, anios = c(1980, 2007, 2019, 2025)) {
  t1 <- do.call(rbind, lapply(redes, function(r) {
    do.call(rbind, lapply(anios, function(a) {
      d <- ind[ind$red == r & ind$anio == a, ]
      filas <- rbind(t(sapply(poblacion_ent(d, r)[vars_ent[[r]]], describir)),
                     t(sapply(d[d$es_nucleo, vars_sal[[r]]], describir)))
      data.frame(red = r, anio = a, indicador = rownames(filas),
                 round(filas, 2), row.names = NULL)
    }))
  }))
  # El "ciclo" del núcleo calculado en la red completa: su denominador usa
  # el grado de salida hacia todos los países. Es la versión que aproxima
  # los valores de "ciclo" del panel C del paper (0,12 y 0,10); el paper no
  # aclara en qué red lo calcula.
  ciclo <- do.call(rbind, lapply(anios, function(a) {
    d <- ind[ind$red == "completa" & ind$anio == a & ind$es_nucleo, ]
    data.frame(red = "nucleo_nucleo", anio = a,
               indicador = "cl_cic_red_completa",
               t(round(describir(d$cl_cic), 2)))
  }))
  rbind(t1, ciclo)
}

tabla1 <- calcular_tabla1(ind_nodos)

# 5.3 Medias de la Tabla 1 de Minoiu y Reyes (2013), para comparar.
#     Fuerza en miles de millones de USD de 2009; NA = no reportado.
paper_t1 <- data.frame(
  red = c(rep("completa", 7), rep("nucleo_periferia", 6),
          rep("nucleo_nucleo", 8)),
  indicador = c("grado_ent", "fuerza_ent", "hhi_ent", "cl_ent",
                "grado_sal", "fuerza_sal", "hhi_sal",
                "grado_ent", "fuerza_ent", "hhi_ent",
                "grado_sal", "fuerza_sal", "hhi_sal",
                "grado_ent", "fuerza_ent", "hhi_ent", "cl_ent",
                "grado_sal", "fuerza_sal", "hhi_sal", "cl_cic_red_completa"),
  paper_1980 = c(5.7, 4.0, 0.32, 0.62, 53.9, 37.6, 0.17,
                 5.2, 1.8, 0.33, 44.1, 15.2, 0.11,
                 9.8, 22.4, 0.28, 0.68, 9.8, 22.4, 0.31, 0.12),
  paper_2007 = c(7.0, 24.7, 0.43, 0.75, 85.9, 302.6, 0.19,
                 6.6, 6.4, 0.44, 74.8, 72.0, 0.11,
                 11.1, 230.6, 0.29, 0.80, 11.1, 230.6, 0.29, 0.10))

orden_ind <- c("grado_ent", "fuerza_ent", "hhi_ent", "cl_ent", "grado_sal",
               "fuerza_sal", "hhi_sal", "cl_cic", "cl_cic_red_completa")
comparar_tabla1 <- function(t1) {
  medias <- reshape(t1[c("red", "anio", "indicador", "media")],
                    idvar = c("red", "indicador"), timevar = "anio",
                    direction = "wide")
  names(medias) <- sub("^media\\.", "nuestro_", names(medias))
  x <- merge(medias, paper_t1, by = c("red", "indicador"), all.x = TRUE)
  x[order(match(x$red, redes), match(x$indicador, orden_ind)),
    c("red", "indicador", "paper_1980", "nuestro_1980",
      "paper_2007", "nuestro_2007", intersect(c("nuestro_2019", "nuestro_2025"), names(x)))]
}

comparacion_t1 <- comparar_tabla1(tabla1)

cat("\nComparación con la Tabla 1 del paper (medias):\n")
print(comparacion_t1, row.names = FALSE)

cat("\nIndicadores de la red núcleo-núcleo, años seleccionados:\n")
print(ind_red[ind_red$red == "nucleo_nucleo" &
                ind_red$anio %in% c(1980, 1990, 2000, 2007, 2008, 2009,
                                    2010, 2015, 2020, 2025),
              c("anio", "conectividad", "cl_ent", "cl_cic", "hhi_sal",
                "flujo_total")],
      row.names = FALSE, digits = 3)

saveRDS(list(anual = anual, R = R_base, ind_nodos = ind_nodos,
             ind_red = ind_red),
        file.path(dir_procesados, "redes_anuales.rds"))
write.csv(ind_nodos, file.path(dir_procesados, "indicadores_nodo_anual.csv"),
          row.names = FALSE)
guardar_tabla(ind_red,        "rep_indicadores_red_anuales_1978_2025.csv")
guardar_tabla(tabla1,         "rep_tabla1_estadisticas_1980_2007.csv")
guardar_tabla(comparacion_t1, "rep_tabla1_comparacion_con_paper.csv")


# -----------------------------------------------------------------------------
# Etapa 6. Replicación figura 3 del paper: indicadores de red, 1978-2025
# -----------------------------------------------------------------------------

# Línea vertical gris: fin de la muestra del paper (2010); a la derecha, el
# período de la extensión.
lty_red   <- c(completa = 1, nucleo_periferia = 2, nucleo_nucleo = 3)
etiquetas <- c(completa = "Red completa",
               nucleo_periferia = "Núcleo-periferia",
               nucleo_nucleo = "Núcleo-núcleo")

panel_fig3 <- function(vars, redes_p, titulo, eje, crisis = NULL) {
  ys <- mapply(function(r, v) ind_red[ind_red$red == r, v], redes_p, vars,
               SIMPLIFY = FALSE)
  plot(anios, ys[[1]], type = "n", ylim = range(unlist(ys)),
       xlab = "", ylab = eje, main = titulo, cex.main = 1, las = 1)
  abline(v = 2010.5, col = "grey55", lwd = 1.5)
  for (k in seq_along(ys)) {
    lines(anios, ys[[k]], lwd = 2, lty = lty_red[redes_p[k]])
  }
  box()
}

dibujar_figura3 <- function() {
  par(mfrow = c(2, 3), mar = c(3, 4.5, 2.5, 1), oma = c(3, 0, 0, 0))
  panel_fig3(rep("conectividad", 3), redes, "A. Conectividad", "proporción")
  panel_fig3(rep("cl_ent", 2), c("completa", "nucleo_nucleo"),
             "B. Clustering binario (de entrada)", "proporción")
  panel_fig3(rep("cl_cic", 2), c("completa", "nucleo_nucleo"),
             "C. Clustering binario (ciclo)", "proporción")
  panel_fig3(rep("flujo_total", 3), redes,
             "D. Flujo total", "miles de millones USD 2009")
  panel_fig3(rep("cl_ent_pond", 2), c("completa", "nucleo_nucleo"),
             "E. Clustering ponderado (de entrada)", "índice")
  panel_fig3(rep("hhi_sal", 2), c("completa", "nucleo_nucleo"),
             "F. HHI de salida (núcleo)", "índice")
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
      new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", legend = etiquetas, lty = lty_red, lwd = 2, horiz = TRUE,
         bty = "n", xpd = TRUE)
}

# Guarda una figura en PNG y en PDF.
guardar_figura <- function(dibujar, nombre, ancho, alto) {
  png(file.path(dir_figuras, paste0(nombre, ".png")),
      width = ancho * 125, height = alto * 125, res = 130)
  dibujar()
  dev.off()
  pdf(file.path(dir_figuras, paste0(nombre, ".pdf")), width = ancho,
      height = alto)
  dibujar()
  dev.off()
}

guardar_figura(dibujar_figura3, "rep_fig3_indicadores_red_1978_2025", 12, 7)


# -----------------------------------------------------------------------------
# Etapa 7. Estudio de eventos alrededor de las crisis (Tabla 5 y Figura 8)
# -----------------------------------------------------------------------------

# 7.1 Fechas de crisis bancarias sistémicas: Laeven y Valencia (2026),
#     "Systemic Banking Crises Database: 1970-2025", IMF WP 26/94.
#     Archivo descargado manualmente del sitio del FMI.
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
archivo_crisis <- file.path(dir_brutos,
                            "SYSTEMIC_BANKING_CRISES_DATABASE_2026.xlsx")
# Se lee una copia temporal: en Windows, readxl no abre archivos cuya ruta
# tiene caracteres no ASCII.
copia_crisis <- tempfile(fileext = ".xlsx")
stopifnot(file.copy(archivo_crisis, copia_crisis, overwrite = TRUE))
hoja <- as.data.frame(readxl::read_excel(copia_crisis,
                                         sheet = "Crisis Resolution and Outcomes"))
crisis <- data.frame(
  pais_lv = trimws(as.character(hoja[[1]])),
  inicio  = suppressWarnings(as.integer(substr(as.character(hoja[[3]]), 1, 4))))
crisis <- crisis[!is.na(crisis$inicio), ]
stopifnot(nrow(crisis) == 164)   # total de episodios informado por los autores

# Los casos "borderline" llevan una nota junto al nombre ("5/" o "6/"; la
# nota "4/" de Armenia no es de ese tipo). Se incluyen, como en el paper.
crisis$borderline <- grepl("[56]/", crisis$pais_lv)
crisis$pais_lv    <- trimws(sub("\\s*[0-9]/$", "", crisis$pais_lv))

# Correspondencia entre los nombres de Laeven y Valencia y los códigos ISO
# de dos letras que usa el BIS.
iso2_lv <- c(
  "Albania" = "AL", "Algeria" = "DZ", "Angola" = "AO", "Argentina" = "AR",
  "Armenia" = "AM", "Austria" = "AT", "Azerbaijan" = "AZ",
  "Bangladesh" = "BD", "Belarus" = "BY", "Belgium" = "BE", "Benin" = "BJ",
  "Bolivia" = "BO", "Bosnia and Herzegovina" = "BA", "Brazil" = "BR",
  "Bulgaria" = "BG", "Burkina Faso" = "BF", "Burundi" = "BI",
  "Cameroon" = "CM", "Cape Verde" = "CV", "Central African Republic" = "CF",
  "Chad" = "TD", "Chile" = "CL", "China" = "CN", "Colombia" = "CO",
  "Costa Rica" = "CR", "Cote d'Ivoire" = "CI", "Croatia" = "HR",
  "Cyprus" = "CY", "Czech Republic" = "CZ",
  "Democratic Republic of Congo" = "CD", "Denmark" = "DK", "Djibouti" = "DJ",
  "Dominican Republic" = "DO", "Ecuador" = "EC", "Egypt" = "EG",
  "El Salvador" = "SV", "Equatorial Guinea" = "GQ", "Eritrea" = "ER",
  "Estonia" = "EE", "Eswatini" = "SZ", "Finland" = "FI", "France" = "FR",
  "Georgia" = "GE", "Germany" = "DE", "Ghana" = "GH", "Greece" = "GR",
  "Guinea" = "GN", "Guinea-Bissau" = "GW", "Guyana" = "GY", "Haiti" = "HT",
  "Hungary" = "HU", "Iceland" = "IS", "India" = "IN", "Indonesia" = "ID",
  "Ireland" = "IE", "Israel" = "IL", "Italy" = "IT", "Jamaica" = "JM",
  "Japan" = "JP", "Jordan" = "JO", "Kazakhstan" = "KZ", "Kenya" = "KE",
  "Korea" = "KR", "Kuwait" = "KW", "Kyrgyz Republic" = "KG", "Latvia" = "LV",
  "Lebanon" = "LB", "Liberia" = "LR", "Lithuania" = "LT",
  "Luxembourg" = "LU", "Madagascar" = "MG", "Malaysia" = "MY", "Mali" = "ML",
  "Mauritania" = "MR", "Mexico" = "MX", "Moldova" = "MD", "Mongolia" = "MN",
  "Morocco" = "MA", "Mozambique" = "MZ", "Nepal" = "NP",
  "Netherlands" = "NL", "Nicaragua" = "NI", "Niger" = "NE", "Nigeria" = "NG",
  "North Macedonia" = "MK", "Norway" = "NO", "Panama" = "PA",
  "Paraguay" = "PY", "Peru" = "PE", "Philippines" = "PH", "Poland" = "PL",
  "Portugal" = "PT", "Republic of Congo" = "CG", "Romania" = "RO",
  "Russia" = "RU", "Senegal" = "SN", "Sierra Leone" = "SL",
  "Slovak Republic" = "SK", "Slovenia" = "SI", "Spain" = "ES",
  "Sri Lanka" = "LK", "Sweden" = "SE", "Switzerland" = "CH",
  "São Tomé and Príncipe" = "ST", "Tajikistan" = "TJ", "Tanzania" = "TZ",
  "Thailand" = "TH", "Togo" = "TG", "Tunisia" = "TN", "Türkiye" = "TR",
  "Uganda" = "UG", "Ukraine" = "UA", "United Kingdom" = "GB",
  "United States" = "US", "Uruguay" = "UY", "Venezuela" = "VE",
  "Vietnam" = "VN", "Yemen" = "YE", "Zambia" = "ZM", "Zimbabwe" = "ZW")
stopifnot(all(crisis$pais_lv %in% names(iso2_lv)))
crisis$pais <- unname(iso2_lv[crisis$pais_lv])

# 7.2 Ventana de eventos. Para el país i en el año t y una crisis que empieza
#     en el año s, k = t - s. Cinco tramos, como en la Tabla 5 del paper. Si
#     un país tiene varias crisis, la variable vale 1 si el año cae en el
#     tramo de cualquiera de ellas.
tramos <- list(antes_3_4   = -4:-3, antes_1_2   = -2:-1, inicio = 0,
               despues_1_2 = 1:2,   despues_3_4 = 3:4)

agregar_ventanas <- function(d, cr) {
  for (tr in names(tramos)) d[[tr]] <- 0
  for (j in seq_len(nrow(cr))) {
    sel <- d$pais == cr$pais[j]
    k   <- d$anio[sel] - cr$inicio[j]
    for (tr in names(tramos)) {
      d[[tr]][sel] <- pmax(d[[tr]][sel], as.numeric(k %in% tramos[[tr]]))
    }
  }
  d
}

# 7.3 Especificaciones:
#   replica:   1978-2010 y crisis que empiezan en 1982-2003 o en 2007-2008,
#              como en el paper (cinco años de margen en los extremos).
#   extension: 1978-2025 y crisis que empiezan en 1982-2021.
#   Cada una se estima con efectos fijos por país (como el paper) y, como
#   robustez, con efectos fijos por país y por año ("_ef_anio"). Los efectos
#   de año absorben los shocks comunes a todos los países (por ejemplo, la
#   contracción global de 2008), de modo que los coeficientes miden solo el
#   desvío propio del país en crisis.
especificaciones <- list(
  replica   = list(anios = 1978:2010,
                   crisis = crisis$inicio %in% c(1982:2003, 2007:2008)),
  extension = list(anios = 1978:2025,
                   crisis = crisis$inicio %in% 1982:2021))

# 7.4 Variables dependientes. Las fuerzas y el clustering ponderado entran
#     como log(1 + x), con la fuerza en millones de USD de 2009, para incluir
#     los ceros. El "ciclo" del núcleo se toma de la red completa (Etapa 5).
ind_nodos$log_fuerza_ent  <- log1p(1000 * ind_nodos$fuerza_ent)
ind_nodos$log_fuerza_sal  <- log1p(1000 * ind_nodos$fuerza_sal)
ind_nodos$log_cl_ent_pond <- log1p(ind_nodos$cl_ent_pond)
ind_nodos$log_cl_cic_pond <- log1p(ind_nodos$cl_cic_pond)

panel_A <- ind_nodos[ind_nodos$red == "completa", ]
panel_B <- ind_nodos[ind_nodos$red == "nucleo_nucleo",
                     setdiff(names(ind_nodos),
                             c("cl_cic", "cl_cic_pond", "log_cl_cic_pond"))]
panel_B <- merge(panel_B,
                 panel_A[panel_A$es_nucleo,
                         c("anio", "pais", "cl_cic", "log_cl_cic_pond")],
                 by = c("anio", "pais"))

# Columnas de la Tabla 5. "todos": todos los nodos; "nucleo": solo el núcleo.
columnas_A <- list(grado_ent = "todos", log_fuerza_ent = "todos",
                   grado_sal = "nucleo", log_fuerza_sal = "nucleo",
                   hhi_ent = "todos", cl_ent = "todos",
                   log_cl_ent_pond = "todos")
columnas_B <- c("grado_ent", "log_fuerza_ent", "grado_sal", "log_fuerza_sal",
                "hhi_sal", "cl_ent", "log_cl_ent_pond", "cl_cic",
                "log_cl_cic_pond")

# 7.5 MCO con efectos fijos y errores agrupados por país.
#     y_it = a_i (+ g_t) + sum_k b_k D_k,it + e_it
#     Varianza agrupada (CR1): (X'X)^-1 [sum_g X_g' u_g u_g' X_g] (X'X)^-1,
#     ajustada por G/(G-1) (N-1)/(N-K). Test F conjunto de los cinco b_k.
estimar <- function(d, y, ef_anio = FALSE) {
  d <- d[is.finite(d[[y]]), ]
  f <- as.formula(paste(y, "~", paste(names(tramos), collapse = " + "),
                        "+ factor(pais)", if (ef_anio) "+ factor(anio)"))
  m  <- lm(f, data = d)
  ok <- !is.na(coef(m))
  X  <- model.matrix(m)[, ok, drop = FALSE]
  u  <- residuals(m)
  b  <- coef(m)[ok]
  G  <- length(unique(d$pais)); N <- nrow(X); K <- ncol(X)
  XtX_inv <- solve(crossprod(X))
  S <- Reduce(`+`, lapply(split(seq_len(N), d$pais), function(i) {
    s <- crossprod(X[i, , drop = FALSE], u[i])
    s %*% t(s)
  }))
  V  <- XtX_inv %*% S %*% XtX_inv * (G / (G - 1)) * ((N - 1) / (N - K))
  tr <- intersect(names(tramos), names(b))
  bt <- b[tr]
  Vt <- V[tr, tr, drop = FALSE]
  ee <- sqrt(diag(Vt))
  F_est <- drop(t(bt) %*% solve(Vt) %*% bt) / length(bt)
  data.frame(variable = y, tramo = tr, coef = bt, ee = ee,
             p = 2 * pt(abs(bt / ee), G - 1, lower.tail = FALSE),
             p_F = pf(F_est, length(bt), G - 1, lower.tail = FALSE),
             n = N, r2 = summary(m)$r.squared, row.names = NULL)
}

tabla5 <- do.call(rbind, lapply(names(especificaciones), function(e) {
  es <- especificaciones[[e]]
  cr <- crisis[es$crisis, ]
  A  <- agregar_ventanas(panel_A[panel_A$anio %in% es$anios, ], cr)
  B  <- agregar_ventanas(panel_B[panel_B$anio %in% es$anios, ], cr)
  do.call(rbind, lapply(c(FALSE, TRUE), function(ef) {
    res_A <- lapply(names(columnas_A), function(y) {
      d <- if (columnas_A[[y]] == "nucleo") A[A$es_nucleo, ] else A
      estimar(d, y, ef)
    })
    res_B <- lapply(columnas_B, function(y) estimar(B, y, ef))
    nombre <- paste0(e, if (ef) "_ef_anio")
    rbind(cbind(especificacion = nombre, panel = "A_completa",
                do.call(rbind, res_A)),
          cbind(especificacion = nombre, panel = "B_nucleo_nucleo",
                do.call(rbind, res_B)))
  }))
}))
tabla5$estrellas <- ifelse(tabla5$p < 0.01, "***",
                    ifelse(tabla5$p < 0.05, "**",
                    ifelse(tabla5$p < 0.10, "*", "")))

cat("\nCrisis usadas: réplica =", sum(especificaciones$replica$crisis),
    "| extensión =", sum(especificaciones$extension$crisis), "\n")

# 7.6 Comparación con el panel A del paper (columnas 1 a 4).
paper_t5 <- data.frame(
  variable = rep(c("grado_ent", "log_fuerza_ent", "grado_sal",
                   "log_fuerza_sal"), each = 5),
  tramo = rep(names(tramos), 4),
  paper = c(0.34, 0.50, -0.61, -0.98, -0.40,
            0.39, 0.32, -0.18, -0.50, -0.15,
            10.27, 12.82, 1.87, -2.29, -0.46,
            0.91, 1.03, 0.60, -0.18, 0.01))
comparacion_t5 <- paper_t5
for (e in unique(tabla5$especificacion)) {
  x <- tabla5[tabla5$especificacion == e & tabla5$panel == "A_completa", ]
  x[[e]] <- sprintf("%.2f%s (%.2f)", x$coef, x$estrellas, x$ee)
  comparacion_t5 <- merge(comparacion_t5, x[c("variable", "tramo", e)],
                          by = c("variable", "tramo"), sort = FALSE)
}
comparacion_t5 <- comparacion_t5[order(match(comparacion_t5$variable,
                                             unique(paper_t5$variable)),
                                       match(comparacion_t5$tramo,
                                             names(tramos))), ]
cat("\nTabla 5, panel A (red completa): coeficiente (error estándar)\n")
print(comparacion_t5, row.names = FALSE)

# Efectos acumulados en los cuatro años previos (como en el paper):
#   grado: b(3-4 antes) + b(1-2 antes);
#   fuerza en logaritmos: (e^b1 - 1) + (e^b2 - 1).
acumulado <- function(e, v, log = FALSE) {
  b <- tabla5$coef[tabla5$especificacion == e & tabla5$panel == "A_completa" &
                     tabla5$variable == v][1:2]
  if (log) sum(exp(b) - 1) else sum(b)
}
efectos_previos <- do.call(rbind, lapply(unique(tabla5$especificacion),
                                         function(e) {
  data.frame(especificacion = e,
             grado_ent  = acumulado(e, "grado_ent"),
             fuerza_ent = acumulado(e, "log_fuerza_ent", TRUE),
             grado_sal  = acumulado(e, "grado_sal"),
             fuerza_sal = acumulado(e, "log_fuerza_sal", TRUE))
}))
efectos_previos <- rbind(data.frame(especificacion = "paper", grado_ent = 0.84,
                                    fuerza_ent = 0.85, grado_sal = 23.09,
                                    fuerza_sal = 3.29),
                         efectos_previos)
cat("\nEfectos acumulados en los cuatro años previos a la crisis:\n")
print(efectos_previos, row.names = FALSE, digits = 3)

guardar_tabla(crisis,          "datos_crisis_bancarias_laeven_valencia_2026.csv")
guardar_tabla(tabla5,          "rep_tabla5_estudio_eventos_todas.csv")
guardar_tabla(comparacion_t5,  "rep_tabla5_comparacion_con_paper.csv")
guardar_tabla(efectos_previos, "rep_tabla5_efectos_acumulados_previos.csv")

# 7.7 Figura 8: coeficientes e intervalos de 95% para el grado y la fuerza de
#     entrada (red completa), réplica y extensión (efectos fijos por país).
dibujar_figura8 <- function() {
  par(mfrow = c(1, 2), mar = c(4, 4.5, 2.5, 1))
  etiq <- c("3-4\nantes", "1-2\nantes", "inicio", "1-2\ndespués", "3-4\ndespués")
  for (v in c("grado_ent", "log_fuerza_ent")) {
    x1 <- tabla5[tabla5$especificacion == "replica" &
                   tabla5$panel == "A_completa" & tabla5$variable == v, ]
    x2 <- tabla5[tabla5$especificacion == "extension" &
                   tabla5$panel == "A_completa" & tabla5$variable == v, ]
    lim <- range(c(x1$coef - 1.96 * x1$ee, x1$coef + 1.96 * x1$ee,
                   x2$coef - 1.96 * x2$ee, x2$coef + 1.96 * x2$ee))
    plot(1:5 - 0.1, x1$coef, ylim = lim, xlim = c(0.6, 5.4), pch = 19,
         xaxt = "n", xlab = "", ylab = "coeficiente", las = 1,
         main = ifelse(v == "grado_ent", "A. Grado de entrada",
                       "B. log(1 + fuerza de entrada)"), cex.main = 1)
    axis(1, at = 1:5, labels = etiq, padj = 0.5, cex.axis = 0.85)
    abline(h = 0, lty = 3)
    arrows(1:5 - 0.1, x1$coef - 1.96 * x1$ee, 1:5 - 0.1,
           x1$coef + 1.96 * x1$ee, angle = 90, code = 3, length = 0.03)
    points(1:5 + 0.1, x2$coef, pch = 1)
    arrows(1:5 + 0.1, x2$coef - 1.96 * x2$ee, 1:5 + 0.1,
           x2$coef + 1.96 * x2$ee, angle = 90, code = 3, length = 0.03,
           lty = 2)
    legend("bottomleft", c("1978-2010", "1978-2025"), pch = c(19, 1),
           lty = 1:2, bty = "n", cex = 0.85)
  }
}
guardar_figura(dibujar_figura8, "rep_fig8_crisis_grado_fuerza_entrada", 11, 5)


# -----------------------------------------------------------------------------
# Etapa 8. Robustez: núcleo sin Países Bajos
# -----------------------------------------------------------------------------

# Países Bajos tiene cerca de 60% de sus flujos bilaterales suprimidos por
# confidencialidad (ver cobertura_declarantes). Se repiten las Etapas 1 a 5
# con un núcleo de 11 países; Países Bajos pasa a la periferia.
nucleo_rob <- setdiff(nucleo, "NL")
R_rob      <- armar_redes(armar_panel_anual(nucleo_rob), nucleo_rob)
ind_rob    <- calcular_ind_nodos(R_rob)
red_rob    <- calcular_ind_red(ind_rob, nucleo_rob)

anios_sel <- 1978:2025   # todos los años de la muestra
comparar_red <- function(v) {
  data.frame(
    red = rep(redes, each = length(anios_sel)),
    anio = rep(anios_sel, length(redes)),
    indicador = v,
    nucleo_12 = unlist(lapply(redes, function(r)
      ind_red[ind_red$red == r & ind_red$anio %in% anios_sel, v])),
    nucleo_sin_NL = unlist(lapply(redes, function(r)
      red_rob[red_rob$red == r & red_rob$anio %in% anios_sel, v])))
}
robustez <- rbind(comparar_red("conectividad"), comparar_red("cl_ent"))
robustez$nucleo_12     <- round(robustez$nucleo_12, 3)
robustez$nucleo_sin_NL <- round(robustez$nucleo_sin_NL, 3)

cat("\nRobustez sin Países Bajos: conectividad de la red núcleo-núcleo\n")
rob_nn <- robustez[robustez$red == "nucleo_nucleo" &
                    robustez$indicador == "conectividad", ]
rob_nn$dif <- rob_nn$nucleo_sin_NL - rob_nn$nucleo_12
print(rob_nn, row.names = FALSE)
cat("Diferencia absoluta máxima:", max(abs(rob_nn$dif)), "en",
    rob_nn$anio[which.max(abs(rob_nn$dif))], "\n")

guardar_tabla(robustez, "rob_nucleo_sin_paises_bajos_indicadores_red.csv")
guardar_tabla(comparar_tabla1(calcular_tabla1(ind_rob)),
              "rob_nucleo_sin_paises_bajos_tabla1.csv")

cat("\nListo. Tablas en", dir_tablas, "\nFiguras en", dir_figuras, "\n")
