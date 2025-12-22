# =========================================================== #
# TRABAJO PRÁCTICO: ANÁLISIS DE CORPUS                        #
# Pablo Scarpaci                                     #
# =========================================================== #

# Limpieza de entorno
rm(list = ls())

# Obtener dirección
getwd()

# Cargo las librerías que instalé
library(tidyverse)
library(tidytext)
library(wordcloud2)

# Busco los archivos en la carpeta corpus2
ruta_corpus <- "corpus2"
mis_archivos <- list.files(path = ruta_corpus, pattern = "\\.txt$", full.names = TRUE)

# Creo un contenedor para los datos
datos_textos <- tibble()

for (i in 1:length(mis_archivos)) {
  # Lectura línea por línea
  lineas <- read_lines(mis_archivos[i])

  # Armo una tabla temporal para este archivo
  temp <- tibble(
    documento = basename(mis_archivos[i]),
    linea = seq_along(lineas),
    contenido = lineas
  )
  
  # Voy pegando todo en la tabla principal
  datos_textos <- bind_rows(datos_textos, temp)
}

#tokenizacion, limpieza de palabras vacias: preposiciones, determinativos y conjunciones
palabras_sueltas <- datos_textos %>%
  unnest_tokens(palabra, contenido)

# Cargo la lista de palabras vacías con funcion tidytext para el filtrado
vacias_es <- get_stopwords("es")

# Quito conectores, números y palabras muy cortas
tokenizacion_limpia <- palabras_sueltas %>%
  filter(!palabra %in% vacias_es$word) %>%
  filter(!str_detect(palabra, "^[0-9]+$")) %>%
  filter(nchar(palabra) > 2)

# Borrado manual de otras palabras
tokenizacion_limpia <- tokenizacion_limpia %>%
  filter(!palabra %in% c("hacer", "tan", "solo", "puede"))

# Análisis de frecuencia
frecuencia_palabras <- tokenizacion_limpia %>%
  count(palabra, sort = TRUE)

# Gráfico de barras horizontales
# Elegí violeta para la estética
ggplot(head(frecuencia_palabras, 20), aes(x = reorder(palabra, n), y = n)) +
  geom_col(fill = "#9b59b6") + 
  coord_flip() +
  labs(
    title = "Palabras más repetidas en el corpus",
    subtitle = "Análisis basado en archivos .txt de la carpeta corpus2",
    x = "Términos encontrados",
    y = "Cantidad de apariciones"
  ) +
  theme_light()

#Nube de palabras: wordcloud
# Preparo los datos con las 70 palabras más frecuentes
para_la_nube <- frecuencia_palabras %>% 
  slice_max(n, n = 70)

# Generación del gráfico interactivo, uso color blanco de fondo
wordcloud2(para_la_nube, 
           size = 0.6, 
           color = "random-dark", 
           backgroundColor = "white",
           shape = "circle")