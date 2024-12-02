# Establecer directorios
input_file <- "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/1C 2024 - Actualización técnica/TP Final/Correspondencia-Victoria-Ocampo-y-Virginia-Woolf.pdf"
output_dir <- "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/Resultados"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
setwd(output_dir)

# Instalar y cargar librerías necesarias
required_packages <- c("pdftools", "tm", "wordcloud", "RColorBrewer", "ggplot2", "dplyr", "textclean", "stringi", "syuzhet", "reshape2")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
library(pdftools)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(textclean)
library(stringi)
library(syuzhet)
library(reshape2)

# Crear lista completa de stopwords personalizada
custom_stopwords <- c(
  "el", "la", "los", "las", "un", "una", "unos", "unas", "yo", "tu", "tú", "él", "ella",
  "nosotros", "nosotras", "vosotros", "vosotras", "ellos", "ellas", "usted", "ustedes", "me", 
  "te", "se", "nos", "os", "mi", "mis", "tu", "tus", "su", "sus", "nuestro", "nuestra", "nuestros", 
  "nuestras", "vuestro", "vuestra", "vuestros", "vuestras", "que", "qué", "quien", "quién", 
  "quienes", "quiénes", "cual", "cuál", "cuales", "cuáles", "cuyo", "cuya", "cuyos", "cuyas", 
  "donde", "dónde", "como", "cómo", "cuando", "cuándo", "cuanto", "cuánto", "cuanta", "cuánta", 
  "cuantos", "cuántos", "cuantas", "cuántas", "a", "ante", "bajo", "cabe", "con", "contra", "de", 
  "del", "desde", "en", "entre", "hacia", "hasta", "para", "por", "según", "sin", "sobre", "tras",
  "aqui", "aquí", "alli", "allí", "alla", "allá", "aca", "acá", "ahi", "ahí", "cerca", "lejos", 
  "dentro", "fuera", "encima", "debajo", "enfrente", "detras", "detrás", "alrededor", "arriba", 
  "abajo", "delante", "ahora", "antes", "despues", "después", "luego", "pronto", "tarde", "temprano",
  "todavia", "todavía", "aun", "aún", "ya", "ayer", "hoy", "mañana", "siempre", "nunca", "jamás",
  "anoche", "enseguida", "asi", "así", "bien", "mal", "despacio", "rapido", "rápido", "mejor",
  "peor", "igual", "regular", "facilmente", "fácilmente", "dificilmente", "difícilmente", "muy",
  "mucho", "poco", "bastante", "demasiado", "mas", "más", "menos", "algo", "casi", "todo", 
  "nada", "medio", "suficiente", "aproximadamente", "si", "sí", "tambien", "también", "ciertamente",
  "efectivamente", "claro", "desde luego", "por supuesto", "no", "nunca", "jamas", "jamás", 
  "quizá", "quizas", "quizás", "acaso", "tal vez", "probablemente", "posiblemente", 
  "del", "ese", "pero", "esta", "the", "hay", "solo", "este", "esa", "eso", "era", "tal", 
  "anos", "esto", "estas", "porque", "vez"
)

# Función para normalizar texto
normalize_text <- function(text) {
  text <- tolower(text)
  text <- stri_trans_general(text, "Latin-ASCII") # Elimina acentos
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, custom_stopwords)
  text <- stripWhitespace(text)
  return(text)
}

# Leer y procesar el PDF
pdf_text_raw <- pdf_text(input_file)
pdf_text_clean <- sapply(pdf_text_raw, normalize_text)

# Crear un corpus a partir del texto limpio
corpus <- Corpus(VectorSource(pdf_text_clean))

# Crear un Document Term Matrix
dtm <- DocumentTermMatrix(corpus)
dtm_matrix <- as.matrix(dtm)

# Agrupación temática sugerida
themes <- list(
  "amistad" = c("amiga", "amistad", "carta", "querida"),
  "literatura" = c("libro", "escribir", "literatura", "obra"),
  "género" = c("mujer", "feminismo", "género", "igualdad")
)

# Cruce temáticas y sentimientos
theme_sentiment <- data.frame()
for (theme in names(themes)) {
  terms <- themes[[theme]]
  terms_in_matrix <- terms[terms %in% colnames(dtm_matrix)]
  
  for (term in terms_in_matrix) {
    term_sentiment <- get_nrc_sentiment(term, language = "spanish")
    term_sentiment_summary <- colSums(term_sentiment)
    temp <- data.frame(theme = theme, sentiment = names(term_sentiment_summary), count = term_sentiment_summary)
    theme_sentiment <- rbind(theme_sentiment, temp)
  }
}

# Guardar cruce temáticas y sentimientos
write.csv(theme_sentiment, "theme_sentiment_analysis.csv", row.names = FALSE)

# Visualización de cruce temáticas y sentimientos (heatmap)
heatmap_data <- dcast(theme_sentiment, theme ~ sentiment, value.var = "count", fun.aggregate = sum)
rownames(heatmap_data) <- heatmap_data$theme
heatmap_data <- heatmap_data[, -1]

jpeg("temas_sentimientos_heatmap.jpg", width = 1200, height = 800, quality = 100)
heatmap(as.matrix(heatmap_data), Rowv = NA, Colv = NA, col = brewer.pal(9, "YlGnBu"), scale = "row",
        margins = c(6, 10), main = "Cruce Temáticas y Sentimientos (Heatmap)")
dev.off()

# Visualización alternativa (barras agrupadas)
jpeg("temas_sentimientos_barras.jpg", width = 1200, height = 800, quality = 100)
ggplot(theme_sentiment, aes(x = theme, y = count, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cruce Temáticas y Sentimientos", x = "Temática", y = "Frecuencia", fill = "Sentimiento") +
  theme_minimal()
dev.off()
