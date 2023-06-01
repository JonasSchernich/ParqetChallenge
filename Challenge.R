library(dplyr)
data <- read_csv2("Challenge.csv")
str(data)
head(data)

# Wertpaiere nach wichtigkeit sortieren
data_ordered <- arrange(data, desc(data$`number of holdings`), desc(data$`number of activities`))
view(data_ordered)

# "Fonds" in "Aktie" ändern

newData <- data %>%
  mutate(security.type = ifelse(security.type == "Fonds", "Aktie", security.type))


# Logos fixen
library(dplyr)
library(httr)

# 1. Funktion zum Überprüfen der Dateigröße
check_file_size <- function(url) {
  response <- try(GET(url), silent = TRUE)
  if (inherits(response, "try-error")) {
    return(NA)  # Fehler beim Abrufen der URL
  } else {
    size <- as.numeric(response$headers$`content-length`)
    return(size)
  }
}

# 2. Funktion zum Überprüfen des Dateiformats
check_file_format <- function(url) {
  file_ext <- tools::file_ext(url)
  return(file_ext == "jpg" || file_ext == "jpeg" || file_ext == "png")
}

# Datenframe aktualisieren
data <- data %>%
  mutate(file_size = sapply(logo, check_file_size),
         valid_format = sapply(logo, check_file_format),
         valid_logo = !is.na(file_size) & valid_format & file_size <= 15000)

# Daten nach fehlerhaften Logos filtern
filtered_data <- data %>%
  filter(!valid_logo | !valid_format | file_size > 15000)

# Ergebnis ansehen
View(data)



