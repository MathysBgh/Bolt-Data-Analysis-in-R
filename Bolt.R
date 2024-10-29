# Charger les packages nécessaires

library(dplyr)     # Pour la manipulation des données
library(readr)     # Pour lire le fichier CSV
library(ggplot2)   # Pour visualiser les données manquantes (optionnel)
library(naniar)    # Pour une analyse approfondie des données manquantes
library(dplyr)     # Pour la manipulation des données
library(lubridate) # Pour manipuler les dates et les heures
library(tidyr)     # Pour manipuler les données en format long et large

# Afficher le tableau croisé
print(tableau_croise)



# Charger les données
data <- read_csv("bolt-merged-data-2014.csv")

# Afficher un aperçu des données
head(data)
summary(data)

# Analyser les données manquantes
# Compter le nombre de valeurs manquantes par colonne
missing_summary <- data %>% summarise(across(everything(), ~sum(is.na(.))))
print(missing_summary)


# Convertir la colonne Date/Time en format date-heure (DateTime) en format Posix
data <- data %>%
  mutate(
    DateTime = mdy_hms(`Date/Time`),  # Convertir en format date-heure
    Date = as.Date(DateTime),         # Extraire la date
    Hour = format(DateTime, "%H:%M:%S"),  # Extraire l'heure
    JourSemaine = weekdays(DateTime), # Extraire le jour de la semaine
    Year = year(DateTime)            # Extraire l'année
  )

# Créer un tableau croisé pour le nombre de trajets par jour de la semaine et par heure
tableau_croise <- data %>%
  mutate(Heure_simple = substr(Hour, 1, 2)) %>%  # Extraire uniquement l'heure
  group_by(JourSemaine, Heure_simple) %>%
  summarise(Nombre_de_trajets = n(), .groups = 'drop') %>%  # Ajout de .groups = 'drop' pour éviter le warning
  pivot_wider(names_from = Heure_simple, values_from = Nombre_de_trajets, values_fill = 0)

# Afficher le tableau croisé
print(tableau_croise, width = Inf)



print(head(data), width = Inf)
