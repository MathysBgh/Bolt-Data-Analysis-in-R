library(dplyr)     # Pour la manipulation des données
library(readr)     # Pour lire le fichier CSV
library(ggplot2)   # Pour visualiser les données
library(naniar)    # Pour une analyse approfondie des données manquantes
library(lubridate) # Pour manipuler les dates et les heures
library(tidyr)     # Pour manipuler les données en format long et large
library(forcats)   # Pour manipuler les facteurs

# Supposons que bolt.raw.data.combined est déjà défini
tableau_croise <- bolt.raw.data.combined

# Vérifier le résultat
head(tableau_croise)

# Afficher le tableau croisé
print(tableau_croise)

# Charger les données
data <- tableau_croise

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
    DateTime = mdy_hms(`Date.Time`),  # Convertir en format date-heure
    Date = as.Date(DateTime),         # Extraire la date
    Hour = format(DateTime, "%H:%M:%S"),  # Extraire l'heure
    JourSemaine = weekdays(DateTime), # Extraire le jour de la semaine
    Year = year(DateTime)            # Extraire l'année
  )

head(data)


heatmap_data <- data %>%
  mutate(Heure_simple = substr(Hour, 1, 2),
         JourSemaine = factor(JourSemaine, levels = c("dimanche", "samedi", "vendredi", "jeudi", "mercredi", "mardi", "lundi"))) %>%
  group_by(JourSemaine, Heure_simple) %>%
  summarise(Nombre_de_trajets = n(), .groups = 'drop')

ggplot(heatmap_data, aes(x = Heure_simple, y = JourSemaine, fill = Nombre_de_trajets)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(title = "Nombre de trajets par heure et jour de la semaine",
       x = "Heure",
       y = "Jour de la semaine",
       fill = "Nombre de trajets") +
  theme_minimal(base_size = 20) +  # Augmenter la taille de texte pour améliorer la lisibilité
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incliner les heures pour plus de clarté

ggplot(heatmap_data, aes(x = as.numeric(Heure_simple), y = Nombre_de_trajets, color = JourSemaine, group = JourSemaine)) +
  geom_line(size = 1) +
  labs(title = "Heures de pointe par jour de la semaine",
       x = "Heure",
       y = "Nombre de trajets",
       color = "Jour de la semaine") +
  theme_minimal()

# Calculer le nombre total de trajets par jour
daily_counts <- data %>%
  group_by(JourSemaine) %>%
  summarise(Total_Trajets = n())

# Graphique
ggplot(daily_counts, aes(x = reorder(JourSemaine, -Total_Trajets), y = Total_Trajets, fill = JourSemaine)) +
  geom_bar(stat = "identity") +
  labs(title = "Nombre total de trajets par jour de la semaine",
       x = "Jour de la semaine",
       y = "Nombre de trajets") +
  theme_minimal() +
  theme(legend.position = "none")

# Exemple de création des colonnes 'Month' et 'JourSemaine'
data <- data %>%
  mutate(
    Month = format(as.Date(`Date.Time`), "%B"),  # Extraire le mois
    JourSemaine = weekdays(as.Date(`Date.Time`))  # Extraire le jour de la semaine
  )

# Agréger les données par mois et par jour de la semaine
monthly_weekday_counts <- data %>%
  group_by(Month, JourSemaine) %>%
  summarise(Nombre_de_trajets = n(), .groups = 'drop')

# Convertir les colonnes 'Month' et 'JourSemaine' en facteurs avec les ordres chronologiques
monthly_weekday_counts <- monthly_weekday_counts %>%
  mutate(
    Month = factor(Month, levels = c("January", "February", "March", "April",
                                     "May", "June", "July", "August",
                                     "September", "October", "November", "December")),
    JourSemaine = factor(JourSemaine, levels = c("dimanche", "samedi", "vendredi", "jeudi",
                                                 "mercredi", "mardi", "lundi"))
  )

# Créer un graphique en barres empilées
ggplot(monthly_weekday_counts, aes(x = Month, y = Nombre_de_trajets, fill = JourSemaine)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Nombre de trajets par mois et par jour de la semaine",
       x = "Mois",
       y = "Nombre de trajets",
       fill = "Jour de la semaine") +
  theme_minimal()
# Afficher les premières lignes du data frame avec une largeur de 100 caractères
print(head(data), width = 100)

