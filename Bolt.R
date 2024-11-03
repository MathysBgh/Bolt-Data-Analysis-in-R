# Charger les packages nécessaires

library(dplyr)     # Pour la manipulation des données
library(readr)     # Pour lire le fichier CSV
library(ggplot2)   # Pour visualiser les données manquantes (optionnel)
library(naniar)    # Pour une analyse approfondie des données manquantes
library(dplyr)     # Pour la manipulation des données
library(lubridate) # Pour manipuler les dates et les heures
library(tidyr)     # Pour manipuler les données en format long et large


# Charger les données
data <- read_csv("bolt-merged-data-2014.csv")
# Échantillonner aléatoirement 100 000 points
data = data %>% sample_n(100000)

# Compter le nombre de valeurs manquantes par colonne
missing_summary <- data %>% summarise(across(everything(), ~sum(is.na(.))))
print(missing_summary)


weather <- read_csv("new_york_weather_2014.csv", skip = 3)
head(weather)



# Convertir la colonne DATE en format Date dans les deux datasets
# Convertir la colonne Date/Time en format date-heure (DateTime) en format Posix
data <- data %>%
  mutate(
    DateTime = mdy_hms(`Date/Time`),  # Convertir en format date-heure
    Date = as.Date(DateTime),         # Extraire la date
    Hour = format(DateTime, "%H:%M:%S"),  # Extraire l'heure
    JourSemaine = weekdays(DateTime), # Extraire le jour de la semaine
    Year = year(DateTime)            # Extraire l'année
  )

weather <- weather %>%
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d"))

# Fusionner les données des trajets avec les données météo
data <- data %>%
  left_join(weather, by = c("Date" = "DATE"))


# Vérifier un aperçu après la jointure sans tronquer les colonnes
glimpse(data)






library(forcats)

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


# Calculer le nombre de trajets par mois et par jour de la semaine
monthly_weekday_counts <- data %>%
  group_by(Month, JourSemaine) %>%
  summarise(Nombre_de_trajets = n(), .groups = 'drop')

# Convertir les colonnes 'Month' et 'JourSemaine' en facteurs avec les ordres chronologiques
monthly_weekday_counts <- monthly_weekday_counts %>%
  mutate(
    Month = factor(Month, levels = c("January", "February", "March", "April", 
                                     "May", "June", "July", "August", 
                                     "September", "October", "November", "December")),
         JourSemaine = factor(JourSemaine, levels = c("dimanche", "samedi", "vendredi", "jeudi", "mercredi", "mardi", "lundi")))

# Créer un graphique en barres empilées
ggplot(monthly_weekday_counts, aes(x = Month, y = Nombre_de_trajets, fill = JourSemaine)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Nombre de trajets par mois et par jour de la semaine",
       x = "Mois",
       y = "Nombre de trajets",
       fill = "Jour de la semaine") +
  theme_minimal()

# Calculer le nombre total de trajets par base
base_counts <- data %>%
  group_by(Base) %>%
  summarise(Nombre_de_trajets = n(), .groups = 'drop')

# Créer un graphique en barres
ggplot(base_counts, aes(x = reorder(Base, -Nombre_de_trajets), y = Nombre_de_trajets, fill = Base)) +
  geom_bar(stat = "identity") +
  labs(title = "Nombre total de trajets par base",
       x = "Base",
       y = "Nombre de trajets") +
  theme_minimal() +
  theme(legend.position = "none")  # Cacher la légende si elle n'est pas nécessaire




library(sf)
library(maps)

# Charger une carte simplifiée (par exemple, des États-Unis ou une région plus générale)
world <- st_as_sf(map("world", plot = FALSE, fill = TRUE))

# Visualiser les points des trajets sur la carte
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "white") +
  geom_point(data = data, aes(x = Lon, y = Lat), color = "blue", alpha = 0.3, size = 0.5) +
  labs(title = "Répartition géographique des trajets Bolt",
       x = "Longitude",
       y = "Latitude") +
  coord_sf() +
  theme_minimal()

# Définir les limites de la carte pour zoomer sur la région (par exemple, autour de New York)
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "white") +
  geom_point(data = data, aes(x = Lon, y = Lat, color = Base), alpha = 0.5, size = 1) +
  labs(title = "Répartition des trajets par base",
       x = "Longitude",
       y = "Latitude",
       color = "Base") +
  coord_sf(xlim = c(-74.3, -73.7), ylim = c(40.5, 41.0)) +  # Ajuste ces valeurs selon ta région
  theme_minimal()


# Définir des limites géographiques pour la côte Est pour vérifier si il y a des valeurs aberrantes
limites_cote_est_longitude <- c(-80, -65)  # Plage de longitudes pour la côte Est
limites_cote_est_latitude <- c(24, 50)     # Plage de latitudes pour la côte Est

# Filtrer les points en dehors de la côte Est
points_hors_cote_est <- data %>%
  filter(Lon < limites_cote_est_longitude[1] | Lon > limites_cote_est_longitude[2] |
         Lat < limites_cote_est_latitude[1] | Lat > limites_cote_est_latitude[2])

# Afficher le nombre de points et un aperçu
nrow(points_hors_cote_est)

# Créer une heatmap de densité sur la carte de New York
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "white") +
  stat_density2d(
    data = data,
    aes(x = Lon, y = Lat, fill = ..level..),
    geom = "polygon",
    alpha = 0.5
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(
    title = "Heatmap de la densité des trajets Bolt",
    x = "Longitude",
    y = "Latitude",
    fill = "Densité"
  ) +
  coord_sf(xlim = c(-74.3, -73.7), ylim = c(40.5, 41.0)) +  # Ajuste ces valeurs selon ta région
  theme_minimal()



# Ajouter une variable `Moment_journee` avec des intervalles de 6 heures
data <- data %>%
  mutate(
    Hour_numeric = as.numeric(substr(Hour, 1, 2)),  # Convertir l'heure en numérique
    Moment_journee = case_when(
      Hour_numeric >= 0 & Hour_numeric < 6 ~ "Nuit",
      Hour_numeric >= 6 & Hour_numeric < 12 ~ "Matin",
      Hour_numeric >= 12 & Hour_numeric < 18 ~ "Après-midi",
      Hour_numeric >= 18 & Hour_numeric < 24 ~ "Soir"
    )
  )

# Regrouper les données par Date, JourSemaine, Month et Moment_journee, puis compter le nombre de trajets
data_grouped <- data %>%
  group_by(Date, JourSemaine, Moment_journee, Month) %>%
  summarise(Nombre_de_trajets = n(), .groups = 'drop')

# Joindre les données météo pour inclure les variables explicatives
data_grouped <- data_grouped %>%
  left_join(weather, by = c("Date" = "DATE"))

# Vérifier un aperçu des données regroupées sans tronquer les colonnes
head(data_grouped)

set.seed(123)  # Pour garantir la reproductibilité
train_indices <- sample(1:nrow(data_grouped), 0.7 * nrow(data_grouped))
train_data <- data_grouped[train_indices, ]
test_data <- data_grouped[-train_indices, ]

# Créer le modèle avec l'ensemble d'entraînement
modele <- lm(Nombre_de_trajets ~ JourSemaine + Moment_journee + Month + MAX_TEMPERATURE_C +
             PRECIP_TOTAL_DAY_MM + HUMIDITY_MAX_PERCENT + WINDSPEED_MAX_KMH +
             VISIBILITY_AVG_KM, data = train_data)

# Évaluer le modèle avec l'ensemble de test
predictions <- predict(modele, newdata = test_data)

summary(modele)
# Calculer l'Erreur Absolue Moyenne (MAE)
mae <- mean(abs(predictions - test_data$Nombre_de_trajets))

# Calculer l'Erreur Quadratique Moyenne (MSE)
mse <- mean((predictions - test_data$Nombre_de_trajets)^2)

# Calculer la Racine de l'Erreur Quadratique Moyenne (RMSE)
rmse <- sqrt(mse)

# Calculer le R² (Coefficient de détermination)
sst <- sum((test_data$Nombre_de_trajets - mean(test_data$Nombre_de_trajets))^2)  # Somme des carrés totaux
sse <- sum((predictions - test_data$Nombre_de_trajets)^2)  # Somme des carrés résiduels
r_squared <- 1 - (sse / sst)

# Afficher les métriques
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R²:", r_squared, "\n")


