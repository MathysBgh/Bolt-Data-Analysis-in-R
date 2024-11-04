# Charger les packages nécessaires

library(dplyr)     # Pour la manipulation des données
library(readr)     # Pour lire le fichier CSV
library(ggplot2)   # Pour visualiser les données manquantes (optionnel)
library(naniar)    # Pour une analyse approfondie des données manquantes
library(dplyr)     # Pour la manipulation des données
library(lubridate) # Pour manipuler les dates et les heures
library(tidyr)     # Pour manipuler les données en format long et large
library(forcats)  # Pour manipuler les facteurs
library(car)  # Charger le package car pour la fonction vif
library(randomForest) # Charger le package randomForest pour l'algorithme de forêt aléatoire


# Liste des fichiers à lire avec le chemin complet
files <- list.files(pattern = "bolt-raw-data-.*\\.csv", full.names = TRUE)
print(files)

# Liste des mois correspondants
months <- c("April", "May", "June", "July", "August", "September")

# Fonction pour lire et ajouter la colonne 'Month'
read_and_label <- function(file, month) {
  data <- read.csv(file)
  data$Month <- month
  return(data)
}


# Lecture et fusion des fichiers
data <- bind_rows(
  lapply(seq_along(files), function(i) read_and_label(files[i], months[i]))
)

data <- data %>%
  rename(`Date/Time` = Date.Time)
  
# Afficher le nombre de lignes
nrow(data)  
names(data)
head(data)

# Retirer les lignes doublons dans le DataFrame
data <- data %>% distinct()

# Afficher le nombre de lignes après suppression des doublons
nrow(data)


# Compter le nombre de valeurs manquantes par colonne
missing_summary <- data %>% summarise(across(everything(), ~sum(is.na(.))))
print(missing_summary)

# Échantillonner aléatoirement 100 000 points
data = data %>% sample_n(100000)



weather <- read_csv("new_york_weather_2014.csv", skip = 3)
head(weather)
names(weather)


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

# Définir les limites de la carte pour zoomer autour de New York
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "white") +
  geom_point(data = data, aes(x = Lon, y = Lat, color = Base), alpha = 0.5, size = 1) +
  labs(title = "Répartition des trajets par base",
       x = "Longitude",
       y = "Latitude",
       color = "Base") +
  coord_sf(xlim = c(-74.3, -73.7), ylim = c(40.5, 41.0)) + 
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
    Hour_numeric = ifelse(as.numeric(substr(Hour, 1, 2)) == 23 & as.numeric(substr(Hour, 4, 5)) >= 30, 
                          23,  # Si l'heure est 23:30 ou plus, rester à 23
                          as.numeric(substr(Hour, 1, 2)) + ifelse(as.numeric(substr(Hour, 4, 5)) >= 30, 1, 0)
    ),
    Moment_journee = case_when(
      Hour_numeric >= 0 & Hour_numeric < 6 ~ "Nuit",
      Hour_numeric >= 6 & Hour_numeric < 12 ~ "Matin",
      Hour_numeric >= 12 & Hour_numeric < 18 ~ "Après-midi",
      Hour_numeric >= 18 & Hour_numeric < 24 ~ "Soir"
    )
  )

# afficher head(data) mais que les colonnes JourSemaine + Moment_journee + Month + MAX_TEMPERATURE_C
head(data[, c("JourSemaine","Hour_numeric","Hour", "Moment_journee", "Month", "MAX_TEMPERATURE_C")],10)

# Regrouper les données par Date, JourSemaine, Month et Moment_journee, puis compter le nombre de trajets
data_grouped <- data %>%
  group_by(Date, JourSemaine, Moment_journee, Month) %>%
  summarise(Nombre_de_trajets = n(), .groups = 'drop')

# Joindre les données météo pour inclure les variables explicatives
data_grouped <- data_grouped %>%
  left_join(weather, by = c("Date" = "DATE"))


data_grouped <- data_grouped %>%
  mutate(
    Temperature_moment_journee = case_when(
      Moment_journee == "Matin" ~ TEMPERATURE_MORNING_C,
      Moment_journee == "Après-midi" ~ TEMPERATURE_NOON_C,
      Moment_journee == "Soir" ~ TEMPERATURE_EVENING_C,
      Moment_journee == "Nuit" ~ TEMPERATURE_NIGHT_C
    )
  )


# Vérifier un aperçu des données regroupées utilisées pour la modélisation certaines colonnes sans tronquer
head(data_grouped[, c("Date","JourSemaine", "Moment_journee", "Month", "Temperature_moment_journee", "Nombre_de_trajets")],10)

set.seed(123)  # Pour garantir la reproductibilité
train_indices <- sample(seq_len(nrow(data_grouped)), 0.7 * nrow(data_grouped))
train_data <- data_grouped[train_indices, ]
test_data <- data_grouped[-train_indices, ]

# tester la corrélation entre les variables explicatives et la variable cible
cor(train_data$Nombre_de_trajets, train_data$Temperature_moment_journee)
cor(train_data$Nombre_de_trajets, train_data$PRECIP_TOTAL_DAY_MM)
cor(train_data$Nombre_de_trajets, train_data$HUMIDITY_MAX_PERCENT)
cor(train_data$Nombre_de_trajets, train_data$WINDSPEED_MAX_KMH)
cor(train_data$Nombre_de_trajets, train_data$VISIBILITY_AVG_KM)

# Effectuer une ANOVA
summary(aov(Nombre_de_trajets ~ Moment_journee, data = train_data))
summary(aov(Nombre_de_trajets ~ Month, data = train_data))
summary(aov(Nombre_de_trajets ~ JourSemaine, data = train_data))


# faire un vif pour étudier la multicollinéarité entre les variables explicatives
vif(lm(Nombre_de_trajets ~ JourSemaine + Moment_journee + Month + Temperature_moment_journee + VISIBILITY_AVG_KM, data = train_data))

# Calcul des corrélations entre Temperature_moment_journee et d'autres variables numériques
cor_matrix <- cor(train_data %>% 
                  select(Temperature_moment_journee, MAX_TEMPERATURE_C, PRECIP_TOTAL_DAY_MM, 
                         HUMIDITY_MAX_PERCENT, WINDSPEED_MAX_KMH, VISIBILITY_AVG_KM,), use = "complete.obs")

# Afficher la matrice de corrélation
print(cor_matrix)



# Créer le modèle avec l'ensemble d'entraînement
modele <- lm(Nombre_de_trajets ~ JourSemaine + Moment_journee + Month + VISIBILITY_AVG_KM, data = train_data)


# Évaluer le modèle avec l'ensemble de test
predictions <- predict(modele, newdata = test_data)

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
cat("R²:", r_squared, "\n")
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")

# pareil avec random forest


# Créer le modèle avec l'ensemble d'entraînement

modele_rf <- randomForest(Nombre_de_trajets ~ JourSemaine + Moment_journee + Month + VISIBILITY_AVG_KM, data = train_data)

# Évaluer le modèle avec l'ensemble de test
predictions_rf <- predict(modele_rf, newdata = test_data)

# Calculer l'Erreur Absolue Moyenne (MAE)
mae_rf <- mean(abs(predictions_rf - test_data$Nombre_de_trajets))

# Calculer l'Erreur Quadratique Moyenne (MSE)
mse_rf <- mean((predictions_rf - test_data$Nombre_de_trajets)^2)

# Calculer la Racine de l'Erreur Quadratique Moyenne (RMSE)
rmse_rf <- sqrt(mse_rf)

# Calculer le R² (Coefficient de détermination)
sst_rf <- sum((test_data$Nombre_de_trajets - mean(test_data$Nombre_de_trajets))^2)  # Somme des carrés totaux
sse_rf <- sum((predictions_rf - test_data$Nombre_de_trajets)^2)  # Somme des carrés résiduels
r_squared_rf <- 1 - (sse_rf / sst_rf)

# Afficher les métriques
cat("R² (Random Forest):", r_squared_rf, "\n")
