install.packages("tidyverse")
library(tidyverse)

#Constitution de mon frame qui contiendra les variables qui seront étudié

###############################################
fichier <- read.csv("API_SL/doc1.csv", skip = 4)

maroc <- fichier[fichier$Country.Code == "MAR", ]

maroc_2000_2024 <- maroc[, c("Country.Name", "Country.Code","Indicator.Name","Indicator.Code", paste0("X", 2000:2024))]
maroc_1 <- maroc_2000_2024 %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Year",
               values_to = "Chomage_Global") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))
view(maroc_1)

##############################################

fichier2 <- read.csv("ch/ch1.csv", skip = 4)

maroc <- fichier2[fichier2$Country.Code == "MAR", ]

maroc_2000_2024 <- maroc[, c("Country.Name", "Country.Code","Indicator.Name","Indicator.Code", paste0("X", 2000:2024))]
maroc_2 <- maroc_2000_2024 %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Year",
               values_to = "taux_femmes") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))
view(maroc_2)


###############################################
fichier3 <- read.csv("cf/cf1.csv", skip = 4)

maroc <- fichier3[fichier3$Country.Code == "MAR", ]

maroc_2000_2024 <- maroc[, c("Country.Name", "Country.Code","Indicator.Name","Indicator.Code", paste0("X", 2000:2024))]
maroc_3<- maroc_2000_2024 %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Year",
               values_to = "taux_hommes") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))
view(maroc_3)

###############################################
fichier4 <- read.csv("API_NY-2/1111.csv", skip = 4)

maroc <- fichier4[fichier4$Country.Code == "MAR", ]


maroc_2000_2024 <- maroc[, c("Country.Name", "Country.Code","Indicator.Name","Indicator.Code", paste0("X", 2000:2024))]


maroc_4<- maroc_2000_2024 %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Year",
               values_to = "PIB") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))

View(maroc_4)
###############################################
fichier5 <- read.csv("API_SL-2/11.csv", skip = 4)

maroc <- fichier5[fichier5$Country.Code == "MAR", ]


maroc_2000_2024 <- maroc[, c("Country.Name", "Country.Code","Indicator.Name","Indicator.Code", paste0("X", 2000:2024))]


maroc_5<- maroc_2000_2024 %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Year",
               values_to = "LABOR") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))

View(maroc_5)

############################################
morocco <- cbind(
  maroc_1,
  Chomage_Femmes = maroc_2$taux_femmes,
  Chomage_Hommes=maroc_3$taux_hommes,
  PIB_Croissance = maroc_4$PIB,
  Pop_Active= maroc_5$LABOR
)
View(morocco)
morocco <- morocco %>%
  select(-Indicator.Code, -Indicator.Name, -Country.Code, -Country.Name)

View(morocco)
###############################################

#Statistique descriptive de mes variables

library(dplyr)

stats_descriptives <- data.frame(
  Indicateur = c("Moyenne", "Écart-type", "Minimum", "Maximum", "Médiane"),
  
  Hommes = c(
    round(mean(morocco$Chomage_Hommes, na.rm = TRUE), 2),
    round(sd(morocco$Chomage_Hommes, na.rm = TRUE), 2),
    round(min(morocco$Chomage_Hommes, na.rm = TRUE), 2),
    round(max(morocco$Chomage_Hommes, na.rm = TRUE), 2),
    round(median(morocco$Chomage_Hommes, na.rm = TRUE), 2)
  ),
  
  Femmes = c(
    round(mean(morocco$Chomage_Femmes, na.rm = TRUE), 2),
    round(sd(morocco$Chomage_Femmes, na.rm = TRUE), 2),
    round(min(morocco$Chomage_Femmes, na.rm = TRUE), 2),
    round(max(morocco$Chomage_Femmes, na.rm = TRUE), 2),
    round(median(morocco$Chomage_Femmes, na.rm = TRUE), 2)
  ),
  
  General = c(
    round(mean(morocco$Chomage_Global, na.rm = TRUE), 2),
    round(sd(morocco$Chomage_Global, na.rm = TRUE), 2),
    round(min(morocco$Chomage_Global, na.rm = TRUE), 2),
    round(max(morocco$Chomage_Global, na.rm = TRUE), 2),
    round(median(morocco$Chomage_Global, na.rm = TRUE), 2)
 ),
Pop_Active = c(
  round(mean(morocco$Pop_Active, na.rm = TRUE), 2),
  round(sd(morocco$Pop_Active, na.rm = TRUE), 2),
  round(min(morocco$Pop_Active, na.rm = TRUE), 2),
  round(max(morocco$Pop_Active, na.rm = TRUE), 2),
  round(median(morocco$Pop_Active, na.rm = TRUE), 2)
),
  Taux_de_croissance = c(
    round(mean(morocco$PIB_Croissance, na.rm = TRUE), 2),
    round(sd(morocco$PIB_Croissance, na.rm = TRUE), 2),
    round(min(morocco$PIB_Croissance, na.rm = TRUE), 2),
    round(max(morocco$PIB_Croissance, na.rm = TRUE), 2),
    round(median(morocco$PIB_Croissance, na.rm = TRUE), 2)
  )
)

print(stats_descriptives)

###############################################

#courbe Évolution du taux de chômage des jeunes (15–24 ans) au Maroc

library(ggplot2)
library(tidyr)
library(dplyr)

morocco_long <- morocco %>%
  pivot_longer(
    cols = c(Chomage_Hommes, Chomage_Femmes, Chomage_Global),
    names_to = "Categorie",
    values_to = "Taux_chomage"
  ) %>%
  mutate(Categorie = case_when(
    Categorie == "Chomage_Hommes"  ~ "Hommes",
    Categorie == "Chomage_Femmes"  ~ "Femmes",
    Categorie == "Chomage_Global"  ~ "Général"
  ))

ggplot(morocco_long, aes(x = Year, y = Taux_chomage, color = Categorie)) +
  geom_line(size = 1.3) +
  geom_point(size = 2.5) +
  labs(
    title = "Évolution du taux de chômage des jeunes (15–24 ans) au Maroc",
    subtitle = "Comparaison Hommes vs Femmes vs Général",
    x = "Année",
    y = "Taux de chômage (%)",
    color = "Catégorie"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  ) +
  scale_color_manual(values = c(
    "Hommes" = "#2E86AB",
    "Femmes" = "#A23B72",
    "Général" = "#F4A261"
  )) +
  scale_x_continuous(breaks = seq(min(morocco$Year), max(morocco$Year), by = 2))

ggsave("evolution_chomage_maroc.png", width = 12, height = 7, dpi = 300)
###############################################

#Boite à moustache Hommes VS Femmes

morocco_hf <- morocco_long %>%
  filter(Categorie %in% c("Hommes", "Femmes"))

ggplot(morocco_hf, aes(x = Categorie, y = Taux_chomage, fill = Categorie)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 5, fill = "red") +
  labs(
    title = "Distribution du taux de chômage par sexe",
    subtitle = "Jeunes 15–24 ans, Maroc",
    x = "Sexe",
    y = "Taux de chômage (%)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c(
    "Hommes" = "#2E86AB",
    "Femmes" = "#A23B72"
  )) +
  theme(legend.position = "none")

ggsave("boxplot_chomage_sexe.png", width = 8, height = 6, dpi = 300)
###############################################

#Courbe femmes-hommes

ecart_moyen <- mean(morocco$ecart_genre, na.rm = TRUE)

ggplot(morocco, aes(x = Year, y = ecart_genre)) +
  geom_line(color = "#E63946", size = 1.3) +
  geom_point(color = "#E63946", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = ecart_moyen, linetype = "dotted") +
  labs(
    title = "Écart de chômage Femmes – Hommes",
    subtitle = "Valeur positive = chômage féminin plus élevé",
    x = "Année",
    y = "Différence (points de %)"
  ) +
  theme_minimal()

ggsave("ecart_genre_temps.png", width = 10, height = 6, dpi = 300)



###############################################

chomage_hommes <- maroc_3$taux_hommes
chomage_femmes <- maroc_2$taux_femmes
chomage_global <- morocco$Chomage_Global


classes <- pretty(c(chomage_hommes, chomage_femmes, chomage_global), n = 12)

# Histogrammes comparatifs hommes-femmes-général

par(mfrow = c(3,1), mar = c(4,4,3,1))

hist(chomage_hommes, breaks = classes, col = "#2E86AB", border = "white",
     xlim = range(classes), main = "Histogramme – Hommes", xlab = "Taux (%)", ylab = "Fréquence")

hist(chomage_femmes, breaks = classes, col = "#A23B72", border = "white",
     xlim = range(classes), main = "Histogramme – Femmes", xlab = "Taux (%)", ylab = "Fréquence")

hist(chomage_global, breaks = classes, col = "#F4A261", border = "white",
     xlim = range(classes), main = "Histogramme – Global", xlab = "Taux (%)", ylab = "Fréquence")

###############################################
library(ggplot2)
library(dplyr)


donnees_evo <- data.frame(
  Year = 2000:2024,
  Chomage_Global = morocco$Chomage_Global,
  Pop_Active = morocco$Pop_Active
)

# Graphique combiné du taux de chômage global et de la population active (2000-2024)

ggplot(donnees_evo, aes(x = Year)) +
  geom_line(aes(y = Chomage_Global, color = "Chômage Global"), size = 1.3) +
  geom_point(aes(y = Chomage_Global, color = "Chômage Global"), size = 2) +
  geom_line(aes(y = Pop_Active, color = "Population Active"), size = 1.3) +
  geom_point(aes(y = Pop_Active, color = "Population Active"), size = 2, shape = 17) +
  scale_y_continuous(
    name = "Taux de chômage (%)",
    sec.axis = sec_axis(~., name = "Population Active")
  ) +
  scale_color_manual(values = c("Chômage Global" = "red", "Population Active" = "#2E86AB")) +
  labs(title = "Évolution du taux de chômage global et de la population active (2000-2024)",
       x = "Année",
       color = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "bottom",
    axis.title.y.left = element_text(color = "red", face = "bold"),
    axis.title.y.right = element_text(color = "#2E86AB", face = "bold")
  )


ggsave("evolution_chomage_popactive.png", width = 12, height = 6, dpi = 300)

###############################################
library(ggplot2)
library(dplyr)

donnees_evo_pib <- data.frame(
  Year = 2000:2024,
  Chomage_Global = morocco$Chomage_Global,
  PIB_Croissance = morocco$PIB_Croissance
)

# Graphique combiné du taux de chômage global et de la croissance du PIB (2000-2024)
ggplot(donnees_evo_pib, aes(x = Year)) +
  geom_line(aes(y = Chomage_Global, color = "Chômage Global"), size = 1.3) +
  geom_point(aes(y = Chomage_Global, color = "Chômage Global"), size = 2) +
  geom_line(aes(y = PIB_Croissance, color = "Croissance PIB"), size = 1.3) +
  geom_point(aes(y = PIB_Croissance, color = "Croissance PIB"), size = 2)+
  scale_y_continuous(
    name = "Taux de chômage (%)",
    sec.axis = sec_axis(~., name = "Croissance du PIB (%)")
  ) +
  scale_color_manual(values = c("Chômage Global" = "#F4A261", "Croissance PIB" = "#2E86AB")) +
  labs(title = "Évolution du taux de chômage global et de la croissance du PIB (2000-2024)",
       x = "Année",
       color = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "bottom",
    axis.title.y.left = element_text(color = "#F4A261", face = "bold"),
    axis.title.y.right = element_text(color = "#2E86AB", face = "bold")
  )

ggsave("evolution_chomage_pib.png", width = 12, height = 6, dpi = 300)

###############################################

library(ggplot2)
library(dplyr)


donnees <- data.frame(
  Chomage_Global = morocco$Chomage_Global,
  PIB_Croissance = morocco$PIB_Croissance
)

#le modèle linéaire
modele <- lm(Chomage_Global ~ PIB_Croissance, data = donnees)


summary(modele)


cat("Équation de régression : Chomage_Global =", round(coef(modele)[1],2), 
    "+", round(coef(modele)[2],2), "* PIB_Croissance\n")

# Graphique avec droite de régression de PIB vs chomage

ggplot(donnees, aes(x = PIB_Croissance, y = Chomage_Global)) +
  geom_point(color = "black", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Régression linéaire : Taux de chômage vs Croissance du PIB",
    x = "Croissance du PIB (%)",
    y = "Taux de chômage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold")
  )


ggsave("regression_chomage_pib.png", width = 10, height = 6, dpi = 300)

###############################################

library(ggplot2)
library(dplyr)


donnees <- data.frame(
  Chomage_Global = morocco$Chomage_Global,
  Pop_Active = morocco$Pop_Active
)

#le modèle linéaire
modele <- lm(Chomage_Global ~ Pop_Active, data = donnees)

summary(modele)

cat("Équation de régression : Chomage_Global =", round(coef(modele)[1],2), 
    "+", round(coef(modele)[2],2), "* Pop_Active\n")

# Graphique avec droite de régression de Pop_Active vs chomage

ggplot(donnees, aes(x = Pop_Active, y = Chomage_Global)) +
  geom_point(color = "black", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Régression linéaire : Taux de chômage vs Population Active",
    x = "Population Active (%)",
    y = "Taux de chômage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold")
  )


ggsave("regression_chomage_Pop_Active.png", width = 10, height = 6, dpi = 300)

###############################################
# Charger les bibliothèques
library(ggplot2)
library(tidyr)


donnees_corr <- data.frame(
  Chomage_Global = morocco$Chomage_Global,
  Chomage_Femmes = morocco$Chomage_Femmes,
  Chomage_Hommes = morocco$Chomage_Hommes,
  PIB_Croissance = morocco$PIB_Croissance,
  Pop_Active = morocco$Pop_Active
)

#la matrice de corrélation
mat_corr <- cor(donnees_corr, use = "complete.obs")


mat_long <- as.data.frame(as.table(mat_corr))

corr_plot <- ggplot(mat_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Matrice de corrélation", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14))

print(corr_plot)
ggsave("matrice_correlation.png", plot = corr_plot, width = 8, height = 6, dpi = 300)
