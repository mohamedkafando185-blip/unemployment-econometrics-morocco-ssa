

#  1INSTALLATION ET CHARGEMENT DES PACKAGES 
# Cette boucle vérifie si tu as les packages. Sinon, elle les installe.
packages_necessaires <- c("wbstats", "dplyr", "tidyr", "corrplot")

for (pkg in packages_necessaires) {
  if (!require(pkg, character.only = TRUE)) {
    print(paste("Installation du package manquant :", pkg))
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#2. GESTION DES DOSSIERS
# Pour éviter l'erreur "unable to start png() device", on crée le dossier
if (!dir.exists("images")) {
  dir.create("images")
  print("dossier 'images' créé.")
} else {
  print("dossier 'images' exite deja")
}

# 3. DÉFINITION DES VARIABLES (CODES TECHNIQUES UNIQUEMENT)
# On utilise uniquement les codes WB pour éviter l'erreur "Bad Request"
codes_banque_mondiale <- c(
  "SL.UEM.1524.ZS",       # 1. Chômage Jeunes (Global)
  "SL.UEM.1524.MA.ZS",    # 2. Chômage Jeunes (Hommes)
  "SL.UEM.1524.FE.ZS",    # 3. Chômage Jeunes (Femmes)
  "SL.TLF.ACTI.1524.ZS",  # 4. Population Active (Participation)
  "FP.CPI.TOTL.ZG",       # 5. Inflation
  "BX.KLT.DINV.WD.GD.ZS", # 6. IDE (Investissement Etranger)
  "NV.AGR.TOTL.ZS",       # 7. Agriculture (VAB)
  "NE.GDI.FTOT.ZS",       # 8. Investissement Domestique (FBCF)
  "NY.GDP.MKTP.KD.ZG"     # 9. Croissance PIB
)


#  4. TÉLÉCHARGEMENT DES DONNÉES 
# On récupère tout d'un coup pour le Maroc (MA) de 2000 à 2024
raw_data <- wb_data(
  country = "MA", 
  indicator = codes_banque_mondiale, 
  start_date = 2000, 
  end_date = 2024,
  return_wide = FALSE
)

print("teléchargement terminé.")

#5. NETTOYAGE ET TRADUCTION (RENOMMAGE PROPRE)
data_complet <- raw_data %>%
  select(date, indicator_id, value) %>%
  mutate(Variable_Nom = case_when(
    indicator_id == "SL.UEM.1524.ZS"       ~ "Chomage_Global",
    indicator_id == "SL.UEM.1524.MA.ZS"    ~ "Chomage_Hommes",
    indicator_id == "SL.UEM.1524.FE.ZS"    ~ "Chomage_Femmes",
    indicator_id == "SL.TLF.ACTI.1524.ZS"  ~ "Pop_Active",
    indicator_id == "FP.CPI.TOTL.ZG"       ~ "Inflation",
    indicator_id == "BX.KLT.DINV.WD.GD.ZS" ~ "IDE_Etranger",
    indicator_id == "NV.AGR.TOTL.ZS"       ~ "Agriculture_VAB",
    indicator_id == "NE.GDI.FTOT.ZS"       ~ "Inv_Domestique",
    indicator_id == "NY.GDP.MKTP.KD.ZG"    ~ "PIB_Croissance"
  )) %>%
  select(-indicator_id) %>%
  pivot_wider(names_from = Variable_Nom, values_from = value) %>%
  rename(Annee = date) %>%
  arrange(Annee)

# Sauvegarde du fichier propre pour tout le groupe
write.csv(data_complet, "data_maroc_final_complet.csv", row.names = FALSE)
print("tape 3 : Fichier 'data_maroc_final_complet.csv' sauvegardé.")

# 6. CRÉATION DE LA MATRICE DE CORRÉLATION "WAOUH"
print("réation du graphique...")

# Préparation : on enlève l'année (pas de sens mathématique)
data_corr <- data_complet %>% select(-Annee)

# Calcul de la matrice
Matrice <- cor(data_corr, use = "complete.obs")

# Sauvegarde de l'image haute définition
png("images/Matrice_Correlation_Globale.png", width=1200, height=1200, res=130)

corrplot(Matrice, 
         method = "color",        # Carrés de couleur
         type = "upper",          # Triangle supérieur
         order = "hclust",        # Regroupement intelligent
         addCoef.col = "black",   # Chiffres en noir
         tl.col = "black",        # Texte en noir
         tl.srt = 45,             # Texte incliné
         number.cex = 0.7,        # Taille des chiffres
         diag = FALSE,            # Pas de diagonale
         title = "Matrice des Corrélations : Déterminants du Chômage au Maroc",
         mar = c(0,0,2,0)
)

dev.off() # IMPORTANT : Ferme et enregistre l'image

print("TERMINÉ ! Ouvre le dossier 'images' pour voir ton graphique.")
print("Tu as aussi le fichier 'data_maroc_final_complet.csv' pour tes camarades.")


install.packages("gridExtra")
install.packages(c("gridExtra", "ggpubr"))
# 1. Chargement des packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)   # Pour assembler les graphiques
library(corrplot)
library(gridExtra) # Pour le tableau des stats

# 2. Chargement des données (On reprend le fichier complet généré avant)
df <- read.csv("data_maroc_final_complet.csv")

# 1. Calcul des stats
mes_stats <- df %>%
  select(Inflation, IDE_Etranger, Agriculture_VAB, Inv_Domestique, Chomage_Global) %>%
  summarise_all(list(
    Moyenne = ~mean(., na.rm = TRUE),
    Ecart_Type = ~sd(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Max = ~max(., na.rm = TRUE)
  )) %>%
  pivot_longer(everything(), names_to = "Mesure", values_to = "Valeur") %>%
  separate(Mesure, into = c("Variable", "Stat"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Stat, values_from = Valeur)

# 2. Arrondir UNIQUEMENT les colonnes numériques (C'est la correction !)
mes_stats_propres <- mes_stats %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# 3. Sauvegarder le tableau en image
png("images/m2_stats_table.png", width=600, height=300)
grid.table(mes_stats_propres) # On utilise le tableau corrigé ici
dev.off()


# ÉTAPE 2 : SCATTERPLOTS (Analyse bivariée)-

# Graphe A : Chômage vs Agriculture (Le facteur pluie)
p1 <- ggplot(df, aes(x=Agriculture_VAB, y=Chomage_Global)) +
  geom_point(color="darkgreen", size=3, alpha=0.6) +
  geom_smooth(method="lm", color="red", se=FALSE) +
  labs(title="Agriculture vs Chômage", x="VAB Agricole (% PIB)", y="Chômage (%)") +
  theme_bw()

# Graphe B : Chômage vs Investissement (Le paradoxe)
p2 <- ggplot(df, aes(x=Inv_Domestique, y=Chomage_Global)) +
  geom_point(color="blue", size=3, alpha=0.6) +
  geom_smooth(method="lm", color="red", se=FALSE) +
  labs(title="Investissement vs Chômage", x="Inv. Domestique (% PIB)", y="Chômage (%)") +
  theme_bw()

# Graphe C : Chômage vs Inflation (Courbe de Phillips ?)
p3 <- ggplot(df, aes(x=Inflation, y=Chomage_Global)) +
  geom_point(color="purple", size=3, alpha=0.6) +
  geom_smooth(method="lm", color="red", se=FALSE) +
  labs(title="Inflation vs Chômage", x="Inflation (%)", y="Chômage (%)") +
  theme_bw()

# Graphe D : Chômage vs IDE (L'impact étranger)
p4 <- ggplot(df, aes(x=IDE_Etranger, y=Chomage_Global)) +
  geom_point(color="orange", size=3, alpha=0.6) +
  geom_smooth(method="lm", color="red", se=FALSE) +
  labs(title="IDE vs Chômage", x="IDE (% PIB)", y="Chômage (%)") +
  theme_bw()

# Assemblage des 4 graphes en une seule image
graphe_final <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
ggsave("images/m2_scatterplots_grid.png", graphe_final, width = 10, height = 8)

print("✅ Étape 2 : Scatterplots générés et sauvegardés.")


# Préparation des données (sans l'année)
data_corr <- df %>% select(-Annee)
Matrice <- cor(data_corr, use = "complete.obs")

# Sauvegarde 
png("images/Matrice_Correlation_Globale.png", width=1000, height=1000, res=100)
corrplot(Matrice, method="color", type="upper", order="hclust", 
         addCoef.col="black", tl.col="black", tl.srt=45, diag=FALSE,
         title="Matrice des Corrélations : Déterminants du Chômage", mar=c(0,0,2,0))
dev.off()

print("matrice générée")
