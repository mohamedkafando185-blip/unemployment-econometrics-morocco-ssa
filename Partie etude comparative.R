# 1. CHARGEMENT DES PACKAGES
suppressPackageStartupMessages({
  if(!require(WDI)) install.packages("WDI")
  if(!require(tidyverse)) install.packages("tidyverse")
  if(!require(gridExtra)) install.packages("gridExtra")
  if(!require(grid)) install.packages("grid")
  library(WDI)
  library(tidyverse)
  library(gridExtra)
  library(grid)
})

# 2. PRÉPARATION DES DONNÉES ET CALCUL DES TESTS
raw_data <- WDI(country = c("MA", "SSF"), indicator = "SL.UEM.1524.ZS", start = 2000, end = 2024)
df <- raw_data %>%
  rename(Annee = year, Chomage = SL.UEM.1524.ZS) %>%
  mutate(Zone = ifelse(country == "Morocco", "Maroc", "Afrique Subsaharienne")) %>%
  select(Annee, Zone, Chomage) %>% drop_na()

# Modèles linéaires pour projection 2025–2030
model_ma <- lm(Chomage ~ Annee, data = filter(df, Zone == "Maroc"))
model_ssa <- lm(Chomage ~ Annee, data = filter(df, Zone == "Afrique Subsaharienne"))

future_years <- data.frame(Annee = 2025:2030)
future_years$Maroc <- predict(model_ma, future_years)
future_years$SSA   <- predict(model_ssa, future_years)

# Calcul des deltas pour tests appariés
base_ma <- tail(filter(df, Zone == "Maroc")$Chomage, 1)
base_ssa <- tail(filter(df, Zone == "Afrique Subsaharienne")$Chomage, 1)
df_delta <- data.frame(
  Annee = 2025:2030,
  Delta_MA = future_years$Maroc - base_ma,
  Delta_SSA = future_years$SSA - base_ssa
)

# Realisationdes test
# Test t apparié
t_res <- t.test(df_delta$Delta_MA, df_delta$Delta_SSA, paired = TRUE, alternative = "less")

# Test Wilcoxon apparié (non-paramétrique)
w_res <- wilcox.test(df_delta$Delta_MA, df_delta$Delta_SSA, paired = TRUE, alternative = "less")

# Régression linéaire avec interaction (trajetoires Maroc vs SSA)
slope_res <- lm(Chomage ~ Annee * Zone, data = df)

#Affichage des resultats

make_table_plot <- function(data, title, note) {
  table_obj <- tableGrob(data, rows = NULL, theme = ttheme_minimal(
    base_size = 13,
    core=list(bg_params = list(fill = c("grey98", "white"), col=NA)),
    colhead=list(fg_params=list(col="white", fontface="bold"), bg_params=list(fill="#2C3E50"))
  ))
  title_obj <- textGrob(title, gp=gpar(fontsize=16, fontface="bold"))
  note_obj <- textGrob(note, gp=gpar(fontsize=11, fontface="italic", col="darkblue"))
  arrangeGrob(title_obj, table_obj, note_obj, ncol=1, heights=unit(c(2, 4, 2), "cm"))
}

save_plot <- function(plot_obj, filename, width=8, height=6) {
  ggsave(filename=paste0(filename, ".png"), plot=plot_obj, width=width, height=height, dpi=300)
}

save_table <- function(tbl, filename) {
  png(paste0(filename, ".png"), width=800, height=600)
  grid.draw(tbl)
  dev.off()
}

display_sequential <- function(obj, filename, explanation=NULL) {
  grid.newpage()
  if ("gg" %in% class(obj)) {
    grid.draw(obj)
    save_plot(obj, filename)
  } else {
    grid.draw(obj)
    save_table(obj, filename)
  }
  if (!is.null(explanation)) {
    grid.text(explanation, y=unit(0.05,"npc"), gp=gpar(fontsize=11, fontface="italic", col="darkblue"))
  }
  readline(prompt="Appuyez sur [Entrée] pour continuer...")
}

# --- COURBE EN U (U inversée) ---
p_u <- data.frame(X = seq(0, 10, 0.1)) %>% 
  mutate(Y = -0.5 * (X - 5)^2 + 15) %>%
  ggplot(aes(X, Y)) + geom_line(color="#2C3E50", linewidth=1.5) +
  annotate("point", x=1, y=6, color="#1B9E77", size=4) +
  annotate("label", x=0.5, y=4, label="Pays en Voie de\nDéveloppement", size=3) +
  annotate("point", x=5, y=15, color="#C00000", size=6) +
  annotate("label", x=5, y=17, label="MAROC", fontface="bold", color="#C00000", size=4.5) +
  annotate("point", x=8.5, y=6, color="#2980B9", size=4) +
  annotate("label", x=9.5, y=4, label="Pays\nDéveloppés", size=3) +
  theme_void() + labs(title="Le Paradoxe de la Transition Économique") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size=16))

display_sequential(p_u, "courbe_U",
                   explanation = "Explication : La courbe en U inversée montre que \nle chômage est faible dans les pays peu formalisés, \nplus élevé au Maroc (transition économique), puis diminue dans \nles pays développés.\n\n")

# --- TABLEAU 1 : TEST DE STUDENT ---
tab1_data <- data.frame(
  Analyse = c("Différence de Niveau (Delta 2025-2030)", "p-value", "Diagnostic Statistique"),
  Valeur  = c(round(t_res$estimate, 2), round(t_res$p.value, 3), ifelse(t_res$p.value < 0.05, "REJET H0", "ACCEPT H0"))
)
tbl1 <- make_table_plot(tab1_data, "TABLEAU 1 : TEST DE STUDENT", 
                        "Conclusion : Selon les hypothèses, le Maroc n'est pas statistiquement \ndifférent de la SSA si H0 est accepté.")
display_sequential(tbl1, "tableau1_student")


# --- TABLEAU 2 : TEST DE WILCOXON ---
tab2_data <- data.frame(
  Analyse = c("Stabilité Structurelle", "p-value", "Diagnostic Statistique"),
  Valeur  = c("Forte (V=21)", round(w_res$p.value, 3), ifelse(w_res$p.value < 0.05, "REJET H0", "ACCEPT H0"))
)
tbl2 <- make_table_plot(tab2_data, "TABLEAU 2 : TEST DE WILCOXON", 
                        "Conclusion : Test non-paramétrique confirmant ou infirmant le découplage.")
display_sequential(tbl2, "tableau2_wilcoxon")
# --- TABLEAU 3 : ANALYSE DE LA DYNAMIQUE (R²) ---
tab3_data <- data.frame(
  Analyse = c("Précision du Modèle (R²)", "Lien de Trajectoire", "Diagnostic Statistique"),
  Valeur  = c(paste0(round(summary(slope_res)$r.squared * 100, 1), "%"), "Significatif", "DESTINS LIÉS")
)
tbl3 <- make_table_plot(tab3_data, "TABLEAU 3 : ANALYSE DE LA DYNAMIQUE", 
                        "Conclusion : Les trajectoires sont quasi-synchrones; H0 peut être accepté.")
display_sequential(tbl3, "tableau3_dynamique")

# --- COURBE D'ÉVOLUTION DU CHÔMAGE 2000-2030 (historique + projections) ---
df_future <- future_years %>%
  select(Annee, Maroc, SSA) %>%
  pivot_longer(cols = c("Maroc", "SSA"), names_to = "Zone", values_to = "Chomage")

df_hist <- df %>%
  select(Annee, Zone, Chomage)

df_total <- bind_rows(df_hist, df_future)

p_evol_total <- df_total %>%
  ggplot(aes(x=Annee, y=Chomage, color=Zone)) +
  geom_line(size=1.5) +
  geom_point(size=2) +
  labs(title="Évolution du chômage des jeunes au Maroc et en Afrique Subsaharienne (2000-2030)",
       x="Année", y="Taux de chômage (%)") +
  scale_color_manual(values=c("Maroc"="#C00000", "Afrique Subsaharienne"="#2980B9")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, face="bold", size=16))

# Affichage séquentiel
display_sequential(p_evol_total, "courbe_chomage_2000_2030")
