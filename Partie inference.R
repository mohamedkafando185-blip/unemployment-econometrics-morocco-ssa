fichier <- read.csv2("C:/Users/dinis/OneDrive/Documents/Tps langage R/chomage(2000-2024).csv", skip=4, sep=',')
fichier2<- read.csv2("C:/Users/dinis/OneDrive/Documents/Tps langage R/GDP_growth.csv", skip=4, sep=',')
fichier3<- read.csv2("C:/Users/dinis/OneDrive/Documents/Tps langage R/PA.csv", skip=4,sep=',')
fichier4<- read.csv2("C:/Users/dinis/OneDrive/Documents/Tps langage R/111.csv",skip=4,sep=',')
fichier5<- read.csv2("C:/Users/dinis/OneDrive/Documents/Tps langage R/Inflation.csv",skip=4,sep=',')
fichier6<- read.csv2("C:/Users/dinis/OneDrive/Documents/Tps langage R/PIB.csv",skip=4,sep=',')
fichier7<- read.csv2("C:/Users/dinis/OneDrive/Documents/Tps langage R/Indice_invest_E.csv",skip=4,sep=',')
fichier8<- read.csv2("C:/Users/dinis/OneDrive/Documents/Tps langage R/Chms.csv",skip=4,sep=',')
fichier9<- read.csv2("C:/Users/dinis/OneDrive/Documents/Tps langage R/Cfms.csv",skip=4,sep=',')
fichier10<- read.csv2("C:/Users/dinis/OneDrive/Documents/Tps langage R/infla.csv",skip=4,sep=',')
fichier11<- read.csv2("C:/Users/dinis/OneDrive/Documents/Tps langage R/Agriva.csv",skip=4,sep=',')
fichier12<- read.csv2("C:/Users/dinis/OneDrive/Documents/Tps langage R/Inv_Dom.csv",skip=4,sep=',')

View(fichier3)
morocco <-data.frame(
  Annee=2000:2024,
  Chomage_Global= as.numeric(fichier[149, 45:(ncol(fichier)-1)]),
  Chomage_Hommes= as.numeric(fichier8[149, 45:(ncol(fichier8)-1)]),
  Chomage_Femmes= as.numeric(fichier9[149, 45:(ncol(fichier9)-1)]),
  Pop_Active=as.numeric(fichier3[149, 45:(ncol(fichier3)-1)]),
  Inflation= as.numeric(fichier10[149, 45:(ncol(fichier10)-1)]),
  IDE_Etranger=as.numeric(fichier7[149, 45:(ncol(fichier7)-1)]),
  Agriculturee_VAB=as.numeric(fichier11[149, 45:(ncol(fichier11)-1)]),
  Inv_Domestique=as.numeric(fichier12[149, 45:(ncol(fichier12)-1)]),
  PIB_Croissance=as.numeric(fichier2[149, 45:(ncol(fichier2)-1)])
  
)
summary(morocco)
cor.test(morocco$Inflation, morocco$Chomage_Global, method="pearson")
cor.test(morocco$Agriculturee_VAB, morocco$Chomage_Global, method="pearson")
cor.test(morocco$Volatilite_Agricole, morocco$Chomage_Global, method="pearson")

model1 <- lm(Chomage_Global ~ Inflation + Agriculturee_VAB, data=morocco)
summary(model1)
res <- residuals(model1)
shapiro.test(res)


morocco$Volatilite_Agricole_Lag <- c(NA, morocco$Volatilite_Agricole[-length(morocco$Volatilite_Agricole)])
model_vol_lag <- lm(Chomage_Global ~ Inflation + Volatilite_Agricole_Lag, data=morocco)
summary(model_vol_lag)
shapiro.test(residuals(model_vol_lag))
install.packages('ggplot2')
install.packages('reshape2')
library(ggplot2)
library(reshape2)
df_plot <- morocco[, c("Annee", "Chomage_Global", "Inflation", "Volatilite_Agricole")]
df_melt <- melt(df_plot, id.vars = "Annee")
ggplot(df_melt, aes(x = Annee, y = value, color = variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Évolution conjointe du Chômage, Inflation et Volatilité Agricole",
       x = "Année", y = "Valeur",
       color = "Variable") +
  theme_minimal() +
  theme(text = element_text(size = 12))



df_plot_lag <- morocco[, c("Annee", "Chomage_Global", "Inflation", "Volatilite_Agricole_Lag")]
df_melt_lag <- melt(df_plot_lag, id.vars = "Annee")

ggplot(df_melt_lag, aes(x = Annee, y = value, color = variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Évolution du Chômage, Inflation et Volatilité Agricole (Lag 1 an)",
       x = "Année", y = "Valeur",
       color = "Variable") +
  theme_minimal() +
  theme(text = element_text(size = 12))

