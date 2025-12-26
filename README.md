# unemployment-econometrics-morocco-ssa
📊 Étude économétrique du chômage des jeunes au Maroc (2000-2024). Modélisation sous R des déterminants macroéconomiques (PIB, VAB Agricole, Inflation) et analyse de synchronisation régionale avec l'Afrique Subsaharienne via tests d'inférence (Wilcoxon, Student).

# 🇲🇦 Analyse Statistique du Chômage des Jeunes au Maroc (2000-2024)

[cite_start]Ce projet de fin de module en **Inférence Statistique** (EMI - Département Informatique) analyse les racines de la "croissance sans emploi" au Maroc à travers une approche empirique et économétrique[cite: 191, 209].

## 🎯 Objectifs Scientifiques
- [cite_start]**Déconstruire les mécanismes du chômage** en mobilisant les outils de l'inférence statistique[cite: 212].
- [cite_start]**Tester les modèles classiques** : Validité de la Loi d'Okun et de la Courbe de Phillips dans le contexte marocain[cite: 215].
- [cite_start]**Analyse de Synchronisation Régionale** : Comparaison des trajectoires entre le Maroc et l'Afrique Subsaharienne (SSA)[cite: 216].

## 📊 Variables & Dataset (Source : Banque Mondiale)
[cite_start]L'étude s'appuie sur les indicateurs officiels (World Development Indicators) de 2000 à 2024[cite: 228, 229]:
* [cite_start]**Variable Expliquée (Y)** : Taux de chômage des jeunes (15-24 ans)[cite: 233, 235].
* **Variables Explicatives (X)** : 
    * [cite_start]Valeur Ajoutée Agricole (VAB) - Proxy de la pluviométrie[cite: 238, 240].
    * [cite_start]Croissance du PIB[cite: 248].
    * [cite_start]Inflation (Indice des prix)[cite: 243].
    * [cite_start]Investissements (FBCF & IDE)[cite: 241, 246].

## 🧪 Méthodologie Statistique (R)
L'analyse a été conduite via :
1. [cite_start]**Tests de Normalité** : Shapiro-Wilk sur les résidus[cite: 132].
2. [cite_start]**Comparaison de Moyennes** : Test de Student apparié[cite: 203].
3. [cite_start]**Analyse de Stabilité Structurelle** : Test de **Wilcoxon**[cite: 203].
4. [cite_start]**Régressions Linéaires** : Modélisation des impacts des variables macroéconomiques[cite: 222].

## 📈 Résultats & Insights Majeurs
* [cite_start]**Invalidation de Phillips** : L'inflation n'a pas d'effet significatif sur la réduction du chômage au Maroc (p-value > 0.05)[cite: 135, 137].
* [cite_start]**Loi d'Okun Modérée** : La croissance économique explique environ **22%** de la variation du chômage (R² = 0.219)[cite: 84].
* [cite_start]**Symbiose Régionale** : Un parallélisme structurel frappant avec l'Afrique Subsaharienne, avec un **R² exceptionnel de 91,4%** prouvant que les trajectoires sont quasi-synchrone[cite: 159, 169].
* [cite_start]**Paradoxe de la Modernisation** : Le Maroc se situe au sommet de la courbe en U inversée, signe d'une transition économique où la modernisation détruit plus d'emplois traditionnels qu'elle n'en crée[cite: 171].

## 🛠 Tech Stack
- [cite_start]**Language** : R (ggplot2, stats, stats4) [cite: 219]
- [cite_start]**Données** : Banque Mondiale / HCP [cite: 228]
- [cite_start]**Modélisation** : Séries temporelles et Inférence [cite: 222]

---
[cite_start]**Réalisé à l'École Mohammadia d'Ingénieurs (EMI)** *Équipe : ABOUBAKAR A., ZOUNGRANA A., DIN I., KAFANDO M.* [cite: 196, 197, 198]
