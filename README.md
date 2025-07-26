# cerealise

## Description

`cerealise` est un package R développé dans le cadre du projet de fin de module "Traitement Statistique avec R" à l’ENSAE (ISEP 2, année 2025).  
Il permet de traiter, nettoyer, convertir et analyser les données de consommation alimentaire issues de l’enquête EHCVM (1ère édition - Sénégal), notamment à partir de la base des céréales.

## Fonctionnalités principales

- Nettoyage et formatage des bases de consommation céréalière
- Conversion des unités de mesure
- Calcul des quantités et des dépenses alimentaires
- Production d’indicateurs statistiques cohérents
- Pipeline automatisé pour générer une base apurée prête à l’analyse

## Installation

Vous pouvez installer ce package directement depuis GitHub :

```r
# Installer 'devtools' si ce n'est pas déjà fait
install.packages("devtools")

# Installer le package depuis GitHub
devtools::install_github("mathlsedensae/MonPackage")
