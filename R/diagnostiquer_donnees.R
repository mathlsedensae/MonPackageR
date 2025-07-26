#' Diagnostiquer les problemes potentiels dans les donnees
#' @param donnees Liste des donnees brutes
#' @return Liste des diagnostics
#' @export
diagnostiquer_donnees <- function(donnees) {

  diagnostics <- list()

  # Diagnostic des donnees cereales
  cereales <- donnees$cereales
  diagnostics$cereales <- list(
    nb_lignes = nrow(cereales),
    nb_colonnes = ncol(cereales),
    nb_menages_uniques = length(unique(cereales[[1]])),
    nb_produits_uniques = length(unique(cereales[[3]])),
    nb_valeurs_manquantes = sum(is.na(cereales)),
    pct_valeurs_manquantes = round(sum(is.na(cereales)) / (nrow(cereales) * ncol(cereales)) * 100, 2)
  )

  # Diagnostic de la table de conversion
  conversion <- donnees$TableConversionP2
  diagnostics$conversion <- list(
    nb_lignes = nrow(conversion),
    nb_triplets_uniques = nrow(unique(conversion[, 1:3])),
    nb_poids_manquants = sum(is.na(conversion$poids) | conversion$poids == "" | conversion$poids == "0")
  )

  # Diagnostic des calories
  calories <- donnees$calorie_conversion_tgo2021
  diagnostics$calories <- list(
    nb_produits = nrow(calories),
    nb_calories_manquantes = sum(is.na(calories$cal) | calories$cal == 0)
  )

  return(diagnostics)
}
