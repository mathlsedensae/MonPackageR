#' Valider la structure des donnees d'entree
#' @param donnees Liste des donnees chargees
#' @return TRUE si la structure est valide, erreur sinon
#' @export
valider_structure_donnees <- function(donnees) {

  # Verifier la presence des bases necessaires
  bases_requises <- c("cereales", "S00_S01_membres", "Ehcvm_all",
                      "calorie_conversion_tgo2021", "TableConversionP2")

  for (base in bases_requises) {
    if (!base %in% names(donnees)) {
      stop(paste("Base manquante :", base))
    }
    if (nrow(donnees[[base]]) == 0) {
      stop(paste("Base vide :", base))
    }
  }

  # Verifier les colonnes critiques dans cereales
  colonnes_critiques_cereales <- c("interview__key", "interview__id",
                                   "s07q01", "s07q03", "s07q05")

  for (col in colonnes_critiques_cereales) {
    if (!col %in% names(donnees$cereales)) {
      warning(paste("Colonne manquante dans cereales :", col))
    }
  }

  cat("Structure des donnees validee avec succes\n")
  return(TRUE)
}
