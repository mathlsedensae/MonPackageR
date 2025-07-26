#' Afficher un resume des verifications de coherence
#' @param verifications Liste des verifications retournee par verifier_coherence_donnees
#' @export
afficher_resume_verifications <- function(verifications) {
  cat("RESUME DES INCOHERENCES :\n\n")
  for (nom in names(verifications)) {
    cat("- ", nom, ":", nrow(verifications[[nom]]), "lignes\n")
  }
}
