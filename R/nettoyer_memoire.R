#' Nettoyer la memoire et optimiser les performances
#' @export
nettoyer_memoire <- function() {
  # Forcer le garbage collector
  gc()

  # Afficher l'utilisation de la memoire
  cat("Utilisation memoire apres nettoyage:\n")
  print(gc())

  invisible()
}
