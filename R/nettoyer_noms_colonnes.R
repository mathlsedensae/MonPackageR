#' Nettoyer les noms de colonnes pour eviter les caracteres speciaux
#' @param noms_colonnes Vecteur des noms de colonnes
#' @return Vecteur des noms nettoyes
#' @export
nettoyer_noms_colonnes <- function(noms_colonnes) {
  # Remplacer les caracteres speciaux par des underscores
  noms_nettoyes <- gsub("[^A-Za-z0-9_]", "_", noms_colonnes)
  # Eliminer les underscores multiples
  noms_nettoyes <- gsub("_{2,}", "_", noms_nettoyes)
  # Eliminer les underscores en debut et fin
  noms_nettoyes <- gsub("^_|_$", "", noms_nettoyes)

  return(noms_nettoyes)
}
