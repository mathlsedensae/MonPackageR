#' Sauvegarder un dataframe en CSV avec encodage UTF-8
#' @param data Dataframe a sauvegarder
#' @param chemin_fichier Chemin complet du fichier de sortie
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
sauvegarder_csv_utf8 <- function(data, chemin_fichier) {
  write.csv(data, file = chemin_fichier, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Fichier sauvegarde :", chemin_fichier, "\n")
}
