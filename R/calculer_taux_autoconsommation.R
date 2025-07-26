#' Calculer le taux d'autoconsommation
#' @param Base_menage Dataframe de la base menage
#' @return Dataframe avec taux d'autoconsommation calcule
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
calculer_taux_autoconsommation <- function(Base_menage) {

  Base_menage %>%
    dplyr::mutate(taux_autoconsommation = total_val_auto / total_valeur_consommee)
}
