#' Calculer le rendement kcal par FCFA depense
#' @param Base_menage Dataframe de la base menage
#' @return Dataframe avec rendement calcule
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
calculer_rendement_kcal_fcfa <- function(Base_menage) {

  Base_menage %>%
    dplyr::mutate(rendement_kcal_fcfa = kcal_menage / total_valeur_consommee)
}
