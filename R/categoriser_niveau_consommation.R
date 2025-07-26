#' Categoriser le niveau de consommation en cereales
#' @param data Dataframe avec kcal_par_tete
#' @param seuil_min Seuil minimum (defaut: 1114)
#' @param cv_seuil Coefficient de variation (defaut: 0.29)
#' @return Dataframe avec la categorisation
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
categoriser_niveau_consommation <- function(data, seuil_min = 1114, cv_seuil = 0.29) {

  seuil_bas <- seuil_min * (1 - cv_seuil)
  seuil_haut <- seuil_min * (1 + cv_seuil)

  data %>%
    dplyr::mutate(
      niveau_consommation_cereales = dplyr::case_when(
        is.na(kcal_par_tete) ~ NA_character_,
        kcal_par_tete < seuil_bas ~ "Faible",
        kcal_par_tete > seuil_haut ~ "Elevee",
        TRUE ~ "Normale"
      ))
}
