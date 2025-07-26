#' Calculer les calories consommees
#' @param data Dataframe
#' @param conversion_cal Table de conversion caloriques
#' @return Dataframe avec les calories calculees
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export
calculer_calories <- function(data, conversion_cal) {

  # Preparer la table de conversion
  conversion_cal <- conversion_cal %>%
    dplyr::transmute(
      produit = as.numeric(codpr),
      kcal_par_kg = cal * 10
    )

  # Joindre et calculer les calories
  data <- data %>%
    dplyr::mutate(produit = as.numeric(produit)) %>%
    dplyr::left_join(conversion_cal, by = "produit") %>%
    dplyr::mutate(kcal = qte_consommee * kcal_par_kg)

  return(data)
}
