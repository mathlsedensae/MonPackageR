#' Calculer l'indice de diversite cerealiere de Simpson
#' @param data Dataframe au niveau produit-menage
#' @return Dataframe avec indices de diversite par menage
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
calculer_indice_diversite_simpson <- function(data) {

  indice_simpson <- data %>%
    dplyr::filter(!is.na(kcal_menage), !is.na(kcal), kcal_menage > 0) %>%
    dplyr::group_by(menage_id, produit, kcal_menage) %>%
    dplyr::summarise(kcal_produit = sum(kcal, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(p_i = kcal_produit / kcal_menage) %>%
    dplyr::group_by(menage_id) %>%
    dplyr::summarise(indice_div_cereales = 1 - sum(p_i^2, na.rm = TRUE), .groups = "drop")

  return(indice_simpson)
}
