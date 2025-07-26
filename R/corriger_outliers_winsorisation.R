#' Corriger les outliers par winsorisation
#' @param data Dataframe
#' @param var_name Nom de la variable a corriger
#' @return Dataframe avec les outliers corriges
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
corriger_outliers_winsorisation <- function(data, var_name) {

  var_sym <- rlang::sym(var_name)

  # Calcul des bornes par groupe
  bornes_iqr <- data %>%
    dplyr::group_by(produit, taille_cons) %>%
    dplyr::summarise(
      q1 = quantile(!!var_sym, 0.25, na.rm = TRUE),
      q3 = quantile(!!var_sym, 0.75, na.rm = TRUE),
      iqr = q3 - q1,
      borne_sup = q3 + 1.5 * iqr,
      .groups = "drop"
    )

  # Application de la correction
  data_corrige <- data %>%
    dplyr::left_join(bornes_iqr, by = c("produit", "taille_cons")) %>%
    dplyr::mutate(
      !!var_sym := ifelse(!!var_sym > borne_sup, borne_sup, !!var_sym)
    ) %>%
    dplyr::select(-q1, -q3, -iqr, -borne_sup)

  return(data_corrige    )
}
