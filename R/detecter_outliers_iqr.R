#' Detecter les outliers par la methode IQR
#' @param data Dataframe
#' @param var_name Nom de la variable (caractere)
#' @return Indices des outliers
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
detecter_outliers_iqr <- function(data, var_name) {

  var <- data[[var_name]]
  var_sym <- rlang::sym(var_name)

  # Detection par groupe produit-taille
  data_out <- data %>%
    dplyr::group_by(produit, taille_cons) %>%
    dplyr::mutate(
      q1 = quantile(!!var_sym, 0.25, na.rm = TRUE),
      q3 = quantile(!!var_sym, 0.75, na.rm = TRUE),
      iqr = q3 - q1,
      borne_inf = q1 - 1.5 * iqr,
      borne_sup = q3 + 1.5 * iqr,
      is_outlier = (!!var_sym < borne_inf | !!var_sym > borne_sup)
    ) %>%
    dplyr::ungroup()

  # Statistiques
  cat("Statistiques de la variable", var_name, ":\n")
  print(summary(var))
  cat("Ecart-type :", sd(var, na.rm = TRUE), "\n")
  cat("Nombre de NA :", sum(is.na(var)), "\n\n")

  n_outliers <- sum(data_out$is_outlier, na.rm = TRUE)
  pct_outliers <- round(n_outliers / sum(!is.na(var)) * 100, 2)

  cat("Nombre d'outliers :", n_outliers, "(", pct_outliers, "%)\n\n")

  return(which(data_out$is_outlier))
}
