#' Imputer les valeurs manquantes par k-NN
#' @param data Dataframe avec valeurs manquantes
#' @param vars_a_imputer Variables a imputer
#' @param vars_explicatives Variables explicatives pour l'imputation
#' @param k Nombre de voisins (defaut: 5)
#' @return Dataframe avec valeurs imputees
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
imputer_valeurs_manquantes <- function(data, vars_a_imputer, vars_explicatives, k = 5) {

  if (!requireNamespace("VIM", quietly = TRUE)) {
    stop("Le package 'VIM' est requis pour l'imputation par k-NN")
  }

  # Preparer les donnees pour l'imputation
  df_impute <- data %>%
    dplyr::select(all_of(c(vars_a_imputer, vars_explicatives))) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("produit", "unite", "taille", "region", "departement", "milieu")), as.factor))

  # Effectuer l'imputation
  df_imputed <- VIM::kNN(
    df_impute,
    variable = vars_a_imputer,
    dist_var = vars_explicatives,
    k = k
  )

  # Remplacer les valeurs dans les donnees originales
  for (var in vars_a_imputer) {
    data[[var]] <- df_imputed[[var]]
  }

  return(data)
}
