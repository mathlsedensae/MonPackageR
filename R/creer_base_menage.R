#' Creer la base menage a partir des donnees individuelles
#' @param data Dataframe avec donnees au niveau produit-menage
#' @return Dataframe agrege au niveau menage
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
creer_base_menage <- function(data) {

  Base_menage <- data %>%
    dplyr::group_by(menage_id) %>%
    dplyr::summarise(
      kcal_menage = dplyr::first(kcal_menage),
      nb_personnes = dplyr::first(nb_personnes),
      kcal_par_tete = dplyr::first(kcal_par_tete),
      niveau_consommation_cereales = dplyr::first(niveau_consommation_cereales),
      total_valeur_consommee = sum(valeur_consommee, na.rm = TRUE),
      total_val_achat = sum(val_achat_cons, na.rm = TRUE),
      total_val_auto = sum(val_auto_cons, na.rm = TRUE),
      total_val_don = sum(val_don_cons, na.rm = TRUE),
      nb_produits = dplyr::n_distinct(produit),
      region = dplyr::first(region),
      departement = dplyr::first(departement),
      milieu = dplyr::first(milieu),
      .groups = "drop"
    )

  return(Base_menage)
}
