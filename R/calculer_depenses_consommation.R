#' Calculer les depenses de consommation
#' @param cereales Dataframe des cereales
#' @param baseVU Base des valeurs unitaires de reference
#' @return Dataframe avec les valeurs de consommation calculees
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
calculer_depenses_consommation <- function(cereales, baseVU) {

  # Renommer pour la jointure
  baseVU <- baseVU %>%
    dplyr::rename(vu_ref = VU)

  # Joindre les VU de reference
  cereales <- cereales %>%
    dplyr::left_join(baseVU, by = c("produit" = "Produit", "unite_cons" = "Unite", "taille_cons" = "Taille"))

  # Calculer les valeurs de consommation
  cereales <- cereales %>%
    dplyr::mutate(
      valeur_consommee = ifelse(!is.na(qte_consommee) & !is.na(vu_ref),
                                qte_consommee * vu_ref,
                                NA_real_),
      val_auto_cons = ifelse(!is.na(qte_autoconsommation) & !is.na(vu_ref),
                             qte_autoconsommation * vu_ref, NA_real_),
      val_don_cons = ifelse(!is.na(qte_dons_troc) & !is.na(vu_ref),
                            qte_dons_troc * vu_ref, NA_real_),
      val_achat_cons = ifelse(!is.na(valeur_consommee) & !is.na(val_auto_cons) & !is.na(val_don_cons) & !is.na(vu_ref),
                              valeur_consommee - val_auto_cons - val_don_cons, NA_real_)
    )

  return(cereales)
}
