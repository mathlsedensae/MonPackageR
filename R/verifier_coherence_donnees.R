#' Verifier la coherence interne des donnees
#' @param cereales Dataframe des cereales
#' @return Liste des verifications avec les lignes problematiques
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
verifier_coherence_donnees <- function(cereales) {

  verifications <- list()

  # Declaration incomplete
  verifications[["declaration_incomplete"]] <- cereales %>%
    dplyr::filter(!is.na(produit) &
                    is.na(qte_consommee) &
                    is.na(qte_autoconsommation) &
                    is.na(qte_dons_troc))

  # Quantites incoherentes
  verifications[["quantites_incoherentes"]] <- cereales %>%
    dplyr::filter(
      (!is.na(qte_consommee) &
         (qte_autoconsommation + qte_dons_troc) > qte_consommee) |
        (!is.na(qte_achat) & qte_achat > 0 & (is.na(val_achat) | val_achat == 0)) |
        (!is.na(val_achat) & val_achat > 0 & (is.na(qte_achat) | qte_achat == 0))
    )

  # Achat incoherent
  verifications[["achat_incoherent"]] <- cereales %>%
    dplyr::filter((dernier_achat %in% 1:3 & (is.na(qte_achat) | qte_achat == 0)) |
                    (dernier_achat == 5 & !is.na(qte_achat) & qte_achat > 0))

  # Formats incoherents
  verifications[["formats_incoherents"]] <- cereales %>%
    dplyr::filter((unite_cons == 100 & !is.na(taille_cons) & taille_cons != 0) |
                    (is.na(unite_cons) & !is.na(taille_cons)) |
                    (!is.na(unite_cons) & is.na(taille_cons)))

  return(verifications)
}
