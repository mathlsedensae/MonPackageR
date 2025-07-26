#' Appliquer la table de conversion pour obtenir les poids en kg
#' @param cereales Dataframe des cereales
#' @param TableConversionP2 Table de conversion
#' @return Dataframe avec les quantites en kg
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
appliquer_conversion_poids <- function(cereales, TableConversionP2) {

  # Preparer la table de conversion
  conversion <- TableConversionP2 %>%
    dplyr::rename(
      produit = produitID,
      unite_cons = uniteID,
      taille_cons = tailleID
    ) %>%
    dplyr::mutate(
      poids = gsub(",", ".", poids),
      poids = gsub(";", ".", poids),
      poids = gsub("[^0-9\\.]", "", poids),
      poids = as.numeric(poids),
      poids_kg = poids / 1000
    )

  # Joindre et calculer les quantites en kg
  cereales <- cereales %>%
    dplyr::left_join(conversion, by = c("produit", "unite_cons", "taille_cons")) %>%
    dplyr::mutate(qte_kg = ifelse(!is.na(qte_consommee) & !is.na(poids_kg),
                                  qte_consommee * poids_kg,
                                  NA_real_))

  return(cereales)
}
