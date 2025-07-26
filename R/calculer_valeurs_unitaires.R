#' Calculer les valeurs unitaires
#' @param cereales Dataframe des cereales
#' @return Dataframe avec la variable vu ajoutee
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
calculer_valeurs_unitaires <- function(cereales) {
  cereales %>%
    dplyr::mutate(
      vu = ifelse(
        !is.na(dernier_achat) & dernier_achat %in% 1:3 &
          !is.na(qte_achat) & qte_achat > 0 &
          !is.na(val_achat) & val_achat > 0,
        val_achat / qte_achat,
        NA_real_
      )
    )
}
