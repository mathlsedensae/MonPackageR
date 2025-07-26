#' Traiter les valeurs manquantes pour les variables de source
#' @param data Dataframe
#' @return Dataframe avec valeurs manquantes traitees
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
traiter_valeurs_manquantes_sources <- function(data) {
  data %>%
    dplyr::mutate(
      val_auto_cons = ifelse(is.na(val_auto_cons), 0, val_auto_cons),
      val_don_cons = ifelse(is.na(val_don_cons), 0, val_don_cons),
      val_achat_cons = ifelse(!is.na(valeur_consommee) & is.na(val_achat_cons) &
                                !is.na(val_auto_cons) & !is.na(val_don_cons),
                              valeur_consommee - val_auto_cons - val_don_cons,
                              val_achat_cons)
    )
}
