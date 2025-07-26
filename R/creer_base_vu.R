#' Creer la base des valeurs unitaires de reference
#' @param cereales Dataframe des cereales avec VU calculees
#' @return Dataframe avec les VU de reference par produit-unite-taille
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
creer_base_vu <- function(cereales) {

  vu_valides <- cereales %>%
    dplyr::filter(!is.na(vu),
                  dernier_achat %in% 1:3,
                  !is.na(unite_achat), !is.na(taille_achat),
                  !is.na(produit))

  baseVU <- vu_valides %>%
    dplyr::group_by(produit, unite_achat, taille_achat) %>%
    dplyr::summarise(
      mediane_vu = median(vu, na.rm = TRUE),
      mode_vu = mode_hsm_safe(vu),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      vu_ref = dplyr::case_when(
        is.na(mode_vu) ~ mediane_vu,
        abs(mediane_vu - mode_vu) / mediane_vu < 0.10 ~ (mediane_vu + mode_vu) / 2,
        TRUE ~ mediane_vu
      )
    ) %>%
    dplyr::select(produit, unite_achat, taille_achat, vu_ref)

  # Renommer les colonnes
  colnames(baseVU) <- c("Produit", "Unite", "Taille", "VU")

  return(baseVU)
}
