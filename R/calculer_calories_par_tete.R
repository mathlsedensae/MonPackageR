#' Calculer les calories par tete
#' @param data Dataframe avec les calories individuelles
#' @param membres_menage Table des membres du menage
#' @return Dataframe avec les calories par tete calculees
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
calculer_calories_par_tete <- function(data, membres_menage) {

  # Calculer la taille des menages
  membres_menage <- membres_menage %>%
    dplyr::mutate(ifmember = ifelse(s01q12 == 1 | s01q13 == 1, 1, 0)) %>%
    dplyr::rename(menage_id = interview__key)

  taille_menage <- membres_menage %>%
    dplyr::group_by(menage_id) %>%
    dplyr::summarise(nb_personnes = sum(ifmember, na.rm = TRUE), .groups = "drop")

  # Calculer les calories totales par menage
  kcal_menage <- data %>%
    dplyr::group_by(menage_id) %>%
    dplyr::summarise(
      kcal_menage = if (any(is.na(kcal))) NA_real_ else sum(kcal),
      .groups = "drop"
    )

  # Joindre toutes les informations
  data <- data %>%
    dplyr::left_join(taille_menage, by = "menage_id") %>%
    dplyr::left_join(kcal_menage, by = "menage_id") %>%
    dplyr::mutate(kcal_par_tete = kcal_menage / (7 * nb_personnes))

  return(data)
}
