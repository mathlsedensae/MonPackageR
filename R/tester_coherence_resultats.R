#' Executer des tests de coherence sur les resultats
#' @param Base_menage Dataframe de la base menage
#' @return Liste des resultats de tests
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
tester_coherence_resultats <- function(Base_menage) {

  tests <- list()

  # Test 1: Coherence des totaux
  tests$coherence_totaux <- Base_menage %>%
    dplyr::mutate(
      total_calcule = total_val_achat + total_val_auto + total_val_don,
      diff_totaux = abs(total_valeur_consommee - total_calcule),
      test_ok = diff_totaux < 0.01 * total_valeur_consommee | is.na(diff_totaux)
    ) %>%
    dplyr::summarise(
      nb_incoherents = sum(!test_ok, na.rm = TRUE),
      pct_incoherents = mean(!test_ok, na.rm = TRUE) * 100
    )

  # Test 2: Valeurs aberrantes dans les indicateurs
  tests$valeurs_aberrantes <- list(
    taux_auto_negatif = sum(Base_menage$taux_autoconsommation < 0, na.rm = TRUE),
    taux_auto_superieur_1 = sum(Base_menage$taux_autoconsommation > 1, na.rm = TRUE),
    kcal_negatif = sum(Base_menage$kcal_par_tete < 0, na.rm = TRUE),
    depense_negative = sum(Base_menage$Depense_de_consommation_par_tete < 0, na.rm = TRUE)
  )

  # Test 3: Distribution des variables cles
  tests$distributions <- list(
    nb_menages_total = nrow(Base_menage),
    nb_menages_avec_calories = sum(!is.na(Base_menage$kcal_par_tete)),
    nb_menages_avec_depenses = sum(!is.na(Base_menage$Depense_de_consommation_par_tete)),
    mediane_kcal = median(Base_menage$kcal_par_tete, na.rm = TRUE),
    mediane_depenses = median(Base_menage$Depense_de_consommation_par_tete, na.rm = TRUE)
  )

  return(tests)
}
