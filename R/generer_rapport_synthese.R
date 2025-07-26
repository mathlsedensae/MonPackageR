#' Generer un rapport de synthese des analyses
#' @param resultats Liste retournee par pipeline_analyse_ehcvm
#' @param chemin_rapport Chemin pour sauvegarder le rapport
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
generer_rapport_synthese <- function(resultats, chemin_rapport = NULL) {

  Base_menage <- resultats$Base_menage
  cereales <- resultats$cereales
  verifications <- resultats$verifications

  # Statistiques generales
  rapport <- list()

  rapport$statistiques_generales <- list(
    nb_menages = nrow(Base_menage),
    nb_observations_cereales = nrow(cereales),
    nb_produits_differents = length(unique(cereales$produit)),
    periode_analyse = "7 derniers jours"
  )

  # Repartitions
  rapport$repartitions <- list(
    par_region = table(Base_menage$region),
    par_milieu = table(Base_menage$milieu),
    par_niveau_consommation = table(Base_menage$niveau_consommation_cereales),
    par_taille_menage = table(Base_menage$taille_menage_cat)
  )

  # Indicateurs moyens
  rapport$indicateurs_moyens <- Base_menage %>%
    dplyr::summarise(
      depense_moyenne_par_tete = mean(Depense_de_consommation_par_tete, na.rm = TRUE),
      kcal_moyen_par_tete = mean(kcal_par_tete, na.rm = TRUE),
      taux_autoconsommation_moyen = mean(taux_autoconsommation, na.rm = TRUE),
      diversite_moyenne = mean(indice_div_cereales, na.rm = TRUE),
      rendement_moyen = mean(rendement_kcal_fcfa, na.rm = TRUE),
      taille_menage_moyenne = mean(nb_personnes, na.rm = TRUE)
    )

  # Comparaisons par milieu
  rapport$comparaisons_milieu <- Base_menage %>%
    dplyr::group_by(milieu) %>%
    dplyr::summarise(
      nb_menages = dplyr::n(),
      depense_moyenne = mean(Depense_de_consommation_par_tete, na.rm = TRUE),
      kcal_moyen = mean(kcal_par_tete, na.rm = TRUE),
      taux_auto_moyen = mean(taux_autoconsommation, na.rm = TRUE),
      .groups = "drop"
    )

  # Problemes detectes
  rapport$problemes_detectes <- list(
    nb_declarations_incompletes = nrow(verifications$declaration_incomplete),
    nb_quantites_incoherentes = nrow(verifications$quantites_incoherentes),
    nb_achats_incoherents = nrow(verifications$achat_incoherent),
    nb_formats_incoherents = nrow(verifications$formats_incoherents)
  )

  # Sauvegarde si chemin fourni
  if (!is.null(chemin_rapport)) {
    saveRDS(rapport, paste0(chemin_rapport, "/rapport_synthese.rds"))

    # Version lisible
    capture.output({
      cat("=== RAPPORT DE SYNTHESE ANALYSE EHCVM ===\n\n")
      cat("1. STATISTIQUES GENERALES\n")
      print(rapport$statistiques_generales)
      cat("\n2. INDICATEURS MOYENS\n")
      print(rapport$indicateurs_moyens)
      cat("\n3. COMPARAISONS PAR MILIEU\n")
      print(rapport$comparaisons_milieu)
      cat("\n4. PROBLEMES DETECTES\n")
      print(rapport$problemes_detectes)
    }, file = paste0(chemin_rapport, "/rapport_synthese.txt"))
  }

  return(rapport)
}
