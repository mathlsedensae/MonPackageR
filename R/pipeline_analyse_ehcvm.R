#' Pipeline complet d'analyse des donnees EHCVM
#' @param chemin_donnees Chemin vers le dossier des donnees
#' @param chemin_sortie Chemin vers le dossier de sortie
#' @return Liste contenant toutes les bases finales
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
pipeline_analyse_ehcvm <- function(chemin_donnees, chemin_sortie) {

  cat("=== DEBUT DU PIPELINE D'ANALYSE EHCVM ===\n\n")

  # 1. Chargement des donnees
  cat("1. Chargement des donnees...\n")
  donnees <- charger_donnees_ehcvm(chemin_donnees)

  # 2. Preparation des donnees cereales
  cat("2. Preparation des donnees cereales...\n")
  cereales <- preparer_colonnes_cereales(donnees$cereales)
  cereales <- ajouter_labels_modalites(cereales)

  # 3. Verification de coherence
  cat("3. Verification de coherence...\n")
  verifications <- verifier_coherence_donnees(cereales)
  afficher_resume_verifications(verifications)

  # 4. Calcul des valeurs unitaires
  cat("4. Calcul des valeurs unitaires...\n")
  cereales <- calculer_valeurs_unitaires(cereales)
  baseVU <- creer_base_vu(cereales)

  # 5. Calcul des depenses de consommation
  cat("5. Calcul des depenses de consommation...\n")
  cereales <- calculer_depenses_consommation(cereales, baseVU)

  # 6. Conversion et standardisation
  cat("6. Conversion et standardisation...\n")
  cereales <- appliquer_conversion_poids(cereales, donnees$TableConversionP2)
  cereales <- calculer_frequence_achat(cereales)

  # 7. Detection et correction des outliers
  cat("7. Detection et correction des outliers...\n")
  outliers <- detecter_outliers_iqr(cereales, "qte_kg")
  cereales <- corriger_outliers_winsorisation(cereales, "qte_kg")

  # 8. Calcul des calories
  cat("8. Calcul des calories...\n")
  cereales <- calculer_calories(cereales, donnees$calorie_conversion_tgo2021)
  cereales <- calculer_calories_par_tete(cereales, donnees$S00_S01_membres)
  cereales <- categoriser_niveau_consommation(cereales)

  # 9. Enrichissement geographique
  cat("9. Enrichissement geographique...\n")
  cereales <- enrichir_donnees_geo(cereales, donnees$Ehcvm_all)

  # 10. Gestion des valeurs manquantes
  cat("10. Gestion des valeurs manquantes...\n")
  cereales <- traiter_valeurs_manquantes_sources(cereales)

  # Imputation si necessaire
  vars_a_imputer <- c("valeur_consommee", "kcal")
  vars_explicatives <- c("produit", "unite_cons", "taille_cons", "nb_personnes",
                         "qte_consommee", "frequence_achat",
                         "region", "departement", "milieu")
  cereales <- imputer_valeurs_manquantes(cereales, vars_a_imputer, vars_explicatives)

  # 11. Creation de la base menage
  cat("11. Creation de la base menage...\n")
  Base_menage <- creer_base_menage(cereales)
  Base_menage <- ajouter_labels_base_menage(Base_menage)
  Base_menage <- creer_typologie_taille_menage(Base_menage)
  Base_menage <- calculer_depenses_par_tete(Base_menage)
  Base_menage <- definir_source_dominante(Base_menage)

  # 12. Calcul des indicateurs
  cat("12. Calcul des indicateurs...\n")
  Base_menage <- calculer_taux_autoconsommation(Base_menage)
  indice_diversite <- calculer_indice_diversite_simpson(cereales)
  Base_menage <- Base_menage %>% dplyr::left_join(indice_diversite, by = "menage_id")
  Base_menage <- calculer_rendement_kcal_fcfa(Base_menage)

  # 13. Sauvegarde des resultats
  cat("13. Sauvegarde des resultats...\n")
  sauvegarder_csv_utf8(baseVU, paste0(chemin_sortie, "//baseVU.csv"))
  sauvegarder_csv_utf8(cereales, paste0(chemin_sortie, "//Base_X1_Apuree.csv"))
  sauvegarder_csv_utf8(Base_menage, paste0(chemin_sortie, "//Base_menage.csv"))

  cat("\n=== FIN DU PIPELINE D'ANALYSE EHCVM ===\n")

  # Retourner les resultats
  return(list(
    cereales = cereales,
    Base_menage = Base_menage,
    baseVU = baseVU,
    verifications = verifications
  ))
}
