#' Exemple d'utilisation complete du package
#'
#' Cette fonction montre comment utiliser l'ensemble des fonctions
#' pour une analyse complete des donnees EHCVM
#'
#' @param chemin_donnees Chemin vers le dossier contenant les fichiers de donnees
#' @param chemin_sortie Chemin vers le dossier de sortie des resultats
#' @examples
#' \dontrun{
#' # Utilisation du pipeline complet
#' resultats <- pipeline_analyse_ehcvm(
#'   chemin_donnees = "C:/Users/HP/Desktop/data",
#'   chemin_sortie = "C:/Users/HP/Desktop/outputs"
#' )
#'
#' # Ou utilisation etape par etape
#' donnees <- charger_donnees_ehcvm("C:/Users/HP/Desktop/data")
#' cereales <- preparer_colonnes_cereales(donnees$cereales)
#' cereales <- ajouter_labels_modalites(cereales)
#' # ... suite des etapes
#' }
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
exemple_utilisation_complete <- function(chemin_donnees, chemin_sortie) {

  # Exemple d'utilisation du pipeline complet
  resultats <- pipeline_analyse_ehcvm(chemin_donnees, chemin_sortie)

  # Exemples d'analyses complementaires
  Base_menage <- resultats$Base_menage

  # Analyse par region
  analyse_region <- Base_menage %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      nb_menages = dplyr::n(),
      depense_moyenne = mean(Depense_de_consommation_par_tete, na.rm = TRUE),
      kcal_moyen = mean(kcal_par_tete, na.rm = TRUE),
      taux_auto_moyen = mean(taux_autoconsommation, na.rm = TRUE),
      .groups = "drop"
    )

  # Analyse par milieu
  analyse_milieu <- Base_menage %>%
    dplyr::group_by(milieu) %>%
    dplyr::summarise(
      nb_menages = dplyr::n(),
      depense_moyenne = mean(Depense_de_consommation_par_tete, na.rm = TRUE),
      kcal_moyen = mean(kcal_par_tete, na.rm = TRUE),
      taux_auto_moyen = mean(taux_autoconsommation, na.rm = TRUE),
      .groups = "drop"
    )

  # Graphiques d'exemple
  g1 <- plot_camembert(Base_menage, niveau_consommation_cereales,
                       "Repartition par niveau de consommation")

  g2 <- plot_barres_groupees(Base_menage, milieu, niveau_consommation_cereales,
                             "Niveau de consommation par milieu")

  g3 <- plot_barres_stat_par_groupe(Base_menage, Depense_de_consommation_par_tete,
                                    taille_menage_cat, "mean",
                                    "Depense moyenne par taille de menage")

  return(list(
    resultats_principaux = resultats,
    analyse_region = analyse_region,
    analyse_milieu = analyse_milieu,
    graphiques = list(g1 = g1, g2 = g2, g3 = g3)
  ))
}
