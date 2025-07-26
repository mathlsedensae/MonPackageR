#' Fonction principale d'analyse EHCVM - Point d'entree unique
#'
#' Cette fonction execute l'analyse complete avec gestion d'erreurs
#' et generation automatique de rapports
#'
#' @param chemin_donnees Chemin vers les donnees
#' @param chemin_sortie Chemin de sortie
#' @param generer_rapport Generer un rapport de synthese (TRUE/FALSE)
#' @param generer_graphiques Generer les graphiques (TRUE/FALSE)
#' @param format_sortie Format de sortie ("csv", "excel", "both")
#' @importFrom utils capture.output
#' @return Liste complete des resultats
#' @export
analyser_ehcvm <- function(chemin_donnees, chemin_sortie,
                           generer_rapport = TRUE,
                           generer_graphiques = TRUE,
                           format_sortie = "csv") {

  # Verification preliminaire
  config_ok <- verifier_configuration()
  if (!isTRUE(config_ok)) {
    warning("Problemes de configuration detectes")
  }

  # Creation du dossier de sortie si necessaire
  if (!dir.exists(chemin_sortie)) {
    dir.create(chemin_sortie, recursive = TRUE)
  }

  # Execution du pipeline principal
  tryCatch({

    # Analyse principale
    resultats <- pipeline_analyse_ehcvm(chemin_donnees, chemin_sortie)

    # Export selon le format demande
    if (format_sortie %in% c("excel", "both")) {
      exporter_vers_excel(resultats, paste0(chemin_sortie, "/resultats_ehcvm.xlsx"))
    }

    # Generation du rapport
    if (generer_rapport) {
      rapport <- generer_rapport_synthese(resultats, chemin_sortie)
      resultats$rapport <- rapport
    }

    # Generation des graphiques
    if (generer_graphiques) {
      tableau_bord <- creer_tableau_bord(resultats$Base_menage)
      resultats$graphiques <- tableau_bord

      # Sauvegarder les graphiques
      for (i in seq_along(tableau_bord)) {
        nom_graph <- names(tableau_bord)[i]
        ggplot2::ggsave(
          filename = paste0(chemin_sortie, "/graphique_", nom_graph, ".png"),
          plot = tableau_bord[[i]],
          width = 10, height = 8, dpi = 300
        )
      }
    }

    # Tests de coherence finaux
    tests <- tester_coherence_resultats(resultats$Base_menage)
    resultats$tests_coherence <- tests

    cat("\n=== ANALYSE TERMINEE AVEC SUCCES ===\n")
    cat("Resultats disponibles dans:", chemin_sortie, "\n")

    return(resultats)

  }, error = function(e) {
    cat("ERREUR lors de l'analyse:", conditionMessage(e), "\n")
    stop(e)
  })
}
