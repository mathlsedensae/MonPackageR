#' Exporter les resultats vers Excel avec plusieurs onglets
#' @param resultats Liste des resultats
#' @param chemin_excel Chemin du fichier Excel de sortie
#' @export
exporter_vers_excel <- function(resultats, chemin_excel) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Le package 'openxlsx' est requis pour l'export Excel")
  }

  # Creer un classeur
  wb <- openxlsx::createWorkbook()

  # Ajouter les onglets
  openxlsx::addWorksheet(wb, "Base_menage")
  openxlsx::addWorksheet(wb, "Base_cereales")
  openxlsx::addWorksheet(wb, "Base_VU")
  openxlsx::addWorksheet(wb, "Synthese")

  # Ecrire les donnees
  openxlsx::writeData(wb, "Base_menage", resultats$Base_menage)
  openxlsx::writeData(wb, "Base_cereales", resultats$cereales)
  openxlsx::writeData(wb, "Base_VU", resultats$baseVU)

  # Creer un onglet de synthese
  synthese <- data.frame(
    Indicateur = c("Nombre de menages", "Nombre d'observations cereales",
                   "Depense moyenne par tete", "Kcal moyen par tete",
                   "Taux autoconsommation moyen"),
    Valeur = c(nrow(resultats$Base_menage), nrow(resultats$cereales),
               round(mean(resultats$Base_menage$Depense_de_consommation_par_tete, na.rm = TRUE), 2),
               round(mean(resultats$Base_menage$kcal_par_tete, na.rm = TRUE), 2),
               round(mean(resultats$Base_menage$taux_autoconsommation, na.rm = TRUE), 3))
  )

  openxlsx::writeData(wb, "Synthese", synthese)

  # Sauvegarder
  openxlsx::saveWorkbook(wb, chemin_excel, overwrite = TRUE)
  cat("Fichier Excel sauvegarde :", chemin_excel, "\n")
}
