# ------------------------------------------------------------------------------
# 1. FONCTIONS DE PREPARATION DES DONNEES
# ------------------------------------------------------------------------------

#' Charger et preparer les donnees EHCVM
#' @param chemin_donnees Chemin vers le dossier contenant les fichiers de donnees
#' @return Liste contenant toutes les bases chargees
#' @export
charger_donnees_ehcvm <- function(chemin_donnees) {

  # Verification des packages necessaires
  required_packages <- c("haven", "tidyverse", "dplyr", "ggplot2", "readxl", "labelled", "scales", "sf")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Le package", pkg, "est requis mais n'est pas installe"))
    }
  }

  # Chargement des bases
  donnees <- list()
  donnees$cereales <- haven::read_dta(paste0(chemin_donnees, "//cereales.dta"))
  donnees$legumes <- haven::read_dta(paste0(chemin_donnees, "//legumes.dta"))
  donnees$S00_S01_membres <- haven::read_dta(paste0(chemin_donnees, "//S00_S01_membres.dta"))
  donnees$Ehcvm_all <- haven::read_dta(paste0(chemin_donnees, "//Ehcvm_all.dta"))
  donnees$calorie_conversion_tgo2021 <- haven::read_dta(paste0(chemin_donnees, "//calorie_conversion_tgo2021.dta"))
  donnees$TableConversionP2 <- readxl::read_xlsx(paste0(chemin_donnees, "//TableConversionP2.xlsx"))

  return(donnees)
}
