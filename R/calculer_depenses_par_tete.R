#' Calculer les depenses de consommation par tete
#' @param Base_menage Dataframe de la base menage
#' @return Dataframe avec depenses par tete calculees
#' @export
calculer_depenses_par_tete <- function(Base_menage) {

  Base_menage$Depense_de_consommation_par_tete <- Base_menage$total_valeur_consommee / Base_menage$nb_personnes
  labelled::var_label(Base_menage$Depense_de_consommation_par_tete) <- "Depenses de consommation par tete du menage en cereale sur les 7 dernier jours"

  return(Base_menage)
}
