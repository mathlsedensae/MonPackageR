#' Creer un tableau de bord interactif simple
#' @param Base_menage Dataframe de la base menage
#' @return Liste de graphiques pour tableau de bord
#' @export
creer_tableau_bord <- function(Base_menage) {

  tableau_bord <- list()

  # Graphique 1: Repartition par niveau de consommation
  tableau_bord$niveau_consommation <- plot_camembert(
    Base_menage, niveau_consommation_cereales,
    "Repartition par niveau de consommation de cereales"
  )

  # Graphique 2: Depenses par taille de menage
  tableau_bord$depenses_taille <- plot_barres_stat_par_groupe(
    Base_menage, Depense_de_consommation_par_tete, taille_menage_cat, "mean",
    "Depense moyenne par taille de menage"
  )

  # Graphique 3: Comparaison urbain/rural
  tableau_bord$urbain_rural <- plot_barres_groupees(
    Base_menage, milieu, niveau_consommation_cereales,
    "Niveau de consommation par milieu"
  )

  # Graphique 4: Distribution des calories
  tableau_bord$distribution_kcal <- plot_histogramme(
    Base_menage, kcal_par_tete,
    "Distribution des calories par tete par jour"
  )

  # Graphique 5: Taux d'autoconsommation par milieu
  tableau_bord$autoconsommation_milieu <- plot_barres_stat_par_groupe(
    Base_menage, taux_autoconsommation, milieu, "mean",
    "Taux d'autoconsommation moyen par milieu"
  )

  return(tableau_bord)
}
