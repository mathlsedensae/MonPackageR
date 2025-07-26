#' Ajouter les labels aux variables de la base menage
#' @param Base_menage Dataframe de la base menage
#' @return Dataframe avec labels ajoutes
#' @export
ajouter_labels_base_menage <- function(Base_menage) {

  labelled::var_label(Base_menage$menage_id) <- "Identifiant unique du menage"
  labelled::var_label(Base_menage$kcal_menage) <- "Apport calorique total du menage sur les 7 derniers jours kcal"
  labelled::var_label(Base_menage$nb_personnes) <- "Nombre de personnes dans le menage"
  labelled::var_label(Base_menage$kcal_par_tete) <- "Apport calorique moyen par personne par jour sur les 7 derniers jours"
  labelled::var_label(Base_menage$niveau_consommation_cereales) <- "Niveau de consommation du menage en cereales faible normal ou eleve"
  labelled::var_label(Base_menage$total_valeur_consommee) <- "Valeur totale de la consommation alimentaire FCFA sur les 7 derniers jours"
  labelled::var_label(Base_menage$total_val_achat) <- "Valeur totale des achats alimentaires FCFA sur les 7 derniers jours"
  labelled::var_label(Base_menage$total_val_auto) <- "Valeur totale de l autoconsommation FCFA sur les 7 derniers jours"
  labelled::var_label(Base_menage$total_val_don) <- "Valeur totale des dons alimentaires FCFA sur les 7 derniers jours"
  labelled::var_label(Base_menage$nb_produits) <- "Nombre de produits alimentaires differents consommes sur les 7 derniers jours"
  labelled::var_label(Base_menage$region) <- "Region de residence du menage"
  labelled::var_label(Base_menage$departement) <- "Departement de residence du menage"
  labelled::var_label(Base_menage$milieu) <- "Milieu de residence urbain rural"

  return(Base_menage)
}
