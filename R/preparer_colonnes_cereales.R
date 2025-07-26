#' Renommer et labelliser les colonnes de la base cereales
#' @param cereales Dataframe des cereales
#' @return Dataframe avec colonnes renommees et labellisees
#' @export
preparer_colonnes_cereales <- function(cereales) {

  # Renommage des colonnes
  colnames(cereales) <- c(
    "menage_id", "interview_id", "produit", "autre_produit",
    "qte_consommee", "unite_cons", "taille_cons", "qte_autoconsommation",
    "qte_dons_troc", "dernier_achat", "qte_achat",
    "unite_achat", "taille_achat", "val_achat"
  )

  # Labellisation des variables
  labelled::var_label(cereales$menage_id) <- "Identifiant du menage"
  labelled::var_label(cereales$produit) <- "Nom du produit cerealier"
  labelled::var_label(cereales$autre_produit) <- "Nom de la cereale si Autres est selectionne"
  labelled::var_label(cereales$qte_consommee) <- "Quantite totale consommee durant les 7 derniers jours"
  labelled::var_label(cereales$unite_cons) <- "Unite dans laquelle la consommation a ete declaree"
  labelled::var_label(cereales$taille_cons) <- "Taille de l unite de consommation declaree"
  labelled::var_label(cereales$qte_autoconsommation) <- "Quantite issue de la production propre"
  labelled::var_label(cereales$qte_dons_troc) <- "Quantite obtenue via dons commerce troc etc"
  labelled::var_label(cereales$dernier_achat) <- "Periode du dernier achat de cette cereale"
  labelled::var_label(cereales$qte_achat) <- "Quantite achetee la derniere fois"
  labelled::var_label(cereales$unite_achat) <- "Unite utilisee pour l achat"
  labelled::var_label(cereales$taille_achat) <- "Taille de l unite d achat"
  labelled::var_label(cereales$val_achat) <- "Valeur monetaire de l achat FCFA"

  return(cereales)
}
