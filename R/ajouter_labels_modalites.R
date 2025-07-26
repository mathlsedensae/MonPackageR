#' Ajouter les labels de modalites aux variables categoriques
#' @param cereales Dataframe des cereales
#' @return Dataframe avec modalites labellisees
#' @export
ajouter_labels_modalites <- function(cereales) {

  # Labels pour les produits
  cereales$produit <- labelled::set_value_labels(cereales$produit, values = c(
    "Riz local brise" = 1, "Riz local entier" = 2, "Riz importe brise" = 3, "Riz importe entier" = 4,
    "Mais en epi" = 5, "Mais en grain" = 6, "Mil" = 7, "Sorgho" = 8, "Ble" = 9, "Fonio" = 10,
    "Autres cereales" = 11, "Farine de mais" = 12, "semoule de mais" = 13, "Farine de mil" = 14,
    "semoule de mil" = 15, "Farine de ble local ou importe" = 16, "semoule de ble" = 17,
    "Autres farines de cereales" = 18, "Autres semoules de cereales" = 19, "Pates alimentaires" = 20,
    "Pain moderne" = 21, "Pain traditionnel" = 22, "Croissants" = 23, "Biscuits" = 24, "Gateaux" = 25,
    "Beignets galettes" = 26, "Cereales de petit dejeuner" = 169
  ))

  # Labels pour les unites de consommation
  cereales$unite_cons <- labelled::set_value_labels(cereales$unite_cons, values = c(
    "Kg" = 100, "Boite de tomate" = 108, "Bol" = 109, "Calebasse" = 115, "Cup gobelet" = 123,
    "Louche traditionnelle" = 125, "Morceau" = 126, "Paquet" = 129, "Sac 100 Kg" = 135, "Sac 25 Kg" = 136,
    "Sac 5 Kg" = 137, "Sac 50 Kg" = 138, "Sachet" = 139, "Tas" = 143, "Tine" = 145, "Unite" = 147,
    "Yorouba" = 149, "Quart-Yorouba" = 251, "Demi-Yorouba" = 252, "Cornet" = 562,
    "Cuillere a soupe" = 564, "miche" = 568, "Demi-miche" = 569, "Tiers de miche" = 570, "Quart de miche" = 571
  ))

  # Labels pour les tailles
  cereales$taille_cons <- labelled::set_value_labels(cereales$taille_cons, values = c(
    "Taille unique" = 0, "Petit" = 1, "Moyen" = 2, "Grand" = 3, "Quart" = 4, "Demi" = 5, "Entier" = 6, "Tres Petite" = 7
  ))

  # Labels pour le dernier achat
  cereales$dernier_achat <- labelled::set_value_labels(cereales$dernier_achat, values = c(
    "Hier" = 1, "7 dernier jours" = 2, "30 derniers jours" = 3, "Plus de 30 jours" = 4, "Jamais" = 5
  ))

  # Labels pour les unites d'achat (memes que consommation)
  cereales$unite_achat <- labelled::set_value_labels(cereales$unite_achat, values = c(
    "Kg" = 100, "Boite de tomate" = 108, "Bol" = 109, "Calebasse" = 115, "Cup gobelet" = 123, "Louche traditionnelle" = 125,
    "Morceau" = 126, "Paquet" = 129, "Sac 100 Kg" = 135, "Sac 25 Kg" = 136, "Sac 5 Kg" = 137,
    "Sac 50 Kg" = 138, "Sachet" = 139, "Tas" = 143, "Tine" = 145, "Unite" = 147, "Yorouba" = 149,
    "Quart-Yorouba" = 251, "Demi-Yorouba" = 252, "Cornet" = 562, "Cuillere a soupe" = 564, "miche" = 568,
    "Demi-miche" = 569, "Tiers de miche" = 570, "Quart de miche" = 571
  ))

  # Labels pour les tailles d'achat
  cereales$taille_achat <- labelled::set_value_labels(cereales$taille_achat, values = c(
    "Taille unique" = 0, "Petit" = 1, "Moyen" = 2, "Grand" = 3, "Quart" = 4, "Demi" = 5, "Entier" = 6
  ))

  return(cereales)
}
