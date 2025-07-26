# utils-globalVariables.R
# Declaration des variables globales pour eviter les notes lors du check R CMD
# Utilisees principalement dans les pipelines dplyr ou dans des contextes non standards

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "produitID", "uniteID", "tailleID", "poids", "qte_consommee",
    "poids_kg", "codpr", "cal", "quantite_consommee_kg", "kcal_par_kg",
    "s01q12", "s01q13", "interview__key", "menage_id", "ifmember", "kcal",
    "nb_personnes", "VU", "vu_ref", "qte_autoconsommation", "qte_dons_troc",
    "valeur_consommee", "val_auto_cons", "val_don_cons", "produit", "taille",
    "q1", "q3", "iqr", "borne_sup", "borne_inf", "dernier_achat", "vu",
    "unite_achat", "taille_achat", "val_achat", "kcal_menage", "region",
    "departement", "milieu", "taille_cons", "unite_cons", "quantite",
    "niveau_consommation_cereales", "p_i", "label", "n", "pct", "percentage",
    "percentage_label", "valeur_stat", "taille_menage_cat", "total_val_auto",
    "total_valeur_consommee", "kcal_par_tete", "kcal_produit", "val_achat_cons", "qte_achat",
    "s00q01", "s00q02", "s00q04", "count", "median", "Depense_de_consommation_par_tete", "taux_autoconsommation",
    "indice_div_cereales", "rendement_kcal_fcfa",
    "total_val_achat", "total_val_don", "total_calcule", "diff_totaux", "test_ok", "sym", "ggsave", "indice_moyen", "NOMREG",
    "centroid", "geometry", "lat", "lon"
  ))
}
