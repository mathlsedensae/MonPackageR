# tests/testthat/test_calculer_depenses_consommation.R

test_that("calculer_depenses_consommation retourne les bonnes colonnes et valeurs", {

  # Simuler un petit jeu de données
  cereales <- data.frame(
    produit = c("Riz", "Mil"),
    unite_cons = c("kg", "kg"),
    taille_cons = c("1kg", "1kg"),
    qte_consommee = c(10, 20),
    qte_autoconsommation = c(2, 5),
    qte_dons_troc = c(1, 2)
  )

  baseVU <- data.frame(
    Produit = c("Riz", "Mil"),
    Unite = c("kg", "kg"),
    Taille = c("1kg", "1kg"),
    VU = c(500, 300)  # prix unitaire en FCFA
  )

  # Appliquer la fonction
  result <- calculer_depenses_consommation(cereales, baseVU)

  # Vérifier les colonnes attendues
  expect_true(all(c("valeur_consommee", "val_auto_cons", "val_don_cons", "val_achat_cons") %in% colnames(result)))

  # Vérifier quelques résultats attendus
  expect_equal(result$valeur_consommee[1], 10 * 500)
  expect_equal(result$val_auto_cons[2], 5 * 300)
  expect_equal(result$val_achat_cons[1], (10 * 500) - (2 * 500) - (1 * 500))

  # Vérifier que les résultats sont numériques
  expect_type(result$val_achat_cons, "double")
})
