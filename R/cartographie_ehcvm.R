#' Fonction complete de cartographie pour les donnees EHCVM du Senegal
#'
#' Cette fonction integre toutes les etapes de cartographie :
#' 1. Chargement des donnees geographiques
#' 2. Preparation des donnees statistiques
#' 3. Creation des cartes choroplethes
#' 4. Analyse spatiale
#' 5. Export des resultats
#'
#' @param chemin_shapefile Chemin vers le fichier shapefile (.shp)
#' @param donnees_menage Dataframe Base_menage
#' @param chemin_sortie Dossier de sortie pour les resultats
#' @param niveau_admin Niveau administratif ("region" ou "departement")
#' @param var_depense Nom de la variable des depenses
#' @param var_calories Nom de la variable calorique
#' @param var_autoconsommation Nom de la variable d'autoconsommation
#' @param var_diversite Nom de la variable de diversite
#' @return Liste contenant tous les resultats et cartes
#' @importFrom dplyr group_by summarise mutate recode
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_viridis_c theme_void labs theme element_text
#' @importFrom sf st_read st_transform
#' @importFrom utils install.packages
#' @importFrom dplyr rename left_join
#' @export
cartographie_ehcvm <- function(chemin_shapefile,
                               donnees_menage,
                               chemin_sortie,
                               niveau_admin = "region",
                               var_depense = "Depense_de_consommation_par_tete",
                               var_calories = "kcal_par_tete",
                               var_autoconsommation = "taux_autoconsommation",
                               var_diversite = "indice_div_cereales") {

  # Chargement des packages necessaires
  if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")

  library(sf)
  library(ggplot2)
  library(dplyr)
  library(scales)

  # 1. Chargement du shapefile
  charger_shapefile <- function(chemin, niveau) {
    if (!file.exists(chemin)) stop("Fichier shapefile introuvable")

    shp <- st_read(chemin, quiet = TRUE)
    shp <- st_transform(shp, crs = 32628)

    noms_col <- if (niveau == "region") {
      c("NOM_REG", "REGION", "NAME_1", "ADM1_FR", "nom_region")
    } else {
      c("NOM_DEPT", "DEPARTEMENT", "NAME_2", "ADM2_FR", "nom_dept")
    }

    col_nom <- intersect(toupper(noms_col), toupper(names(shp)))

    if (length(col_nom) > 0) {
      shp <- shp %>% rename(nom_admin = !!col_nom[1])
    } else {
      warning("Colonne des noms non detectee")
    }

    return(shp)
  }

  senegal_sf <- charger_shapefile(chemin_shapefile, niveau_admin)

  # 2. Preparation des donnees
  preparer_donnees <- function(data, niveau) {
    res <- data %>%
      group_by(region) %>%
      summarise(
        nb_menages = n(),
        depense_moyenne = mean(!!sym(var_depense), na.rm = TRUE),
        kcal_moyen = mean(!!sym(var_calories), na.rm = TRUE),
        taux_auto_moyen = mean(!!sym(var_autoconsommation), na.rm = TRUE),
        diversite_moyenne = mean(!!sym(var_diversite), na.rm = TRUE),
        .groups = "drop"
      )

    noms_regions <- c(
      "1" = "DAKAR", "2" = "ZIGUINCHOR", "3" = "DIOURBEL",
      "4" = "SAINT-LOUIS", "5" = "TAMBACOUNDA", "6" = "KAOLACK",
      "7" = "THIES", "8" = "LOUGA", "9" = "FATICK", "10" = "KOLDA",
      "11" = "MATAM", "12" = "KAFFRINE", "13" = "KEDOUGOU",
      "14" = "SEDHIOU"
    )

    res <- res %>%
      mutate(nom_region = recode(as.character(region), !!!noms_regions))

    return(res)
  }

  donnees_region <- preparer_donnees(donnees_menage, niveau_admin)

  # 3. Creation des cartes
  creer_carte <- function(shp, data, var, titre, palette) {
    ggplot() +
      geom_sf(data = shp, aes(fill = !!sym(var)), color = "white", size = 0.3) +
      scale_fill_viridis_c(option = palette, na.value = "grey90") +
      theme_void() +
      labs(title = titre, fill = "") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  }

  cartes <- list(
    depenses = list(
      var = "depense_moyenne",
      titre = "Depenses moyennes par tete (FCFA/7j)",
      palette = "viridis"
    ),
    calories = list(
      var = "kcal_moyen",
      titre = "Apport calorique moyen (kcal/jour)",
      palette = "plasma"
    ),
    autoconsommation = list(
      var = "taux_auto_moyen",
      titre = "Taux moyen d'autoconsommation",
      palette = "Blues"
    ),
    diversite = list(
      var = "diversite_moyenne",
      titre = "Indice de diversite cerealiere",
      palette = "Greens"
    )
  )

  resultats_cartes <- lapply(cartes, function(carte) {
    shp_join <- senegal_sf %>%
      left_join(donnees_region, by = c("nom_admin" = "nom_region"))

    creer_carte(
      shp = shp_join,
      data = donnees_region,
      var = carte$var,
      titre = carte$titre,
      palette = carte$palette
    )
  })

  # 4. Export des resultats
  if (!dir.exists(chemin_sortie)) dir.create(chemin_sortie, recursive = TRUE)

  lapply(names(resultats_cartes), function(nom) {
    ggsave(
      paste0(chemin_sortie, "/carte_", nom, ".png"),
      resultats_cartes[[nom]],
      width = 10, height = 8, dpi = 300
    )
  })

  write.csv(donnees_region, paste0(chemin_sortie, "/donnees_par_region.csv"), row.names = FALSE)

  # 5. Retour
  return(list(
    shapefile = senegal_sf,
    donnees = donnees_region,
    cartes = resultats_cartes,
    parametres = list(
      niveau_administratif = niveau_admin,
      variables_utilisees = c(
        depenses = var_depense,
        calories = var_calories,
        autoconsommation = var_autoconsommation,
        diversite = var_diversite
      )
    )
  ))
}
