#' Carte de l'indice moyen de diversite des cereales par region
#'
#' Cette fonction calcule l'indice moyen de diversite des cereales par region
#' a partir de la base menage, puis le projette sur une carte a partir d'un fichier shapefile des regions.
#'
#' @param base_menage Un data.frame contenant au moins les colonnes `region` et `indice_div_cereales`.
#' @param regions_sf Objet sf contenant les polygones des regions (shapefile deja charge).
#'
#' @return Une carte ggplot2 affichant l'indice moyen par region.
#' @importFrom sf st_read st_centroid st_coordinates
#' @importFrom ggplot2 ggplot geom_sf geom_text scale_fill_viridis_c theme_minimal labs theme element_blank element_text
#' @export
cartographier_diversite_cereales <- function(base_menage, regions_sf) {
  
  # Verifier que regions_sf est un objet sf
  if (!inherits(regions_sf, "sf")) {
    stop("L'objet 'regions_sf' doit etre un objet sf.")
  }
  
  # Calcul de l'indice moyen par region
  indice_par_region <- base_menage %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      indice_moyen = mean(indice_div_cereales, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      region = as.character(region),
      region_nom = dplyr::case_when(
        region == "SAINT-LOUIS" ~ "SAINT LOUIS",
        TRUE ~ region
      )
    )
  
  # Jointure avec le shapefile
  regions_sf_joined <- dplyr::left_join(
    regions_sf,
    indice_par_region,
    by = c("NOMREG" = "region_nom")
  )
  
  # Calcul des centroïdes pour l'affichage des labels
  regions_sf_joined <- regions_sf_joined %>%
    dplyr::mutate(centroid = sf::st_centroid(geometry)) %>%
    dplyr::mutate(
      lon = sf::st_coordinates(centroid)[,1],
      lat = sf::st_coordinates(centroid)[,2]
    )
  
  # Construction de la carte
  carte <- ggplot2::ggplot(regions_sf_joined) +
    ggplot2::geom_sf(ggplot2::aes(fill = indice_moyen), color = "black", size = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(x = lon, y = lat, label = paste0(NOMREG, "\n", round(indice_moyen, 2))),
      size = 2.5, fontface = "bold"
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "plasma", direction = -1,
      name = "Indice cereales", na.value = "grey80"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Indice moyen de diversite des cereales par region"
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
      legend.title = ggplot2::element_text(face = "bold")
    )
  
  return(carte)
}
