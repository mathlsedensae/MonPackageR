#' Creer un graphique en barres pour une variable numerique par groupe
#' @param data Dataframe
#' @param var_num Variable numerique
#' @param var_groupe Variable de groupement
#' @param stat_fonction Fonction statistique (mean, median, sum)
#' @param titre Titre du graphique
#' @return Objet ggplot
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
plot_barres_stat_par_groupe <- function(data, var_num, var_groupe, stat_fonction = "mean", titre = NULL) {

  var_num <- rlang::ensym(var_num)
  var_groupe <- rlang::ensym(var_groupe)

  # Choisir la fonction statistique
  stat_func <- switch(stat_fonction,
                      "mean" = mean,
                      "median" = median,
                      "sum" = sum,
                      mean)

  donnees_stat <- data %>%
    dplyr::filter(!is.na(!!var_num), !is.na(!!var_groupe)) %>%
    dplyr::group_by(!!var_groupe) %>%
    dplyr::summarise(valeur_stat = stat_func(!!var_num, na.rm = TRUE), .groups = "drop")

  ggplot2::ggplot(donnees_stat, ggplot2::aes(x = !!var_groupe, y = valeur_stat)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
    ggplot2::labs(
      title = titre %||% paste(stat_fonction, "de", rlang::as_label(var_num), "par", rlang::as_label(var_groupe)),
      x = rlang::as_label(var_groupe),
      y = paste(stat_fonction, "de", rlang::as_label(var_num))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
