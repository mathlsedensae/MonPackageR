#' Creer un graphique en barres groupees pour deux variables categorielles
#' @param data Dataframe
#' @param var_x Variable pour l'axe x
#' @param var_fill Variable pour le remplissage
#' @param titre Titre du graphique
#' @return Objet ggplot
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
plot_barres_groupees <- function(data, var_x, var_fill, titre = NULL) {

  var_x <- rlang::ensym(var_x)
  var_fill <- rlang::ensym(var_fill)

  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Le package 'scales' est requis pour cette fonction")
  }

  donnees_graph <- data %>%
    dplyr::filter(!is.na(!!var_x), !is.na(!!var_fill)) %>%
    dplyr::group_by(!!var_x, !!var_fill) %>%
    dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
    dplyr::group_by(!!var_x) %>%
    dplyr::mutate(
      percentage = (count / sum(count)) * 100,
      percentage_label = paste0(round(percentage, 1), "%")
    ) %>%
    dplyr::ungroup()

  ggplot2::ggplot(donnees_graph, ggplot2::aes(x = !!var_x, y = percentage, fill = !!var_fill)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = percentage_label),
                       position = ggplot2::position_dodge(width = 0.7),
                       vjust = -0.5,
                       size = 3) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::labs(
      title = titre %||% paste("Repartition de", rlang::as_label(var_fill), "selon", rlang::as_label(var_x)),
      x = rlang::as_label(var_x),
      y = "Pourcentage",
      fill = rlang::as_label(var_fill)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
