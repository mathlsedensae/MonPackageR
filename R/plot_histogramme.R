#' Creer un histogramme pour une variable
#' @param data Dataframe
#' @param var Variable a representer
#' @param titre Titre du graphique (optionnel)
#' @return Objet ggplot
#' @export
plot_histogramme <- function(data, var, titre = NULL) {
  var <- rlang::ensym(var)

  ggplot2::ggplot(data, ggplot2::aes(x = !!var)) +
    ggplot2::geom_histogram(bins = 40, fill = "lightcoral", color = "black", alpha = 0.7) +
    ggplot2::labs(
      title = titre %||% paste("Histogramme de", rlang::as_label(var)),
      x = rlang::as_label(var),
      y = "Frequence"
    ) +
    ggplot2::theme_minimal()
}
