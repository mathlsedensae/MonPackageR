#' Creer un boxplot pour une variable
#' @param data Dataframe
#' @param var Variable a representer
#' @param titre Titre du graphique (optionnel)
#' @return Objet ggplot
#' @export
plot_boxplot_variable <- function(data, var, titre = NULL) {
  var <- rlang::ensym(var)

  ggplot2::ggplot(data, ggplot2::aes(y = "", x = !!var)) +
    ggplot2::geom_boxplot(fill = "lightblue", outlier.color = "red") +
    ggplot2::labs(
      title = titre %||% paste("Boxplot de", rlang::as_label(var)),
      y = rlang::as_label(var),
      x = ""
    ) +
    ggplot2::theme_minimal()
}
