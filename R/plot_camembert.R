#' Creer un graphique en camembert pour une variable categorique
#' @param data Dataframe
#' @param var Variable categorique (nom sans guillemets)
#' @param titre Titre du graphique
#' @return Objet ggplot
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
plot_camembert <- function(data, var, titre = NULL) {

  var <- rlang::ensym(var)

  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Le package 'scales' est requis pour cette fonction")
  }

  stats <- data %>%
    dplyr::count(!!var) %>%
    dplyr::mutate(
      pct = n / sum(n),
      label = scales::percent(pct)
    ) %>%
    dplyr::filter(!is.na(!!var))

  ggplot2::ggplot(stats, ggplot2::aes(x = "", y = n, fill = !!var)) +
    ggplot2::geom_col(width = 1, color = "white") +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::geom_text(ggplot2::aes(label = label),
                       position = ggplot2::position_stack(vjust = 0.5),
                       size = 3.5) +
    ggplot2::labs(
      title = titre %||% paste("Repartition de", rlang::as_label(var)),
      fill = rlang::as_label(var)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}
