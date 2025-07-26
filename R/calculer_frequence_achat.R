#' Calculer la frequence d'achat
#' @param cereales Dataframe des cereales
#' @return Dataframe avec la frequence d'achat calculee
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
calculer_frequence_achat <- function(cereales) {
  cereales %>%
    dplyr::mutate(
      frequence_achat = dplyr::case_when(
        dernier_achat == 1 ~ 7.0,
        dernier_achat == 2 ~ 3.0,
        dernier_achat == 3 ~ 1.0,
        dernier_achat == 4 ~ 0.2,
        dernier_achat == 5 ~ 0.0,
        TRUE ~ NA_real_
      )
    )
}
