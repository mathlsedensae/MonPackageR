#' Definir la source dominante de consommation
#' @param Base_menage Dataframe de la base menage
#' @return Dataframe avec source dominante definie
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
definir_source_dominante <- function(Base_menage) {

  Base_menage %>%
    dplyr::mutate(source_dominante = dplyr::case_when(
      total_val_achat >= total_val_auto & total_val_achat >= total_val_don ~ "Achat",
      total_val_auto >= total_val_achat & total_val_auto >= total_val_don ~ "Autoconsommation",
      total_val_don >= total_val_achat & total_val_don >= total_val_auto ~ "Don",
      TRUE ~ NA_character_
    ))
}
