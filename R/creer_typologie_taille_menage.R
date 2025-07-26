#' Creer une typologie de la taille du menage
#' @param Base_menage Dataframe de la base menage
#' @return Dataframe avec typologie de taille ajoutee
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
creer_typologie_taille_menage <- function(Base_menage) {

  Base_menage <- Base_menage %>%
    dplyr::mutate(
      taille_menage_cat = dplyr::case_when(
        nb_personnes >= 1 & nb_personnes <= 4 ~ "1-4 personnes",
        nb_personnes >= 5 & nb_personnes <= 9 ~ "5-9 personnes",
        nb_personnes >= 10 & nb_personnes <= 14 ~ "10-14 personnes",
        nb_personnes >= 15 & nb_personnes <= 19 ~ "15-19 personnes",
        nb_personnes >= 20 ~ "20 personnes ou plus",
        TRUE ~ NA_character_
      ),
      taille_menage_cat = factor(taille_menage_cat,
                                 levels = c("1-4 personnes", "5-9 personnes", "10-14 personnes",
                                            "15-19 personnes", "20 personnes ou plus"),
                                 ordered = TRUE)
    )

  labelled::var_label(Base_menage$taille_menage_cat) <- "Typologie de la taille du menage"

  return(Base_menage)
}
