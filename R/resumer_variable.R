#' Resumer une variable numerique
#' @param data Dataframe
#' @param var Variable a resumer (nom sans guillemets)
#' @return Dataframe avec les statistiques descriptives
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
resumer_variable <- function(data, var) {
  var <- rlang::ensym(var)

  if (!requireNamespace("moments", quietly = TRUE)) {
    warning("Le package 'moments' n'est pas installe. Skewness et kurtosis ne seront pas calcules.")
    skew_val <- NA_real_
    kurt_val <- NA_real_
  } else {
    skew_val <- moments::skewness(dplyr::pull(data, !!var), na.rm = TRUE)
    kurt_val <- moments::kurtosis(dplyr::pull(data, !!var), na.rm = TRUE)
  }

  data %>%
    dplyr::summarise(
      n = sum(!is.na(!!var) | is.na(!!var)),
      n_NA = sum(is.na(!!var)),
      mean = mean(!!var, na.rm = TRUE),
      median = median(!!var, na.rm = TRUE),
      sd = sd(!!var, na.rm = TRUE),
      var = var(!!var, na.rm = TRUE),
      min = min(!!var, na.rm = TRUE),
      max = max(!!var, na.rm = TRUE),
      Q1 = quantile(!!var, 0.25, na.rm = TRUE),
      Q3 = quantile(!!var, 0.75, na.rm = TRUE),
      skewness = skew_val,
      kurtosis = kurt_val
    )
}
