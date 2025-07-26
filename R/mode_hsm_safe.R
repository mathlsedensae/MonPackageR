#' Calculer le mode en utilisant la methode HSM
#' @param x Vecteur numerique
#' @return Valeur du mode ou NA si impossible a calculer
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
mode_hsm_safe <- function(x) {
  x <- na.omit(x)
  if (length(x) < 3) {
    message("Trop peu de valeurs pour calculer le mode.")
    return(NA_real_)
  }

  if (!requireNamespace("modeest", quietly = TRUE)) {
    warning("Le package 'modeest' n'est pas disponible. Retour de NA.")
    return(NA_real_)
  }

  tryCatch({
    result <- suppressWarnings(modeest::mlv(x, method = "hsm", na.rm = TRUE))
    if (is.numeric(result)) {
      return(result[1])
    } else if (!is.null(result$M)) {
      return(result$M[1])
    } else {
      return(NA_real_)
    }
  }, error = function(e) {
    message("Erreur lors du calcul du mode HSM : ", e$message)
    return(NA_real_)
  })
}
