#' Verifier la configuration systeme requise
#' @return TRUE si tout est OK, liste des problemes sinon
#' @export
verifier_configuration <- function() {

  problemes <- c()

  # Verifier les packages requis
  packages_requis <- c("haven", "tidyverse", "dplyr", "ggplot2", "readxl",
                       "labelled", "VIM", "scales", "moments", "modeest")

  for (pkg in packages_requis) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      problemes <- c(problemes, paste("Package manquant:", pkg))
    }
  }

  # Verifier la version de R
  version_r <- as.numeric(paste0(R.version$major, ".", R.version$minor))
  if (version_r < 4.0) {
    problemes <- c(problemes, "Version R < 4.0 detectee, certaines fonctions peuvent ne pas marcher")
  }

  # Verifier la memoire disponible (Windows uniquement)
  if (.Platform$OS.type == "windows") {
    mem_info <- try(system("wmic computersystem get TotalPhysicalMemory", intern = TRUE), silent = TRUE)
    if (!inherits(mem_info, "try-error")) {
      cat("Memoire systeme detectee\n")
    }
  }

  # Resultat final
  if (length(problemes) == 0) {
    cat("Configuration systeme OK\n")
    return(TRUE)
  } else {
    cat("Problemes detectes:\n")
    for (prob in problemes) {
      cat("-", prob, "\n")
    }
    return(problemes)
  }
}
