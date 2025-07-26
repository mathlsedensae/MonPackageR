#' Enrichir les donnees avec les informations geographiques
#' @param data Dataframe principal
#' @param Ehcvm_all Base complete EHCVM
#' @return Dataframe enrichi avec les informations geographiques
#' @importFrom stats median na.omit quantile sd
#' @importFrom utils write.csv
#' @importFrom dplyr all_of
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom utils capture.output
#' @export
enrichir_donnees_geo <- function(data, Ehcvm_all) {

  # Extraire les informations geographiques
  infos_geo <- Ehcvm_all %>%
    dplyr::select(
      menage_id = interview__key,
      region = s00q01,
      departement = s00q02,
      milieu = s00q04
    ) %>%
    dplyr::distinct()

  # Labels pour les regions
  labels_region <- c(
    DAKAR = 1, ZIGUINCHOR = 2, DIOURBEL = 3, "SAINT-LOUIS" = 4,
    TAMBACOUNDA = 5, KAOLACK = 6, THIES = 7, LOUGA = 8,
    FATICK = 9, KOLDA = 10, MATAM = 11, KAFFRINE = 12,
    KEDOUGOU = 13, SEDHIOU = 14
  )

  # Labels pour les departements
  labels_departement <- c(
    DAKAR = 11, PIKINE = 12, RUFISQUE = 13, GUEDIAWAYE = 14,
    BIGNONA = 21, OUSSOUYE = 22, ZIGUINCHOR = 23,
    BAMBEY = 31, DIOURBEL = 32, M_BACKE = 33,
    DAGANA = 41, PODOR = 42, "SAINT LOUIS" = 43,
    BAKEL = 51, TAMBACOUNDA = 52, GOUDIRY = 53, KOUPENTOUM = 54,
    KAOLACK = 61, NIORO = 62, GUINGUINEO = 63,
    M_BOUR = 71, THIES = 72, TIVAOUANE = 73,
    KEBEMER = 81, LINGUERE = 82, LOUGA = 83,
    FATICK = 91, FOUNDIOUGNE = 92, GOSSAS = 93,
    KOLDA = 101, VELINGARA = 102, MEDINA_YORO_FOULAH = 103,
    MATAM = 111, KANEL = 112, RANEROU = 113,
    KAFFRINE = 121, BIRKELANE = 122, KOUNGHEUL = 123,
    MALEM_HODDAR = 124,
    KEDOUGOU = 131, SALEMATA = 132, SARAYA = 133,
    SEDHIOU = 141, BOUNKILING = 142, GOUDOMP = 143
  )

  # Labels pour le milieu
  labels_milieu <- c(
    Urbain = 1, Rural = 2
  )

  # Joindre et labelliser
  data_enrichi <- data %>%
    dplyr::left_join(infos_geo, by = "menage_id") %>%
    dplyr::mutate(
      region = labelled::set_value_labels(region, labels = labels_region),
      departement = labelled::set_value_labels(departement, labels = labels_departement),
      milieu = labelled::set_value_labels(milieu, labels = labels_milieu)
    )

  return(data_enrichi)
}
