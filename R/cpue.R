#' cpue
#'
#' @param data A data.frame containing the data.
#' @param effectifs A numeric column representing captured quantities.
#' @param debut A column with start dates (must be in `Date` or `POSIXct` format).
#' @param fin A column with end dates (must be in `Date` or `POSIXct` format).
#'
#' @return A data.frame grouped by `debut` and `fin`.
#' - `effectifs`: Sum of captured quantities for each group.
#' - `nb_jours`: Number of days between start and end dates.
#' - `cpue`: Catch per unit effort (calculated as `effectifs / nb_jours`).
#' @importFrom dplyr group_by summarise mutate %>%
#' @export
#'
#' @examples
#' data <- data.frame(
#'   debut = as.Date(c("2024-01-01", "2024-01-15", "2024-01-22")),
#'   fin = as.Date(c("2024-01-03", "2024-01-29", "2024-02-04")),
#'   effectifs = c(10, 20, 30))
#' cpue(data,effectifs,debut,fin)

cpue <- function(data,effectifs,debut,fin){
  #data= jeu de donnees    effectifs= effectifs captures   debut= date de debut   fin= date de fin
  res <- data %>% dplyr::group_by(debut={{debut}},fin={{fin}}) %>%
    #groupement des donnees par les dates de debut et de fin
    dplyr::summarise(effectifs= sum({{effectifs}})) %>%
    #somme des effectifs par date
    dplyr::mutate(nb_jours= as.numeric(fin-debut, "days"), #minutes= mins heures= hours jours= days --> Si pas le format cherche, faire simple calcul de conversion.
           #nombre de jours entre la date de fin et de debut de piegeage
           cpue= effectifs/nb_jours)
  #calcul de la cpue
  res
  #affichage du resultat
}

# variables globales
utils::globalVariables(c("nb_jours"))
