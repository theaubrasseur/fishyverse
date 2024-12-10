#' freq
#'
#' @param data A data.frame containing the data.
#' @param x A categorical variable (factor or character) to compute frequencies for.
#'
#' @return A data.frame with the following columns:
#' - `modalites`: unique levels of the variable `x`.
#' - `freq`: absolute frequencies of each level.
#' - `freq_relative`: relative frequencies in percentages.
#' - `cumfreq`: cumulative absolute frequencies.
#' - `cumfreq_relative`: cumulative relative frequencies in percentages.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr pull
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' data <- data.frame(
#' id = factor(c(1,1,2,2,2,2,3,3,3,4,4,4,5,5,5),levels= c(1,2,3,4,5)))
#' freq(data,id)

freq <- function(data,x){
  x <- dplyr::pull(data,{{x}})
  #conversion de la variable "x" en vecteur dans l'objet "x"
  n <- data.frame(table(x))
  #calcul dans le data.frame "n" de la frequence absolue de chaque modalites du facteur
  res <- n %>% dplyr::rename(modalites = x, freq = Freq) %>%
    #les deux premieres variables sont renomees
    dplyr::mutate(freq_relative=(freq/sum(freq))*100, cumfreq=cumsum(freq), cumfreq_relative= cumsum(freq_relative))
  #calcul de la frequence relative, de la frequence cumulee absolue et de la frequence cumulee relative
  res
  #affichage du resultat
}

# variables globales
utils::globalVariables(c("Freq", "freq_relative"))
