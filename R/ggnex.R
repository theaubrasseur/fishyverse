#' ggnex
#'
#' @param data A data.frame containing the data.
#' @param x A categorical variable in the data.frame.
#'
#' @return A ggplot object with modified x-axis labels.
#' @export
#'
#' @examples
#' data <- data.frame(x=factor(c('A','A','A','B','B','C')))
#' ggplot2::ggplot(data) + ggplot2::geom_bar(ggplot2::aes(x=x)) + ggnex(data,x)

ggnex <- function(data,x){
  #data= jeu de donnees    x= facteur
  x <- dplyr::pull(data,{{x}})
  #conversion de la variable "x" en vecteur dans l'objet "x"
  res <- paste0(levels(x),"\nn= ",table(x))
  #calcul du nombre d'observations par modalites de facteur
  ggplot2::scale_x_discrete(labels=res)
  #fonction qui insere le calcul dans le graphique
}
