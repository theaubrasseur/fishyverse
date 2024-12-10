#' ggnin
#'
#' @param data A data.frame containing the data.
#' @param x A categorical variable (factor) used for grouping.
#' @param y A numeric variable used for the y-axis.
#'
#' @return A ggplot object with the count of observations displayed.
#' @export
#'
#' @examples
#' data <- data.frame(x=factor(c('A','A','A','B','B','C','C')),y=c(10,20,30,40,50,60,70))
#' ggplot2::ggplot(data) + ggplot2::geom_violin(ggplot2::aes(x=x, y=y)) + ggnin(data,x,y)

ggnin <- function(data,x,y){
  #data= jeu de donnees    x= facteur   y= ordonnee
  x <- dplyr::pull(data,{{x}})
  #conversion de la variable "x" en vecteur dans l'objet "x"
  y <- dplyr::pull(data,{{y}})
  #conversion de la variable "y" en vecteur dans l'objet "y"
  min <- min(y)-min(y)*0.35
  #calcul de la position du texte sur l'ordonnee pour un violinplot (pour un boxplot, mettre 0.05)
  n_fun <- function(x){return(data.frame(y=min,label=paste0("n= ",length(x))))}
  #calcul du nb d'observations
  ggplot2::stat_summary(ggplot2::aes(x=x,y=y),fun.data=n_fun, geom="text", colour="black", size=6, fontface=1)
  #insertion du nb d'observations dans le graph
}
