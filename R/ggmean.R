#' ggmean
#'
#' @param size Size of the point representing the average.
#' @param color Color of the point outline.
#' @param fill Dot fill color.
#' @param shape Point shape.
#'
#' @return A ggplot stat_summary addition to display the average.
#' @export
#'
#' @examples
#' data <- data.frame(x=factor(c('A','A','A','B','B','C','C')),y=c(10,20,30,40,50,60,70))
#' ggplot2::ggplot(data,ggplot2::aes(x=x,y=y)) + ggplot2::geom_violin() + ggmean()

ggmean <- function(size=4.5,color="black",fill="white",shape=21) {
  ggplot2::stat_summary(fun=mean,geom="point",size=size,colour=color,fill=fill,shape=shape)
  #creation du point representant la moyenne
}

# variables globales
utils::globalVariables(c("x","y"))
