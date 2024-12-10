#' bottom_legend
#'
#' @return A ggplot object with a graphical guide for the scale on the bottom.
#' @export
#'
#' @examples
#' data <- data.frame(x=factor(c('A','A','A','B','B','C','C')),y=c(10,20,30,40,50,60,70))
#' ggplot2::ggplot(data) + ggplot2::geom_violin(ggplot2::aes(x=x,y=y,fill=x)) + bottom_legend()

bottom_legend <- function(){
  #pas d'arguments necessaires
  ggplot2::theme(
    legend.position="bottom",legend.title=ggplot2::element_text(size=15,face="bold",hjust=0.5),
    #legende en bas et parametrage de la police du titre de legende.
    legend.background=ggplot2::element_rect(fill="gray90",linewidth= 0.5,linetype="solid",colour="black"),
    #parametrage de l'arriere-plan de la legende.
    legend.text=ggplot2::element_text(size=14,hjust=0.5),legend.key=ggplot2::element_rect(fill="white",color=NA)
    #parametrage de l'arriere-plan des elements de la legende.
  )
  }
