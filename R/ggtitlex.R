#' ggtitlex
#'
#' @return A ggplot object with a graphic charter.
#' @export
#'
#' @examples
#' data <- data.frame(x=factor(c('A','A','A','B','B','C','C')),y=c(10,20,30,40,50,60,70))
#' ggplot2::ggplot(data) + ggplot2::geom_violin(ggplot2::aes(x=x, y=y)) + ggtitlex()

ggtitlex <- function(){
  #pas d'arguments necessaires
  ggplot2::theme_grey(base_size= 17) +
    #theme de base
    ggplot2::theme(
      plot.title= ggplot2::element_text(face= "bold", size= 25, hjust= 0.5),
      #titre du graphique
      plot.subtitle= ggplot2::element_text(face= "italic", size= 15, hjust= 0.5),
      #sous-titre
      plot.background= ggplot2::element_rect(colour="black",linewidth=1),
      #contour noir sur le graph complet
      panel.background= ggplot2::element_rect(fill="gray90",colour="black",linewidth=0.5),
      #contour noir autour du quadrillage
      panel.grid.major= ggplot2::element_line(colour="black",linewidth=0.025),
      #trait noir du quadrillage principal
      panel.grid.minor= ggplot2::element_line(colour="black",linewidth=0.0125,linetype="dashed"),
      #trait noir du quadrillage secondaire
      axis.title.x= ggplot2::element_text(size= 20),
      #titre de l'abscisse
      axis.title.y= ggplot2::element_text(size= 20),
      #titre de l'ordonnee
      plot.caption = ggplot2::element_text(color = "black", face = "italic", size=12, hjust= 0.5),
      #titre des sources
      axis.text.x= ggplot2::element_text(colour= "black", size= 15),
      #graduations de l'abscisse
      axis.text.y= ggplot2::element_text(colour= "black", size= 15),
      #graduations de l'ordonnee
      strip.text.x = ggplot2::element_text(size= 17.5),
      #titre des facettes
      panel.spacing = ggplot2::unit(0.3, "lines"),
      #espacement entre les facettes
      legend.position= "none"
      #pas de legende
    )
  }
