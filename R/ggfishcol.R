#' ggfishcol
#'
#' @return A ggplot object with a fill color depending on the species.
#' @export
#'
#' @examples
#' data <- data.frame(x=factor(c('SAT','BRO','PER')),y=c('12','15','28'))
#' ggplot2::ggplot(data) + ggplot2::geom_bar(ggplot2::aes(x=x,y=y,fill=x),stat="identity") + ggfishcol()

ggfishcol <- function(){
  #pas d'arguments necessaires
  ggplot2::scale_fill_manual(values=c(SAT="#00EEFF",TRM="#0BDDE5",TRF="#56B4E9",ANG="#5F767A",LPX="#BEA595",LPP="#997A8D",LPF="#614355",LPM="#31202A",ALA="#054E10",ALF="#087D19",CHA="#0F262A",GAR="#EFB022",CCO="#844D17",LOF="#9F7154",CHE="#FFE900",GOU="#C776F2",OCL="red4",SDF="#4A8898",'Salvelinus sp.'="#4A8898",EPI="#D500C2",EPT="#920085",VAN="#005984",TAN="#FF9D00",CYP="#EA4545",TAC="#003BC1",'Oncorhynchus sp.'="#003BC1",ASL="#3FF290",ROT="#FF7800",ABL="#AAFF00",GDL="red2",BOU="#D3D3D3",IDE="#5A3E7C",BRE="#836200",GRE="red",FLE="#66B540",BRB="#D6A000",PCH="#D51010",BAF="#88C20D",MUP="#FFFF3B",IND="#FF00C6",SRP="#D78DFF",PER="#EF5222",PES="red3",BRO="#2ABD00",SAN="#B0FF93",BBG="#711010",SIL="#393939"))
  #couleurs de remplissage pour les especes
  }
