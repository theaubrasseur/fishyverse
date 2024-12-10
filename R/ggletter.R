#' ggletter
#'
#' @param data A data.frame containing the data.
#' @param x A categorical variable (factor).
#' @param y A numeric variable.
#' @param test The name of the statistical test.
#'
#' @return A ggplot object with the significative difference letter marker.
#' @export
#'
#' @examples
#' data <- data.frame(
#' x = factor(c("A","A","A","A","A","B","B","B","C","C","C","C","C")),
#' y = c(10,10,11,20,30,40,50,10,1600,1600,2100,4000,2500))
#' ggplot2::ggplot(data,ggplot2::aes(x=x,y=y)) + ggplot2::geom_violin() + ggletter(data,x,y,"kruskal")

ggletter <- function(data,x,y,test){
  #data= jeu de donnees   x= facteur   y= variable numerique   test= test pour le calcul des lettres
  tri.to.squ<-function(x)
  {
    rn <- row.names(x)
    cn <- colnames(x)
    an <- unique(c(cn,rn))
    myval <-  x[!is.na(x)]
    mymat <-  matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
    for(ext in 1:length(cn))
    {
      for(int in 1:length(rn))
      {
        if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
        mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
        mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
      }
    }
    return(mymat)
  }
  #fonction qui convertit les donnees en matrice sous forme de tableau
  x <- dplyr::pull(data, {{x}})
  #conversion de la variable "x" en vecteur dans l'objet "x"
  y <- dplyr::pull(data, {{y}})
  #conversion de la variable "y" en vecteur dans l'objet "y"
  if(test=="wilcoxon"){
    #condition pour realiser le test de wilcoxon
    pwt <- pairwise.wilcox.test(y,x,p.adjust.method="bonferroni",exact=F)
    #test de wilcoxon par paires
    mymat <- tri.to.squ(pwt$p.value)
    #met les p-values dans le dataframe mymat
    myletters <- multcompView::multcompLetters(mymat,compare="<=",threshold=0.05,Letters=letters)
    #calcule les lettres selon les p-values
    myletters_df <- data.frame(group=names(myletters$Letters),letter= myletters$Letters)
    #met les lettres dans le dataframe myletters_df
    wilcoxon <- ggpubr::stat_compare_means(geom="text",method= "wilcox.test", label.y = max(y)+max(y)*0.1, size=6, color="black")
    #test de wilcoxon
    lettres <- ggplot2::annotate("text", x=myletters_df$group, y=max(y)+max(y)*0.05, label=myletters_df$letter, size=6, color= "black")
    #dispose les lettres dans le graph
    list(wilcoxon,lettres)
    #affichage du resultat
  }
  else if(test=="student"){
    #condition pour realiser le test de student
    pwt <- pairwise.t.test(y,x,p.adjust.method="bonferroni",exact=F)
    #test de student par paires
    mymat <- tri.to.squ(pwt$p.value)
    #met les p-values dans le dataframe mymat
    myletters <- multcompView::multcompLetters(mymat,compare="<=",threshold=0.05,Letters=letters)
    #calcule les lettres selon les p-values
    myletters_df <- data.frame(group=names(myletters$Letters),letter= myletters$Letters)
    #met les lettres dans le dataframe myletters_df
    student <- ggpubr::stat_compare_means(geom="text",method= "t.test", label.y = max(y)+max(y)*0.1, size=6, color="black")
    #test de student
    lettres <- ggplot2::annotate("text", x=myletters_df$group, y=max(y)+max(y)*0.05, label=myletters_df$letter, size=6, color= "black")
    #dispose les lettres dans le graph
    list(student,lettres)
    #affichage du resultat
  }
  else if(test=="kruskal"){
    #condition pour realiser le test de kruska-wallis
    pwt <- pairwise.wilcox.test(y,x,p.adjust.method="bonferroni",exact=F)
    #test de wilcoxon
    mymat <- tri.to.squ(pwt$p.value)
    #met les p-values dans le dataframe mymat
    myletters <- multcompView::multcompLetters(mymat,compare="<=",threshold=0.05,Letters=letters)
    #calcule les lettres selon les p-values
    myletters_df <- data.frame(group=names(myletters$Letters),letter= myletters$Letters)
    #met les lettres dans le dataframe myletters_df
    kruskal <- ggpubr::stat_compare_means(geom="text",method= "kruskal.test", label.y = max(y)+max(y)*0.1, size=6, color="black")
    #test de kruskal-wallis
    lettres <- ggplot2::annotate("text", x=myletters_df$group, y=max(y)+max(y)*0.05, label=myletters_df$letter, size=6, color= "black")
    #dispose les lettres dans le graph
    list(kruskal,lettres)
    #affichage du resultat
  }
  else if(test=="anova"){
    #condition pour realiser le test d'anova
    pwt <- pairwise.t.test(y,x,p.adjust.method="bonferroni",exact=F)
    #test post-hoc de student par paires
    mymat <- tri.to.squ(pwt$p.value)
    #met les p-values dans le dataframe mymat
    myletters <- multcompView::multcompLetters(mymat,compare="<=",threshold=0.05,Letters=letters)
    #calcule les lettres selon les p-values
    myletters_df <- data.frame(group=names(myletters$Letters),letter= myletters$Letters)
    #met les lettres dans le dataframe myletters_df
    anova <- ggpubr::stat_compare_means(geom="text",method= "anova", label.y = max(y)+max(y)*0.1, size=6, color="black")
    #test de student
    lettres <- ggplot2::annotate("text", x=myletters_df$group, y=max(y)+max(y)*0.05, label=myletters_df$letter, size=6, color= "black")
    #dispose les lettres dans le graph
    list(anova,lettres)
    #affichage du resultat
  }
}

# variables globales
utils::globalVariables(c("pairwise.wilcox.test","pairwise.t.test"))
