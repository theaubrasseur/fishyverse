#' stat2
#'
#' @param data A data.frame containing the data.
#' @param x A numeric variable.
#' @param y A categorical variable (factor).
#'
#' @return A statistical summary of the data.
#' @export
#'
#' @examples
#' data <- data.frame(x=factor(c('A','A','A','A','A','B','B','B','B','B','B')),y=c(10,20,30,40,50,60,70,80,90,100,110))
#' stat2(data,x=y,y=x)

stat2 <- function(data,x,y){
  #x= variable numérique   y= facteur   data= jeu de donnees
  stat <- skimr::skim_with(numeric=skimr::sfl(p0=~quantile(.,probs=.0), p5=~quantile(.,probs=.05), p10=~quantile(.,probs=.1), p15=~quantile(.,probs=.15), p20=~quantile(.,probs=.2), p25=~quantile(.,probs=.25), p30=~quantile(.,probs=.3), p33=~quantile(.,probs=.33), p35=~quantile(.,probs=.35), p40=~quantile(.,probs=.4), p45=~quantile(.,probs=.45), p50=~quantile(.,probs=.5), p55=~quantile(.,probs=.55), p60=~quantile(.,probs=.6), p65=~quantile(.,probs=.65), p66=~quantile(.,probs=.66), p70=~quantile(.,probs=.7), p75=~quantile(.,probs=.75), p80=~quantile(.,probs=.8), p85=~quantile(.,probs=.85), p90=~quantile(.,probs=.9), p95=~quantile(.,probs=.95), p100=~quantile(.,probs=1), n=length, sum=sum, mean=mean, median=median, min=min, max=max, range=~max(.)-min(.), sd=sd, cv=BioStatR::cvar, iqr=IQR, var=var), append=F) ; options(pillar.sigfig= 10)
  #ensemble de statistiques descriptives dans l'objet stat
  stata <- data %>% group_by({{y}}) %>% stat({{x}})
  #calcul des statistiques descriptives selon les modalites du facteur
  x <- dplyr::pull(data,{{x}})
  #conversion de la variable "x" en vecteur dans l'objet "x"
  y <- dplyr::pull(data,{{y}})
  #conversion de la variable "y" en vecteur dans l'objet "y"
  shapiro.test.data <- RcmdrMisc::normalityTest(x,test="shapiro.test")
  #test de shapiro-wilk pour l'ensemble des donnees
  shapiro.test.groups <- RcmdrMisc::normalityTest(x~y,test="shapiro.test")
  #test de shapiro-wilk par groupes
  ks.test.data <- RcmdrMisc::normalityTest(x,test="lillie.test")
  #test de kolmogorov-smirnov pour l'ensemble des donnees
  ks.test.groups <- RcmdrMisc::normalityTest(x~y,test="lillie.test")
  #test de kolmogorov-smirnov par groupes
  fisher.test <- var.test(x~y,alternative='two.sided',conf.level=.95)
  #test de fisher
  t.test <- t.test(x~y,alternative='two.sided',conf.level=.95,var.equal=T)
  #test de student
  wilcox.test <- wilcox.test(x~y,alternative="two.sided",exact=F)
  #test de wilcoxon
  list("Test de Shapiro-Wilk pour verifier la normalite des donnees par groupes (a utiliser uniquement si effectifs <50, sinon prendre le test de Kolmogorov-Smirnov). Si p<0.05 -> non-normal."=shapiro.test.groups,
       "Test de Kolmogorov-Smirnov pour verifier la normalite des donnees par groupes (a utiliser uniquement si effectifs >50, sinon prendre le test de Shapiro-Wilk). Si p<0.05 -> non-normal."=ks.test.groups,
       "Test de Shapiro-Wilk pour verifier la normalite des donnees pour l'ensemble du jeu de donnees (a utiliser uniquement si effectifs <50, sinon prendre le test de Kolmogorov-Smirnov). Si p<0.05 -> non-normal."=shapiro.test.data,
       "Test de Kolmogorov-Smirnov pour verifier la normalite des donnees pour l'ensemble du jeu de donnees (a utiliser uniquement si effectifs >50, sinon prendre le test de Shapiro-Wilk). Si p<0.05 -> non-normal."=ks.test.data,
       "Test de Fisher pour verifier l'egalite des variances ou homoscedasticite (a verifier uniquement lorsque les effectifs sont inegaux). Si p<0.05 -> les variances ne sont pas egales."= fisher.test,
       "Test de Student (si il y a: 1-la normalite des donnees ou si effectifs > 30 et 2-egalite des variances (si les effectifs sont inegaux). Si p<0.05 -> difference significative."= t.test,
       "Test de Wilcoxon (si il n'y a pas egalite des variances). Si p<0.05 -> difference significative."= wilcox.test,
       "Statistiques descriptives selon les modalites du facteur"=stata)
  #list --> sorties (nom des sorties entre guillemets et objet concerne apres le signe egal)
}

# variables globales
utils::globalVariables(c("median","sd","IQR","var","var.test"))
