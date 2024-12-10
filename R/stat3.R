#' stat3
#'
#' @param data A data.frame containing the data.
#' @param x A numeric variable.
#' @param y A categorical variable (factor).
#'
#' @return A statistical summary of the data.
#' @export
#'
#' @examples
#' data <- data.frame(x=factor(c('A','A','A','A','A','B','B','B','B','B','B','C','C','C','C','C')),y=c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))
#' stat3(data,x=y,y=x)

stat3 <- function(data,x,y){
  stat <- skimr::skim_with(numeric=skimr::sfl(p0=~quantile(.,probs=.0), p5=~quantile(.,probs=.05), p10=~quantile(.,probs=.1), p15=~quantile(.,probs=.15), p20=~quantile(.,probs=.2), p25=~quantile(.,probs=.25), p30=~quantile(.,probs=.3), p33=~quantile(.,probs=.33), p35=~quantile(.,probs=.35), p40=~quantile(.,probs=.4), p45=~quantile(.,probs=.45), p50=~quantile(.,probs=.5), p55=~quantile(.,probs=.55), p60=~quantile(.,probs=.6), p65=~quantile(.,probs=.65), p66=~quantile(.,probs=.66), p70=~quantile(.,probs=.7), p75=~quantile(.,probs=.75), p80=~quantile(.,probs=.8), p85=~quantile(.,probs=.85), p90=~quantile(.,probs=.9), p95=~quantile(.,probs=.95), p100=~quantile(.,probs=1), n=length, sum=sum, mean=mean, median=median, min=min, max=max, range=~max(.)-min(.), sd=sd, cv=BioStatR::cvar, iqr=IQR, var=var), append=F) ; options(pillar.sigfig= 10)
  #ensemble de statistiques descriptives dans l'objet stat
  stata <- data %>% group_by({{y}}) %>% stat({{x}})
  #calcul des statistiques descriptives selon les modalites du facteur
  x <- dplyr::pull(data, {{x}})
  #conversion de la variable "x" en vecteur dans l'objet "x"
  y <- dplyr::pull(data, {{y}})
  #conversion de la variable "y" en vecteur dans l'objet "y"
  shapiro.test.data <- RcmdrMisc::normalityTest(x,test="shapiro.test")
  #test de shapiro-wilk pour l'ensemble des donnees
  shapiro.test.groups <- RcmdrMisc::normalityTest(x~y,test="shapiro.test")
  #test de shapiro-wilk par groupes
  ks.test.data <- RcmdrMisc::normalityTest(x,test="lillie.test")
  #test de kolmogorov-smirnov pour l'ensemble des donnees
  ks.test.groups <- RcmdrMisc::normalityTest(x~y,test="lillie.test")
  #test de kolmogorov-smirnov par groupes
  bartlett.test <- bartlett.test(x~y)
  #test de bartlett
  anova <- aov(x~y)
  #test d'anova
  anova.test <- summary(anova)
  #resume du test d'anova
  tukey.test <- summary(multcomp::glht(anova, linfct=multcomp::mcp(y="Tukey")))
  #test post-hoc de tukey
  tukey.test.1 <- TukeyHSD(anova)
  #suite du test post-hoc de tukey
  tukey.letters <- multcomp::cld(tukey.test)
  #lettres pour test de tukey
  kruskal.test <- kruskal.test(x~y)
  #test de kruskal-wallis
  pairwise.wilcox.test <- pairwise.wilcox.test(x,y,p.adjust.method="bonferroni",exact=F)
  #test post-hoc de somme des rangs de Wilcoxon par paires
  list("Test de Shapiro-Wilk pour verifier la normalite des donnees par groupes (a utiliser uniquement si effectifs <50, sinon prendre le test de Kolmogorov-Smirnov). Si p<0.05 -> non-normal."=shapiro.test.groups,
       "Test de Kolmogorov-Smirnov pour verifier la normalite des donnees par groupes (a utiliser uniquement si effectifs >50, sinon prendre le test de Shapiro-Wilk). Si p<0.05 -> non-normal."=ks.test.groups,
       "Test de Shapiro-Wilk pour verifier la normalite des donnees sur l'ensemble du jeu de donnees (a utiliser uniquement si effectifs <50, sinon prendre le test de Kolmogorov-Smirnov). Si p<0.05 -> non-normal."=shapiro.test.data,
       "Test de Kolmogorov-Smirnov pour verifier la normalite des donnees sur l'ensemble du jeu de donnees (a utiliser uniquement si effectifs >50, sinon prendre le test de Shapiro-Wilk). Si p<0.05 -> non-normal."=ks.test.data,
       "Test de Bartlett pour verifier l'egalite des variances ou homoscedasticite. Si p<0.05 -> les variances ne sont pas egales."= bartlett.test,
       "Test de Anova (si il y a: 1-la normalite des donnees et 2-egalite des variances ou homoscedasticite. Test parametrique. Si p<0.05 -> difference significative."= anova.test,
       "Test de Tukey (test parametrique en post-hoc d'une anova). Si p<0.05 -> difference significative."= tukey.test,
       "Suite du test de Tukey"= tukey.test.1,
       "Lettres de differences significatives issues du test de Tukey"=tukey.letters,
       "Test de Kruskal-Wallis (test non-parametrique a utiliser si les conditions de l'anova ne sont pas respectees). Si p<0.05 -> difference significative."=kruskal.test,
       "Test de somme des rangs de Wilcoxon par paires avec la methode d'ajustement de la p-value de bonferroni (test non-parametrique en post-hoc d'un Kruskal-Wallis). Si p<0.05 -> difference significative."=pairwise.wilcox.test,
       "Statistiques descriptives selon les modalites du facteur"=stata)
  #list --> sorties (nom des sorties entre guillemets et objet concerne apres le signe egal)
}

# variables globales
utils::globalVariables(c("median","sd","IQR","var","var.test","aov","TukeyHSD"))
