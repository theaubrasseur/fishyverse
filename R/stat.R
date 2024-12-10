#' stat
#'
#' @param ... Additional arguments (currently unused).
#'
#' @return A statistical summary of the data.
#' @export
#'
#' @examples
#' data <- data.frame(y=c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))
#' stat(data$y)

stat <- function(...){
stat<-skimr::skim_with(numeric=skimr::sfl(p0=~quantile(.,probs=.0), p5=~quantile(.,probs=.05), p10=~quantile(.,probs=.1), p15=~quantile(.,probs=.15), p20=~quantile(.,probs=.2), p25=~quantile(.,probs=.25), p30=~quantile(.,probs=.3), p33=~quantile(.,probs=.33), p35=~quantile(.,probs=.35), p40=~quantile(.,probs=.4), p45=~quantile(.,probs=.45), p50=~quantile(.,probs=.5), p55=~quantile(.,probs=.55), p60=~quantile(.,probs=.6), p65=~quantile(.,probs=.65), p66=~quantile(.,probs=.66), p70=~quantile(.,probs=.7), p75=~quantile(.,probs=.75), p80=~quantile(.,probs=.8), p85=~quantile(.,probs=.85), p90=~quantile(.,probs=.9), p95=~quantile(.,probs=.95), p100=~quantile(.,probs=1), n=length, sum=sum, mean=mean, median=median, min=min, max=max, range=~max(.)-min(.), sd=sd, cv=BioStatR::cvar, iqr=IQR, var=var), append=F) ; options(pillar.sigfig= 10)
}
