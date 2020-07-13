#' Commodity position and confidence interval check for a discrete number of randomizations
#'
#' The \code{tie_cicheck} is a wrapper for checking the confidence intervals for data with ties. The function calculates the worth
#' values for a specific number of randomizations and reports the confidence intervals for the commodity means.
#'
#' @param R number of maximum randomization steps
#' @param dat imported raw data (should be binary, if not, will be binarized automatically)
#' @param SV name of the side variable
#' @param RF name of the reference fluid variable
#' @param CF name of the combination fluid variable
#' @param id subject IDs
#' @param RV name of the response variable
#' @param seed TRUE/FALSE for constant seeding
#' @param prefLimit preference limit for binarization threshold
#' @param ord item category order
#' @param default default item in worth value estimation (usually the lowest worth value)
#' @param compstudy label of the compiled sub study (used for filtering)
#' @param showplot show the errorplot with confidence intervals
#' @param ciLvl Level of confidence (default: 0.95)
#' @param showstats calculate ANOVA1 and Tukey's test for the commodities
#'
#' @import ggplot2
#' @import ggpubr
#' @import reshape2
#' @importFrom stats aov TukeyHSD
#' @import Rmisc
#'
#' @return Exports random binarize response for distance cutoff selection
#'
#' @export

  tie_cicheck <- function(data=tiefightR::mouse, R=NULL, ciLvl=0.95, seed=TRUE, SV=NULL, RF=NULL,
                          CF=NULL,id=NULL, RV=NULL, ord=NULL, prefLimit=50, compstudy=NULL, default=NULL,
                          showplot=TRUE, showstats=FALSE, ylim=c(0.1,0.35)){

    # do the desired randomizations
    if(seed==TRUE){
      set.seed(123)
    }else{}

    h     <- NULL
    h     <- replicate(R, tie_rwalk(dat        = data,
                                    SV         = SV,
                                    RF         = RF,
                                    CF         = CF,
                                    id         = id,
                                    RV         = RV,
                                    ord        = ord,
                                    prefLimit  = prefLimit,
                                    setseed    = FALSE,
                                    compstudy  = compstudy,
                                    default    = default))

    # collate the worth value arrays
    n     <- names(h[,,1])
    set   <- NULL
    for(j in 1:dim(h)[3]){
      s   <- h[,,j]
      set <- rbind(set, s[names(s)==n  ])
    }

    # do da melt!
    M     <- melt(set)

    # plot or not
    if(showplot==TRUE){
      colnames(M) <- c("R","commodity","worth")
      Rplot <- ggerrorplot(M, x = "commodity", y = "worth",
                           desc_stat = "mean_ci", color = "commodity", ci = ciLvl,
                           palette="uchicago", size=.4) +
        ylim(ylim) +
        labs(x="",
             y = "Worth value",
             title    = "Bootstrapped worth value on ties",
             subtitle = paste("Error bars depicting mean ", "and ", ciLvl,"% ","CIs ", "(R=", R,")",sep="")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1, size=12))
      print(Rplot)
    }else{}

    # do some sadistics!
    if(showstats==TRUE){
      fit <- aov(M$worth ~ M$commodity, data=M)
      # print(summary)
      print(TukeyHSD(fit))
    }else{}


    return(list(M,p=Rplot))

}
