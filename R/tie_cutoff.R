#' Cutoff determination for increasing number of randomizations
#'
#' The \code{tie_cutoff} function calculates the mean Euclidean distance between commodity worth values. This becomes
#' relevant when ties are present in the data. Depending on how ties are resolved (see prefLimit argument in the function),
#' the items' position will change a lot. Since their relative positions are a function of the number of ties, more
#' randomizations will stabilize their means and thus commodity positions. Increasing the number of randomizations usually leads
#' not only to a stabilized mean but also to smaller confidence intervals. By defining a relative cutoff (e.g., 5 or 10%) for the
#' range of the CIs regarding the maximum range in the data, a cutoff for the number of randomizations can be found.
#'
#' @param R number of maximum randomization steps
#' @param dat imported raw data (should be binary, if not, will be binarized automatically)
#' @param cpus No. of used local CPUs for parallel computing (you should have more than 2)
#' @param SV name of the side variable
#' @param RF name of the reference fluid variable
#' @param CF name of the combination fluid variable
#' @param id subject IDs
#' @param RV name of the response variable
#' @param prefLimit preference limit for binarization threshold
#' @param ord item category order
#' @param default default item in worth value estimation (usually the lowest worth value)
#' @param compstudy label of the compiled sub study (used for filtering)
#' @param showplot show the plot for randomization cutoff determination
#' @param cutoff Percent cutoff level (default: 0.05) - means CI range < than cutoff value
#' @param showCutoff show vertical line of the cutoff
#' @param standardize standardize on the maximum CI value?
#'
#' @import magrittr
#' @import dplyr
#' @import reshape2
#' @import prefmod
#' @import gnm
#' @import parallel
#' @import doParallel
#' @import foreach
#' @import ggplot2
#' @import Rmisc
#' @import doRNG
#'
#' @return Exports cutoff value and plots
#'
#' @export
#'
#'
tie_cutoff <- function(data=tiefightR::mouse, R=50, ciLvl= 0.95, cpus=2, SV=NULL, RF=NULL,
                       CF=NULL,id=NULL, RV=NULL, ord=NULL, prefLimit=50, compstudy=NULL,
                       default=NULL, showplot=FALSE, showCutoff=FALSE){

  #if(seed==TRUE){
  #  set.seed(123)
  #}else{}

  pos   <- NULL
  registerDoParallel(cpus)
  registerDoRNG(seed = 123)
  pos   <- foreach (i=1:R ,
                    .combine=rbind,
                    .packages=c("magrittr","tidyverse","reshape2","Rmisc","base", "tiefightR")) %dopar% {

                      h     <- NULL
                      h     <- replicate(i, tie_rwalk(dat        = data,
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
                      df <- data.frame(R=i,
                                       dist=mean(dist(set)),
                                       upr= as.numeric(Rmisc::CI( dist(set), ci =ciLvl)[1]),
                                       lwr= as.numeric(Rmisc::CI( dist(set), ci =ciLvl)[3]) )
                    }
  stopImplicitCluster()


  delta <- c()
  for(i in 2:dim(pos)[1]){
    delta[i] <- pos$dist[i] - pos$dist[i-1]
  }
  pos$delta <- NULL
  pos$delta <- delta

  pos <- pos %>%
    mutate(pos, normdelta=pos$delta/max(pos$delta, na.rm=TRUE))

  return(pos)
}
