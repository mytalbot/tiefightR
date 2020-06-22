#' Tie random wrapper for the rwalk function
#'
#' The \code{tie_rwrap} is a wrapper function for tie_rwalk and uses a number of R randomization steps to calculate the mean
#' Euclidean distance between worth values. The resulting curve will tell something about tie stability in the data and can
#' be used for the definition of a randomization step cut-off value.
#'
#' @param R number of maximum randomization steps
#' @param dat imported raw data (should be binary, if not, will be binarized automatically)
#' @param SV name of the side variable
#' @param RF name of the reference fluid variable
#' @param CF name of the combination fluid variable
#' @param id subject IDs
#' @param RV name of the response variable
#' @param setseed TRUE/FALSE for seeding
#' @param prefLimit preference limit for binarization threshold
#' @param ord item category order
#' @param default default item in worth value estimation (usually the lowest worth value)
#' @param compstudy label of the compiled sub study (used for filtering)
#'
#' @import ggplot2
#' @import ggpubr
#' @import ggsci
#' @importFrom stats sd dist
#'
#' @return Exports random binarize response for distance cutoff selection
#'
#' @export

tie_rwrap  <- function(dat        = NULL,
                       SV         = NULL,
                       RF         = NULL,
                       CF         = NULL,
                       id         = NULL,
                       RV         = NULL,
                       ord        = NULL,
                       default    = NULL,
                       prefLimit  = 50,
                       setseed    = FALSE,
                       compstudy  = NULL,
                       R          = 2){

  # randomize n-times
  h     <- NULL
  h     <- replicate(R, tie_rwalk(dat        = dat,
                                  SV         = SV,
                                  RF         = RF,
                                  CF         = CF,
                                  id         = id,
                                  RV         = RV,
                                  ord        = ord,
                                  prefLimit  = prefLimit,
                                  setseed    = setseed,
                                  compstudy  = compstudy,
                                  default    = default))


  # collate the arrays
  n     <- names(h[,,1])
  set   <- NULL
  for(j in 1:dim(h)[3]){
    s   <- h[,,j]
    set <- rbind(set, s[names(s)==n  ])
  }

  # how many randomizations are "needed" for a stable mean?
  # the former is dependent on the individual scales, so we also calculate the generalized distance (d) and SD (dsd)
  means   <- NULL
  SD      <- NULL
  d       <- NULL
  dsd     <- NULL
  for(i in 2:R){
    means <- rbind(means, colMeans(set[1:i,]))
    SD    <- rbind(SD, apply(set[1:i,],2, sd ))
    d     <- rbind(d,   data.frame( d      = mean( dist( colMeans( set[1:i,]) ) ),
                                    dsd    = mean( dist( apply( set[1:i,],2,sd))),
                                    randos = i))
  }


  plot_flow <- ggplot(d, aes(x=randos, y=d )) +
    geom_line(lwd=1.2)   +
    #geom_pointrange(aes(ymin=d-dsd, ymax=d+dsd)) +
    ylab(expression(paste(Delta," mean Euclidean distance"))) +
    xlab("No. of randomizations")  +
    theme_minimal() +
    theme(text = element_text(size=11)) +
    labs(color="Preference limit") +
    theme(legend.key=element_blank(),legend.background=element_blank())
  plot_flow <- plot_flow + scale_color_startrek()
  plot_flow <- plot_flow + theme(legend.position="top")

  plot_SD <- ggplot(d, aes(x=randos, y=dsd )) +
    geom_line(lwd=1.2)  +
    ylab(expression(paste("Fluctuation: Variance of ", Delta," distance"))) +
    xlab("No of. randomizations")  +
    theme_minimal() +
    theme(text = element_text(size=11)) +
    labs(color="Preference limit") +
    theme(legend.key=element_blank(),legend.background=element_blank())
  plot_SD <- plot_SD + scale_color_startrek()
  plot_SD <- plot_SD + theme(legend.position="top")

  return( list(p1=plot_flow, p2=plot_SD, d=d ) )
}










