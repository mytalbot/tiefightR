#' Test Function
#'
#' The \code{tie_test} function can be used for individual item testing.
#'
#' @param xdata imported (binarized) data frame
#' @param esti worth estimator (default, "worth", alt: "estimator")
#' @param SV name of the side variable
#' @param RF name of the reference fluid variable
#' @param CF name of the combination fluid variable
#' @param id subject IDs
#' @param RV name of the response variable
#' @param intrans calculate intransitivities (calculation intense!)
#' @param r1 label of the test item (e.g., "Lake")
#' @param r2 label(s) of the remaining item(s)
#' @param compstudy label of the compiled sub study (used for filtering)
#' @param showplot show worth plot TRUE/FALSE
#' @param ymax maximum y-scale of the worth plot
#' @param ordn item category order
#' @param default default item in worth value estimation (usually the lowest worth value)
#'
#' @importFrom reshape2 dcast
#' @importFrom stats as.formula poisson
#' @import dplyr
#' @import Rmisc
#' @import prefmod
#' @importFrom magrittr "%>%"
#' @import gnm
#'
#' @return A frequency table for item positions during the simulation; Tukey's HSD Test for positions; Position Bubble Plot
#'
#' @export


tie_test <- function(xdata      = NULL,
                     R          = NULL,
                     intrans    = TRUE,
                     compstudy  = NULL,
                     default    = NULL,
                     ord        = NULL,
                     seed       = TRUE,
                     testme     = NULL,
                     against    = NULL){

  if(seed==TRUE){
    set.seed(123)
  }else{}

  run        <- NULL
  run        <- (replicate(R,
                           ran <-  tie_worth(xdata     = xdata,
                                             showplot  = FALSE,
                                             intrans   = intrans,
                                             compstudy = compstudy,
                                             default   = default,
                                             ordn      = ord,
                                             r1        = testme,
                                             r2        = against) ))

  # if the intransitivity is checked, conditional outcomes!
  intrcol    <- which(rownames(run)=="intrans")
  worthcol   <- which(rownames(run)=="worth")

  if(intrans==TRUE){
    ran      <- as.data.frame(run[worthcol,])
    I        <- as.numeric(run[intrcol,])
  }else{
    ran      <- as.data.frame(run[worthcol,])
  }

  W          <- rowMeans(ran)
  W          <- W[order(W)  ]
  res        <- data.frame(against = paste(against,collapse = ","),
                           worth   = round(W,3),
                           pos     = 1:length(W),
                           intrans = round(mean(I),2),
                           I_sd    = round(sd(I),2),
                           upr     = round(Rmisc::CI(I)[1],2),
                           lwr     = round(Rmisc::CI(I)[3],2))

  zeigsmir   <- res[rownames(res)==testme, ]
  return(zeigsmir)
}







