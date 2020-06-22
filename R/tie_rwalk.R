#' Tie random walk function
#'
#' The \code{tie_rwalk} function prepares binary and continuous data form import into the tiefightR analysis.
#'
#' @param xdata imported (binarized) data frame
#' @param SV name of the side variable
#' @param RF name of the reference fluid variable
#' @param CF name of the combination fluid variable
#' @param id subject IDs
#' @param RV name of the response variable
#' @param compstudy label of the compiled sub study (used for filtering)
#' @param ord item category order
#' @param default default item in worth value estimation (usually the lowest worth value)
#'
#' @import ggplot2
#' @import ggpubr
#'
#' @return Exports random binarize response for distance cutoff selection
#'
#' @export

tie_rwalk  <- function(dat        = NULL,
                       SV         = NULL,
                       RF         = NULL,
                       CF         = NULL,
                       id         = NULL,
                       RV         = NULL,
                       ord        = NULL,
                       prefLimit  = 50,
                       setseed    = FALSE,
                       compstudy  = NULL,
                       default    = NULL,
                       R          = NULL){

  # check if response variable is binary
  responseV <- dat[,names(dat)==RV]

  # binarize data automatically, if continous
  if ( (all(responseV %in% 0:1))==FALSE ) {

    bin_mouse  <- tie_binarize(xdata  = dat,
                               SV   = SV,
                               RF   = RF,
                               CF   = CF,
                               id   = id,
                               RV   = RV,
                               compiled_studies = compstudy,
                               setseed    = setseed,
                               prefLimit  = prefLimit)

    mouse      <- tie_worth(xdata         = bin_mouse,
                            compstudy     = compstudy,
                            default       = default,
                            ordn          = ord)

    res        <-  mouse$worth

  }else{ # binary

    mouse      <- tie_worth(xdata   = dat,
                            SV      = SV,
                            RF      = RF,
                            CF      = CF,
                            id      = id,
                            RV      = RV,
                            compstudy     = compstudy,
                            default       = default,
                            ordn          = ord)

    res        <-  mouse$worth
  }
  return(res)
}


