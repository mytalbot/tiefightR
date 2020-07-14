#' Tie Simulation Function
#'
#' The \code{tie_sim} function starts a simulation of item pairings and introduces random pairs for the remaining
#' combinations. In parallel, the intransitivity of triple pairings can be calculated to estimate their position quality.
#' Good transitiviy and massed localization in a position will improve the Likelihood of a good fit for the item.
#'
#' @param xdata imported (binarized) data frame
#' @param RF name of the reference fluid variable
#' @param CF name of the combination fluid variable
#' @param id subject IDs
#' @param RV name of the response variable
#' @param intrans calculate intransitivities (calculation intense!)
#' @param compstudy label of the compiled sub study (used for filtering)
#' @param default default item in worth value estimation (usually the lowest worth value)
#' @param ord item category order
#' @param cpus No. of used local CPUs for parallel computing (you should have more than 2)
#' @param v1 testing variable (can be one item from the item list)
#' @param R No. of randomization steps
#'
#' @import magrittr
#' @import dplyr
#' @import reshape2
#' @import prefmod
#' @import gnm
#' @import parallel
#' @import doParallel
#' @import foreach
#'
#' @return data.frame with the simulation results
#'
#' @export


tie_sim <- function(xdata     = NULL,
                    R         = 2,
                    RF        = "img1",
                    CF        = "img2",
                    id        = "ID",
                    RV        = "pref_img1",
                    intrans   = TRUE,
                    compstudy = "LagreValenceRange_SpringSchool",
                    default   = "War",
                    cpus      = 2,
                    ord       = NULL,
                    v1        = NULL){

  # calculate the  no of possible combinations
  restOrd  <- ord[!(ord %in% v1)]
  combis   <- expand.grid(rep(list(0:1), length(restOrd)))
  colnames(combis) <- restOrd

  # register CPU cores and prep dfs
  registerDoParallel(cpus)
  L        <- length(ord)-1
  runs     <- dim(combis)[1]
  res      <- NULL
  h        <- NULL
  # do the parallel processing
  res   <- foreach (j=2:runs,
                    .combine=rbind, .packages=c("magrittr","tidyverse","reshape2","prefmod","tiefightR","gnm")) %dopar% {

                      v2  <- restOrd[combis[j,]==1]
                      h   <- NULL
                      h   <- replicate(R,
                                       tie_worth(xdata     = xdata,
                                                 RF        = RF,
                                                 CF        = CF,
                                                 id        = id,
                                                 RV        = RV,
                                                 showplot  = FALSE,
                                                 intrans   = intrans,
                                                 compstudy = compstudy,
                                                 default   = default,
                                                 ordn      = ord,
                                                 r1        = v1,
                                                 r2        = v2) )


                      # include intransitivity calculations or skip for faster performance?
                      if(intrans==TRUE){

                        intrcol    <- which(rownames(h)=="intrans")
                        worthcol   <- which(rownames(h)=="worth")

                        data.frame(worth    = rowMeans(as.data.frame(h[worthcol,])) [order(rowMeans(as.data.frame(h[worthcol,])))],
                                   picture  = names(rowMeans(as.data.frame(h[worthcol,])) [order(rowMeans(as.data.frame(h[worthcol,])))]),
                                   pos      = 1:length(ord),
                                   intrans  = mean(as.numeric(h[intrcol,])),
                                   combinr  = j,
                                   noVars   = length(v2),
                                   r2       = paste(v2,collapse = ","))
                      }else{
                        worthcol   <- which(rownames(h)=="worth")

                        data.frame(worth    = rowMeans(as.data.frame(h[worthcol,])) [order(rowMeans(as.data.frame(h[worthcol,])))],
                                   picture  = names(rowMeans(as.data.frame(h[worthcol,])) [order(rowMeans(as.data.frame(h[worthcol,])))]),
                                   pos      = 1:length(ord),
                                   combinr  = j,
                                   noVars   = length(v2),
                                   r2       = paste(v2,collapse = ","))
                      }

                    }
  rownames(res) <- NULL
  stopImplicitCluster()

  return(res)
}
