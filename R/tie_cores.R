#' CPU Core Detection
#'
#' The \code{tie_cores} detects the number of available CPUs for parallel computing. Don't overdo it!
#'
#' @importFrom parallel detectCores
#'
#' @return No of CPUs on your machine
#'
#' @export

tie_cores <- function(){
  return(detectCores())
}
