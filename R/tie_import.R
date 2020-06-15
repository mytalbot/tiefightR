#' Import Function
#'
#' The \code{tie_import} function loads the raw data
#'
#' @param path path to the raw data
#' @param valenceset subset filtering argument
#'
#' @return data.frame with the filtered subset
#'
#' @export
#'

tie_import <- function(path= NULL, valenceset=NULL){

  raw      <- utils::read.table(file=path, header=T, sep="\t", fill=T)

  if(is.null(valenceset)){
  }else{
    raw    <- raw[raw$compiled_studies==valenceset, ]
  }

  return(raw)
}
