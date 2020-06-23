#' Import Function
#'
#' The \code{tie_import} function loads the raw data as a data frame. This function can be skipped when one of the three internal
#' data sets (human, mouse, rhesus) are used. For a user who wants to import own data this function is a good start. Make sure that
#' the imported data has at least the following information: data, subset (even if none is there), SV (side variable, left/right),
#' RF (reference fluid/item), CF (combination fluid), id (animal id), RV (response variable)).
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
