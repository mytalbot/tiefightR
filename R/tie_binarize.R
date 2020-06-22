#' Binarize continous data
#'
#' Uses \code{tie_import} output as input.
#'
#' @param xdata imported (binarized) data frame
#' @param SV name of the side variable
#' @param RF name of the reference fluid variable
#' @param CF name of the combination fluid variable
#' @param id subject IDs
#' @param RV name of the response variable
#' @param compiled_studies label of the compiled sub study (used for filtering)
#' @param setseed TRUE/FALSE for seeding
#' @param prefLimit preference limit for binarization threshold
#' @param datalabel universal study label for the binarized data
#' @param sidevar the name of the standardized side variable in the org data
#' @param refval the name of the standardized reference variable in the org data
#' @param oval the name of the standardized combination variable in the org data
#' @param aid  the name of the standardized animal id column in the org data
#' @param fldrnk the name of the standardizedresponse variable in the org data
#'
#' @import dplyr
#' @importFrom stats runif
#' @importFrom magrittr "%>%"
#'
#' @return tada
#'
#' @export
#'

tie_binarize <- function(xdata      = NULL,
                         SV         = "side",
                         RF         = "fluidType",
                         CF         = "combinationWith",
                         id         = "animalID",
                         RV         = "numOF_visits_with_Licks",
                         datalabel  = "binarized",
                         compiled_studies	 = 1,
                         setseed    = TRUE,
                         prefLimit  = 50,
                         sidevar    = "side",
                         refval     = "refValue",
                         oval       = "otherValue",
                         aid        = "animalID",
                         fldrnk     = "fluid_drunk"){

  colnames(xdata)[colnames(xdata)==SV] <- sidevar             #the name of your side variable
  colnames(xdata)[colnames(xdata)==RF] <- refval              #the name of your reference fluid variable
  colnames(xdata)[colnames(xdata)==CF] <- oval                #the name of your combination fluid variable
  colnames(xdata)[colnames(xdata)==id] <- aid                 #the name of your subject IDs variable
  colnames(xdata)[colnames(xdata)==RV] <- fldrnk              #the name of your response variable

  #create column called "check" for later use
  xdata$check=paste(xdata$refValue, xdata$otherValue, sep = "")

  #transform continious response to binary response (name should be 'binary')
  xdata$fluid_total <- 0
  # check if all entries come in pairs
  for (i in 1:nrow(xdata)) {
    if (xdata$side[i] == 'left') {
      if ((xdata$side[i+1] != 'right') || (xdata$animalID[i] != xdata$animalID[i+1]) || (xdata$refValue[i] != xdata$otherValue[i+1]))
        warning('found single entry in data, right entry is missing: ', as.character(xdata$date[i]), ' ', as.character(xdata$animalID[i]))
      else
        xdata$fluid_total[i] <- xdata$fluid_drunk[i] + xdata$fluid_drunk[i+1]
    }
    if (xdata$side[i] == 'right') {
      if ((xdata$side[i-1] != 'left') || (xdata$animalID[i] != xdata$animalID[i-1]) || (xdata$refValue[i] != xdata$otherValue[i-1]))
        warning('found single entry in data, left entry is missing: ', as.character(xdata$date[i]), ' ', as.character(xdata$animalID[i]))
      else
        xdata$fluid_total[i] <- xdata$fluid_drunk[i] + xdata$fluid_drunk[i-1]
    }
  }


  #set percentage 50% (more than 50%) to 99.9%
  prefLimit <- prefLimit

  #check for ties
  xdata$binaryNEW[(xdata$fluid_total * (prefLimit/100))       < xdata$fluid_drunk] <- 1
  xdata$binaryNEW[(xdata$fluid_total * (1 - (prefLimit/100))) > xdata$fluid_drunk] <- 0
  xdata$ties <- TRUE
  xdata$ties[!is.na(xdata$binaryNEW)] <- FALSE
  xx         <- xdata$ties

  #randomize answers for ties

  if(setseed==TRUE){
    set.seed(0) # Sets a seed here, so that the randomization is always the same
  }else{}

  for (i in 1:(nrow(xdata)/2)) {
    if (xdata$ties[(i*2)-1]) {
      tempRand                 <- round(runif(1, 0, 1))
      xdata$binaryNEW[(i*2)-1] <- tempRand
      xdata$binaryNEW[i*2]     <- (tempRand-1) * (-1)
    }
  }
  rm(i, tempRand)

  #to show ties
  xx=xdata[xdata$ties == TRUE,] #we have 9 ties (reponse num_of_visits_with likcks) combined in dataset 1 and 2


  # Choose data set
  levels(as.factor(xdata$test_no) )#1,2
  dataSet1 <- xdata %>%
    as_tibble() %>%
    filter(test_no	== compiled_studies	)

  # need to drop levels due to separating the datasets
  dataSet1 <- dataSet1 %>%
    mutate_at(vars(refValue,otherValue),droplevels) %>%
    rename("concat_test_name" = "check")


  # dat = your data, as a data.frame
  dat         = dataSet1
  dat$binResp = dat$binaryNEW                     #the name of your response variable

  # compile the new data set in the same manner as the binary human data
  new                  <- dat[, c("animalID","refValue","otherValue","side","binResp")]
  new$compiled_studies <- NULL
  new$compiled_studies <- compiled_studies

  new$compiled_studies <- as.factor(compiled_studies)
  new$data             <- NULL
  new$data             <- datalabel

  colnames(new)        <- c("ID", "img1", "img2", "side_img1", "pref_img1","compiled_studies","data")
  new                  <- new[,c("data","compiled_studies","ID","img1","img2","side_img1","pref_img1")]
  new$pref_img1        <- as.integer(new$pref_img1)

  new$img1             <- new %>% pull(img1)
  new$img2             <- new %>% pull(img2)

  new                  <- as.data.frame(new)
  return(new)
}
