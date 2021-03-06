#' Main preference function
#'
#' The \code{tie_import} function prepares binary and continuous data form import into the tiefightR analysis. The user has
#' to specicy the names of the input columns (if they deviate from the default values in the function argument list). The function
#' randomizes the response variable for any non chosen item test combination and reports the worth values.
#'
#' @param xdata imported (binarized) data frame
#' @param esti worth estimator (default, "worth", alt: "estimator")
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
#' @param ymin minimum y-scale of the worth plot
#' @param ordn item category order
#' @param default default item in worth value estimation (usually the lowest worth value)
#'
#' @importFrom reshape2 dcast
#' @importFrom graphics plot
#' @importFrom stats as.formula poisson
#'
#' @import dplyr
#' @import prefmod
#' @importFrom magrittr "%>%"
#' @import gnm
#'
#' @return Exports the results of the worth value calculation, including the GNM analysis.
#'
#' @export
#'
#'
tie_worth <- function(xdata     = NULL,
                      esti      = "worth",
                      RF        = "img1",
                      CF        = "img2",
                      id        = "ID",
                      RV        = "pref_img1",
                      default   = NULL,
                      showplot  = FALSE,
                      intrans   = FALSE,
                      compstudy = NULL,
                      ordn      = NULL,
                      r1        = NULL,
                      r2        = NULL,
                      ymin      = 0,
                      ymax      = 0.5){


  ### Select data and randomize the rest
  if(is.null(r1)+is.null(r2)!=2){

    dat <- xdata[xdata$compiled_studies==compstudy, ]

    ### entferne ein Picture komplett
    raus       <- r1
    # modify data
    mod        <- dat
    mod        <- mod[!(mod[[RF]] %in% raus),] #mod[!(mod$img1 %in% raus),]
    mod        <- mod[!(mod[[CF]] %in% raus),] #mod[!(mod$img2 %in% raus),]
    #sum(mod$img1==raus)
    #sum(mod$img2==raus)

    ### Packe eine Kombination des entfernten Pictures wieder rein & randomize the rest
    # of the combinations with that entfernte picture
    add      <- r2

    # remaining combos with added picture
    #restOrd  <- ord[!(ord %in% add)]
    #combis   <- expand.grid(rep(list(0:1), length(restOrd)))
    #colnames(combis) <- restOrd

    ### Add the removed picture & a combination plus randomize the remainers
    S <- NULL
    for(i in 1:length(r2)){
      s1 <- subset(dat, (dat[names(dat)==RF]==r1    & dat[names(dat)==CF]==r2[i])) # subset(dat, (img1==r1    & img2==r2[i]))
      s2 <- subset(dat, (dat[names(dat)==RF]==r2[i] & dat[names(dat)==CF]==r1))    # subset(dat, (img1==r2[i] & img2==r1))
      s  <- rbind(s1,s2)
      S  <- rbind(S,s)
    }

    # these stay untouched
    X     <- S

    # these get randomized
    i1    <- dat[dat[[RF]] %in% raus & !(dat[[CF]] %in% add),]
    i2    <- dat[dat[[CF]] %in% raus & !(dat[[RF]] %in% add),]
    inter <- rbind(i1,i2)

    inter[, RV] <- sample(c(0,1),
                          replace = TRUE,
                          size    = dim(inter)[1])

    xdata <- rbind(mod,X,inter)
  }else{
    xdata <- xdata[xdata$compiled_studies==compstudy, ]
  }



  # calculate intransitivity?
  if(intrans==F){
  }else{
    itr <- tie_intrans(mydata=xdata)
  }


  # continue analysis
 # colnames(xdata)[colnames(xdata)==SV] <- "side"        #the name of your side variable
  colnames(xdata)[colnames(xdata)==RF] <- "refValue"    #the name of your reference fluid variable
  colnames(xdata)[colnames(xdata)==CF] <- "otherValue"  #the name of your combination fluid variable
  colnames(xdata)[colnames(xdata)==id] <- "animalID"    #the name of your subject IDs variable
  colnames(xdata)[colnames(xdata)==RV] <- "binResp"     #the name of your response variable


  #create column called "check" for later use
  xdata$check      <- paste(xdata$refValue, xdata$otherValue, sep = "")

  dataSet1         <- xdata %>%
    as_tibble() %>%
    filter(compiled_studies==compstudy)

  # drop the remaining levels
  dat              <- dataSet1 %>%
    mutate_at(vars(refValue,otherValue),droplevels) %>%
    rename("concat_test_name" = "check")

  # get the number of items
  len_ord          <- length(ordn)

  # this sets up how the data.frame columns should be structured
  p1_num <- NULL
  p2_num <- NULL
  for (i in 1:(len_ord-1)) {
    p1_num <- c(p1_num, 1:i)
    p2_num <- c(p2_num, rep(i+1, i))
  }

  # the number of unique pairings in the order they should be
  pairings               <- paste0(ordn[p1_num], ordn[p2_num])

  # but they may be written the other way in the data
  pairings_switch        <- paste0(ordn[p2_num], ordn[p1_num])

  # so we fix that using this
  for(i in 1:length(pairings)){
    dat$concat_test_name <- gsub(paste0(pairings[i],"|",pairings_switch[i]),
                                 pairings[i], dat$concat_test_name)
  }

  #tbl <- table(dat$concat_test_name,dat$side) #to check if everything is correctly assigned


  ##################################################################################################
  # STOP !!!
  ##################################################################################################
  #code which fluid was chosen
  dat$chosenfluid <- ifelse(dat$binResp==1, as.character(dat$refValue), as.character(dat$otherValue))
  dat$chosenOther <- NA

  # convert the item chosen column
  for(i in 1:nrow(dat)) {
    # is the word in chosenfluid the same word as the first
    # item in the pairing?
    t_or_f             <- substr(dat$concat_test_name[i],1, nchar(dat$chosenfluid[i])) == dat$chosenfluid[i]
    # 1 if preferred, -1 if the other item was chosen
    dat$chosenOther[i] <- ifelse(t_or_f, 1, -1)
  }

  # make concat_test_name a factor and order it by pairings
  dat$concat_test_name <- factor(dat$concat_test_name, levels = pairings)

  # order it correctly
  dat                  <- with(dat, dat[order(concat_test_name),])

  #give subject integer tag
  dat$Trial            <- rep(1:table(dat$concat_test_name)[1], choose(len_ord, 2))

  #for creating a trial number that is grouped by category and side
  #dat                  <- dat %>%
  #  group_by(concat_test_name,side) %>%
  #  mutate(Trial_withSide = 1:n())

  dat                  <- dat %>%
    group_by(concat_test_name)

  ### run model
  excl_ord             <- ordn[!ordn==default]

  # model for plotting (with side removed)
  mdat_DS1             <- dcast(dat, formula = Trial ~ concat_test_name, fun.aggregate = sum,
                                value.var = "chosenOther")[,-1]

  model_DS1            <- llbt.design(mdat_DS1, nitems = len_ord, objnames = ordn)

  Formula2             <- as.formula(paste("y~", paste(excl_ord, collapse="+")))
  h1_DS1               <- gnm(Formula2, ia=T, #ia set to true= dependent
                              data = model_DS1, family =  poisson) #NOTE: War is in the intercept

  resNoSide            <- summary(h1_DS1)


  #generate and plot worth values
  hwor_DS1 <- llbt.worth(h1_DS1, outmat = esti)
  worth    <- hwor_DS1

  if(showplot==TRUE){
    plot(hwor_DS1, ylim=c(ymin,ymax), ylab="worth value")
  }else{}

  if(intrans==F){
    list(dat=dat, res=resNoSide, modelout=hwor_DS1, worth=worth)
  }else{
    list(dat=dat, res=resNoSide, modelout=hwor_DS1, worth=worth, intrans=itr, Ipct=itr/dim(dat)[1]*100)
  }

}



