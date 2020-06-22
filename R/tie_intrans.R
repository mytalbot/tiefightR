#' Calculate Intransitivity
#'
#' The \code{tie_intrans} function loads the raw data
#'
#' @param mydata input data frame
#' @param idcolumn name of the ID column in the input data
#' @param I1 name of the test image column in the input data
#' @param I2 name of the other (tested) items column in the input data
#' @param response name of the response variable
#'
#' @return intranscount intransitivity counts
#'
#' @export
#'


tie_intrans <- function(mydata=NULL, idcolumn="ID", I1="img1", I2="img2", response="pref_img1"){

  # make global settings local
  vIMG1       <- which(names(mydata)==I1)
  vIMG2       <- which(names(mydata)==I2)

  # prepare data
  mydata$img1 <- as.character(mydata[,vIMG1])
  mydata$img2 <- as.character(mydata[,vIMG2])

  resVar      <- which(names(mydata)==response)
  intIDcol    <- which(names(mydata)==idcolumn)

  uIDs   = unique( mydata[,intIDcol])
  uItems = unique( c(mydata[,vIMG1], mydata[,vIMG2]) )
  count  = 0

  #length(uIDs)
  #length(uItems)

  # calculate item intransitivity
  i1=1
  i2=2
  i3=3
  intranscount=0
  tripletcount=0

  for(thisID in uIDs)
  {
    thisData   = subset(mydata, mydata[names(mydata)==idcolumn]==thisID)
    lenDataset = length(thisData[[1]])

    for(i1 in 1:(length(uItems)-2))
    {
      for(i2 in (i1+1):(length(uItems)-1))
      {
        for(i3 in (i2+1):length(uItems))
        {
          firstI =  uItems[i1]
          secondI = uItems[i2]
          thirdI =  uItems[i3]

          for(count in 1:lenDataset)
          {
            #find i1 vs i2
            if(thisData[count,vIMG1]==firstI & thisData[count, vIMG2]==secondI) if(thisData[count,resVar]==1) AoverB=1 else AoverB=0 # A>B
            if(thisData[count,vIMG1]==secondI & thisData[count,vIMG2]==firstI) if(thisData[count, resVar]==0) AoverB=1 else AoverB=0 # A>B
            #find i2 vs i3
            if(thisData[count,vIMG1]==secondI & thisData[count,vIMG2]==thirdI) if(thisData[count, resVar]==1) BoverC=1 else BoverC=0 # A>B
            if(thisData[count,vIMG1]==thirdI & thisData[count, vIMG2]==secondI) if(thisData[count,resVar]==0) BoverC=1 else BoverC=0 # A>B
            #find i3 vs i1
            if(thisData[count,vIMG1]==thirdI & thisData[count, vIMG2]==firstI) if(thisData[count, resVar]==1) CoverA=1 else CoverA=0 # A>B
            if(thisData[count,vIMG1]==firstI & thisData[count, vIMG2]==thirdI) if(thisData[count, resVar]==0) CoverA=1 else CoverA=0 # A>B
          }
          #Invalid cases (A>B)&(B>C)&(C>A) OR (B>A)&(C>B)&(A>C)
          if(AoverB & BoverC & CoverA)
          {
            #print(paste(thisID,":",firstI,">",secondI,">",thirdI,">",firstI, "(clockwise A>B>C>A)"))
            intranscount=intranscount+1
          }
          if(!AoverB & !BoverC & !CoverA)
          {
            #print(paste(thisID,":",firstI,">",thirdI,">",secondI,">",firstI, "(counterclockwise A>C>B>A)"))
            intranscount=intranscount+1
          }
          tripletcount=tripletcount+1
        }
      }
    }
  }
    # print(paste(intranscount, "intransitive triplets from ", tripletcount, "total triplets"))
    return(intranscount)
  }

