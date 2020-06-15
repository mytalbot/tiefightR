#' Calculate Intransitivity
#'
#' The \code{tie_intrans} function loads the raw data
#'
#' @param mydata input data frame
#'
#' @return intranscount intransitivity counts
#'
#' @export
#'


tie_intrans <- function(mydata=NULL){

  # prepare data
  mydata$img1 <- as.character(mydata$img1 )
  mydata$img2 <- as.character(mydata$img2 )

  uIDs   = unique( mydata[,3])
  uItems = unique( c(mydata[,4], mydata[,5]) )
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
    thisData   = subset(mydata, ID==thisID)
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
            if(thisData[count,4]==firstI & thisData[count, 5]==secondI) if(thisData[count,7]==1) AoverB=1 else AoverB=0 # A>B
            if(thisData[count,4]==secondI & thisData[count,5]==firstI) if(thisData[count, 7]==0) AoverB=1 else AoverB=0 # A>B
            #find i2 vs i3
            if(thisData[count,4]==secondI & thisData[count,5]==thirdI) if(thisData[count, 7]==1) BoverC=1 else BoverC=0 # A>B
            if(thisData[count,4]==thirdI & thisData[count, 5]==secondI) if(thisData[count,7]==0) BoverC=1 else BoverC=0 # A>B
            #find i3 vs i1
            if(thisData[count,4]==thirdI & thisData[count, 5]==firstI) if(thisData[count, 7]==1) CoverA=1 else CoverA=0 # A>B
            if(thisData[count,4]==firstI & thisData[count, 5]==thirdI) if(thisData[count, 7]==0) CoverA=1 else CoverA=0 # A>B
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
