#' Simulation report
#'
#' The \code{tie_simrep} function prepares a frequency table and plots for analysing the simulation output. Can also be used
#' for saving the output to file when a path is provided.
#'
#' @param res result or output from the simulation
#' @param v1 test variable
#' @param path path to where the report shall be stored (inluding plots)
#'
#' @import ggplot2
#' @import viridis
#' @importFrom stats sd dist TukeyHSD aov
#' @importFrom grDevices dev.off tiff
#'
#' @return A frequency table for item positions during the simulation; Tukey's HSD Test for positions; Position Bubble Plot
#'
#' @export
#'


tie_simrep <- function(res=NULL, v1=NULL, path=NULL){

  # Diagnostics
  ff  <- res[res$picture==v1, ]

    # do the Bubble Plot
  set.seed(123)
  L  <- length(unique(res$picture))
  p1 <- ggplot(ff, aes(x=factor(pos), y=intrans)) +
    geom_jitter( aes(size = noVars, fill = noVars), shape = 21, alpha = 0.7, width=0.2) +
    scale_fill_viridis_c(guide = "legend") +
    scale_size_continuous(range = c(1, L-1)) +
    xlab("Position") +
    ylab("Mean intransitivity")   +
    labs(subtitle = v1,
         fill     = "Number\n of items",
         size     = "Number\n of items") +
    theme_bw() +
    theme(axis.title.x = element_text(hjust= 0.5)) +
    theme(axis.title.y = element_text(hjust= 0.5))
  p1 <- p1 + scale_x_discrete(limits = factor(1:L))

  # output depends on path == TRUE/FALSE
  if(is.null(path)){
    Ftable <- table(res[res$picture==v1,"pos"])
    fit    <- aov(intrans ~ factor(pos), data=ff)
    print(p1)

    return(Ftable=Ftable)

  }else{
    # Export: Frequency table
    sink(paste(path,v1," ", "F-Table.txt",sep=""))
    print("Position Table: Simulated Frequency Distribution")
    print(table(res[res$picture==v1,"pos"]))
    sink()

    # Export: Tukey HSD position test
    sink( paste(path,v1," ", "Tukey Position Test.txt",sep="") )
    fit <- aov(intrans ~ factor(pos), data=ff)
    print(TukeyHSD(fit))
    sink()

    # Export: Bubble Plot
    tiff(filename=paste(path,v1," ", "Position Plot.tiff",sep=""), width = 1600, height = 1300,
         units = "px", pointsize = 4,res = 300, compression = "lzw")
    print(p1)
    dev.off()

  }
}
