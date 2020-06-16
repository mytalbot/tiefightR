

library(tiefightR)
library(ggpubr)
library(ggsci)

raw_mouse  <- tie_import(path = "C:/MHH Bleich/Aktuelles/Pfefferle Preference/data/mice_prefTest_20200416.txt" )

# how many randomization steps are needed to get the ties straight? --------
tie_rwalk  <- function(dat = raw_mouse, ord = items, prefLimit=50, setseed=FALSE, compstudy=NULL, default="HCl"){

  bin_mouse  <- tie_binarize(xdata      = raw_mouse,
                             compiled_studies = compstudy,
                             setseed    = setseed,
                             prefLimit  = prefLimit)

  mouse      <- tie_worth(xdata         = bin_mouse,
                          compstudy     = compstudy,
                          default       = default,
                          ordn          = ord)

  res        <-  mouse$worth
  return(res )
}

# randomize 150-times
set.seed(0)
R     = 150
items = c("m10MSac", "m5MSac", "HCl", "NaCl", "water")
h     <- replicate(R, tie_rwalk(dat = raw_mouse, ord=items,prefLimit=50,compstudy=1, setseed=FALSE))

# collate the arrays
n     <- names(h[,,1])
set   <- NULL
for(j in 1:dim(h)[3]){
  s   <- h[,,j]
  set <- rbind(set, s[names(s)==n  ])
}

# how many randomizations do we need for a stable mean?
# the former is dependent on the individual scales, so we also calculate the generalized distance (d)
means   <- NULL
SD      <- NULL
d       <- NULL
dsd     <- NULL
for(i in 2:R){
  means <- rbind(means, colMeans(set[1:i,]))
  SD    <- rbind(SD, apply(set[1:i,],2, sd ))
  d     <- rbind(d,   data.frame( d      = mean( dist( colMeans( set[1:i,]) ) ),
                                  dsd    = mean( dist( apply( set[1:i,],2,sd))),
                                  randos = i))
}
# plot(d$randos, d$d, type="l", lwd=2, ylab="Mean Euclidean distance", xlab="No. of randomizations")


plot_flow <- ggplot(d, aes(x=randos, y=d )) +
  geom_line(lwd=1.2)   +
  geom_pointrange(aes(ymin=d-dsd, ymax=d+dsd)) +
  ylab(expression(paste(Delta," mean Euclidean distance"))) +
  xlab("No. of randomizations")  +
  theme_minimal() +
  theme(text = element_text(size=11)) +
  labs(color="Preference limit") +
  theme(legend.key=element_blank(),legend.background=element_blank())
plot_flow <- plot_flow + scale_color_startrek()
plot_flow <- plot_flow + theme(legend.position="top")
plot_flow



plot_SD <- ggplot(d, aes(x=randos, y=dsd )) +
  geom_line(lwd=1.2)  +
  ylab(expression(paste("Fluctuation: Variance of ", Delta," distance"))) +
  xlab("No of. randomizations")  +
  theme_minimal() +
  theme(text = element_text(size=11)) +
  labs(color="Preference limit") +
  theme(legend.key=element_blank(),legend.background=element_blank())
plot_SD <- plot_SD + scale_color_startrek()
plot_SD <- plot_SD + theme(legend.position="top")
plot_SD








