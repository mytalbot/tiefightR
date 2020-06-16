# tiefightR test sheet
library(tiefightR)

############################################################################
# Import continous data (human exmpl.)
############################################################################
raw     <- tie_import(path     = "C:/MHH Bleich/Aktuelles/Pfefferle Preference/data/HumanPrefBinaer_20200414.txt" )

human   <- tie_worth(xdata     = raw,
                     SV        = "side_img1",
                     RF        = "img1",
                     CF        = "img2",
                     id        = "ID",
                     RV        = "pref_img1",
                     showplot  = TRUE,
                     ymax      = 0.5,
                     intrans   = TRUE, # change to false so save some calculation time
                     compstudy = "LagreValenceRange_SpringSchool",
                     default   = "War",
                     ordn      = c("Cat", "Crow", "Doctor", "Frustrated", "Lake", "War", "Fire"),
                     r1        = NULL, # test
                     r2        = NULL) # against
human$worth
human$intrans


# Use case: test "Lake vs Cat", etc. ----------------------------------------
# You can repeat this analysis multiple times. The result will be different everý time because of
# the introduced random ties for the remaining combinations
human_test <- tie_worth(xdata = raw,
                     showplot  = TRUE,
                     compstudy = "LagreValenceRange_SpringSchool",
                     default   = "War",
                     ordn      = c("Cat", "Crow", "Doctor", "Frustrated", "Lake", "War", "Fire"),
                     r1        = "Lake", # change this
                     r2        = "Cat")  # change this / to multiple combis
human_test$worth
human_test$intrans



############################################################################
# Import non binary data and calculate worth values
############################################################################
### Import raw data (exmpl: mouse data)
raw_mouse  <- tie_import(path         = "C:/MHH Bleich/Aktuelles/Pfefferle Preference/data/mice_prefTest_20200416.txt" )

### binarize data (w/ constant seeding) - study/test_no 1
# NOTE: the binarized set is calculated with a constant seeding.
# You can change this w/ setseed = FALSE!
bin_mouse  <- tie_binarize(xdata      = raw_mouse,
                           SV         = "side",
                           RF         = "fluidType",
                           CF         = "combinationWith",
                           id         = "animalID",
                           RV         = "numOF_visits_with_Licks",
                           compiled_studies	 = 1,
                           setseed    = TRUE,
                           prefLimit  = 0.5)

# calculate worth values w/ the binarized data
mouse      <- tie_worth(xdata         = bin_mouse,
                        SV            = "side_img1",
                        RF            = "img1",
                        CF            = "img2",
                        id            = "ID",
                        RV            = "pref_img1",
                        showplot      = TRUE,
                        ymin          = 0.16,
                        ymax          = 0.24,
                        intrans       = TRUE,
                        compstudy     = 1,
                        default       = "HCl",
                        ordn          = c("m10MSac", "m5MSac", "HCl", "NaCl", "water"),
                        r1            = NULL,
                        r2            = NULL)
mouse$worth
mouse$intrans


# Use case: test the other "set" no 2 -------------------------------------
bin_mouse_2<- tie_binarize(xdata      = raw_mouse,
                           SV         = "side",
                           RF         = "fluidType",
                           CF         = "combinationWith",
                           id         = "animalID",
                           RV         = "numOF_visits_with_Licks",
                           compiled_studies	 = 2, # this was changed
                           setseed    = TRUE,
                           prefLimit  = 0.5)

mouse2      <- tie_worth(xdata         = bin_mouse_2,
                        SV            = "side_img1",
                        RF            = "img1",
                        CF            = "img2",
                        id            = "ID",
                        RV            = "pref_img1",
                        showplot      = TRUE,
                        ymin          = 0,
                        ymax          = 0.6,
                        intrans       = TRUE,
                        compstudy     = 2,       # this was changed
                        default       = "HCl",
                        ordn          = c("AlmondMilk", "AppleJuice", "HCl", "quinine", "water"),
                        r1            = NULL,
                        r2            = NULL)
mouse2$worth
mouse2$intrans


############################################################################
# Simulation - Human
############################################################################
### Check the number of available CPU cores on your machine.
### Use k-2 later in the simulation or you'll regret it...
### Don't do this analysis on a single core machine.
k <- tie_cores()
k

# load some data for the simulation ---------------------------------------
raw     <- tie_import(path = "C:/MHH Bleich/Aktuelles/Pfefferle Preference/data/HumanPrefBinaer_20200414.txt" )
ord     <- c("Cat", "Crow", "Doctor", "Frustrated", "Lake", "War", "Fire")

# Let's simulate ----------------------------------------------------------
set.seed(123)

testme     <- "War"
sim_humans <- tie_sim(xdata     = raw,
                      R         = 2,
                      SV        = "side_img1",
                      RF        = "img1",
                      CF        = "img2",
                      id        = "ID",
                      RV        = "pref_img1",
                      intrans   = TRUE,
                      compstudy = "LagreValenceRange_SpringSchool",
                      default   = "War",
                      cpus      = 6,
                      ord       = ord,
                      v1        = testme)

tie_simrep(res  = sim_humans,
           v1   = testme)


############################################################################
# Use Case: individual test scenario --------------------------------------
############################################################################
# Observe the position and mean intransitivity after R randomizations
raw     <- tie_import(path = "C:/MHH Bleich/Aktuelles/Pfefferle Preference/data/HumanPrefBinaer_20200414.txt" )
ord     <- c("Cat", "Crow", "Doctor", "Frustrated", "Lake", "War", "Fire")

mytest  <- tie_test(xdata     = raw,
                    R         = 2,
                    cpus      = 2,
                    SV        = "side_img1",
                    RF        = "img1",
                    CF        = "img2",
                    id        = "ID",
                    RV        = "pref_img1",
                    intrans   = TRUE,
                    compstudy = "LagreValenceRange_SpringSchool",
                    default   = "War",
                    ord       = ord,
                    seed      = TRUE,
                    testme    = "Lake",                # Change this
                    against   = c("Cat","Crow","War")) # Change items and watch intrans move
mytest




