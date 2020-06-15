# tiefightR construction sheet
library(tiefightR)

# Import continous data ---------------------------------------------------
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


### Use case: test Lake vs Cat, etc.
# You can repeat this analysis multiple times. The result will be different everý time because of
# the introduced ranom ties
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
# import non binary data and calculate worth values -----------------------
############################################################################
### Import raw data
raw_mouse  <- tie_import(path         = "C:/MHH Bleich/Aktuelles/Pfefferle Preference/data/mice_prefTest_20200416.txt" )

### binarize data (w/ constant seeding) - study/test_no 1
bin_mouse  <- tie_binarize(xdata      = raw_mouse,
                           SV         = "side",
                           RF         = "fluidType",
                           CF         = "combinationWith",
                           id         = "animalID",
                           RV         = "numOF_visits_with_Licks",
                           compiled_studies	 = 1,
                           setseed    = TRUE,
                           prefLimit  = 0.5)

### calculate worth value with the transformed values
mouse      <- tie_worth(xdata         = bin_mouse,
                        SV            = "side_img1",
                        RF            = "img1",
                        CF            = "img2",
                        id            = "ID",
                        RV            = "pref_img1",
                        showplot      = TRUE,
                        ymax          = 0.3,
                        intrans       = TRUE,
                        compstudy     = 1,
                        default       = "HCl",
                        ordn          = c("m10MSac", "m5MSac", "HCl", "NaCl", "water"),
                        r1            = NULL,
                        r2            = NULL)
mouse$worth
mouse$intrans











