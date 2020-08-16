## demography params from csv
sw <- read.csv("inst/extdata/DemogrWild.csv")
sm <- read.csv("inst/extdata/DemogrManaged.csv")
fp <- 0.92

## make combined object with all the params
s <- sm
s$PopID <- sw$PopID[match(s$Subpopulation, sw$Subpopulation)]
s$Scw <- sw$Scw[match(s$Subpopulation, sw$Subpopulation)]
s$Saw <- sw$Saw[match(s$Subpopulation, sw$Subpopulation)]
s$Fm <- fp
s$Fw <- fp
s$PenType <- c(MP="mat.pen", PE="pred.excl", MR="moose.red", WR="wolf.red",
               CB="cons.breed")[as.character(s$RecoveryAction)]
s <- s[,c("RecoveryAction", "PenType", "PopID", "Subpopulation",
           "Scw","Scm","Saw","Sam","Fw", "Fm")]
rownames(s) <- paste0(s$PopID, "_", s$RecoveryAction)
s$RecoveryAction <- as.character(s$RecoveryAction)
s$PopID <- as.character(s$PopID)
s$Subpopulation <- as.character(s$Subpopulation)
dput(s)
# now copy this over to .get_demography (df object inside)
