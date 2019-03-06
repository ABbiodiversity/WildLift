## original

### Model to simulate the demographic effects of maternal penning, habitat penning, and habitat restoration
### on woodland caribou, along with likely economic effectiveness
# By Sophie Gilbert and Rob Serrouya
# Version: 10/29/2015

### load libraries:
library(popbio)    # popbio has a lot of good tools for population dynamics
library(reshape2)   # this lets us reshape matrices and arrays (change the dimensions)
library(abind)    # this lets us bind arrays (like matrices) into 3-D stacks, like pages of a book


##### There are 3 components to the model: the habitat restoration model, the penning model, and the main model

#### Penning submodel: evaluate 2 types of pens, a maternal pen and a predator exclusion pen

##### initial conditions:


# how many years to run sim?
tmax = 20
# initial population size (females >2 years old)
pop.start = 100
# option. If this is selected "on" and next line is commented "off",
# numeric rather than percent
#fem.numeric = FALSE

# vary number of females penned
#fem.pen.num.vec <- seq(0, 100, 1)
# vary proportion of females penned
fem.pen.prop.vec <- round(seq(0, 1, 0.01), 2)
#fem.pen.prop.vec[36]=0.35     # fix weird R problem : (


############## Series of loops ot vary pen type, proportion of females penned, etc. ##################

for (pen.type in c("mat.pen", "pred.excl")) {

if (pen.type=="mat.pen") {
    pen.cap <- 35       # how many individual caribou can live in mat pen?
    pen.cost1 <- 500    # cost in thousands to set up pen
    pen.cost2 <- 200    # cost in thousands for shepherd
    pen.cost3 <- 250    # cost in thousands to capture cows, monitor, survey, calf collar
    pen.cost4 <- 100    # unexpected contingency/repair costs
    pen.cost5 <- 80     # costs of  project manager
}
if (pen.type=="pred.excl") {
    pen.cap <- 35        # how many individual caribou can live in big pen?
    pen.cost1 <- (77*24) + 20 # cost in thousands to set up pen, & xtra cost of initial pred removal in yr1
    pen.cost2 <- 80     # cost in thousands for removing predators annually
    pen.cost3 <- 500    # cost in thousands for patrolling and repairing fence
    pen.cost4 <- 200    # cost in thousands to capture cows, monitor, survey, calf collar
    pen.cost5 <- 100    # unexpected contingencies costs
}


## Caribou vital rates
if (pen.type=="mat.pen") {
    c.surv1.capt <- 0.9       # calf survival rate when captive, 0-1 month
    c.surv2.capt <- 0.6       # calf survival rate when captive, 1-12 months
    c.surv.capt <- c.surv1.capt*c.surv2.capt # calf survival rate when captive, annual
    c.surv.wild <- 0.163       # calf survival rate in the wild, annual
    f.surv.wild <- 0.853       # maternal survival when wild, annual
    f.surv.capt <- 0.903      # maternal survival when captive higher than wild
    preg <- 0.92         # pregnancy rate, same for captive and wild
    f.preg.capt <- preg       # pregnancy rate, same for captive and wild
}
if (pen.type=="pred.excl") {
    c.surv1.capt <- 0.9       # calf survival rate when captive, 0-1 month
    c.surv2.capt <- 0.8       # calf survival rate when captive, 1-12 months
    c.surv.capt <- c.surv1.capt*c.surv2.capt # calf survival rate when captive, annual
    c.surv.wild <- 0.163       # calf survival rate in the wild, annual
    f.surv.wild <- 0.853       # maternal survival when wild, annual
    f.surv.capt <- 0.95       # maternal survival when captive higher than wild
    preg <- 0.92         # pregnancy rate, same for captive and wild
    f.preg.capt <- preg       # pregnancy rate, same for captive and wild
}


############### Code to calculate


# loop through proportions of females penned
for(k in 1:length(fem.pen.prop.vec)) {
    fem.pen.prop <- fem.pen.prop.vec[k]
    ### get stable stage distribution for year 1 ###
    # females penned is either a rate or numeric
#    if (fem.numeric){
#        fpen.prop <- fem.pen.num/pop.start
#    }else{
        fpen.prop <- fem.pen.prop
#    }
    # mean female surv = weighted av. of pen/wild vitals
    surv.f <- fpen.prop*f.surv.capt + (1-fpen.prop)*f.surv.wild
    # and for pregnancy
    preg.f <- fpen.prop*f.preg.capt + (1-fpen.prop)*preg
    surv.c <- fpen.prop*c.surv.capt + (1-fpen.prop)*c.surv.wild
    A <- matrix(c(
        0,      0,      0,      0.5*preg.f*surv.f,# Fecundity of stages
        surv.c, 0,      0,      0,      # Survival of stage (age) 0-1
        0,      surv.f, 0,      0,      # Survival of stage (age) 1-2
        0,      0,      surv.f, surv.f),# Survival of stage (age) 2-3, and 3+
        nrow=4, byrow=TRUE)
    # extract stable stage distribution
    Stable.st <- eigen.analysis(A)$stable.stage
    # assign correct # of animals to each age class
    Nstart <- matrix(pop.start*Stable.st, ncol=1)
  #tmax=tmax            # number of time steps
    # starting populations for time loop
    N1 <- N2 <- Nstart
    # loop through time to project population
    for(i in seq_len(tmax)) {
        fpen.prop <- fem.pen.prop
        # .f1 denotes vitals for pop that's partially penned
        surv.f1 <- fpen.prop*f.surv.capt + (1-fpen.prop)*f.surv.wild
        preg.f1 <- fpen.prop*f.preg.capt + (1-fpen.prop)*preg
        surv.c1 <- fpen.prop*c.surv.capt + (1-fpen.prop)*c.surv.wild
        # .f2 denotes vitals for pop that's entirely wild
        surv.f2 <- f.surv.wild
        preg.f2 <- preg
        surv.c2 <- c.surv.wild
        A1 <- matrix(c(
            0,   0,   0,   0.5*preg.f1*surv.f1,  # population with penning
            surv.c1, 0,   0,   0,
            0,   surv.f1, 0,   0,
            0,   0,  surv.f1, surv.f1),
            nrow=4, byrow=TRUE)

        A2 <- matrix(c(
            0,   0,   0,   0.5*preg.f2*surv.f2, # population without penning
            surv.c2, 0,   0,   0,
            0,   surv.f2, 0,   0,
            0,   0,  surv.f2, surv.f2),
            nrow=4, byrow=TRUE)
        # performance of pen pop if pen removed, at  t
        pen.removed <- A2 %*% N1
        # project population (w/pen) to t
        N1 <- A1%*%N1
        # project population (no pen) to t
        N2 <- A2%*%N2
        # eigen analysis of each population
        eigs.A1 <- eigen.analysis(A1)
        eigs.A2 <- eigen.analysis(A2)
        # demographic boost of the pen, in yr. t
        pen.diff <- N1 - pen.removed
        # additional juves from penning, time t
        juv.from.pen.t <- pen.diff[2,]
        # additional adults from penning, time t?
        adult.from.pen.t <-  sum(pen.diff[3:4,])
        # how many total reproductive adults in penned pop
        rep.adult.pen <- N1[4,]
        # how many total rep. adults in wild pop
        rep.adult.nopen <- N2[4,]
        # total pop. size of penned pop
        tot.pen <- sum(N1[,1])
        # total pop. size of wild pop
        tot.nopen <- sum(N2[,1])
        # how many new bou made in time t?
        new.bou.t <- tot.pen-tot.nopen

        #### Calculate costs of penning
        # how many pens exist at time t-1?
        if (i==1) {
            pens.avail <- 0
        } else {
            pens.avail <- pens.needed
        }
        # no partial pens allowed... current pen needs.
        pens.needed <- ceiling(round(tot.pen)/pen.cap)
        new.pens <- pens.needed-pens.avail
        num.pens <- pens.avail + new.pens
        pens.cost.t <- (pen.cost1*new.pens + # cost to construct new pens
            pen.cost2*num.pens +            # cost to maintain all pens
            pen.cost3*num.pens +
            pen.cost4*num.pens +
            pen.cost5*num.pens)/1000
        # how much will these pens cost per new bou?
        pens.cost.per.bou <- pens.cost.t/new.bou.t
        # cumulative cost of penning
        if (i==1) {
            pens.cost.cum=pens.cost.t
        } else {
            pens.cost.cum=sum(pens.cost.t, Npop$pens.cost.t)
        }
        # cumulative caribou produced
        if (i==1) {
            cum.bou = new.bou.t
        } else {
            cum.bou =sum(new.bou.t, Npop$new.bou.t)
        }
        # cost per bou: cumulative cost/cum caribou pop diff
        pens.cost.cum.bou <- pens.cost.cum/cum.bou

        Nt <- data.frame(
            lam.pen = eigs.A1$lambda1,
            lam.nopen = eigs.A2$lambda1, # store the data for time t
            N.pen = tot.pen,
            N.nopen = tot.nopen,
            new.bou.t = new.bou.t,
            pens.needed = pens.needed,
            pens.cost.t = pens.cost.t,
            pens.cost.per.bou = pens.cost.per.bou,
            pens.cost.cum = pens.cost.cum,
            pens.cost.cum.bou = pens.cost.cum.bou,
            rep.adult.pen = rep.adult.pen,
            rep.adult.nopen=rep.adult.nopen,
            juv.from.pen.t = juv.from.pen.t,
            adult.from.pen.t = adult.from.pen.t,
            s.c.pop.pen= surv.c1,
            s.c.pop.nopen=surv.c2,
            s.f.pop.pen = surv.f1,
            s.f.pop.nopen=surv.f2)
        # store the data for all t
        if (i==1) {
            Npop = Nt
        } else {
            Npop=rbind(Npop, Nt)
        }
    } # end time (years) loop

    Npop.r1 <- data.frame(
        lam.pen = eigs.A1$lambda1,
        lam.nopen = eigs.A2$lambda1, # make data for year 0 (start)
        N.pen = sum(Nstart),
        N.nopen = sum(Nstart),
        new.bou.t=0,
        pens.needed= 0,
        pens.cost.t = 0,
        pens.cost.per.bou = 0,
        pens.cost.cum = 0,
        pens.cost.cum.bou = 0,
        rep.adult.pen = Nstart[4,],
        rep.adult.nopen= Nstart[4,],
        juv.from.pen.t = 0,
        adult.from.pen.t = 0,
        s.c.pop.pen= surv.c1,
        s.c.pop.nopen=surv.c2,
        s.f.pop.pen = surv.f1,
        s.f.pop.nopen=surv.f2)
    # add year 0 data to projection data
    Npop <- rbind(Npop.r1, Npop)
    if (k==1) {
        Nstack <- Npop
    } else {
        Nstack <- abind(Nstack, Npop, along=3)
    }

}  # end proportion penned loop
if(pen.type=="mat.pen"){
    Nstack1 <- Nstack
} else {
    Nstack2 <- Nstack
}

}                       # end pen type loop




############ Plot results ################

hab.time = read.csv("inst/extdata/output.hab.time.csv")
hab.cost = read.csv("inst/extdata/output.hab.cost.csv")


#quartz(width=6.5, height=2.5)
par(mar=c(4,4.1,4,2.9)+.1, mfrow=c(1,3))

ylim1= c(0, 550)
ylim2=c(0, 40)
ylim3 = c(0, 1)
xlab1 = "Years"
legend.y = 550


col1 = "goldenrod"
col2 = "darkorange3"
col3 = "red3"
col4 = "dodgerblue3"
col5 = "seagreen3"

### Panel A: time series of population sizes, maternal penning
plot(1:(tmax+1), data.frame(Nstack1[,,1])$N.nopen, ylab="Population size", xlab = xlab1, ylim=ylim1, type="l", lty=1, col = "black", las =1)
lines(1:(tmax+1), data.frame(Nstack1[,,which(fem.pen.prop.vec==0.35)])$N.pen, lty=2, col=col2)
lines(1:(tmax+1), data.frame(Nstack1[,,which(fem.pen.prop.vec==0.50)])$N.pen, lty=3, col = col3)
lines(1:(tmax+1), data.frame(Nstack1[,,which(fem.pen.prop.vec==0.75)])$N.pen, lty=4, col = col4)
#lines(1:(tmax+1), data.frame(Nstack1[,,which(fem.pen.prop.vec==1)])$N.pen, lty=5, col = col5)
legend(1, y=legend.y, legend = c("0% penned", "35% penned", "50% penned", "75% penned"), lty = c(1,2,3,4), cex= 0.8, box.col="transparent",
 col = c("black", col2, col3, col4))
mtext("a", at = -1, line = 2, font = 1)

### Panel B: time series of population sizes, predator exclusion penning
plot(1:(tmax+1), data.frame(Nstack2[,,1])$N.nopen, ylab="", xlab = xlab1, ylim=ylim1, type="l", lty=1, col = "black", las =1)
lines(1:(tmax+1), data.frame(Nstack2[,,which(fem.pen.prop.vec==0.35)])$N.pen, lty=2, col=col2)
lines(1:(tmax+1), data.frame(Nstack2[,,which(fem.pen.prop.vec==0.50)])$N.pen, lty=3, col = col3)
lines(1:(tmax+1), data.frame(Nstack2[,,which(fem.pen.prop.vec==0.75)])$N.pen, lty=4, col = col4)
#lines(1:(tmax+1), data.frame(Nstack2[,,which(fem.pen.prop.vec==1)])$N.pen, lty=5, col = col5)
mtext("b", at = -1, line = 2, font = 1)

### Panel C: time series of population sizes, habitat restoration
plot(1:20, hab.time$cum.lam.BA.Deac*100, col="darkred",type="l", xlab="Years", ylab="", ylim = ylim1, lty=2, las=1)
lines(1:20, hab.time$cum.lam.BA.Base*100, col="black", lty=1)
lines(1:20, hab.time$cum.lam.BA.LRest*100, col="darkred", lty=3)
legend(1, y = legend.y, legend=c("Baseline", "Restoration", "Deactivation"),
box.col="transparent", col=c("black", "darkred", "darkred"),
lty = c(1,2,3), cex= 0.8)
mtext("c", at = -1, line = 2, font = 1)



### Calculate some costs for different penning proportions, for a table


lam.pen.vec1 = data.frame(
    pen.prop = fem.pen.prop.vec,
    pen.lam = c(Nstack1[1,1,]))
break.even1 = lam.pen.vec1$pen.pro[which(lam.pen.vec1$pen.lam>=1)][1]

lam.pen.vec2 = data.frame(
    pen.prop = fem.pen.prop.vec,
    pen.lam = c(Nstack2[1,1,]))
break.even2 = lam.pen.vec2$pen.pro[which(lam.pen.vec2$pen.lam>=1)][1]

pen1.35 <- round(data.frame(Nstack1[,,which(fem.pen.prop.vec==0.35)]), digits=2)
new1.bou.35 <- pen1.35$N.pen[nrow(pen1.35)]-pen1.35$N.nopen[nrow(pen1.35)]
cost1.bou.35 <- round(sum(pen1.35$pens.cost.t)/new1.bou.35, digits=2)
cost1.lamb.35 <- sum(pen1.35$pens.cost.t)/(new1.bou.35/100)

pen2.35 <- round(data.frame(Nstack2[,,which(fem.pen.prop.vec==0.35)]), digits=2)
new2.bou.35 <- pen2.35$N.pen[nrow(pen2.35)]-pen2.35$N.nopen[nrow(pen2.35)]
cost2.bou.35 <- round(sum(pen2.35$pens.cost.t)/new2.bou.35, digits=2)
cost2.lamb.35 <- sum(pen2.35$pens.cost.t)/(new2.bou.35/100)

pen1.even <- round(data.frame(Nstack1[,,which(fem.pen.prop.vec==break.even1)]), digits=2)   # 57% penned, mat pen
new1.bou.even <- pen1.even$N.pen[nrow(pen1.even)]-pen1.even$N.nopen[nrow(pen1.even)]
cost1.bou.even <- round(sum(pen1.even$pens.cost.t)/new1.bou.even, digits=2)
cost1.lamb.even <- sum(pen1.even$pens.cost.t)/(new1.bou.even/100)

pen2.even <- round(data.frame(Nstack2[,,which(fem.pen.prop.vec==break.even1)]), digits=2)   # 57% penned, safe haven
new2.bou.even <- pen2.even$N.pen[nrow(pen2.even)]-pen2.even$N.nopen[nrow(pen2.even)]
cost2.bou.even <- round(sum(pen2.even$pens.cost.t)/new2.bou.even, digits=2)
cost2.lamb.even <- sum(pen2.even$pens.cost.t)/(new2.bou.even/100)

hab.cost = round(hab.cost[hab.cost$Nstart==100, ], digits=2)

## no habitat version
caribou.cost.nohab = data.frame(
Method = c("Vitals base","Mat pen, 35%", "*Mat pen, 57%",
           "*Safe hav., 35%", "Safe hav., 57%"),
N.end = c(pen1.35$N.nopen[nrow(pen2.35)],
          pen1.35$N.pen[nrow(pen1.35)], 100, 100,
          pen2.even$N.pen[nrow(pen1.35)]),
N.new = c("--", new1.bou.35, new1.bou.even, new2.bou.35, new2.bou.even),
Cost.cum = c("--", sum(pen1.35$pens.cost.t), sum(pen1.even$pens.cost.t),
             sum(pen2.35$pens.cost.t), sum(pen2.even$pens.cost.t)),
Cost.bou = c("--", cost1.bou.35, cost1.bou.even, cost2.bou.35,
             cost2.bou.even))

caribou.cost = data.frame(
Method = c("Vitals base","Hab base","Mat pen, 35%", "*Mat pen, 57%",
           "*Safe hav., 35%", "Safe hav., 57%","Hab rest", "Hab deact"),
N.end = c(pen1.35$N.nopen[nrow(pen2.35)], hab.cost$Nend.BA.Base,
          pen1.35$N.pen[nrow(pen1.35)], 100, 100, pen2.even$N.pen[nrow(pen1.35)],
          hab.cost$Nend.BA.LRest, hab.cost$Nend.BA.Deac),
N.new = c("--", "--", new1.bou.35, new1.bou.even, new2.bou.35, new2.bou.even,
          hab.cost$Nend.BA.LRest-hab.cost$Nend.BA.Base,
          hab.cost$Nend.BA.Deac-hab.cost$Nend.BA.Base),
Cost.cum = c("--", "--", sum(pen1.35$pens.cost.t), sum(pen1.even$pens.cost.t),
             sum(pen2.35$pens.cost.t), sum(pen2.even$pens.cost.t),
             175.5, 175.5),
Cost.bou = c("--", "--", cost1.bou.35, cost1.bou.even, cost2.bou.35,
             cost2.bou.even, hab.cost$Diff.BA.LRest.tEnd,
             hab.cost$Diff.BA.Deac.tEnd))


write.table(caribou.cost, "/Users/sophiegilbert/Google Drive/Gilbert_Projects/Caribou_Canada/Penning Econ/Output_files/output.allcosts.csv", sep=",", row.names=F)


##### costs plot

barplot(rbind(as.numeric(as.character(caribou.cost$N.new[3:8])), as.numeric(as.character(caribou.cost$Cost.cum[3:8])), as.numeric(as.character(caribou.cost$Cost.bou[3:8]))*10), beside=T,
las=1, ylim=c(0, 250), col=c("black", "grey50", "red"),
names.arg=caribou.cost$Method[3:8],)
legend(x=1, y=250, legend=c("New caribou produced", "Cost, $CAD millions", "Cost per 10 caribou"), fill=c("black", "grey50", "red"), box.col="transparent")

