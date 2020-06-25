### Model to simulate the demographic effects of maternal penning, Pred exclosure areas, and habitat restoration
### on woodland caribou, along with likely economic effectiveness
# By Sophie Gilbert and Rob Serrouya
# Version: 10/29/2018

### load libraries:
library(popbio)				# popbio has a lot of good tools for population dynamics
library(reshape2)			# this lets us reshape matrices and arrays (change the dimensions)
library(abind)				# this lets us bind arrays (like matrices) into 3-D stacks, like pages of a book



##### There are 3 components to the model: the habitat restoration model, the penning model, and the main model

#### Penning submodel: evaluate 2 types of pens, a maternal pen and a predator exclusion area

##### initial conditions:

tmax = 20									# how many years to run sim?
pop.start = 100								# initial population size (females >2 years old)
fem.numeric = T								# option. If this is selected "on" and next line is commented "off", numeric rather than percent
fem.numeric = F								# If this is not commented out, will use percent penned rather than numeric

fem.pen.num.vec = seq(0, 100, 1)			# vary number of females penned
fem.pen.prop.vec = seq(0,1, 0.01)			# vary proportion of females penned
fem.pen.prop.vec[36]=0.35					# fix weird R problem : (


############## Series of loops ot vary pen type, proportion of females penned, etc. ##################


for(q in 1:2){
	if(q==1){pen.type = "mat.pen"}else{pen.type = "pred.excl"}

if(pen.type=="mat.pen"){					# If evaluating the maternal pen...
pen.cap = 35;								# how many individual caribou can live in mat pen?
pen.cost1 = 500;							# cost in thousands to set up pen
pen.cost2 = 200;							# cost in thousands for shepherd
pen.cost3 = 250;							# cost in thousands to capture cows, monitor, survey, calf collar
pen.cost4 = 100;							# unexpected contingency/repair costs
pen.cost5 = 80								# costs of  project manager
}

if(pen.type=="pred.excl"){				# If evaluating the predator exclusion pen...
pen.cap = 35;								# how many individual caribou can live in big pen?
pen.cost1 = (77*24) + 20;					# cost in thousands to set up pen, & xtra cost of initial pred removal in yr1
pen.cost2 = 80;								# cost in thousands for removing predators annually
pen.cost3 = 500;								# cost in thousands for patrolling and repairing fence
pen.cost4 = 200;								# cost in thousands to capture cows, monitor, survey, calf collar
pen.cost5 = 100								# unexpected contingencies costs
}


## Caribou vital rates

if(pen.type=="mat.pen"){
c.surv1.capt = 0.9;							# calf survival rate when captive, 0-1 month
c.surv2.capt = 0.6;							# calf survival rate when captive, 1-12 months
c.surv.capt = c.surv1.capt*c.surv2.capt;	# calf survival rate when captive, annual
c.surv.wild = 0.163;							# calf survival rate in the wild, annual
f.surv.wild = 0.853;							# maternal survival when wild, annual
f.surv.capt = 0.903	;						# maternal survival when captive higher than wild
preg = 0.92;									# pregnancy rate, same for captive and wild
f.preg.capt = preg							# pregnancy rate, same for captive and wild
}


## Caribou vital rates, predator exclusion pen

if(pen.type=="pred.excl"){
c.surv1.capt = 0.9;							# calf survival rate when captive, 0-1 month
c.surv2.capt = 0.8;							# calf survival rate when captive, 1-12 months
c.surv.capt = c.surv1.capt*c.surv2.capt;	# calf survival rate when captive, annual
c.surv.wild = 0.163;							# calf survival rate in the wild, annual
f.surv.wild = 0.853;							# maternal survival when wild, annual
f.surv.capt = 0.95;							# maternal survival when captive higher than wild
preg = 0.92;									# pregnancy rate, same for captive and wild
f.preg.capt = preg							# pregnancy rate, same for captive and wild
}


############### Code to calculate


	for(k in 1:length(fem.pen.prop.vec)){															# loop through proportions of females penned
		fem.pen.prop = fem.pen.prop.vec[k]

		### get stable stage distribution for year 1 ###

		if(fem.numeric==T){fpen.prop = fem.pen.num/pop.start}else{fpen.prop=fem.pen.prop}			# females penned is either a rate or numeric
		surv.f = fpen.prop*f.surv.capt + (1-fpen.prop)*f.surv.wild									# mean female surv = weighted av. of pen/wild vitals
		preg.f = fpen.prop*f.preg.capt + (1-fpen.prop)*preg											# and for pregnancy
		surv.c = fpen.prop*c.surv.capt + (1-fpen.prop)*c.surv.wild


		A = matrix(c(	0, 		0, 		0, 		0.5*preg.f*surv.f,									# Fecundity of stages
   						surv.c, 0, 		0, 		0, 													# Survival of stage (age) 0-1
    					0, 		surv.f, 0, 		0, 														# Survival of stage (age) 1-2
    					0, 		0,		surv.f, surv.f), 												# Survival of stage (age) 2-3, and 3+
    						nrow=4, byrow=T)

		Stable.st = eigen.analysis(A)$stable.stage 													# extract stable stage distribution
		Nstart=matrix(c((Stable.st[1]/Stable.st[4])*pop.start, 										# number of females in each class
						(Stable.st[2]/Stable.st[4])*pop.start,
						(Stable.st[3]/Stable.st[4])*pop.start,										# so that number of adult (class 4 females)
						pop.start), ncol=1)															# is = to pop.start


		tmax=tmax																					# number of time steps
		N1 = N2 = Nstart																				# starting populations for time loop

		for(t in 1:tmax)	{																			# loop through time to project population
			fpen.prop=fem.pen.prop
			surv.f1 = fpen.prop*f.surv.capt + (1-fpen.prop)*f.surv.wild								# .f1 denotes vitals for pop that's partially penned
			preg.f1 = fpen.prop*f.preg.capt + (1-fpen.prop)*preg
			surv.c1 = fpen.prop*c.surv.capt + (1-fpen.prop)*c.surv.wild
			surv.f2 = f.surv.wild																		# .f2 denotes vitals for pop that's entirely wild
			preg.f2 = preg
			surv.c2 = c.surv.wild

   			A1 = matrix(c(	0, 		0, 		0, 		0.5*preg.f1*surv.f1,											# population with penning
    						surv.c1, 0, 		0, 		0,
    						0, 		surv.f1, 0, 		0,
    						0, 		0,		surv.f1, surv.f1),
    						nrow=4, byrow=T)

	   		A2 = matrix(c(	0, 		0, 		0, 		0.5*preg.f2*surv.f2,											# population without penning
	        					surv.c2, 0, 		0, 		0,
    						0, 		surv.f2, 0, 		0,
    						0, 		0,		surv.f2, surv.f2),
    						nrow=4, byrow=T)


    		pen.removed = A2%*%N1																		# performance of pen pop if pen removed, at  t
			N1 = A1%*%N1																				# project population (w/pen) to t
			N2 = A2%*%N2																				# project population (no pen) to t
			eigs.A1 = eigen.analysis(A1)																# eigen analysis of each population
			eigs.A2 = eigen.analysis(A2)
			pen.diff = N1 - pen.removed																	# demographic boost of the pen, in yr. t
			juv.from.pen.t = pen.diff[2,]																# additional juves from penning, time t
			adult.from.pen.t =  sum(pen.diff[3:4,])														# additional adults from penning, time t?
			rep.adult.pen = N1[4,]																		# how many total reproductive adults in penned pop
			rep.adult.nopen =N2[4,]																		# how many total rep. adults in wild pop
			tot.pen = sum(N1[,1])																		# total pop. size of penned pop
			tot.adult.in.pen = N1[4,1]*fpen.prop														# how many adult females in the pen?
			tot.nopen = sum(N2[,1])																		# total pop. size of wild pop
			new.bou.t = tot.pen-tot.nopen																# how many new bou made in time t?


			# Calculate costs of penning
			if(t==1){pens.avail= 0}else{pens.avail=pens.needed}											# how many pens exist at time t-1?
			pens.needed = ceiling(round(tot.adult.in.pen)/pen.cap)												# no partial pens allowed... current pen needs.
			new.pens = pens.needed-pens.avail
			num.pens = pens.avail + new.pens
			pens.cost.t = (pen.cost1*new.pens +															# cost to construct new pens
			pen.cost2*num.pens +																		# cost to maintain all pens
			pen.cost3*num.pens +
			pen.cost4*num.pens +
			pen.cost5*num.pens)/1000
			pens.cost.per.bou = pens.cost.t/new.bou.t													# how much will these pens cost per new bou?
			if(t==1){pens.cost.cum=pens.cost.t}else{pens.cost.cum=sum(pens.cost.t, Npop$pens.cost.t)}	# cumulative cost of penning
			if(t==1){cum.bou = new.bou.t}else{cum.bou =sum(new.bou.t, Npop$new.bou.t)}					# cumulative caribou produced
			pens.cost.cum.bou=pens.cost.cum/cum.bou														# cost per bou: cumulative cost/cum caribou pop diff

			Nt = data.frame(		lam.pen = eigs.A1$lambda1, lam.nopen = eigs.A2$lambda1, 					# store the data for time t
							N.pen = tot.pen, N.nopen = tot.nopen,
							new.bou.t = new.bou.t,
							pens.needed = pens.needed, pens.cost.t = pens.cost.t,
							pens.cost.per.bou = pens.cost.per.bou, pens.cost.cum = pens.cost.cum, pens.cost.cum.bou = pens.cost.cum.bou,
							rep.adult.pen = rep.adult.pen, rep.adult.nopen=rep.adult.nopen,
							juv.from.pen.t = juv.from.pen.t, adult.from.pen.t = adult.from.pen.t,
							s.c.pop.pen= surv.c1, s.c.pop.nopen=surv.c2,
							s.f.pop.pen = surv.f1, s.f.pop.nopen=surv.f2
							)

			if(t==1){Npop = Nt}else{Npop=rbind(Npop, Nt)}												# store the data for all t
				}																						# end time (years) loop

		Npop.r1 = data.frame(		lam.pen = eigs.A1$lambda1, lam.nopen = eigs.A2$lambda1, 				# make data for year 0 (start)
									N.pen = sum(Nstart), N.nopen = sum(Nstart),
									new.bou.t=0,
									pens.needed= 0, pens.cost.t = 0,
									pens.cost.per.bou = 0, pens.cost.cum = 0, pens.cost.cum.bou = 0,
									rep.adult.pen = Nstart[4,], rep.adult.nopen= Nstart[4,],
									juv.from.pen.t = 0, adult.from.pen.t = 0,
									s.c.pop.pen= surv.c1, s.c.pop.nopen=surv.c2,
									s.f.pop.pen = surv.f1, s.f.pop.nopen=surv.f2)

		Npop=rbind(Npop.r1, Npop)																		# add year 0 data to projection data

		if(k==1){Nstack = Npop}else{Nstack=abind(Nstack, Npop, along=3)}

			}																						# end proportion penned loop
if(pen.type=="mat.pen"){Nstack1 = Nstack}else{Nstack2 = Nstack}

}																							# end pen type loop






############ Plot results ################

hab.time = read.csv("~/Google Drive/Gilbert_Projects/Caribou_Canada/Penning Econ/Output_files/output.hab.time.csv")
hab.cost = read.csv("~/Google Drive/Gilbert_Projects/Caribou_Canada/Penning Econ/Output_files/output.hab.cost.csv")


quartz(width=6.5, height=2.5)
par(mar=c(4,4.1,4,2.9)+.1, mfrow=c(1,3))

ylim1= c(0, 450)
ylim2=c(0, 40)
ylim3 = c(0, 1)
xlab1 = "Years"
legend.y = 450


col1 = "goldenrod"
col2 = "darkorange3"
col3 = "red3"
col4 = "dodgerblue3"
col5 = "seagreen3"

### Panel A: time series of population sizes, maternal penning
plot(1:(tmax+1), data.frame(Nstack1[,,1])$rep.adult.nopen, ylab="Population size", xlab = xlab1, ylim=ylim1, type="l", lty=1, col = "black", las =1)
lines(1:(tmax+1), data.frame(Nstack1[,,which(fem.pen.prop.vec==0.35)])$rep.adult.pen, lty=2, col=col2)
lines(1:(tmax+1), data.frame(Nstack1[,,which(fem.pen.prop.vec==0.50)])$rep.adult.pen, lty=3, col = col3)
lines(1:(tmax+1), data.frame(Nstack1[,,which(fem.pen.prop.vec==0.75)])$rep.adult.pen, lty=4, col = col4)
#lines(1:(tmax+1), data.frame(Nstack1[,,which(fem.pen.prop.vec==1)])$rep.adult.pen, lty=5, col = col5)
legend(1, y=legend.y, legend = c("0% penned", "35% penned", "50% penned", "75% penned"), lty = c(1,2,3,4), cex= 0.8, box.col="transparent",
	col = c("black", col2, col3, col4))
mtext("a", at = -1, line = 2, font = 1)

### Panel B: time series of population sizes, predator exclusion penning
plot(1:(tmax+1), data.frame(Nstack2[,,1])$rep.adult.nopen, ylab="", xlab = xlab1, ylim=ylim1, type="l", lty=1, col = "black", las =1)
lines(1:(tmax+1), data.frame(Nstack2[,,which(fem.pen.prop.vec==0.35)])$rep.adult.pen, lty=2, col=col2)
lines(1:(tmax+1), data.frame(Nstack2[,,which(fem.pen.prop.vec==0.50)])$rep.adult.pen, lty=3, col = col3)
lines(1:(tmax+1), data.frame(Nstack2[,,which(fem.pen.prop.vec==0.75)])$rep.adult.pen, lty=4, col = col4)
#lines(1:(tmax+1), data.frame(Nstack2[,,which(fem.pen.prop.vec==1)])$rep.adult.pen, lty=5, col = col5)
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



lam.pen.vec1 = data.frame(pen.prop = fem.pen.prop.vec, pen.lam = c(Nstack1[1,1,]))
break.even1 = lam.pen.vec1$pen.pro[which(lam.pen.vec1$pen.lam>=1)]
break.even1 = break.even1[1]

lam.pen.vec2 = data.frame(pen.prop = fem.pen.prop.vec, pen.lam = c(Nstack2[1,1,]))
break.even2 = lam.pen.vec2$pen.pro[which(lam.pen.vec2$pen.lam>=1)]
break.even2 = break.even2[1]

pen1.35 <- round(data.frame(Nstack1[,,which(fem.pen.prop.vec==0.35)]), digits=2)
new1.bou.35 <- round(sum(pen1.35$rep.adult.pen[nrow(pen1.35)]-pen1.35$rep.adult.nopen[nrow(pen1.35)]), digits=0)
cost1.bou.35 <- round(sum(pen1.35$pens.cost.t)/new1.bou.35, digits=2)
cost1.lamb.35 <- sum(pen1.35$pens.cost.t)/(new1.bou.35/100)

pen2.35 <- round(data.frame(Nstack2[,,which(fem.pen.prop.vec==0.35)]), digits=2)
new2.bou.35 <- round(sum(pen2.35$rep.adult.pen[nrow(pen2.35)]-pen2.35$rep.adult.nopen[nrow(pen2.35)]), digits=0)
cost2.bou.35 <- round(sum(pen2.35$pens.cost.t)/new2.bou.35, digits=2)
cost2.lamb.35 <- sum(pen2.35$pens.cost.t)/(new2.bou.35/100)

pen1.even <- round(data.frame(Nstack1[,,which(fem.pen.prop.vec==break.even1)]), digits=2)			# 57% penned, mat pen
new1.bou.even <- round(sum(pen1.even$rep.adult.pen[nrow(pen1.even)]-pen1.even$rep.adult.nopen[nrow(pen1.even)]), digits=0)
cost1.bou.even <- round(sum(pen1.even$pens.cost.t)/new1.bou.even, digits=2)
cost1.lamb.even <- sum(pen1.even$pens.cost.t)/(new1.bou.even/100)

pen2.even <- round(data.frame(Nstack2[,,which(fem.pen.prop.vec==break.even1)]), digits=2)			# 57% penned, safe haven
new2.bou.even <- round(sum(pen2.even$rep.adult.pen[nrow(pen2.even)]-pen2.even$rep.adult.nopen[nrow(pen2.even)]), digits=0)
cost2.bou.even <- round(sum(pen2.even$pens.cost.t)/new2.bou.even, digits=2)
cost2.lamb.even <- sum(pen2.even$pens.cost.t)/(new2.bou.even/100)

hab.cost1 = hab.cost[hab.cost$Nstart==100, ]

caribou.cost = data.frame(
Method = c("Vitals base","Hab base","Mat pen, 35%", "*Mat pen, 57%", "*PEA., 35%", "PEA., 57%","Hab rest", "Hab deact"),
N.end = c(pen1.35$rep.adult.nopen[nrow(pen2.35)], hab.cost1$Nend.BA.Base, pen1.35$rep.adult.pen[nrow(pen1.35)], 100, 100, pen2.even$rep.adult.pen[nrow(pen1.35)], hab.cost1$Nend.BA.LRest, hab.cost1$Nend.BA.Deac),
N.new = c("--", "--", new1.bou.35, new1.bou.even, new2.bou.35, new2.bou.even, round(hab.cost1$Nend.BA.LRest-hab.cost1$Nend.BA.Base, digits=0), round(hab.cost1$Nend.BA.Deac-hab.cost1$Nend.BA.Base, digits=0)),
Cost.cum = c("--", "--", sum(pen1.35$pens.cost.t), sum(pen1.even$pens.cost.t), sum(pen2.35$pens.cost.t), sum(pen2.even$pens.cost.t), 175.5, 175.5),
Cost.bou = c("--", "--", cost1.bou.35, cost1.bou.even, cost2.bou.35, cost2.bou.even, 175.5/round(hab.cost1$Nend.BA.LRest-hab.cost1$Nend.BA.Base, digits=0), round(175.5/round(hab.cost1$Nend.BA.Deac-hab.cost1$Nend.BA.Base, digits=0), digits=1)))


write.table(caribou.cost, "~/Google Drive/Gilbert_Projects/Caribou_Canada/Penning Econ/Output_files/output.allcosts.csv", sep=",", row.names=F)


##### costs plot

barplot(rbind(as.numeric(as.character(caribou.cost$N.new[3:8])), as.numeric(as.character(caribou.cost$Cost.cum[3:8])), as.numeric(as.character(caribou.cost$Cost.bou[3:8]))*10), beside=T,
las=1, ylim=c(0, 250), col=c("black", "grey50", "red"),
names.arg=caribou.cost$Method[3:8],)
legend(x=1, y=250, legend=c("New caribou produced", "Cost, $CAD millions", "Cost per 10 caribou"), fill=c("black", "grey50", "red"), box.col="transparent")








################### Now sensitivity analysis for vital rates ####

# data to do sensitivity analysis on:

c.surv.capt.sen = expand.grid(seq(0.05, 1, 0.05), f.surv.wild, preg)
f.surv.capt.sen = expand.grid(c.surv.wild, seq(0.05, 1, 0.05), preg)
f.preg.capt.sen = expand.grid(c.surv.wild, f.surv.wild, seq(0.05, 1, 0.05))

sens.vecs= list(c.surv.capt.sen, f.surv.capt.sen, f.preg.capt.sen)			#list of all the sensitivities we want to evaluate
names(sens.vecs) = c("calf", "fem", "preg")


fem.pen.prop.vec = seq(0,1, 0.01)

#produces a list for each vital rate varied (in-pen calf survival, female survival, .s; .c; pregnancy, .p)
# p loop goes through each vital rate (3)
# j loop goes through each vital's sensitivity seuence (0.1 to 1, by 0.05, = 19 list elements)
# k loops through proportion of female papulation penned = 101 rows)
# within k loop, we produce 11 variables = 11 columns
# Each output, sen.c, sen.s, and sen.p, = 101 x 11 x 19 list

cex = 2.0

for(p in 1:length(sens.vecs)){												# loop through which vital rate varies (3 vitals)
	sens.dat = data.frame(sens.vecs[[p]])									# vital rate data for this loop
	vital = names(sens.vecs)[p]												# name of vital rate that varies

for(j in 1:nrow(sens.dat)){													# loop through the variations on the vital rate in question (sequence)

	c.surv.capt = sens.dat[j,1]												# assign value for calf survival in pen
	f.surv.capt = sens.dat[j,2]												# assign value for female survival in pen
	f.preg.capt = sens.dat[j,3]												# assign value for female pregancy in pen

	for(k in 1:length(fem.pen.prop.vec)){												# loop through diff. proportions of females penned

		fem.pen.prop = fem.pen.prop.vec[k]												# proportion of females penned

		### get stable stage distribution for year 1 ###

		if(fem.numeric==T){fpen.prop = fem.pen.num/pop.start}else{fpen.prop=fem.pen.prop}
		surv.f = fpen.prop*f.surv.capt + (1-fpen.prop)*f.surv.wild						# average female survival rate for pop
		preg.f = fpen.prop*f.preg.capt + (1-fpen.prop)*preg								# average female pregnancy rate for pop
		surv.c = ((fpen.prop*f.preg.capt/preg.f)*c.surv.capt) + 							# average calf surv rate for pop, depends on preg rate too
				 ((1-fpen.prop)*preg/preg.f*c.surv.wild)

		A.best = matrix(c(	0, 		0, 		0, 		0.5*f.preg.capt*f.surv.capt,		# Fertility of stages
   						c.surv.capt, 0, 		0, 		0, 									# Survival of stage (age) 0-1
    					0, 		f.surv.capt, 0, 		0, 									# Survival of stage (age) 1-2
    					0, 		0,		f.surv.capt, f.surv.capt), 						# Survival of stage (age) 2-3, and 3+
    					nrow=4, byrow=T)

																								# A: matrix for the average population
		A = matrix(c(	0, 		0, 		0, 		0.5*preg.f*surv.f,								# Fertility of stages
   						surv.c, 0, 		0, 		0, 												# Survival of stage (age) 0-1
    					0, 		surv.f, 0, 		0, 													# Survival of stage (age) 1-2
    					0, 		0,		surv.f, surv.f), 											# Survival of stage (age) 2-3, and 3+
    					nrow=4, byrow=T)

		if(fem.numeric==T){fpen.prop = fem.pen.num/N1[4,]}else{fpen.prop=fem.pen.prop}
			surv.f1 = fpen.prop*f.surv.capt + (1-fpen.prop)*f.surv.wild
			surv.f2 = f.surv.wild
			preg.f1 = fpen.prop*f.preg.capt + (1-fpen.prop)*preg
			preg.f2 = preg
			surv.c1 = fpen.prop*c.surv.capt + (1-fpen.prop)*c.surv.wild
			surv.c2 = c.surv.wild


   			A1 = matrix(c(	0, 		0, 		0, 		0.5*preg.f1*surv.f1,											# population with penning
    						surv.c1, 0, 		0, 		0,
    						0, 		surv.f1, 0, 		0,
    						0, 		0,		surv.f1, surv.f1),
    						nrow=4, byrow=T)

	   		A2 = matrix(c(	0, 		0, 		0, 		0.5*preg.f2*surv.f2,											# population without penning
	        					surv.c2, 0, 		0, 		0,
    						0, 		surv.f2, 0, 		0,
    						0, 		0,		surv.f2, surv.f2), 														# Survival of stages
    						nrow=4, byrow=T)

    			eigs.Abest = lambda(A.best)
    			eigs.A1 = lambda(A1)
			eigs.A2 = lambda(A2)

			Nt = data.frame(	lam.pen = eigs.A1,
							lam.nopen = eigs.A2,
							lam.allpenned = eigs.Abest,
							pen.prop = fem.pen.prop,
							f.surv.capt = f.surv.capt,
							f.preg.capt = f.preg.capt,
							c.sur.capt = c.surv.capt,
							s.c.pop.pen= surv.c1, s.c.pop.nopen=surv.c2,
							s.f.pop.pen = surv.f1, s.f.pop.nopen=surv.f2
							)

			if(k==1){Npop = Nt}else{Npop=rbind(Npop, Nt)}
			}																								# end proportion penned loop


if(j==1){Nstack = Npop}else{Nstack =abind(Nstack, Npop, along=3)	}
 	}

 	if(p==1){sen.c = Nstack}
 	if(p==2){sen.f = Nstack}
 	if(p==3){sen.p = Nstack}

 	}




	#sen.c					# sensitivity of calf survival:
	#sen.f					# sensitivity of female survival
	#sen.p					# sensitivity of pregnancy rate


# plot proportion penned needed to get lambda = 1 across a range of vital rates

col.fun = colorRampPalette(c("black","grey20", "grey40","white"))
col.lam = col.fun(10000)

lam.dat.c = sen.c[,1,]
min.lam = 0
max.lam = max(lam.dat.c)
lam.seq = seq(min.lam, max.lam, (max.lam-min.lam)/(length(col.lam)-1))


dat.plot = list(sen.f, sen.p, sen.c)
dim.vit = c(5,6,7)
name.vit = c("Fem survival", "Fem pregnancy", "Calf survival")
par(mfrow=c(1,3))


for(n in 1:3){
plot(seq(0.05,1, 0.05), seq(0.05, 1, 0.05), col="transparent", xlab=name.vit[n], ylab = "Proportion penned")

dat = dat.plot[[n]]

for(i in 1:dim(dat)[1]){
	dat.pen = dat[i,4,]			# pen proportions for vital rate
	dat.vit = dat[i,dim.vit[n],]	# vital rate varies
	dat.lam = dat[i,1,]			# lambda for penned population

	for(k in 1:length(dat.pen)){
	col.match = col.lam[findInterval(lam.seq, c(dat.lam[k]-0.0001, dat.lam[k]+0.0001))==1]
	col.plot = col.match[1]
	lam.mat = dat.lam[k]
	col=col.plot
	#col = "grey"
	if(lam.mat>=1.00){col="darkturquoise"}

	points(dat.vit[k], dat.pen[k], col =col, pch=15, cex=cex)
								}
						}
	if(n==1){abline(v=f.surv.wild, lwd=2, col="red")}
	if(n==2){abline(v=preg,  lwd=2, col="red")}
	if(n==3){abline(v=c.surv.wild, lwd=2, col="red")}
				}








