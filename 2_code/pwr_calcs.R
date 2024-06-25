#Power calculations for EHF experiments
#
library(here)
library(foreign)
library(modelsummary)
library(pwr)

#
wrps<-read.csv(here("0_data", "FreemanMedoff94Survey","wrps1","wrps1.csv"))

bl.us<-sum(wrps$q41a==1, na.rm=T)/sum(!is.na(wrps$q41a))
bl.us<-mean(wrps$q41b==1, na.rm=T)

pwr.2p.test(h=ES.h(p1 =bl.us + .05 , p2 = bl.us ),
	sig.level = 0.05, power = .8)

pwr.t.test(n = 600, sig.level = 0.05, power=.8)



pwr.t.test(n = 600, sig.level = 0.05, power=.8)


pwr.t.test(d = .3/.87, sig.level = 0.05, power=.8)


pwr.2p.test(h=ES.h(p1 = .157, p2 = 0.25 ),  #from THD experiment
	sig.level = 0.05, power = .8)




rm(list=ls())
#install.packages("randomizr")
library(randomizr)    # randomizr package for complete random assignment

possible.ns <- seq(from=200, to=2000, by=25)
power.atleastone <- rep(NA, length(possible.ns))
power.bothtreatments <- rep(NA, length(possible.ns))
power.fullranking <- rep(NA, length(possible.ns))
alpha <- 0.1  #(one-tailed test at .05 level)
sims <- 100
#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]
  p.T1vsC <- rep(NA, sims)
  p.T2vsC <- rep(NA, sims)
  p.T2vsT1 <- rep(NA, sims)
  c.T1vsC <- rep(NA, sims)
  c.T2vsC <- rep(NA, sims)
  c.T2vsT1 <- rep(NA, sims)
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rbinom(n=N, prob = 0.25, size =1)
    tau_1 <- .157
    tau_2 <- .25 -.66*(.25-.157)
    Y1 <- rbinom(n=N, prob = tau_1, size =1)
    Y2 <- rbinom(n=N, prob = tau_2, size =1)
    Z.sim <- complete_ra(N=N, num_arms=3)
    Y.sim <- Y0*(Z.sim=="T3") + Y1*(Z.sim=="T1") + Y2*(Z.sim=="T2")
    frame.sim <- data.frame(Y.sim, Z.sim)
    fit.T1vsC.sim <- glm(Y.sim ~ Z.sim=="T1", data=subset(frame.sim, Z.sim!="T2"), family = binomial)
    fit.T2vsC.sim <- glm(Y.sim ~ Z.sim=="T2", data=subset(frame.sim, Z.sim!="T1"), family = binomial)
    fit.T2vsT1.sim <- glm(Y.sim ~ Z.sim=="T2", data=subset(frame.sim, Z.sim!="T3"), family = binomial)
    
    ### Need to capture coefficients and pvalues (one-tailed tests, so signs are important)
    c.T1vsC[i] <- summary(fit.T1vsC.sim)$coefficients[2,1]
    c.T2vsC[i] <- summary(fit.T2vsC.sim)$coefficients[2,1]
    c.T2vsT1[i] <- summary(fit.T2vsT1.sim)$coefficients[2,1]
    p.T1vsC[i] <- summary(fit.T1vsC.sim)$coefficients[2,4]
    p.T2vsC[i] <- summary(fit.T2vsC.sim)$coefficients[2,4]
    p.T2vsT1[i] <- summary(fit.T2vsT1.sim)$coefficients[2,4]
  }
  power.atleastone[j] <- mean(c.T1vsC<0 & c.T2vsC<0 & (p.T1vsC < alpha/2 | p.T2vsC < alpha/2))
  power.bothtreatments[j] <- mean(c.T1vsC<0 & c.T2vsC<0 & p.T1vsC < alpha/2 & p.T2vsC < alpha/2)
  power.fullranking[j] <- mean(c.T1vsC<0 & c.T2vsC<0 & c.T2vsT1 > 0 & p.T1vsC < alpha/2 & p.T2vsT1 < alpha/2)
  print(j)
}

plot(possible.ns, power.atleastone, ylim=c(0,1))
points(possible.ns, power.bothtreatments, col="red")
points(possible.ns, power.fullranking, col="blue")

plot(power.atleastone ~possible.ns, ylim=c(0,1), type="n", ylab = "power",
	main = "Power curve for Bernoulli outcome and 2 treatment arms",
	xlab="N")
lines(possible.ns, predict(loess(power.atleastone ~ possible.ns, span=0.5)), lwd=2)
lines(possible.ns, predict(loess(power.bothtreatments ~ possible.ns, span=0.5)),
	col = "red", lwd=2)
#lines(possible.ns, predict(loess(power.fullranking ~ possible.ns, span=0.5)),
#		col = "slateblue", lwd=2)
abline(h=.8, lty=2)
legend("bottomright", legend = c("At least one", "Both"), 
	col=c("black", "red"), lty=1, lwd=2)
