#rm(list = ls(all= T)) #clears environment - don't run because I am actually pulling things in from the CMR_2024 and transparent.R code

# This file uses outputs from the "Agapostemon_simple.R" output for plotting at the end
library(plotrix)
library(RMark)
MarkPath="~/Documents/Coding_manuscript"

setwd("~/Documents/Coding_manuscript")
source("transparent.R")

# import capture histories and keep the leading zeroes
dat<-read.csv("captures_for_Mark 03.01.2024.csv", colClasses = c("character", "factor", "factor", "real", "integer")) 
# create a table of capture histories
table(dat$ch)
summary(dat)
# summarize capture histories for each patch type at each site
bees = dat[,1:3]
bees.processed <- process.data(bees, model = "CJS", groups = c("pot", "site"), begin.time = 0, time.intervals = dat$int[1:15])

# create design dataframe for MARK analysis
bees.processed$group.covariates
bees.ddl <- make.design.data(bees.processed)
bees.ddl

# compile % impervious surface values for each patch (100m centered at the patch) and add to design dataframe
# this seems like there should be a better way to do it, but I'm following an example online
# https://www.montana.edu/rotella/documents/502/lab05Rmark.html  (thanks, Jay Rotella!)
#Site Imp_park    Imp_G    Imp_N  Imp_mid
#1  TUF 44.47775 45.69342 35.47667 40.99312
#2  BIK 74.30458 74.09439 75.46075 74.69607
#3  SAC 39.13071 41.08356 42.98422 38.16981
#4  MCM 60.79926 61.53920 56.09997 58.55867
#5  POW 48.74737 49.75448 51.78936 51.25506
#6  WAL 93.01502 93.44576 93.54654 93.79086
#7  LIN 54.99569 53.48956 52.00804 52.96444
#8  WIN 24.47131 21.97830 18.91047 20.52606


bees.ddl$p$imperv = 74.09439 # value for BIK, G
bees.ddl$p$imperv[bees.ddl$p$pot == "N"] = 75.46075  # value for BIK, N
bees.ddl$p$imperv[bees.ddl$p$site == "LIN" & bees.ddl$p$pot == "G"] = 53.48956 
bees.ddl$p$imperv[bees.ddl$p$site == "LIN" & bees.ddl$p$pot == "N"] = 52.00804 
bees.ddl$p$imperv[bees.ddl$p$site == "MCM" & bees.ddl$p$pot == "G"] = 61.53920 
bees.ddl$p$imperv[bees.ddl$p$site == "MCM" & bees.ddl$p$pot == "N"] = 56.09997 
bees.ddl$p$imperv[bees.ddl$p$site == "POW" & bees.ddl$p$pot == "G"] = 49.75448 
bees.ddl$p$imperv[bees.ddl$p$site == "POW" & bees.ddl$p$pot == "N"] = 51.78936 
bees.ddl$p$imperv[bees.ddl$p$site == "SAC" & bees.ddl$p$pot == "G"] = 41.08356 
bees.ddl$p$imperv[bees.ddl$p$site == "SAC" & bees.ddl$p$pot == "N"] = 42.98422 
bees.ddl$p$imperv[bees.ddl$p$site == "TUF" & bees.ddl$p$pot == "G"] = 45.69342 
bees.ddl$p$imperv[bees.ddl$p$site == "TUF" & bees.ddl$p$pot == "N"] = 35.47667 
bees.ddl$p$imperv[bees.ddl$p$site == "WAL" & bees.ddl$p$pot == "G"] = 93.44576 
bees.ddl$p$imperv[bees.ddl$p$site == "WAL" & bees.ddl$p$pot == "N"] = 93.54654 

bees.ddl$Phi$imperv = 74.09439 # value for BIK, G
bees.ddl$Phi$imperv[bees.ddl$Phi$pot == "N"] = 75.46075  # value for BIK, N
bees.ddl$Phi$imperv[bees.ddl$Phi$site == "LIN" & bees.ddl$Phi$pot == "G"] = 53.48956 
bees.ddl$Phi$imperv[bees.ddl$Phi$site == "LIN" & bees.ddl$Phi$pot == "N"] = 52.00804 
bees.ddl$Phi$imperv[bees.ddl$Phi$site == "MCM" & bees.ddl$Phi$pot == "G"] = 61.53920 
bees.ddl$Phi$imperv[bees.ddl$Phi$site == "MCM" & bees.ddl$Phi$pot == "N"] = 56.09997 
bees.ddl$Phi$imperv[bees.ddl$Phi$site == "POW" & bees.ddl$Phi$pot == "G"] = 49.75448 
bees.ddl$Phi$imperv[bees.ddl$Phi$site == "POW" & bees.ddl$Phi$pot == "N"] = 51.78936 
bees.ddl$Phi$imperv[bees.ddl$Phi$site == "SAC" & bees.ddl$Phi$pot == "G"] = 41.08356 
bees.ddl$Phi$imperv[bees.ddl$Phi$site == "SAC" & bees.ddl$Phi$pot == "N"] = 42.98422 
bees.ddl$Phi$imperv[bees.ddl$Phi$site == "TUF" & bees.ddl$Phi$pot == "G"] = 45.69342 
bees.ddl$Phi$imperv[bees.ddl$Phi$site == "TUF" & bees.ddl$Phi$pot == "N"] = 35.47667 
bees.ddl$Phi$imperv[bees.ddl$Phi$site == "WAL" & bees.ddl$Phi$pot == "G"] = 93.44576 
bees.ddl$Phi$imperv[bees.ddl$Phi$site == "WAL" & bees.ddl$Phi$pot == "N"] = 93.54654 

bees.ddl


# the next lines of code define possible predictors of survival and capture probability
Phi.dot = list(formula = ~1)
Phi.pot = list(formula = ~pot) # patch type (garden or non-garden)
Phi.site = list(formula = ~site) 
Phi.imp = list(formula = ~imperv) # % impervious surface
Phi.both = list(formula = ~pot+site)
Phi.act = list(formula = ~pot*site)
Phi.both2 = list(formula = ~pot+imperv)
Phi.act2 = list(formula = ~pot*imperv)

p.dot = list(formula = ~1)
p.pot = list(formula = ~pot)
p.site = list(formula = ~site)
p.imp = list(formula = ~imperv)
p.both = list(formula = ~pot+site)
p.act = list(formula = ~pot*site)
p.both2 = list(formula = ~pot+imperv)
p.act2 = list(formula = ~pot*imperv)

# this code creates all possible combinations of possible predictors of survival and capture probability
bees.model.list = create.model.list("CJS")

# set 'silent = F' to print out all the models as they are run
bees.results = mark.wrapper(bees.model.list, data = bees.processed, ddl = bees.ddl, silent = T) 

bees.results # amazingly, the winning model has an interaction between pot type and impervious surface.  It also has constant survival.  

# model coefficients on a (logit) link function scale
bees.results$Phi.dot.p.act2$results$beta
# reference group is gardens, non garden pots get the coefficient
# SO with:
#                 estimate        se       lcl        ucl
#p:(Intercept)    1.2220754 1.1936402 -1.1174594  3.5616102
#p:potN          -3.1333794 1.3893711 -5.8565469 -0.4102119
#p:imperv        -0.0569165 0.0238882 -0.1037374 -0.0100955
#p:potN:imperv    0.0794841 0.0268538  0.0268506  0.1321175
# at low impervious surface, non-garden pots have lower capture probability than garden pots (potN coefficient is negative)
# capture probability _declines_ weakly with impervious surface in garden pots (p:imperv coefficient is negative, and we need to remember this applies only to the reference group)
# capture probability _increases_ with impervious surface in non-garden pots (p:potN:imperv is positive and its absolute value is greater than p:imperv, remembering that the coefficient for garden pots is the sum of the two slopes, i.e., -0.057 + 0.079 = 0.022)

bees.results$Phi.pot.p.act2$results$beta # 2nd best model is similar but has slightly lower apparent survival in non-garden than garden pots

# effects parameterization for plotting?
p.act2b = list(formula = ~0 + pot + pot:imperv)

bees.Phi.dot.p.act2b = mark(bees.processed,bees.ddl, model.parameters = list(Phi = Phi.dot, p = p.act2b))
bees.Phi.dot.p.act2b$results$beta # these are slopes and intercepts
bees.Phi.dot.p.act2b$links # and it is a logit link

# getting means for each patch-type group
p.act2c = list(formula = ~0 + pot)
bees.Phi.dot.p.act2c = mark(bees.processed,bees.ddl, model.parameters = list(Phi = Phi.dot, p = p.act2c))
bees.Phi.dot.p.act2c$results$beta 
bees.Phi.dot.p.act2c$results$real 

# second-best model - main effects and slopes
Phi.pot2 = list(formula = ~0 + pot)
bees.Phi.pot2.p.act2b = mark(bees.processed,bees.ddl, model.parameters = list(Phi = Phi.pot2, p = p.act2b))
bees.Phi.pot2.p.act2b$results$real # back-transformed values of apparent survival
bees.Phi.pot2.p.act2b$results$beta # slopes of capture probabilities


###############################################
# site-specific estimates for plotting
# plot of effects on capture probability
###############################################
bees.Phi.dot.p.allpots = mark(bees.processed,bees.ddl, model.parameters = list(Phi = Phi.dot, p = p.act))
bees.Phi.dot.p.allpots$results$real # hey it worked again!
sitevals = data.frame(bees.Phi.dot.p.allpots$results$real)
rownames(sitevals)
sitevals$pot.type = substr(rownames(sitevals), start = 4, stop = 4)
sitevals$Site = substr(rownames(sitevals), start = 5, stop = 7)
sitevals
sitevals2 = merge(sitevals, AGVI.sum[,1:2], by = "Site", all.x = T, all.y = T)
names(AGVI.num.imp)[2] = "pot.type"
sitevals2 = merge(sitevals2, AGVI.num.imp[,c(1,2,10)])
sitevals2$mycols = "darkorchid"
sitevals2$mycols[sitevals2$pot.type == "N"] = "cornsilk3"
sitevals2$mypch = 24
sitevals2$mypch[sitevals2$pot.type == "N"] = 25

#detour to calculate regression confidence intervals from RMark output
#again there might be a better way than doing the math by hand
vcov = bees.Phi.dot.p.act2b$results$beta.vcv
coefs = bees.Phi.dot.p.act2b$results$beta

# this part is easy - just use the intercept and slope to get the expected values
Imps = seq(35, 93, 0.1)
predsG = plogis(coefs[2,1]+coefs[4,1]*Imps)
predsN = plogis(coefs[3,1]+coefs[5,1]*Imps)
# now, I'm calculating the sampling variance (SE^2) of the expected values, using 
# https://en.wikipedia.org/wiki/Variance#Weighted_sum_of_variables
varG = vcov[2,2]+vcov[4,4]*(Imps^2)+2*Imps*vcov[2,4]
predsG.low = plogis(coefs[2,1]+coefs[4,1]*Imps - 1.4*sqrt(varG))
predsG.upp = plogis(coefs[2,1]+coefs[4,1]*Imps + 1.4*sqrt(varG))

varN = vcov[3,3]+vcov[5,5]*(Imps^2)+2*Imps*vcov[3,5]
predsN.low = plogis(coefs[3,1]+coefs[5,1]*Imps - 1.4*sqrt(varN))
predsN.upp = plogis(coefs[3,1]+coefs[5,1]*Imps + 1.4*sqrt(varN))


plot(Imps, predsG, col = "darkorchid4", type = "l", ylim = c(0,1), xlab = "", ylab = "", lwd = 3)
points(Imps, predsN, col = "cornsilk4", type = "l", lwd = 3)
mtext(side = 1, line = 2, "% impervious surface")
mtext(side = 2, line = 2, "capture probability")
polygon(c(Imps, rev(Imps)), c((predsG.upp), rev((predsG.low))), col = mytranspG, border = NA)
polygon(c(Imps, rev(Imps)), c((predsN.upp), rev((predsN.low))), col = mytranspN, border = NA)

with(sitevals2[sitevals2$pot.type == "G",], plotCI(jitter(Imp_vals), estimate, ui = ucl, li = lcl, pch = 24, pt.bg = mycols, add = T))
with(sitevals2[sitevals2$pot.type == "N",], plotCI(jitter(Imp_vals), estimate, ui = ucl, li = lcl, pch = 25, pt.bg = mycols, add = T))
#with(sitevals2, plotCI(jitter(Imp_vals), estimate, ui = 0.1, li = 0.1, pch = mypch, pt.bg = mycols, add = T))

legend("topleft", pch = c(24,25), pt.bg = c("darkorchid", "cornsilk3"), legend = c("Garden", "Non-garden"), cex = 0.8, pt.cex = 1)     

###############################################
# site-specific estimates for plotting
# plot of effects on apparent survival
###############################################
bees.Phi.allpots.p.imp = mark(bees.processed,bees.ddl, model.parameters = list(Phi = Phi.both, p = p.act2))
bees.Phi.allpots.p.imp$results$real # hey it worked again!
sitevals.phi = data.frame(bees.Phi.allpots.p.imp$results$real)[1:14,]
rownames(sitevals.phi)
sitevals.phi$pot.type = substr(rownames(sitevals.phi), start = 6, stop = 6)
sitevals.phi$Site = substr(rownames(sitevals.phi), start = 7, stop = 9)
sitevals.phi
sitevals.phi2 = merge(sitevals.phi, AGVI.sum[,1:2], by = "Site", all.x = T, all.y = T)
#names(AGVI.num.imp)[2] = "pot.type"
sitevals.phi2 = merge(sitevals.phi2, AGVI.num.imp[,c(1,2,10)])
sitevals.phi2$mycols = "darkorchid"
sitevals.phi2$mycols[sitevals.phi2$pot.type == "N"] = "cornsilk3"
sitevals.phi2$mypch = 24
sitevals.phi2$mypch[sitevals.phi2$pot.type == "N"] = 25

sitevals.phi2

#detour to calculate regression confidence intervals from RMark output
#again there might be a better way than doing the math by hand
Phi.both_a = list(formula = ~0 + pot + imperv)
bees.Phi.act2.p.act2 = mark(bees.processed,bees.ddl, model.parameters = list(Phi = Phi.both_a, p = p.act2))
vcov2 = bees.Phi.act2.p.act2$results$beta.vcv
coefs2 = bees.Phi.act2.p.act2$results$beta
coefs2
vcov2
# this part is easy - just use the intercept and slope to get the expected values
Imps = seq(35, 93, 0.1)
predsG2 = plogis(coefs2[1,1]+coefs2[3,1]*Imps)
predsN2 = plogis(coefs2[2,1]+coefs2[3,1]*Imps)
# now, I'm calculating the sampling variance (SE^2) of the expected values, using 
# https://en.wikipedia.org/wiki/Variance#Weighted_sum_of_variables
varG2 = vcov2[1,1]+vcov2[3,3]*(Imps^2)+2*Imps*vcov2[1,3]
# 1.4 for 84% CIs ( recommended by Payton et al. 2003)
predsG.low2 = plogis(coefs2[1,1]+coefs2[3,1]*Imps - 1.4*sqrt(varG2))
predsG.upp2 = plogis(coefs2[1,1]+coefs2[3,1]*Imps + 1.4*sqrt(varG2))

varN2 = vcov2[2,2]+vcov2[3,3]*(Imps^2)+2*Imps*vcov2[2,3]
predsN.low2 = plogis(coefs2[2,1]+coefs2[3,1]*Imps - 1.4*sqrt(varG2))
predsN.upp2 = plogis(coefs2[2,1]+coefs2[3,1]*Imps + 1.4*sqrt(varG2))


plot(Imps, predsG2, col = "darkorchid4", type = "l", ylim = c(0,1), xlab = "", ylab = "", lwd = 3)
points(Imps, predsN2, col = "cornsilk4", type = "l", lwd = 3)
mtext(side = 1, line = 2, "% impervious surface")
mtext(side = 2, line = 2, "apparent survival")
polygon(c(Imps, rev(Imps)), c((predsG.upp2), rev((predsG.low2))), col = mytranspG, border = NA)
polygon(c(Imps, rev(Imps)), c((predsN.upp2), rev((predsN.low2))), col = mytranspN, border = NA)

#library(plotrix)
with(sitevals.phi2[sitevals.phi2$pot.type == "G",], plotCI(jitter(Imp_vals), estimate, ui = ucl, li = lcl, pch = 24, pt.bg = mycols, add = T))
with(sitevals.phi2[sitevals.phi2$pot.type == "N",], plotCI(jitter(Imp_vals), estimate, ui = ucl, li = lcl, pch = 25, pt.bg = mycols, add = T))
#with(sitevals2, plotCI(jitter(Imp_vals), estimate, ui = 0.1, li = 0.1, pch = mypch, pt.bg = mycols, add = T))

legend("bottomleft", pch = c(24,25), pt.bg = c("darkorchid", "cornsilk3"), legend = c("Garden", "Non-garden"), cex = 0.8, pt.cex = 1)     

