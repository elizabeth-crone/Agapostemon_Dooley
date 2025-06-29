setwd("~/Documents/Coding_manuscript")
library(tidyverse)
source("transparent.R")

# starting with a general summarization of survey results
# read in raw mark recapture data
dat = read.csv("C:/Users/ecrone/Box/All Box Documents/Karen Dooley/EcoApps final files/Mark_recap_raw.csv")
unique(dat$Site)

# remove sites that were not selected for final analyses
use.sites = c("POW", "BIK", "TUF", "MCM", "SAC", "LIN", "WAL")
dat2 = dat[dat$Site %in% use.sites,]

# create a data table with site as the first column
AGVI.sum = tibble(.rows = 7)
AGVI.sum$Site = unique(dat2$Site)

# total number of *captured* bees at each site (includes recaptures) 
AGVI.sum$Ncap = as.numeric(unlist(table(dat2$Site)))
# pull out each unique three color ID used for the study
tmp = with(dat2, tapply(Mark_ID, Site, unique))
# total number of *marked* bees at each site (excludes recaptures)
AGVI.sum$Nbees = unlist(lapply(tmp, length))
# total number of *captured* bees at each garden patch (includes recaptures)
AGVI.sum$Ncap.G = as.numeric(unlist(table(dat2$Site[dat2$Pot == "G"])))
# total number of *captured* bees at each non-garden patch (includes recaptures)
AGVI.sum$Ncap.N = as.numeric(unlist(table(dat2$Site[dat2$Pot == "N"])))
# total number of *recaptured* bees at each site
AGVI.sum$Nrecap = unlist(with(dat2, tapply(Recap, Site, sum)))
# total number of *marked* bees at each garden patch (excludes recaptures)
tmp2 = with(dat2[dat2$Pot == "G",], tapply(Mark_ID, Site, unique))
AGVI.sum$Nbees.G = unlist(lapply(tmp2, length))
# total number of *marked* bees at each non-garden patch (excludes recaptures)
tmp3 = with(dat2[dat2$Pot == "N",], tapply(Mark_ID, Site, unique))
AGVI.sum$Nbees.N = unlist(lapply(tmp3, length))

# final table with totals of each metric measured at each site 
AGVI.sum



sum(AGVI.sum$Ncap)
table(dat2$move_Type)
(Nmoves = sum(table(dat2$move_Type))-145)
1-10/81 # proportion of recaptures in a new pot


#########################
# set up data for statistical testing
AGVI.num1 = pivot_longer(AGVI.sum[,c(1,2,7,8)], 
                         cols = c("Nbees.G", "Nbees.N"), values_to = "Nbees")
AGVI.num1$pot = substr(AGVI.num1$name, start = 7, stop = 7)
AGVI.num2 = pivot_longer(AGVI.sum[,c(1,2,4,5)], cols = c("Ncap.G", "Ncap.N"), values_to = "Ncaps")
AGVI.num2$pot = substr(AGVI.num2$name, start = 6, stop = 6)
AGVI.num = merge(AGVI.num1, AGVI.num2, by = c("Site", "pot"))
AGVI.num

# read in % impervious surface values calculated for each site
new_imp = read.csv("C:/Users/ecrone/Box/All Box Documents/Karen Dooley/EcoApps final files/Imp_vals_perpot.csv")
new_imp
imp2 = pivot_longer(new_imp[,c(1,3,4)], cols = c("Imp_G", "Imp_N"), values_to = "Imp_vals")
imp2
imp2$pot = substr(imp2$name, start = 5, stop = 5)

# merge the mark-recapture summarized data with the % impervious surface data
AGVI.num.imp = merge(AGVI.num, imp2)
AGVI.num.imp 

# read in garden size data and estimated floral counts from garden surveys
gard.size = read.csv("C:/Users/ecrone/Box/All Box Documents/Karen Dooley/EcoApps final files/Gard_size.csv")

AGVI.num.gards = merge(AGVI.num.imp[AGVI.num.imp$pot == "G",], gard.size, by = "Site") 
AGVI.num.gards # data merged with garden size and floral counts
library(MASS)
library(car)


#####################
# simple statistical tests of bee counts as a function of impervious surface
# test of number of captures
# this is the most similar to counts at flowers without marking
b0 = glm.nb(Ncaps~Imp_vals*pot, data = AGVI.num.imp)
Anova(b0)
summary(b0)

# test of number of individuals
b1 = glm.nb(Nbees~Imp_vals*pot, data = AGVI.num.imp)
Anova(b1)
summary(b1)


#####################
# elements for graph results
bmin = min(AGVI.num.imp$Imp_vals)
bmax = max(AGVI.num.imp$Imp_vals)

Imps = seq(bmin, bmax, 0.1)
predsG = predict(b1, newdata = data.frame(Imp_vals = Imps, pot = "G"), se.fit = T)
predsN = predict(b1, newdata = data.frame(Imp_vals = Imps, pot = "N"), se.fit = T)
predsG0 = predict(b0, newdata = data.frame(Imp_vals = Imps, pot = "G"), se.fit = T)
predsN0 = predict(b0, newdata = data.frame(Imp_vals = Imps, pot = "N"), se.fit = T)

AGVI.num.imp$mycols = "darkorchid"
AGVI.num.imp$mycols[AGVI.num$pot == "N"] = "cornsilk3"
AGVI.num.imp$mypch = 24
AGVI.num.imp$mypch[AGVI.num$pot == "N"] = 25
mytranspG = t_col("darkorchid", percent = 70)
mytranspN = t_col("cornsilk3", percent = 70)

par(mfrow = c(1,1))
with(AGVI.num.imp, plot(Imp_vals, Nbees, pch = mypch, bg = mycols, ylim = c(0,30), xlab = "", ylab = ""))
mtext(side = 1, line = 2, "% impervious surface")
mtext(side = 2, line = 2, "# individual bees seen")
polygon(c(Imps, rev(Imps)), c(exp(predsG$fit + 1.4*predsG$se.fit), 
                              rev(exp(predsG$fit - 1.4*predsG$se.fit))), 
                              col = mytranspG, border = "darkorchid4", lty = "dashed")
polygon(c(Imps, rev(Imps)), c(exp(predsN$fit + 1.4*predsN$se.fit), 
                              rev(exp(predsN$fit - 1.4*predsN$se.fit))), 
                              col = mytranspN, border = "cornsilk4")
points(Imps, exp(predsG$fit), type = "l", col = "darkorchid4", lwd = 3, lty = "dashed")
points(Imps, exp(predsN$fit), type = "l", col = "cornsilk4", lwd = 3)
with(AGVI.num.imp, points(Imp_vals, Nbees, pch = mypch, bg = mycols)) # redraw points in foreground
legend("topright", pch = c(24,25), pt.bg = c("darkorchid", "cornsilk3"), 
       legend = c("Garden", "Non-garden"), lty = c("dashed","solid"), cex = 0.8, pt.cex = 1)   


with(AGVI.num.imp, plot(Imp_vals, Ncaps, pch = mypch, bg = mycols, ylim = c(0,40), xlab = "", ylab = ""))
mtext(side = 1, line = 2, "% impervious surface")
mtext(side = 2, line = 2, "total # of captures")
polygon(c(Imps, rev(Imps)), c(exp(predsG0$fit + 1.4*predsG0$se.fit), 
                              rev(exp(predsG0$fit - 1.4*predsG0$se.fit))), 
                              col = mytranspG, border = NA)
polygon(c(Imps, rev(Imps)), c(exp(predsN0$fit + 1.4*predsN0$se.fit), 
                              rev(exp(predsG0$fit - 1.4*predsG0$se.fit))), 
                              col = mytranspN, border = NA)
points(Imps, exp(predsG0$fit), type = "l", col = "darkorchid4", lwd = 3)
points(Imps, exp(predsN0$fit), type = "l", col = "cornsilk4", lwd = 3)
with(AGVI.num.imp, points(Imp_vals, Ncaps, pch = mypch, bg = mycols)) # redraw points in foreground
legend("topright", pch = c(24,25), pt.bg = c("darkorchid", "cornsilk3"), 
       legend = c("Garden", "Non-garden"), cex = 0.8, pt.cex = 1)    


#####################
# descriptive statistics for text
# max and min impervious surface values
min(AGVI.num.imp$Imp_vals)
max(AGVI.num.imp$Imp_vals)

# mean, max, and minimum number of *captured* bees at each site
mean(AGVI.num.imp$Ncaps)
min(AGVI.num.imp$Ncaps)
max(AGVI.num.imp$Ncaps)

# mean, max, and minimum number of *marked* bees at each site
mean(AGVI.num.imp$Nbees)
min(AGVI.num.imp$Nbees)
max(AGVI.num.imp$Nbees)


#####################
# simple statistical tests of garden characteristics as a function of imp surface
# test of floral counts
head(AGVI.num.gards)
m.flwr = glm(Fl_counts_ingard ~ Imp_vals, data = AGVI.num.gards)
Anova(m.flwr)

summary(m.flwr)


# elements for graph results
bmin = min(AGVI.num.gards$Imp_vals)
bmax = max(AGVI.num.gards$Imp_vals)
Imps = seq(bmin, bmax, 0.1)
predsF = predict(m.flwr, newdata = data.frame(Imp_vals = Imps), se.fit = T)

mytranspF = t_col("deeppink3", percent = 70)

par(mfrow = c(1,1))
with(AGVI.num.gards, plot(Imp_vals, Fl_counts_ingard, pch = 21, bg = "deeppink3", 
                          ylim = c(0,10000), xlab = "", ylab = "flower count"))
mtext(side = 1, line = 2, "% impervious surface")
mtext(side = 2, line = 2, "in community garden")
polygon(c(Imps, rev(Imps)), c((predsF$fit + 1.96*predsF$se.fit), 
                              rev((predsF$fit - 1.96*predsF$se.fit))), 
                              col = mytranspF, border = NA)
points(Imps, (predsF$fit), type = "l", col = "deeppink4", lwd = 3)
with(AGVI.num.gards, points(Imp_vals, Fl_counts_ingard, pch = 21, bg = "deeppink3")) # redraw points in foreground


# test of garden size
m.size = glm(Area_m2 ~ Imp_vals, data = AGVI.num.gards)
hist(resid(m.size))
Anova(m.size)
summary(m.size)


# elements for graph results
predsS = predict(m.size, newdata = data.frame(Imp_vals = Imps), se.fit = T)

mytranspS = t_col("darkolivegreen", percent = 70)

par(mfrow = c(1,1))
with(AGVI.num.gards, plot(Imp_vals, Area_m2, pch = 21, bg = "darkolivegreen4", ylim = c(0,800), xlab = "", ylab = ""))
mtext(side = 1, line = 2, "% impervious surface")
mtext(side = 2, line = 2, "garden area (m2)")
polygon(c(Imps, rev(Imps)), c((predsS$fit + 1.96*predsS$se.fit), 
                              rev((predsS$fit - 1.96*predsS$se.fit))), 
                              col = mytranspS, border = NA)
points(Imps, (predsS$fit), type = "l", col = "darkolivegreen", lwd = 3)
with(AGVI.num.gards, points(Imp_vals, Area_m2, pch = 21, bg = "darkolivegreen4")) # redraw points in foreground


#####################
# simple statistical tests of floral count per bee as a function of impervious surface
# done with number of individual bees
#m.flwrbees = lm(log(Fl_counts_ingard/Nbees) ~ Imp_vals, data = AGVI.num.gards)
m.flwrbees = glm.nb(Nbees ~ Imp_vals + offset(log(Fl_counts_ingard)), data = AGVI.num.gards)
Anova(m.flwrbees)
summary(m.flwrbees)

# elements for graph results
predsFB = predict(m.flwrbees, newdata = data.frame(Imp_vals = Imps, Fl_counts_ingard = 1), se.fit = T)

mytranspF = t_col("gold2", percent = 70)

meanbees = mean(AGVI.num.gards$Nbees)
par(mfrow = c(1,1))
with(AGVI.num.gards, plot(Imp_vals, Fl_counts_ingard/Nbees, pch = 21, bg = "gold2", 
                          ylim = c(0,3000), xlab = "", ylab = "flower count", cex = sqrt(Nbees/meanbees)))
mtext(side = 1, line = 2, "% impervious surface")
mtext(side = 2, line = 2, "per individual bee")
polygon(c(Imps, rev(Imps)), c(1/exp(predsFB$fit + 1.96*predsFB$se.fit), 
                              rev(1/exp(predsFB$fit - 1.96*predsFB$se.fit))), 
                              col = mytranspF, border = NA)
points(Imps, 1/exp(predsFB$fit), type = "l", col = "gold3", lwd = 3)
with(AGVI.num.gards, points(Imp_vals, Fl_counts_ingard/Nbees, pch = 21, bg = "gold2", 
                            cex = sqrt(Nbees/meanbees))) # redraw points in foreground


# alternative figure with axis flipped
meanbees = mean(AGVI.num.gards$Nbees)
with(AGVI.num.gards, plot(Imp_vals, Nbees/Fl_counts_ingard, pch = 21, bg = "gold2", 
                          ylim = c(0,0.005), xlab = "", ylab = "# bees (unique individuals)", cex = sqrt(Nbees/meanbees)))
mtext(side = 1, line = 2, "% impervious surface")
mtext(side = 2, line = 2, "per flower in garden")
polygon(c(Imps, rev(Imps)), c(exp(predsFB$fit + 1.96*predsFB$se.fit), 
                              rev(exp(predsFB$fit - 1.96*predsFB$se.fit))), 
                              col = mytranspF, border = NA)
points(Imps, exp(predsFB$fit), type = "l", col = "gold3", lwd = 3)
with(AGVI.num.gards, points(Imp_vals, Nbees/Fl_counts_ingard, pch = 21, bg = "gold2", 
                            cex = sqrt(Nbees/meanbees))) # redraw points in foreground
