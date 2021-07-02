library(psych)
library(lme4)  # loads in package to run HLM analyses
library(nlme)
#library(growthmodels)
library(ggplot2)
library(lattice)

setwd("/Users/SamanthaGarrett/Desktop/USF - HLM - Spring 2017/Final Paper")  # sets directory to where data file is stored

dat <- read.csv("HLM final project.csv")		# loading in the dataset
str(dat)
describe(dat)
summary(dat)

team.avg <- aggregate(dat,list(dat$Team),mean,na.rm=TRUE)
str(team.avg)
describe(team.avg)
summary(team.avg)			# level 2 descriptives (provides average for each team on each variable)
head(team.avg)


dat2 <- data.frame(matrix(NA,nrow(team.avg) ,5))
colnames(dat2)<-c("Team","team_consc","team_agree","team_extra","team_proptrust")
dat2$Team <- team.avg$Team
dat2$team_consc <- team.avg$CAvg
dat2$team_agree <- team.avg$AAvg
dat2$team_extra <- team.avg$EAvg
dat2$team_proptrust <- team.avg$PtTAvg
dat2 <- data.frame(dat2)
team.dat <- dat2[,2:6]


dat <-merge(team.dat,dat,by = "Team")		# combining level 1 and level 2 datasets

dat$consc.c <- scale(dat$consc, center=T, scale=F)		# centering conscientiousness
dat$agree.c <- scale(dat$agree, center=T, scale=F)		# centering agreeableness
dat$extra.c <- scale(dat$extra, center=T, scale=F)		# centering extraversion
dat$proptrust.c <- scale(dat$proptrust, center=T, scale=F)		# centering propensity to trust
dat$perform.c <- scale(dat$performance, center=T, scale=F)		# centering performance


############################################################################
################################## Models ##################################
############################################################################

model1 <- lmer(potency ~ 1 + (1|team), data = dat)		# Unconditional model
summary(model1)


# hypothesis 1
model2 <- lmer(potency ~ 1 + performance + (1|team), data = dat)	# hypothesis 1	
summary(model2)


# hypotheses 2-4
model3 <- lmer(potency ~ 1 + consc.c + agree.c + extra.c + (1|team), data = dat)	# hypotheses 2-4
summary(model3)

model3a <- lmer(potency ~ 1 + consc.c + (1|team), data = dat)	
summary(model3a)

model3b <- lmer(potency ~ 1 + agree.c + (1|team), data = dat)	
summary(model3b)

model3c <- lmer(potency ~ 1 + extra.c + (1|team), data = dat)	
summary(model3c)



model4 <- lmer(potency ~ 1 + consc.c + agree.c + extra.c + performance + (1|team), data = dat)
summary(model4)

model4a <- lmer(potency ~ 1 + consc.c + performance + (1|team), data = dat)	
summary(model4a)

model4b <- lmer(potency ~ 1 + agree.c + performance + (1|team), data = dat)	
summary(model4b)

model4c <- lmer(potency ~ 1 + extra.c + performance + (1|team), data = dat)	
summary(model4c)






model5 <- lmer(potency ~ 1 + consc.c*performance + agree.c*performance + extra.c*performance + performance + (1|team), data = dat)
summary(model5)

model5a <- lmer(potency ~ 1 + consc.c*performance + (1|team), data = dat)	
summary(model5a)

model5b <- lmer(potency ~ 1 + agree.c*performance + (1|team), data = dat)	
summary(model5b)

model5c <- lmer(potency ~ 1 + extra.c*performance + (1|team), data = dat)	
summary(model5c)






#######################################################################################
################################## Residual Analysis ##################################
#######################################################################################

resid <- residuals(model1)
resid <- scale(resid)
describe(resid)
plot(resid)
plot(density(resid),lwd=2)
curve(dnorm,add=T,lty=2)
legend("topleft",c("Density curve", "Normal curve"),lty = c(1,2),lwd=c(2,1),box.lty = 0)

# check the normality of level-1 residuals using either a boxplot or a q-q plot
boxplot(resid)
qqmath(model1)

# Check the homoscedasticity assumption by observing a scatterplot between level-1 residuals and level-1 predicted values (level-1 residuals on the Y axis and level-1 predicted values on the X axis)
plot(model1, resid(.) ~ predict(.))










#######################################################################################
################################## Interaction Plots ##################################
#######################################################################################

minpt <- (3.13 - .18*c + 1.64*a - .10*e + .01*p + .01*cp - .03*ap + 0*ep)



# min perf
a <- (-1.74)
p <- (1)
minpt.0 <- (3.13 + 1.64*a + .01*p - .03*(a*p))
a <- (.91)
p <- (1)
maxpt.0 <- (3.13 + 1.64*a + .01*p - .03*(a*p))
minpt.0
maxpt.0

# max perf
a <- (-1.74)
p <- (70)
minpt.1 <- (3.13 + 1.64*a + .01*p - .03*(a*p))
a <- (.91)
p <- (70)
maxpt.1 <- (3.13 + 1.64*a + .01*p - .03*(a*p))
minpt.1
maxpt.1




################################################################################
################################## OLD Models ##################################
################################################################################







model5a <- lmer(potency ~ 1 + consc.c*performance + agree.c + extra.c + performance + (1|team), data = dat)
summary(model5a)

model5b <- lmer(potency ~ 1 + consc.c + agree.c*performance + extra.c + performance + (1|team), data = dat)
summary(model5b)

model5c <- lmer(potency ~ 1 + consc.c + agree.c + extra.c*performance + performance + (1|team), data = dat)
summary(model5c)

model6 <- lmer(potency ~ 1 + consc.c*performance + agree.c*performance + extra.c*performance + performance + (1|team), data = dat)
summary(model6)

model7 <- lmer(potency ~ 1 + consc.c*performance + agree.c*performance + extra.c*performance + performance + (1 + performance|team), data = dat)
summary(model7)

model8 <- lmer(potency ~ 1 + consc.c + agree.c + extra.c + performance + proptrust.c + (1|team), data = dat)
summary(model8)

model9 <- lmer(potency ~ 1 + consc.c + agree.c + extra.c + performance + proptrust.c + performance*proptrust.c + (1|team), data = dat)
summary(model9)


############################################################################
################################## Models ##################################
############################################################################

model1a <- lmer(potency ~ 1 + (1|team), data = dat)		# Unconditional model
summary(model1a)

model1b <- lmer(satisfaction ~ 1 + (1|team), data = dat)		# Unconditional model
summary(model1b)

model2a <- lmer(potency ~ 1 + performance + (1|team), data = dat)		
summary(model2a)

model2b <- lmer(satisfaction ~ 1 + performance + (1|team), data = dat)		
summary(model2b)


dat$proptrust2 <- dat$proptrust.c*dat$proptrust.c


model2 <- lmer(potency ~ 1 + proptrust.c + (1|team), data = dat)		
summary(model2)

model3 <- lmer(potency ~ 1 + performance + (1|team), data = dat)		
summary(model3)

model4 <- lmer(potency ~ 1 + proptrust.c + performance + (1|team), data = dat)		
summary(model4)

model4b <- lmer(potency ~ 1 + proptrust.c + performance + (1 + performance|team), data = dat)
summary(model4b)

model5 <- lmer(potency ~ 1 + proptrust.c*performance + (1|team), data = dat)		
summary(model5)

model6 <- lmer(potency ~ 1 + proptrust.c*performance + (1 + performance|team), data = dat)	
summary(model6)

anova(model4,model4b)
anova(model4,model5)
anova(model6,model5)




model5 <- lmer(potency ~ 1 + proptrust.c + agree.c + performance + (1|team), data = dat)	
summary(model5)

model6 <- lmer(potency ~ 1 + proptrust.c + agree.c + performance + (1 + performance|team), data = dat)	
summary(model6)

model7 <- lmer(potency ~ 1 + proptrust.c*performance + agree.c + performance + (1|team), data = dat)	
summary(model7)

model8 <- lmer(potency ~ 1 + proptrust.c + agree.c*performance + performance + (1|team), data = dat)	
summary(model8)

model8b <- lmer(potency ~ 1 + proptrust.c + agree.c*performance + performance + (1 + performance|team), data = dat)	
summary(model8b)

anova(model8,model8b)



model9 <- lmer(potency ~ 1 + proptrust.c*performance + agree.c*performance + performance + (1|team), data = dat)	
summary(model9)







model7 <- lmer(potency ~ 1 + proptrust.c + proptrust2 + performance + (1|team), data = dat)	
summary(model7)

model7 <- lmer(potency ~ 1 + proptrust.c*performance + proptrust2 + (1|team), data = dat)	
summary(model7)







model2 <- lmer(satisfaction ~ 1 + proptrust.c + (1|team), data = dat)		
summary(model2)

model3 <- lmer(satisfaction ~ 1 + performance + (1|team), data = dat)		
summary(model3)

model4 <- lmer(satisfaction ~ 1 + proptrust.c + performance + (1|team), data = dat)		
summary(model4)

model5 <- lmer(satisfaction ~ 1 + proptrust.c*performance + (1|team), data = dat)		
summary(model5)

model6 <- lmer(satisfaction ~ 1 + proptrust.c*performance + (1 + performance|team), data = dat)
summary(model6)



