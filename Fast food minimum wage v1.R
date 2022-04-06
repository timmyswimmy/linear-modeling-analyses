################################
#Statistics 151A: Final Project#
#     Timothy Wang 23073372    #
################################

################################
#########  Question 2  #########
################################

##################
###   Set-up   ###
##################

require(foreign)
require(ggplot2)
setwd("/Users/timwang/Desktop/Stat 151A/Final Exam")
fastfood <- read.csv("fastfood.csv", na.strings = c('.', 'NA'), header = T)
fastfood$TotalEMP1 <- fastfood$EMPFT + fastfood$EMPPT + fastfood$NMGRS
fastfood$TotalEMP2 <- fastfood$EMPFT2 + fastfood$EMPPT2 + fastfood$NMGRS2
fastfood <- fastfood[fastfood$STATUS2 == 1,] # We only want restaurants that replied for the second time.
	
head(fastfood)
summary(fastfood)
names(fastfood)

#####################################
#  Factorize categorical variables  #
#####################################

dfastfood = data.frame(1:399)
dfastfood$SPECIAL2 <- factor(fastfood$SPECIAL2)
dfastfood$BONUS <- factor(fastfood$BONUS)
dfastfood$MEALS <- factor(fastfood$MEALS)
dfastfood$MEALS2 <- factor(fastfood$MEALS2)
dfastfood$CHAIN <- factor(fastfood$CHAIN)
dfastfood$CO_OWNED <- factor(fastfood$CO_OWNED)
dfastfood$STATE <- factor(fastfood$STATE)

###########################
#  Compute all % changes  #
###########################

dfastfood$PFRY <- fastfood$PFRY2 / fastfood$PFRY - 1
dfastfood$PSODA <- fastfood$PSODA2 / fastfood$PSODA - 1
dfastfood$PENTREE <- fastfood$PENTREE2 / fastfood$PENTREE - 1
dfastfood$HRSOPEN <- fastfood$HRSOPEN2 / fastfood$HRSOPEN - 1
dfastfood$NREGS <- fastfood$NREGS2 / fastfood$NREGS - 1
dfastfood$NREGS11 <- fastfood$NREGS112 / fastfood$NREGS11 - 1
dfastfood$WAGE_ST <- fastfood$WAGE_ST2 / fastfood$WAGE_ST - 1
dfastfood$FIRSTINC <- fastfood$FIRSTIN2 / fastfood$FIRSTINC - 1
# dfastfood$EMPFT <- fastfood$EMPFT2 / fastfood$EMPFT - 1
# dfastfood$EMPPT <- fastfood$EMPPT2 / fastfood$EMPPT - 1
# dfastfood$NMGRS <- fastfood$NMGRS2 / fastfood$NMGRS - 1
# dfastfood$WRKRS <- (fastfood$EMPFT2 + fastfood$EMPPT2) / (fastfood$EMPFT + fastfood$EMPPT) - 1
dfastfood$TOTAL <- fastfood$TotalEMP2 / fastfood$TotalEMP1 - 1
dfastfood$INCTIME <- fastfood$INCTIME2 / fastfood$INCTIME - 1
fastfood$OPEN[fastfood$OPEN == 0] <- NA
dfastfood$OPEN <- fastfood$OPEN2R / fastfood$OPEN - 1

##############################
# Keep "% of staff affected" #
##############################

dfastfood$PCTAFF <- fastfood$PCTAFF
# dfastfood$INCTIME <- fastfood$INCTIME
# dfastfood$INCTIME2 <- fastfood$INCTIME2
# dfastfood$OPEN <- fastfood$OPEN
# dfastfood$OPEN2R <- fastfood$OPEN2R
dfastfood <- dfastfood[, -1]

############################
# Remove NA's from workers #
############################

remNA = match(c("TOTAL"), names(dfastfood))
# remNA = match(c("WRKRS"), names(dfastfood))
# remNA = match(c("TOTAL", "WAGE_ST"), names(dfastfood))
for(i in remNA)
{
	dfastfood <- dfastfood[!is.na(dfastfood[, i]), ]
}

# for(j in 1:length(names(dfastfood)))
# {
	# dfastfood <- dfastfood[is.finite(dfastfood[, j]),]
# }

dim(dfastfood)
# [1] 378  19

#################################
# Explore variables of interest #
#      FIGURE 2.1 IN TEXT       #
#################################

indi <- match(c("WAGE_ST", "TOTAL", "PENTREE", "PSODA", "PFRY"), names(dfastfood))
par(mfcol = c(1, 2))
pairs(dfastfood[, indi], main = "Figure 2.1A: Pairs Plot of Variables of Interest - Before")
dim(dfastfood[dfastfood$WAGE_ST < 0.4,])
dfastfood <- dfastfood[dfastfood$WAGE_ST < 0.4,]
pairs(dfastfood[, indi], main = "Figure 2.1B: Pairs Plot of Variables of Interest - After")

##########################
###      ANALYSIS      ###
##########################

##############
# Full Model #
##############

require(car)
pairs(dfastfood)
mod <- lm(TOTAL ~ ., data = dfastfood)
# mod <- lm(WRKRS ~ ., data = dfastfood)
summary(mod)
avPlots(mod)
crPlots(mod)

##########################################################
# All-subsets to find best model without transformations #
#                    FIGURE 2.2 IN TEXT                  #
##########################################################

require(leaps)
regs <- regsubsets(TOTAL ~ ., data = dfastfood, nvmax = dim(dfastfood)[2])
summary(regs)
bestModel <- match(min(summary(regs)$cp), summary(regs)$cp)
coefs <- which(summary(regs)$which[bestModel,])[-1]
coefs
# FIRSTINC     OPEN 
#       22       24 
mod2 <- lm(TOTAL~FIRSTINC + OPEN, data = dfastfood)
summary(mod2)
par(mfrow = c(2, 2))
avPlots(mod2, main = "Figure 2.2A: Added Variable Plots")
crPlots(mod2, main = "Figure 2.2B: Component + Residual Plots") # There is an influential point for OPEN; remove to get better picture.

###########################
# Remove outliers in OPEN #
#    FIGURE 2.3 IN TEXT   #
###########################

dim(dfastfood) # 378
dfastfood <- dfastfood[dfastfood$OPEN > -0.8, ]
dim(dfastfood) # 376

mod2woout <- lm(TOTAL~FIRSTINC + OPEN, data = dfastfood)
summary(mod2woout)
avPlots(mod2woout, main = "Figure 2.3A: Added Variable Plots Without Leverage Point")
crPlots(mod2woout, main = "Figure 2.3B: Component + Residual Plots Without Leverage Point") # No outlier does not particularly reveal a shape anyways.

#####################################
# Use results from AV and CR Plots. #
#         FIGURE 2.4 IN TEXT        #
#####################################

mod3 <- lm(TOTAL~I(FIRSTINC^3) + I(FIRSTINC^2) + FIRSTINC + OPEN, data = dfastfood)
summary(mod3)
avPlots(mod3)
crPlots(mod3)
anova(mod2woout, mod3, test = "LRT")
par(mfrow=c(2,2))
plot(mod3, caption = list("Figure 2.4A: Residuals vs Fitted", "Figure 2.4B: Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage", expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))

######################################################
# Add STATE as interaction to indicate change in MW. #
#        See if this addition is significant.        #
#                 FIGURE 2.5 IN TEXT                 #
######################################################

mod4 <- lm(TOTAL~STATE*(I(FIRSTINC^3) + I(FIRSTINC^2) + FIRSTINC + OPEN), data = dfastfood)
summary(mod4)
avPlots(mod4)
crPlots(mod4)
anova(mod2woout, mod3, mod4, test = "LRT")

####################################
# Analyze change in prices - SETUP #
#        FIGURE 2.6 IN TEXT        #
####################################

# par(mfcol = c(2, 2))
# hist(dfastfood$PENTREE)
# hist(dfastfood$PFRY)
# hist(dfastfood$PSODA)
# dfastfood$PRICE <- dfastfood$PENTREE + dfastfood$PFRY + dfastfood$PSODA
# hist(dfastfood$PRICE)
# inds <- match(c("PENTREE", "PFRY", "PSODA"), names(dfastfood))
dataPENTREE <- dfastfood[, -match(c("PFRY", "PSODA"), names(dfastfood))]
dataPFRY <- dfastfood[, -match(c("PENTREE", "PSODA"), names(dfastfood))]
dataPSODA <- dfastfood[, -match(c("PENTREE", "PFRY"), names(dfastfood))]
# for(i in inds)
# {
# 	dataP <- dataP[!is.na(dfastfood[, i]), ]
# }
# dim(dataP)
# # [1] 376  19
modPENTREE <- lm(PENTREE ~ ., data = dataPENTREE)
modPFRY <- lm(PFRY ~ ., data = dataPFRY)
modPSODA <- lm(PSODA ~ ., data = dataPSODA)
summary(modPENTREE)
avPlots(modPENTREE)
crPlots(modPENTREE)
summary(modPFRY)
avPlots(modPFRY)
crPlots(modPFRY)
summary(modPSODA)
avPlots(modPSODA)
crPlots(modPSODA)

####################################
# Analyze change in entree prices. #
####################################

regsPENTREE <- regsubsets(PENTREE ~ ., data = dataPENTREE, nvmax = dim(dataPENTREE)[2])
summary(regsPENTREE)
bestModelPENTREE <- match(min(summary(regsPENTREE)$cp), summary(regsPENTREE)$cp)
coefsPENTREE <- which(summary(regsPENTREE)$which[bestModelPENTREE,])[-1]
coefsPENTREE
#   CHAIN4 STATE1   OPEN PCTAFF 
#       12     14     22     23 
modP2E <- lm(PENTREE ~ CHAIN + STATE + OPEN + PCTAFF, data = dataPENTREE)
summary(modP2E)
avPlots(modP2E)
crPlots(modP2E)
plot(modP2E)

modP3E <- lm(PENTREE ~ STATE * (CHAIN + OPEN + PCTAFF), data = dataPENTREE)
summary(modP3E)
avPlots(modP3E)
crPlots(modP3E)
plot(modP3E)
anova(modP2E, modP3E, test = "LRT")

modP4E <- lm(PENTREE ~ STATE * CHAIN * (OPEN + PCTAFF), data = dataPENTREE)
summary(modP4E)
avPlots(modP4E)
crPlots(modP4E)
anova(modP2E, modP3E, modP4E, test = "LRT")
par(mfcol = c(2,2))
plot(modP4, caption = list("Figure 2: Residuals vs Fitted", "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage", expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))

###################################
# Analyze change in fries prices. #
###################################

regsPFRY <- regsubsets(PFRY ~ ., data = dataPFRY, nvmax = dim(dataPFRY)[2])
summary(regsPFRY)
bestModelPFRY <- match(min(summary(regsPFRY)$cp), summary(regsPFRY)$cp)
coefsPFRY <- which(summary(regsPFRY)$which[bestModelPFRY,])[-1]
coefsPFRY
#     CHAIN3 FIRSTINC   PCTAFF 
#	      11       19       23 
modP2F <- lm(PFRY ~ CHAIN + FIRSTINC + PCTAFF, data = dataPFRY)
summary(modP2F)
avPlots(modP2F)
crPlots(modP2F)
plot(modP2F)

modP3F <- lm(PFRY ~ CHAIN * (FIRSTINC + PCTAFF), data = dataPFRY)
summary(modP3F)
avPlots(modP3F)
crPlots(modP3F)
plot(modP3F)
anova(modP2F, modP3F, test = "LRT")

modP4F <- lm(PFRY ~ STATE * CHAIN * (FIRSTINC + PCTAFF), data = dataPFRY)
summary(modP4F)
avPlots(modP4F)
crPlots(modP4F)
plot(modP4F)
anova(modP2F, modP3F, modP4F, test = "LRT")
par(mfcol = c(2,2))
plot(modP4, caption = list("Figure 2: Residuals vs Fitted", "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage", expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))

##################################
# Analyze change in soda prices. #
##################################

regsPSODA <- regsubsets(PSODA ~ ., data = dataPSODA, nvmax = dim(dataPSODA)[2])
summary(regsPSODA)
bestModelPSODA <- match(min(summary(regsPSODA)$cp), summary(regsPSODA)$cp)
coefsPSODA <- which(summary(regsPSODA)$which[bestModelPSODA,])[-1]
coefsPSODA
#    MEALS1  CHAIN4  STATE1   NREGS WAGE_ST    OPEN 
#	      4      12      14      16      18      22 
modP2S <- lm(PSODA ~ MEALS + CHAIN + STATE + NREGS + WAGE_ST + OPEN, data = dataPSODA)
summary(modP2S)
avPlots(modP2S)
crPlots(modP2S)
plot(modP2S)

modP3S <- lm(PSODA ~ STATE * (MEALS + CHAIN + NREGS + WAGE_ST + OPEN), data = dataPSODA)
summary(modP3S)
avPlots(modP3S)
crPlots(modP3S)
plot(modP3S)
anova(modP2S, modP3S, test = "LRT")

modP4S <- lm(PSODA ~ STATE * CHAIN * (MEALS + NREGS + WAGE_ST + OPEN), data = dataPSODA)
summary(modP4S)
avPlots(modP4S)
crPlots(modP4S)
plot(modP4S)
anova(modP2S, modP3S, modP4S, test = "LRT")
par(mfcol = c(2,2))
plot(modP4, caption = list("Figure 2: Residuals vs Fitted", "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage", expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))

#####################################
# Analyze change in overall prices. #
#####################################

par(mfcol = c(2, 2))
hist(dfastfood$PENTREE)
hist(dfastfood$PFRY)
hist(dfastfood$PSODA)
dfastfood$PRICE <- dfastfood$PENTREE + dfastfood$PFRY + dfastfood$PSODA
hist(dfastfood$PRICE)
inds <- match(c("PENTREE", "PFRY", "PSODA"), names(dfastfood))
dataP <- dfastfood[, -inds]
dataP <- dataP[!is.na(dataP$PRICE), ]
dim(dataP)
# [1] 313  17
modP <- lm(PRICE ~ ., data = dataP)
summary(modP)
avPlots(modP)
crPlots(modP)

regsP <- regsubsets(PRICE ~ ., data = dataP, nvmax = dim(dataP)[2])
summary(regsP)
bestModelP <- match(min(summary(regsP)$cp), summary(regsP)$cp)
coefsP <- which(summary(regsP)$which[bestModelP,])[-1]
coefsP
#  CHAIN4   STATE1    NREGS FIRSTINC    TOTAL   PCTAFF 
#      12       14       16       19       20       23 
modP2 <- lm(PRICE ~ CHAIN + STATE + NREGS + FIRSTINC + TOTAL + PCTAFF, data = dataP)
summary(modP2)
avPlots(modP2)
crPlots(modP2)
plot(modP2)

modP3 <- lm(PRICE ~ CHAIN * (STATE + NREGS + FIRSTINC + TOTAL + PCTAFF), data = dataP)
summary(modP3)
avPlots(modP3)
crPlots(modP3)
plot(modP3)
anova(modP2, modP3, test = "LRT")

modP4 <- lm(PRICE ~ STATE * CHAIN * (NREGS + FIRSTINC + TOTAL + PCTAFF), data = dataP)
summary(modP4)
avPlots(modP4)
crPlots(modP4)
anova(modP2, modP3, modP4, test = "LRT")
par(mfcol = c(2,2))
plot(modP4, caption = list("Figure 2: Residuals vs Fitted", "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage", expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))
