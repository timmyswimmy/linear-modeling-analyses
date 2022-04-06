################################
#Statistics 151A: Final Project#
#     Timothy Wang 23073372    #
################################

################################
#########  Question 2  #########
################################

## Set-up
require(foreign)
require(ggplot2)
setwd("/Users/timwang/Desktop/Stat 151A/Final Exam")
fastfood <- read.csv("fastfood.csv", na.strings = c('.', 'NA'), header = T)
	
head(fastfood)
summary(fastfood)
names(fastfood)

fastfood <- fastfood[fastfood$STATUS2 == 1,] # We only want restaurants that replied for the second time.
fastfood$TotalEMP1 <- fastfood$EMPFT + fastfood$EMPPT + fastfood$NMGRS
fastfood$TotalEMP2 <- fastfood$EMPFT2 + fastfood$EMPPT2 + fastfood$NMGRS2
fastfood$ChangeTotalEMP <- fastfood$TotalEMP2 / fastfood$TotalEMP1 - 1
fastfood$WageDiff <- fastfood$WAGE_ST2 / fastfood$WAGE_ST - 1

factorizing <- match(c("SPECIAL2", "STATE", "BONUS", "MEALS2", "MEALS", "CHAIN"), names(fastfood))
for(j in factorizing)
{
	fastfood[, j] <- factor(fastfood[, j])
}

remNA = match(c("ChangeTotalEMP"), names(fastfood))
#remNA = match(c("ChangeTotalEMP", "WageDiff"), names(fastfood))
for(i in remNA)
{
	fastfood <- fastfood[!is.na(fastfood[, i]), ]
}

dim(fastfood)
# [1] 378  50
# So we still have 378 observations to work with.

plot(fastfood$ChangeTotalEMP, fastfood$WageDiff)
plot(fastfood$ChangeTotalEMP, fastfood$FIRSTIN2)

cbind(names(fastfood), sapply(1:length(names(fastfood)), function(x){class(fastfood[, x])}))

#leaveout <- match(c("SHEET", "SOUTHJ", "CENTRALJ", "NORTHJ", "PA1", "PA2", "SHORE", "NCALLS", "OPEN", "TYPE2", "DATE2", "NCALLS2", "OPEN2R", "EMPFT", "EMPPT", "NMGRS", "EMPFT2", "EMPPT2", "NMGRS2", "TotalEMP1", "TotalEMP2", "STATUS2", "WageDiff"), names(fastfood))

leaveout <- match(c("SHEET", "SOUTHJ", "CENTRALJ", "NORTHJ", "PA1", "PA2", "SHORE", "NCALLS", "OPEN", "TYPE2", "DATE2", "NCALLS2", "OPEN2R", "EMPFT", "EMPPT", "NMGRS", "EMPFT2", "EMPPT2", "NMGRS2", "TotalEMP1", "TotalEMP2", "STATUS2"), names(fastfood))

require(leaps)
trimmed <- fastfood[, -leaveout]
finpls <- match("STATE", names(trimmed)) # 3
regs <- regsubsets(ChangeTotalEMP ~ ., data = trimmed, nvmax = 15)
#regs <- regsubsets(ChangeTotalEMP ~ ., data = trimmed, nvmax = 15, force.in = 5)
summary(regs)
bestModel <- match(min(summary(regs)$cp), summary(regs)$cp)
coefs <- which(summary(regs)$which[bestModel,])[-1]
coefs
#  CHAIN2   CHAIN3 FIRSTINC  PENTREE FIRSTIN2  MEALS23   NREGS2 
#       2        3        9       18       23       27       32 
mod <- lm(ChangeTotalEMP~CHAIN + FIRSTINC + PENTREE + FIRSTIN2 + MEALS2 + NREGS2, data = fastfood)
pairscand <- match(c("CHAIN", "FIRSTINC", "PENTREE", "FIRSTIN2", "MEALS2", "NREGS2", "ChangeTotalEMP"), names(fastfood))
pairs(fastfood[, pairscand])

require(car)
avPlots(mod)
crPlots(mod)
# mod2 <- lm(ChangeTotalEMP ~ STATE*(CHAIN + FIRSTINC + PENTREE + FIRSTIN2 + MEALS2 + NREGS2), data = fastfood)
# mod3 <- lm(ChangeTotalEMP~CHAIN * FIRSTINC * PENTREE * FIRSTIN2 * MEALS2 * NREGS2, data = fastfood)
# mod4 <- lm(ChangeTotalEMP~STATE * CHAIN * FIRSTINC * PENTREE * FIRSTIN2 * MEALS2 * NREGS2, data = fastfood)
# summary(mod)
# summary(mod2)
# summary(mod3)
# summary(mod4)
# anova(mod, mod2)
# anova(mod2, mod3)
# anova(mod3, mod4)
# plot(mod4)
#summary(mod3)


keepInd <- match(c("STATE", "EMPFT", "EMPPT", "NMGRS", "WAGE_ST", "HRSOPEN", "PSODA", "PFRY", "PENTREE", "STATUS2", "EMPFT2", "EMPPT2", "NMGRS2", "WAGE_ST2", "HRSOPEN2", "PSODA2", "PFRY2", "PENTREE2", "NREGS11", "NREGS112", "PCTAFF"), names(fastfood))

fastfood <- fastfood[, keepInd]
dim(na.omit(fastfood))
