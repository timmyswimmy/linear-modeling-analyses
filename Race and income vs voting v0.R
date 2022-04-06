 ################################
#Statistics 151A: Final Project#
#     Timothy Wang 23073372    #
################################

################################
#########  Question 1  #########
################################
{
	######################################
	##### Set-up and Reading in Data #####
	######################################
	
	require(foreign)
	require(ggplot2)
	setwd("/Users/timwang/Desktop/Stat 151A/Final Exam")
	annen2004 <- read.dta("annen2004_processed.dta")
	summary(annen2004)
	dim(annen2004)
	
	#########################
	##### Data-cleaning #####
	#########################
	
	cand <- !is.na(annen2004$votechoice) # only Bush and Kerry votes
	annen2004 <- annen2004[cand,]
	#summary(annen2004)
	cand <- !is.na(annen2004$income) # no NA income
	annen2004 <- annen2004[cand,]
	#summary(annen2004)
	cand <- annen2004$race == "white" | annen2004$race == "black" # only black and white race
	annen2004 <- annen2004[cand,]
	#summary(annen2004)
	names(annen2004)
	dim(annen2004)

	################################################
	#####        EXPLORATORY ANALYSIS          #####
	################################################
	### Distribution of income across race types ###
	########  FIGURES 1.1 - 1.3 IN TEXT  ###########
	################################################
	
	##########################
	### FIGURE 1.1 IN TEXT ###
	##########################
	
	dataWhite <- annen2004[annen2004$race == "white",]
	dim(dataWhite)
	whiteCount <- sapply(1:5, function(x){sum(as.numeric(dataWhite$income) == x)})
	whiteCount2 <- sapply(0:1, function(x){sum(as.numeric(dataWhite$votechoice) == x)})
	whiteProp <- whiteCount / sum(whiteCount)
	whiteProp2 <- whiteCount2 / sum(whiteCount)
	whiteProp # income distribution for whites
	# [1] 0.18099573 0.11815078 0.37650071 0.25081081 0.07354196
	whiteProp2 # voting distribution for whites
	# [1] 0.4547368 0.5452632

	dataBlack <- annen2004[annen2004$race == "black",]
	dim(dataBlack)
	blackCount <- sapply(1:5, function(x){sum(as.numeric(dataBlack$income) == x)})
	blackCount2 <- sapply(0:1, function(x){sum(as.numeric(dataBlack$votechoice) == x)})
	blackProp <- blackCount / sum(blackCount)
	blackProp2 <- blackCount2 / sum(blackCount)
	blackProp # income distribution for blacks
	# [1] 0.30803048 0.14185229 0.35111372 0.16119578 0.03780774
	blackProp2 # voting distribution for whites
	# [1] 0.9021102 0.0978898
	
	par(mfcol = c(1,2))
	##########################
	### FIGURE 1.2 IN TEXT ###
	##########################
	
	tograph <- data.frame(t(as.matrix(data.frame(whiteProp, blackProp))))
	names(tograph) <- c("Under $10,000", "$10,000 - $15,000", "$15,000 - $25,000", "$25,000 - $35,000", "$35,000 - $50,000")
	
	barplot(as.matrix(tograph), main="Figure 1.2: Proportion of Population in Income Groups", ylab="Proportion of Population", beside=TRUE, col=c("white", "black"), xlab = "Income Level")
	legend(13, 0.35, c("Whites", "Blacks"), cex=0.6, fill=c("white", "black"))
	
	##########################
	### FIGURE 1.3 IN TEXT ###
	##########################
	
	tograph2 <- data.frame(t(as.matrix(data.frame(whiteProp2, blackProp2))))
	names(tograph2) <- c("Democrat", "Republican")
	
	barplot(as.matrix(tograph2), main="Figure 1.3: Proportion of Population by Party Voted", ylab="Proportion of Population", beside=TRUE, col=c("white", "black"), xlab = "Party")
	legend(5, 0.85, c("Whites", "Blacks"), cex=0.6, fill=c("white", "black"))
	
	## Pairs Plot
	{
		inds <- match(c("race", "income", "state", "votechoice"), names(annen2004))
		# pairs(annen2004[,inds]) # Not useful, try to de-factorize income
	}

	## De-factorize income
	{
		base <- c(0, 10000, 15000, 25000, 35000)
		ind <- as.numeric(annen2004$income)
		baseInc <- base[ind]
		inc <- c(10000, 5000, 10000, 10000, 15000)
		incInd <- inc[ind]
		randInc <- runif(length(incInd), 0, incInd)
		randAmt <- baseInc + randInc
		annen2004$income2 <- randAmt
		summary(annen2004)
		
		## Median incomes
		med <- c(5000, 12500, 20000, 30000, 42500)
		annen2004$medIncome <- med[ind]
	}
	
	## Pairs Plot, Round 2!
	{
		inds <- match(c("race", "income2", "state", "votechoice"), names(annen2004))
		#pairs(annen2004[,inds]) # Not useful, try log income
		annen2004$logIncome <- log(annen2004$income2)
		inds <- match(c("race", "logIncome", "state", "votechoice"), names(annen2004))
		#pairs(annen2004[,inds]) # Not useful, try to de-factorize income
	}
	## Pairs Plot is not useful...
	
	## Explore the 3 states: CT - 7, OH - 35, MS - 24
	{
		
		##########################
		##   FIGURE 1.4 IN TEXT ##
		##########################
		
		## Get distribution of income levels in 3 model states
		nonNA <- annen2004[!is.na(annen2004$state),]
		levs <- 1:5
		ctData <- nonNA[nonNA$state == 7,]
		ctCount <- sapply(levs, function(x){sum(as.numeric(ctData$income) == x)})
		ctProp <- ctCount / sum(ctCount)
		ohData <- nonNA[nonNA$state == 35,]
		ohCount <- sapply(levs, function(x){sum(as.numeric(ohData$income) == x)})
		ohProp <- ohCount / sum(ohCount)
		msData <- nonNA[nonNA$state == 24,]
		msCount <- sapply(levs, function(x){sum(as.numeric(msData$income) == x)})
		msProp <- msCount / sum(msCount)
		ctProp
		# [1] 0.14621410 0.06527415 0.30026110 0.33942559 0.14882507
		ohProp
		# [1] 0.19896373 0.12227979 0.40569948 0.22849741 0.04455959
		msProp
		# [1] 0.26283988 0.14199396 0.36253776 0.18126888 0.05135952
		
		## Classify all states
		summary(factor(annen2004$state)) ## State numbers 2 and 11 are missing.
		stateInd <- 1:50
		stateInd <- stateInd[c(-2, -11)]
		classification <- rep(0, times = 50) # 1 = rich, 2 = middle, 3 = poor
		for(i in stateInd) ## Use a chi-square-type approach to classify.
		{
			stateData <- nonNA[nonNA$state == i,]
			stateCount <- sapply(1:5, function(x){sum(as.numeric(stateData$income) == x)})
			stateProp <- stateCount / sum(stateCount)
			x1 <- sum((stateProp - ctProp)^2 / ctProp) # (O-E)^2 / E
			x2 <- sum((stateProp - ohProp)^2 / ohProp)
			x3 <- sum((stateProp - msProp)^2 / msProp)
			finzzz <- c(x1, x2, x3) # Take the smallest X^2 statistic.
			classification[i] = match(min(finzzz), finzzz)
		}
		annen2004$classification <- factor(classification[annen2004$state])
		inds <- match(c("race", "classification", "state", "votechoice"), names(annen2004))
		pairs(annen2004[,inds]) # Still not very useful.
				
		##########################
		##   FIGURE 1.5 IN TEXT ##
		##########################
		
		unique(annen2004[!is.na(annen2004$classification) & annen2004$classification == 1,]$cst) # Rich states
		# [1] "NY" "CA" "VA" "MA" "MD" "NJ" "CT" "NH"
		
		unique(annen2004[!is.na(annen2004$classification) & annen2004$classification == 2,]$cst) # Middle states
		#  [1] "NC" "WI" "IL" "MO" "FL" "WY" "ME" "PA" "NV" "OH" "MI" "GA" "TX" "MN" "CO" "IN" "AZ" "DE" "LA" "IA" "KS" "RI"
		# [23] "OR" "UT" "WA" "NE" "VT" "SD"
		
		unique(annen2004[!is.na(annen2004$classification) & annen2004$classification == 3,]$cst) # Poor states
		#  [1] "ND" "TN" "OK" "MS" "SC" "KY" "AL" "NM" "WV" "AR" "ID" "MT"
		
		length(unique(annen2004[!is.na(annen2004$classification) & annen2004$classification == 1,]$cst)) # Rich states
		length(unique(annen2004[!is.na(annen2004$classification) & annen2004$classification == 2,]$cst)) # Middle states
		length(unique(annen2004[!is.na(annen2004$classification) & annen2004$classification == 3,]$cst)) # Poor states
		
		

	}
	
	####################
	##### ANALYSIS #####
	####################
	
	## GLM's and ANOVA
	{
		require(car)
		annen2004$state <- factor(annen2004$state)
		voteNull <- glm(votechoice~race+classification+state+income, family = "binomial", data = annen2004)
		voteAlt2 <- glm(votechoice~race + classification + state + income, family = "poisson", data = annen2004)
		anova(voteNull, voteAlt2, test = "LRT") # Binomial VS Poisson
		
		
		voteAltNoRaceState <- glm(votechoice~classification*income, family = "poisson", data = annen2004)
		voteAltNoRace <- glm(votechoice~classification*income + state, family = "poisson", data = annen2004)
		voteAltRace <- glm(votechoice~race + classification*income + state, family = "poisson", data = annen2004)
		curious <- glm(votechoice~race + state, family = "poisson", data = annen2004)
		voteAltRaceInt <- glm(votechoice~race*classification*income + state, family = "poisson", data = annen2004)
		
		
		##########################
		##   FIGURE 1.6 IN TEXT ##
		##########################
		
		anova(voteAltNoRaceState, voteAltNoRace, voteAltRaceInt, voteAltRace, curious, test = "LRT") # Comparisons: Adding state, adding race, adding race interaction, removing income-classification interaction
		
		
	}

}
