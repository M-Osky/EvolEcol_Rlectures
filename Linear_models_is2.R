#################################
#####  GENERAL R LINEAR MODELS ####
#################################

#CHECK AND SET WORKING DIRECTORY
getwd()
setwd("D:/Dropbox/EvolEco")    #Use 'Tab'

#install if needed with install.packages("fitdistrplus")
library("fitdistrplus")

# OPEN AND CHECK INPUT FILE
dataset = read.table("filename.csv", sep = '', header = )	#read the data into a table, specify the separator (comma), read headers (TRUE) 	#Use 'Tab'
str(dataset)      #General structure and organisation


# EXTRACT VARIABLES FROM A DATA MATRIX      

#Choose a couple of continuous response variables to extract
BiteF <- dataset[[2]]   #Save one variable = Bite Force

######
choose_one_variable <- dataset [[number of the column]]    #Save one variable = Choose one between Head Height (4) and Tail Length (10)
choose_one_variable #check it
######

TLength <- dataset[[11]] #Save total length
TLength

Sex <- dataset[[13]] #Save one grouping variable Sex
Sex

Pop <- dataset[[14]]   #Save one grouping variable Pop
Pop

Group <- dataset[[12]]   #Save one grouping variable Pop
Group


# CHECK GLOBAL DISTRIBUTION
descdist(your_variable, boot=1000)
descdist(BiteF, boot=1000)
descdist(TLength, boot=1000)

# CHECK NORMALITY of the response variable(s)

# Make a subdataset with all the variables for each group
Fem <- dataset[which (dataset$Sex =="F"),]
Mal <- dataset[which (dataset$Sex =="M"),]


## Do the same with PK and PM (Pop)##
PM <- dataset[which (dataset$Pop==....]
PK <- dataset[which ....]

######

# Extract the variable from each group

# Sex
testFem <- (Fem$LtHip)	#Do this for your variable
testMas <- (Mal$LtHip)

#Pop
#Do the same for your variable in each population
testPK <- (PK$
testPM <- (

# Shapir-O'Neal test of normality

#Sex
shapiro.test ((testFem))
shapiro.test ((testMas))

#Pop
shapiro.test ((testPK))
shapiro.test ((testPM))



###LINEAR MODELS###

#function EXAMPLES:
model <- lm(y~x, data=dataset) #y is a response variable and x is a explanatory variable
model <- lm(y~x1+x2, data=dataset) #y is a response variable and x1 and x2 are explanatory variables, we test their effect in y
model <- lm(y~x1*x2, data=dataset) #this model will evaluate the effect of x1 and x2 in y and their interaction
summary(model)



#Response variable continuous, one discrete explanatory variable --> One-way ANOVA
ow_aov <- lm (TLength~Sex, data=dataset)	#Example
ow_aov <- lm (variable~Sex, data=dataset)	#Try with your variable as response variable
#To see the results:
summary(ow_aov)

#Response variable continuous, multiple discrete explanatory variables --> Multi-way ANOVA
mw_aov <- lm (TLength~Pop*Sex, data=dataset)	#Example
mw_aov <- lm (variable~Pop*Sex, data=dataset)	#Try with your variable as response variable
summary(mw_aov)
#Response variable continuous, one continuous explanatory variable --> Linear Regression 
lr <- lm (BiteF~TLength, data=dataset)	#Example
lr1 <- lm (variable~TLength, data=dataset)	#Try with your variable as response variable
lr2 <- lm (BiteF~variable, data=dataset)	#Try with your variable as explanatory variable
summary()

#Response variable continuous, two continuous explanatory variables --> Multiple Regression 
mr <- lm (BiteF~TLength*variable, data=dataset)	#Try with your variable as explanatory variable

#Response variable continuous, multiple explanatory variables, discrete and continuous --> ANCOVA
acov <- lm (BiteF~Group*TLength, data=dataset)	#Example
acov1 <- lm (variable~Group*TLength, data=dataset)	#Try with your variable as response variable
acov2 <- lm (BiteF~Group*variable, data=dataset)	#Try with your variable as a explanatory variable



###  OUTPUT  ###

# MODEL USED

# RESIDUALS: distance from the modeled data (line)

# COEFFICIENTS
# Proportion / average differences of each variable in each treatment
# On average the indiv from the group shown have a value of the variable e times the average of the other group
# e = estimated value shown, control group = intercept value
# p-value for the differences against the intercept (usually not helpful)

# R^2 <- measure of the adjustment: how many of the variance in our data is explained by the model
# Statistic calculated, degrees of freedom, overall p-value


##################################################################################################
################	   Was any of the response variables Normal?	   ###########################
################	   Which model was better fitted?				   ###########################
################	   Was any of the analysis significant?			   ###########################
##################################################################################################






###########################################################################
#########################     FINALLY GLMs     ############################
###########################################################################


#install.packages("car")
library(car)
#install.packages("boot")
library(boot)

# Choose a response variable, ANY, Continuous, categorical, discrete, binary...

#Choose to which family it belongs

#REMEMBER

#   DATA									  		FAMILY
# Normal Distribution						->		"gaussian"
# Not Normal skewed									"inverse.gaussian"
# Positive continuous data, no zeros		->		"Gamma"
# Gamma distribution						->		"Gamma"
# Binary data (y/n)							->		"binomial"
# Proportions (3:1)							->		"binomial"
# Logistic distribution						->		"binomial"
# Counts									->		"poisson"
# Categorical (SD= ȳ)						->		"poisson"
# Poisson distribution						->		"poisson"
# Log Normal distribution					->		"poisson"
# Other categorical							->		negative.binomial(1)


# If you are not sure to which family belongs your data, check its distribution

#FASTEST WAY: fit a distribution with "fitdistr()" and check it adjustment to the data
fit1<-fitdistr(variable, "normal")   	#This is the default method, but it may output an error depending on your variable characteristics if some values don not fit. fitdistr can't handle NA's

#fit the distributions using na.omit to overcome the values that can not be computed
#Probably you will not be able to fit a variable in all the distributin types: "beta", "cauchy", "chi-squared", "exponential", "gamma", "geometric", "log-normal", "lognormal", "logistic", "negative binomial", "normal", "Poisson", "t" and "weibull"
#Try some of them depending on your data
fit1<-fitdistr(na.omit(variable), "normal")
fit2<-fitdistr(na.omit(variable), "lognormal")
fit3<-fitdistr(na.omit(variable), "Poisson")
fit4<-fitdistr(na.omit(variable), "negative binomial")
fit5<-fitdistr(na.omit(variable), "Gamma")
fit6<-fitdistr(na.omit(variable), "Logistic")

# If your data has negative values or zeros and fitdistr can not handle it, add +1 or +5 to the variable
fit5<-fitdistr(na.omit(variable+2), "Gamma")


#check the Akaike Information Criterion and choose the best fitted model (lowest).
AIC(fit1,fit2,fit3,fit4,fit5,fit6)	# for the distributions you managed to fit, obviously not all of them are going to fit

# DISCLAIMER: fitdistr() can not fit all the possible distributions that glm() handles; glm() can not handle all the distribution that fitdistr() handles.
# For the sake of not making this overly complex adding more packages and commands we will handle just a few distributions/families (the table above) and your data may not fit them very well.

# GLM
#Now that you know to which distribution your variable belongs, Run the glm with the right family. Families: binomial, gaussian, Gamma, inverse.gaussian, poisson, quasi, variance, quasibinomial, quasipoisson, 
#function:
glm(response_variable~ExplanatoryVariable1*ExplanatoryVariable2, family="chosen family", data=dataset)


#################################
########   GLM OUTPUT    ########
#################################


# Is the data well fitted? Is it over dispersed? Are there some interesting significant interactions?
#Check the p-values from the coefficients for interactions
summary(glm_out)

#Check if data is overdispersed
#Is residual deviance smaller than residual degrees of freedom * 2 ??
glm1$deviance
glm1$df.residual

#Is the model well fited? Does it explain most of the variance?

glm1$null.deviance
Rsquar <- ((glm1$null.deviance - glm1$deviance) / glm1$null.deviance)
Rsquar

# So are there significant differences between groups? differences?
# Now that all the data is transformed and fitted to the model, we can do a simple ANOVA to have a quick p-value
summary(aov(glm_out))


#Do more glms trying different response variables and diferent explanatory variables, careful when choosing the family
#Try non continuous variables
#If you test the same variables than with lm() compare the results

########################################################################

#################################
########   PLOT    ########
#################################



#Once you have analyced various combinations of continuous and categorical variables, and checked the goodness of fit to the model and their effect (significant or not):
# PLOTS

plot(glm1)
plot(Sex~Pop, datset, col=c("black","red2"))

#TRY TO MAKE NICE PLOTS, change the plot type, change the colors, tags, add error bars, etc. Use internet

https://www.rdocumentation.org
https://www.r-bloggers.com
https://stackoverflow.com/questions/tagged/r

mypalette <- c("purple3", "goldenrod1", "firebrick3", "darkgreen")
plot(dataset$HHght~dataset$Group, col=mypalette, xlab="Groups from each island and sex", ylab="HHght", main="Italian wall lizard head height")		#Plot











