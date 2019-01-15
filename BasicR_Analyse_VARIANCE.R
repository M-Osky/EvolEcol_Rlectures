
#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###          NOW THAT WE (SHOULD BE ABLE TO) UNDERSTAND THE ANALYSIS, LET'S CHOOSE THE RIGHT ONE      ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################

# PROCESS
######################################################
######  0) What do we want to know?                  #
######  1) Which type of data do we have?            #
######  	  How many degrees of freedom?           #
######  	  Which is the distribution of your data #
######  2) Descriptive statistics / plots            #
######  3) Tests and its limitations                  #
######################################################
# *We are skipping here the point "2" to adjust the time of the lesson to 1 hour
#	and because we will go further into that in next lessons



###########



# 0) What do we want to know?  	  <--This is "0" because we should think this in advance when planning the experiment
#	Is there significant differences between groups?



############## check if your data is Normal

# SET WORKING DIRECTORY
setwd("D:/Dropbox/MOSKY/CURRO/CLASS")    #Use 'Tab'

# OPEN INPUT FILE
dataset = read.table("Dataset_log_RAW.csv", sep = ',', header =TRUE)	#read the data into a table, read headers 	#Use 'Tab'

# CHECK INPUT FILE
str(dataset)      #General structure and organisation


# EXTRACT VARIABLES FROM A DATA MATRIX      
# First we extract one of the variables we want to analyse.
# We will analyse differences in Head Height

HHght <- dataset[[6]]   #Save one variable = column 6
HHght

# Then we extract one of the grouping variables
# We are interested in analyse differences between the four groups defined by Island and population


groups <- dataset[[5]] #Save one grouping variable = column 5
groups

# Check distribution
install.packages("fitdistrplus")
library(fitdistrplus)

# Analyze Normality
descdist(HHght, boot=1000)



# Chek distribution of each group
# Extract dataset for each group
PK <- dataset[which (dataset$Pop =="PK"),]
PM <- dataset[which (dataset$Pop =="PM"),]
PMm <- dataset[which (dataset$Group =="PM-M"),]
PMf <- dataset[which (dataset$Group =="PM-F"),]
PKm <- dataset[which (dataset$Group =="PK-M"),]
PKf <- dataset[which (dataset$Group =="PK-F"),]



# Extract the variable from each group

# Population
testPK <- (PK$HHght)
testPM <- (PM$HHght)

# Group
testPKm <- (PKm$HHght)
testPKf <- (PKf$HHght)
testPMf <- (PMf$HHght)
testPMm <- (PMm$HHght)



# SHAPIR-O'NEAL TEST OF NORMALITY

#Island
shapiro.test ((testPK))
shapiro.test ((testPM))

#Group
shapiro.test ((testPMm))
shapiro.test ((testPMf))
shapiro.test ((testPKm))
shapiro.test ((testPKf))



######################################

# DEGREES OF FREEDOM > 1

# ANOVA -- NORMAL
results_aov = aov(HHght~groups, data=dataset)
summary(results_aov)

# KRUSKAL-WALLIS - NON PARAMETRIC
kwout=kruskal.test(HHght~groups,data=dataset)
kwout


#PAIR TEST (DEGREES OF FREEDOM = 1)

# WILCOXON-MANN-WHITNEY
pairWMW = pairwise.wilcox.test(HHght, groups, p.adjust.method = "bonferroni")
pairWMW

#T TEST
pairTtest = pairwise.t.test(HHght, groups, p.adjust.method = "bonferroni")
pairTtest


