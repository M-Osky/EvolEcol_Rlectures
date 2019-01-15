#################################
#####  BASIC R INTRODUCTION  ####
#################################


#CHECK WORKING DIRECTORY
getwd()

# SET WORKING DIRECTORY
setwd("D:/Name_of_your_path/directory")    #Use 'Tab'

# OPEN INPUT FILE
dataset = read.table("Filenamehere", sep = ',', header =TRUE)	#read the data into a table, read headers 	#Use 'Tab'

# CHECK INPUT FILE
str(dataset)      #General structure and organisation
head(dataset)     #First few lines
tail(dataset)     #Last few lines


######## FIRST STATISTICAL ANALYSIS ################


# EXTRACT VARIABLES FROM A DATA MATRIX      
# First we extract one of the variables we want to analyse.
# We will analyse differences in Head Height

HHght <- dataset[[6]]   #Save one variable = column 6
HHght

# Then we extract one of the grouping variables
# We are interested in analyse differences between the four groups defined by Island and population


groups <- dataset[[]] #Save one grouping variable = column 5
groups

# EXTRACT HEADERS (Useful for plots and to tell loops which variables to analyse)

names		# NEW COMMAND #

names(dataset) 		#Just to check the headers

# Save only the names of the variables (not the grouping variables)
varinfo <- dataset[c(6:10)] #Save a range of variables as a different "Data object" 	#Columns 6 to 100
varnames <- names(varinfo)
varnames      #Check



##### BASIC ANalysis Of VAriance (ANOVA or AOV) 

aov		# NEW COMMAND #

results_aov = aov(HHght~groups, data=dataset) #Perform the basic ANOVA



############ DISPLAY RESULTS################

# Analysis Usually have lots of different results, so is quite exhausting to go throught all of them

summary		# NEW COMMAND #

# Print on screen
print(summary (results_aov))		#Just to view results

# Save them in a "Data object"
ANOVA <- summary(results_aov)		#Better approach if we are going to do post-hoc analysis with those results
ANOVA

# Capture output of an analysis

capture.output		# NEW COMMAND #

aov_out <- capture.output(summary(results_aov))		# Best approach if we want to save them as a text file
aov_out






########  SAVING OUTPUTS FROM R: DATA  #########

help(cat)

# Options
# 1) Text or 'object' to print
# 2) Name of the file to save


# First try with some text
cat("## ANOVA OUTPUT ##", file="ANOVAout.txt")

#Then try with the whole ANOVA output
cat(results_aov, file="ANOVAout.txt")

#########






#We need to print text or vectors (lists of variables or lists of text strings)

# More options
# 3) How we want to separate each element of the list ('\n' '\t' ' ' ';')
# 4) If the file exist should R overwrite it or append the text bellow?

# Try again
cat("\t## ANOVA OUTPUT ##\n\n", file="ANOVAout.txt")  #The file was deleted by the previous failed command
cat(aov_out,                    file="ANOVAout.txt", sep="\n", append=TRUE)




#################################################################################################


	#		QUESTIONS AND DOUBTS ABOUT THE ANALYSIS:
			# Are there significant differences?
			# Are they due to sex or two the population differences?
			# How can we find out that?
			##
		
	

	
		##
		# Pair-wise comparation?
		# Subsets of data?
		# Using Pop or Sex as a grouping variable in ANOVA?


################################################################################################


# We can repeat the analysis with a subset of the data

PK <- dataset[which (dataset$Pop =="PK"),]		#Select only the data in which the Pop variable equals "PK"
HHpk <- PK[[6]]
HHpk

sexpk <- PK[[3]]
sexpk

resultsPK_aov = aov(HHpk~sexpk, data=PK)
summary(resultsPK_aov)


################################################################################################


	#		QUESTIONS AND DOUBTS ABOUT THE ANALYSIS:
			# Are there significant differences?
			# Now we know if there are significant differences between one of the islands sexes
			# Is this what we wanted to know?
			##
		
	

	
		
		##
		# Is interesting, althought we already knew there was sexual dimorphism
		# We will need to check a subset of only males or only females



######### PICK ONE OF THE SEXES (1 or 0) AND SAVE A SUBSET
		# Extract one of the variables: HHgth (6) or any other (until 11)
		# Select the grouping variable that refers to the island (Pop)
		# run an ANOVA and share the results
