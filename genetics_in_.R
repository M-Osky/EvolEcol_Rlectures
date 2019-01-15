#R sucks!
# Adegenet basics and other genetic packages
# Packages for population genetics




#FIRST, IF YOU DIDN't TRY TO INSTAL THIS TOOLS THAT WILL HELP TO IMPLEMENT SOME ADVANCED OPTIONS
# Let's see if we can download and install Rtools in our computer, a program to compile R packages
#	https://cran.r-project.org/bin/windows/Rtools/ 
# If it worked, try to execute this
#install.packages("devtools")
library("devtools")   	#multiporpouse package with plenty tools and for handling other packages
#Also if you didn't, install this dependencies
# devtools::install_github('wrengels/HWxtest', subdir='pkg')

#THEN THE GENETIC PACKAGES (Some of them may need to have installed and loaded the above tools)

#install.packages("adegenet")
#install.packages("adegraphics")
library ("adegenet")   	#main genetic package
library("adegraphics")   	#graphic outputs for adegenet
#install.packages("poppr")
library("poppr")   	#a package for population-wise genetic analysis
#install.packages("diveRsity")
library("diveRsity")   	# A new package with lots of new functionalities, still under development
#install.packages("pegas")
library("pegas")   	# We will use it mainly for testing HWeq


#install.packages("ggplot2")
library("ggplot2")   	#Your favourite multi purpose graphic package


setwd("D:/Dropbox/MOSKY/CURRO/PODARCIS/PopGenet/Adegenet")
##############################
####   INPUT FILE CHECK   ####
##############################

#Learn the structure of a STructure input file (a widely used format and program)
#Open the input file to check how many SNPs, samples, etc.
dataset = read.table("lizardSNPs.str", sep = '\t', header =TRUE)
str(dataset)

#If too long (too many SNPs, we can reduce it to see only the first 10 columns)
str(dataset, list.len=10)

#rows x columns
dim(dataset)

#How many extra columns with meta-data there are? Sample name and population name
#How many SNPs? The rest of columns
ncol(dataset)

#Samples are coded by rows
#Number of samples is not number of rows!
#Number of samples = levels of first variable
dataset$X
levels(dataset$X)
samplelist <- levels(dataset$X)
length(samplelist)

#Populations
dataset$X.1
levels(dataset$X.1)

#we will need this
populationnames <- levels(dataset$X.1)

#Now that we know how is our input file we need to import it in Adegenet format (GENIND)





###############################
####   INPUT FILE IMPORT   ####
###############################

#Adegenet an many genetic packages for R don't use dataframes as input files, they use files that handle the allelic frequencies
#Most of them use genind format, we will need to transform our data to this format

#input file, open from Genepop format (a widely used format for genetics software)
genind_genepop <- read.genepop("LizardSNPs.gen", ncode=3L)   	#we only need to tell them how many digits per allele are coded. Open file in notepad to check

#we could import it also from the previous format (Structure) but we will need to provide a lot of information
# row.marknames= Is there a row with marker names, in which position?; onerowperind= Is there one row per individual?
# n.indiv= How many individuals?; n.loc= Number of loci?; col.lab= Is there a column with labels for the individuals, which one?
# col.pop= Is there a column with labels for the populations, which one? NA.char= How are the missing values coded?; ask= Do you want to be asked about the output file format?
rawgenind <- read.structure("lizardSNPs.str", row.marknames=1, onerowperind=FALSE, n.ind=124, n.loc=500, col.lab=1, col.pop=2, NA.char = "0", ask=FALSE)


#Compare both imports
rawgenind
genind_genepop
The only difference should be this
rawgenind@pop
genind_genepop@pop
#Genepop takes as population name the first sample tag from each population but it requires less parameters to import

#check
is.data.frame(rawgenind)
str(rawgenind, list.len=10)
# the genind object is not a dataframe it has multiple levels to which we can acces through @ and $
#loci names and alleles
rawgenind@all.names
rawgenind@all.names$"1423_27"

#See information that could be useful for plots or analysis
nInd(rawgenind)
indNames(rawgenind)
nLoc(rawgenind)
locNames(rawgenind)
alleles(rawgenind)
ploidy(rawgenind)
pop(rawgenind)


# PLOT SOME OF THE FILE INFO

#palette (c("darkorange3", "darkgreen", "orange", "chartreuse3", "black", "red1", "goldenrod1", "purple3", "blue", "magenta"))
mypalette <- c("black", "red1", "goldenrod1", "purple3", "darkorange3", "darkgreen", "blue", "magenta", "orange", "chartreuse3")
#myshapes=c(15:18)

#Let's plot how many samples there is of each population
#we need to count how many times each pop name appears
#let's save the population rates as a table for later use
#table() will make a table by groupin values that repeat themselves
popnames <- table(pop(rawgenind))
popnames #grouped as a table

barplot(popnames, col=mypalette, xlab="Population", ylab="Sample size")

#Default Y axis is too short, if we feel perfectionist we can fix it
#Check the table, max value is 19, let's give it a range from 0 to 20
barplot(popnames, col=mypalette, las=3, xlab="Population", ylim = c(0,20), ylab="Sample size")


#summarize the info:
# We can save some basic statistics and info from our genind file in a different file
sum_rawgenind <- summary(rawgenind)
sum_rawgenind
#The structure is different, it has some indexes calculated and some per population descriptors
str(sum_rawgenind)





###########################
###  Allelic diversity  ###
###########################
# ALLELIC RICHNESS

#Number of alleles
sum_rawgenind$pop.n.all
barplot(sum_rawgenind$pop.n.all, col=mypalette, xlab="Population", ylab="Number of alleles")

#Why number of alleles and not Allelic Richness (Ar)?

# Allelic richness is related to the total alleles
# the number of alleles is highly influenced by the number of samples. 
# Solution: Use rarefaction techniques that account for that differece

################################# FOR THIS PART WE WILL NEED THE PACKAGE diveRsity
################################# its kind of Beta and requires RTools, so may not work



#It will calculate not only the Ar but other indexes
#Needs genepop file, we want Confidence Intervals for Fis and Ar, for that we bootstrap 999 iterations,
#we want Rarefaction to correct for the number of samples, the p-value is going to be tested for 0.05 as usual
loadofstats<- basicStats(infile="LizardSNPs.gen", fis_ci=TRUE, ar_ci=FALSE, fis_boots=99, mc_reps=99, rarefaction=TRUE, ar_alpha=0.05, fis_alpha=0.05)
loadofstats

#Lots of data, but right now we will only check the overall indexes (per population)
str(loadofstats$main_tab, list.len=5)
str(loadofstats$main_tab$PK01, list.len=501)
str(loadofstats$main_tab$PK01$overall)

#lapply -> shows the results of aplying the same function to a list of arguments
# lapply(x="object_with_the_list_of_arguments", "description of how to apply the function("object_with_the_list_of_arguments")" the_actual_function)

# not function provided, is just going to read the values that fit the formula
#so extract the "overall" slot from each population
totals <- lapply(loadofstats$main_tab, function(x) (x)$overall) #Store the results in an object called "totals"


totals

#Let's assign the right names to the populations
names(totals) <- levels(dataset[listrows,2])
names(totals) <- populationnames
totals # better

class(totals)

#lapply generates a list of results but we need a proper dataframe to be able to plot it with the tags and everything
#transform the output of lapply to a proper dataframe
total.df <- as.data.frame(totals)
total.df
class(total.df)

#problem, it didn't keep the name of each index (just numbers)
#now we need to identify each value with the index it belongs to
#those names are stored for each population in the output
loadofstats$main_tab$PK01
rownames(loadofstats$main_tab$PK01)
indexnames <- rownames(loadofstats$main_tab$PK01)	#save it as a list of names
indexnames

#add he column names
total.df
rownames(total.df)
rownames(total.df) <- indexnames
total.df	#cool!

str(total.df)
#more problems, the dataframe has the population as variables and the variables as observations
#R will not understand this

#we need to translocate (swap columns by rows)
#use t() and save the output to a new object
indexes <- t(total.df)

#let's see the allelic richness
indexes
class(indexes)
#is a matrix, not a dataframe, we can not access the variables
#let's transform it to data frame so we can access specific variables and add names
indexes$ar   	#<-- ERROR

indexs.df <- as.data.frame(indexes)
indexs.df
indexs.df$ar
#sUPER

#save allelic richness
AR <- indexs.df$ar
AR
#add population names to it
names(AR) <- populationnames
AR
#plot
barplot(AR, col=mypalette, xlab="Population", ylab="Allelic Richness")

# Compare with the previous plot




#   	BEFORE CONTINUING
########################################
####   HARDY-WEINBERG EQUILIBRIUM   ####
########################################

# Are all the loci valid?
#######################################################################################################################
# CONDITIONS MODEL


#Hardy-Weimberg equilibrium: large population, random breeding, no population substructure...
# Important because most of the test for population structure (geographical) will asume the markers are
# neutral and randomly distributed

# We will test it using the package "pegas"

#Do the test
allHWpval <- hw.test(rawgenind, B=99)

#check output
head(allHWpval, n=10L)
str(allHWpval)

#weird structure? How to know how many are significant?
# Usually we will check the p-values of HW like this:
allHWpval$Pr.exact
#unsurprisingly the HWeq test of a gengind object is not a dataframe

is.data.frame(allHWpval)
is.atomic(allHWpval)
# Is a vector, we can not access variables with "$"

#transform
allHWtable <- as.data.frame(allHWpval)
head(allHWtable)
str(allHWtable)
is.data.frame(allHWtable)

#Now extract the significant p-values from the dataframe
#we use the which to extract tto make a subset, but now instead of the rows which are "male"/"female" or "PK"/"PM"
#we extract the rows wich have a value below 0.05 to a new subset
sigNOHW <- allHWtable[which (allHWtable$Pr.exact <0.05),]
#check the number of rows of the new dataframe to know how many loci
nrow(sigNOHW)
#SO MANY?!?

#For pure population structure analysis we need random markers no under selection (HW) not population structure!
# Is this logical? May be a little bit paradoxical?
# We need to test HW in a subset that we know there is not structure...

# How should we do that?
# SHORT ANSWER: YES, ALWAYS
# Realistic answer: Not now, let's just proceed for now for the sake of the class.
########################################################################################################################



########################################
####   NOW WITH ALL THE LOCI IN HW:   ####
########################################

#Lets make a summary table of exploratory indexes with poppr package
# This time with a more broad approach of genetic diversity
# This is a thorough analysis done population by population and overall
# sample= permutations to calculate the p-values; missing= which value input when values missing, cutoff= which proportion of missing is accepted
# quiet= not print any warning or message on screen; plot= plot; minsamp0 minimum number of samples per population allowed; legend = legend
popprout <- poppr(rawgenind, sample = 99, method = 2, missing = "mean", cutoff = 0.2, quiet = FALSE, plot = TRUE, minsamp = 6, legend = TRUE)
popprout
# Every index is briefly explained in the legend, check their values

#Shannon-Wiener and Lambda(Simpsons) seem mostly the same for all the populations...

#Brown's ia= linkage disequilibrium, cloning
# Populations whose members are undergoing clonal reproduction, however, generally do so via mitosis.
# that suposes a random shuffling of alleles
# This means that the most likely mechanism for a change in genotype is via mutation.
# The rate of mutation varies from species to species, but it is rarely sufficiently high to approximate a random shuffling of alleles.
# The index of association is a calculation based on the ratio of the variance of the raw number of differences between individuals and the sum of those variances over each locus
# 

last_plot()

#missing per pop
info_table(rawgenind, plot = TRUE)









#   	PRIVATE ALLELES
#######################

#Plot the ones calculated by Adegenet (no rarefaction)
pa <- private_alleles(rawgenind, report = "data.frame", form=locus~.,count.alleles = FALSE)
pa
ggplot(pa) + geom_tile(aes(x = population, y = locus, fill = count))








#   	HETEROZYGOSITY
######################

#Heterozygosity is an important information to check
#Is given by all the different packages we used
#Including the basic Adegenet summary
#Expected heterozygosity (under HW) is usefull as a way to compared the real (observed) with the ideal (expected)
plot(sum_rawgenind$Hexp, sum_rawgenind$Hobs, pch=1, cex=1, col="red1", xlim=c(0,0.6), ylim=c(0,0.6), xlab="Expected Heterozygosity", ylab="Observed Heterozygosity")
#add a line to see if the propotion is balanced
abline(0,1,lty=2)
#yeah, ok, may be the line was not even necesary

#what does this mean?
#HWe violation (linkage, imbreeding)

#Are they significantly different?
#  Bartlett test of homogeneity of variances
shapiro.test(sum_rawgenind$Hexp)
shapiro.test(sum_rawgenind$Hobs)
bartlett.test(list(sum_rawgenind$Hexp, sum_rawgenind$Hobs))
# Chi^2 test of independence
chisq.test(sum_rawgenind$Hexp, sum_rawgenind$Hobs, simulate.p.value=TRUE, B=99)



#another aproximation to an ideal expected heterozygosity is the within population gene diversity (Hs)
# Let's plot Hs, is equivalent but NOT EQUAL to expected heterozygosity by pop
#gendind objects are tricky, we can not treat it as a table, and just plot the data inside
#Usually to calculate population-wise indexes we will use a genpop object (equivalent indeed to our input file format).

#Transform to genpop format used by lots of classical population genetics packages
rawgenpop <- genind2genpop(rawgenind)
rawgenpop
popNames(rawgenpop)
#compute Hs
GeneDiversityHet <- Hs(rawgenpop)
# 
barplot(GeneDiversityHet, col=mypalette, xlab="Population", ylab="Hs: Within population gene diversity (~He)")


#Compare with the one we calculated at the beginning with "diveRsity"
#unbiased (for the difference in sample size) expected Heterozygosity
indexs.df
indexs.df$uexp_het
uHe <- indexs.df$uexp_het
#add population names
names(He) <- populationnames
#plot
barplot(He, col=mypalette, xlab="Population", ylab="Unbiased  Expected  Heterozygosity")




# OBSERVED HETEROZYGOSITY

# For some reason there is not a straight forward method to plot Ho per pop
# In part because genind compresses data into genetic frequencies and descriptors, therefore is not a dataframe
# we could do this also with "diveRsity" but adegenet requires less additional packages, computational time and so
#Also this way we will learn some new commands to transform data objects

# let's use the Adegenet command "seppop" to split the genind object information per population
per_pop <- seppop(rawgenind)

#check
per_pop
#now we have a data object structured as one genind per population
#Save one population and see
Split <- per_pop$ST
Split
#See the summary for one population
summaryST <- summary(Split)
summaryST
#See the structure of the data from that population
str(summaryST)

#Observed heterozygosity for each locus is stored in Hobs
summaryST$Hobs
#Now we just need to extract this from each population and calculate the mean
#There are some NaN probably from loci that are missing in this population, this may be problematic

#Calculate the mean heterozygosity for each population
#We use lapply again
#lapply (shows the results of aplying the same function to a list of arguments)
#the function this time will be to calculate the mean of each $Hobs value stored for each population:

# -> mean(summary(per_pop$"each population found")$Hobs)
mean(summary(per_pop$PK)$Hobs) #this for all the populations

#list of arguments: each population stored in per_pop
lapply(X=per_pop, function(per_pop) mean(summary(per_pop)$Hobs))
#NaN (Not a Number) some SNPs are missing in the entire population and give infinite.
# We will need to fix this



###### FIX THE MISSING VALUES

# Some functions of some genetic packages (like Adegenet) allow us to replace/input the missing values with the average/mean of the population or with the global mean
# In the case of the SNPs this will be biologically wrong; au contraire to the microsatellites, they are not sizes but alleles (A, T, C, or G)
# we can open the file again but considering the 0s as a different allele/mutation
#This could be right if a SNP is missing in an entire population, as it will code as "something new" , not as "the average".
nomissgenind <- read.structure("lizardSNPs.str", row.marknames=1, onerowperind=FALSE, n.ind=124, n.loc=500, col.lab=1, col.pop=2, NA.char = "-9", ask=FALSE)
nomissgenind
sum_nomissgenind <- summary(nomissgenind)
#check
sum_nomissgenind
#Let's repeat the process
per_pop2 <- seppop(nomissgenind)
#calculate and save the mean for each population
averages <- lapply(per_pop2, function(X) mean(summary(X)$Hobs))   	#Not need to right the object (per_pop or X=per_pop) each time




#the command "c" (concatenate) can be used to combine each value (mean Hobs) to the argument of the lapply function used (population name)
#do.call -> takes a list of arguments and a function and instead of calling them one by one holds a list with all of them
# we will use do.call to run "c" an assemble the results (each pair of population name and mean heterozygosity) in one vector

mean.hobs <- do.call("c", averages)
mean.hobs

#See it
barplot(mean.hobs2, col=mypalette, xlab="Population", ylab="Observed  Heterozygosity")

#Compare with the one we calculated at the beginning with "diveRsity"
indexs.df
indexs.df$obs_het
Ho <- indexs.df$obs_het
#add population names
names(Ho) <- populationnames
#plot
barplot(Ho, col=mypalette, xlab="Population", ylab="Observed  Heterozygosity")









###########################################################
####    GENETIC DISTANCES and POPULATION DIFERENCES    ####
###########################################################


#install.packages("hierfstat")
library ("hierfstat")

#This package will quickly provide some indexes of genetic diversity and genetic structure/distance
basicstats <- basic.stats(rawgenind, diploid = TRUE)
basicstats

#Let's skip the locus-by-locus results, please
basicstats$overall
#Summary table: Ho heterozygosity, Hs within pop gene diversity, Ht over-all gene diversity, Dst among samples gene diversity
# Htp corrected Ht, Dstp corrected Dst, Fst overall genetic structure, Fstp Corrected Fst, Fis inbreeding coefficient,
# Des population differentiation



###   	F STATISTICS: Fis 

#Imbreeding differences per population?
#Check if confidence intervals overlap
Fis <- boot.ppfis(rawgenind, quant=c(0.025,0.975),diploid=TRUE,dig=4)
Fis

# We need to plot only the min and max numbers from Fis (at $fis.ci)
str(Fis)
boxplot(Fis$fis.ci)

#R doesn't understand that the rows are the population and the columns are the variables
#Probably because the row are not labelled

#translocate the data
Fis$fis.ci
FisCI <- t(Fis$fis.ci)
FisCI

#add the population names
colnames(FisCI)
colnames(FisCI) <- populationnames
FisCI
#Now we can plot
boxplot(FisCI, boxwex=0.05, border=0, col=mypalette)

#compare with "diveRsity" calculations
indexs.df
Fisdata <- data.frame(indexs.df$fis_hi, indexs.df$fis_lo)
Fisdata
Fis <- t(Fisdata)
Fis
colnames(Fis) <- populationnames
Fis
boxplot(Fis, boxwex=0.05, border=0, col=mypalette)




###   	Global Fstat
fstat(rawgenind, fstonly=TRUE)

# Is it high? 
# 0 = panmixia
# 1 = no historical relationship


#Is it significant?
#Goudet's G-statistic Monte Carlo test
psic.gtest <- gstat.randtest(nomissgenind, nsim=99)
psic.gtest

#Fst only makes sense comparatively
####	   PAIR WISE FST
#let's then go from overall to pair-wise: genetic distance for each pair of population
pairFst <- pairwise.fst(nomissgenind,res.type="matrix")
pairFst


#Lets see it graphically
table.image(pairFst)
#lame... Let's give it more resolution
table.image(pairFst, nclass=16)

#Difficult to appreciate the genetic distances between each pair? Plot the distribution of Fst values of each population
#Box plot
temp <- pairFst
#The diagonal of the Matrix is just zeros (self-genetic distance). Let's replace with "NA" so it's not plotted
diag(temp) <- NA

boxplot(temp, col=mypalette, xlab="Population", ylab="Fst")

#To appreciate even better the genetic distances between each pair build a tree
library("ape")
psic.tree <- nj(pairFst)	   #NETWORK JOINING TREE
#plot
plot(psic.tree, type="unr", tip.col=mypalette, font=2, main="Genetic distances (pair-wise Fst) tree-plot")
#add a bar to indicate the relationship between the length and the Fst value
add.scale.bar()
#we can also add the Fst values if we wish to have the information, although it may look messy
annot <- round(psic.tree$edge.length,2)
edgelabels(annot[annot>0], which(annot>0), frame="n")


#Outliers?
# Compare with the map, is it what we will expect?


#Some of those Pairwise Fst may not be significant, lets run them with a different method to check and plot that
#This method requires RTools and certain Java configuration. It may not work.

# IF it does work it will take a lot of time, so I'll run it in this computer while you do the exercises

library("rJava")
library("xlsx")
library("sendplot")
install.packages("plotrix")
library("plotrix")

pairFstci <- fastDivPart(infile ="LizardSNPs.gen", outfile = "pairwisestat", gp = 3, pairwise = TRUE, fst = TRUE, bs_locus = FALSE, bs_pairwise = TRUE, boots= 99, plot = TRUE)
pairFstci
diffPlot(pairFstci, outfile = TRUE, interactive = TRUE)







	#######################
	####      PCAs     ####
	#######################


	#PCA in adegenet
#Use the dataset without missing values
#transform it to a scaled matrix of allele frequencies
PCAmatrix <- scaleGen(nomissgenind)
dim(PCAmatrix)
#as many rows as alleles * locus

#Adegenet PCA (no scaling, no centering, no plot)
pca1 <- dudi.pca(PCAmatrix,cent=FALSE,scale=FALSE,scannf=FALSE,nf=3)

#eigenvalues
barplot(pca1$eig[1:50],main="PCA eigenvalues", col=heat.colors(50))
pca1

#plot samples according to their new coordinates
s.class(pca1$li, pop(nomissgenind), xax=1, yax=2, col=transp(mypalette), pgrid.draw = FALSE)
title("labelled PCA")

#without labels
colorplot(pca1$li, pca1$li, transp=TRUE, cex=3, xlab="PC 1", ylab="PC 2")


















	########################
	####      TO DO     ####
	########################

#1) Analyze the loci that are significantly out of HW IN ONE SINGLE POPULATION
#2) Filter out those loci from the WHOLE DATASET
#3) Re-do the analysis we did with the new dataset
#4) Compare





#  1) HW eq test for one population

#   1.1 - Check the gening we generated with "sepop" command
	per_pop
	# Check the output, it will show the summary information for each Population.
	# You can acces to each population data as regularly using "$" and the name of the population
	per_pop$POPULATION_YOU_CHOSE

#   1.2 - Use "which" as we usually do to make a subset of the dataset
	str(dataset, list.len=10)   	# populations are stored at "X.1"
		dataset[which (dataset$X.1 =="POPULATION_YOU_CHOSE"),]   	#   Use "<-" or "=" to create an object to hold the Population dataset
	
	#Check the new object
	str(POPULATION_DATASET_YOU_DID, list.len=10)
	is.data.frame(POPULATION_DATASET_YOU_DID)
	
	#We don't need now the column "2" that has the population names, as all are the same population
		POPULATION_DATASET_YOU_DID[c(1,3:502)]   	#   Use "<-" or "=" to create an object to hold this subdataset
	str(YOUR_NEW_OBJECT_SUBSET_OF_COLUMNS, list.len=10)
	
#   1.3 - Save as a file
	write.table(YOUR_NEW_OBJECT_SUBSET_OF_COLUMNS,file="name_that_will_indicate_what_this_file_has_in_it.str",sep="\t", append=FALSE, quote=FALSE, row.names=FALSE)

#   1.4 - REPLACE X!!!
	#Sadly R adds X to each column name that starts with a number, also names columns with no names as X, X.1, X.2, etc
	#Open manually as a txt file, and replace (usually Ctrl+F, Ctrl+H or File>Replace) "X.1" with "", and then "X" with ""

#   1.5 - Import the file using "read.structure"
	#Check the file information or check "per_pop$POPULATION_YOU_CHOSE": how many individuals (n.ind=?)
	#now there is no column with the populations. Delete "col.pop=2"
	#remember ro create an object with the dataset using "<-" or "="

#   1.6 - HWeq test
		hw.test(YOUR_IMPORTED_FILE, B=999)   	#Use "<-" or "=" to create an object to hold the hw eq test results!
	#Check
	head(YOUR_HWeq_TEST_RESULTS, n=10L)
	HWtable <- as.data.frame(YOUR_HWeq_TEST_RESULTS)

	#Extract p-values
		HWtable[which (HWtable$Pr.exact <0.05),]   	#   Use "<-" or "=" to create an object to hold the loci out of HW
	#Check the size
	dim(YOUR_LIST_OF_LOCI_OUT_OF_HW_EQUILIBRIUM)
	#If there is a reasonable number, extract the names of the loci
	noHWloci <- row.names(NOeqHW)
	noHWloci



#  2) filter those loci from the dataset

#   2.1 - Check the dataset we opened at the begining of the class, and the loci to delete
	str(dataset, list.len=10)
	noHWloci

#   2.2 - Write a vector with the locinames, separate them with commas, add the X before the name
	shitloci_vector <- c("X6448_57", "X44908_48", "X94062_94", "etc etc...")

#   2.3 - Make a subset of dataset with the columns that do not (!) match (%in%) the names stored
	filtered <- dataset[ , !(names(dataset) %in% shitloci)]
	#check and compare
	dim(filtered)
	dim(dataset)

#   2.4 - Save as a file
	write.table(filtered,file="HWdataset.str",sep="\t", append=FALSE, quote=FALSE, row.names=FALSE)

#   2.5 - REPLACE X!!!
	#Sadly R adds X to each column name that starts with a number, also names columns with no names as X, X.1, X.2, etc
	#Open manually as a txt file, and replace (usually Ctrl+F, Ctrl+H or File>Replace) "X.1" with "", and then "X" with ""



#  3) Re-do the analysis we did with the new dataset

#   3.1 - Import the file using "read.structure"
	#Now the number of loci should be below 500, check how many left: n.loc=?
	#Keep missing as a new allele (NA.char="-9")
	#remember to create an object to hold the dataset using "<-" or "="

#   3.2 - Calculate Allelic richness, Ho, He, Hs, Private alleles, Global Fst and significance, Pair-wise Fsts
#   3.3 - Plot the indexes by population
#   3.4 - Compare differences between observed and expected heterozygosities



#  4) Compare the results with the ones we had with all the SNPs and with missing. Any differences?


############################



#  5) ADVANCED: Are you capable to test significant differences between populations in any of the indexes?
#   		  	Are you able to do a subset without PJ and ST and re-do the PCA and Fsts?



#############################






