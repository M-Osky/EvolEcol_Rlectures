#R sucks!

#PCA script

getwd()
setwd("D:/Dropbox/MOSKY/CURRO/CLASS")
getwd()



############		SET UP THE INFORMATION ABOUT THE INPUT DATA FILE		###############

#input file
dataset = read.table("lizardsagain.csv", sep = ',', header =TRUE)	#read the data into a table, read headers
str(dataset)				#chek the data

#Extract the variable used for grouping. It will be used only to add different colors and tag the plots
# we will use the column 2 -> Group: Sex+Island
i = 2
#extracts as a factor (useful if binary data)
groups <- factor(dataset[[i]])
groups

#extract the grouping variable as numeric levels (from 1 to 4)
group <- (as.numeric(groups))
group

#extract the levels (name of each group)
tags <- levels(dataset[[i]])
tags

# To avoid mistakes is recommended to proceed with the analysis with a dataset that has only the variables we want to analyse
# This is not extrictedly needed, but is also a way for you to see that PCA does not use the grouping variables
varinfo_orig <- dataset[c(5:13)]
str(varinfo_orig)

# Lets standardise all data, that's fitting the variance in a distribution of average 0 and SD=1
# This will end with reduce skew, differences in magnitude and make our data more Normal
varinfo <- scale(dataset[c(5:13)])
str(varinfo)


#PCA is a very visual analysis, be sure to choose a set of colors and shape for the dots, that allows quick visualisation
# A quick guide https://www.statmethods.net/advgraphs/parameters.html
palette (c("darkorange3", "darkgreen", "orange", "chartreuse3", "black", "red1", "goldenrod1", "purple3", "blue", "magenta"))
mypalette <- c("darkorange3", "darkgreen", "orange", "chartreuse3", "black", "red1", "goldenrod1", "purple3", "blue", "magenta")
myshapes=c(15:18)

#See variables by pairs, compare standardised with the other
pairs(varinfo, col = groups, upper.panel = NULL, pch = 19, cex = 0.2)
pairs(varinfo_orig, col = groups, upper.panel = NULL, pch = 19, cex = 0.2)
legend("topright", bty = "n", legend = tags, pch = 19, pt.cex=1, text.width=0.05 ,col = mypalette, xpd = T, cex = 0.7, y.intersp = 0.8)
#This shows the correlation used to determine the PCs
#It may show some correlations between variables and some combinations of them that allow to group the samples

##############################################################################################################
####################    Multivariant analysis: Principal Component Analysis    ###############################
##############################################################################################################

#Do the default R PCA
firstPCA <- prcomp(scale(varinfo))

#decide how many Principal Components to take
outPCA <- capture.output(summary(firstPCA))
outPCA
screeplot(firstPCA, type="lines")
# The variance explained by each PC is very inportant, write it up

#To see the loadings, that is how each variable affects each component
loadingsPCA <- firstPCA$rotation
loadingsPCA <- (format (firstPCA$rotation, digits=4))
loadingsPCA

#Plot the PCA
#See the PCA then: plot the principal components 1 and 2, use the colors and shapes defined, apply them according to the groups
plot(firstPCA$x[,1:2], col = mypalette[as.numeric(groups)], pch=myshapes[as.numeric(groups)])
legend ("bottomleft", legend = tags, pch=myshapes, col=mypalette)




###########################################

# Seems like there is an out-lier, lets identify which sample is it by adding labels to the dots
samples <- dataset[[1]] #Extract the sample names
samples
text(firstPCA$x[,1:2], labels=samples) #Add text to the PCA plot

#If there is many samples it may be more useful to check in which row is it to delete it
datarow <- row.names(dataset) #Extract the row possition of each sample
plot(firstPCA$x[,1:2], col = mypalette[as.numeric(groups)], pch=myshapes[as.numeric(groups)])
legend ("bottomleft", legend = tags, pch=myshapes, col=mypalette)
text(firstPCA$x[,1:2], labels=datarow)

#Comprobation just to be sure
dataset[2,] #check it
dataset <- dataset[-2,] #delete the second row by adding "-" in front of it, and rewrite "dataset"
head(dataset)

# Now repeat all

#########################################


#Sometimes the grouping is quite evident, but sometimes is more subjective
#Do we see any grouping? Is it significant?

# To see the analysis at a samples level we need to check the "scores".
# Scores are the transformed values of the data according to the model
# They are the coordinates of the samples in the new dimensional space:
# Values of each sample for each measurement = coordinates in the multidimensional space of those variables
# Scores of each sample in each PC = coordinates of each sample in the plot with the 2PCs we just saw

# The scores of each sample in each PC will allowed us to sort them according to their relationship with them
# We can check with a post-hoc analysis if there are significant differences between the scores in each group.

#How?

#Remember the GLM? NOOOOOOOOO! YES =)

# 1) Extract scores
pcascores<-firstPCA$x
head(pcascores)
	
# 2) extract the information of individual tags and grouping variables from original file
allinfo <- dataset[c(1:4)]

# 3) save scores and info
write.table(pcascores,file="pca_scores.csv",sep=",", append=FALSE)
write.table(allinfo,file="tags.csv",sep=",", append=FALSE)
	
# 4) join information from both
library (Rcpp)
library (dplyr)
importscores = read.table("pca_scores.csv", sep = ',', header =FALSE, skip=1) #inport
importgroups = read.table("tags.csv", sep = ',', header =FALSE, skip=1) #inport
allimport <- bind_cols(importgroups, importscores)# join both
str(allimport)
# The function bind_cols keeps the column with the sorting order. We need to get rid of them

#Depurate
#According to the file we want the variables 2-6 as grouping and 8-12 as list of scores
pca_scores <- allimport[c(2:5, 7:15)]
str(pca_scores)

# 5) Select first PC scores and grouping variable for the GLM
pc1 <- allimport[[8]]
group <- pca_scores[[i]]

# 6) GLM
#Check which family to apply
library(fitdistrplus)
descdist(pc1, boot=1000)
shapiro.test(pc1)
# If is close enough to Normal the Gaussian family should be able to handle it.

mod1<-glm(pc1~group, data=pca_scores, family=gaussian)
mod1
# It should be well fitted, because the scores are the result of a model similar to a correlation
# Residual DF*2 >> residual deviance

# Now we can do an ANOVA
aov_pc1 <- aov(mod1)->aov_pc1
aov_pca <- summary(aov_pc1)
aov_pca


###############################################################
#Fancier
library(ggplot2)
#library(tidyr)
#library(stringi)
library(ggfortify)
library(digest)

#load the colours and shapes in the ggplo graphic parametres
palettecolor=mypalette
scale_fill_manual(values=mypalette)
scale_colour_manual(values=mypalette)
myshapes=c(22,21,24,23)

# Calculate the variance explained by each PC
firstPCA$sdev
prop.pca = firstPCA$sdev^2/sum(firstPCA$sdev^2)
Xlabel <- paste("PC1 (", round(100*(prop.pca[1]), digits=2), "%)", sep="")
Ylabel <- paste("PC2 (", round(100*(prop.pca[2]), digits=2), "%)", sep="")

#Make a subset with the grouping variable and all the variables used in the analysis
podinfo = dataset[c(i, 6:11)]

autoplot(firstPCA, data=podinfo) + #Plot the data
labs(x = Xlabel, y = Ylabel) + coord_fixed(ratio = 1) + #add the % of variance explained to the axis, make both axis equal
theme_bw() + #choose a b/w theme for the background
geom_point(aes(shape = group, fill = group), size = 2.5) + #chose the shape, colour and size of dots, do it by group
#extract the colors and shapes from the deffined palettes
scale_fill_manual(values=mypalette)+
scale_colour_manual(values=mypalette) +
scale_shape_manual(values=myshapes)



# You can see there is differences but is difficult to see the influence of sex and Island separately


# A) Try to add the color to the PCA only based on population and/or only based on sex
# For this you need to extract the variables Sex and Pop and add them to the plots
	# You can just change the value of "i" to the column that you want in the plot
	# Or you can do it properly, extract Sex and Pop as factor and as numeric and replace "group" and "groups"in the plots
	# This is just a visualization tool, for seeing only the variables separately do point "C)"
	
# B) You can check if there are any variables which doesn't seem to add anything to the main trend
# Delete them (when defining "varinfo") and repeat the analysis
	# Check if the % of variance explained increases
	# Check if the group differenciation is more clear in the PCA plot
	
# C) You can now analyse each group separately:
# Extract one of the groups and plot according to the other
	#Extract Females, then plot their PCA according to Pop, to see how different females are between islands
	#Extract Males and plot according to Pop to see how different males are between islands
	#Extract Individuals from PK and plot according to Sex to see the sexual dimorphism in the Island
	#Extract Individuals from PM and plot according to Sex to see the sexual dimorphism in the Island
	
	#For that you will need to make a subset.
	# Either males, females, PK or PM.
	# Example:
	#   	Mal <- dataset[which (dataset$Sex =="1"),]
	#then change the whole dataset object name ("dataset") in the script, by the new subset ("Mal" in this case)
	#and change the column of the grouping variable used to extract the cathegories
	#   	str(Mal)
	#   	i=4
	

