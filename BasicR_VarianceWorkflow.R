
#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###          NOW THAT WE (SHOULD BE ABLE TO) UNDERSTAND THE ANALYSIS, LET'S CHOOSE THE RIGHT ONE      ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################


#PACKAGES		INSTALL IF NEEDED 
#install.packages("fitdistrplus")

library(broom)
library(psych)
library(fitdistrplus)



# SET WORKING DIRECTORY
setwd("D:/Dropbox/MOSKY/CURRO/CLASS")    #Use 'Tab'

# OPEN INPUT FILE
dataset = read.table("Dataset_log_RAW.csv", sep = ',', header =TRUE)	#read the data into a table, read headers 	#Use 'Tab'

# CHECK INPUT FILE
str(dataset)      #General structure and organisation


# EXTRACT VARIABLES FROM A DATA MATRIX      
HHght <- dataset[[6]]   #Save one variable = column 6
HHght

# Then we extract one of the grouping variables
Group <- dataset[[5]] #Save one grouping variable = column 5
Group

Sex <- dataset[[3]] #Save one grouping variable = column 5
Sex






# Check global distribution
descdist(HHght, boot=1000)




# IF IS NOT ABSURDILY FAR, CHECK NORMALITY

# Make a subdataset with all the variables for each group

PK <- dataset[which (dataset$Pop =="PK"),]
Fem <- dataset[which (dataset$Sex =="0"),]
PMm <- dataset[which (dataset$Group =="PM-M"),]


# Extract the variable from each group

# Sex
testFem <- (Fem$HHght)


# SHAPIR-O'NEAL TEST OF NORMALITY

#Sex
shapiro.test ((testFem))


#TRANSFORMATION
#If all groups are not Normal, and distribution in the plot was not that far away


AllTransf <- ((logistic(HHght)))
AllTransf <- ((log(HHght)))
AllTransf <- ((log(HHght+1)))
AllTransf <- ((exp(HHght)))
AllTransf <- (((HHght)^(1/2)))
AllTransf <- (((HHght)^2))
AllTransf <- (((HHght)^3))
AllTransf <- (((HHght)^4))
AllTransf <- ((asin(HHght)))
AllTransf <- ((asin(scale((HHght), center=TRUE, scale=TRUE))))




#Check distribution again with "descdist" plot
descdist(AllTransf, boot=1000)

#If not far away from Normal Apply the transformation to each subgroup
Fem_asin <- (asin(testFem))

# Run Shapiro-Wilk for each subgroup
shapiro.test ((Fem_asin))



# REPEAT FOR EACH TRANSFORMATION IF NEEDED





############################################################################################################################
#######  TESTS for either one raw variable from the whole dataset or one transformed variable from the whole dataset  ######
#############################################################################################################################

# DEGREES OF FREEDOM > 1

# ANOVA 
results_aov = aov(HHght~Group, data=dataset)
summary(results_aov)

# KRUSKAL-WALLIS
kwout=kruskal.test(HHght~Group,data=dataset)
kwout


# MULTIPLE COMPARISONS

# WILCOXON-MANN-WHITNEY
pairWMW = pairwise.wilcox.test(HHght, Group, p.adjust.method = "bonferroni")
pairWMW

#T TEST
pairTtest = pairwise.t.test(HHght, Group, p.adjust.method = "bonferroni")
pairTtest




# GROUPING VARIABLES WITH TWO LEVELS

#T TEST
results_ttest = t.test(HHght~Sex, data=dataset)
results_ttest

# WILCOXON-MANN-WHITNEY
results_wilcox=wilcox.test(HHght~Sex,data=dataset)
results_wilcox



#WRITE IN THE BLACK BOARD
# WHICH VARIABLE FOR WHICH GROUP WAS ANALYSED
# IF IT WAS NORMAL (FOR WHICH TRANSFORMATION)
# TEST USED AND SIGNIFICANCE


#IF YOU HAVE TIME:
# PLOT A SCATTER PLOT OF THE VARIABLES ANALYSED
plot(dataset$HHght)

# AN HISTOGRAM FOR THE FREQUENCIES IN EACH GROUP OF THE GROUPING VARIABLES ANALYSED
plot (dataset$Group)

# A BOX PLOT FOR THE VARIABLES AND GROUPS
plot(dataset$HHght~dataset$Group)



#TRY TO MAKE THEM BEAUTIFUL, PLAY WITH THE COLORS, TAGS, ERROR BARS, USE INTERNET IF NEEDED

https://www.rdocumentation.org
https://www.r-bloggers.com
https://stackoverflow.com/questions/tagged/r

mypalette <- c("purple3", "goldenrod1", "firebrick3", "darkgreen")
plot(dataset$HHght~dataset$Group, col=mypalette, xlab="Groups from each island and sex", ylab="HHght", main="Italian wall lizard head height")		#Plot



################ MORE COMPLEX PLOTS

install.packages('ggplot2')
install.packages("ggpubr")
install.packages("ggsignif")
library("ggpubr")
library("ggplot2")
library("ggsignif")




tempdata <- dataset[c(5,6)]
tvariable <- tempdata[[2]]
tgroup <- tempdata[[1]]
groupnames <- levels(tgroup)
Indiv <- dataset[[1]]
nameplot <- "Groovy_boxplot.png"
group <- factor(dataset[[5]])
coltags <- names(dataset)

mypalette <- c("darkorange3", "darkgreen", "orange", "chartreuse3")
palette (c("darkorange3", "darkgreen", "orange", "chartreuse3", "goldenrod1", "green3", "purple3", "black", "gray", "blue", "magenta"))

scale_fill_manual(values=mypalette)
scale_colour_manual(values=mypalette)

my_title <- expression(paste(bold("Head Height differences between two populations of "), bolditalic("Podarcis siculus")))





#png(filename=nameplot, units="in", width=10, height=9, res=400)						#Save to file
#print( 


  ggboxplot (tempdata, x=coltags[5], y=coltags[6], size=1, add="dotplot", add.params = list(color = coltags[5], binwidth=0.004, alpha=0.5), color=coltags[5], legend="none", outlier.size=4) +
    geom_hline (yintercept=mean (tvariable), linetype = 2) +
    stat_summary(fun.y=mean, geom="point", shape=95, size=6) +
    stat_compare_means (method="anova", label.y.npc="centre", label.x.npc="right") +
    geom_signif(comparisons = list(c(groupnames[1], groupnames[3])), y_position=-0.05, test="t.test", map_signif_level=TRUE, tip_length= 0.02) +
    geom_signif(comparisons = list(c(groupnames[2], groupnames[4])), y_position=-0.07, test="t.test", map_signif_level=TRUE, tip_length= 0.02) +
    geom_signif(comparisons = list(c(groupnames[1], groupnames[2]), c(groupnames[3], groupnames[4])), y_position=-0.328, test="t.test", map_signif_level=TRUE, tip_length=0, vjust=2) +
    geom_signif(comparisons = list(c(groupnames[2], groupnames[3])), y_position=-0.252, test="t.test", map_signif_level=TRUE, tip_length= 0, vjust=2)+
    geom_signif(comparisons = list(c(groupnames[1], groupnames[4])), y_position=-0.352, test="t.test", map_signif_level=TRUE, tip_length= -0.02, vjust=3)+
    scale_fill_manual(values=mypalette) + scale_colour_manual(values=mypalette)+
    ggtitle(my_title) + xlab("Island of origin (PK / PM) - Sex (Male / Female)") +
    theme(plot.title = element_text(size=18, hjust=0.5, margin=margin(0,0,30,0), face="bold"), axis.title.x = element_text(vjust=0, face="bold", size=14, margin=margin(12,0,0,0)), axis.title.y = element_text(vjust = 1, face="bold", size=14, margin=margin(0,10,0,0)))

  
#)
#dev.off()







#TRY TO EXPORT THE RESULTS AND PLOTS

#ANOVA
aov_out <- capture.output(summary(results_aov))		# Best approach if we want to save them as a text file
aov_out
cat("\t## ANOVA OUTPUT ##\n\n", file="ANOVAout.txt")
cat(aov_out,                    file="ANOVAout.txt", sep="\n", append=TRUE)

#PLOTS
png("another_rplot.png")		#Define format (png) and name of the file
plot(dataset$HHght~dataset$Group)
dev.off()


