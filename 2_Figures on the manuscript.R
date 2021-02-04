#======================================================================================================================
# Title		:	Literature review of gender distribution in VL clinical trials
# Data version	:	23-Dec-2019 (from Sauman Singh)
# Author		:	Prabin Dahal
# Script Date	: 	02-02-2021
#======================================================================================================================
#rm(list=ls())

library(readxl)

# graphics packages
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(ggpubr)
library(gapminder)
library(cowplot)

setwd("C:/Users/pdahl")

#=================================================
# Figure 2: Forest plot
#=================================================
 
mydata <- dput(structure(list(class = structure(c(5L, 5L, 5L, 5L, 5L, 5L, 1L, 
1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L), .Label = c("Age", 
"HIV", "Period", "Randomisation", "Regional"), class = "factor"), 
    Group = structure(c(12L, 7L, 10L, 17L, 15L, 16L, 11L, 4L, 
    5L, 18L, 9L, 18L, 8L, 6L, 1L, 2L, 3L, 13L, 14L, 18L), .Label = c("1990_1999", 
    "2000_2009", "2010_2019", "Adults", "All ages", "Before 1990", 
    "Eastern Africa", "Excluded", "Included", "Indian Subcontinent", 
    "Less than 15 years", "Multiregional", "Non-randomised ", 
    "Randomised", "South America", "Southern and Western Asia", 
    "The Mediterranean", "Unclear"), class = "factor"), k = c(3L, 
    24L, 91L, 8L, 7L, 2L, 15L, 14L, 100L, 6L, 14L, 55L, 66L, 
    7L, 51L, 41L, 36L, 69L, 55L, 11L), re = c(0.615, 0.741, 0.676, 
    0.623, 0.572, 0.598, 0.579, 0.782, 0.681, 0.687, 0.765, 0.711, 
    0.64, 0.771, 0.71, 0.665, 0.644, 0.659, 0.704, 0.659), lower = c(0.524, 
    0.684, 0.655, 0.489, 0.43, 0.388, 0.542, 0.66, 0.66, 0.608, 
    0.634, 0.685, 0.614, 0.702, 0.68, 0.626, 0.603, 0.63, 0.676, 
    0.546), upper = c(0.699, 0.791, 0.697, 0.741, 0.704, 0.777, 
    0.615, 0.869, 0.702, 0.756, 0.859, 0.735, 0.665, 0.828, 0.738, 
    0.703, 0.682, 0.686, 0.73, 0.756), I2 = c(0, 0.944, 0.0091, 
    0.0085, 0.0087, 0.0049, 0.144, 0.892, 0.925, 0.782, 0.982, 
    0.823, 0.902, 0.0071, 0.008, 0.0092, 0.0096, 0.915, 0.901, 
    0.94)), class = "data.frame", row.names = c(NA, -20L))
)

mydata <- cbind(mydata[,c(1:3)], mydata[,c(4:7)]*100)
region_data <- mydata[which(mydata$class=="Regional"),]

a <- ggplot(region_data [which(region_data$class=="Regional"),], aes(x=reorder(Group, re), y=re)) + 
  	geom_pointrange(aes(ymin=lower, ymax=upper),pch=15,size=0.6)+
	ylab("") +
	ylim(0,100)+
	xlab("")+
	ggtitle("A: Region")+
	coord_flip()+
		geom_hline(yintercept=50,size=1,linetype="dashed")+
	# add anotations
	annotate("text", y = 20, x = 1, size=2.5,label = "paste(italic(I)^ 2, \" = 86.6% ; n=7\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 2, size=2.5,label = "paste(italic(I)^ 2, \" = 48.8% ; n=2\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 3, size=2.5,label = "paste(italic(I)^ 2, \" = 0.0% ; n=3\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 4, size=2.5,label = "paste(italic(I)^ 2, \" = 84.7% ; n=8\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 5, size=2.5,label = "paste(italic(I)^ 2, \" = 90.5% ; n=91\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 6, size=2.5,label = "paste(italic(I)^ 2, \" = 94.4% ; n=24\")",  parse = TRUE) 
	#annotate("text", y = 20, x = 7, size=2.5,label = "paste(italic(I)^ 2, \" = N/A   ; n=1\")",  parse = TRUE) 

b <- ggplot(mydata [which(mydata$class=="Age"),], aes(x=reorder(Group, re), y=re)) + 
  	geom_pointrange(aes(ymin=lower, ymax=upper),pch=15,size=0.6)+
	ylab("") +
	ylim(0,100)+
	xlab("")+
	ggtitle("B: Age group")+
	coord_flip()+
		geom_hline(yintercept=50,size=1,linetype="dashed")+
	annotate("text", y = 20, x = 1, size=2.5,label = "paste(italic(I)^ 2, \" = 14.4% ; n=15\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 2, size=2.5,label = "paste(italic(I)^ 2, \" = 92.5% ; n=100\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 3, size=2.5,label = "paste(italic(I)^ 2, \" = 78.2% ; n=6\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 4, size=2.5,label = "paste(italic(I)^ 2, \" = 89.2% ; n=14\")",  parse = TRUE)  

c <- ggplot(mydata [which(mydata$class=="HIV"),], aes(x=reorder(Group, re), y=re)) + 
  	geom_pointrange(aes(ymin=lower, ymax=upper),pch=15,size=0.6)+
	ylab("Percentage of males enrolled") +
	ylim(0,100)+
	xlab("")+
	ggtitle("C: HIV status ")+
	coord_flip()+
		geom_hline(yintercept=50,size=1,linetype="dashed")+
	annotate("text", y = 20, x = 1, size=2.5,label = "paste(italic(I)^ 2, \" = 90.2% ; n=66\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 2, size=2.5,label = "paste(italic(I)^ 2, \" = 82.3% ; n=55\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 3, size=2.5,label = "paste(italic(I)^ 2, \" = 98.2% ; n=14\")",  parse = TRUE) 

d <- ggplot(mydata [which(mydata$class=="Period"),], aes(x=reorder(Group, re), y=re)) + 
  	geom_pointrange(aes(ymin=lower, ymax=upper),pch=15,size=0.6)+
	ylab("Percentage of males enrolled") +
	ylim(0,100)+
	xlab("")+
	ggtitle("D: Time period")+
	coord_flip()+
		geom_hline(yintercept=50,size=1,linetype="dashed")+
	annotate("text", y = 20, x = 1, size=2.5,label = "paste(italic(I)^ 2, \" = 95.8% ; n=36\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 2, size=2.5,label = "paste(italic(I)^ 2, \" = 92.2% ; n=41\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 3, size=2.5,label = "paste(italic(I)^ 2, \" = 79.6% ; n=51\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 4, size=2.5,label = "paste(italic(I)^ 2, \" = 70.5% ; n=7\")",  parse = TRUE)  

# Export as a tiff file
tiff(file="Figure_2.tiff", 
            	width=24, 
			height=14, 
			units="cm", 
            	pointsize="13", 
			compression = "lzw+p", 
            	bg="white",
			res=600, 
			antialias = "none"
		)
plot_grid(a,b,c,d)
dev.off() # End export

#==================================================================
# Figure 3: Forest plot for pregnancy related inclusion criteria
#==================================================================

mydata <- dput(structure(list(class = structure(c(4L, 4L, 4L, 1L, 1L, 1L, 1L, 
2L, 2L, 2L, 2L, 3L, 3L), .Label = c("Childbearing", "FDA", "Miltefosine", 
"pregnancy"), class = "factor"), Group = structure(c(5L, 6L, 
9L, 5L, 6L, 4L, 9L, 1L, 2L, 3L, 8L, 10L, 7L), .Label = c("Class B", 
"Class C", "Class D", "Conditional on contraception use", "Excluded", 
"Included", "No", "Unassigned/Unclear", "Unclear", "Yes"), class = "factor"), 
    k = c(57L, 11L, 67L, 1L, 11L, 15L, 108L, 51L, 52L, 22L, 10L, 
    22L, 113L), re = c(0.6826, 0.6688, 0.678, 0.8146, 0.6845, 
    0.6792, 0.6777, 0.6723, 0.6911, 0.6552, 0.701, 0.6552, 0.6845
    ), lower = c(0.6563, 0.6098, 0.6428, 0.7445, 0.6202, 0.618, 
    0.6536, 0.6443, 0.6507, 0.6161, 0.6323, 0.6161, 0.6608), 
    upper = c(0.7078, 0.7229, 0.7113, 0.8688, 0.7425, 0.7347, 
    0.7009, 0.6992, 0.7287, 0.6922, 0.7617, 0.6922, 0.7073), 
    I2 = c(0.88, 0.942, 0.927, 0, 0.918, 0.953, 0.909, 0.825, 
    0.952, 0.912, 0.602, 0.912, 0.918)), class = "data.frame", row.names = c(NA, 
	-13L))
	)

mydata <- cbind(mydata[,c(1:3)], mydata[,c(4:7)]*100)

a <- ggplot(mydata[which(mydata$class=="pregnancy"),], aes(x=reorder(Group, re), y=re)) + 
  	geom_pointrange(aes(ymin=lower, ymax=upper),pch=15,size=0.6)+
	ylab("") +
	ylim(0,100)+
	xlab("")+
	ggtitle("A: Inclusion of pregnant women")+
	coord_flip()+
		geom_hline(yintercept=50,size=1,linetype="dashed")+
	# add anotations
	annotate("text", y = 20, x = 1, size=2.5,label = "paste(italic(I)^ 2, \" = 94.2% ; n=11\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 2, size=2.5,label = "paste(italic(I)^ 2, \" = 92.7% ; n=67\")",  parse = TRUE) +
	annotate("text", y = 20, x = 3, size=2.5,label = "paste(italic(I)^ 2, \" = 88.0% ; n=57\")",  parse = TRUE)  

b <- ggplot(mydata [which(mydata$class=="Childbearing"),], aes(x=reorder(Group, re), y=re)) + 
  	geom_pointrange(aes(ymin=lower, ymax=upper),pch=15,size=0.6)+
	ylab("") +
	ylim(0,100)+
	xlab("")+
	ggtitle("B: Inclusion of women of childbearing age")+
	coord_flip()+
		geom_hline(yintercept=50,size=1,linetype="dashed")+
	annotate("text", y = 20, x = 1, size=2.5,label = "paste(italic(I)^ 2, \" = 90.9% ; n=108\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 2, size=2.5,label = "paste(italic(I)^ 2, \" = 95.3% ; n=15\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 3, size=2.5,label = "paste(italic(I)^ 2, \" = 91.8% ; n=11\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 4, size=2.5,label = "paste(italic(I)^ 2, \" = N/A ; n=1\")",  parse = TRUE)  

c <- ggplot(mydata [which(mydata$class=="Miltefosine"),], aes(x=reorder(Group, re), y=re)) + 
  	geom_pointrange(aes(ymin=lower, ymax=upper),pch=15,size=0.6)+
	ylab("") +
	ylim(0,100)+
	xlab("")+
	ggtitle("C: Was Miltefosine one of the trial arms?")+
	coord_flip()+
		geom_hline(yintercept=50,size=1,linetype="dashed")+
	annotate("text", y = 20, x = 1, size=2.5,label = "paste(italic(I)^ 2, \" = 91.2% ; n=22\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 2, size=2.5,label = "paste(italic(I)^ 2, \" = 91.8% ; n=113\")",  parse = TRUE)  

d <- ggplot(mydata [which(mydata$class=="FDA"),], aes(x=reorder(Group, re), y=re)) + 
  	geom_pointrange(aes(ymin=lower, ymax=upper),pch=15,size=0.6)+
	ylab("") +
	ylim(0,100)+
	xlab("")+
	ggtitle("D: FDA risk category for use in pregnancy")+
	coord_flip()+
		geom_hline(yintercept=50,size=1,linetype="dashed")+
	annotate("text", y = 20, x = 1, size=2.5,label = "paste(italic(I)^ 2, \" = 91.2% ; n=22\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 2, size=2.5,label = "paste(italic(I)^ 2, \" = 82.5% ; n=51\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 3, size=2.5,label = "paste(italic(I)^ 2, \" = 95.2% ; n=52\")",  parse = TRUE) + 
	annotate("text", y = 20, x = 4, size=2.5,label = "paste(italic(I)^ 2, \" = 60.2% ; n=10\")",  parse = TRUE) 

tiff(file="Figure_3.tiff", 
            	width=28, 
			height=14, 
			units="cm", 
            	pointsize="13", 
			compression = "lzw+p", 
            	bg="white",
			res=600, 
			antialias = "none"
		)
plot_grid(a,b,c,d)
dev.off() # End export

#==================================================================
# Figure 4: Distribution of mean duration of illness over time
#==================================================================

dat <- read_excel("Supplemental file 3_data.xlsx", 
		sheet = "duration_of_illness"
	)

cor.test(dat$mean_duration_days, dat$year)          

tiff(file="Figure_4.tiff", 
            	width=18, 
			height=14, 
			units="cm", 
            	pointsize="13", 
			compression = "lzw+p", 
            	bg="white",
			res=600, 
			antialias = "none"
		)

ggplot(dat, aes(x=year, y=mean_duration_days,size = n_treated)) + 
  		geom_point(pch=1, col="#56B4E9")+
		scale_size_area(max_size = 12) +
  		geom_smooth(show_guide = FALSE)+
		ggtitle("")+
  		xlab("Publication year") + 
		ylab("Mean duration of illness at baseline (days)")+
		xlim(1980,2020)
dev.off()

# End (Not Run)