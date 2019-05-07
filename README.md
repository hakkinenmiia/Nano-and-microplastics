# Nano-and-microplastics
###STATISTICS

#Setup ####
setwd("~/R/Thesis")

#LIBRARY 
library(readr)
library(reshape)
library(ggmap)
library(ggplot2)
library(oce)
library(effects)
library(stats)
library(stats4)
library(vegan)
library(car)

###CULTURE MEDIA DATA 
#Import Data
#CURRENT FILE:
M <- read.csv("Data .csv", header=TRUE, sep=";") 
#problem with the code, % of dead and alive cells factor not numeric, 
#data opened from files import function: Data_
cvs <- Data_


D <- subset(cvs, Time==48)
View(D)

?subset


#TEST FOR DIFFERENCE 

####Media, the entire dataset (both 0h and 48h), TEST FOR MORTALITY 
#Normality test to prove that our data is parametric: 
s <- glm(cvs$`% dead`~cvs$Medium)
shapiro.test(resid(s)) #is parametric because p-value =0.1683
                        #Our data is parametric
#visual check if data is parametric: 
hist(resid(s))
qqnorm(resid(s))
qqline(resid(s))
# test for homogeniety of variance:
bartlett.test(cvs$`% dead`~cvs$Medium) #p-value = 0.858, equal variance OK
leveneTest(cvs$`% dead`~cvs$Medium) # equal variance OK


t.test(cvs$`% dead`~cvs$Medium)
#p-value = 0.64

boxplot(cvs$`% dead`~cvs$Medium)

####48h samples, because 0h used PBS and viability is very low, in 48h used seawater instead.
#DIFFERENCE IN MORTALITY (% of dead cells)
#Normality test to prove that our data is parametric: 
s <- glm(D$`% dead`~D$Medium)
shapiro.test(resid(s)) #is parametric because p-value = 0.8447
#Our data is parametric

#visual check if data is parametric: 
hist(resid(s))
qqnorm(resid(s))
qqline(resid(s))
# test for homogeniety of variance:
bartlett.test(D$`% dead`~D$Medium) #p-value = 0.1212, equal variance OK
leveneTest(D$`% dead`~D$Medium) # equal variance OK

t.test(D$`% dead`~D$Medium)
# p-value = 0.334
boxplot(D$`% dead`~D$Medium)

