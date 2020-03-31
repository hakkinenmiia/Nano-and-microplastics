###THESIS - STATISTICS

### MEDIUM DATA ANALYSIS
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
library(readxl)

###CULTURE MEDIA DATA 
#Import Data
#CURRENT FILE:

M <- read_excel("Medium.xlsx", col_types = c("numeric","numeric","text","numeric","numeric","numeric","numeric","numeric"))

View(M)

D <- subset(M,`Time h`==48)
View(D)
#48h sample subset was used because treatment exposure is 24h



#TEST FOR DIFFERENCE 
#Check of assumptions
#Normality test to prove that our data is parametric: 
s <- glm(D$`% dead`~D$Medium)
shapiro.test(resid(s)) #is parametric because p-value =0.8448
                        #Our data is parametric
hist(resid(s))
qqnorm(resid(s))
qqline(resid(s))

#Test for equal variance:
bartlett.test(D$`% dead`~D$Medium) #p-value = 0.1212, equal variance OK
leveneTest(D$`% dead`~D$Medium) # equal variance OK

##STATISTICAL TEST - DIFFERENCE, 2 groups, 1 nominal factor -> T-test 

t.test(D$`% dead`~D$Medium)
#p-value = 0.3341

boxplot(D$`% dead`~D$Medium, xlab="Medium", ylab="Mortality (%)")


### EXPERIMENT DATA ANALYSIS
#Statistical calculations: mortality, phagocytosity ablity and capacity 
#Objective of the study: compare mortality and phagocytic activity between exposure groups 

#Setup ####
setwd("~/R/Thesis")

install.packages("ggpubr")
install.packages("dplyr")

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
library(ggpubr)
library(reshape2)

###IMPORT DATA 

#Data imported "automatically" using import function in the files. 
library(readxl)
#OBS! Don't use this file, use the one below 
Data_ <- read_excel("Rdata.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric","numeric", "numeric", "numeric", "numeric","numeric", "numeric","numeric","numeric","numeric", "numeric", "numeric", "numeric","numeric", "numeric","numeric","numeric","numeric"))[,1:23]

View(Data_)
Data_$treatment <-as.factor(Data_$treatment)  
Data_$concentration <-as.factor(Data_$concentration)
head(Data_)


#nd=newdata OBS: USE THIS!!
nd <- read.csv("Data_11022020.csv", sep= ";", dec = ",")

nd$concentration <-as.factor(nd$concentration)

View(high)

high <- subset(nd, concentration==c("high", "none")) 
subset = nd$concentration %in% c("high", "none") -> high

View(high)



###BACKGROUND INFORMATION OF MUSSELS 
#lenght and weight
mussel_lenght <- c(5.3, 5.6, 5.8, 5.1, 5, 5, 4.9, 4.8)
mussel_weight <- c(18.63, 23.50, 26.52, 21.48, 18.20, 22.31, 17.53, 16.76, 17,45)

mean(mussel_lenght) #5.1875
mean(mussel_weight) #22.693
sd(mussel_lenght) #0.3522884

sd(mussel_weight) #8.468765

#####STATISTICAL ANALYSIS#####

#PHAGOCYTIC ACTIVITY 
#PHAGO ABILITY 
mfrow=c(2, 1)
#high
boxplot(nd$ability~nd$treatment | nd$concentration, ylab="Phagocytic ability (%)")

#low
boxplot(nd$ability~nd$treatment, ylab="Phagocytic ability (%)", subset = nd$concentration %in% c("low", "none"))

#combined concentrations
ggplot(nd, aes(x=treatment, y=ability, colour=concentration, fill=concentration, ylab="ability %")) + geom_boxplot() + labs(y= "ability %") + theme(panel.border = element_blank(),  panel.grid.major = element_blank(),
                                                                                                           panel.grid.minor = element_blank(),
                                                                                                           panel.background = element_blank(),
                                                                                                           axis.line = element_line(colour = "grey"),
                                                                                                           text = element_text(size = 18))

##ANOVA

#Normality test to prove that our data is parametric: 
s <- glm(nd$ability~nd$treatment)
shapiro.test(resid(s)) #is parametric because p-value = 0.3031

#test for homogeniety of variance:
bartlett.test(nd$ability~nd$treatment) #p-value = 0.2727, equal variance OK
leveneTest(nd$ability~nd$treatment) # equal variance OK, p-value= 0.4897, equal variance OK

#visual check: 
hist(resid(s))
qqnorm(resid(s))
qqline(resid(s))

#Our data is parametric.



###2-way ANOVA - Ability   

#with interactions
res.aov2 <- aov(nd$ability~nd$treatment + nd$concentration + nd$treatment * nd$concentration)
plot(res.aov2)

summary(res.aov2)

#                              Df Sum Sq Mean Sq F value Pr(>F)
#nd$treatment                   4  121.3   30.32   0.269  0.894
#nd$concentration               1  104.3  104.27   0.924  0.352
#nd$treatment:nd$concentration  2  164.5   82.26   0.729  0.499
#Residuals                     15 1692.4  112.83               
#1 observation deleted due to missingness


#PHAGO CAPACITY

#high
boxplot(nd$capacity~nd$treatment, ylab="Phagocytic index", subset = nd$concentration %in% c("high", "none"))
#low
boxplot(nd$capacity~nd$treatment, ylab="Phagocytic index", subset = nd$concentration %in% c("low", "none"))


#dot plots of PI's 
dotchart(ns_high$capacity, groups = ns_high$treatment, xlab = "Phagocytic index", color = "blue")

dotchart(ns_low$capacity, groups = ns_low$treatment, xlab = "Phagocytic index", color = "green")


#Normality test: 
s2 <- glm(nd$capacity~nd$treatment)
shapiro.test(resid(s2)) #is parametric because p-value = 0.05803
#test for homogeniety of variance:
bartlett.test(nd$capacity~nd$treatment) #p-value = 0.5219, equal variance OK
hist(resid(s2))
qqnorm(resid(s2))


###2-way ANOVA - Capacity w/interactions

plot(res.aov3)

res.aov3 <- aov(nd$capacity~nd$treatment + nd$concentration + nd$treatment * nd$concentration)
plot(res.aov3)

summary(res.aov3)


#                              Df Sum Sq Mean Sq F value Pr(>F)
#nd$treatment                   4 0.1458 0.03646   0.636  0.645
#nd$concentration               1 0.0649 0.06491   1.133  0.304
#nd$treatment:nd$concentration  2 0.0778 0.03890   0.679  0.522
#Residuals                     15 0.8594 0.05730               
#1 observation deleted due to missingness


##NANOPARTICLES VS. SILICA 


#Trial: include control group into analysis and perform statitical test
ns_high <- subset(nd, concentration  %in% c("high", "none"))
ns_only <- subset(ns_high, treatment  %in% c("NP", "nanosilica", "control"))

View(ns_only)

ggplot(ns_only, aes(x=treatment, y=ability, colour=concentration, fill=concentration, ylab="ability %")) + geom_boxplot() + labs(y= "ability %") + theme(panel.border = element_blank(),  panel.grid.major = element_blank(),
                                                                                                                                                    panel.grid.minor = element_blank())


#Test for difference: 
#phagocytic ability, 2 groups, one nominal factor >> t-test

#Assumptions
#Normality test to prove that our data is parametric: 
e <- glm(ns_high$ability~ns_high$treatment, subset = ns_high$treatment %in% c("NP", "nanosilica"))
shapiro.test(resid(e)) #is parametric because p-value = 0.8175
#test for homogeniety of variance:
bartlett.test(ns_high$ability~ns_high$treatment, subset = ns_high$treatment %in% c("NP", "nanosilica")) #p-value = 0.3277, equal variance OK

#T-test
t.test(ns_high$ability~ns_high$treatment, subset = ns_high$treatment %in% c("NP", "nanosilica"))
#no significant difference, p-value = 0.4668

#Phagocytic capacity:
t.test(ns_high$capacity~ns_high$treatment, subset = ns_high$treatment %in% c("NP", "nanosilica"))
#no significant difference, p-value = 0.5175


##Ability tested with one-way ANOVA (double check, should be similar results than t-test) 

fitl=aov(ns_high$ability~ns_high$treatment, subset = ns_high$treatment %in% c("NP", "nanosilica", "control")) 
summary.lm(fitl) 

#Result:
#Residuals:
#  5       6      10      11      12      13      14      15 
#-6.0073  6.0073 -3.6677  1.2236  2.4440 -6.9349  5.9777  0.9572 
#Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                   27.500      3.443   7.987  0.000497 ***
#ns_high$treatmentnanosilica  -2.556     4.870  -0.525  0.622123    
#ns_high$treatmentNP            3.931      5.444   0.722  0.502660    
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##Capacity

fitl=aov(ns_high$capacity~ns_high$treatment, subset = ns_high$treatment %in% c("NP", "nanosilica", "control")) 
summary.lm(fitl) 

#Call:
#aov(formula = ns_high$capacity ~ ns_high$treatment, subset = ns_high$treatment %in% 
#      c("NP", "nanosilica", "control"))
#Residuals:
#  5        6       10       11       12       13       14       15 
#-0.24943  0.24943 -0.04710 -0.05562  0.10272 -0.14109  0.05012  0.09096 
#Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                  2.23339    0.10677  20.918 4.62e-06 ***
#ns_high$treatmentnanosilica -0.05831    0.15099  -0.386    0.715    
#ns_high$treatmentNP          0.17529    0.16881   1.038    0.347    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 0.1849 on 5 degrees of freedom
#(1 observation deleted due to missingness)
#Multiple R-squared:  0.2839,	Adjusted R-squared:  -0.002521 
#F-statistic: 0.9912 on 2 and 5 DF,  p-value: 0.4339



