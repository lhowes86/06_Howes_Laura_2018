#'----------------------
#'
#' Homework week 06
#' 
#' @Date 2018-10-26
#' 
#' @author Laura Howes
#' 
#' ---------------------


library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)


#---
# Problem 1
#---

###Correlation - W&S Chapter 16: Questions 15, 19

###15

setwd("C:/Users/Laura/Dropbox/Grad school/BIOL 607 Biostats/Homework/data")
getwd()

grey_matter <- read_csv("./chap16q15LanguageGreyMatter.csv")

head(grey_matter)

####15a Scatter Plot

Plot_grey_matter <- ggplot(data = grey_matter,
                           mapping = aes(x = proficiency, y = greymatter)) +
  geom_point()

Plot_grey_matter

####15b Calculate the correlation

grey_matter_mod <- lm(proficiency ~ greymatter, data = grey_matter)

grey_matter_mod

summary(grey_matter_mod)

#####R^2 is 0.6696. R is the correlation coefficient, so sqrt(0.6696) = 0.8182909 = R.

####15c Test null hypothesis of 0 correlation

#####I need to do a F-test:

anova(grey_matter_mod)

#####F value = 40.539, P = 3.264e-06, which means the null hypothesis of 0 correlation can be rejected

####15d

#####My assumptions of part c are: the sample is random, and that the measurements have a bivariate (3D) normal distribution.
#####This means the relationship between greymatter and proficiency is linear, the cloud of points in a scatter plot has a circular or elliptical shape, and the frequency distributions of greymatter and proficiency separately are normal.

####15e

#####The scatterplot generally supports these assumptions, as the relationship seems linear, however I think we would need to look at more data to be sure.

###19

rat_liver <- read_csv("./chap16q19LiverPreparation.csv")

head(rat_liver)

####19a Calculate Correlation Coefficient

rat_liver_mod <- lm(unboundFraction ~ concentration, data = rat_liver)

summary(rat_liver_mod)

#####R^2 is 0.7334. R is the correlation coefficient, so sqrt(0.7334) = 0.8563878 = R.

#####After completing problem 2, I learned this function to figure out the Correlation Coefficient:

cor(rat_liver$concentration, rat_liver$unboundFraction)

#####Final Correlation Coefficient is -0.8563765

####19b Plot

rat_liver_plot <- ggplot(data = rat_liver, 
                         mapping= aes(x = concentration, y = unboundFraction)) +
  geom_point()

rat_liver_plot

####19c

#####The correlation coefficient is not near the maximum value because the relationship is not linear. It looks negatively exponential. 

####19d

#####To deal with non-linearity, I need to do a log transformation:

rat_liver_mod <- lm(log(unboundFraction) ~ concentration, data = rat_liver)

plot(rat_liver_mod, which = 2)

#####or I could:

rat_liver_plot +
  scale_y_continuous(trans = "log") +
  stat_smooth(method = lm)


#---
# Problem 2
#---

###Correlation SE

#####I created a CSV file

cat_happy <- read_csv("./hw06catshappiness_LH_created.csv")

head(cat_happy)

####2a.Are these two variables correlated? What is the output of cor() here. 
####What does a test show you?

cat_happy_mod <- lm(happiness_score ~ cats, data = cat_happy)

cat_happy_mod

summary(cat_happy_mod)

#####F value = 6.728, P-value = 0.03193, so null hypothesis can be rejected, showing the two variables are correlated

cor(cat_happy$cats, cat_happy$happiness_score)

#####The output of cor() is the correlation coefficiant. The Correlation coefficient is 0.6758738
#####The strongest correlation coefficients are r = 1.0 or -1.0, so this value shows a somewhat strong positive correlation.

####2b.What is the SE of the correlation based on the info from cor.test()

cor.test(cat_happy$cats, cat_happy$happiness_score)

#####CI = 0.08050709, 0.91578829
#####CI range = .91578829 - 0.08050709 = 0.8352812/2 = 0.4176406
#####Then I need to divide 0.4176406 by 1.96 (since 1.96*2SE = CI)
#####0.4176406/1.96

#####SE of the correlation = 0.2130819

####2c.Now, what is the SE via simulation? To do this, you'll need to use cor() and get the 
####relevant parameter from the output (remember - you get a matrix back, so, what's the right
####index!), replicate(), and sample() or dplyr::sample_n() with replace=TRUE to get, let's say,
####1000 correlations. How does this ## [1] 0.1524226 compare to your value above?

SE_sim <- replicate(1000, cor(sample_n(cat_happy, nrow(cat_happy), replace = TRUE)))

#1 sample from data n with n draws with replacement
#2 get the test statistic 
#3 repeat a 1000 times

sd(SE_sim) #calculate a mean, and SD of your estimate (which is the SE)

#[1] 0.2010088

#####It's smaller than my value.

                    
#---
# Problem 3
#---

###W&S Chapter 17: Questions 19, 30, 31

###19

nutrients <- read_csv("./chap17q19GrasslandNutrientsPlantSpecies.csv")

head(nutrients)

####19a Scatter Plot

nutrients_plot <- ggplot(data = nutrients, 
                         mapping= aes(x = nutrients, y = species)) +
  geom_point()

nutrients_plot

#####The explanatory variable (x) should be nutrients and the response variable (y) should be species
#####Because the amount of species present is dependent on how many nutrients there are.

####19b Rate of change and SE

nutrients_mod <- lm(nutrients$species ~ nutrients$nutrients)

summary(nutrients_mod)

#####The rate of change is -3.339 of the number of plant species supported by nutrient type (a negative relationship) and the SE = 1.098.

####19c Add the least squares regression to your scatter plot

nutrients_plot_lsqs <- ggplot(data = nutrients, 
                         mapping= aes(x = nutrients, y = species)) +
  geom_point() +
  scale_y_continuous(trans = "log") +
  stat_smooth(method = lm)

nutrients_plot_lsqs

#####R-squared is a statistical measure of how close the data are to the fitted regression line.
#####R^2 from summary(nutrients_mod) = 0.536, so 53.6% is explained. 

####19d Test the null hypothesis

anova(nutrients_mod)

#####F value = 9.2406 and P value = 0.01607, so the null is rejected that there is no correlation between nutrients and species.

###30

cadaver_teeth <- read_csv("./chap17q30NuclearTeeth.csv")

head(cadaver_teeth)

#####plotting the least squares regression line for practice:

cadaver_teeth_plot_lsqs <- ggplot(data = cadaver_teeth, 
                                  mapping= aes(x = deltaC14, y = dateOfBirth)) +
  geom_point() +
  scale_y_continuous(trans = "log") +
  stat_smooth(method = lm)

cadaver_teeth_plot_lsqs

####30a What is the slope of regression line?

lm(formula = cadaver_teeth$dateOfBirth ~ cadaver_teeth$deltaC14)

#####The slope of the regression line is -0.05326  

####30b

#####The narrower band of lines show the CI band. The CI bands tells us the 95% precision of the predict y mean for each value of x. 
 
####30c

#####The wider bands show the prediction interval. The prediction interval tells us the precision for each predicted y value (not mean y value like CI bands) for each x.

###31

portions <- read_csv("./chap17q31LastSupperPortionSize.csv")

head(portions)

####31a Calculate Regression Line

portions_plot <-ggplot(data = portions, 
                       mapping = aes(x = year, y = portionSize)) + 
  geom_point()

portions_plot

#need to create a linear regression model

portions_mod <- lm(portionSize ~ year, data = portions)

lm(formula = portions$portionSize ~ portions$year)

#####For equation, slope = 0.003329  and y intercept = -1.168842 
#####Regression line would be y = 0.003329x - 1.168842 

#####The trend shows a positive increase of portion size with time. The change in portion is 0.0033229 portion size increase per year.

####31b What is the most plausible range of values for the slope? Calculate the 95% CI. 

portions_plot_lsqs <- ggplot(data = portions, 
                                  mapping= aes(x = year, y = portionSize)) +
  geom_point() +
  scale_y_continuous(trans = "log") +
  stat_smooth(method = lm)

portions_plot_lsqs

#####The most plausible range of values for the slope of this relationship are within the grey area 
#####The values with the Confidence Interval Bands.

#####To calculate the actual plausible range of slope:

confint(portions_mod)

#####The plausible range/95% CI of slope is 0.001349556 to 0.005308266  

####31c Test for a change in relative portion size painted in these works 
####with the year in which they were painted.  

#####My regression line from 31a: y = 0.003329x - 1.168842 
#####To test for my regression line, I will plug in an x value form the portions table and see if it equals the y from the table:

#####size (x) 3.08, year (y) = 999

##### Does 0.003329(3.08) - 1.168842 = 999?
##### My answer is -998.9897, which is pretty close.

####31d Residual Plot

plot(portions_mod, which = 1)

#####The residual plot shows the data doesn't nicely fit around the horizontal line (it fits better with the red line).
#####To address the problem we could do a log transformation of the data.

#---
# Problem 4
#---

###Intervals and simulation - Fit the deet and bites model from lab. 

deet <- read_csv("./17q24DEETMosquiteBites.csv")

head(deet)

deet_plot <- ggplot(data = deet, mapping = aes(x = dose, y = bites)) +
  geom_point() +
  stat_smooth(method= "lm")

deet_plot

deet_mod <- lm(bites ~ dose, data = deet)

#####look at vcov()

vcov(deet_mod)

#####This maintains the best fit possible, despite deviations in the slope and intercept

install.packages("mnormt")
library(mnormt)

rmnorm(4, mean = coef(deet_mod), varcov = vcov(deet_mod))

####4a. Fit simulations. Using geom_abline() make a plot that has the following layers and shows 
####that these simulated lines match up well with the fit CI. 1) the data, 2) the lm fit with a CI
####, and 3) simulated lines. You might have to much around to make it look as good as possible.

#simulation
sim_coeff <- data.frame(rmnorm(50, mean = coef(bites_mod), varcov = vcov(bites_mod)))

deet_plot_geom_abline <- ggplot(data = deet, mapping = aes(x = dose, y = bites)) +
  geom_point() +
  stat_smooth(method= "lm") + 
  geom_abline()

deet_plot_geom_abline


####4b. Prediction simulations




