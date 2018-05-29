################################################################################################################################
# In this file we investigate how you life expectency in countries is correlated with different variables over a set of years. #
################################################################################################################################

###############################################################
# Lets begin by loading all libraries that we will use.       #
###############################################################
library(dplyr)
library(ggplot2)
library(googleVis)
library(coefplot)
library(ggrepel)
library(lfe)
library(GGally)
library(Rmisc)

#######################################################################################################
# Before we investigate new variables, lets study how the life expectency is changing over the years. #
#######################################################################################################

# Pre process the data
life_expectency <- read.csv('Datasets/LifeExpectency.csv')
life_expectency <- data.frame(
  Year = life_expectency$Year.s., 
  Gender = life_expectency$GENDER, 
  LifeExpectency = life_expectency$Value,
  CountryOrArea = life_expectency$Country.or.Area
  )

# Filter out the data for both genders
both_sexes_life_expect = filter(life_expectency, Gender == 'Both sexes')

# Lets plot the different life expectencies over the years
ggplot(show.legend = F, data = both_sexes_life_expect, aes(x = Year, y = LifeExpectency, group = CountryOrArea, color = cm.colors(length(CountryOrArea)))) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) 

# How does the correlation look between years and life expectency.
ggpairs(both_sexes_life_expect, c("Year", "LifeExpectency"))

# Lets investigate the statistical significans of a linear model. 
model = lm(LifeExpectency ~ Year, data = both_sexes_life_expect)
summary(model)

# From the above we see that there is a positive correlation between the years and life expectancy.
# We also see that the year is statistically significant when looking at the life expectency

# Also, lets treat the years as categories and investigate where the difference are statistically significant.
model = aov(LifeExpectency ~ factor(Year), data = both_sexes_life_expect)
summary(model)
tkh <- TukeyHSD(model, conf.level = 0.95)
plot(tkh, las=1)
# From the tukey husky model we see that there is a significant difference between the average lifespan comparing 2000-2012 and 1990 - 2012
# We cannot determin a statistical difference between 1990 and 20000
# We therefore focus on the significant years later.

# We also animate the change of life expectency for a great visualization of the change
M <- gvisMotionChart(both_sexes_life_expect, 
                     idvar = 'CountryOrArea', 
                     timevar = 'Year')
plot(M)


# Lets look at a subset of countries for an even easier visualization
both_sexes_filted_contries = filter(both_sexes_life_expect, 
                                    CountryOrArea == 'Sweden' |
                                    CountryOrArea == 'Republic of Korea' |
                                    CountryOrArea == 'Peru' |
                                    CountryOrArea == 'United States of America' |
                                    CountryOrArea == 'Somalia'
                                     )
M <- gvisMotionChart(both_sexes_filted_contries, 
                     idvar = 'CountryOrArea', 
                     timevar = 'Year')
plot(M)

######################################################################################################
# From this analysis we see that the overall life expectency is rising in the world!                 #
# Lets continue the analysis by looking at some individual years and finding significant variables.  #
######################################################################################################

# ----------- Checking individual yeras for the public healt gdp spending compared to life expectency ----------------- #
public_health_expend <- read.csv("Datasets/Public Health GDP.csv")
public_health_expend <- data.frame(
  Year = public_health_expend$Year.s., 
  PublicHealthGDP = public_health_expend$Value,
  CountryOrArea = public_health_expend$Country.or.Area
)
public_health_expend_year = filter(public_health_expend, Year == 2012)
both_sexes_filtered_year = filter(both_sexes_life_expect, Year == 2012)

life_expect_vs_public_health <- merge(public_health_expend_year,both_sexes_filtered_year )

# What is the correlation
ggpairs(life_expect_vs_public_health, c("PublicHealthGDP", "LifeExpectency"))



# What does our linear model say
model = lm(LifeExpectency ~ PublicHealthGDP, data = life_expect_vs_public_health)
summary(model)
# We have a low p value, hence it seems like that public health gdp has a statistical significans for the life expectancy
# Though the R-Squared is quite low, which means there are more variables to be found

# Also the coeficiants are quite low for the public health gdp
coef(model)
coefplot(model)

# Also if we look at the linear model we see that it is not that good of a fit
ggplot(data = life_expect_vs_public_health, aes(x = PublicHealthGDP, y = LifeExpectency)) +
  geom_point(aes(color = CountryOrArea, size = PublicHealthGDP), show.legend=F) +
  geom_text(aes(label = CountryOrArea), size = 2 ) + 
  stat_smooth(method='lm')



# ----------- Checking individual yeras for the total GDP/capita compared to life expectency ----------------- #
gdp <- read.csv("Datasets/GDP.csv")
gdp <- data.frame(
  Year = gdp$Year, 
  GDP = gdp$Value,
  CountryOrArea = gdp$Country.or.Area
)
gdp_year <- filter(gdp, Year == 2012)
both_sexes_life_expect_gdp <- filter(both_sexes_life_expect, Year == 2012)
life_expect_vs_gdp<- merge(both_sexes_life_expect_gdp, gdp_year)

# The correlation still exists
ggpairs(life_expect_vs_gdp, c("GDP", "LifeExpectency"))

# But we have a similar model as before and plot as before
model = lm(LifeExpectency ~ PublicHealthGDP, data = life_expect_vs_public_health)
summary(model)

ggplot(life_expect_vs_gdp, aes(x=GDP, y = LifeExpectency)) +
  geom_point(aes(color = CountryOrArea, size = GDP), show.legend=F) +
  geom_text(aes(label = CountryOrArea), size = 1 ) + 
  stat_smooth(method='lm')


# ----------- Lets check the combination of GDP spendings and Public Health GDP ----------------- #
life_expect_vs_public_health_vs_gdp <- merge(life_expect_vs_gdp, life_expect_vs_public_health)

# we begin by looking at the correaltion between public health spendings and GDP
ggpairs(life_expect_vs_public_health_vs_gdp, c("GDP", "PublicHealthGDP"))
# An very high correlation exists

# The same holds for a combination of the variables
ggpairs(life_expect_vs_public_health_vs_gdp, c("GDP", "PublicHealthGDP", "LifeExpectency"))

model = lm(LifeExpectency ~ PublicHealthGDP * GDP, data = life_expect_vs_public_health_vs_gdp)
summary(model)
# In the combined modell we see that there is a statistical significans within the variables


# ----------- We also look at a percentage of public health spent on GDP ----------------- #
life_expect_vs_public_health_vs_gdp$PublicHealthPercentage = life_expect_vs_public_health_vs_gdp$PublicHealthGDP/life_expect_vs_public_health_vs_gdp$GDP
ggplot(life_expect_vs_public_health_vs_gdp, aes(y = LifeExpectency, x = PublicHealthPercentage)) +
  geom_point() + 
  stat_smooth(method='lm', se = F)
model <- lm(LifeExpectency ~  PublicHealthPercentage, data = life_expect_vs_public_health_vs_gdp)
summary(model)
# Another statistical significans is found within the data

#---------- Finally, lets look at a combination of all the variables for the year 2012 -------------#
life_expect_vs_public_health_vs_gdp
model = lm(LifeExpectency ~ PublicHealthGDP * GDP * PublicHealthPercentage, data = life_expect_vs_public_health_vs_gdp)
summary(model)
coefplot(model)
# We see there are multiple statistical significanses in the model. Though one also has to look at the coeficiants i respect to the variable size

# ------- We do a final analysis of a single year based on inequality of income -------------- #
inequality <- read.csv("Datasets/Inequality Income.csv", header = T, skip = 1)

inequality <- data.frame(
  CountryOrArea = inequality$Country,
  Year = as.integer(2012),
  Inequality = inequality$X2012
)

life_expect_vs_inequality <- merge(inequality, life_expect_vs_public_health_vs_gdp)

ggpairs(life_expect_vs_inequality, c("Inequality", "LifeExpectency"))
# There seems to be a negative correlation between Inequality and Life Expectency

ggplot(life_expect_vs_inequality, aes(x = Inequality, y = LifeExpectency)) + 
  geom_point(aes(color = CountryOrArea), show.legend = F) + 
  geom_text_repel(aes(label = CountryOrArea), size = 3)
# Can somewhat see in the graph that high GDP countries with low inequality have high life expectancy
# Lets check what our linear model say 
model = lm(LifeExpectency ~ Inequality, data = life_expect_vs_inequality)
summary(model)
# There is a statistical significance between inequality and life expectancy


# -------------------------- Lets try to take multiple years into account  -------------------- #
# Reset all the data
life_expectency <- read.csv('Datasets/LifeExpectency.csv')
life_expectency <- data.frame(
  Year = life_expectency$Year.s., 
  Gender = life_expectency$GENDER, 
  LifeExpectency = life_expectency$Value,
  CountryOrArea = life_expectency$Country.or.Area
)
both_sexes_life_expect = filter(life_expectency, Gender == 'Both sexes')
public_health_expend <- read.csv("Datasets/Public Health GDP.csv")
public_health_expend <- data.frame(
  Year = public_health_expend$Year.s., 
  PublicHealthGDP = public_health_expend$Value,
  CountryOrArea = public_health_expend$Country.or.Area
)
gdp <- read.csv("Datasets/GDP.csv")
gdp <- data.frame(
  Year = gdp$Year, 
  GDP = gdp$Value,
  CountryOrArea = gdp$Country.or.Area
)

merged_data <- merge(both_sexes_life_expect, gdp)
merged_data <- merge(merged_data, public_health_expend)
merged_data <- filter(merged_data, Year == "2000" | Year == "2012")
merged_data$PublicHealthPercentage = merged_data$PublicHealthGDP/merged_data$GDP

# Lets do a regresion model grouped by year
model = felm(LifeExpectency ~ PublicHealthGDP * GDP * PublicHealthPercentage + G(Year), data = merged_data)
summary(model)
# We notice a lot of statistical signficant differences between the years
coefplot(model)

# ----------- Lets look at different food and water supplys and how they change over the years ---------------# 
# If food supplys have a statistically signficant difference between the years, then maybe it impacts life expectency

# ----------------- Supply of Animal Protein ------------------#

animal_protein <- read.csv("Datasets/AnimalProteinSupply.csv")
animal_protein <- data.frame(
  Year = animal_protein$Year, 
  AnimalProteinSupply = animal_protein$Value,
  CountryOrArea = animal_protein$Country.or.Area
)
animal_prot_2012 = filter(animal_protein, Year == '2010-2012')
animal_prot_2012$Year = as.integer(2012)
animal_prot_2000 = filter(animal_protein, Year == '1999-2001')
animal_prot_2000$Year = as.integer(2000)
animal_protein <- rbind(animal_prot_2000, animal_prot_2012)

# Lets check if there is a statistical difference in the animal supply between the years
model = aov(AnimalProteinSupply ~ factor(Year), data = animal_protein)
summary(model)
# There is accutally not any statistical difference in the mean supply of animal protein for all the countries

# ----------------- Supply of Vegetables Protein ------------------#

vegetables <- read.csv("Datasets/VegetableSupply.csv", sep = ',')
vegetables <- data.frame(
  Year = vegetables$Year,
  CountryOrArea = vegetables$Country.or.Area,
  VegetableSupply = vegetables$Value,
  Unit = vegetables$Unit
)
vegetables = filter(vegetables, Unit == 'kcal/capita/day' & (Year == '2012' | Year == '2000') ) 

# Lets check if there is a statistical difference in the vegetable supply between the years
model = aov(VegetableSupply ~ factor(Year), data = vegetables)
summary(model)
# There is a statistical difference in the mean supply of vegetable protein for all the countries
summarySE(data=vegetables, measurevar = 'VegetableSupply', groupvars=c('Year'))
# We see that the mean coffe supply has increased

# Lets merge it into our full data set
merged_data = merge(merged_data, vegetables)


# ----------------- Supply of Coffee (Because I like coffee) ------------------#
coffee <- read.csv("Datasets/CoffeSupply.csv")
coffee <- data.frame(
  Year = coffee$Year,
  CountryOrArea = coffee$Country.or.Area,
  CoffeeSupply = coffee$Value,
  Unit = coffee$Unit
)

coffee = filter(coffee, Unit == 'kcal/capita/day' & (Year == '2012' | Year == '2000') ) 

model = aov(CoffeeSupply ~ factor(Year), data = coffee)
summary(model)
# There is a statistical significant difference in coffee supply between the years. 
summarySE(data=coffee, measurevar = 'CoffeeSupply', groupvars=c('Year'))
# We see that the mean coffe supply has increased

# We merge the data with the rest
merged_data = merge(merged_data, coffee)

# ----------------- Supply of fresh water ------------------#
water <- read.csv("Datasets/WaterSupply.csv")
water <- data.frame(
  Year = water$Year,
  CountryOrArea = water$Country.or.Area,
  WaterSupply = water$Value
)

water = filter(water, (Year == '2012' | Year == '2000') ) 


model = aov(WaterSupply ~ factor(Year), data = water)
summary(model)
# We have a very confident statistical increase in water supply of fresh water in the world
summarySE(data=water, measurevar = 'WaterSupply', groupvars=c('Year'))
# We see that the mean fresh water supply has increased

# We merge in the final supply
merged_data = merge(merged_data, water)
merged_data

ggpairs(merged_data, c("LifeExpectency", "WaterSupply", "CoffeeSupply", "VegetableSupply"))
# We see an especially large correlation between water supply and Life Expectency

# Lets do a linear model to check! 
model = felm(LifeExpectency ~ WaterSupply + G(Year), data = merged_data)
summary(model)
# We have a very good model with the water supply

ggplot(merged_data, aes(x = WaterSupply, y = LifeExpectency, color = Year, shape=factor(Year))) +
  geom_smooth(method="lm", fill=NA) +
  geom_point() 

# We finish the analysis by taking all the variables into account in a linear model! 
model = felm(LifeExpectency ~ GDP +
               PublicHealthGDP +
               PublicHealthPercentage +
               VegetableSupply +
               CoffeeSupply +
               WaterSupply + G(Year), data = merged_data)
summary(model)
# As expected all variables that are usually considered important for ones healt are statistically significant



