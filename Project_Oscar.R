# Lets read the current statistics and make it a bit more readable
life_expectency <- read.csv('Datasets/LifeExpectency.csv')
#life_expectency <- read.csv("Datasets/Life expectancy at birth (years).csv", header = T)
str(life_expectency)
life_expectency <- data.frame(
  Year = life_expectency$Year.s., 
  Gender = life_expectency$GENDER, 
  LifeExpectency = life_expectency$Value,
  CountryOrArea = life_expectency$Country.or.Area
  )
life_expectency
class(life_expectency)
str(life_expectency)

# Filter out the different genders
library(dplyr)
both_sexes_life_expect = filter(life_expectency, Gender == 'Both sexes')
male_life_expect = filter(life_expectency, Gender == 'Male')
female_life_expect = filter(life_expectency, Gender == 'Female')

# Lets plot the different life expectencies
length(both_sexes_life_expect$CountryOrArea)
library(ggplot2)
ggplot(show.legend = F, data = both_sexes_life_expect, aes(x = Year, y = LifeExpectency, group = CountryOrArea, color = cm.colors(length(CountryOrArea)))) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) +
  geom_text(nudge_x = 1.5, size = 2, aes(label=CountryOrArea), show.legend = F) 

model = lm(LifeExpectency ~ Year, data = both_sexes_life_expect)
summary(model)
# There is a statistical significant difference of the Life Expectancy over the Years

# Lets try to animate!
library(googleVis)
M <- gvisMotionChart(male_life_expect, 
                     idvar = 'CountryOrArea', 
                     timevar = 'Year')


plot(M)


# Lets look at a subset of countries for easier visualization

both_sexes_filted_contries = filter(both_sexes_life_expect, 
                                    CountryOrArea == 'Sweden' |
                                    CountryOrArea == 'Republic of Korea' |
                                    CountryOrArea == 'Peru' |
                                    CountryOrArea == 'United States of America' |
                                    CountryOrArea == 'Somalia'
                                     )

both_sexes_filted_contries
M <- gvisMotionChart(both_sexes_filted_contries, 
                     idvar = 'CountryOrArea', 
                     timevar = 'Year')


plot(M)

# Lets check the statistical significans for the change of life expectancy over all the countries between 1990 and 2012
both_sexes_filtered_year = filter(both_sexes_life_expect, Year == 1990 | Year == 2012)
str(both_sexes_filtered_year)
both_sexes_filtered_year$Year <- factor(both_sexes_filtered_year$Year)
model = aov(LifeExpectency ~ Year, data = both_sexes_filtered_year)
summary(model)
# We have an extremely small p value, the increase in life expectancy is statistically significant between the years 1990 and 2012

# ----------------------------------------------------------------------- #
public_health_expend <- read.csv("Datasets/Public Health GDP.csv")
public_health_expend <- data.frame(
  Year = public_health_expend$Year.s., 
  PublicHealthGDP = public_health_expend$Value,
  CountryOrArea = public_health_expend$Country.or.Area
)
public_health_expend_year = filter(public_health_expend, Year == 2012)
both_sexes_filtered_year = filter(both_sexes_life_expect, Year == 2012)

life_expect_vs_public_health <- merge(public_health_expend_year,both_sexes_filtered_year )

# Lets compare life expectancy with the expendature on health care of the country 2012

model = lm(LifeExpectency ~ PublicHealthGDP, data = life_expect_vs_public_health)
summary(model)
# We have a low p value, hence it seems like that public helth gdp somewhat correlates with the life expectancy
# Though the R-Squared is quite low, which means there are more variables to be found
# Also the coeficiants are quite low for the public health gdp

library(coefplot)
coef(model)
coefplot(model)

# Also if we look at the linear model we see that it is not that good of a fit
ggplot(data = life_expect_vs_public_health, aes(x = PublicHealthGDP, y = LifeExpectency)) +
  geom_point(aes(color = CountryOrArea, size = PublicHealthGDP), show.legend=F) +
  geom_text(aes(label = CountryOrArea), size = 2 ) + 
  stat_smooth()


# LETS ALSO LOOK AT THE PERCENTAGES BETWEEN GDP AND PUBLIC HEALTH SPENDING

# Instead lets look at the full GDP (US dollars) of the countries and see how it affects the life expectancy
gdp <- read.csv("Datasets/GDP.csv")
str(gdp)
gdp <- data.frame(
  Year = gdp$Year, 
  GDP = gdp$Value,
  CountryOrArea = gdp$Country.or.Area
)
gdp_year <- filter(gdp, Year == 2012)
gdp_year
both_sexes_life_expect_gdp <- filter(both_sexes_life_expect, Year == 2012)
both_sexes_life_expect_gdp
life_expect_vs_gdp<- merge(both_sexes_life_expect_gdp, gdp_year)
ggplot(life_expect_vs_gdp, aes(x=GDP, y = LifeExpectency)) +
  geom_point(aes(color = CountryOrArea, size = GDP), show.legend=F) +
  geom_text(aes(label = CountryOrArea), size = 1 ) + 
  stat_smooth(method='lm')

# Make this look more pretty
ggplot(life_expect_vs_public_health_vs_gdp, aes(x = GDP, y = PublicHealthGDP) )  +
  geom_point()

model = lm(LifeExpectency ~ GDP, data = life_expect_vs_gdp)
summary(model)
# We have a similar result as with public health gdp

# Lets combine the two variables
life_expect_vs_public_health_vs_gdp <- merge(life_expect_vs_gdp, life_expect_vs_public_health)

model = lm(LifeExpectency ~ PublicHealthGDP + GDP, data = life_expect_vs_public_health_vs_gdp)
summary(model)
# In the combined modell we see that there is a statistical significans with the public Health GDP

# Lets take inequality of income into account
inequality <- read.csv("Datasets/Inequality Income.csv", header = T, skip = 1)

inequality <- data.frame(
  CountryOrArea = inequality$Country,
  Year = as.integer(2012),
  Inequality = inequality$X2012
)

life_expect_vs_inequality <- merge(inequality, life_expect_vs_public_health_vs_gdp)


library(ggrepel)
ggplot(life_expect_vs_inequality, aes(x = Inequality, y = LifeExpectency)) + 
  geom_point(aes(color = CountryOrArea), show.legend = F) + 
  geom_text_repel(aes(label = CountryOrArea), size = 3)
# Can somewhat see in the graph that high GDP countries with low inequality have high life expectancy
# Lets check what our linear model say 
model = lm(LifeExpectency ~ Inequality, data = life_expect_vs_inequality)
summary(model)
# There is a statistical significance between inequality and life expectancy

model = lm(LifeExpectency ~ Inequality + PublicHealthGDP + GDP, data = life_expect_vs_inequality)
summary(model)
# But if we look into all the variables of the model, it is only Public Healt and GDP that are significant.
# Though, note that the coefficiants are extremely low! 
