# Lets read the current statistics and make it a bit more readable
life_expectency <- read.csv('./Data_Jongpil/LifeExpectency.csv')
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

# -------------------------time series regression----------------------------- #

