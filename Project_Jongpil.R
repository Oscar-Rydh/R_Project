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

# ----------------------------------------------------------------------- #
public_health_expend <- read.csv("Datasets/Public Health GDP.csv")
public_health_expend <- data.frame(
  Year = public_health_expend$Year.s., 
  PublicHealthGDP = public_health_expend$Value,
  CountryOrArea = public_health_expend$Country.or.Area
)

# -------------------------time series regression----------------------------- #

library(lfe)
life_and_health = merge(x=both_sexes_life_expect, y=public_health_expend, by=c('Year','CountryOrArea'), all=FALSE)

model = felm(life_and_health$LifeExpectency ~ life_and_health$PublicHealthGDP + G(life_and_health$Year) + G(life_and_health$CountryOrArea))
