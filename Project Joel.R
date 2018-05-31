###############################
#     Umemployment rate       #
###############################
library(dplyr)

#################### Highly related datapoints
# - GDP
# - Absolute Population
# - Absolute enrolled in tertiary education
# - Cellular subscriptions
# - Percent of internet users
# - Carbon dioxide emissions per capita

# Unemployment rate
# URL: http://data.un.org/Data.aspx?q=unemployment&d=SDGs&f=series%3aSL_TLF_UEM
unemployment.raw <- read.table('Data_Joel/unemployment_rate.csv', header=TRUE, sep=',')

# Look at world data
unemployment.world <- unemployment.raw %>%
  filter(Reference.Area == 'World (SDG-MDG)') %>%
  filter(Age.group == '15+') %>%
  select(Reference.Area, Sex, Time.Period, Value)
colnames(unemployment.world) <- c('Country', 'Sex', 'Year', 'Unemployment.Rate')
str(unemployment.world)

# Look at the world unemployment rate for different genders
library(ggplot2)
ggplot(unemployment.world, aes(x=Year, y=Unemployment.Rate, color=Sex)) + 
  geom_line(aes(y=Unemployment.Rate, color=Sex))

# Look at the difference between male and female rate vs total rate
library(tidyr)
ggplot(spread(unemployment.world, Sex, Unemployment.Rate), aes(x=Year)) + 
  geom_line(aes(y=Female-Male), color='red') + 
  geom_line(aes(y=Total), color='blue') + 
  xlab('Year') + ylab('Mean difference')



# Look at country level
unemployment.all <- unemployment.raw %>%
  filter(Nature == 'Country Data') %>%
  filter(Age.group == '15+') %>%
  select(Reference.Area, Sex, Time.Period, Value)
colnames(unemployment.all) <- c('Country', 'Sex', 'Year', 'Unemployment.Rate')
str(unemployment.all)

unemployment.total <- unemployment.all %>% 
  filter(Sex == 'Total')
str(unemployment.total)

unemployment.male <- unemployment.all %>% 
  filter(Sex == 'Male')
str(unemployment.male)

unemployment.female <- unemployment.all %>% 
  filter(Sex == 'Female')
str(unemployment.female)

# How much data is in every data set?
length(unemployment.female$Country)
length(unemployment.male$Country)
length(unemployment.total$Country)

# Looking at mean of countries
function() {
  # Plot mean of unemployment rate for different gender distributions
  # library(ggplot2)
  # ggplot(unemployment.all, aes(x=Year)) + 
  #   geom_line(aes(y=Unemployment.Rate, color=Sex), stat='summary', fun.y='mean')
  # We can easily see the difference between genders
  # We can also see that all curves are following a quite similar path
  
  # Check how the difference of mean between male and female differs over the years
  # Maybe vs the total
  # unemployment.mean <- data.frame(
  #   Year=sort(unique(unemployment.all$Year)), 
  #   Total=with(unemployment.total, tapply(Unemployment.Rate, Year, mean)),
  #   Male=with(unemployment.male, tapply(Unemployment.Rate, Year, mean)),
  #   Female=with(unemployment.female, tapply(Unemployment.Rate, Year, mean))
  # )
  
  # Plot the difference of mean in unemployment rate between sexes, compared to the total
  # ggplot(unemployment.mean, aes(x=Year)) + 
  #   geom_line(aes(y=Female-Male), color='red') + 
  #   geom_line(aes(y=Total), color='blue') + 
  #   xlab('Year') + ylab('Mean difference')
  # We can see that as the unemployment rate decreases, 
  # the difference between male and female unemployment increases
  # This tells us that as when we see a decrease in unemployment rate, 
  # it is likely due to lots of male being employed
}

# Check if there is a significant difference in unemployment rate during the years
unemployment.total.lm <- lm(Unemployment.Rate ~ Year, data=unemployment.total)
summary(unemployment.total.lm)
# There is a significant decrease in total

unemployment.male.lm <- lm(Unemployment.Rate ~ Year, data=unemployment.male)
summary(unemployment.male.lm)
# There is not a significant decrease for males

unemployment.female.lm <- lm(Unemployment.Rate ~ Year, data=unemployment.female)
summary(unemployment.female.lm)
# There is not a significant decrease for females

# ANOVA tests
unemployment.total.subset <- unemployment.total %>%
  filter(Year == 2000 | Year == 2015)
unemployment.total.subset$Year <- factor(unemployment.total.subset$Year)
unemployment.total.aov = aov(Unemployment.Rate ~ Year, data = unemployment.total.subset)
summary(unemployment.total.aov)
# No significant decrease

unemployment.male.subset <- unemployment.male %>%
  filter(Year == 2000 | Year == 2015)
unemployment.male.subset$Year <- factor(unemployment.male.subset$Year)
unemployment.male.aov = aov(Unemployment.Rate ~ Year, data = unemployment.male.subset)
summary(unemployment.male.aov)
# No significant decrease

unemployment.female.subset <- unemployment.female %>%
  filter(Year == 2000 | Year == 2015)
unemployment.female.subset$Year <- factor(unemployment.female.subset$Year)
unemployment.female.aov = aov(Unemployment.Rate ~ Year, data = unemployment.female.subset)
summary(unemployment.female.aov)
# No significant decrease

# Takeaway
# With the linear regression test, we can see that there has been 
# a significant decrease during 2000 - 2015 when looking at world population

# Lets try and correlate it



####################################
# GDP per capita
# URL: http://data.un.org/Data.aspx?d=SNAAMA&f=grID%3a101%3bcurrID%3aUSD%3bpcFlag%3a1%3bitID%3a9
# Measured in US dollars
####################################
gdp.raw <- read.table('Data_Joel/GDPcapita.csv', header=TRUE, sep=',')
str(gdp.raw)

gdp <- gdp.raw %>% 
  rename(Country = Country.or.Area, GDP = Value) %>%
  select(Country, Year, GDP)

unemployment.total.and.gdp <- merge(unemployment.total, gdp)
rowloss <- length(unemployment.total$Country) - length(unemployment.total.and.gdp$Country)
cat('We lost', rowloss, 'rows. Percent:', rowloss/length(unemployment.total$Country)*100)
# We lost 7% of rows

# Plot the amount of unemployment we have, per GDP per capita
# ggplot(unemployment.total.and.gdp, aes(x=Year)) + 
#   geom_line(aes(y=Unemployment.Rate/GDP), stat='summary', fun.y='mean', col='blue')
ggplot(unemployment.total.and.gdp, aes(x=GDP, y=Unemployment.Rate)) + 
  geom_point() + 
  stat_smooth()
# My hypothesis is that the higher the GDP, the lower the unemployment rate
# There seems to be a span from 20.000 - 100.000 where unemployment decreases

unemployment.total.and.gdp.lm <- lm(Unemployment.Rate ~ GDP, data=unemployment.total.and.gdp)
summary(unemployment.total.and.gdp.lm)
# GDP is super significant
# Unemployment rate decreases as GDP increases
# One could argue though, that the unemployment rate affects the GDP rather than the other way around



####################################
# Total Population
# Measured in individuals
# URL: http://data.un.org/Data.aspx?q=population+total&d=WDI&f=Indicator_Code%3aSP.POP.TOTL
####################################
population.raw <- read.table('Data_Joel/TotalPopulation.csv', header=TRUE, sep=',', nrows=13123)
str(population.raw)

population <- population.raw %>% 
  rename(Country = Country.or.Area, Population = Value) %>%
  select(Country, Year, Population)

unemployment.total.and.population <- merge(unemployment.total, population)
rowloss <- length(unemployment.total$Country) - length(unemployment.total.and.population$Country)
cat('We lost', rowloss, 'rows. Percent:', rowloss/length(unemployment.total$Country)*100)
# Lost a bit of data

ggplot(unemployment.total.and.population, aes(x=Population, y=Unemployment.Rate)) + 
  geom_point(stat='summary', fun.y='mean')
# We can see that the countries with 1 billion+ population are doing quite good at 5%
# lets discard those

ggplot(unemployment.total.and.population %>% filter(Population < 1000000000), 
       aes(x=Population, y=Unemployment.Rate)) + 
  geom_line(stat='summary', fun.y='mean') + 
  stat_smooth() + 
  geom_vline(xintercept=140000000, color='red') + # 140.000.000
  geom_vline(xintercept=20000000, color='red') # 20.000.000
# We can see that countries with population about 20 million and 140 million are doing the best

unemployment.total.and.population.lm <- lm(Unemployment.Rate ~ Population, data=unemployment.total.and.population)
summary(unemployment.total.and.population.lm)
# Population is significant
# Unemployment rate increases as population decreases


####################################
# Tertiary Education
# URL: http://data.un.org/Data.aspx?d=UNESCO&f=series%3aE_56
####################################
tertiary_education.raw <- read.table('Data_Joel/TertiaryEducation.csv', header=TRUE, sep=',')
str(tertiary_education.raw)

tertiary_education <- tertiary_education.raw %>% 
  filter(Units.of.measurement == 'Number') %>%
  filter(Sex == 'All genders') %>%
  rename(Country = Reference.Area, TertiaryEnrolled = Observation.Value, Year = Time.Period) %>%
  select(Country, Year, TertiaryEnrolled)

unemployment.total.and.tertiary <- merge(unemployment.total.and.population, tertiary_education)
rowloss <- length(unemployment.total$Country) - length(unemployment.total.and.tertiary$Country)
cat('We lost', rowloss, 'rows. Percent:', rowloss/length(unemployment.total$Country)*100)
# Lost 34% of rows

ggplot(unemployment.total.and.tertiary, aes(x=TertiaryEnrolled/Population, y=Unemployment.Rate)) + 
  geom_point() + 
  stat_smooth()
# We cant really spot any relation with the eye

unemployment.total.and.tertiary <- unemployment.total.and.tertiary %>%
  mutate(Quote = TertiaryEnrolled/Population)
unemployment.total.and.tertiary.lm <- lm(Unemployment.Rate ~ Quote, data=unemployment.total.and.tertiary)
summary(unemployment.total.and.tertiary.lm)
# Percentage of population in education is not significant

unemployment.total.and.tertiary.lm <- lm(Unemployment.Rate ~ TertiaryEnrolled, data=unemployment.total.and.tertiary)
summary(unemployment.total.and.tertiary.lm)
# Absolute amount in education is significant. 
# Unemployment rate decreases as amount in educaiton increases


####################################
# Cellular subscriptions per 100 inhabitants
# URL: http://data.un.org/Data.aspx?d=ITU&f=ind1Code%3aI911
####################################
cellular_subscriptions.raw <- read.table('Data_Joel/CellularSubscriptions.csv', header=TRUE, sep=',')
str(cellular_subscriptions.raw)

cellular_subscriptions <- cellular_subscriptions.raw %>% 
  rename(Country = Country.or.Area, CellularSubscriptions = Value) %>%
  select(Country, Year, CellularSubscriptions)

unemployment.total.and.cellular <- merge(unemployment.total, cellular_subscriptions)
rowloss <- length(unemployment.total$Country) - length(unemployment.total.and.cellular$Country)
cat('We lost', rowloss, 'rows. Percent:', rowloss/length(unemployment.total$Country)*100)
# Lost 22% of rows

ggplot(unemployment.total.and.cellular, aes(x=CellularSubscriptions, y=Unemployment.Rate)) + 
  geom_point() + 
  stat_smooth()
# We cant really spot any relation with the eye

unemployment.total.and.cellular.lm <- lm(Unemployment.Rate ~ CellularSubscriptions, data=unemployment.total.and.cellular)
summary(unemployment.total.and.cellular.lm)
# Cellular subscriptions is super significant
# Unemployment rate decreases as cellular subscriptions increasaes


####################################
# Percentage of Internet users
# URL: http://data.un.org/Data.aspx?d=ITU&f=ind1Code%3aI99H
####################################
internet_users.raw <- read.table('Data_Joel/InternetUsers.csv', header=TRUE, sep=',', nrows=4495)
str(internet_users.raw)

internet_users <- internet_users.raw %>% 
  rename(Country = Country.or.Area, InternetUsers = Value) %>%
  select(Country, Year, InternetUsers)

unemployment.total.and.internet <- merge(unemployment.total, internet_users)
rowloss <- length(unemployment.total$Country) - length(unemployment.total.and.internet$Country)
cat('We lost', rowloss, 'rows. Percent:', rowloss/length(unemployment.total$Country)*100)
# Lost 23% of rows

ggplot(unemployment.total.and.internet, aes(x=InternetUsers, y=Unemployment.Rate)) + 
  geom_point() + 
  stat_smooth()
# We cant really spot any relation with the eye

unemployment.total.and.internet.lm <- lm(Unemployment.Rate ~ InternetUsers, data=unemployment.total.and.internet)
summary(unemployment.total.and.internet.lm)
# Internet users is super significant
# Unemployment rate decreases as internet users increasaes



####################################
# Carbon dioxide emissions per capita. Measured in metric tons
# URL: http://data.un.org/Data.aspx?d=MDG&f=seriesRowID%3a751
####################################
emissions.raw <- read.table('Data_Joel/CarbonDioxideEmissionsPerCapita.csv', header=TRUE, sep=',')
str(emissions.raw)

emissions <- emissions.raw %>% 
  rename(Country = Country.or.Area, Emissions = Value) %>%
  select(Country, Year, Emissions)

unemployment.total.and.emissions <- merge(unemployment.total, emissions)
rowloss <- length(unemployment.total$Country) - length(unemployment.total.and.emissions$Country)
cat('We lost', rowloss, 'rows. Percent:', rowloss/length(unemployment.total$Country)*100)
# Lost 31% of rows

ggplot(unemployment.total.and.emissions, aes(x=Emissions, y=Unemployment.Rate)) + 
  geom_point() + 
  stat_smooth()
# We can kind of see a relation with the eye

unemployment.total.and.emissions.lm <- lm(Unemployment.Rate ~ Emissions, data=unemployment.total.and.emissions)
summary(unemployment.total.and.emissions.lm)
# Carbon dioxide emissions is super significant
# Unemployment rate decreases as emissions per capita increasaes



####################################
# Proportion of the population using improved drinking water sources
# URL: http://data.un.org/Data.aspx?d=MDG&f=seriesRowID%3a665
####################################
water.raw <- read.table('Data_Joel/ImprovedWaterSources.csv', header=TRUE, sep=',')
str(water.raw)

water <- water.raw %>% 
  rename(Country = Country.or.Area, Water = Value) %>%
  select(Country, Year, Water)

unemployment.total.and.water <- merge(unemployment.total, water)
rowloss <- length(unemployment.total$Country) - length(unemployment.total.and.water$Country)
cat('We lost', rowloss, 'rows. Percent:', rowloss/length(unemployment.total$Country)*100)
# Lost 7% of rows

ggplot(unemployment.total.and.water, aes(x=Water, y=Unemployment.Rate)) + 
  geom_point() + 
  stat_smooth()
# We can kind of see a relation with the eye

unemployment.total.and.water.lm <- lm(Unemployment.Rate ~ Water, data=unemployment.total.and.water)
summary(unemployment.total.and.water.lm)
# Water sources is not significant





####################################
# Youth Literacy rate
# URL: http://data.un.org/Data.aspx?d=UNESCO&f=series%3aLR_AG15T24
####################################
youth_literacy.raw <- read.table('Data_Joel/YouthLiteracyRate.csv', header=TRUE, sep=',')
str(youth_literacy.raw)

youth_literacy <- youth_literacy.raw %>% 
  rename(Country = Reference.Area, Year = Time.Period, YouthLiteracy = Observation.Value) %>%
  filter(Sex == 'All genders') %>%
  select(Country, Year, YouthLiteracy)

unemployment.total.and.youth_literacy <- merge(unemployment.total, youth_literacy)
rowloss <- length(unemployment.total$Country) - length(unemployment.total.and.youth_literacy$Country)
cat('We lost', rowloss, 'rows. Percent:', rowloss/length(unemployment.total$Country)*100)
# Significant lost of data, but we will keep going

ggplot(unemployment.total.and.youth_literacy, aes(x=YouthLiteracy, y=Unemployment.Rate)) + 
  stat_smooth()
# Interestingly, there is an inrease in unemployment rate as youth literacy increases

unemployment.total.and.youth_literacy.lm <- lm(Unemployment.Rate ~ YouthLiteracy, data=unemployment.total.and.youth_literacy)
summary(unemployment.total.and.youth_literacy.lm)
# Youth Literacy is significant.
# Unemployment rate increases as youth literacy increases


###################################################
# Lets draw a conclusion whether employment rate will increase or decrease
####################################
# World population prospects
# Measured in thousands
# URL: http://data.un.org/Data.aspx?d=PopDiv&f=variableID%3a12
####################################
population_prospects.raw <- read.table('Data_Joel/PopulationProspects.csv', header=TRUE, sep=',')
str(population_prospects.raw)

population_prospects <- population_prospects.raw %>% 
  rename(Country = Country.or.Area, Year = Year.s., Population = Value) %>%
  mutate(Population = Population*1000)

# Lets plot the prospects
ggplot(population_prospects, aes(x=Year, y=Population, color=Variant)) + 
  geom_line(stat='summary', fun.y='sum')

# Create a prediction function
m = unemployment.total.and.population.lm$coefficients[1]
k = unemployment.total.and.population.lm$coefficients[2]
predict_unemployment_rate <- function(x) {
  return(k*x + m)
}

# Only look at rows that are for contries
population_prospects.countries <- population_prospects %>%
  filter(Country != 'Africa') %>%
  filter(Country != 'Asia') %>%
  filter(Country != 'Australia/New Zealand') %>%
  filter(Country != 'Central America') %>%
  filter(Country != 'Central Asia') %>%
  filter(Country != 'Eastern Asia') %>%
  filter(Country != 'Eastern Africa')

# Estimate unemployment rate for all countries
population_prospects.countries$UnemploymentRate <- predict_unemployment_rate(population_prospects.countries$Population)
# Remove negative unemployment rates
population_prospects.countries <- population_prospects.countries %>% filter(UnemploymentRate >= 0)

# Räkna ut hur många som är unemployed i varje land
# Gruppera sedan efter år och variant
# Räkna ut rate genom att ta unemployed / population 
population_prospects.summarized <- population_prospects.countries %>%
  mutate(Unemployed = Population * (UnemploymentRate/100)) %>%
  group_by(Year, Variant) %>%
  summarize(GlobalUnemploymentRate = sum(Unemployed) / sum(Population))
  

# Plot the estimation
ggplot(population_prospects.summarized, aes(x=Year, y=GlobalUnemploymentRate, color=Variant)) + 
  geom_line() + 
  ggtitle('Predicted world unemployment rate') + 
  xlab("Year") + 
  ylab('Unemployment rate')







z####################################
# Adult Literacy rate
# URL: http://data.un.org/Data.aspx?q=literacy&d=WHO&f=MEASURE_CODE%3aWHS9_85
####################################
adult_literacy.raw <- read.table('Data_Joel/AdultLiteracyRate.csv', header=TRUE, sep=',', nrows=547)
str(adult_literacy.raw)

adult_literacy <- adult_literacy.raw %>% 
  rename(Country = Country.or.Area, Year = Year.s., AdultLiteracy = Value) %>%
  select(Country, Year, AdultLiteracy)

unemployment.total.and.adult_literacy <- merge(unemployment.total, adult_literacy)
rowloss <- length(unemployment.total$Country) - length(unemployment.total.and.adult_literacy$Country)
cat('We lost', rowloss, 'rows. Percent:', rowloss/length(unemployment.total$Country)*100)
# We lost 83% of data

ggplot(unemployment.total.and.adult_literacy, aes(x=AdultLiteracy, y=Unemployment.Rate)) + 
  geom_point() + 
  stat_smooth()
# Interestingly, there is an inrease in unemployment rate as adult literacy increases too!

unemployment.total.and.adult_literacy.lm <- lm(Unemployment.Rate ~ AdultLiteracy, data=unemployment.total.and.adult_literacy)
summary(unemployment.total.and.adult_literacy.lm)
# Adult Literacy is significant.
# Unemployment rate increases as adult literacy increases







##################################################################
#
# Old stuff for GDP
#
##################################################################


# GDP
GDP <- read.table('data/GDP.csv', header=TRUE, sep=',')
str(GDP)
GDP <- GDP %>% select(Country.or.Area, Year, Value)
colnames(GDP) <- c('Country', 'Year', 'GDP')

unemployment_vs_gdp <- merge(unemployment, GDP)

with(unemployment_vs_gdp, cor(Unemployment.Rate, GDP))
# Correlation shows weak negative correlation

unemployment_vs_gdp.lm <- lm(Unemployment.Rate ~ GDP, data=unemployment_vs_gdp)
summary(unemployment_vs_gdp.lm)
# Linear model shows GDP is supersignificant, but with a very low coefficient

# Refugees
# URL: http://data.un.org/Data.aspx?d=UNHCR&f=indID%3aType-Ref
refugees <- read.table('data/refugees.csv', header=TRUE, sep=',', nrows=96061)
str(refugees)
refugees <- refugees %>% 
  select(Country.or.territory.of.asylum.or.residence, Year, Total.refugees.and.people.in.refugee.like.situations.sup.....sup.)
colnames(refugees) <- c("Country", "Year", "Refugees")
refugees <- refugees %>% group_by(Country, Year) %>%
  summarize(Refugees = sum(Refugees, na.rm = TRUE))
refugees$Refugees <- as.numeric(refugees$Refugees)

unemployment_vs_refugees <- merge(unemployment, refugees)

with(unemployment_vs_refugees, cor(Unemployment.Rate, Refugees))

unemployment_vs_refugees.lm <- lm(Unemployment.Rate ~ Refugees, data=unemployment_vs_refugees)
summary(unemployment_vs_refugees.lm)

# Check percentage of refugees vs population
# Check total amount of refugees, and not new ones











library(dplyr)
library(GGally)

# GDP
GDP <- read.table('GDP.csv', header=TRUE, sep=',')
GDP <- GDP %>%
  select(Country.or.Area, Year, Value)
colnames(GDP) <- c("Country", "Year", "GDP")


# SchoolExpectancy
SchoolExpectancy <- read.table('school_life_expectancy.csv', header=TRUE, sep=',')
SchoolExpectancy <- SchoolExpectancy %>%
  filter(Sex == 'All genders') %>%
  select(Reference.Area, Time.Period, Observation.Value)
colnames(SchoolExpectancy) <- c("Country", "Year", "SchoolExpectancy")
un_data <- merge(GDP, SchoolExpectancy)
ggpairs(un_data[c(-1,-2)])


a <- un_data %>%
  filter(Country == 'USA')
ggpairs(a[c(-1,-2)])

# Youth Literacy rate
YouthLiteracyRate <- read.table('youth_literacy_rate.csv', header=TRUE, sep=',')
YouthLiteracyRate <- YouthLiteracyRate %>%
  filter(Sex == 'All genders') %>%
  select(Reference.Area, Time.Period, Observation.Value)
colnames(YouthLiteracyRate) <- c("Country", "Year", "YouthLiteracyRate")
un_data <- merge(GDP, YouthLiteracyRate)
ggpairs(un_data[c(-1,-2)])

a <- un_data %>%
  filter(Country == 'Republic of Korea'); ggpairs(a[c(-1,-2)])

# Industrial Waste Production
IndustrialWasteProduction <- read.table('industrial_waste_production.csv', header=TRUE, sep=',', nrows=763)
IndustrialWasteProduction <- IndustrialWasteProduction %>%
  select(Country.or.Area, Year, Quantity)
colnames(IndustrialWasteProduction) <- c("Country", "Year", "IndustrialWasteProduction")
un_data <- merge(GDP, IndustrialWasteProduction)
ggpairs(un_data[c(-1,-2)])
