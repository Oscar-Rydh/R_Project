###############################
#     Umemployment rate       #
###############################
library(dplyr)

# Unemployment rate
# URL: http://data.un.org/Data.aspx?q=unemployment&d=SDGs&f=series%3aSL_TLF_UEM
unemployment.raw <- read.table('Data_Joel/unemployment_rate.csv', header=TRUE, sep=',')

# Filter out gender specific data
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


# Plot mean of unemployment rate for different gender distributions
library(ggplot2)
ggplot(unemployment.all, aes(x=Year)) + 
  geom_line(aes(y=Unemployment.Rate, color=Sex), stat='summary', fun.y='mean')
# We can easily see the difference between genders
# We can also see that all curves are following a quite similar path

# Check how the difference of mean between male and female differs over the years
# Maybe vs the total
unemployment.mean <- data.frame(
  Year=sort(unique(unemployment.all$Year)), 
  Total=with(unemployment.total, tapply(Unemployment.Rate, Year, mean)),
  Male=with(unemployment.male, tapply(Unemployment.Rate, Year, mean)),
  Female=with(unemployment.female, tapply(Unemployment.Rate, Year, mean))
)

# Plot the difference of mean in unemployment rate between sexes, compared to the total
ggplot(unemployment.mean, aes(x=Year)) + 
  geom_line(aes(y=Female-Male), color='red') + 
  geom_line(aes(y=Total), color='blue') + 
  xlab('Year') + ylab('Mean difference')
# We can see that as the unemployment rate decreases, 
# the difference between male and female unemployment increases
# This tells us that as when we see a decrease in unemployment rate, 
# it is likely due to lots of male being employed

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

# # ANOVA tests
# unemployment.total$Year <- factor(unemployment.total$Year)
# unemployment.total.aov = aov(Unemployment.Rate ~ Year, data = unemployment.total %>% filter(Year == 2000 | Year == 2015))
# summary(unemployment.total.aov)
# # No significant decrease
# 
# unemployment.male$Year <- factor(unemployment.male$Year)
# unemployment.male.aov = aov(Unemployment.Rate ~ Year, data = unemployment.male %>% filter(Year == 2000 | Year == 2015))
# summary(unemployment.male.aov)
# # No significant decrease
# 
# unemployment.female$Year <- factor(unemployment.female$Year)
# unemployment.female.aov = aov(Unemployment.Rate ~ Year, data = unemployment.female %>% filter(Year == 2000 | Year == 2015))
# summary(unemployment.female.aov)
# # No significant decrease

# Our takeaway is that unemployment rate has not changed significantly during the time 2000 - 2015
# Lets try to find something that has!



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

# Plot the amount of unemployment we have, per GDP per capita
# ggplot(unemployment.total.and.gdp, aes(x=Year)) + 
#   geom_line(aes(y=Unemployment.Rate/GDP), stat='summary', fun.y='mean', col='blue')
ggplot(unemployment.total.and.gdp, aes(x=GDP, y=Unemployment.Rate)) + 
  stat_smooth()
# My hypothesis is that the higher the GDP, the lower the unemployment rate
# There seems to be a span from 20.000 - 100.000 where unemployment decreases

unemployment.total.and.gdp.lm <- lm(Unemployment.Rate ~ GDP, data=unemployment.total.and.gdp)
summary(unemployment.total.and.gdp.lm)
# GDP is super significant
# Unemployment rate decreases as GDP increases
# One could argue though, that the unemployment rate affects the GDP rather than the other way around



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



####################################
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
# Significant lost of data, but we will keep going

ggplot(unemployment.total.and.adult_literacy, aes(x=AdultLiteracy, y=Unemployment.Rate)) + 
  stat_smooth()
# Interestingly, there is an inrease in unemployment rate as adult literacy increases too!

unemployment.total.and.adult_literacy.lm <- lm(Unemployment.Rate ~ AdultLiteracy, data=unemployment.total.and.adult_literacy)
summary(unemployment.total.and.adult_literacy.lm)
# Adult Literacy is significant.
# Unemployment rate increases as adult literacy increases


####################################
# Total Population
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
