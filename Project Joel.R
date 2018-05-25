###############################
#     Umemployment rate       #
###############################
library(dplyr)

# Unemployment rate
# URL: http://data.un.org/Data.aspx?q=unemployment&d=SDGs&f=series%3aSL_TLF_UEM
unemployment.raw <- read.table('data/unemployment_rate.csv', header=TRUE, sep=',')

# Filter out the interesting data
unemployment <- unemployment.raw %>% 
  filter(Nature == 'Country Data') %>%
  filter(Sex == 'Total') %>%
  filter(Age.group == '15+') %>%
  select(Reference.Area, Time.Period, Value)
colnames(unemployment) <- c('Country', 'Year', 'Unemployment.Rate')

# Learn things about the data
library(ggplot2)
ggplot(unemployment, aes(x=Year)) + 
  geom_bar(aes(y=Unemployment.Rate), stat='summary', fun.y='mean') + 
  ggtitle('Mean of unemployment quota between all countries') + 
  xlab('Year') + 
  ylab('Mean') + 
  theme_minimal()

ggplot(unemployment, aes(x=Year)) + 
  geom_bar() + 
  ggtitle('Amount of data rows for every year') + 
  xlab('Year') + 
  ylab('# Data rows') + 
  theme_minimal()


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
