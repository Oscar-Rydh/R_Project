# read libraries and possible variables
library(tidyr)
library(reshape)
library(lfe)

# -------------------------Mean years of schooling----------------------------- #

meanschooling <- read.csv("Data_Jongpil/Mean years of schooling (years).csv", header = T, skip = 1)
meanschooling <- meanschooling[,apply(meanschooling, 2, function(x) {sum(!is.na(x)) > 0})]
meanschooling <- meanschooling[, !(colnames(meanschooling) %in% c("X.5","HDI.Rank..2015."))]

meanschooling <- melt(meanschooling, id.vars=1)
colnames(meanschooling) <- c('Country','Year','meanschooling')

# -------------------------Inequality in income----------------------------- #

inequalityincome <- read.csv("Data_Jongpil/Inequality in income (%).csv", header = T, skip = 1)
inequalityincome <- inequalityincome[,apply(inequalityincome, 2, function(x) {sum(!is.na(x)) > 0})]
inequalityincome <- inequalityincome[, !(colnames(inequalityincome) %in% c("X.5","HDI.Rank..2015."))]

inequalityincome <- melt(inequalityincome, id.vars=1)
colnames(inequalityincome) <- c('Country','Year','inequalityincome')

# -------------------------Inequality in education----------------------------- #

inequalityeducation <- read.csv("Data_Jongpil/Inequality in education (%).csv", header = T, skip = 1)
inequalityeducation <- inequalityeducation[,apply(inequalityeducation, 2, function(x) {sum(!is.na(x)) > 0})]
inequalityeducation <- inequalityeducation[, !(colnames(inequalityeducation) %in% c("X.5","HDI.Rank..2015."))]

inequalityeducation <- melt(inequalityeducation, id.vars=1)
colnames(inequalityeducation) <- c('Country','Year','inequalityeducation')

# -------------------------Internet users----------------------------- #

internetusers <- read.csv("Data_Jongpil/Internet users (% of population).csv", header = T, skip = 1)
internetusers <- internetusers[,apply(internetusers, 2, function(x) {sum(!is.na(x)) > 0})]
internetusers <- internetusers[, !(colnames(internetusers) %in% c("X.5","HDI.Rank..2015."))]

internetusers <- melt(internetusers, id.vars=1)
colnames(internetusers) <- c('Country','Year','internetusers')

# -------------------------Mobile phone subscriptions----------------------------- #

mobilephone <- read.csv("Data_Jongpil/Mobile phone subscriptions (per 100 people).csv", header = T, skip = 1)
mobilephone <- mobilephone[,apply(mobilephone, 2, function(x) {sum(!is.na(x)) > 0})]
mobilephone <- mobilephone[, !(colnames(mobilephone) %in% c("X.5","HDI.Rank..2015."))]

mobilephone <- melt(mobilephone, id.vars=1)
colnames(mobilephone) <- c('Country','Year','mobilephone')

# -------------------------Total unemployment rate----------------------------- #

unemploymentrate <- read.csv("Data_Jongpil/Total unemployment rate (% of labour force).csv", header = T, skip = 1)
unemploymentrate <- unemploymentrate[,apply(unemploymentrate, 2, function(x) {sum(!is.na(x)) > 0})]
unemploymentrate <- unemploymentrate[, !(colnames(unemploymentrate) %in% c("X.5","HDI.Rank..2015."))]

unemploymentrate <- melt(unemploymentrate, id.vars=1)
colnames(unemploymentrate) <- c('Country','Year','unemploymentrate')

# -------------------------Government expenditure on education----------------------------- #

expenditureonedu <- read.csv("Data_Jongpil/Government expenditure on education (% of GDP).csv", header = T, skip = 1)
expenditureonedu <- expenditureonedu[,apply(expenditureonedu, 2, function(x) {sum(!is.na(x)) > 0})]
expenditureonedu <- expenditureonedu[, !(colnames(expenditureonedu) %in% c("X.5","HDI.Rank..2015."))]

expenditureonedu <- melt(expenditureonedu, id.vars=1)
colnames(expenditureonedu) <- c('Country','Year','expenditureonedu')

# -------------------------GDP total----------------------------- #

gdptotal <- read.csv("Data_Jongpil/Gross domestic product (GDP), total (2011 PPP $ billions).csv", header = T, skip = 1)
gdptotal <- gdptotal[,apply(gdptotal, 2, function(x) {sum(!is.na(x)) > 0})]
gdptotal <- gdptotal[, !(colnames(gdptotal) %in% c("X.5","HDI.Rank..2015."))]

gdptotal <- melt(gdptotal, id.vars=1)
colnames(gdptotal) <- c('Country','Year','gdptotal')



# -------------------------data merge----------------------------- #

meanschooling_inequalityincome = merge(x=meanschooling, y=inequalityincome, by=c('Year','Country'), all=FALSE)
meanschooling_inequalityincome <- meanschooling_inequalityincome[complete.cases(meanschooling_inequalityincome),]
meanschooling_inequalityincome$meanschooling <- as.numeric(meanschooling_inequalityincome$meanschooling)
meanschooling_inequalityincome$inequalityincome <- as.numeric(meanschooling_inequalityincome$inequalityincome)

meanschooling_inequalityeducation = merge(x=meanschooling, y=inequalityeducation, by=c('Year','Country'), all=FALSE)
meanschooling_inequalityeducation <- meanschooling_inequalityeducation[complete.cases(meanschooling_inequalityeducation),]
meanschooling_inequalityeducation$meanschooling <- as.numeric(meanschooling_inequalityeducation$meanschooling)
meanschooling_inequalityeducation$inequalityeducation <- as.numeric(meanschooling_inequalityeducation$inequalityeducation)

meanschooling_internetusers = merge(x=meanschooling, y=internetusers, by=c('Year','Country'), all=FALSE)
meanschooling_internetusers <- meanschooling_internetusers[complete.cases(meanschooling_internetusers),]
meanschooling_internetusers$meanschooling <- as.numeric(meanschooling_internetusers$meanschooling)
meanschooling_internetusers$internetusers <- as.numeric(meanschooling_internetusers$internetusers)

meanschooling_mobilephone = merge(x=meanschooling, y=mobilephone, by=c('Year','Country'), all=FALSE)
meanschooling_mobilephone <- meanschooling_mobilephone[complete.cases(meanschooling_mobilephone),]
meanschooling_mobilephone$meanschooling <- as.numeric(meanschooling_mobilephone$meanschooling)
meanschooling_mobilephone$mobilephone <- as.numeric(meanschooling_mobilephone$mobilephone)

meanschooling_unemploymentrate = merge(x=meanschooling, y=unemploymentrate, by=c('Year','Country'), all=FALSE)
meanschooling_unemploymentrate <- meanschooling_unemploymentrate[complete.cases(meanschooling_unemploymentrate),]
meanschooling_unemploymentrate$meanschooling <- as.numeric(meanschooling_unemploymentrate$meanschooling)
meanschooling_unemploymentrate$unemploymentrate <- as.numeric(meanschooling_unemploymentrate$unemploymentrate)

meanschooling_expenditureonedu = merge(x=meanschooling, y=expenditureonedu, by=c('Year','Country'), all=FALSE)
meanschooling_expenditureonedu <- meanschooling_expenditureonedu[complete.cases(meanschooling_expenditureonedu),]
meanschooling_expenditureonedu$meanschooling <- as.numeric(meanschooling_expenditureonedu$meanschooling)
meanschooling_expenditureonedu$expenditureonedu <- as.numeric(meanschooling_expenditureonedu$expenditureonedu)

meanschooling_gdptotal = merge(x=meanschooling, y=gdptotal, by=c('Year','Country'), all=FALSE)
meanschooling_gdptotal <- meanschooling_gdptotal[complete.cases(meanschooling_gdptotal),]
meanschooling_gdptotal$meanschooling <- as.numeric(meanschooling_gdptotal$meanschooling)
meanschooling_gdptotal$gdptotal <- as.numeric(meanschooling_gdptotal$gdptotal)

# -------------------------time series regression----------------------------- #

model = felm(meanschooling_inequalityincome$meanschooling ~ meanschooling_inequalityincome$inequalityincome + G(meanschooling_inequalityincome$Year) + G(meanschooling_inequalityincome$Country))
summary(model)

model = felm(meanschooling_inequalityeducation$meanschooling ~ meanschooling_inequalityeducation$inequalityeducation + G(meanschooling_inequalityeducation$Year) + G(meanschooling_inequalityeducation$Country))
summary(model)

model = felm(meanschooling_internetusers$meanschooling ~ meanschooling_internetusers$internetusers + G(meanschooling_internetusers$Year) + G(meanschooling_internetusers$Country))
summary(model)

model = felm(meanschooling_mobilephone$meanschooling ~ meanschooling_mobilephone$mobilephone + G(meanschooling_mobilephone$Year) + G(meanschooling_mobilephone$Country))
summary(model)

model = felm(meanschooling_unemploymentrate$meanschooling ~ meanschooling_unemploymentrate$unemploymentrate + G(meanschooling_unemploymentrate$Year) + G(meanschooling_unemploymentrate$Country))
summary(model)

model = felm(meanschooling_expenditureonedu$meanschooling ~ meanschooling_expenditureonedu$expenditureonedu + G(meanschooling_expenditureonedu$Year) + G(meanschooling_expenditureonedu$Country))
summary(model)

model = felm(meanschooling_gdptotal$meanschooling ~ meanschooling_gdptotal$gdptotal + G(meanschooling_gdptotal$Year) + G(meanschooling_gdptotal$Country))
summary(model)

#model = lm(meanschooling_gdptotal$meanschooling ~ meanschooling_gdptotal$gdptotal)
#summary(model)
