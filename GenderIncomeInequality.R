setwd("/Users/jungeunyoo/Desktop/J/GCT564/project/")
getwd()
rm(list=ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(Matrix)
library(lfe)
library(rela)

# ----------------- Gender data from HDR 2016 ------------------#
gender_dt<- read.csv("gender_simple.csv",header=F,stringsAsFactors=FALSE)
                     #colClasses = c("integer","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

ind = grep("-1",gender_dt$V3)
gender_dt <- gender_dt[-ind,]
gender_df = data.frame(HDI = gender_dt[,1],
                       Country = gender_dt[,2],
                       GIIVal = gender_dt[,3],
                       GIIRank = gender_dt[,4],
                       MatMortality = gender_dt[,5],
                       AdolBirth = gender_dt[,6],
                       ParlSeat = gender_dt[,7],
                       SecondF = gender_dt[,8],
                       SecondM = gender_dt[,9],
                       LaborF = gender_dt[,10],
                       LaborM = gender_dt[,11],
                       GDVal = gender_dt[,12],
                       GDGroup = gender_dt[,13],
                       HDIF = gender_dt[,14],
                       HDIM = gender_dt[,15],
                       LifeF = gender_dt[,16],
                       LifeM = gender_dt[,17],
                       ExSchoolingF = gender_dt[,18],
                       ExSchoolingM = gender_dt[,19],
                       MSchoolingF = gender_dt[,20],
                       MSchoolingM = gender_dt[,21],
                       IncomeF = gender_dt[,22],
                       IncomeM = gender_dt[,23],
                       Development = gender_dt[,24])

#colnames(gender_dt) = c("HDI","Country","GII_Val","GII_Rank","Mat_Mortality","Adolesc_Birth","Parl_Seat", "SecondaryF","SecondaryM","LaborF", "LaborM")
head(gender_df)
# gender_df$IncomeF <- as.numeric(gender_df$IncomeF)
# gender_df$IncomeM <- as.numeric(gender_df$IncomeM)
# head(gender_df)
fit <- lm(IncomeF ~ MSchoolingF, data=gender_df)
summary(fit)

# ----------------- gender data difference ------------------#
##### ended up not using this #######

genderDiff <- data.frame(HDI = gender_df[,1],
                        Country = gender_df[,2],
                        SecondaryDiff = gender_df[,8]/gender_df[,9],
                        LaborDiff = gender_df[,10]/gender_df[,11],
                        HDIDiff = gender_df[,14]/gender_df[,15],
                        LifeDiff = gender_df$LifeF/gender_df$LifeM,
                        ExSchoolDiff = gender_df$ExSchoolingF/gender_df$ExSchoolingM,
                        MSchoolDiff = gender_df$MSchoolingF/gender_df$MSchoolingM,
                        IncomeDiff = gender_df$IncomeF/gender_df$IncomeM)

head(genderDiff)

### labor vs income
fit <- lm(IncomeDiff ~ LaborDiff, data = genderDiff)
par(bg='lightyellow')
title = paste('IncomeDiff = ',round(fit$coefficients[1],3),
              '+', round(fit$coefficients[2],3), 'x LaborDiff')
plot(genderDiff$LaborDiff, genderDiff$IncomeDiff,
     pch=21, bg='cyan', xlab="Labor differences",
     ylab="Income differences", main=title)
lines(genderDiff$LaborDiff, fit$fitted.values, col="red")


### secondary vs income
fit <- lm(IncomeDiff ~ SecondaryDiff, data = genderDiff)
par(bg='lightyellow')
title = paste('IncomeDiff = ',round(fit$coefficients[1],3),
              '+', round(fit$coefficients[2],3), 'x SecondaryDiff')
plot(genderDiff$Secondary, genderDiff$IncomeDiff,
     pch=21, bg='cyan', xlab="Secondary Education differences",
     ylab="Income differences", main=title)
lines(genderDiff$SecondaryDiff, fit$fitted.values, col="red")


### secondary vs income
fit <- lm(IncomeDiff ~ MSchoolDiff, data = genderDiff)
par(bg='lightyellow')
title = paste('IncomeDiff = ',round(fit$coefficients[1],3),
              '+', round(fit$coefficients[2],3), 'x MeanSchoolDiff')
plot(genderDiff$MSchoolDiff, genderDiff$IncomeDiff,
     pch=21, bg='cyan', xlab="Mean schooling differences",
     ylab="Income differences", main=title)
lines(genderDiff$MSchoolDiff, fit$fitted.values, col="red")

pcacor <- cor(edat)

#difference in income gender
income_df <- rbind(data.frame(Income = gender_df$IncomeF, Gender="Female"),
                   data.frame(Income = gender_df$IncomeM, Gender="Male"))
t.test(Income ~ Gender, data=income_df, var.equal=TRUE, conf.level = 0.95)


# ----------------- GII data in 2014 ------------------#
gii14<- read.csv("GII_simple.csv",header=F,stringsAsFactors=FALSE)
colnames(gii14) = c("Country","GIIVal14","GIIRank14","MatMortality13","AdolBirth10_15","ParlSeat","SecondF05_14","SecondM05_14",
                    "LaborF13","LaborM13")

#gender14-15
ind = grep("-1",gii14$GIIVal)
gii14 <- gii14[-ind,]

gender_df_ = gender_df[(gender_df$Country %in% gii14$Country),]
gii14_ = gii14[(gii14$Country %in% gender_df$Country),]
gii1415 = cbind(gender_df_[,1:11], gii14_, gender_df_[,24])

# gii1415_byYear = rbind(data.frame(Year = 2015, SecondF = gii1415$SecondF, Country = gii1415$Country),
#                        data.frame(Year=2014, SecondF = gii1415$SecondF05_14, Country = gii1415$Country))

colnames(gii1415)[13:22] = c("GIIVal","GIIRank","MatMortality","AdolBirth","ParlSeat","SecondF","SecondM","LaborF","LaborM","Development")
gii1415_byYear = rbind(data.frame(Year = 2015, gii1415[,c(3:11)], Country = gii1415$Country, Development = gii1415$Development),
                       data.frame(Year=2014, gii1415[,c(13:21)], Country = gii1415$Country,Development = gii1415$Development))


ggplot(data=gii1415_byYear, aes(x = Year, y = LaborF,group=Country,color = Country)) + 
  geom_point(show.legend = FALSE) + 
  geom_line(show.legend=FALSE)

t.test(LaborF ~ Year, data=gii1415_byYear,paired=TRUE)

# ----------------- fit model ------------------#
model = felm(gii1415_byYear$LaborM ~ gii1415_byYear$SecondM + G(gii1415_byYear$Year) + G(gii1415_byYear$Country))


# ----------------- linear regression on variables ------------------#
#maternal leave
matLeave_raw <- read.csv("maternityLeave.csv",header=F,stringsAsFactors=FALSE)
matLeave <- matLeave_raw[-1,c(1,6)]
colnames(matLeave) = c("Country", "days")
matLeave$days = as.numeric(matLeave$days)

#income vs mat
income_for_mat = gender_df[(gender_df$Country %in% matLeave$Country),]
mat_for_income = matLeave[(matLeave$Country %in% gender_df$Country),]

income_for_mat <- income_for_mat[order(income_for_mat$Country),]
mat_for_income <- mat_for_income[order(mat_for_income$Country),]
income_vs_mat <- data.frame(Country = income_for_mat$Country, Income = income_for_mat$IncomeF, IncomeRatio = income_for_mat$IncomeF/income_for_mat$IncomeM, MaternityLeave = mat_for_income$days)

fit =lm(IncomeRatio ~ MaternityLeave, data=income_vs_mat)

ggplot(data=income_vs_mat, aes(x = MaternityLeave, y = IncomeRatio, group=Country,color = Country)) + 
  geom_point(show.legend = FALSE) 

ggplot(data=income_vs_mat, aes(x = MaternityLeave, y = Income, group=Country,color = Country)) + 
  geom_point(show.legend = FALSE) 

#labor participation vs matleave

labor_vs_mat <- data.frame(Country = income_for_mat$Country, LaborRatio = income_for_mat$LaborF/income_for_mat$LaborM, MaternityLeave = mat_for_income$days)
ggplot(data=labor_vs_mat, aes(x = LaborRatio, y = MaternityLeave, group=Country,color = Country)) + 
  geom_point(show.legend = FALSE) 

fit <- lm(LaborRatio ~ MaternityLeave, data=labor_vs_mat)

#labour participation vs income
labor_vs_income <- data.frame(Country = gender_df$Country, Development = gender_df$Development, SecondEdu = gender_df$SecondF, Labor = gender_df$LaborF, Income = gender_df$IncomeF)
labor_vs_income <- data.frame(Country = gender_df$Country, Development = gender_df$Development, SecondEdu = gender_df$SecondF, Labor = gender_df$LaborF, IncomeRatio = gender_df$IncomeF/gender_df$IncomeM)

labor_vs_income <- filter(labor_vs_income, Development == "M" | Development=="L")

#overall
ggplot(data=labor_vs_income, aes(x = Labor, y = Income)) + 
  geom_point(show.legend = FALSE)+
  geom_smooth(method='lm',formula=y~x,show.legend=FALSE,se = FALSE)
summary(lm(Income~Labor, data=labor_vs_income))

#parl seat vs income
parl_vs_income <- data.frame(Country = gender_df$Country, Development = gender_df$Development, ParliamentSeat = gender_df$ParlSeat, Labor = gender_df$LaborF, Income = gender_df$IncomeF)
parl_vs_income <- data.frame(Country = gender_df$Country, Development = gender_df$Development, ParliamentSeat = gender_df$ParlSeat, Labor = gender_df$LaborF, IncomeRatio = gender_df$IncomeF/gender_df$IncomeM)
#overall
ggplot(data=parl_vs_income, aes(x = ParliamentSeat, y = IncomeRatio)) + 
  geom_point(show.legend = FALSE)+
  geom_smooth(method='lm',formula=y~x,show.legend=FALSE,se = FALSE)
summary(lm(IncomeRatio~ParliamentSeat, data=parl_vs_income))

#secondary education vs matleave

second_vs_mat <- data.frame(Country = income_for_mat$Country, SecondEdu = income_for_mat$SecondF, MaternityLeave = mat_for_income$days)
ggplot(data=second_vs_mat, aes(x = SecondEdu, y = MaternityLeave, group=Country,color = Country)) + 
  geom_point(show.legend = FALSE) 

second_vs_income <- data.frame(Country = gender_df$Country, SecondEdu = gender_df$SecondF, Income = gender_df$IncomeF)
ggplot(data=second_vs_income, aes(x = SecondEdu, y = Income)) + 
  geom_point(show.legend = FALSE)+
  geom_smooth(method='lm',formula=y~x,show.legend=FALSE,se = FALSE)
  
summary(lm(Income~SecondEdu, data=second_vs_income))



#second vs income based on development status
second_vs_income <- data.frame(Country = gender_df$Country, Development = gender_df$Development, SecondEdu = gender_df$SecondF, Income = gender_df$IncomeF)
second_vs_income <- data.frame(Country = gender_df$Country, Development = gender_df$Development, SecondEdu = gender_df$SecondF, IncomeRatio = gender_df$IncomeF/gender_df$IncomeM)

#overall
ggplot(data=second_vs_income, aes(x = SecondEdu, y = IncomeRatio)) + 
  geom_point(show.legend = FALSE)+
  geom_smooth(method='lm',formula=y~x,show.legend=FALSE,se = FALSE)
summary(lm(Income~SecondEdu, data=second_vs_income))

second_vs_income_VH <- filter(second_vs_income, Development == "VH" | Development=="H")
ggplot(data=second_vs_income_VH, aes(x = SecondEdu, y = Income))+#, group=Country,color = Country)) + 
  geom_point(show.legend = FALSE)+
  geom_smooth(method='lm', formula = y~x, show.legend = FALSE, se=FALSE)+
  ggtitle("Income v.s. Secondary Education of Highly Developed Countries")
summary(lm(Income~SecondEdu, data=second_vs_income_VH))

second_vs_income_Dev <- filter(second_vs_income, Development == "M" | Development=="L")
ggplot(data=second_vs_income_Dev, aes(x = SecondEdu, y = Income))+#, group=Country,color = Country)) + 
  geom_point(show.legend = FALSE) +
  geom_smooth(method='lm', formula = y~x, show.legend = FALSE, se=FALSE)+
  ggtitle("Income v.s. Secondary Education of Development Country")
summary(lm(Income~SecondEdu, data=second_vs_income_Dev))

# ----------------- GDI14 ------------------#

GDI14<- read.csv("GDI14.csv",header=F,stringsAsFactors=FALSE)
colnames(GDI14) = c("HDI","Country", "GDVal","GDGroup","HDIF", "HDIM", "LifeF","LifeM","ExSchoolingF","ExSchoolingM",
             "MSchoolingF","MSchoolingM","IncomeF","IncomeM")

ind = grep("-1",GDI14$GDVal)
GDI14 <- GDI14[-ind,]

# ----------------- GDI13 ------------------#

GDI13 <- read.csv("GDI13.csv", header=F,stringsAsFactors=FALSE)
colnames(GDI13) = c("HDI","Country","MSchoolingF","MSchoolingM","ExSchoolingF","ExSchoolingM",
                    "IncomeF","IncomeM","Development")

ind = grep("-1",GDI13$MSchoolingF)
GDI13 <- GDI13[-ind,]

ind = grep("-1",GDI13$ExSchoolingF)
GDI13 <- GDI13[-ind,]

ind = grep("-1",GDI13$IncomeF)
GDI13 <- GDI13[-ind,]

# ----------------- GDI14 vs 15 ------------------#

GDI15 <- gender_df[,c(1,2,12:24)]

GDI14m = GDI14[(GDI14$Country %in% GDI15$Country),]
GDI15m = GDI15[(GDI15$Country %in% GDI14$Country),]
GDI15m <- GDI15m[order(GDI15m$Country),]
GDI14m <- cbind(GDI14m[order(GDI14m$Country),],Development = GDI15m$Development)


GDI1415 <- data.frame( 
  Year = rep(c("2014", "2015"), each = 152),
  rbind(GDI14m,GDI15m))

GDI1415_Compare <- GDI1415 %>% 
  filter(Development == "VH" | Development=="H")

t.test(IncomeF ~ Year, data=GDI1415_Compare,paired=TRUE)
GDI1415_Compare$IncomeR <- GDI1415_Compare$IncomeF/GDI1415_Compare$IncomeM
t.test(IncomeF/IncomeM ~ Year, data=GDI1415_Compare,paired=TRUE)
t.test(MSchoolingF ~ Year, data=GDI1415_Compare,paired=TRUE)


# ----------------- GDI13 vs 15 ------------------#
GDI15 <- gender_df[,c(1,2,20,21,18,19,22:24)]

GDI13m = GDI13[(GDI13$Country %in% GDI15$Country),]
GDI15m = GDI15[(GDI15$Country %in% GDI13$Country),]
GDI15m <- GDI15m[order(GDI15m$Country),]
GDI13m <- GDI13m[order(GDI13m$Country),]
GDI13m$Development <- GDI15m$Development

GDI1315 <- data.frame( 
  Year = rep(c("2013", "2015"), each = 140),
  rbind(GDI13m,GDI15m))

GDI1315_Compare <- GDI1315 %>% 
  filter(Development == "VH" | Development=="H")

t.test(IncomeF ~ Year, data=GDI1315_Compare,paired=TRUE)
t.test(IncomeF/IncomeM ~ Year, data=GDI1315_Compare,paired=TRUE)
t.test(MSchoolingF ~ Year, data=GDI1415_Compare,paired=TRUE)
# ----------------- GDI00------------------#
GDI00_raw <- read.csv("GDI2000.csv", header=F,stringsAsFactors=FALSE)
GDI00 <- GDI00_raw[,c(1:6,9:12)]

colnames(GDI00) = c("HDI","Country","GDRank","GDVal","LifeF","LifeM","EnrolmentF","EnrolmentM",
                    "IncomeF","IncomeM")

ind = grep("-1",GDI00$GDVal)
GDI00 <- GDI00[-ind,]

# ----------------- GDI00 vs 15 ------------------#
GDI15 <- gender_df[,c(1,2,20,21,18,19,22:24)]

GDI00m = GDI00[(GDI00$Country %in% GDI15$Country),]
GDI15m = GDI15[(GDI15$Country %in% GDI00$Country),]
GDI15m <- GDI15m[order(GDI15m$Country),]
GDI00m <- GDI00m[order(GDI00m$Country),]
GDI00m$Development <- GDI15m$Development

GDI0015 <- data.frame( 
  Year = rep(c("2000", "2015"), each = 130),
  rbind(GDI00m[,c(1:2,9:11)],GDI15m[,c(1:2,7:9)]))

GDI0015_Compare <- GDI0015 %>% 
  filter(Development == "VH" | Development=="H")

t.test(IncomeF ~ Year, data=GDI0015_Compare,paired=TRUE)
t.test(IncomeF/IncomeM ~ Year, data=GDI0015_Compare,paired=TRUE)
t.test(MSchoolingF ~ Year, data=GDI1415_Compare,paired=TRUE)

# ----------------- Income~year ------------------#

GDI00to15 <- rbind(data.frame(Year = "1998", Country = GDI00$Country, IncomeF = GDI00$IncomeF, IncomeM = GDI00$IncomeM),
                   data.frame(Year = "2013", Country = GDI13$Country, IncomeF = GDI13$IncomeF, IncomeM = GDI13$IncomeM),
                   data.frame(Year = "2014", Country = GDI14$Country, IncomeF = GDI14$IncomeF, IncomeM = GDI14$IncomeM),
                   data.frame(Year = "2015", Country = GDI15$Country, IncomeF = GDI15$IncomeF, IncomeM = GDI15$IncomeM))
ggplot(data=GDI00to15, aes(x = Year, y = IncomeF/IncomeM,group=Country,color = Country)) + 
  geom_point(show.legend = FALSE) + 
  geom_line(show.legend=FALSE)


plot(GDI00to15$Year, GDI00to15$IncomeF)
title(main = "Female Income over Time", xlab = "Years", ylab = "GDP per capita")


plot(GDI00to15$Year, GDI00to15$IncomeF/GDI00to15$IncomeM)
title(main = "Ratio of Female Income to Male Income over Time", xlab = "Years", ylab = "Ratio")


# ----------------- Multivariate Analysis ------------------#

GII_GDI_15 = gender_df[(gender_df$Country %in% GDI15$Country),]
GDI_GII_15 = GDI15[(GDI15$Country %in% gender_df$Country),]
GII_GDI_15 <- GII_GDI_15[order(GII_GDI_15$Country),]
GDI_GII_15 <- GDI_GII_15[order(GDI_GII_15$Country),]

multivar = lm(IncomeF ~ SecondF + LaborF + ParlSeat, data= GII_GDI_15)

library(coefplot)
coefplot(multivar)

GII_GDI_14 = gii14[(gii14$Country %in% GDI14$Country),]
GDI_GII_14 = GDI15[(GDI14$Country %in% gii14$Country),]
GII_GDI_14 <- GII_GDI_14[order(GII_GDI_14$Country),]
GDI_GII_14 <- GDI_GII_14[order(GDI_GII_14$Country),]


# ----------------- Random forest & variable importance------------------#
train<-sample(1:nrow(GII_GDI_15),100)
library(randomForest)

rf1 <- randomForest(IncomeF ~ SecondF + LaborF + ParlSeat, data=GII_GDI_15,
                    importance=TRUE)
importance(rf1)      
varImpPlot(rf1,scale=T,main='Variable Importance Plot')


rf2 <- randomForest(IncomeF/IncomeM ~ SecondF + LaborF + ParlSeat, data=GII_GDI_15,
                    importance=TRUE)
importance(rf2)      
varImpPlot(rf2,scale=T,main='Variable Importance Plot')

