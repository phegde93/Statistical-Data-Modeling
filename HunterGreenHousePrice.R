library(readxl)
library(ggplot2)
setwd("C:/Users/prasa/Downloads")
# Uploading the Data file
df = read_excel("HuntersGreenHomeSales.xlsx")
View(df)

#Feature Engineering
df$baths = df$bathsfull + df$bathshalf * 0.5
df$specialsale = ifelse(df$splsale == 'None',0,1)
df$tileroof = ifelse(df$Roof == 'Tile' | df$Roof == 'Slate' | 
                       df$Roof == 'Slate, Tile' | 
                       df$Roof == 'Concrete, Tile', 1,0)
df$privatepool = ifelse(df$Pool =='Private' |
                          df$Pool == 'Private, Community', 1,0)
df$communitypool = ifelse(df$Pool == 'Community' |
                            df$Pool == 'Private, Community',1,0)
df$year = as.numeric(format(df$PendingDate, '%Y'))

#Checking for null values
which(complete.cases(df))
colSums(is.na(df))

#Spa column has many null values, so we are dropping that column
df = subset(df, select = -c(spa))
sum(is.na(df))

# Removing all the 4 rows with na values
df = na.omit(df)

# As we perofrmed feature engineering using bathstotal, baths half, pool,roof and
# Pending year, we are dropping these columns to avoid multi-collinearity

df = subset(df, select = -c(slnoskm,Status,Address,bathsfull,
                            bathshalf,bathstotal,Roof,Pool,subdivn,
                            PendingDate,sppersqft,lppersqft,datesold,
                            splsale))

#Data Visualization
hist(df$pricesold)
hist(df$adom_agentdaysonmarket)
hist(log(df$adom_agentdaysonmarket))

temp = df[,c(1:10,15)]

# Correlation matrix
library(PerformanceAnalytics)
chart.Correlation(temp)

# High correlation between listprice, sqft around 0.8, Since sqft is important predictor
# dropping listprice from predictor

# Regression Model

m1 = lm(pricesold~Beds+sqft+garages+baths+tileroof+yrblt+privatepool+
          communitypool+specialsale+year, data = df)
summary(m1)

m2 = lm(pricesold~sqft*Beds+baths*sqft+garages+tileroof+yrblt+privatepool+
          communitypool+specialsale+year, data = df)
summary(m2)

# To check the effect of Year on Price
m3 = lm(pricesold~year,data=df)
summary(m3)

library(stargazer)
stargazer(m1,m2, type = 'text',single.row = TRUE)

#Test For Assumption

#Linearity
plot(m2,1)

# Linearity assumption Failed for m2 model

#Normality
plot(m2,2)

# Normality Assumption Failed

#Homoscadacity test
plot(m2,3)

#We can see concave nature in the graph, so Homoscadeacity fails

# Multi-collinearity
library("car")
vif(m2)

#Autocrrelation
library(lmtest)
dwtest(m2)

#Conclusion:

# 3 Regression Assumption Failed for our best model, So regression is not a 
# Suitable Model for given Data



















