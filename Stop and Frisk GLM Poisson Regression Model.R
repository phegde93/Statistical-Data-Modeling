#Loading Frisk-Stop Data set.
df = read.table("http://www.stat.columbia.edu/~gelman/arm/examples/police/frisk_with_noise.dat",
                skip = 6,header = TRUE)
dim(df)
str(df)

#Converting eth and crime to object and factor data type.
# eth - 1 = Black, 2= Hispanic, 3=White
# Crime - 1= violent, 2= weapons, 3=property, 4=drugs

df$eth = ifelse(df$eth==1,"black",ifelse(df$eth==2,"hispanic","white"))
str(df)
df$eth = as.factor(df$eth)
# Releveling the ethnicity base to white
df$eth = relevel(df$eth,"white")

df$crime = ifelse(df$crime==1,"violent",ifelse(df$crime==2,"weapons",ifelse(
  df$crime==3,"property","drugs"
)))

#Data Visualization
den= density(df$stops)
hist(df$stops,breaks = 20,prob=T,main = "Histogram of Stops")
lines(den,col="red")

#Initial Analysis from Graph

#stop is not normally distributed, stops is more of count variable and Poison Model
# is the best suitable model for the given Data.

# Trying to convert stops to normal by applying log transformation.

den = density(log(df$stops))
hist(log(df$stops), breaks = 20, prob=T,main = "Log Transformation")
lines(den,col="red")

#After Log transformation Stop datapoints looks more like Normal Distribution

# Visualization on stops and past arrest, In order to understand is there any relation
library(ggplot2)
ggplot(df, aes(x=past.arrests,y=log(stops)))+
  geom_point(color="steelblue")+
  geom_smooth(color="red")

#There is no linear relation b/w past arrest, and stops there is fanning in the relation

ggplot(df,aes(x=log(past.arrests),y=log(stops)))+
  geom_point(color="steelblue")+
  geom_smooth(color="red")
#It looks like log of past arrest and log of stops have nearly linear relation

#We want to Analyze the Data across all the crime,
# So we are grouping crime by presint and ethnicity

library(dplyr)
df.sum = df %>%group_by(precinct,eth)%>%
  summarise(stops=sum(stops),past_arrests=sum(past.arrests), pop=round(mean(pop)))

# Building Regression model with OLS technique

ols1 = lm(log(stops)~log(past_arrests)+eth,data=df.sum)
  
summary(ols1)

# Building Poisson Regression model using MLE Technique

poisson = glm(stops~log(past_arrests)+eth,family = poisson(link = log),data = df.sum)

summary(poisson)  

stargazer(ols1,poisson, type = "text")  
library(AER)  
dispersiontest(poisson) 
#Poisson model has over dispersion, null deviance > Residual Deviance.
#In this case, a quasi-possion model or negative binomial model is appropriate.

qpoisson =  glm(stops ~ log(past_arrests) + eth, 
                family=quasipoisson (link=log), data=df.sum)
summary(qpoisson)  

#' Given that the dispersion parameter (lambda) 
#' is 169 and quite far from 1, negative binomial 
#' regression may be more appropriate.

library(MASS)
nbionm = glm.nb(stops ~ log(past_arrests) + eth, data=df.sum)
summary(nbionm)

# Negative binomial model is a robust model for over dispersion data
