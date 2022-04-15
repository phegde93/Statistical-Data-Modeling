setwd("C:/Users/prasa/Downloads")
library(readxl)
df = read_excel("DoctorVisits.xlsx", sheet = "Data")
str(df)
dim(df)
# We have 5190 Observations and 12 Variables in our data

# Converting gender, private,freepoor, freeold,nhronic,lchronic to factor variables
col = c("gender","private","freepoor","freeold","nchronic","lchronic")
df[col] = lapply(df[col], factor)
str(df)

#Data Visualization
hist(df$visits)
# Not follows Normal Distribution
hist(log(df$visits))
lines(density(log(df$visits)),col="red")
#Density Function is a flat line

# Correlation between numeric variables
df_numeric = subset(df, select =c(age,income,illness,reduced,health))
cor(df_numeric)
library(corrplot)
df_temp = cor(cbind(df[, 1], df[, 3:7]))
corrplot(df_temp,method = "circle")
# There is no problem of Multi-collinearity.

dtemp <- df[, c(1, 3:7)]
str(dtemp)
library(PerformanceAnalytics)
chart.Correlation(dtemp)

library(ggplot2)
ggplot(df, aes(x=illness, y=log(visits))) +
  geom_point(color= "steelblue") +
  geom_smooth(method="loess", color="red")

# Calculating number of zeros in Target variable "visits"

table(df$visits)
#There are 4141 data points where visits is zero i..e 80% of our observation is zero


#' Predictor selection
#' 
#' gender  : Women may require more doctors' office visits than men
#' age     : Older people usually have more ailments and require more visits
#' income  : Typically does not effect visits, because visits are paid by insurance - DROP
#' illness : People with more illnesses will require more doctors' visits
#' reduced : Number of days of reduced activity due to illness or injury will be multicollinear with illnesses - DROP
#' health  : People in good health will require fewer visits
#' private : People with private insurance may visit doctors more than people without insurance
#' freepoor: People with freepoor insurance may visit doctors more than people without insurance
#' freeold : People with freeold insurance may visit doctors more than people without insurance
#' nchronic: People with chronic conditions may require more visits 
#' lchronic: People with chronic conditions limiting activity cannot visit doctors frequently


# Count Models: Poisson, Quasi-Poisson, and Negative Binomial

poisson = glm(visits~illness + age + gender + health + private + freepoor + freeold+
                nchronic + lchronic, family = poisson (link = log),data = df)
summary(poisson)

#' Poisson models require two additional assumptions: (1) Dispersion, 
#' and (2) No excess zeros.
#' Comparing null deviance with df, we see that our model is slightly overdispersed.
#' We can confirm this further using the following dispersion test.


library(AER)
dispersiontest(poisson)

#' Given the overdispersion, it will be appropriate to 
#' use QuasiPoisson or Negative Binomial
#' models. However, since the overdispersion is not much, we don't expect 
#' to see a big bias in beta coefficients from the Poission models to the other two models.

qpoisson <- glm(visits ~ illness + age + gender + health + private + freepoor 
                + freeold + nchronic + lchronic, family=quasipoisson (link=log), data=df)

library(MASS)
nbinom <- glm.nb(visits ~ illness + age + gender + health + private + freepoor + freeold
                 + nchronic + lchronic, data=df)
library(stargazer)
stargazer(poisson,qpoisson,nbinom, type = "text",single.row = TRUE)



#' Hurdle Models and Zero-Inflated Models
#' 
#' Some people who are uninsured may not want to visit a doctor to avoid an expensive 
#' out-of-pocket expense. Some insured people who did not have any illnesses may also 
#' have no motivation to visit a doctor. There can be a segment of the sample who fit these 
#' two criteria for whom visits=0, and their lack of visits may not have anything to do with
#' the predictors in our previous Poisson models. These are called excess zeros, whihc are 
#' predicted by a different data generating process (illnesses and insurance) than our 
#' Poisson model above (predictors above). To analyze this data, we can seperate out the 
#' excess zero observations using a logit model and then run a Poisson/QuasiPoisson/Negative 
#' Binomial model for the rest of the data. 
#' 
#' Since we have three types of insurance in the data (private, freepoor, and freeold), we 
#' have to create a new variable called insurance for people who have either of these three
#' types of insurance


df$insurance <- ifelse(df$private=="yes" | df$freepoor=="yes" 
                      | df$freeold=="yes", 1, 0)
View(df)
sum(df$insurance)

library(pscl)

hpoisson <- hurdle(visits ~ illness + age + gender + health + private + freepoor + freeold
                   + nchronic + lchronic+ illness | insurance + illness, data=df, link="logit", dist="poisson")
summary(hpoisson)

zip <- zeroinfl(visits ~ illness + age + gender + health + private + freepoor + freeold
                + nchronic + lchronic+illness | insurance + illness, data=df, link="logit", dist="poisson")
summary(zip)

stargazer(hpoisson,zip,type = "text",single.row = TRUE)

# As model has excess Zero Problem, Normal Poisson Model is not suitable for this given Dataset
# Hurdle or Zero-inflated Poisson model Provides a relevent coefiicient value by accounting the effect of excess zero.














