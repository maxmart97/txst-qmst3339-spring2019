#Regression-A4


########################## START SCRIPT ##########################


###### Linear Regression ######

#Setup

#Import and attach data
artsy <- read.csv("Artsy.csv", header=T, sep=",")
attach(artsy)

#1
cor(Salary, Experience)
cor(Experience, Salary)

#2
artsySLR <- lm(Salary~Experience, data=artsy)
artsySLR
summary(artsySLR)

#3
artsyMLR <- lm(Salary~Experience+factor(Gender), data=artsy)
artsyMLR
summary(artsyMLR)

female <- data.frame(Gender=0, Experience=5)
femalePrediction <- predict(artsyMLR, newdata=female, interval="prediction")
femalePrediction

# extra - just to test male base salary with 5 years experience
male_MLR <- data.frame(Gender=1, Experience=5)
malePrediction_MLR <- predict(artsyMLR, newdata=male_MLR, interval="prediction")
malePrediction_MLR

#4
artsyMLR_Interaction=lm(Salary~Experience+factor(Gender)+Experience*factor(Gender))
summary(artsyMLR_Interaction)


###### Logistic Regression ######

#Setup

#Import and attach data
healthData <- read.csv("chd.csv", header=T, sep=",")
attach(healthData)

#1, #2
healthData$CHD <- factor(healthData$CHD)
mylogit <- glm(CHD ~ AGE + LOCATION + PHY, data = healthData, family = "binomial")
summary(mylogit)

#3
male_logit <- data.frame(AGE=20, LOCATION="CITY", PHY=5)
predict(mylogit, newdata=male_logit, interval="prediction","response")


########################## END SCRIPT ##########################