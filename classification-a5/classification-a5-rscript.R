#Classification-A5


########################## START SCRIPT ##########################


###### Classification ######

# Setup
library(rpart)
library(rpart.plot)
library(caret)

# Import data
carSeats <- read.csv("Carseats.csv", header=T, sep=",")

# Discretize value of Sale into a new variable called HighSales
HighSales = ifelse(carSeats$Sales>8,"Yes","No");
carSeats = data.frame(carSeats,HighSales)

# View to verify data and then attach
View(carSeats)
attach(carSeats)

# 1
set.seed(1)
sIndexes <- sample(nrow(carSeats), size=360)
carSeatsTraining <- carSeats[sIndexes,]
carSeatsTest <- carSeats[-sIndexes,]

# 2
dTree <- rpart(HighSales ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc +
                 Age + Education + Urban + US,
              method="class", 
              data=carSeatsTraining,
              control=rpart.control(minsplit=1, cp=0.04, minbucket=30),
              parms=list(split='information'))
rpart.plot(dTree, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)

# 4
carSeatsTest$pred <- ifelse(predict(dTree,newdata=carSeatsTest)[,2]>.5, "yes", "no")
carSeatsTest$pred

# 5
hsActualVsPred <- table(Actual=carSeatsTest$HighSales, Prediction=carSeatsTest$pred)
addmargins(hsActualVsPred)

# 6
sum(diag(hsActualVsPred)) / 40 * 100

# 10
testStore <- data.frame(CompPrice = 138, Income = 73, Advertising = 0, Population = 27, 
                      Price = 120, ShelveLoc = "Good", Age = 49, Education = 16,
                      Urban = "Yes", US = "Yes")
predict(dTree,newdata=testStore)
predict(dTree,newdata=testStore,type=c("class"))
