#Association-A3


########################## START SCRIPT ##########################


#Setup

#Install various packages if needed.
install.packages("arules", dependencies=T)

#Libraries
library(arules)

#Import data
lastfm <- read.table("lastfm.csv", sep = ",", header = T)


########################## 1.2.1 - Data Pre-Processing ##########################


#1
View(lastfm)

#2
dim(table(lastfm$country)) #159 unique countries
dim(table(lastfm$user)) #15000 unique users
dim(table(lastfm$artist)) #1004 unique artists

#3
lastfm[200:201,]

#4
lastfm$user <- factor(lastfm$user)

#Manipulation to an itemMatrix using arules package
playlist <- split(x=lastfm[,"artist"],f=lastfm$user)
playlist <- lapply(playlist,unique)
playlist <- as(playlist,"transactions")


########################## 1.2.2 - Data Analysis ##########################


#1
View(sort(itemFrequency(playlist), decreasing = T))

#2
graphics.off()
par(mar = c(5, 4, 4, 5) + 0.1)
itemFrequencyPlot(playlist, support=.1, horiz = T, main = "Bands with Support >= 10% of Users", xlab = "Support")

#3
summary(itemFrequency(playlist))

#4
rules <- apriori(playlist, parameter=list(minlen=1, support = 0.02187, confidence = 0.40, target="rules"))
summary(rules)

#5
highLiftRules <- head(sort(rules, by="lift"), 5)
inspect(highLiftRules)


########################## END SCRIPT ##########################