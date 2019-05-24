#Visualization-A1

########################## START SCRIPT ##########################

#Setup
pollution <- read.csv("avgpm25.csv", header=T, sep=",")
attach(pollution)

########################## Question #1 ##########################

#1A
head(pollution, n=5)

#1B
fivenum(pm25)

#1C
graphics.off()
boxplot(pollution$pm25, ylim=c(0,20), col="red", main="Boxplot for Particle Pollution", ylab="Pollution Level (PM 2.5)")

#1D
graphics.off()
boxplot(pm25~region, ylim=c(0,20), col="red", ylab="Pollution Level (PM 2.5)", xlab="Region", main="Pollution Level by Region")

#1E
graphics.off()
hist(pm25, breaks=100, col="green", xlim=c(0,20), ylim=c(0,40), xlab="Pollution Level (PM 2.5)")
rug(pm25, col="magenta")
abline(v=median(pm25), col="magenta", lwd=2)

#1F
west <- subset(pollution, region=="west")
east <- subset(pollution, region=="east")
  #Two scatterplots side by side.
    graphics.off()
    par(mfrow=c(1,2))
    plot(west$latitude, west$pm25, ylim=c(0,20), xlim=c(min(west$latitude),max(west$latitude)), xlab="Latitude", ylab="Pollution Level (PM 2.5)", main="West", col="red")
    plot(east$latitude, east$pm25, ylim=c(0,20), xlim=c(min(west$latitude),max(west$latitude)), xlab="Latitude", ylab="Pollution Level (PM 2.5)", main="East", col="blue")
  #IN USE - West and east scatterplot data on top of each other in same plotting frame
    graphics.off()
    plot(west$pm25~west$latitude,
        main="Pollution by Latitude in the West and East Regions", xlab = "Latitude", ylab = "Pollution Level (PM 2.5)",
        col="red", pch=0, ylim=c(0,20), xlim=c(min(west$latitude),max(west$latitude)))
    par(new=TRUE)
    plot(east$pm25~east$latitude,
        main="Pollution by Latitude in the West and East Regions", xlab = "Latitude", ylab = "Pollution Level (PM 2.5)",
        col="blue", pch=1, ylim=c(0,20), xlim=c(min(west$latitude),max(west$latitude)))
    legend(x=55, y=18, legend=c("West","East"), col=c("red","blue"), pch=c(0,1))

#1G
  #Ho: µ(west$pm25[1:134]) = µ(east$pm25[1:134])
  #Ha: µ(west$pm25[1:134]) != µ(east$pm25[1:134])
    west <- subset(pollution, region=="west")
    east <- subset(pollution, region=="east")
    t.test(west$pm25[1:134], east$pm25[1:134])

########################## Question #2 ##########################

#2A
library(lattice)
xyplot(pm25~longitude | region, data=pollution, layout=c(1,2))
xyplot(pm25~longitude | region, data=pollution, layout=c(2,1))

########################## Question #3 ##########################

#3A
library(ggplot2)
library(datasets)
qplot(Sepal.Length, Petal.Length, data = iris, color = Species) + theme_bw()
qplot(Sepal.Length, Petal.Length, data = iris, facets = . ~ Species) + theme_bw()

########################## END SCRIPT ##########################