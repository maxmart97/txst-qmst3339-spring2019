# DETERMINING THE SALARY OF MAJOR LEAGUE BASEBALL PLAYERS

# Script for data analytics project

# Data: hitters.csv
# Sources: http://lib.stat.cmu.edu/datasets/baseball.data, https://rucore.libraries.rutgers.edu/rutgers-lib/30896/


#####################################################################################################################################


########## START SCRIPT


### Hitters Analysis

#Input Data


  hFull <- na.omit(read.csv("hitters.csv", header=T, sep=","))
  View(hFull)

  
  # View all variables and their indexes
  vars <- data.frame(colnames(hFull))
  vars
  
  
  # Remove these categorical vars: name, team.1986, position.1986, league.1987, and team.1987
  hSub <- hFull[,c(-1,-17,-18,-23,-24)]
  
  
#Descriptive statistics of data set
  
  
  summary(hSub)
  sumStats <- data.frame(unclass(summary(hSub)), check.names = FALSE, stringsAsFactors = FALSE)
  hist(hSub$at.bats.1986, breaks = 50, xlim=c(100,700), ylim=c(0, 10), main="At Bats during 1986 Season", xlab="At Bats", ylab="Player Count")
  hist(hSub$home.runs.1986, breaks = 50, xlim=c(0,150), ylim=c(0, 90), main="Home Runs during 1986 Season", xlab="Home Runs", ylab="Player Count")
  hist(hSub$runs.1986, breaks = 50, xlim=c(0,140), ylim=c(0, 15), main="Runs during 1986 Season", xlab="Runs", ylab="Player Count")
  hist(hSub$rbi.1986, breaks = 50, xlim=c(0,140), ylim=c(0, 15), main="RBI during 1986 Season", xlab="Runs Batted In", ylab="Player Count")
  hist(hSub$walks.1986, breaks = 50, xlim=c(0,120), ylim=c(0, 20), main="Walks during 1986 Season", xlab="Walks", ylab="Player Count")
  hist(hSub$years.in.mlb, xlim=c(0,25), ylim=c(0, 60), main="Years in MLB", xlab="Years", ylab="Player Count")
  hist(hSub$at.bats.career, breaks = 50, xlim=c(0, 15000), main="Career At Bats", xlab="At Bats", ylab="Player Count")
  hist(hSub$hits.career, breaks = 50, xlim=c(0, 5000), main="Career Hits", xlab="Hits", ylab="Player Count")
  hist(hSub$home.runs.career, breaks = 50, xlim=c(0, 600), ylim=c(0, 50), main="Career Home Runs", xlab="Home Runs", ylab="Player Count")
  hist(hSub$runs.career, breaks = 50, xlim=c(0, 2500), ylim=c(0, 40), main="Career Runs", xlab="Runs", ylab="Player Count")
  hist(hSub$rbi.career, breaks = 50, xlim=c(0, 2500), ylim=c(0, 40), main="Career RBI", xlab="Runs Batted In", ylab="Player Count")
  hist(hSub$walks.career, breaks = 50, xlim=c(0, 2000), ylim=c(0, 50), main="Career Walks", xlab="Walks", ylab="Player Count")
  plot(hSub$league.1986, col=c("blue","red"), ylim = c(0, 180), main="League Count in 1986", ylab="Number of Players", xlab="League (American vs. National)")
  plot(hSub$division.1986, col=c("green","yellow"), ylim = c(0, 180), main="Division Count in 1986", ylab="Number of Players", xlab="Divison (East vs. West)")
  hist(hSub$put.outs.1986, breaks = 50, xlim=c(0, 1500), ylim=c(0, 20), main="Put Outs during 1986 Season", xlab="Put Outs", ylab="Player Count")
  hist(hSub$assists.1986, breaks = 50, xlim=c(0, 500), ylim=c(0, 100), main="Assists during 1986 Season", xlab="Assists", ylab="Player Count")
  hist(hSub$errors.1986, breaks = 50, xlim=c(0, 35), ylim=c(0, 30), main="Errors during 1986 Season", xlab="Errors", ylab="Player Count")
  hist(hSub$salary.1987, breaks = 50, xlim=c(0, 2500), ylim=c(0, 30), main="Salary in 1987", xlab="Salary (in thousands of dollars)", ylab="Player Count")
  hist(log(hSub$salary.1987), breaks = 50, xlim=c(4, 8), ylim=c(0, 25), main="Log of Salary in 1987", xlab="Log of Salary", ylab="Player Count")
  

#Figure out which model to use
  
  
  # Change salary to logSalary.1987
  hSub$logSalary.1987 <- log(hSub$salary.1987)
  
  
  # Remove regular salary from hSub
  hSub <- hSub[,c(-19)]
  
  
  # Subset data into training and test data -> SKIP THIS STEP AND JUST LOAD NEXT STEP (Max - 04/29/2019)
  
      # Initial selection of 80% of data as training and 20% as test
      library(caret)
      trainIdx <- createDataPartition(hSub$logSalary.1987, 100, p=0.80, list=F)
      View(trainIdx)
      
      # Select 50th data partition as indexes
      sample50 <- as.data.frame(trainIdx[,50])
      
      # Create training and test sets
      hSub_train <- hSub[sample50[,1],]
      hSub_test <- hSub[-sample50[,1],]
      
      # Save the training and test sets to be used again
      saveRDS(hSub_train, file="hSub_train.Rda")
      saveRDS(hSub_test, file="hSub_test.Rda")
  
      
  # Load training and test sets into R
  hSub_train <- readRDS(file="hSub_train.Rda")
  hSub_test <- readRDS(file="hSub_test.Rda")
  
  
  # Load MASS package to peform linear regression
  library(MASS)
  
  
  # Determine lowest AIC model with all vars in hSub_train using step-wise direction = "both"
  mlr_allVars <- lm(logSalary.1987 ~ .,data=hSub_train)
  sW_allVars_both <- stepAIC(mlr_allVars, direction="both")
  sW_allVars_both$anova # AIC: -253.9885
  
  
  # Analyze model with lowest AIC
  mlr_sW_both <- lm(logSalary.1987 ~ at.bats.1986 + hits.1986 + home.runs.1986 + 
                      runs.1986 + walks.1986 + years.in.mlb + put.outs.1986 + errors.1986, data = hSub_train)
  summary(mlr_sW_both) # Multiple R-squared: 0.6497,	Adjusted R-squared: 0.6361, Residual standard error: 0.5412, p-value: < 2.2e-16
  df_both_all <- data.frame(mlr_sW_both$coefficients)
  df_both_all$unitPercentChanges <- (exp(df_both_all$mlr_sW_both.coefficients) - 1) * 100
  df_both_all
  
  
  # Analyze model with lowest AIC but only keeping significant variables
  mlr_sW_both_sigVars <- lm(logSalary.1987 ~ hits.1986 + home.runs.1986 + years.in.mlb + put.outs.1986, data = hSub_train)
  summary(mlr_sW_both_sigVars) # Multiple R-squared: 0.6183,	Adjusted R-squared: 0.611, Residual standard error: 0.5595, p-value: < 2.2e-16
  df_both_sigVars <- data.frame(mlr_sW_both_sigVars$coefficients)
  df_both_sigVars$unitPercentChanges <- (exp(df_both_sigVars$mlr_sW_both_sigVars.coefficients) - 1) * 100
  df_both_sigVars
  
  
  # Analyze model again only keeping significant variables
  #mlr_sW_both_sigVars2 <- lm(logSalary.1987 ~ hits.1986 + home.runs.1986 + years.in.mlb + put.outs.1986 + walks.1986, data = hSub_train)
  #summary(mlr_sW_both_sigVars2) # Multiple R-squared: 0.6359,	Adjusted R-squared: 0.6253, Residual standard error: 0.5529, p-value: < 2.2e-16
  #df_both_sigVars2 <- data.frame(mlr_sW_both_sigVars2$coefficients)
  #df_both_sigVars2$unitPercentChanges <- (exp(df_both_sigVars2$mlr_sW_both_sigVars2.coefficients) - 1) * 100
  #df_both_sigVars2
  
  
  #
  # Model choice
  #
  # We choose the model "mlr_sW_both_sigVars" because it meets all of our criteria:
  # most parsimonious, relatively high r-squared, low difference in multiple r-squared 
  # and adjusted r-squared, and all variables are significant at the 0.05 level of significance.
  #
  # mlr_sW_both_sigVars <- lm(logSalary.1987 ~ hits.1986 + home.runs.1986 + years.in.mlb + put.outs.1986, data = hSub_train)
  # summary(mlr_sW_both_sigVars)
  #
  # Multiple R-squared:       0.6183
  # Adjusted R-squared:       0.611
  # Residual standard error:  0.5595
  # Model p-value:            < 2.2e-16
  #
  
  
#Validation analysis of test data
  
  # This is CIs for single point predictions on the career model
  predCis_hSub_test <- predict(mlr_sW_both_sigVars, newdata=hSub_test, interval="prediction")

  # Visualization of accuracy of testCareer predictions
  plot(hSub_test[,19], type="p", 
       ylim=c(min(predCis_hSub_test[,2],hSub_test[,19]),max(predCis_hSub_test[,3],hSub_test[,19])), 
       ylab="Log of Salary in 1987", main="Prediction Accuracy of Model")
  lines(predCis_hSub_test[,1],col="red")
  lines(predCis_hSub_test[,2],col="green")
  lines(predCis_hSub_test[,3],col="green")


  # True/False if hSub_test data points fall above or below the career predCIs.
  error <- (hSub_test$logSalary.1987 > predCis_hSub_test[,3]) | (hSub_test$logSalary.1987 < predCis_hSub_test[,2])
  error
  sum(error) / (dim(hSub_test)[1]) * 100 # Error rate: 3.921569
  

#Check linear regression assumptions
  
  attach(hSub_train)
  
  # Assumption 1 - Independent variables (X) and Y have a linear relationship
  par(mfrow=c(1,4))
  plot(logSalary.1987~ hits.1986, pch=19)
  plot(logSalary.1987~ home.runs.1986, pch=19)       
  plot(logSalary.1987~ years.in.mlb, pch=19)  
  plot(logSalary.1987~ put.outs.1986, pch=19)
  
  
  # Assumption 2 - Constant Variance of Residuals Assumption
  residualsResults <- residuals(mlr_sW_both_sigVars)
  graphics.off()
  plot(logSalary.1987, residualsResults, main="Model Residuals Compared with Model Dependent Variable", ylab="Residual Value") #A linear pattern, but potentially constant variance at each range of logSalary
  
  
  # Assumption 3 - Normality of the Residuals
  par(mfrow=c(1,3))
  plot(residualsResults, ylab="Residual Value", main="Scatterplot of Model Residuals") #Randomly scattered
  hist(residualsResults, xlab="Residual Value", main="Histogram of Model Residuals") #Normal distribution
  qqnorm(residualsResults) #45 degree linear grouping
  library(nortest)
  lillie.test(residualsResults) #p-value = p-value = 0.8083; Fail to reject that the errors are normal
  
  
  # Assumption 4 - Independence of residuals
  acf(residualsResults, main="Autocorrelations of Residuals") #All autocorrelations are within dashed lines except for 1
  library(car)
  durbinWatsonTest(mlr_sW_both_sigVars) #P-value = 0.708, fail to reject the null (no autocorrelation), can suggest there is no autocorrelation among residuals
  
  
  # Assumption 5 - No multicollinearity between independent variables
  library(car)
  vif(mlr_sW_both_sigVars)
  #
  # at.bats.1986    hits.1986       home.runs.1986        
  # 15.248254       13.757157       1.735736    
  # !!!             !!!             !
  #
  # rbi.1986        walks.1986      years.in.mlb
  # 3.876014        1.789959        1.103909
  # !               !               !
  #
  # Legend:
  # 1 < VIF < 5	  = Moderately correlated             !
  # VIF > 5 to 10	= Highly correlated                 !!
  # VIF > 10      = Very serious multicollinearity    !!!
  
  
########## END SCRIPT