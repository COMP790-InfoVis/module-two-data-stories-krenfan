#comp790 assignment 2

#set working directory
setwd("/Users/kelly/OneDrive - University of North Carolina at Chapel Hill/Documents/790/assignment 2")

#load dataset
sleepscore=read.csv(file='SleepStudy.csv',header=TRUE,stringsAsFactors=TRUE)

# depression linear regression line
d_model <- lm(PoorSleepQuality ~ DepressionScore, data = sleepscore)

summary(d_model)

plot(sleepscore$DepressionScore, sleepscore$PoorSleepQuality, 
     main="Scatterplot of Depression Score as Predictor for Poor Sleep Quality", 
     xlab="Depression Score", ylab="Poor Sleep Quality")
abline(a=coef(d_model)[1], b=coef(d_model)[2])
mtext("y = 0.21101x + 5.15931", side=3)

# anxiety linear regression line
a_model <- lm(PoorSleepQuality ~ AnxietyScore, data = sleepscore)

summary(a_model)

plot(sleepscore$AnxietyScore, sleepscore$PoorSleepQuality, 
     main="Scatterplot of Anxiety Score as Predictor for Poor Sleep Quality", 
     xlab="Anxiety Score", ylab="Poor Sleep Quality")
abline(a=coef(a_model)[1], b=coef(a_model)[2])
mtext("y = 0.25354x + 4.89503", side=3)

# stress linear regression line
s_model <- lm(PoorSleepQuality ~ StressScore, data = sleepscore)

summary(s_model)

plot(sleepscore$StressScore, sleepscore$PoorSleepQuality, 
     main="Scatterplot of Stress Score as Predictor for Poor Sleep Quality", 
     xlab="Stress Score", ylab="Poor Sleep Quality")
abline(a=coef(s_model)[1], b=coef(s_model)[2])
mtext("y = 0.1203x + 5.1183", side=3)
