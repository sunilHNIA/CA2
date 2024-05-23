#Installing the required libraries
if (!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}
library(readxl)

#Creating the dataframe using excel data
health_data <- read_excel('/Users/sunilsharma/Desktop/DataScienceTest/Dataset_2024.xlsx')
str(health_data)
View(health_data)
#Total no of rows in the data frame
nrow(health_data)
nrow(na.omit(health_data))

colnames(health_data)[colnames(health_data) == "Age (years)"] <- "Age"
colnames(health_data)[colnames(health_data) == "Body fat (%)"] <- "Body_fat"
colnames(health_data)[colnames(health_data) == "Chest circumference (cm)"] <- "Chest_circumference"
colnames(health_data)[colnames(health_data) == "Knee circumference (cm)"] <- "Knee_circumference"
colnames(health_data)[colnames(health_data) == "Density (g/cm³)"] <- "Density"
colnames(health_data)[colnames(health_data) == "Weight (lbs)"] <- "Weight"

str(health_data)

summary(health_data)

library(psych)
pairs.panels(health_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

scatter.smooth(x = health_data$Age,
               y = health_data$Body_fat,
               xlab = "Age",
               ylab = "Body fat %", main = "Correlation of Bodyfat ~ Age")

scatter.smooth(x = health_data$Chest_circumference,
               y = health_data$Body_fat,
               xlab = " Chest circumference",
               ylab = "Body fat %", main = "Correlation of Bodyfat ~ Chest_circumference")

scatter.smooth(x = health_data$Knee_circumference,
               y = health_data$Body_fat,
               xlab = " Knee_circumference",
               ylab = "Body fat %", main = "Correlation of Bodyfat ~ Knee_circumference")

scatter.smooth(x = health_data$Density,
               y = health_data$Body_fat,
               xlab = "Density",
               ylab = "Body fat %", main = "Correlation of Bodyfat ~ Density")


scatter.smooth(x = health_data$Weight,
               y = health_data$Body_fat,
               xlab = "Weight",
               ylab = "Body fat %", main = "Correlation of Bodyfat ~ Weight")




# Calculate descriptive statistics


#examing corr in R
cor(health_data)

attach(health_data)
# Examining the other variables
paste("Correlation for Body_fat and Age: ", round(cor(Body_fat, Age),2))
paste("Correlation for Body_fat and Chest_circumference: ", round(cor(Body_fat, Chest_circumference),2))
paste("Correlation for Body_fat and Knee_circumference: ", round(cor(Body_fat, Knee_circumference),2))
paste("Correlation for Body_fat and Density: ", round(cor(Body_fat, Density),2))
paste("Correlation for Body_fat and Weight: ", round(cor(Body_fat, Weight),2))
#Age and Knee_circumference have very low corr with bodyfat

#boxplots
par(mar = c(1, 1, 1, 1))
attach(health_data)

boxplot(Body_fat,
        main = "Body_fat") # box plot for 'Body_fat'

boxplot(Age,
        main = "Age") # box plot for 'Age'

boxplot(Chest_circumference,
        main = "Chest_circumference") # box plot for 'Chest_circumference'

boxplot(Knee_circumference,
        main = "Knee_circumference") # box plot for 'Knee_circumference'


boxplot(Density,
        main = "Density") # box plot for 'Density'


boxplot(Weight,
        main = "Weight") # box plot for 'Weight'

# Use boxplot.stats() function to generate relevant outliers

outlier_values <- boxplot.stats(health_data$Weight)$out # outlier values.
paste("Weight outliers: ", paste(outlier_values, sep =", "))

# Remove population outliers
health_data <- subset(health_data,
                 health_data$Weight != 262.75
                 & health_data$Weight != 363.15)

outlier_values <- boxplot.stats(health_data$Weight)$out # outlier values.
paste("Weight outliers: ", paste(outlier_values, sep =", "))
par(mar = c(1, 1, 1, 1))
par(mfrow = c(3, 2))
#skewness test
plot(density(health_data$Body_fat),
     main = "Density plot : Body_fat",
     ylab = "Frequency", xlab = "Body_fat",
     sub = paste("Skewness : ", round(e1071::skewness(health_data$Body_fat), 2)))
polygon(density(health_data$Body_fat), col = "red")

plot(density(health_data$Age),
     main = "Density plot : Age",
     ylab = "Frequency", xlab = "Body_fat",
     sub = paste("Skewness : ", round(e1071::skewness(health_data$Age), 2)))
polygon(density(health_data$Age), col = "red")

plot(density(health_data$Chest_circumference),
     main = "Density plot : Chest_circumference",
     ylab = "Frequency", xlab = "Chest_circumference",
     sub = paste("Skewness : ", round(e1071::skewness(health_data$Chest_circumference), 2)))
polygon(density(health_data$Chest_circumference), col = "red")

plot(density(health_data$Knee_circumference),
     main = "Density plot : Knee_circumference",
     ylab = "Frequency", xlab = "Knee_circumference",
     sub = paste("Skewness : ", round(e1071::skewness(health_data$Knee_circumference), 2)))
polygon(density(health_data$Knee_circumference), col = "red")

plot(density(health_data$Density),
     main = "Density plot : Density",
     ylab = "Frequency", xlab = "Density",
     sub = paste("Skewness : ", round(e1071::skewness(health_data$Density), 2)))
polygon(density(health_data$Density), col = "red")

plot(density(health_data$Weight),
     main = "Density plot : Weight",
     ylab = "Frequency", xlab = "Weight",
     sub = paste("Skewness : ", round(e1071::skewness(health_data$Weight), 2)))
polygon(density(health_data$Weight), col = "red")

# Minimal skewness = -0.11 - slightly skewed to the left. 
# NB a skewness value <-1 or >1 = highly skewed. 
# Skewness -1 to -.05 and 0.5 to 1 = moderately skewed. 
# And skewness -0.5 to 0.5 = approx symetric.

paste("Skewness for Body_fat : ", round(e1071::skewness(health_data$Body_fat), 2))
paste("Skewness for Age : ", round(e1071::skewness(health_data$Age), 2))
paste("Skewness for Chest_circumference) : ", round(e1071::skewness(health_data$Chest_circumference), 2))
paste("Skewness for Knee_circumference : ", round(e1071::skewness(health_data$Knee_circumference), 2))
paste("Skewness for Density : ", round(e1071::skewness(health_data$Density), 2))
paste("Skewness for Weight: ", round(e1071::skewness(health_data$Weight), 2))

#all variables are symetric

# Check normality of all variables using normality test
shapiro.test(health_data$Body_fat)
shapiro.test(health_data$Age) 
shapiro.test(health_data$Chest_circumference)
shapiro.test(health_data$Knee_circumference)
shapiro.test(health_data$Density) 
shapiro.test(health_data$Weight)
#Weight is not normally distributed
#Chest_circumference is not normally distributed
#Age is not normally distributed

# Need to transform Weight, Chest_circumference and Age
attach(health_data)
library(MASS)

box_cox_transform <- boxcox(Body_fat~Weight)
box_cox_transform
#windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_Weight <-(Body_fat^lamda-1)/lamda
normalised_Weight
hist(normalised_Weight)
shapiro.test(normalised_Weight)

##Modify the variable Population
health_data$Weight_new <- normalised_Weight
shapiro.test(health_data$Weight_new)

View(health_data)

#normalising chest circumference
box_cox_transform <- boxcox(Body_fat~Chest_circumference)
box_cox_transform
#windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_Chest_circumference <-(Body_fat^lamda-1)/lamda
normalised_Chest_circumference
hist(normalised_Chest_circumference)
shapiro.test(normalised_Chest_circumference)

##Modify the variable Population
health_data$Chest_circumference_new <- normalised_Chest_circumference
shapiro.test(health_data$Chest_circumference_new)

#View(health_data)

#normalising Age variable
box_cox_transform <- boxcox(Body_fat~Age)
box_cox_transform
#windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_Age <-(Body_fat^lamda-1)/lamda
normalised_Age
hist(normalised_Age)
shapiro.test(normalised_Age)

#Modify the variable Age
health_data$Age_new <- normalised_Age
shapiro.test(health_data$Age_new)


# check multicollinearity
#The closer r is to +1, the stronger the positive correlation is.
#The closer r is to -1, the stronger the negative correlation is. If |r| = 1 exactly,
#the two variables are perfectly correlated! 
#Temperature in Celsius and Fahrenheit are perfectly correlated.
cor(health_data)

str(health_data)
attach(health_data)
model_1 <- lm( Body_fat ~ 
                 Age_new + 
                 Chest_circumference_new +
                 Density + 
                 Knee_circumference + 
                 Weight_new)
          

model_1
summary(model_1)

#calculate confidence interval for regression coefficient for 'Body_fat'
confint(model_1, 'Body_fat', level=0.95)

model_2 <- lm( Body_fat ~ 
                 Age_new + 
                 Chest_circumference_new +
                 Weight_new)


model_2
summary(model_2)

AIC(model_1)
AIC(model_2)
BIC(model_1)
BIC(model_2)


##residuals normally distributed
shapiro.test(residuals(model_1))
##residuals diffrent from zero
t.test(residuals(model_1),mu=0) 


# randomness of residuals (autocorrelation) -- Durbin-Watson test
dwtest(model_1)


##residuals normally distributed
shapiro.test(residuals(model_2))
##residuals diffrent from zero
t.test(residuals(model_2),mu=0) 


# randomness of residuals (autocorrelation) -- Durbin-Watson test
#install.packages("lmtest")
library(lmtest)
dwtest(model_2)

#VIF = 1: There is no correlation between a given predictor variable and any other predictor variables in the model.
#VIF between 1 and 5: There is moderate correlation between a given predictor variable and other predictor variables in the model.
#VIF > 5: There is severe correlation between a given predictor variable and other predictor variables in the model.


install.packages("car")
library(car)
v1 <-vif(model_1)
v1
v2 <-vif(model_2)
v2


model_3 <- lm( Body_fat ~ 
                 Age + 
                 Density +
                 Weight_new)


model_3
summary(model_3)

model_4 <- lm( Body_fat ~ 
                    Age + 
                    Density +
                    Weight +
                    Chest_circumference +
                     Knee_circumference)


model_4
summary(model_4)
dwtest(model_3)
dwtest(model_4)


v3 <-vif(model_3)
v3
v4 <-vif(model_4)
v4

AIC(model_3)
AIC(model_4)
BIC(model_3)
BIC(model_4)


##residuals normally distributed
shapiro.test(residuals(model_3))
##residuals diffrent from zero
t.test(residuals(model_3),mu=0) 

##residuals normally distributed
shapiro.test(residuals(model_4))
##residuals diffrent from zero
t.test(residuals(model_4),mu=0) 


