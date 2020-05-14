# Importing the MPV library for datasets
library(MPV)
# Displaying the table B1
table.b1
# Answer 2.1.A : Fitting a linear equation
LinearEquation = lm(y ~ x8, data = table.b1)
LinearEquation
# Answer 2.1.B COnducting AOV test
summary(aov(y ~ x8, data = table.b1))
# Singnificance of Regression
summary(LinearEquation)

# Answer 2.1.C 95% CI on the slope
confint(LinearEquation, level=0.95)
# Answer D: the R-squared value is 0.5447 
#the percent of the total variability explained in this model is 54.47 %

x = 2000
beta_0 = coef(LinearEquation)[1]
beta_1 = coef(LinearEquation)[2]
y = beta_0 + beta_1 * x
beta_0
beta_1
y

# ANswer 2.1.E
xvals = data.frame(x8 = x)
xvals
predict(LinearEquation, newdata = xvals, interval = "confidence", level = 0.95)

# Answer 2.13.A
p2.13
plot(days ~ index, data = p2.13)

# Answer 2.13.B

Linear_Equation_2_13 = lm(days ~ index, data = p2.13)
Linear_Equation_2_13

# Answer 2.13.C significance of regression

summary(aov(days ~ index, data = p2.13))

# Answer 2.13.d

index1 = data.frame(index = seq(16,18.2, length = 200))
confidence_band = predict(Linear_Equation_2_13, index1, interval = "confidence", confidence = 0.95 )
prediction_band = predict(Linear_Equation_2_13, index1, interval = "prediction", confidence = 0.95 )

plot(days ~ index, data = p2.13, ylim = c(-20,160))
abline(Linear_Equation_2_13, lwd =2, col = "red")
lines(index1[,1], confidence_band[,2], lty = 2, col = "blue", lwd = 2)
lines(index1[,1], confidence_band[,3], lty = 2, col = "blue", lwd = 2)
lines(index1[,1], prediction_band[,2], lty = 4, col = "green", lwd = 2)
lines(index1[,1], prediction_band[,3], lty = 4, col = "green", lwd = 2)
legend("topright", legend = c("Fit", "95% CI", "95% PI"), lty = c(2,4), lwd = c(2,2), col = c("red","blue", "green"))

# Answer 2.15.A

Linear_equation_2_15 = lm(visc ~ temp, data = p2.15)
(Linear_equation_2_15)

# Answer 2.15.B
summary(Linear_equation_2_15)
summary(aov(Linear_equation_2_15))

# Answer 2.15.C

Linear_equation_2_15 = lm(visc ~ temp, data = p2.15)

index1 = data.frame(temp = seq(20,100, length = 200))
confidence_band = predict(Linear_equation_2_15, index1, interval = "confidence", confidence = 0.95 )
prediction_band = predict(Linear_equation_2_15, index1, interval = "prediction", confidence = 0.95 )

plot(visc ~ temp, data = p2.15, ylim = c(0,1.5))
abline(Linear_equation_2_15, lwd =2, col = "red")
lines(index1[,1], confidence_band[,2], lty = 2, col = "blue", lwd = 2)
lines(index1[,1], confidence_band[,3], lty = 2, col = "blue", lwd = 2)
lines(index1[,1], prediction_band[,2], lty = 4, col = "green", lwd = 2)
lines(index1[,1], prediction_band[,3], lty = 4, col = "green", lwd = 2)
legend("topright", legend = c("Fit", "95% CI", "95% PI"), lty = c(2,4), lwd = c(2,2), col = c("red","blue", "green"))