# Importing the MPV library for datasets
library(MPV)
# Displaying the table B1
table.b1
# Answer 3.1.A : Fitting a linear equation
LinearEquation3_1 = lm(y ~ x2+x7+x8, data = table.b1)
LinearEquation3_1

# Answer 3.1.B COnducting AOV test
summary(aov(y ~ x2+x7+x8, data = table.b1))


anova_table <- anova(lm(y ~ x2+x7+x8, data = table.b1))
anova_table

# Singnificance of Regression / R2 and Adjusted R2
summary(LinearEquation3_1)
