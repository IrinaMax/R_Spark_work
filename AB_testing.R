# A/B test
install.packages("bayesAB")
library(bayesAB)
# First collection
control_1 <- rbinom(20, 1, 0.5)
treatment_1 <- rbinom(20, 1, 0.3)

#  Analysis
test1 <- bayesTest(treatment_1, control_1, distribution = "bernoulli", priors = c("alpha" = 10, "beta" = 10))
print(test1)
summary(test1)
plot(test1)

# Second Collection
control_2 <- rbind(control_1, rbinom(20, 1, 0.5))
treatment_2 <- rbind(treatment_1, rbinom(20, 1, 0.3))

#  Analysis 2
test2 <- bayesTest(treatment_2, control_2, distribution = "bernoulli", priors = c("alpha" = 10, "beta" = 10))
print(test2)
summary(test2)
plot(test2)

#We can see that with the additional 40 observations, the distributions have separated more, and the probability that the treatment is less than the control is 98 percent.