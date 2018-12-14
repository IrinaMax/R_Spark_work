# some useful scripts
#I train several models and compare their test metric all in one pipeline. map purrr
models = tibble(Model = c('Linear Regression','MARS','RF','Linear Regression (only time covariates)'),
                models = list(linear.regression, earth, rf, linear.regression.just.time),
                Y_test= list(test.set$WR),
                X_test = list(test.set %>% select(-WR))
)  %>%
  mutate( Y_pred = map2(models, X_test, ~ predict.train(.x, newdata = .y)),
          RMSE = map2_dbl(Y_test, Y_pred, rmse ))

# Simulate the prabability on the areas

library(TeachBayes)
areas <- c(3,6,7,23,15,8)
spinner_plot(areas)
df <-data.frame(Region = 1:6, areas, Probability = areas/sum(areas))
df
# Region areas Probability
# 1      1     3  0.04838710
# 2      2     6  0.09677419
# 3      3     7  0.11290323
# 4      4    23  0.37096774
# 5      5    15  0.24193548
# 6      6     8  0.12903226
ten_spin <- spinner_data(areas, 10)
ten_spin
many_spins <- spinner_data(areas, 1000)
bar_plot(many_spins)
#simulation data displayed as data table
(S <- summarise(group_by(data.frame(Region = many_spins), Region), N = n()))
# A tibble: 6 x 2
# Region     N
# <int> <int>
#   1      1    56
# 2      2   108[2]
# 3      3   118
# 4      4   368
# 5      5   228
# 6      6   122
# Probability of Region
(Freq_1 <- sum(S$N[S$Region ==1]))
#[1] 56
(Prob_1 <- Freq_1/1000)
#  [1] 0.056     is an aproximation to the actual probability 0.48
------------------------------
  # Constracting spinner
  # Define a spinner with five regions: regions
  regions <- c(2,2,2,2,2)
# Plot the spinner
spinner_plot(regions)

# Show the probability distribution
spinner_probs(regions)
---------------------------
  # Define new spinner: regions
  regions <- c(2,2,4)

# Simulation 1000 spins: spins
spins <- spinner_data(regions, 1000)

# Graph the spin data using bar_plot()
bar_plot(spins)

# Construct frequency table of spins
table(spins)
table<- table(spins)

# Find fraction of spins equal to 2
mean(spins == 2)

# Find mean spin value
mean(table(spins))
#-------------------------------------    Next session

(bayes_df <- data.frame(Model = paste("Spinner", c("A", "B", "C", "D"))))
bayes_df$Prior <- rep(0.25, 4)   # example of the uniform prior since prior probabilities are spread uniformly
bayes_df
bayes_df$Likelihood <- round(c(1/3,1/2,1/4,1/6), 2)
bayes_df
# Posterior probabolity is proportional to Prior Prob X Likelihood
# Prior X Likelihood = Product
# Product/sum(Product) = Posterior
bayesian_crank(bayes_df)
#    Model     Prior  Likelihood Product Posterior
# 1 Spinner A  0.25       0.33  0.0825     0.264
# 2 Spinner B  0.25       0.50  0.1250     0.400
# 3 Spinner C  0.25       0.25  0.0625     0.200
# 4 Spinner D  0.25       0.17  0.0425     0.136

# Regions for Spinner A
regA <- c(1, 2, 3)

# Regions for Spinner B
regB <- c(3, 2, 1)

# Create the vector of models: Model
Model <- c("Spinner A", "Spinner B")

# Define the vector of prior probabilities: Prior
Prior <- rep(1/2, 2)
Prior
# Define the vector of likelihoods: Likelihood
Likelihood <- c(1/2, 1/6)
Likelihood
# Make a data frame with variables Model, Prior, Likelihood: bayes_df
bayes_df <- data.frame(Model, Prior, Likelihood)
#bayes_df$Prior <- Prior
#bayes_df$Likelihood <- Likelihood
bayes_df
# Compute the posterior probabilities
bayesian_crank(bayes_df)
prior_post_plot(bayes_df)

# Display the vector of models: Model
Model <- c("Spinner A", "Spinner B")
Model
# Define the vector of prior probabilities: Prior
Prior <- c(0.75, 0.25)
Prior
# Define the vector of likelihoods: Likelihood
Likelihood <- c(0.5,1/6)

# Make a data frame with variables Model, Prior, Likelihood: bayes_df
bayes_df <- data.frame(Model, Prior, Likelihood)
bayes_df
# Compute the posterior probabilities
bayesian_crank(bayes_df)
prior_post_plot(bayes_df)