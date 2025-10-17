# MaximillianBalter_Unit1_GMMAnalysis_BIOL672.r
# Author: Maximillian Balter
# OS: macOS
# Libraries/packages used: ggplot2, dplyr, mixtools, fitdistrplus
# Input data files used: iris_csv.data
# Output files generated: iris_gmm_analysis.pdf, iris_density_models.txt, iris_bic_comparison.txt
# Analysis of iris flower size using multiple probability density models
# This script performs Step 16 from the assignment sheet: fitting and comparing normal, lognormal, gamma, and Gaussian Mixture Models using BIC

## Section 1 — Library Loading and Data Preparation

cat("STEP 16: GAUSSIAN MIXTURE MODEL AND MULTIMODEL INFERENCE\n")
cat("=========================================================\n\n")

# I need to load the required packages for this analysis
if (!require(ggplot2)) { # I check if ggplot2 is available for plotting.
  install.packages('ggplot2') # I install ggplot2 if needed.
  library(ggplot2) # I load ggplot2 for creating visualizations.
}

if (!require(dplyr)) { # I check if dplyr is available for data manipulation.
  install.packages('dplyr') # I install dplyr if needed.
  library(dplyr) # I load dplyr for data processing.
}

if (!require(mixtools)) { # I check if mixtools is available for Gaussian Mixture Models.
  install.packages('mixtools') # I install mixtools if needed.
  library(mixtools) # I load mixtools for GMM fitting.
}

if (!require(fitdistrplus)) { # I check if fitdistrplus is available for distribution fitting.
  install.packages('fitdistrplus') # I install fitdistrplus if needed.
  library(fitdistrplus) # I load fitdistrplus for probability distribution analysis.
}

# I load the original iris dataset
iris_data <- read.csv("Iris_original_data/iris_csv.data", header = FALSE) # I read the iris dataset.
colnames(iris_data) <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "species") # I set appropriate column names.

cat("Dataset loaded successfully.\n")
cat("Dimensions:", nrow(iris_data), "observations,", ncol(iris_data), "variables\n\n")

# I create a composite 'size' variable following the assignment example
iris_data$size <- (iris_data$sepal_length + iris_data$sepal_width + iris_data$petal_length + iris_data$petal_width) / 4 # I calculate average size across all measurements.

cat("Created composite 'size' variable as the average of all four morphological measurements.\n")
cat("Size variable summary:\n")
print(summary(iris_data$size)) # I print summary statistics for the size variable.

## Section 2 — Exploratory Data Analysis

cat("\n--- Initial Distribution Exploration ---\n")

# I examine the distribution of the size variable to see if it appears multimodal
size_data <- iris_data$size # I extract the size variable for analysis.

# I create a histogram to visualize the distribution
hist_plot <- ggplot(iris_data, aes(x = size)) + # I create a histogram of the size variable.
  geom_histogram(bins = 30, alpha = 0.7, color = "black", fill = "lightblue") + # I add histogram bars.
  labs(title = "Distribution of Iris Flower Size", # I add a title.
       subtitle = "Initial exploration for multimodality", # I add a subtitle.
       x = "Average Size (cm)", y = "Frequency") + # I label the axes.
  theme_minimal() # I use a clean theme.

# I also create a density plot to better see potential multimodality
density_plot <- ggplot(iris_data, aes(x = size)) + # I create a density plot.
  geom_density(alpha = 0.7, fill = "lightblue", color = "black") + # I add the density curve.
  labs(title = "Density Plot of Iris Flower Size", # I add a title.
       subtitle = "Smooth density estimation", # I add a subtitle.
       x = "Average Size (cm)", y = "Density") + # I label the axes.
  theme_minimal() # I use a clean theme.

# I examine by species to understand potential sources of multimodality
species_plot <- ggplot(iris_data, aes(x = size, fill = species)) + # I create a plot colored by species.
  geom_density(alpha = 0.6) + # I add density curves for each species.
  labs(title = "Iris Size Distribution by Species", # I add a title.
       subtitle = "Potential source of multimodality", # I add a subtitle.
       x = "Average Size (cm)", y = "Density") + # I label the axes.
  theme_minimal() + # I use a clean theme.
  theme(legend.position = "right") # I position the legend.

cat("Visual inspection suggests potential multimodality, possibly due to species differences.\n")
cat("Proceeding with model fitting to quantify this observation.\n\n")

## Section 3 — Probability Density Model Fitting

cat("--- Fitting Four Probability Density Models ---\n")

# Model 1: Normal Distribution
cat("1. Fitting Normal Distribution...\n")
normal_fit <- fitdist(size_data, "norm") # I fit a normal distribution to the size data.
normal_aic <- normal_fit$aic # I extract the AIC value.
normal_bic <- normal_fit$bic # I extract the BIC value.
normal_loglik <- normal_fit$loglik # I extract the log-likelihood.

cat("   Normal distribution fitted successfully.\n")
cat("   Parameters: mean =", round(normal_fit$estimate[1], 3), ", sd =", round(normal_fit$estimate[2], 3), "\n")
cat("   Log-likelihood =", round(normal_loglik, 3), "\n")
cat("   BIC =", round(normal_bic, 3), "\n\n")

# Model 2: Log-Normal Distribution
cat("2. Fitting Log-Normal Distribution...\n")
lognormal_fit <- fitdist(size_data, "lnorm") # I fit a log-normal distribution.
lognormal_aic <- lognormal_fit$aic # I extract the AIC value.
lognormal_bic <- lognormal_fit$bic # I extract the BIC value.
lognormal_loglik <- lognormal_fit$loglik # I extract the log-likelihood.

cat("   Log-normal distribution fitted successfully.\n")
cat("   Parameters: meanlog =", round(lognormal_fit$estimate[1], 3), ", sdlog =", round(lognormal_fit$estimate[2], 3), "\n")
cat("   Log-likelihood =", round(lognormal_loglik, 3), "\n")
cat("   BIC =", round(lognormal_bic, 3), "\n\n")

# Model 3: Gamma Distribution (my choice)
cat("3. Fitting Gamma Distribution (my additional choice)...\n")
gamma_fit <- fitdist(size_data, "gamma") # I fit a gamma distribution.
gamma_aic <- gamma_fit$aic # I extract the AIC value.
gamma_bic <- gamma_fit$bic # I extract the BIC value.
gamma_loglik <- gamma_fit$loglik # I extract the log-likelihood.

cat("   Gamma distribution fitted successfully.\n")
cat("   Parameters: shape =", round(gamma_fit$estimate[1], 3), ", rate =", round(gamma_fit$estimate[2], 3), "\n")
cat("   Log-likelihood =", round(gamma_loglik, 3), "\n")
cat("   BIC =", round(gamma_bic, 3), "\n\n")

# Model 4: Gaussian Mixture Model
cat("4. Fitting Gaussian Mixture Model (2 components)...\n")

# I fit a 2-component Gaussian Mixture Model using EM algorithm
gmm_fit <- normalmixEM(size_data, k = 2, maxit = 1000, epsilon = 1e-08) # I fit a 2-component GMM.

# I calculate BIC for the GMM manually since mixtools doesn't provide it directly
n <- length(size_data) # I get the number of observations.
k_params <- 5 # I count parameters: 2 means, 2 variances, 1 mixing proportion (other is constrained).
gmm_loglik <- gmm_fit$loglik # I extract the log-likelihood from the GMM fit.
gmm_bic <- -2 * gmm_loglik + k_params * log(n) # I calculate BIC manually.

cat("   Gaussian Mixture Model fitted successfully.\n")
cat("   Component 1: mean =", round(gmm_fit$mu[1], 3), ", sd =", round(sqrt(gmm_fit$sigma[1]), 3), ", weight =", round(gmm_fit$lambda[1], 3), "\n")
cat("   Component 2: mean =", round(gmm_fit$mu[2], 3), ", sd =", round(sqrt(gmm_fit$sigma[2]), 3), ", weight =", round(gmm_fit$lambda[2], 3), "\n")
cat("   Log-likelihood =", round(gmm_loglik, 3), "\n")
cat("   BIC =", round(gmm_bic, 3), "\n\n")

## Section 4 — Model Comparison Using BIC

cat("--- Bayesian Information Criterion (BIC) Comparison ---\n")

# I create a summary table of all model results
model_comparison <- data.frame( # I create a dataframe to compare models.
  Model = c("Normal", "Log-Normal", "Gamma", "Gaussian Mixture Model"), # I list the model names.
  LogLikelihood = c(normal_loglik, lognormal_loglik, gamma_loglik, gmm_loglik), # I include log-likelihoods.
  BIC = c(normal_bic, lognormal_bic, gamma_bic, gmm_bic), # I include BIC values.
  DeltaBIC = NA # I will calculate delta BIC values.
)

# I calculate delta BIC (difference from the best model)
best_bic <- min(model_comparison$BIC) # I find the lowest (best) BIC value.
model_comparison$DeltaBIC <- model_comparison$BIC - best_bic # I calculate differences from the best.

# I sort by BIC (best to worst)
model_comparison <- model_comparison[order(model_comparison$BIC), ] # I sort models by BIC value.

cat("Model Comparison Results (sorted by BIC, lower is better):\n")
print(model_comparison) # I print the comparison table.

# I identify the best model
best_model <- model_comparison$Model[1] # I get the name of the best model.
cat("\nBest fitting model:", best_model, "\n")

# I interpret the BIC differences
cat("\nBIC Interpretation:\n")
for (i in 1:nrow(model_comparison)) { # I loop through each model.
  delta <- model_comparison$DeltaBIC[i] # I get the delta BIC for this model.
  model_name <- model_comparison$Model[i] # I get the model name.
  
  if (delta == 0) { # If this is the best model.
    cat("-", model_name, ": Best model (reference)\n")
  } else if (delta <= 2) { # If delta BIC is small.
    cat("-", model_name, ": Substantial support (ΔIC ≤ 2)\n")
  } else if (delta <= 6) { # If delta BIC is moderate.
    cat("-", model_name, ": Considerably less support (2 < ΔIC ≤ 6)\n")
  } else { # If delta BIC is large.
    cat("-", model_name, ": Essentially no support (ΔIC > 6)\n")
  }
}

## Section 5 — Visualization and Results Interpretation

cat("\n--- Creating Comprehensive Visualization ---\n")

# I create a comprehensive plot showing the data with all fitted models
x_range <- seq(min(size_data), max(size_data), length.out = 1000) # I create a sequence for smooth curves.

# I calculate density values for each fitted distribution
normal_density <- dnorm(x_range, mean = normal_fit$estimate[1], sd = normal_fit$estimate[2]) # I calculate normal density.
lognormal_density <- dlnorm(x_range, meanlog = lognormal_fit$estimate[1], sdlog = lognormal_fit$estimate[2]) # I calculate log-normal density.
gamma_density <- dgamma(x_range, shape = gamma_fit$estimate[1], rate = gamma_fit$estimate[2]) # I calculate gamma density.

# I calculate GMM density as weighted sum of components
gmm_density <- gmm_fit$lambda[1] * dnorm(x_range, mean = gmm_fit$mu[1], sd = sqrt(gmm_fit$sigma[1])) + 
               gmm_fit$lambda[2] * dnorm(x_range, mean = gmm_fit$mu[2], sd = sqrt(gmm_fit$sigma[2])) # I calculate GMM density.

# I create a dataframe for plotting
density_df <- data.frame( # I create a dataframe with all density curves.
  x = rep(x_range, 4), # I repeat x values for each model.
  density = c(normal_density, lognormal_density, gamma_density, gmm_density), # I combine all densities.
  model = rep(c("Normal", "Log-Normal", "Gamma", "GMM"), each = length(x_range)) # I label each curve.
)

# I create the comprehensive comparison plot
comparison_plot <- ggplot() + # I start building the plot.
  geom_histogram(data = iris_data, aes(x = size, y = ..density..), # I add the histogram of observed data.
                 bins = 20, alpha = 0.3, fill = "gray", color = "black") +
  geom_line(data = density_df, aes(x = x, y = density, color = model), size = 1.2) + # I add fitted density curves.
  labs(title = "Comparison of Probability Density Models for Iris Flower Size", # I add a title.
       subtitle = "Histogram shows observed data, lines show fitted models", # I add a subtitle.
       x = "Average Size (cm)", y = "Density", # I label the axes.
       color = "Model") + # I label the legend.
  theme_minimal() + # I use a clean theme.
  theme(legend.position = "right") # I position the legend.

# I save the visualization
ggsave("Assignment1_output/plots/iris_gmm_analysis.pdf", comparison_plot, width = 12, height = 8) # I save the plot.

## Section 6 — Final Interpretation and Conclusions

cat("\n--- Final Analysis and Biological Interpretation ---\n")

# I provide detailed interpretation based on the results
sink('Assignment1_output/results/iris_bic_comparison.txt') # I start writing results to a file.
cat('GAUSSIAN MIXTURE MODEL AND MULTIMODEL INFERENCE RESULTS\n') # I write a header.
cat('Iris Flower Size Analysis using Bayesian Information Criterion\n\n') # I add a description.

cat('ANALYSIS OVERVIEW:\n') # I start the overview section.
cat('This analysis fits four probability density models to iris flower size data:\n') # I explain the purpose.
cat('1. Normal distribution (simple unimodal)\n') # I list the first model.
cat('2. Log-normal distribution (right-skewed unimodal)\n') # I list the second model.
cat('3. Gamma distribution (flexible shape parameter)\n') # I list the third model.
cat('4. Gaussian Mixture Model (multimodal via EM algorithm)\n\n') # I list the fourth model.

cat('MODEL COMPARISON RESULTS:\n') # I start the results section.
print(model_comparison) # I include the comparison table.

cat('\nBEST MODEL:', best_model, '\n') # I report the best model.
cat('This model has the lowest BIC value, indicating the best balance of fit and complexity.\n\n') # I explain what this means.

# I provide biological interpretation
cat('BIOLOGICAL INTERPRETATION:\n') # I start the biological section.
if (best_model == "Gaussian Mixture Model") { # If GMM is best.
  cat('The Gaussian Mixture Model provides the best fit, indicating MULTIMODALITY in iris size.\n') # I interpret multimodality.
  cat('This suggests that the iris dataset contains distinct sub-populations with different size characteristics.\n') # I explain what this means biologically.
  cat('The two components likely correspond to species differences:\n') # I suggest the biological cause.
  cat('- Component 1: mean =', round(gmm_fit$mu[1], 3), 'cm (likely smaller species)\n') # I describe component 1.
  cat('- Component 2: mean =', round(gmm_fit$mu[2], 3), 'cm (likely larger species)\n') # I describe component 2.
  cat('This indicates LATENT STRUCTURE in the data that simple unimodal distributions cannot capture.\n\n') # I emphasize latent structure.
} else { # If a unimodal distribution is best.
  cat('A unimodal distribution (', best_model, ') provides the best fit.\n') # I report the result.
  cat('This suggests that despite species differences, iris size follows a single underlying distribution.\n') # I interpret unimodality.
  cat('The lack of strong multimodality may indicate that species differences are not extreme enough\n') # I explain why multimodality might not be detected.
  cat('to create distinct modes, or that size variation within species overlaps considerably.\n\n') # I continue the explanation.
}

cat('STATISTICAL SIGNIFICANCE:\n') # I start the statistical section.
for (i in 1:nrow(model_comparison)) { # I loop through models.
  delta <- model_comparison$DeltaBIC[i] # I get delta BIC.
  model_name <- model_comparison$Model[i] # I get model name.
  cat(model_name, '(ΔBIC =', round(delta, 2), '):', # I report delta BIC.
      if (delta == 0) 'Best model' else if (delta <= 2) 'Strong support' else if (delta <= 6) 'Moderate support' else 'Weak support', '\n') # I interpret support level.
}

cat('\nCONCLUSION:\n') # I start the conclusion.
cat('The BIC analysis reveals that') # I begin the conclusion statement.
if (best_model == "Gaussian Mixture Model") {
  cat(' latent structure exists in iris flower size data, best captured by a mixture model.\n') # I conclude latent structure exists.
  cat('This supports the hypothesis that complex, multimodal distributions require sophisticated\n') # I connect to the assignment theme.
  cat('modeling approaches beyond simple probability distributions.\n') # I emphasize the methodological point.
} else {
  cat(' iris flower size is adequately described by a simpler', tolower(best_model), 'distribution.\n') # I conclude simpler model is adequate.
  cat('While mixture models can be fitted, they do not provide superior explanatory power\n') # I note that complexity isn't always better.
  cat('for this particular dataset, demonstrating the importance of model selection.\n') # I emphasize model selection.
}

sink() # I stop writing to the file.

cat("Analysis complete!\n")
cat("Results saved to:\n")
cat("- iris_gmm_analysis.pdf (visualization)\n")
cat("- iris_bic_comparison.txt (detailed results)\n\n")

# I provide a final summary to the console
cat("FINAL SUMMARY:\n")
cat("Best model:", best_model, "\n")
cat("Evidence for multimodality:", ifelse(best_model == "Gaussian Mixture Model", "YES - latent structure detected", "NO - unimodal distribution adequate"), "\n")
cat("This", ifelse(best_model == "Gaussian Mixture Model", "supports", "does not support"), "the hypothesis that complex data requires sophisticated mixture modeling.\n")
