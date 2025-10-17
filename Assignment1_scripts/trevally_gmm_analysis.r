# MaximillianBalter_Unit1_TrevallyGMM_BIOL672.r
# Author: Maximillian Balter
# OS: macOS
# Libraries/packages used: MASS, mixtools, ggplot2
# Input data files used: trevally_multivariate_data.txt
# Output files generated: trevally_gmm_analysis.pdf, trevally_bic_comparison.txt
# Analysis of Giant Trevally morphological data using Gaussian Mixture Models and BIC comparison
# This script performs Step 16 from the assignment sheet

# I need to load the required libraries for this analysis
library(MASS)     # I use this for the fitdistr function to fit simple probability distributions
library(mixtools) # I use this for the normalmixEM function to fit Gaussian mixture models  
library(ggplot2)  # I use this for creating plots

# I load the trevally data that I've been working with throughout this assignment
trevally_data <- read.table("Assignment1_data/trevally_multivariate_data.txt", header = TRUE, sep = "\t")

cat("Giant Trevally Morphological Data Analysis\n")
cat("Step 16: Gaussian Mixture Model and BIC Comparison\n")
cat("=================================================\n\n")

# I'm going to analyze the Morphological_Index because it represents the overall size of the villi
size_variable <- trevally_data$Morphological_Index # I extract this variable for my analysis
cat("Variable analyzed: Morphological_Index (overall morphological size)\n")
cat("Sample size:", length(size_variable), "observations\n") # I report how many fish I have data for
cat("Range:", round(min(size_variable), 2), "to", round(max(size_variable), 2), "\n") # I show the range of values
cat("Mean:", round(mean(size_variable), 2), "\n") # I calculate the average size
cat("Standard deviation:", round(sd(size_variable), 2), "\n\n") # I calculate how much variation there is

# I want to see what the distribution looks like before I start fitting models
cat("Creating initial histogram to examine distribution shape...\n")
hist(size_variable, breaks = 15, main = "Distribution of Giant Trevally Morphological Index", 
     xlab = "Morphological Index", ylab = "Frequency", col = "lightblue") # I make a histogram to see the shape

# Now I fit the first model - a normal distribution
cat("Fitting Model 1: Normal Distribution\n")
normal_fit <- fitdistr(size_variable, "normal") # I fit a normal distribution to my data
normal_loglik <- normal_fit$loglik # I extract the log-likelihood value
normal_bic <- BIC(normal_fit) # I calculate the BIC for this model
cat("Normal - Log-likelihood:", round(normal_loglik, 3), "BIC:", round(normal_bic, 3), "\n")

# Now I fit the second model - a log-normal distribution  
cat("Fitting Model 2: Log-Normal Distribution\n")
lognormal_fit <- fitdistr(size_variable, "lognormal") # I fit a log-normal distribution
lognormal_loglik <- lognormal_fit$loglik # I get the log-likelihood
lognormal_bic <- BIC(lognormal_fit) # I calculate BIC for log-normal
cat("Log-normal - Log-likelihood:", round(lognormal_loglik, 3), "BIC:", round(lognormal_bic, 3), "\n")

# For my third model, I choose an exponential distribution
cat("Fitting Model 3: Exponential Distribution (my choice)\n")
# I need to shift the data to make it positive since exponential distributions require positive values
shifted_data <- size_variable - min(size_variable) + 0.01 # I shift all values to be positive
exponential_fit <- fitdistr(shifted_data, "exponential") # I fit an exponential distribution
exponential_loglik <- exponential_fit$loglik # I get the log-likelihood
exponential_bic <- BIC(exponential_fit) # I calculate BIC for exponential
cat("Exponential - Log-likelihood:", round(exponential_loglik, 3), "BIC:", round(exponential_bic, 3), "\n")

# Finally, I fit the Gaussian Mixture Model with 2 components
cat("Fitting Model 4: Gaussian Mixture Model (2 components)\n")
gmm_fit <- normalmixEM(size_variable, k = 2, maxit = 1000) # I fit a 2-component mixture model
gmm_loglik <- gmm_fit$loglik # I extract the log-likelihood from the GMM

# I need to calculate BIC manually for the GMM since the BIC function doesn't work with mixture models
N <- length(size_variable) # I count how many observations I have
gmm_bic <- -2 * gmm_loglik + 4 * log(N) # I use the BIC formula with 4 parameters (2 means, 2 sigmas)
cat("GMM - Log-likelihood:", round(gmm_loglik, 3), "BIC:", round(gmm_bic, 3), "\n")

cat("\nGMM Components:\n") # I want to see what the two components look like
cat("Component 1: mean =", round(gmm_fit$mu[1], 3), "sigma =", round(sqrt(gmm_fit$sigma[1]), 3), "lambda =", round(gmm_fit$lambda[1], 3), "\n")
cat("Component 2: mean =", round(gmm_fit$mu[2], 3), "sigma =", round(sqrt(gmm_fit$sigma[2]), 3), "lambda =", round(gmm_fit$lambda[2], 3), "\n")

# Now I create a table to compare all my models
cat("\n=== BIC COMPARISON RESULTS ===\n")
models <- c("Normal", "Log-Normal", "Exponential", "GMM") # I list all the models I fitted
bic_values <- c(normal_bic, lognormal_bic, exponential_bic, gmm_bic) # I collect all the BIC values
loglik_values <- c(normal_loglik, lognormal_loglik, exponential_loglik, gmm_loglik) # I collect all log-likelihoods

# I make a dataframe to organize the results nicely
results_df <- data.frame(
  Model = models,
  LogLikelihood = round(loglik_values, 3),
  BIC = round(bic_values, 3),
  DeltaBIC = round(bic_values - min(bic_values), 3) # I calculate how much worse each model is than the best
)

# I sort the table so the best model (lowest BIC) is at the top
results_df <- results_df[order(results_df$BIC), ]
print(results_df) # I print the comparison table

# I identify which model won
best_model <- results_df$Model[1] # The first row has the best model
cat("\nBest model (lowest BIC):", best_model, "\n")

# I interpret what the Delta BIC values mean in practical terms
cat("\nDelta BIC Interpretation:\n")
for(i in 1:nrow(results_df)) { # I go through each model in my table
  delta <- results_df$DeltaBIC[i] # I get the delta BIC value
  model <- results_df$Model[i] # I get the model name
  if(delta == 0) { # If delta is zero, this is the best model
    cat(model, ": Best model (reference)\n")
  } else if(delta <= 2) { # If delta is small, there's still good support
    cat(model, ": Substantial support (ΔIC ≤ 2)\n") 
  } else if(delta <= 6) { # If delta is moderate, there's less support
    cat(model, ": Considerably less support (2 < ΔIC ≤ 6)\n")
  } else { # If delta is large, there's basically no support
    cat(model, ": Essentially no support (ΔIC > 6)\n")
  }
}

# Now I want to create plots to show how well each model fits my data
cat("\nCreating comprehensive visualization...\n")

pdf("Assignment1_output/plots/trevally_gmm_analysis.pdf", width = 12, height = 8) # I save my plots to a PDF file

# I set up a 2x2 grid to show all four models
par(mfrow = c(2, 2))

# Plot 1: I show the histogram with the normal distribution curve
hist(size_variable, breaks = 15, freq = FALSE, main = "Normal Distribution Fit",
     xlab = "Morphological Index", ylab = "Density", col = "lightblue")
x_seq <- seq(min(size_variable), max(size_variable), length.out = 100) # I create a smooth sequence for plotting curves
normal_curve <- dnorm(x_seq, mean = normal_fit$estimate[1], sd = normal_fit$estimate[2]) # I calculate the normal curve
lines(x_seq, normal_curve, col = "red", lwd = 2) # I draw the normal curve in red
legend("topright", paste("BIC =", round(normal_bic, 2)), bty = "n") # I add the BIC value to the plot

# Plot 2: I show the histogram with the log-normal distribution curve  
hist(size_variable, breaks = 15, freq = FALSE, main = "Log-Normal Distribution Fit",
     xlab = "Morphological Index", ylab = "Density", col = "lightgreen")
lognormal_curve <- dlnorm(x_seq, meanlog = lognormal_fit$estimate[1], sdlog = lognormal_fit$estimate[2]) # I calculate the log-normal curve
lines(x_seq, lognormal_curve, col = "blue", lwd = 2) # I draw the log-normal curve in blue
legend("topright", paste("BIC =", round(lognormal_bic, 2)), bty = "n") # I add the BIC value

# Plot 3: I show the histogram with the exponential distribution curve
hist(shifted_data, breaks = 15, freq = FALSE, main = "Exponential Distribution Fit",
     xlab = "Shifted Morphological Index", ylab = "Density", col = "lightyellow")
x_seq_shifted <- seq(min(shifted_data), max(shifted_data), length.out = 100) # I create a sequence for the shifted data
exponential_curve <- dexp(x_seq_shifted, rate = exponential_fit$estimate[1]) # I calculate the exponential curve
lines(x_seq_shifted, exponential_curve, col = "orange", lwd = 2) # I draw the exponential curve in orange
legend("topright", paste("BIC =", round(exponential_bic, 2)), bty = "n") # I add the BIC value

# Plot 4: I show the histogram with the Gaussian mixture model
hist(size_variable, breaks = 15, freq = FALSE, main = "Gaussian Mixture Model Fit",
     xlab = "Morphological Index", ylab = "Density", col = "lightcoral")
# I calculate the overall GMM density as a weighted sum of the two components
gmm_curve <- gmm_fit$lambda[1] * dnorm(x_seq, mean = gmm_fit$mu[1], sd = sqrt(gmm_fit$sigma[1])) +
             gmm_fit$lambda[2] * dnorm(x_seq, mean = gmm_fit$mu[2], sd = sqrt(gmm_fit$sigma[2]))
lines(x_seq, gmm_curve, col = "purple", lwd = 2) # I draw the overall mixture curve in purple
# I also show the individual components as dashed lines so I can see what each component looks like
lines(x_seq, gmm_fit$lambda[1] * dnorm(x_seq, mean = gmm_fit$mu[1], sd = sqrt(gmm_fit$sigma[1])), 
      col = "purple", lty = 2) # First component as dashed line
lines(x_seq, gmm_fit$lambda[2] * dnorm(x_seq, mean = gmm_fit$mu[2], sd = sqrt(gmm_fit$sigma[2])), 
      col = "purple", lty = 2) # Second component as dashed line
legend("topright", paste("BIC =", round(gmm_bic, 2)), bty = "n") # I add the BIC value

dev.off() # I close the PDF file

# Save detailed results to file
sink('Assignment1_output/results/trevally_bic_comparison.txt')
cat('GAUSSIAN MIXTURE MODEL AND BIC COMPARISON RESULTS\n')
cat('Giant Trevally (Caranx ignobilis) Morphological Index Analysis\n\n')

cat('DATASET INFORMATION:\n')
cat('Variable: Morphological Index (sum of average length and width)\n')
cat('Sample size:', N, 'observations\n')
cat('Range:', round(min(size_variable), 2), 'to', round(max(size_variable), 2), '\n')
cat('Mean:', round(mean(size_variable), 2), '\n')
cat('Standard deviation:', round(sd(size_variable), 2), '\n\n')

cat('MODEL COMPARISON RESULTS:\n')
print(results_df)

cat('\nBEST MODEL:', best_model, '\n')
cat('This model has the lowest BIC value.\n\n')

cat('DETAILED MODEL PARAMETERS:\n')
cat('Normal Distribution:\n')
cat('  Mean:', round(normal_fit$estimate[1], 3), '\n')
cat('  SD:', round(normal_fit$estimate[2], 3), '\n\n')

cat('Log-Normal Distribution:\n')
cat('  Meanlog:', round(lognormal_fit$estimate[1], 3), '\n')
cat('  Sdlog:', round(lognormal_fit$estimate[2], 3), '\n\n')

cat('Exponential Distribution:\n')
cat('  Rate:', round(exponential_fit$estimate[1], 3), '\n\n')

cat('Gaussian Mixture Model:\n')
cat('  Component 1: mean =', round(gmm_fit$mu[1], 3), ', sigma =', round(sqrt(gmm_fit$sigma[1]), 3), ', weight =', round(gmm_fit$lambda[1], 3), '\n')
cat('  Component 2: mean =', round(gmm_fit$mu[2], 3), ', sigma =', round(sqrt(gmm_fit$sigma[2]), 3), ', weight =', round(gmm_fit$lambda[2], 3), '\n\n')

cat('INTERPRETATION:\n')
if(best_model == "GMM") {
  cat('The Gaussian Mixture Model provides the best fit, indicating MULTIMODALITY.\n')
  cat('This suggests latent structure in the trevally morphological data.\n')
  cat('The two components may represent different morphological types or treatment responses.\n')
} else {
  cat('A simpler distribution (', best_model, ') provides the best fit.\n')
  cat('This suggests the morphological index follows a unimodal distribution.\n')
  cat('No strong evidence for latent subpopulations in the data.\n')
}

cat('\nCONCLUSION:\n')
cat('BIC analysis')
if(best_model == "GMM") {
  cat(' supports the presence of latent structure in Giant Trevally morphological data.\n')
  cat('Complex multimodal distributions require sophisticated mixture modeling approaches.\n')
} else {
  cat(' suggests that Giant Trevally morphological index is adequately described by a', tolower(best_model), 'distribution.\n')
  cat('Simple probability models are sufficient for this dataset.\n')
}

sink()

cat("\nAnalysis complete!\n")
cat("Files generated:\n")
cat("- trevally_gmm_analysis.pdf (4-panel visualization)\n") 
cat("- trevally_bic_comparison.txt (detailed results)\n\n")

cat("FINAL SUMMARY:\n")
cat("Best fitting model:", best_model, "\n")
cat("Evidence for latent structure:", ifelse(best_model == "GMM", "YES", "NO"), "\n")
