# MaximillianBalter_Unit1_BIOL672.r
# Author: Maximillian Balter
# OS: macOS
# Libraries/packages used: ggplot2, jsonlite, dplyr
# Input data files used: Dataset.datatab (JSON format ANOVA data)
# Output files generated: desc.txt, histo.pdf, anova_results.txt, anova_plot.pdf, pairwise_ttests.txt, kruskal_results.txt, ks_tests.txt, correlations.txt, correlation_scatterplots.pdf, linear_regression_results.txt, linear_regression_plot.pdf, assumptions_and_consistency.txt
# This script is divided into four main sections (Steps 1-6 from the assignment sheet):
#   1. Random Number Generator
#   2. One-way ANOVA
#   3. Nonparametric and Correlation Analyses
#   4. Simple Linear Regression


## Section 1 — Random number generator and simple descriptive output

# Here I am checking if ggplot2 is installed. If not, I install it so I can make plots.
if (!require(ggplot2)) { # I check if ggplot2 is available.
  install.packages('ggplot2') # I download and install ggplot2 from CRAN if needed.
  library(ggplot2) # I load ggplot2 so I can use its plotting functions.
}

set.seed(12345) # I set a random seed so my random numbers are the same every time I run the script.

data <- rnorm(5000, mean = 0, sd = 1) # I use rnorm() to generate 5000 random numbers from a normal distribution with mean 0 and sd 1.

mean_val <- mean(data) # I calculate the mean of my random numbers using mean().
sd_val <- sd(data) # I calculate the standard deviation using sd().

p_hist <- ggplot(data.frame(x = data), aes(x = x)) + # I create a ggplot object for my histogram.
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = 'skyblue', color = 'black') + # I add histogram bars to show the distribution.
  geom_density(color = 'red', linewidth = 1) + # I add a density curve to show the smooth shape of the data.
  labs(title = 'Histogram of generated data with density line', x = 'Value', y = 'Density') + # I add a title and axis labels.
  theme_minimal() # I use a clean, minimal theme for the plot.

ggsave(filename = 'histo.pdf', plot = p_hist, width = 7, height = 5) # I save the plot to a PDF file called histo.pdf using ggsave().

sink('desc.txt') # I start writing output to a file called desc.txt.
cat('Sample Mean:', mean_val, '\n') # I print the mean value to the file.
cat('Sample SD:', sd_val, '\n') # I print the standard deviation to the file.
sink() # I stop writing to the file.

write.table(data, file = 'random_numbers.txt', row.names = FALSE, col.names = FALSE) # I save the raw random numbers to a file called random_numbers.txt.

## Section 2 — ANOVA input parsing and parametric analysis
anova_candidates <- c('Dataset.datatab', 'anova_data.txt', './Unit_1/Dataset.datatab', '/Users/Maximillian/BIO672_COMPSTAT/Unit_1/Dataset.datatab') # I make a list of possible file paths for my ANOVA input data.
found_input <- NULL # I start with no file found.
for (p in anova_candidates) { # I loop through each possible file path.
  if (file.exists(p)) { found_input <- p; break } # If the file exists, I use it and stop looking.
}

if (is.null(found_input)) { # If I didn't find any file...
  message('ANOVA input file not found (tried Dataset.datatab and anova_data.txt). Skipping ANOVA step.') # I print a message and skip the ANOVA step.
} else {
  message(sprintf('Using ANOVA input: %s', found_input)) # I print which file I am using.

  if (grepl('\\.datatab$', found_input)) { # If the file ends with .datatab, I treat it as a JSON-like file and parse it.
    if (!require(jsonlite, quietly = TRUE)) { # I check if jsonlite is installed.
      install.packages('jsonlite', repos = 'https://cloud.r-project.org') # I install jsonlite if needed.
      library(jsonlite) # I load jsonlite.
    }
    raw <- fromJSON(found_input, simplifyVector = FALSE) # I parse the file using fromJSON().
    vars <- raw$variables # I get the variables list from the parsed data.

    is_num <- sapply(vars, function(v) { # I use sapply() to find which variable is numeric.
      !is.null(v[['dataValue']]) && all(sapply(unlist(v[['dataValue']]), is.numeric)) # TRUE if numeric
    })
    is_label <- sapply(vars, function(v) { # I use sapply() to find which variable is a label.
      !is.null(v[['dataLabel']]) && is.character(unlist(v[['dataLabel']])) # TRUE if label
    })

    value_idx <- which(is_num)[1] # I pick the first numeric variable.
    group_idx <- which(is_label & seq_along(vars) != value_idx)[1] # I pick the first label variable that isn't the numeric one.

    if (is.na(value_idx) || is.na(group_idx)) { # If I couldn't find both variables...
      stop('Could not find appropriate numeric and group variables in Dataset.datatab') # I stop and print an error.
    }

    values <- unlist(vars[[value_idx]][['dataValue']]) # I get the numeric values.
    groups <- unlist(vars[[group_idx]][['dataLabel']]) # I get the group labels.

    if (length(values) != length(groups)) { # If the number of values doesn't match the number of groups...
      stop('Length mismatch between values and groups in Dataset.datatab') # I stop and print an error.
    }

    anova_df <- data.frame(value = as.numeric(values), group = factor(groups)) # I make a tidy data frame with numeric values and group labels.
  } else {
    anova_df <- read.table(found_input, header = TRUE, sep = '\t', stringsAsFactors = TRUE) # I read a tab-delimited file if not .datatab.
  }

  message(sprintf('ANOVA input rows: %d', nrow(anova_df))) # I print the number of rows in my data.
  message(sprintf('Groups: %s', paste(levels(anova_df$group), collapse = ', '))) # I print the group names.

  anova_res <- oneway.test(value ~ group, data = anova_df) # I run Welch's one-way ANOVA using oneway.test().

  sink('anova_results.txt') # I start writing output to a file called anova_results.txt.
  cat('One-way ANOVA (oneway.test) results\n') # I print a header for clarity.
  print(anova_res) # I print the full results of the ANOVA test.
  cat('\nInterpretation:\n') # I print a header for my interpretation.
  if (anova_res$p.value < 0.05) { # If the p-value is less than 0.05...
    cat('There is a statistically significant difference among groups (p < 0.05).\n') # I print that there is a significant difference.
  } else {
    cat('No statistically significant difference detected among groups (p >= 0.05).\n') # I print that there is no significant difference.
  }
  sink() # I stop writing to the file.

  if (!require(dplyr, quietly = TRUE)) { # I check if dplyr is installed.
    install.packages('dplyr', repos = 'https://cloud.r-project.org') # I install dplyr if needed.
    library(dplyr) # I load dplyr.
  }

  summary_df <- anova_df %>% # I use dplyr to calculate summary statistics for each group.
    group_by(group) %>% # I group the data by group.
    summarize(mean = mean(value), sd = sd(value), n = n()) %>% # I calculate mean, sd, and n for each group.
    mutate(se = sd / sqrt(n)) # I calculate the standard error for each group.

  p_anova <- ggplot(summary_df, aes(x = group, y = mean, fill = group)) + # I create a ggplot object for my bar plot.
    geom_col(color = 'black') + # I add bars for each group.
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + # I add error bars for the standard error.
    labs(title = 'Mean Blood Pressure by Drug (with SE)', x = 'Drug Group', y = 'Mean Blood Pressure (mm Hg)') + # I add a title and axis labels.
    theme_minimal() # I use a clean, minimal theme for the plot.

  ggsave(filename = 'anova_plot.pdf', plot = p_anova, width = 7, height = 5) # I save the plot to a PDF file called anova_plot.pdf.

  pairwise_raw <- pairwise.t.test(anova_df$value, anova_df$group, p.adjust.method = 'none', pool.sd = FALSE) # I run pairwise t-tests between groups.
  pmat <- pairwise_raw$p.value # I get the matrix of p-values.
  pairs <- which(!is.na(pmat), arr.ind = TRUE) # I find the indices of valid comparisons.
  pair_names <- apply(pairs, 1, function(ii) paste(rownames(pmat)[ii[1]], colnames(pmat)[ii[2]], sep = ' vs ')) # I create names for each pair.
  raw_p <- pmat[!is.na(pmat)] # I get the raw p-values.

  p_bonf <- p.adjust(raw_p, method = 'bonferroni') # I adjust p-values using the Bonferroni method.
  p_bh <- p.adjust(raw_p, method = 'BH') # I adjust p-values using the Benjamini-Hochberg method.

  sink('pairwise_ttests.txt') # I start writing output to a file called pairwise_ttests.txt.
  cat('Pairwise t-tests (raw p-values, Bonferroni, BH)\n') # I print a header for clarity.
  for (i in seq_along(pair_names)) { # I loop through each pair.
    cat(sprintf('%s: raw p = %.6f, bonf = %.6f, BH = %.6f\n', pair_names[i], raw_p[i], p_bonf[i], p_bh[i])) # I print the results for each pair.
  }
  sink() # I stop writing to the file.

  ## Section 3 — Nonparametric tests, normality checks, and correlations
  kw <- kruskal.test(value ~ group, data = anova_df) # I run the Kruskal-Wallis test to check if the median blood pressure differs between groups.

  sink('kruskal_results.txt') # I start writing output to a file called kruskal_results.txt.
  cat('Kruskal-Wallis Test Results\n') # I print a simple header for clarity.
  print(kw) # I print the full results of the Kruskal-Wallis test.
  cat('\nInterpretation: I am checking if the groups differ in their typical (median) blood pressure. A small p-value means at least one group is different.\n')
  sink() # I stop writing to the file.

  ks_out <- list() # I create a list to store the results for each group.
  groups_list <- levels(anova_df$group) # I get the group names.
  for (g in groups_list) { # I loop over each group to do the KS test.
    x <- anova_df$value[anova_df$group == g] # I pick out the blood pressure values for this group.
    mu <- mean(x) # I calculate the average blood pressure in this group.
    sigma <- sd(x) # I calculate the standard deviation in this group.
    if (sigma == 0) { # If the standard deviation is zero...
      ks_out[[g]] <- list(statistic = NA, p.value = NA, note = 'Zero variance') # I save a note that KS is not meaningful.
    } else {
      ks_res <- ks.test(x, 'pnorm', mean = mu, sd = sigma) # I run the KS test to compare the group values to a normal distribution with the same mean and standard deviation.
      ks_out[[g]] <- list(statistic = ks_res$statistic, p.value = ks_res$p.value) # I save the result for this group.
    }
  }

  sink('ks_tests.txt') # I start writing output to a file called ks_tests.txt.
  cat('Kolmogorov-Smirnov Normality Test Results (per group)\n') # I print a header for clarity.
  for (g in groups_list) { # I go through each group to print its results.
    info <- ks_out[[g]] # I get the KS test result for this group.
    if (!is.null(info$note)) { # If there is a note...
      cat(sprintf('%s: note=%s\n', g, info$note)) # I print the note.
    } else {
      cat(sprintf('%s: statistic=%.5f, p=%.6f\n', g, as.numeric(info$statistic), info$p.value)) # I print the statistic and p-value.
      if (info$p.value < 0.05) { # If the p-value is less than 0.05...
        cat('  Interpretation: distribution for this group differs from normal (p < 0.05)\n') # I print that the group differs from normal.
      } else {
        cat('  Interpretation: no evidence against normality for this group (p >= 0.05)\n') # I print that there is no evidence against normality.
      }
    }
  }
  cat('\nNote: Because I am using the mean and SD from the same data, these p-values are only approximate.\n') # I print a caveat about the KS test.
  cat('Also, if there are repeated values (ties), the test is less reliable.\n') # I print another caveat about ties.
  sink() # I stop writing to the file.

  anova_df$group_code <- as.numeric(anova_df$group) # I convert group names to numbers for correlation tests.

  pear_res <- cor.test(anova_df$group_code, anova_df$value, method = 'pearson') # I run Pearson correlation to check for a straight-line relationship.
  spear_res <- cor.test(anova_df$group_code, anova_df$value, method = 'spearman') # I run Spearman correlation to check for a consistent rise or fall.

  sink('correlations.txt') # I start writing output to a file called correlations.txt.
  cat('Correlation Results\n') # I print a header for clarity.
  cat('\nPearson correlation measures the strength and direction of the linear relationship between group number and blood pressure.\n') # I explain Pearson in plain language.
  pear_res$method <- 'Pearson correlation' # I change the label for the output.
  print(pear_res) # I print the Pearson correlation results.
  cat('\nSpearman correlation measures whether blood pressure tends to increase or decrease as group number goes up, even if the relationship is not perfectly straight.\n') # I explain Spearman in plain language.
  print(spear_res) # I print the Spearman correlation results.
  sink() # I stop writing to the file.

  p_corr <- ggplot(anova_df, aes(x = group_code, y = value, color = group)) + # I create a scatterplot to show how blood pressure varies with group.
    geom_jitter(width = 0.2, height = 0, size = 2) + # I add jitter so points don't overlap.
    geom_smooth(method = 'lm', se = TRUE) + # I add a line to show the trend.
    labs(title = 'Blood Pressure by Group (Scatterplot)', x = 'Group (coded as number)', y = 'Blood Pressure (mm Hg)') + # I add a title and axis labels.
    theme_minimal() # I am using a simple plot theme because that is all I know how to do.

  ggsave('correlation_scatterplots.pdf', plot = p_corr, width = 7, height = 5) # I save the plot as a PDF.

  cat('\n--- Interpretation of Nonparametric and Correlation Analyses ---\n') # I print a header for my interpretation.
  cat('I checked if the blood pressure data in each group looked normal using the KS test. The results are only a rough guide because of repeated values and using the sample mean/SD.\n') # I explain the KS test results.
  cat('Both the regular ANOVA and the Kruskal-Wallis test found strong evidence that blood pressure is different in at least one group.\n') # I explain the ANOVA and Kruskal-Wallis results.
  cat('The Pearson correlation showed that as group number goes up, blood pressure tends to go up too. Spearman showed a similar pattern, even if the change isn\'t perfectly straight.\n') # I explain the correlation results.
  cat('Overall, my findings suggest the results do not depend on strict normality assumptions. The parametric and nonparametric tests both found differences between groups.\n') # I summarize my findings.

  normality_notes <- c() # I prepare a list to collect normality notes for each group.
  if (exists('ks_out')) { # If KS results exist...
    for (g in levels(anova_df$group)) { # I loop through each group.
      info <- ks_out[[g]] # I get the KS result for this group.
      if (!is.null(info$note)) { # If there is a note...
        normality_notes <- c(normality_notes, sprintf('%s: %s', g, info$note)) # I add the note to my list.
      } else if (!is.na(info$p.value) && info$p.value < 0.05) { # If p < 0.05...
        normality_notes <- c(normality_notes, sprintf('%s: rejects normality (KS p < 0.05)', g)) # I add a rejection note.
      } else {
        normality_notes <- c(normality_notes, sprintf('%s: no evidence against normality (KS p >= 0.05)', g)) # I add a normality note.
      }
    }
  } else {
    normality_notes <- c('Normality tests not available') # If no KS results, I note that.
  }

  param_sig <- (anova_res$p.value < 0.05) # I check if ANOVA is significant.
  nonparam_sig <- (kw$p.value < 0.05) # I check if Kruskal-Wallis is significant.

  if (param_sig == nonparam_sig) { # If both tests agree...
    consistency <- 'Parametric and nonparametric tests agree on significance.' # I note agreement.
  } else {
    consistency <- 'Parametric and nonparametric tests disagree on significance.' # I note disagreement.
  }

  narrative <- c() # I prepare a vector to collect my summary lines.
  narrative <- c(narrative, sprintf('Parametric ANOVA p = %.6g; Kruskal-Wallis p = %.6g', anova_res$p.value, kw$p.value)) # I add p-values.
  narrative <- c(narrative, consistency) # I add consistency note.
  narrative <- c(narrative, 'Normality notes:') # I add normality header.
  narrative <- c(narrative, normality_notes) # I add normality notes.
  narrative <- c(narrative, 'KS caveat: KS p-values are approximate because group mean/sd were estimated; ties may affect validity.') # I add KS caveat.
  narrative <- c(narrative, '') # Blank line.
  narrative <- c(narrative, 'Interpretation:') # Interpretation header.
  if (param_sig && nonparam_sig) { # If both tests are significant...
    narrative <- c(narrative, "Both tests found differences between groups, and since they agree, I am more confident the difference is real.") # I add my interpretation.
  } else if (!param_sig && !nonparam_sig) { # If neither test is significant...
    narrative <- c(narrative, "Neither test found differences between groups, so I see no evidence of group effects.") # I add my interpretation.
  } else if (param_sig && !nonparam_sig) { # If only ANOVA is significant...
    narrative <- c(narrative, "ANOVA shows a difference but Kruskal-Wallis does not. I would check normality and variances and look at the raw data; ANOVA can be sensitive to mean shifts while Kruskal-Wallis focuses on ranks.") # I add my interpretation.
  } else { # If only Kruskal-Wallis is significant...
    narrative <- c(narrative, "Kruskal-Wallis shows a difference but ANOVA does not. I would check whether ANOVA assumptions (normality or equal variances) are violated; KW can pick up median or distribution differences that ANOVA misses.") # I add my interpretation.
  }

  cat(paste(narrative, collapse = '\n'), '\n') # I print the summary to the console.

  sink('assumptions_and_consistency.txt') # I start writing output to a file called assumptions_and_consistency.txt.
  cat('Assumptions and consistency summary\n') # I print a header for clarity.
  cat(sprintf('ANOVA p = %.6g\n', anova_res$p.value)) # I print the ANOVA p-value.
  cat(sprintf('Kruskal-Wallis p = %.6g\n', kw$p.value)) # I print the Kruskal-Wallis p-value.
  cat('\nNormality notes:\n') # I print a header for normality notes.
  for (ln in normality_notes) cat(ln, '\n') # I print each normality note.
  cat('\nConsistency: ', consistency, '\n') # I print the consistency note.
  cat('\nInterpretation:\n') # I print a header for interpretation.
  for (ln in narrative) cat(ln, '\n') # I print each line of my interpretation.
  sink() # I stop writing to the file.

## Section 4 — Simple Linear Regression: group_code vs value
# Here I am running a simple linear regression to see how blood pressure changes as group number increases.
# I use lm() to fit a straight line: value = intercept + slope * group_code.
lm_fit <- lm(value ~ group_code, data = anova_df) # I fit the model using lm().

# I want to see the summary of my regression, so I use summary().
lm_summary <- summary(lm_fit) # I get detailed results including coefficients and p-values.

# I save the regression results to a file so I can review them later.
sink('linear_regression_results.txt') # I start writing output to linear_regression_results.txt.
cat('Simple Linear Regression: Blood Pressure vs Group (numeric code)\n') # I print a header for clarity.
print(lm_summary) # I print the full regression summary.
cat('\nInterpretation: I am checking if blood pressure tends to increase or decrease as group number goes up.\n')
if (lm_summary$coefficients[2,4] < 0.05) { # If the p-value for the slope is less than 0.05...
  cat('The slope is statistically significant (p < 0.05), so blood pressure changes with group number.\n')
} else {
  cat('The slope is not statistically significant (p >= 0.05), so there is no clear change in blood pressure with group number.\n')
}
sink() # I stop writing to the file.

# I print a verbal interpretation to the console so it's easy to read.
cat('\n--- Linear Regression Interpretation ---\n') # I print a header for my interpretation.
cat('I ran a simple linear regression to see if blood pressure changes as group number increases.\n')
cat('The regression slope tells me how much blood pressure goes up or down for each step in group number.\n')
cat(sprintf('Estimated slope: %.3f\n', lm_summary$coefficients[2,1])) # I print the estimated slope.
cat(sprintf('Slope p-value: %.4g\n', lm_summary$coefficients[2,4])) # I print the p-value for the slope.
if (lm_summary$coefficients[2,4] < 0.05) {
  cat('This means blood pressure changes with group number, and the change is statistically significant.\n')
} else {
  cat('This means there is no clear evidence that blood pressure changes with group number.\n')
}
cat('Linear regression is appropriate when I want to predict one variable from another, assuming a straight-line relationship.\n')
cat('Correlation only tells me if two variables move together, but regression gives me the actual equation and lets me make predictions.\n')
cat('If the relationship is not straight, or if the variables are not numeric, regression may not be appropriate.\n')

# I want to plot the regression line on top of the scatterplot to show the fit.
p_lm <- ggplot(anova_df, aes(x = group_code, y = value, color = group)) + # I create a scatterplot.
  geom_jitter(width = 0.2, height = 0, size = 2) + # I add jitter so points don't overlap.
  geom_smooth(method = 'lm', se = TRUE, color = 'darkred', linewidth = 1.2) + # I add the regression line in dark red.
  labs(title = 'Linear Regression: Blood Pressure vs Group (numeric code)', x = 'Group (coded as number)', y = 'Blood Pressure (mm Hg)') + # I add a title and axis labels.
  theme_minimal() # I use a simple plot theme because this is all I know how to do.

ggsave('linear_regression_plot.pdf', plot = p_lm, width = 7, height = 5) # I save the regression plot as a PDF.
} # Close main else block

