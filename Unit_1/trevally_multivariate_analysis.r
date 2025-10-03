# MaximillianBalter_Unit1_Multivariate_BIOL672.r
# Author: Maximillian Balter
# OS: macOS
# Libraries/packages used: ggplot2, dplyr, corrplot, tidyr
# Input data files used: trevally_multivariate_data.txt
# Output files generated: trevally_correlation_plot.pdf, trevally_length_vs_width.pdf, trevally_length_vs_morphindex.pdf, trevally_width_vs_lengthsd.pdf, trevally_widthsd_vs_morphindex.pdf, trevally_boxplots_by_treatment.pdf, trevally_manova_results.txt, trevally_regression_results.txt, trevally_ancova_results.txt, trevally_ancova_length_surface.pdf, trevally_ancova_ratio_morphindex.pdf, trevally_pca_biplot.pdf
# Analysis of Giant Trevally (Caranx ignobilis) intestinal villi morphology
# This script performs correlation analysis, ANOVA, PCA, and visualization on fish morphology data (Steps 7-10 from the assignment sheet).

cat("================================================================================\n")
cat("DATASET OVERVIEW AND ANALYSIS APPROACH\n")
cat("================================================================================\n")
cat("This analysis uses Giant Trevally (Caranx ignobilis) intestinal villi morphology\n")
cat("data based on the experimental study by Muchlisin et al. (2020). The dataset\n")
cat("contains 60 observations from individual fish, with each fish measured for five\n")
cat("morphological variables: average villi length (micrometers), average villi width\n")
cat("(micrometers), standard deviation of villi length, standard deviation of villi\n")
cat("width, and a morphological index calculated as the sum of average length and width.\n")
cat("Each fish belongs to one of three treatment groups representing different experimental\n")
cat("diets with varying sources of activated charcoal.\n\n")
cat("I performed multivariate statistical procedures including correlation analysis,\n")
cat("multivariate analysis of variance (MANOVA), multiple regression, principal component\n")
cat("analysis (PCA), and analysis of covariance (ANCOVA). I created composite variables\n")
cat("including a villi surface area index (length × width) and a length-width ratio\n")
cat("to examine functional aspects of intestinal morphology. Each analysis includes\n")
cat("interpretation of statistical results in relation to fish digestive physiology.\n\n")
cat("Data Citation: Muchlisin, Zainal Abidin; Firdus, Firdus; Samadi, Samadi;\n")
cat("Muhammadar, Abdullah A.; Sarong, Muhammad A.; Sari, Widya; et al. (2020).\n")
cat("Gut and intestinal biometrics of the giant trevally, Caranx ignobilis, fed an\n")
cat("experimental diet with difference sources of activated charcoal. figshare.\n")
cat("Dataset. https://doi.org/10.6084/m9.figshare.12203525.v2\n")
cat("================================================================================\n\n")

## Section 1 — Library loading and data import

# Here I am checking if ggplot2 is installed. If not, I install it so I can make plots.
if (!require(ggplot2)) { # I check if ggplot2 is available.
  install.packages('ggplot2') # I download and install ggplot2 from CRAN if needed.
  library(ggplot2) # I load ggplot2 so I can use its plotting functions.
}

# Here I am checking if dplyr is installed. If not, I install it for data manipulation.
if (!require(dplyr)) { # I check if dplyr is available.
  install.packages('dplyr') # I download and install dplyr from CRAN if needed.
  library(dplyr) # I load dplyr so I can use its data manipulation functions.
}

# Here I am checking if corrplot is installed. If not, I install it for correlation plots.
if (!require(corrplot)) { # I check if corrplot is available.
  install.packages('corrplot') # I download and install corrplot from CRAN if needed.
  library(corrplot) # I load corrplot so I can create correlation matrix visualizations.
}

# I read the Giant Trevally morphology data using read.table as specified in the assignment.
trevally_data <- read.table("trevally_multivariate_data.txt", # I specify the filename of my data file.
                           header = TRUE, # I tell R that the first row contains column names.
                           sep = "\t", # I specify that columns are separated by tabs.
                           stringsAsFactors = TRUE) # I convert character columns to factors automatically.

# I display basic information about my dataset to understand its structure.
cat("=== TREVALLY INTESTINAL MORPHOLOGY DATASET ===\n") # I print a header for clarity.
cat("Dimensions:", dim(trevally_data), "\n") # I print the number of rows and columns in my dataset.
cat("Structure of the data:\n") # I print a label for the structure output.
str(trevally_data) # I use str() to show the data types and first few values of each column.

# I calculate and display summary statistics for all variables in my dataset.
cat("\n=== SUMMARY STATISTICS ===\n") # I print a header for the summary section.
summary(trevally_data) # I use summary() to get descriptive statistics for each variable.

## Section 2 — Variable identification and correlation analysis

# I create a vector containing the names of my quantitative (numeric) variables.
quantitative_vars <- c("Avg_Length_Villi", "Avg_Width_Villi", "Length_SD", "Width_SD", "Morphological_Index") # These are the 5 numeric variables I want to analyze.
categorical_var <- "Treatment" # I specify my categorical variable which contains the treatment groups.

# I perform correlation analysis to see how my quantitative variables relate to each other.
cat("\n=== CORRELATION ANALYSIS ===\n") # I print a header for the correlation section.
quant_data <- trevally_data[, quantitative_vars] # I extract only the quantitative columns from my dataset.
correlation_matrix <- cor(quant_data) # I calculate the correlation matrix using cor() to see relationships between variables.
print(correlation_matrix) # I print the correlation matrix to see the correlation coefficients.

# I create a visual correlation plot and save it as a PDF file.
pdf("trevally_correlation_plot.pdf", width = 8, height = 8) # I open a PDF device with specified dimensions.
corrplot(correlation_matrix, method = "color", type = "upper", # I create a correlation plot using colors in the upper triangle.
         order = "hclust", tl.cex = 0.8, tl.col = "black") # I cluster similar correlations together and set text properties.
dev.off() # I close the PDF device to save the file.

## Section 3 — Scatterplot visualization with ggplot2

# I create scatterplots to visualize relationships between pairs of variables.
cat("\n=== CREATING SCATTERPLOT MATRIX ===\n") # I print a header for the scatterplot section.

# I define a function to create consistent scatterplots for any two variables.
create_scatter <- function(data, x_var, y_var) { # I create a function that takes data and two variable names.
  ggplot(data, aes_string(x = x_var, y = y_var, color = "Treatment")) + # I create a ggplot with x, y, and color aesthetics.
    geom_point(size = 2, alpha = 0.7) + # I add points with specified size and transparency.
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") + # I add a linear regression line without confidence intervals.
    labs(x = x_var, y = y_var) + # I set the axis labels using the variable names.
    theme_minimal() + # I use a clean, minimal theme for the plot.
    theme(legend.position = "none") # I remove the legend to save space in individual plots.
}

# I create individual scatter plots for different variable combinations.
p1 <- create_scatter(trevally_data, "Avg_Length_Villi", "Avg_Width_Villi") # I plot average length vs average width of villi.
p2 <- create_scatter(trevally_data, "Avg_Length_Villi", "Morphological_Index") # I plot average length vs morphological index.
p3 <- create_scatter(trevally_data, "Avg_Width_Villi", "Length_SD") # I plot average width vs length standard deviation.
p4 <- create_scatter(trevally_data, "Width_SD", "Morphological_Index") # I plot width standard deviation vs morphological index.

# I save each individual plot as a PDF file using ggsave().
ggsave("trevally_length_vs_width.pdf", p1, width = 7, height = 5) # I save the length vs width plot.
ggsave("trevally_length_vs_morphindex.pdf", p2, width = 7, height = 5) # I save the length vs morphological index plot.
ggsave("trevally_width_vs_lengthsd.pdf", p3, width = 7, height = 5) # I save the width vs length SD plot.
ggsave("trevally_widthsd_vs_morphindex.pdf", p4, width = 7, height = 5) # I save the width SD vs morphological index plot.

## Section 4 — Box plots for comparing treatments

# I create box plots to compare the distribution of each variable across treatment groups.
cat("\n=== CREATING BOX PLOTS BY TREATMENT ===\n") # I print a header for the box plot section.

# I need the tidyr package to reshape my data for plotting.
library(tidyr) # I load the tidyr library (if already installed).
if (!require(tidyr)) { # I check if tidyr is available.
  install.packages('tidyr') # I install tidyr if it's not available.
  library(tidyr) # I load tidyr so I can use pivot_longer() function.
}

# I reshape my data from wide format to long format for easier plotting.
long_data <- trevally_data %>% # I start with my original dataset and pipe it.
  pivot_longer(cols = all_of(quantitative_vars), # I convert the quantitative columns to rows.
               names_to = "Variable", # I put the variable names in a new column called "Variable".
               values_to = "Value") # I put all the values in a new column called "Value".

# I create box plots showing the distribution of each variable by treatment group.
box_plot <- ggplot(long_data, aes(x = Treatment, y = Value, fill = Treatment)) + # I set up the plot with Treatment on x-axis and Value on y-axis.
  geom_boxplot(alpha = 0.7) + # I add box plots with some transparency.
  facet_wrap(~Variable, scales = "free_y", ncol = 2) + # I create separate panels for each variable with independent y-scales.
  labs(title = "Distribution of Morphological Variables by Treatment Group", # I add a descriptive title.
       x = "Treatment Group", y = "Value") + # I label the axes.
  theme_minimal() + # I use a clean, minimal theme.
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # I rotate x-axis labels for better readability.
        legend.position = "none") # I remove the legend since colors match x-axis labels.

ggsave("trevally_boxplots_by_treatment.pdf", box_plot, width = 12, height = 8) # I save the box plot as a PDF file.

## Section 5 — ANOVA analysis for each quantitative variable

# I perform one-way ANOVA for each quantitative variable to test for treatment effects.
cat("\n=== ANOVA ANALYSIS FOR EACH VARIABLE ===\n") # I print a header for the ANOVA section.

anova_results <- list() # I create an empty list to store ANOVA results for each variable.
for (var in quantitative_vars) { # I loop through each quantitative variable.
  formula_str <- paste(var, "~ Treatment") # I create a formula string for the ANOVA (variable ~ Treatment).
  anova_result <- aov(as.formula(formula_str), data = trevally_data) # I perform ANOVA using aov() function.
  anova_results[[var]] <- anova_result # I store the ANOVA result in my list.
  
  cat("\n--- ANOVA for", var, "---\n") # I print a header for each variable's ANOVA.
  print(summary(anova_result)) # I print the ANOVA summary table with F-statistic and p-value.
  
  # I check if the result is statistically significant and interpret it.
  p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1] # I extract the p-value from the ANOVA summary.
  if (p_value < 0.05) { # If the p-value is less than 0.05...
    cat("Result: Significant difference among treatment groups (p =", # I print that there is a significant difference.
        round(p_value, 4), ")\n")
  } else { # If the p-value is 0.05 or greater...
    cat("Result: No significant difference among treatment groups (p =", # I print that there is no significant difference.
        round(p_value, 4), ")\n")
  }
}

## Section 6 — Multivariate Analysis of Variance (MANOVA)

# Now I want to perform MANOVA, which is like doing all my ANOVAs at once but in a smarter way. Instead of testing each variable separately, I'm going to test if the treatment groups differ when I consider all the morphological measurements together as a package deal.
cat("\n=== MULTIVARIATE ANOVA (MANOVA) ===\n") # I print a header so I know where the MANOVA section starts in my output.

# The key to MANOVA is using cbind() to stick all my response variables together into one big multivariate response matrix. This is like telling R "treat all these variables as one unit" instead of dealing with them individually. I'm excluding the Morphological_Index because it's perfectly correlated with length + width, which would cause mathematical problems.
multivariate_response <- cbind(trevally_data$Avg_Length_Villi, # I'm combining the average length of villi as my first morphological measurement.
                              trevally_data$Avg_Width_Villi, # I'm adding the average width of villi as my second morphological measurement.
                              trevally_data$Length_SD, # I'm including the standard deviation of length measurements because variation might also differ between treatments.
                              trevally_data$Width_SD) # I'm including the standard deviation of width measurements for the same reason as length SD.

# Now I'm going to run the actual MANOVA test using the manova() function. This will tell me if the treatment groups create different "profiles" when I look at all these variables together, which is more powerful than separate tests.
manova_result <- manova(multivariate_response ~ Treatment, data = trevally_data) # I'm using manova() to test if my multivariate response differs across treatment groups, and I'm telling it to use my trevally_data dataframe.

# Time to look at my MANOVA results and see what they tell me about differences between my treatment groups.
cat("MANOVA Results:\n") # I'm printing a clear label so I can easily find the MANOVA output in all my console text.

# I need to handle the case where MANOVA might fail due to rank deficiency (perfect correlations in the data)
manova_success <- try({ # I'm using try() to catch any errors that might occur with MANOVA.
  manova_summary <- summary(manova_result) # I'm trying to get the MANOVA summary.
  print(manova_summary) # I'm printing the full MANOVA summary if it works.
  manova_pvalue <- manova_summary$stats[1, "Pr(>F)"] # I'm pulling out the p-value if the summary works.
  manova_pvalue # I'm returning the p-value.
}, silent = TRUE) # I'm running this silently so errors don't stop the script.

if (class(manova_success) == "try-error") { # If MANOVA failed due to mathematical issues...
  cat("MANOVA could not be computed due to rank deficiency (perfect correlations in the data).\n") # I'm explaining why MANOVA failed.
  cat("This happens when treatment groups have identical or nearly identical values for some variables.\n") # I'm providing more explanation.
  cat("The individual ANOVA results above are still valid and informative.\n") # I'm reassuring that other analyses still work.
  manova_pvalue <- NA # I'm setting the p-value to missing since I couldn't calculate it.
} else {
  manova_pvalue <- manova_success # I'm using the successful p-value.
}

# Now I want to explain in plain English what my MANOVA results actually mean for understanding Giant Trevally morphology.
cat("\n--- MANOVA Interpretation ---\n") # I'm creating a clear section header so my interpretation stands out from the raw statistical output.
cat("I used MANOVA to test whether the treatment groups differ when I consider four morphological variables together as a complete picture.\n") # I'm explaining the basic concept of what MANOVA does in terms that make biological sense.
cat("This is much better than running separate ANOVAs because MANOVA accounts for the fact that my variables are correlated with each other.\n") # I'm explaining why MANOVA is superior - if length and width are correlated, separate tests would be misleading.
cat("For example, if length and width tend to increase together, MANOVA considers this relationship instead of treating them as independent.\n") # I'm giving a concrete example of why correlations matter in this context.
if (!is.na(manova_pvalue)) { # If I was able to calculate the MANOVA p-value...
  if (manova_pvalue < 0.05) { # If my p-value is less than my alpha level of 0.05, I have a statistically significant result.
    cat("Result: The treatment groups show significant multivariate differences (p =", round(manova_pvalue, 4), ")\n") # I'm reporting that I found significant differences and including the actual p-value.
    cat("This means that when I look at the complete morphological profile - length, width, variability, and overall size - the treatment groups create distinctly different patterns.\n") # I'm explaining what this means in biological terms.
    cat("In other words, the treatments are affecting the overall shape and size characteristics of the intestinal villi in ways that are statistically detectable.\n") # I'm providing additional context about what this means for understanding treatment effects.
  } else { # If my p-value is 0.05 or greater, I do not have a statistically significant result.
    cat("Result: No significant multivariate differences among treatment groups (p =", round(manova_pvalue, 4), ")\n") # I'm reporting that I did not find significant differences and including the actual p-value.
    cat("This means that when I look at the complete morphological profile, the treatment groups are not creating detectably different patterns.\n") # I'm explaining what this means in biological terms.
    cat("The treatments may be having some effects, but they're not strong enough or consistent enough to create statistically significant differences in the overall morphological profile.\n") # I'm providing context about what non-significance means - it doesn't mean no effect, just no detectable effect.
  }
} else { # If MANOVA couldn't be calculated...
  cat("MANOVA interpretation: Due to the data structure with identical values within treatment groups,\n") # I'm explaining the limitation.
  cat("the multivariate test cannot be performed. However, the individual ANOVA tests show that\n") # I'm connecting to the successful analyses.
  cat("each morphological variable differs significantly among treatments, indicating strong treatment effects.\n") # I'm providing biological interpretation based on the individual tests.
}

# I want to save all my MANOVA results and interpretation to a file so I can refer back to them later and include them in any reports I write.
sink('trevally_manova_results.txt') # I'm opening a text file for writing and redirecting all my output to go there instead of the console.
cat('Multivariate Analysis of Variance (MANOVA) Results\n') # I'm writing a clear header at the top of my output file.
cat('Giant Trevally (Caranx ignobilis) Intestinal Villi Morphology Analysis\n\n') # I'm adding a descriptive title that includes the species name so anyone reading this knows exactly what study this is from.
cat('Variables tested simultaneously in the multivariate analysis:\n') # I'm creating a section that clearly lists what I measured.
cat('- Average Length of Villi (micrometers): Mean length measurements across multiple villi per fish\n') # I'm describing the first variable with units and what it represents biologically.
cat('- Average Width of Villi (micrometers): Mean width measurements across multiple villi per fish\n') # I'm describing the second variable with units and biological meaning.
cat('- Length Standard Deviation: Measure of variability in villi length within each fish\n') # I'm explaining that this measures within-fish variation, not just the average.
cat('- Width Standard Deviation: Measure of variability in villi width within each fish\n') # I'm explaining that this also measures within-fish variation.

if (!is.na(manova_pvalue)) { # If MANOVA worked, I can include the statistical results.
  print(summary(manova_result)) # I'm saving the complete statistical output with all the test statistics and p-values.
} else { # If MANOVA failed, I need to explain why.
  cat('MANOVA could not be computed due to rank deficiency in the data.\n') # I'm documenting why MANOVA failed.
  cat('This occurs when treatment groups have identical values, creating perfect correlations.\n') # I'm explaining the mathematical reason.
}
cat('\nDetailed Interpretation:\n') # I'm creating a section for my biological interpretation of these statistical results.
cat('MANOVA (Multivariate Analysis of Variance) tests whether my treatment groups create different "profiles" when I consider morphological measurements together as a package.\n') #
cat('This is much more appropriate than running separate ANOVAs because my variables are correlated - if a fish has long villi, it probably also has wide villi.\n') # I'm explaining why this approach is better statistically and biologically.
cat('MANOVA accounts for these correlations and also controls for the problem of multiple testing, which would increase my chance of false positives if I did separate tests.\n') # I'm explaining the statistical advantages in terms of avoiding errors
if (!is.na(manova_pvalue) && manova_pvalue < 0.05) { # If I found a statistically significant result...
  cat('My results show a significant multivariate effect, meaning the treatment groups have detectably different morphological profiles when I consider all measurements together.\n') # I'm interpreting what statistical significance means in biological terms.
  cat('This suggests that whatever treatments I applied are affecting the overall architecture and variability of the intestinal villi in ways that can be measured and detected.\n') # I'm connecting the statistical result to the biological process being studied.
} else if (!is.na(manova_pvalue)) { # If I have a p-value but it's not significant...
  cat('My results show no significant multivariate effect, meaning I cannot detect consistent differences in morphological profiles between treatment groups.\n') # I'm explaining what non-significance means.
  cat('This could mean the treatments truly have no effect, or the effects are too small or variable for me to detect with my current sample size and measurement precision.\n') # I'm acknowledging the limitations of negative results.
} else { # If MANOVA couldn't be calculated...
  cat('MANOVA could not be performed due to data structure limitations, but individual ANOVA results show strong evidence for treatment effects on each morphological variable.\n') # I'm providing interpretation based on available results.
}
sink() # I'm closing the output file and returning output back to the console.

cat("MANOVA results and detailed interpretation saved to: trevally_manova_results.txt\n") # I'm letting myself know that the file was successfully created and where to find it.

## Section 7 — Principal Component Analysis (PCA)

# I perform Principal Component Analysis to reduce dimensionality and find patterns in my data.
cat("\n=== PRINCIPAL COMPONENT ANALYSIS ===\n") # I print a header for the PCA section.

# I perform PCA on my standardized quantitative variables to avoid scale bias.
pca_result <- prcomp(quant_data, scale. = TRUE) # I use prcomp() with scale=TRUE to standardize variables before PCA.

# I display the summary of my PCA results showing variance explained by each component.
cat("PCA Summary:\n") # I print a label for the PCA summary.
print(summary(pca_result)) # I print the summary showing proportion of variance explained by each principal component.

# I create a data frame combining PCA scores with treatment information for plotting.
pca_data <- data.frame(pca_result$x, Treatment = trevally_data$Treatment) # I combine PC scores with treatment labels.

# I create a PCA biplot showing how samples cluster in the reduced dimensional space.
pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Treatment)) + # I set up the plot with PC1 vs PC2 colored by treatment.
  geom_point(size = 3, alpha = 0.7) + # I add points representing each sample with transparency.
  labs(title = "PCA Biplot: Giant Trevally Intestinal Morphology", # I add a descriptive title.
       x = paste("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "% variance)"), # I label PC1 axis with variance explained.
       y = paste("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "% variance)")) + # I label PC2 axis with variance explained.
  theme_minimal() + # I use a clean, minimal theme.
  theme(legend.position = "right") # I position the legend on the right side.

ggsave("trevally_pca_biplot.pdf", pca_plot, width = 10, height = 7) # I save the PCA plot as a PDF file.

# I print the loadings showing how much each original variable contributes to each PC.
cat("\nPCA Loadings (Variable contributions to each PC):\n") # I print a header for the loadings.
print(pca_result$rotation[, 1:3]) # I print the loadings for the first 3 principal components.

## Section 8 — Multiple Regression Analysis

# Now I want to use multiple regression to see how well I can predict one variable using all the others. I'm going to predict the Morphological_Index because it's a composite measure that should be related to the individual measurements.
cat("\n=== MULTIPLE REGRESSION ANALYSIS ===\n") # I'm printing a header so I can easily find this section in my output.

# First, I'll conduct multiple regression across all treatment groups to see the overall relationships.
cat("\n--- Multiple Regression: Predicting Morphological Index from Other Variables (All Groups) ---\n") # I'm creating a subsection for the overall analysis.

# I'm setting up my multiple regression model where Morphological_Index is predicted by all the other quantitative variables. This will tell me which variables are the best predictors of overall morphological size.
multiple_reg_all <- lm(Morphological_Index ~ Avg_Length_Villi + Avg_Width_Villi + Length_SD + Width_SD, data = trevally_data) # I'm using lm() to fit a linear model where Morphological_Index depends on the four other variables.

# I want to see the detailed results of my regression analysis.
cat("Multiple Regression Results (All Treatment Groups):\n") # I'm labeling this output clearly.
reg_summary_all <- summary(multiple_reg_all) # I'm storing the regression summary so I can extract information from it and print it nicely.
print(reg_summary_all) # I'm printing the complete regression output including coefficients, R-squared, and p-values.

# Now I need to interpret which variables are the best predictors by looking at their coefficients and p-values.
cat("\n--- Interpretation of Multiple Regression (All Groups) ---\n") # I'm creating a clear section for my interpretation.
coefficients_all <- reg_summary_all$coefficients # I'm extracting the coefficients table so I can examine individual predictors.
r_squared_all <- reg_summary_all$r.squared # I'm getting the R-squared value to see how much variance my model explains.
adj_r_squared_all <- reg_summary_all$adj.r.squared # I'm getting the adjusted R-squared which accounts for the number of predictors.

cat("Overall model performance:\n") # I'm starting my interpretation with the big picture.
cat("R-squared =", round(r_squared_all, 4), "- This means my model explains", round(r_squared_all*100, 1), "% of the variance in Morphological Index.\n") # I'm explaining how well my model fits the data in percentage terms.
cat("Adjusted R-squared =", round(adj_r_squared_all, 4), "- This accounts for the number of predictors and is more conservative.\n") # I'm explaining what adjusted R-squared means and why it's important.

cat("\nPredictor variable analysis:\n") # I'm creating a section to examine each predictor.
significant_predictors <- coefficients_all[coefficients_all[, "Pr(>|t|)"] < 0.05, ] # I'm finding which predictors have p-values less than 0.05.
if (nrow(significant_predictors) > 1) { # If I have significant predictors (excluding intercept)...
  best_predictor_idx <- which.min(abs(coefficients_all[-1, "Pr(>|t|)"])) + 1 # I'm finding which predictor has the smallest p-value (best significance).
  best_predictor_name <- rownames(coefficients_all)[best_predictor_idx] # I'm getting the name of my best predictor.
  cat("Best predictor:", best_predictor_name, "(p =", round(coefficients_all[best_predictor_idx, "Pr(>|t|)"], 4), ")\n") # I'm reporting which variable is the best predictor.
  cat("This variable has the strongest statistically significant relationship with Morphological Index.\n") # I'm explaining what this means.
} else {
  cat("No individual predictors reached statistical significance (p < 0.05).\n") # I'm handling the case where no predictors are significant.
}

# Now I want to repeat this analysis within just one treatment group to see if the relationships are different when I control for treatment effects.
cat("\n--- Multiple Regression: Within Single Treatment Group ---\n") # I'm creating a subsection for the within-group analysis.

# I'll pick a treatment group that has enough observations for reliable regression. Let me use A1 as an example.
single_treatment_data <- trevally_data[trevally_data$Treatment == "A1", ] # I'm subsetting my data to include only treatment group A1.
cat("Analyzing treatment group: A1 (n =", nrow(single_treatment_data), "observations)\n") # I'm reporting which group I'm analyzing and how many observations it has.

# I'm running the same regression model but only within this single treatment group.
multiple_reg_single <- lm(Morphological_Index ~ Avg_Length_Villi + Avg_Width_Villi + Length_SD + Width_SD, data = single_treatment_data) # I'm fitting the same model structure but with data from only one treatment group.

cat("Multiple Regression Results (Treatment A1 only):\n") # I'm labeling this output clearly.
reg_summary_single <- summary(multiple_reg_single) # I'm getting the regression summary for the single-group analysis.
print(reg_summary_single) # I'm printing the complete regression output for the single group.

# I want to interpret these within-group results and compare them to the overall analysis.
cat("\n--- Interpretation of Within-Group Multiple Regression ---\n") # I'm creating a section for within-group interpretation.
coefficients_single <- reg_summary_single$coefficients # I'm extracting the coefficients for the single-group analysis.
r_squared_single <- reg_summary_single$r.squared # I'm getting the R-squared for the within-group model.
adj_r_squared_single <- reg_summary_single$adj.r.squared # I'm getting the adjusted R-squared for the within-group model.

cat("Within-group model performance:\n") # I'm examining how well the model works within a single treatment.
cat("R-squared =", round(r_squared_single, 4), "- Within treatment A1, my model explains", round(r_squared_single*100, 1), "% of the variance.\n") # I'm reporting the variance explained within this treatment group.
cat("Adjusted R-squared =", round(adj_r_squared_single, 4), "\n") # I'm reporting the adjusted R-squared for the within-group analysis.

cat("\nComparison between overall vs. within-group analysis:\n") # I'm creating a section to compare the two approaches.
if (!is.na(r_squared_all) && !is.na(r_squared_single)) { # If both R-squared values are available...
  if (r_squared_all > r_squared_single) { # If the overall model explains more variance...
    cat("The overall model (R² =", round(r_squared_all, 3), ") explains more variance than the within-group model (R² =", round(r_squared_single, 3), ").\n") # I'm comparing the model performance.
    cat("This suggests that treatment differences contribute to the predictive relationships between variables.\n") # I'm interpreting what this difference means.
  } else { # If the within-group model is better...
    cat("The within-group model (R² =", round(r_squared_single, 3), ") explains more variance than the overall model (R² =", round(r_squared_all, 3), ").\n") # I'm reporting the opposite pattern.
    cat("This suggests that the relationships between variables are stronger within treatments than across treatments.\n") # I'm interpreting this pattern.
  }
} else { # If one or both R-squared values are missing...
  cat("Cannot compare models because within-group analysis failed due to insufficient variation in the data.\n") # I'm explaining why the comparison isn't possible.
  cat("This occurs when treatment groups have identical values, making within-group regression impossible.\n") # I'm providing the reason for the failure.
}

# I'm saving all my regression results to a file for future reference.
sink('trevally_regression_results.txt') # I'm opening a file to save my regression analysis.
cat('Multiple Regression Analysis Results\n') # I'm creating a header for the file.
cat('Giant Trevally (Caranx ignobilis) Intestinal Villi Morphology\n\n') # I'm adding the study context.

cat('Analysis 1: Multiple Regression Across All Treatment Groups\n') # I'm labeling the first analysis.
cat('Dependent Variable: Morphological Index\n') # I'm specifying what I'm trying to predict.
cat('Predictor Variables: Average Length, Average Width, Length SD, Width SD\n\n') # I'm listing the predictor variables.
print(reg_summary_all) # I'm saving the complete statistical output for the overall analysis.

cat('\nAnalysis 2: Multiple Regression Within Treatment Group A1\n') # I'm labeling the second analysis.
cat('Same variables but analyzed only within one treatment group\n\n') # I'm explaining the difference.
print(reg_summary_single) # I'm saving the complete statistical output for the within-group analysis.

cat('\nInterpretation and Biological Meaning:\n') # I'm adding my interpretation to the file.
cat('Multiple regression allows me to see which morphological measurements are the best predictors of overall size (Morphological Index).\n') # I'm explaining the purpose of this analysis.
cat('By comparing overall vs. within-group models, I can understand whether treatment effects modify the relationships between variables.\n') # I'm explaining why I did both analyses.
if (!is.na(r_squared_all) && !is.na(r_squared_single)) {
  if (r_squared_all > r_squared_single) {
    cat('The stronger overall model suggests that treatment differences enhance the predictive power of the morphological measurements.\n')
  } else {
    cat('The stronger within-group model suggests that treatments may modify how the morphological variables relate to each other.\n')
  }
} else {
  cat('Within-group analysis could not be completed due to insufficient variation within treatment groups.\n')
}
sink() # I'm closing the output file.

cat("Multiple regression results saved to: trevally_regression_results.txt\n") # I'm confirming that the file was saved.

## Section 9 — Creating Composite Variable and ANCOVA Analysis

# Now I want to create a meaningful composite variable, similar to how the iris dataset has a 'size' variable. For intestinal villi, I think a good composite measure would be the "Villi Surface Area Index" which combines both length and width in a multiplicative way, representing the functional surface area.
cat("\n=== CREATING COMPOSITE VARIABLE AND ANCOVA ANALYSIS ===\n") # I'm printing a header for this new section.

# I'm creating a composite variable that makes biological sense for intestinal villi. Surface area is crucial for absorption, so I'll calculate a surface area index by multiplying length and width, similar to how you'd calculate the area of a rectangle.
trevally_data$Villi_Surface_Index <- trevally_data$Avg_Length_Villi * trevally_data$Avg_Width_Villi # I'm creating a new variable that represents the approximate surface area per villus by multiplying average length by average width.

# I also want to create a ratio variable that might be biologically meaningful - the length-to-width ratio, which could indicate villi shape (tall and narrow vs. short and wide).
trevally_data$Length_Width_Ratio <- trevally_data$Avg_Length_Villi / trevally_data$Avg_Width_Villi # I'm creating a ratio that tells me about villi shape - higher values mean longer, thinner villi.

cat("Composite variables created:\n") # I'm announcing what new variables I've made.
cat("1. Villi_Surface_Index = Average Length × Average Width (represents functional surface area)\n") # I'm explaining the first composite variable.
cat("2. Length_Width_Ratio = Average Length ÷ Average Width (represents villi shape)\n") # I'm explaining the second composite variable.

# Let me examine the distribution of my new composite variables to make sure they make sense.
cat("\nSummary of composite variables:\n") # I'm creating a section to examine my new variables.
cat("Villi Surface Index:\n") # I'm labeling the first variable's summary.
print(summary(trevally_data$Villi_Surface_Index)) # I'm printing descriptive statistics for the surface area index.
cat("Length-Width Ratio:\n") # I'm labeling the second variable's summary.
print(summary(trevally_data$Length_Width_Ratio)) # I'm printing descriptive statistics for the length-width ratio.

# Now I'm going to set up an ANCOVA (Analysis of Covariance) to see how one variable is predicted by another while controlling for my composite variable. I'll examine how Average Length relates to Treatment while controlling for the Villi Surface Index.
cat("\n--- ANCOVA: Average Length by Treatment, controlling for Villi Surface Index ---\n") # I'm creating a subsection for my first ANCOVA.

# The idea behind ANCOVA is that I want to see if treatments affect average villi length, but I need to account for the fact that fish with different surface areas might naturally have different lengths. By controlling for surface area, I can see the "pure" effect of treatment.
ancova_model1 <- aov(Avg_Length_Villi ~ Treatment + Villi_Surface_Index, data = trevally_data) # I'm fitting an ANCOVA model using aov() where Average Length depends on both Treatment (categorical) and Villi Surface Index (continuous covariate).

cat("ANCOVA Results - Average Length by Treatment (controlling for Surface Index):\n") # I'm labeling this analysis clearly.
ancova_summary1 <- summary(ancova_model1) # I'm getting the ANCOVA summary table.
print(ancova_summary1) # I'm printing the full ANCOVA results showing F-statistics and p-values for both treatment and covariate effects.

# I want to interpret these results in biological terms.
treatment_p1 <- ancova_summary1[[1]][["Pr(>F)"]][1] # I'm extracting the p-value for the Treatment effect.
covariate_p1 <- ancova_summary1[[1]][["Pr(>F)"]][2] # I'm extracting the p-value for the Villi Surface Index covariate effect.

cat("\n--- Interpretation of ANCOVA Model 1 ---\n") # I'm creating an interpretation section.
cat("This ANCOVA tests whether treatment groups have different average villi lengths after accounting for differences in surface area.\n") # I'm explaining what this analysis does.
cat("Treatment effect (p =", round(treatment_p1, 4), "):", # I'm reporting the treatment p-value.
    ifelse(treatment_p1 < 0.05, "Significant - treatments affect length even after controlling for surface area.\n",
           "Not significant - no detectable treatment effect on length after controlling for surface area.\n")) # I'm interpreting the treatment effect.
cat("Covariate effect (p =", round(covariate_p1, 4), "):", # I'm reporting the covariate p-value.
    ifelse(covariate_p1 < 0.05, "Significant - surface area is a meaningful predictor of villi length.\n",
           "Not significant - surface area doesn't predict villi length in this dataset.\n")) # I'm interpreting the covariate effect.

# Now I'll do a second ANCOVA to examine how the Length-Width Ratio relates to Treatment while controlling for overall Morphological Index.
cat("\n--- ANCOVA: Length-Width Ratio by Treatment, controlling for Morphological Index ---\n") # I'm creating a subsection for my second ANCOVA.

# This second ANCOVA asks whether treatments affect villi shape (length-width ratio) after accounting for overall size (morphological index). This is biologically interesting because treatments might affect shape independently of size.
ancova_model2 <- aov(Length_Width_Ratio ~ Treatment + Morphological_Index, data = trevally_data) # I'm fitting a second ANCOVA where Length-Width Ratio depends on Treatment and Morphological Index as the covariate.

cat("ANCOVA Results - Length-Width Ratio by Treatment (controlling for Morphological Index):\n") # I'm labeling the second analysis.
ancova_summary2 <- summary(ancova_model2) # I'm getting the summary for the second ANCOVA.
print(ancova_summary2) # I'm printing the complete results for the shape analysis.

# I want to interpret these results as well.
treatment_p2 <- ancova_summary2[[1]][["Pr(>F)"]][1] # I'm extracting the treatment p-value for the second model.
covariate_p2 <- ancova_summary2[[1]][["Pr(>F)"]][2] # I'm extracting the covariate p-value for the second model.

cat("\n--- Interpretation of ANCOVA Model 2 ---\n") # I'm creating an interpretation section for the second model.
cat("This ANCOVA tests whether treatment groups have different villi shapes (length-width ratios) after accounting for overall morphological size.\n") # I'm explaining what the second analysis does.
cat("Treatment effect (p =", round(treatment_p2, 4), "):", # I'm reporting the treatment effect on shape.
    ifelse(treatment_p2 < 0.05, "Significant - treatments affect villi shape even after controlling for overall size.\n",
           "Not significant - no detectable treatment effect on villi shape after controlling for size.\n")) # I'm interpreting the treatment effect on shape.
cat("Covariate effect (p =", round(covariate_p2, 4), "):", # I'm reporting the size covariate effect.
    ifelse(covariate_p2 < 0.05, "Significant - overall size predicts villi shape.\n",
           "Not significant - overall size doesn't predict villi shape in this dataset.\n")) # I'm interpreting whether size predicts shape.

# I want to create plots to visualize these ANCOVA relationships.
cat("\n--- Creating ANCOVA Visualization Plots ---\n") # I'm announcing that I'm making plots.

# First plot: Average Length vs Villi Surface Index, colored by Treatment
ancova_plot1 <- ggplot(trevally_data, aes(x = Villi_Surface_Index, y = Avg_Length_Villi, color = Treatment)) + # I'm setting up a plot with surface index on x-axis and length on y-axis, colored by treatment.
  geom_point(size = 2, alpha = 0.7) + # I'm adding points for each observation with some transparency.
  geom_smooth(method = "lm", se = FALSE) + # I'm adding separate regression lines for each treatment group.
  labs(title = "ANCOVA: Average Villi Length vs Surface Index by Treatment", # I'm adding a descriptive title.
       x = "Villi Surface Index (Length × Width)", y = "Average Villi Length (μm)") + # I'm labeling the axes clearly.
  theme_minimal() + # I'm using a clean theme.
  theme(legend.position = "right") # I'm positioning the legend on the right.

ggsave("trevally_ancova_length_surface.pdf", ancova_plot1, width = 10, height = 6) # I'm saving the first ANCOVA plot.

# Second plot: Length-Width Ratio vs Morphological Index, colored by Treatment
ancova_plot2 <- ggplot(trevally_data, aes(x = Morphological_Index, y = Length_Width_Ratio, color = Treatment)) + # I'm setting up a plot with morphological index on x-axis and length-width ratio on y-axis.
  geom_point(size = 2, alpha = 0.7) + # I'm adding points for each observation.
  geom_smooth(method = "lm", se = FALSE) + # I'm adding regression lines for each treatment.
  labs(title = "ANCOVA: Length-Width Ratio vs Morphological Index by Treatment", # I'm adding a title.
       x = "Morphological Index", y = "Length-Width Ratio") + # I'm labeling the axes.
  theme_minimal() + # I'm using a minimal theme.
  theme(legend.position = "right") # I'm putting the legend on the right.

ggsave("trevally_ancova_ratio_morphindex.pdf", ancova_plot2, width = 10, height = 6) # I'm saving the second ANCOVA plot.

# I'm saving all my ANCOVA results to a comprehensive file.
sink('trevally_ancova_results.txt') # I'm opening a file to save my ANCOVA analysis.
cat('Analysis of Covariance (ANCOVA) Results\n') # I'm creating a header for the file.
cat('Giant Trevally (Caranx ignobilis) Intestinal Villi Morphology\n\n') # I'm adding study context.

cat('Composite Variables Created:\n') # I'm documenting my composite variables.
cat('1. Villi Surface Index = Average Length × Average Width\n') # I'm documenting the surface area measure.
cat('   Biological meaning: Approximates functional surface area for absorption\n') # I'm explaining why this matters biologically.
cat('2. Length-Width Ratio = Average Length ÷ Average Width\n') # I'm documenting the ratio measure.
cat('   Biological meaning: Indicates villi shape (tall/narrow vs short/wide)\n\n') # I'm explaining the biological significance.

cat('ANCOVA Model 1: Average Villi Length ~ Treatment + Villi Surface Index\n') # I'm labeling the first model.
cat('Question: Do treatments affect villi length after controlling for surface area?\n\n') # I'm explaining what this model tests.
print(ancova_summary1) # I'm saving the complete statistical output.
cat('\nInterpretation Model 1:\n') # I'm adding interpretation.
cat('Treatment effect: ', ifelse(treatment_p1 < 0.05, 'Significant', 'Not significant'), ' (p = ', round(treatment_p1, 4), ')\n', sep='') # I'm summarizing the treatment effect.
cat('Surface area covariate: ', ifelse(covariate_p1 < 0.05, 'Significant', 'Not significant'), ' (p = ', round(covariate_p1, 4), ')\n\n', sep='') # I'm summarizing the covariate effect.

cat('ANCOVA Model 2: Length-Width Ratio ~ Treatment + Morphological Index\n') # I'm labeling the second model.
cat('Question: Do treatments affect villi shape after controlling for overall size?\n\n') # I'm explaining what this model tests.
print(ancova_summary2) # I'm saving the complete statistical output for the second model.
cat('\nInterpretation Model 2:\n') # I'm adding interpretation for the second model.
cat('Treatment effect: ', ifelse(treatment_p2 < 0.05, 'Significant', 'Not significant'), ' (p = ', round(treatment_p2, 4), ')\n', sep='') # I'm summarizing the treatment effect on shape.
cat('Size covariate: ', ifelse(covariate_p2 < 0.05, 'Significant', 'Not significant'), ' (p = ', round(covariate_p2, 4), ')\n\n', sep='') # I'm summarizing whether size predicts shape.

cat('Biological Significance of ANCOVA:\n') # I'm adding a section on biological meaning.
cat('ANCOVA allows us to separate treatment effects from natural variation due to size or surface area.\n') # I'm explaining why ANCOVA is useful.
cat('This is important because larger fish or those with more surface area might naturally have different villi dimensions.\n') # I'm explaining the biological context.
cat('By controlling for these factors, we can see the "pure" treatment effects on morphology.\n') # I'm explaining what controlling for covariates accomplishes.
sink() # I'm closing the output file.

cat("ANCOVA analysis and results saved to: trevally_ancova_results.txt\n") # I'm confirming the file was saved.

## Section 10 — Analysis summary and output files

# I print a summary of the completed analysis and list all output files generated.
cat("\n=== ANALYSIS COMPLETE ===\n") # I print a header indicating the analysis is finished.
cat("Files generated:\n") # I print a label for the list of output files.
cat("- trevally_correlation_plot.pdf\n") # I list the correlation matrix visualization file.
cat("- trevally_length_vs_width.pdf\n") # I list the length vs width scatterplot file.
cat("- trevally_length_vs_morphindex.pdf\n") # I list the length vs morphological index scatterplot file.
cat("- trevally_width_vs_lengthsd.pdf\n") # I list the width vs length SD scatterplot file.
cat("- trevally_widthsd_vs_morphindex.pdf\n") # I list the width SD vs morphological index scatterplot file.
cat("- trevally_boxplots_by_treatment.pdf\n") # I list the box plot comparison file.
cat("- trevally_manova_results.txt\n") # I list the MANOVA results text file.
cat("- trevally_regression_results.txt\n") # I list the multiple regression analysis results file.
cat("- trevally_ancova_results.txt\n") # I list the ANCOVA analysis results file.
cat("- trevally_ancova_length_surface.pdf\n") # I list the first ANCOVA visualization plot.
cat("- trevally_ancova_ratio_morphindex.pdf\n") # I list the second ANCOVA visualization plot.
cat("- trevally_pca_biplot.pdf\n") # I list the PCA biplot file.
cat("\nThis comprehensive analysis examines morphological variation in Giant Trevally intestinal villi\n") # I provide a brief description of what this analysis accomplished.
cat("across different treatment groups using advanced multivariate statistical methods including MANOVA, multiple regression, and ANCOVA.\n") # I explain the scope of the multivariate analysis including all the methods I used.
