## MaximillianBalter_Unit2_Part2_BIOL672.R
# Author: Maximillian Balter
# OS: macOS
# Libraries/packages used: tidyverse, e1071, kernlab, caret, ggplot2
# Input data files used: Unit_2/data/fish_data.csv
# Output files generated: SVM_linear_confusion_table.csv, SVM_polynomial_confusion_table.csv,
#    SVM_radial_confusion_table.csv, KSVM_linear_confusion_table.csv,
#    KSVM_polynomial_confusion_table.csv, KSVM_radial_confusion_table.csv,
#    SVM_linear_correctness_scatter.png, SVM_polynomial_correctness_scatter.png,
#    SVM_radial_correctness_scatter.png, KSVM_linear_correctness_scatter.png,
#    KSVM_polynomial_correctness_scatter.png, KSVM_radial_correctness_scatter.png,
#    cv_per_fold_accuracies_svm.csv, cv_accuracy_summary_svm.csv,
#    cv_accuracy_summary_svm.txt, cv_accuracies_boxplot_svm.png

## Section 1 — Setup and libraries
# Print a header so I know this script is starting
cat("UNIT 2: SCRIPT2 - SUPPORT VECTOR MACHINES\n")
# Print a divider to separate the header from the rest of the output
cat("====================================================\n\n")
# Here I make a vector of the packages I plan to use
pkgs <- c('tidyverse', 'e1071', 'kernlab', 'caret', 'ggplot2')
# Now I go through each package and install it if it's missing and then load it
for(p in pkgs) {
  # If the package isn't installed, I install it from CRAN
  if(!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = 'https://cran.rstudio.com/')
  }
  # Then I attach the package to my R session
  library(p, character.only = TRUE)
}
# I set a seed here so my results are reproducible when I rerun the script
set.seed(2529)

## Section 2 — Locate data and prepare output dir
# Let myself know I'm in the file path logic part
cat("## Section 2 — Locate data and prepare output dir\n")
# I build a list of possible locations where the fish data CSV might live
candidate_paths <- c(
  file.path(getwd(), 'Unit_2', 'data', 'fish_data.csv'),
  file.path(getwd(), '..', 'Unit_2', 'data', 'fish_data.csv'),
  file.path(getwd(), '..', 'data', 'fish_data.csv'),
  file.path(dirname(getwd()), 'Unit_2', 'data', 'fish_data.csv'),
  file.path(Sys.getenv('HOME'), 'BIO672_COMPSTAT', 'Unit_2', 'data', 'fish_data.csv')
)
# I'll store whichever path works in data_path
data_path <- NULL
# I loop through each candidate path
for(p in candidate_paths) {
  # If the file exists at this path I save that path and stop checking others
  if(file.exists(p)) {
    data_path <- p
    break
  }
}
# If I never found a path, I stop with an error message
if(is.null(data_path)) {
  stop('fish_data.csv not found under Unit_2/data. Place the CSV and retry.')
}
# I want all outputs in the existing output directory one level above Unit_2
out_dir <- file.path(dirname(dirname(data_path)), 'output')
# If that directory doesn't exist, I stop so I can create it manually
if(!dir.exists(out_dir)) {
  stop(paste('Expected output directory not found:', out_dir, '\\nPlease create it manually and re‑run this script.'))
}
# Print out the paths I found so I can verify them
cat('Data path:', data_path, '\\nOutput:', out_dir, '\\n\\n')

## Section 3 — Load data and quick inspection
# Announce that I'm loading the data now
cat('## Section 3 — Load data and quick inspection\\n')
# Read the CSV file and coerce the columns to the appropriate types
fish <- readr::read_csv(data_path, show_col_types = FALSE) %>%
  mutate(
    # Species is my response variable so I convert it to a factor
    species = as.factor(species),
    # Length should be numeric
    length  = as.numeric(length),
    # Weight should also be numeric
    weight  = as.numeric(weight)
  )
# Save a copy of the data structure to a text file for documentation
write_lines(capture.output(str(fish)), file.path(out_dir, 'fish_structure.txt'))
# Save summary statistics for reference later
write_lines(capture.output(summary(fish)), file.path(out_dir, 'fish_summary.txt'))
# Print a short summary to the console so I know what I'm working with
cat(
  'Loaded fish data with', nrow(fish), 'rows and species levels:',
  paste(levels(fish$species), collapse = ', '), '\\n\\n'
)

## Section 4 — Train/test holdout predictions and per‑observation plots for SVM kernels
# Announce that I'm starting the SVM holdout section
cat('## Section 4 — Train/test holdout predictions with SVM kernels\\n')
# Create a 70/30 split while preserving species proportions
train_idx <- createDataPartition(fish$species, p = 0.7, list = FALSE)
# Subset the training data
train <- fish[train_idx, ]
# Subset the test data
test  <- fish[-train_idx, ]
# Record how many rows ended up in the train and test sets
write_lines(
  paste0('train n=', nrow(train), '; test n=', nrow(test)),
  file.path(out_dir, 'split_counts_svm.txt')
)
# Define a helper function to save confusion matrices and scatter plots
save_svm_confusion_and_plot <- function(true, pred, df_test, model_name) {
  # Build a tibble of true and predicted labels with a correctness flag
  res <- tibble(true = true, pred = pred) %>% mutate(correct = (true == pred))
  # Combine the numeric features with the result to make a plotting data frame
  res_plot <- bind_cols(df_test %>% dplyr::select(length, weight), res)
  # Create a confusion matrix of true vs predicted classes
  cm <- as.data.frame.matrix(table(res$true, res$pred))
  # Add the model name and convert row names into a column
  cm_out <- bind_cols(
    tibble(model = model_name),
    tibble::rownames_to_column(cm, var = 'true')
  )
  # Save the confusion matrix to a CSV file
  write_csv(cm_out, file.path(out_dir, paste0(model_name, '_confusion_table.csv')))
  # Make a scatter plot of length vs weight colored by correctness
  p <- ggplot(res_plot, aes(x = length, y = weight, color = correct)) +
    geom_point(alpha = 0.7) +
    ggtitle(paste(model_name, '- correct (TRUE) vs incorrect (FALSE)')) +
    theme_minimal()
  # Save the plot to disk
  ggsave(
    filename = file.path(out_dir, paste0(model_name, '_correctness_scatter.png')),
    plot = p, width = 6, height = 4
  )
}
# Let myself know I'm fitting the SVM models
cat('Fitting SVM models on holdout training set...\\n')
# Fit a linear SVM from e1071
svm_linear_fit <- svm(
  species ~ ., data = train, kernel = 'linear'
)
# Make predictions on the test set for the linear SVM
svm_linear_pred <- predict(svm_linear_fit, newdata = test)
# Save the confusion matrix and plot for the linear SVM
save_svm_confusion_and_plot(as.character(test$species), as.character(svm_linear_pred), test, 'SVM_linear')
# Fit a polynomial SVM from e1071 (defaults to degree 3)
svm_poly_fit <- svm(
  species ~ ., data = train, kernel = 'polynomial'
)
# Predict on the test set for the polynomial SVM
svm_poly_pred <- predict(svm_poly_fit, newdata = test)
# Save the confusion matrix and plot for the polynomial SVM
save_svm_confusion_and_plot(as.character(test$species), as.character(svm_poly_pred), test, 'SVM_polynomial')
# Fit an RBF SVM from e1071
svm_rbf_fit <- svm(
  species ~ ., data = train, kernel = 'radial'
)
# Predict on the test set for the RBF SVM
svm_rbf_pred <- predict(svm_rbf_fit, newdata = test)
# Save the confusion matrix and plot for the RBF SVM
save_svm_confusion_and_plot(as.character(test$species), as.character(svm_rbf_pred), test, 'SVM_radial')
# Fit a linear SVM using kernlab (vanilladot kernel)
ksvm_linear_fit <- ksvm(
  species ~ ., data = train,
  kernel = 'vanilladot', type = 'C-svc'
)
# Predict on the test set for the kernlab linear SVM
ksvm_linear_pred <- predict(ksvm_linear_fit, newdata = test)
# Save the confusion matrix and plot for the kernlab linear SVM
save_svm_confusion_and_plot(as.character(test$species), as.character(ksvm_linear_pred), test, 'KSVM_linear')
# Fit a polynomial SVM using kernlab (polydot kernel)
ksvm_poly_fit <- ksvm(
  species ~ ., data = train,
  kernel = 'polydot', type = 'C-svc'
)
# Predict on the test set for the kernlab polynomial SVM
ksvm_poly_pred <- predict(ksvm_poly_fit, newdata = test)
# Save the confusion matrix and plot for the kernlab polynomial SVM
save_svm_confusion_and_plot(as.character(test$species), as.character(ksvm_poly_pred), test, 'KSVM_polynomial')
# Fit an RBF SVM using kernlab (rbfdot kernel)
ksvm_rbf_fit <- ksvm(
  species ~ ., data = train,
  kernel = 'rbfdot', type = 'C-svc'
)
# Predict on the test set for the kernlab RBF SVM
ksvm_rbf_pred <- predict(ksvm_rbf_fit, newdata = test)
# Save the confusion matrix and plot for the kernlab RBF SVM
save_svm_confusion_and_plot(as.character(test$species), as.character(ksvm_rbf_pred), test, 'KSVM_radial')
# Let myself know I'm done with the holdout evaluation
cat('Holdout evaluation for SVM kernels complete; confusion tables and plots saved.\\n\\n')

## Section 5 — 5‑fold cross‑validation and summary
# Print a header for the cross‑validation section
cat('## Section 5 — 5‑fold cross‑validation for SVM kernels\\n')
# Create stratified folds using caret
folds <- createFolds(fish$species, k = 5, list = TRUE, returnTrain = FALSE)
# Start an empty tibble to collect results
cv_results <- tibble(
  fold    = integer(),
  method  = character(),
  accuracy = double()
)
# Loop over each fold index
for(i in seq_along(folds)) {
  # Print which fold I'm working on
  cat('Processing fold', i, '\\n')
  # Get test indices for this fold
  test_idx  <- folds[[i]]
  # Determine training indices for this fold
  train_idx <- setdiff(seq_len(nrow(fish)), test_idx)
  # Subset the training data
  df_train <- fish[train_idx, ]
  # Subset the test data
  df_test  <- fish[test_idx, ]
  # Fit a linear SVM (e1071) on this fold
  fit_svml <- svm(species ~ ., data = df_train, kernel = 'linear')
  # Predict on the test fold for the linear SVM
  pred_svml <- predict(fit_svml, newdata = df_test)
  # Compute the accuracy for the linear SVM
  acc_svml  <- mean(pred_svml == df_test$species)
  # Add this result to the cv_results tibble
  cv_results <- bind_rows(cv_results, tibble(fold = i, method = 'SVM_linear', accuracy = acc_svml))
  # Fit a polynomial SVM (e1071) on this fold
  fit_svmp <- svm(species ~ ., data = df_train, kernel = 'polynomial')
  # Predict on the test fold for the polynomial SVM
  pred_svmp <- predict(fit_svmp, newdata = df_test)
  # Compute the accuracy for the polynomial SVM
  acc_svmp  <- mean(pred_svmp == df_test$species)
  # Add this result to the cv_results tibble
  cv_results <- bind_rows(cv_results, tibble(fold = i, method = 'SVM_polynomial', accuracy = acc_svmp))
  # Fit a radial SVM (e1071) on this fold
  fit_svmr <- svm(species ~ ., data = df_train, kernel = 'radial')
  # Predict on the test fold for the radial SVM
  pred_svmr <- predict(fit_svmr, newdata = df_test)
  # Compute the accuracy for the radial SVM
  acc_svmr  <- mean(pred_svmr == df_test$species)
  # Add this result to the cv_results tibble
  cv_results <- bind_rows(cv_results, tibble(fold = i, method = 'SVM_radial', accuracy = acc_svmr))
  # Fit a linear SVM using kernlab on this fold
  fit_ksvml <- ksvm(species ~ ., data = df_train, kernel = 'vanilladot', type = 'C-svc')
  # Predict on the test fold for the kernlab linear SVM
  pred_ksvml <- predict(fit_ksvml, newdata = df_test)
  # Compute the accuracy for the kernlab linear SVM
  acc_ksvml  <- mean(pred_ksvml == df_test$species)
  # Add this result to the cv_results tibble
  cv_results <- bind_rows(cv_results, tibble(fold = i, method = 'KSVM_linear', accuracy = acc_ksvml))
  # Fit a polynomial SVM using kernlab on this fold
  fit_ksvmp <- ksvm(species ~ ., data = df_train, kernel = 'polydot', type = 'C-svc')
  # Predict on the test fold for the kernlab polynomial SVM
  pred_ksvmp <- predict(fit_ksvmp, newdata = df_test)
  # Compute the accuracy for the kernlab polynomial SVM
  acc_ksvmp  <- mean(pred_ksvmp == df_test$species)
  # Add this result to the cv_results tibble
  cv_results <- bind_rows(cv_results, tibble(fold = i, method = 'KSVM_polynomial', accuracy = acc_ksvmp))
  # Fit a radial SVM using kernlab on this fold
  fit_ksvmr <- ksvm(species ~ ., data = df_train, kernel = 'rbfdot', type = 'C-svc')
  # Predict on the test fold for the kernlab radial SVM
  pred_ksvmr <- predict(fit_ksvmr, newdata = df_test)
  # Compute the accuracy for the kernlab radial SVM
  acc_ksvmr  <- mean(pred_ksvmr == df_test$species)
  # Add this result to the cv_results tibble
  cv_results <- bind_rows(cv_results, tibble(fold = i, method = 'KSVM_radial', accuracy = acc_ksvmr))
}
# Save the per‑fold accuracies to a CSV file for later inspection
write_csv(cv_results, file.path(out_dir, 'cv_per_fold_accuracies_svm.csv'))
# Compute the mean and standard deviation of accuracy by method
cv_summary <- cv_results %>%
  group_by(method) %>%
  summarize(
    mean_accuracy = mean(accuracy),
    sd_accuracy   = sd(accuracy)
  )
# Save the summary to CSV
write_csv(cv_summary, file.path(out_dir, 'cv_accuracy_summary_svm.csv'))
# Save the summary to a text file as well
write_lines(capture.output(cv_summary), file.path(out_dir, 'cv_accuracy_summary_svm.txt'))
# Create a boxplot comparing accuracies across methods
p_cv <- ggplot(cv_results, aes(x = method, y = accuracy)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, height = 0, alpha = 0.7) +
  theme_minimal() +
  ggtitle('5‑fold CV accuracies by SVM method')
# Save the boxplot to a PNG
ggsave(
  filename = file.path(out_dir, 'cv_accuracies_boxplot_svm.png'),
  plot = p_cv, width = 7, height = 5
)
# Let myself know that cross‑validation is complete
cat('Cross‑validation complete; accuracies and summary saved for all SVM kernels.\\n\\n')

## Section 6 — Final notes and completion
# Print a final message so I know the script finished successfully
cat('SCRIPT2 complete. Check the files in', out_dir, '\\n')
# I print a short console summary that answers the main SVM questions for the assignment

# I read in the SVM CV summary from this script
svm_cv_summary <- readr::read_csv(
  file.path(out_dir, "cv_accuracy_summary_svm.csv"),
  show_col_types = FALSE
)

# I also read in the simpler-method CV summary from Script 1
simple_cv_summary <- readr::read_csv(
  file.path(out_dir, "cv_accuracy_summary.csv"),
  show_col_types = FALSE
)

# I find the best-performing SVM kernel based on mean accuracy
svm_best <- svm_cv_summary %>%
  dplyr::arrange(dplyr::desc(mean_accuracy)) %>%
  dplyr::slice(1)

# I find the best-performing simpler method (KNN, NB, LDA, QDA) from Script 1
simple_best <- simple_cv_summary %>%
  dplyr::arrange(dplyr::desc(mean_accuracy)) %>%
  dplyr::slice(1)

cat("\n## Cross-validation summary for SVM kernels\n\n")

# I print each SVM kernel’s mean accuracy and standard deviation
for (i in seq_len(nrow(svm_cv_summary))) {
  cat(
    svm_cv_summary$method[i], ":\n",
    "  Mean accuracy: ", sprintf("%.4f", svm_cv_summary$mean_accuracy[i]), "\n",
    "  SD of accuracy: ", sprintf("%.4f", svm_cv_summary$sd_accuracy[i]), "\n\n",
    sep = ""
  )
}

cat("Best SVM kernel (by mean CV accuracy):\n",
  "  Method: ", svm_best$method, "\n",
  "  Mean accuracy: ", sprintf("%.4f", svm_best$mean_accuracy), "\n\n",
    sep = "")

cat("Best simpler method from Script 1 (KNN / NaiveBayes / LDA / QDA):\n",
  "  Method: ", simple_best$method, "\n",
  "  Mean accuracy: ", sprintf("%.4f", simple_best$mean_accuracy), "\n\n",
    sep = "")

# I directly answer whether SVM outperformed the simpler methods
if (svm_best$mean_accuracy > simple_best$mean_accuracy) {
  cat("Overall conclusion: the best SVM kernel outperformed the best simpler method from Script 1 on this dataset.\n\n")
} else if (svm_best$mean_accuracy < simple_best$mean_accuracy) {
  cat("Overall conclusion: the best simpler method from Script 1 outperformed the best SVM kernel on this dataset.\n\n")
} else {
  cat("Overall conclusion: the best SVM kernel and the best simpler method from Script 1 had the same mean CV accuracy on this dataset.\n\n")
}