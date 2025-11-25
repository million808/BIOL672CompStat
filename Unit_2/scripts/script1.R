## MaximillianBalter_Unit2_BIOL672.R
# Author: Maximillian Balter
# OS: macOS
# Libraries/packages used: tidyverse, caret, class, e1071, MASS, mclust, ggplot2
# Input data files used: Unit_2/data/fish_data.csv
# Output files generated: fish_structure.txt, fish_summary.txt, split_counts.txt,
#    KNN_confusion_table.csv,
#    NaiveBayes_confusion_table.csv,
#    LDA_confusion_table.csv,
#    QDA_confusion_table.csv,
#    KNN_correctness_scatter.png,
#    NaiveBayes_correctness_scatter.png,
#    LDA_correctness_scatter.png,
#    QDA_correctness_scatter.png,
#    cv_per_fold_accuracies.csv, cv_accuracy_summary.csv, cv_accuracy_summary.txt,
#    cv_accuracies_boxplot.png, mclust_table.txt
# This script is divided into sections:
#   1. Setup and libraries
#   2. Locate data and prepare output dir
#   3. Load data and quick inspection
#   4. Train/test holdout predictions and per-observation plots
#   5. 5-fold cross-validation and summary
#   6. Optional EM clustering with mclust
#   7. Final notes and completion


## Section 1 — Setup and libraries
cat("UNIT 2: SCRIPT1 - CLASSICAL METHODS WITH CV & PLOTS\n")  # I print a header so I can see which script is running
cat("====================================================\n\n")  # Simple divider for readability in the console

# First I make a vector of the packages I plan to use
pkgs <- c('tidyverse','caret','class','e1071','MASS','mclust','ggplot2')  # Data, ML models, clustering, plotting

# Now I go through each package and make sure it is installed and loaded
for(p in pkgs) {  # Loop over each package name
  if(!requireNamespace(p, quietly = TRUE)) {               # If the package is not available
    install.packages(p, repos = 'https://cran.rstudio.com/')  # I install it from CRAN
  }
  library(p, character.only = TRUE)  # Then I attach the package to my R session
}
## MaximillianBalter_Unit2_BIOL672.R
# Author: Maximillian Balter
# OS: macOS
# Libraries/packages used: tidyverse, caret, class, e1071, MASS, mclust, ggplot2
# Input data files used: Unit_2/data/fish_data.csv
# Output files generated: fish_structure.txt, fish_summary.txt, split_counts.txt,
#    KNN_confusion_table.csv,
#    NaiveBayes_confusion_table.csv,
#    LDA_confusion_table.csv,
#    QDA_confusion_table.csv,
#    KNN_correctness_scatter.png,
#    NaiveBayes_correctness_scatter.png,
#    LDA_correctness_scatter.png,
#    QDA_correctness_scatter.png,
#    cv_per_fold_accuracies.csv, cv_accuracy_summary.csv, cv_accuracy_summary.txt,
#    cv_accuracies_boxplot.png, mclust_table.txt
# This script is divided into sections:
#   1. Setup and libraries
#   2. Locate data and prepare output dir
#   3. Load data and quick inspection
#   4. Train/test holdout predictions and per-observation plots
#   5. 5-fold cross-validation and summary
#   6. Optional EM clustering with mclust
#   7. Final notes and completion


## Section 1 — Setup and libraries
cat("UNIT 2: SCRIPT1 - CLASSICAL METHODS WITH CV & PLOTS\n")  # I print a header so I can see which script is running
cat("====================================================\n\n")  # Simple divider for readability in the console

# First I make a vector of the packages I plan to use
pkgs <- c('tidyverse','caret','class','e1071','MASS','mclust','ggplot2')  # Data, ML models, clustering, plotting

# Now I go through each package and make sure it is installed and loaded
for(p in pkgs) {  # Loop over each package name
  if(!requireNamespace(p, quietly = TRUE)) {               # If the package is not available
    install.packages(p, repos = 'https://cran.rstudio.com/')  # I install it from CRAN
  }
  library(p, character.only = TRUE)  # Then I attach the package to my R session
}

# I set a seed here so that the model results are reproducible when I rerun the script
set.seed(2529)


## Section 2 — Locate data and prepare output dir
cat("## Section 2 — Locate data and prepare output dir\n")  # Let myself know I’m in the file-path logic part

# I don’t want the script to break if I run it from slightly different working directories,
# so I define several possible locations for fish_data.csv and let the script search for it.
candidate_paths <- c(
  file.path(getwd(), 'Unit_2','data','fish_data.csv'),             # If I run from repo root
  file.path(getwd(), '..','Unit_2','data','fish_data.csv'),        # If I run from inside a subdir
  file.path(getwd(), '..','data','fish_data.csv'),                 # Another alternative
  file.path(dirname(getwd()), 'Unit_2','data','fish_data.csv'),    
  file.path(Sys.getenv('HOME'),'BIO672_COMPSTAT','Unit_2','data','fish_data.csv')  # A “home” fallback path
)

data_path <- NULL  # I’ll store the path that actually works here

# I walk through each candidate path and pick the first one that exists
for(p in candidate_paths) {
  if(file.exists(p)) {  # As soon as I find the file
    data_path <- p      # I save the path
    break               # And stop checking other candidates
  }
}

# If no path was found, I stop the script with a clear message
if(is.null(data_path)) {
  stop('fish_data.csv not found under Unit_2/data. Place the CSV and retry.')
}

# I want all my outputs in a consistent place.
# Use the shared `Unit_2/output` directory but do not create it here.
out_dir <- file.path(dirname(dirname(data_path)), 'output')  # Use existing output/ one level above Unit_2
if(!dir.exists(out_dir)) {
  stop(paste('Expected output directory not found:', out_dir, '\nPlease create it manually and re-run this script.'))
}

# Quick sanity check: I print out where the data came from and where outputs will go
cat('Data path:', data_path, '\nOutput:', out_dir, '\n\n')


## Section 3 — Load data and quick inspection
cat('## Section 3 — Load data and quick inspection\n')  # Announce that I’m loading data

# I read in the fish CSV; I turn off the column type messages to keep the console clean
fish <- readr::read_csv(data_path, show_col_types = FALSE)

# Now I explicitly set the types I expect to use in my models
fish <- fish %>%
  mutate(
    species = as.factor(species),  # Species is my response variable, so I want it as a factor
    length  = as.numeric(length),  # Length should be numeric
    weight  = as.numeric(weight)   # Weight should be numeric as well
    # Any other numeric columns in the CSV (like w_l_ratio) can stay as-is and they’ll be included by species~.
  )

# For documentation and debugging later, I save the structure and summary of the data
write_lines(capture.output(str(fish)), file.path(out_dir,'fish_structure.txt'))  # I write the structure to a text file
write_lines(capture.output(summary(fish)), file.path(out_dir,'fish_summary.txt'))  # Same for summary statistics

# I also print a short summary to the console so I know what I’m working with
cat(
  'Loaded fish data with', nrow(fish), 'rows and species levels:',
  paste(levels(fish$species), collapse=', '), '\n\n'
)


## Section 4 — Train/test split and per-observation plots
cat('## Section 4 — Train/test split and per-observation predictions\n')  # Now I’m doing the 70/30 split and basic models

# I want a 70/30 split but also want species proportions preserved,
# so I use caret::createDataPartition which does stratified sampling.
train_idx <- createDataPartition(fish$species, p = 0.7, list = FALSE)
train <- fish[train_idx, ]   # These rows will be my training data
test  <- fish[-train_idx, ]  # The remaining rows become my test set

# I log how many rows ended up in train and test for my own record
write_lines(
  paste0('train n=', nrow(train), '; test n=', nrow(test)),
  file.path(out_dir,'split_counts.txt')
)

# I’m going to reuse the same logic to create confusion tables and correctness plots for each model,
# so I wrap it in a helper function to avoid copy-pasting the same code four times.
save_confusion_and_plot <- function(true, pred, df_test, model_name) {
  # I build a tibble of true and predicted labels,
  # and add a logical column indicating whether each prediction was correct.
  res <- tibble(true = true, pred = pred) %>%
    mutate(correct = (true == pred))
  
  # For the scatterplot, I want the original numeric features length and weight alongside correctness info.
  res_plot <- bind_cols(df_test %>% dplyr::select(length, weight), res)
  
  # First I create a confusion table (true vs predicted) and save it to a CSV;
  # this matches the assignment request for a confusion matrix.
  cm <- as.data.frame.matrix(table(res$true, res$pred))
  cm_out <- bind_cols(
    tibble(model = model_name),
    tibble::rownames_to_column(cm, var = 'true')
  )
  write_csv(cm_out, file.path(out_dir, paste0(model_name, '_confusion_table.csv')))
  
  # Next I plot length vs weight, coloring points by whether they were correct or not.
  # This gives a nice visual of where the classifier is making mistakes.
  p <- ggplot(res_plot, aes(x = length, y = weight, color = correct)) +
    geom_point(alpha = 0.7) +
    ggtitle(paste(model_name, '- correct (TRUE) vs incorrect (FALSE)')) +
    theme_minimal()
  
  # Finally, I save that plot to a PNG so I can include it in my report or slides.
  ggsave(
    filename = file.path(out_dir, paste0(model_name, '_correctness_scatter.png')),
    plot = p, width = 6, height = 4
  )
}

cat('Fitting KNN, Naive Bayes, LDA, QDA on holdout training set...\n')  # Now I’m actually fitting the four models

# 1) KNN (k = 5) using caret
knn_fit <- train(
  species ~ ., data   = train,               # Use species as the response and all other columns as predictors
  method    = 'knn',                         # K nearest neighbors
  tuneGrid  = data.frame(k = 5),             # I fix k=5, rather than tuning
  preProcess = c('center','scale'),          # I standardize predictors, which KNN really needs
  trControl  = trainControl(method = 'none') # No internal resampling here; this is just the holdout fit
)
knn_pred <- predict(knn_fit, newdata = test)  # I get predictions on the test set
save_confusion_and_plot(as.character(test$species), as.character(knn_pred), test, 'KNN')  # Save confusion matrix + plot

# 2) Naive Bayes using e1071
nb_fit <- naiveBayes(species ~ ., data = train)            # Fit NB using all predictors
nb_pred <- predict(nb_fit, newdata = test)                 # Predictions on the test set
save_confusion_and_plot(as.character(test$species), as.character(nb_pred), test, 'NaiveBayes')

# 3) LDA using MASS::lda
lda_fit <- lda(species ~ ., data = train)                  # Fit LDA to the training data
lda_pred <- predict(lda_fit, newdata = test)$class         # LDA’s predict() returns a list; I pull out the predicted class
save_confusion_and_plot(as.character(test$species), as.character(lda_pred), test, 'LDA')

# 4) QDA using MASS::qda
qda_fit <- qda(species ~ ., data = train)                  # Same idea, but with quadratic boundaries
qda_pred <- predict(qda_fit, newdata = test)$class         # Again, grab the predicted classes from the list
save_confusion_and_plot(as.character(test$species), as.character(qda_pred), test, 'QDA')

cat('Holdout evaluation confusion tables and plots saved.\n\n')


## Section 5 — 5-fold cross-validation for each method
cat('## Section 5 — 5-fold cross-validation for each method\n')  # Now I switch to manual CV

# The assignment specifies a k-fold CV procedure, so here I actually implement 5-fold CV.
# I use createFolds to generate stratified folds based on species.
folds <- createFolds(fish$species, k = 5, list = TRUE, returnTrain = FALSE)

# I’ll collect all the fold-level accuracies here,
# then summarize them at the end.
cv_results <- tibble(
  fold    = integer(),
  method  = character(),
  accuracy = double()
)

# I loop over the 5 folds
# then record the accuracy for each method.
for(i in seq_along(folds)) {
  cat('Processing fold', i, '\n')
  
  test_idx  <- folds[[i]]                            # Indices for this fold’s test set
  train_idx <- setdiff(seq_len(nrow(fish)), test_idx) # The rest become training indices
  
  df_train <- fish[train_idx, ]
  df_test  <- fish[test_idx, ]
  
  # KNN on this fold
  knn_fold <- train(
    species ~ ., data = df_train,
    method    = 'knn',
    tuneGrid  = data.frame(k = 5),
    preProcess = c('center','scale'),
    trControl  = trainControl(method = 'none')
  )
  pred_knn <- predict(knn_fold, newdata = df_test)
  acc_knn  <- mean(pred_knn == df_test$species)
  cv_results <- bind_rows(cv_results, tibble(fold = i, method = 'KNN', accuracy = acc_knn))
  
  # Naive Bayes on this fold
  nb_fold <- naiveBayes(species ~ ., data = df_train)
  pred_nb <- predict(nb_fold, newdata = df_test)
  acc_nb  <- mean(pred_nb == df_test$species)
  cv_results <- bind_rows(cv_results, tibble(fold = i, method = 'NaiveBayes', accuracy = acc_nb))
  
  # LDA on this fold
  lda_fold <- lda(species ~ ., data = df_train)
  pred_lda <- predict(lda_fold, newdata = df_test)$class
  acc_lda  <- mean(pred_lda == df_test$species)
  cv_results <- bind_rows(cv_results, tibble(fold = i, method = 'LDA', accuracy = acc_lda))
  
  # QDA on this fold
  qda_fold <- qda(species ~ ., data = df_train)
  pred_qda <- predict(qda_fold, newdata = df_test)$class
  acc_qda  <- mean(pred_qda == df_test$species)
  cv_results <- bind_rows(cv_results, tibble(fold = i, method = 'QDA', accuracy = acc_qda))
}

# I save the raw per-fold accuracies in case I want to inspect them later
write_csv(cv_results, file.path(out_dir, 'cv_per_fold_accuracies.csv'))

# Then I summarize by method: mean and standard deviation of accuracy across folds
cv_summary <- cv_results %>%
  group_by(method) %>%
  summarize(
    mean_accuracy = mean(accuracy),
    sd_accuracy   = sd(accuracy)
  )

write_csv(cv_summary, file.path(out_dir, 'cv_accuracy_summary.csv'))
write_lines(capture.output(cv_summary), file.path(out_dir, 'cv_accuracy_summary.txt'))

# I also make a boxplot of the cross-validation accuracies
# so I can visually compare how stable each method is.
p_cv <- ggplot(cv_results, aes(x = method, y = accuracy)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, height = 0, alpha = 0.7) +
  theme_minimal() +
  ggtitle('5-fold CV accuracies by method')

ggsave(
  filename = file.path(out_dir, 'cv_accuracies_boxplot.png'),
  plot = p_cv, width = 6, height = 4
)

cat('Cross-validation complete; per-fold accuracies and summary saved.\n\n')


## Section 6 — Optional EM clustering with mclust
cat('## Section 6 — Optional EM clustering with mclust\n')  # Now I do the unsupervised comparison

# For EM clustering, I only want the numeric predictors, not the species labels.
numeric_cols <- fish %>% dplyr::select(-species)

# Mclust will try different Gaussian mixture models and pick the “best” one by BIC.
mclust_fit <- Mclust(numeric_cols)

# I grab the cluster assignments so I can compare them to the true species labels.
cluster_labels <- mclust_fit$classification

mclust_results <- tibble(
  species = fish$species,
  cluster = factor(cluster_labels)
)

# I create a contingency table of species (rows) vs cluster ID (columns)
# to see how well unsupervised clustering corresponds to the actual labels.
mclust_table <- as.data.frame.matrix(table(mclust_results$species, mclust_results$cluster))

# And I save that table to a text file to look at later.
write_lines(
  capture.output(mclust_table),
  file.path(out_dir, 'mclust_table.txt')
)

cat('EM clustering with mclust complete; species vs cluster table saved as mclust_table.txt.\n\n')


## Section 7 — Final notes and completion
cat('SCRIPT1 complete. Check the files in', out_dir, '\n')  # A final message so I know this script finished successfully
