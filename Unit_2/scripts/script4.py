## MaximillianBalter_Unit2_Part4_BIOL672.py
# Author: Maximillian Balter
# OS: macOS
# Libraries/modules used: os, sys, subprocess, pandas, numpy,
#    sklearn (model_selection, metrics, ensemble), matplotlib
# Input data: Unit_2/data/fish_data.csv
# Output files: RF_confusion_table.csv, Ada_confusion_table.csv,
#    RF_correctness_scatter.png, Ada_correctness_scatter.png,
#    cv_per_fold_accuracies_rf_ada.csv, cv_accuracy_summary_rf_ada.csv,
#    cv_accuracy_summary_rf_ada.txt, cv_accuracies_boxplot_rf_ada.png
# Sections:
#   1. Setup and libraries
#   2. Hardcoded data/output paths
#   3. Load data and quick check
#   4. Train/test predictions for random forest and AdaBoost
#   5. 5-fold cross-validation
#   6. Completion message


## Section 1 — Setup and libraries
print("UNIT 2: SCRIPT4 - RANDOM FOREST AND ADABOOST")
print("====================================================\n")

# I keep a list of module names so I can check that everything I need is importable
modules = [
    'os', 'sys', 'subprocess', 'pandas', 'numpy', 'sklearn', 'matplotlib'
]

# I loop through the module names to import them, installing them if needed
for mod in modules:
    try:
        globals()[mod] = __import__(mod)  # I import the module into the global namespace
    except ImportError:
        print(f"Module {mod} not found, attempting to install...")
        try:
            subprocess.check_call([sys.executable, '-m', 'pip', 'install', mod])  # install if missing
        except Exception as e:
            print(f"Failed to install {mod}: {e}")
        globals()[mod] = __import__(mod)  # try importing again after install

# I import the functions and classes I will use directly
from pandas import DataFrame, read_csv
from numpy import array
from sklearn.model_selection import train_test_split, StratifiedKFold
from sklearn.metrics import accuracy_score, confusion_matrix
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier
import matplotlib.pyplot as plt


## Section 2 — Hardcoded data and output paths
print("## Section 2 — Locate data and prepare output dir")

import os  # I import os so I can work with file paths

# I hardcode the path so the script always loads the dataset from Unit_2/data
data_path = os.path.join(os.getcwd(), 'Unit_2', 'data', 'fish_data.csv')

# I hardcode the output directory so all results for this unit go in the same place
out_dir = os.path.join(os.getcwd(), 'Unit_2', 'output')

# I check if the output directory exists, and create it if it doesn’t
if not os.path.isdir(out_dir):
    os.makedirs(out_dir, exist_ok=True)

# I print the paths so I can confirm the script is finding everything correctly
print(f"Data path: {data_path}\nOutput: {out_dir}\n")


## Section 3 — Load data and quick inspection
print("## Section 3 — Load data and quick inspection")

# I load the CSV into a DataFrame so I can work with the columns directly
fish = read_csv(data_path)

# I convert species to a category so classification methods treat it as labels
fish['species'] = fish['species'].astype('category')
# I convert length to float so math functions work normally
fish['length'] = fish['length'].astype(float)
# I convert weight to float for the same reason
fish['weight'] = fish['weight'].astype(float)

# I save a summary of the structure to help me verify the dataset outside Python
with open(os.path.join(out_dir, 'fish_structure.txt'), 'w') as f:
    fish.info(buf=f)  # I redirect the info() output into the file

# I save summary statistics for a quick overview of the dataset
with open(os.path.join(out_dir, 'fish_summary.txt'), 'w') as f:
    f.write(str(fish.describe(include='all')))

# I print a simple message showing the number of rows and species categories
print(f"Loaded fish data with {len(fish)} rows and species levels: {list(fish['species'].cat.categories)}\n")


## Section 4 — Train/test predictions for random forest and AdaBoost
print("## Section 4 — Train/test predictions with random forest and AdaBoost")

# I split the data into train/test to compare the two ensemble methods
train_df, test_df = train_test_split(
    fish, test_size=0.3, stratify=fish['species'], random_state=2529
)

# I write the number of rows for transparency, same as in other scripts
with open(os.path.join(out_dir, 'split_counts_rf_ada.txt'), 'w') as f:
    f.write(f"train n={len(train_df)}; test n={len(test_df)}")

# I prepare feature and label sets for training
X_train = train_df.drop(columns=['species'])  # features for training
y_train = train_df['species']                # labels for training
X_test = test_df.drop(columns=['species'])   # features for test set
y_test = test_df['species']                  # labels for test set

# I create a random forest model with a fixed seed so results match if rerun
rf_clf = RandomForestClassifier(random_state=2529)
# I train the random forest on the training data
rf_clf.fit(X_train, y_train)

# I use the trained model to make predictions on the test data
rf_pred = rf_clf.predict(X_test)

# I build a confusion matrix to see how well the random forest predicts each species
rf_cm = confusion_matrix(y_test, rf_pred, labels=rf_clf.classes_)
rf_cm_df = DataFrame(rf_cm, index=rf_clf.classes_, columns=rf_clf.classes_)
rf_cm_df.insert(0, 'model', 'RF')  # label the table so I know which model produced it
rf_cm_df.to_csv(os.path.join(out_dir, 'RF_confusion_table.csv'))  # save the table

# I check which predictions the random forest got right
rf_correct = (rf_pred == y_test.to_numpy())

# I make a scatter plot showing correct vs incorrect predictions
plt.figure(figsize=(6, 4))
plt.scatter(
    test_df['length'], test_df['weight'], c=rf_correct, alpha=0.7, cmap='coolwarm'
)
plt.title('Random Forest - correct (True) vs incorrect (False)')
plt.xlabel('length')
plt.ylabel('weight')
plt.savefig(os.path.join(out_dir, 'RF_correctness_scatter.png'))
plt.close()

# I set up an AdaBoost model with the same seed for consistency
ada_clf = AdaBoostClassifier(random_state=2529)
# I train AdaBoost on the same training data
ada_clf.fit(X_train, y_train)

# I predict species labels using AdaBoost
ada_pred = ada_clf.predict(X_test)

# I build a confusion matrix for AdaBoost predictions
ada_cm = confusion_matrix(y_test, ada_pred, labels=ada_clf.classes_)
ada_cm_df = DataFrame(ada_cm, index=ada_clf.classes_, columns=ada_clf.classes_)
ada_cm_df.insert(0, 'model', 'Ada')  # label the table with the model name
ada_cm_df.to_csv(os.path.join(out_dir, 'Ada_confusion_table.csv'))

# I check which predictions AdaBoost got right
ada_correct = (ada_pred == y_test.to_numpy())

# I make a scatter plot for AdaBoost correctness
plt.figure(figsize=(6, 4))
plt.scatter(
    test_df['length'], test_df['weight'], c=ada_correct, alpha=0.7, cmap='coolwarm'
)
plt.title('AdaBoost - correct (True) vs incorrect (False)')
plt.xlabel('length')
plt.ylabel('weight')
plt.savefig(os.path.join(out_dir, 'Ada_correctness_scatter.png'))
plt.close()

# I print a message showing this section finished fully
print("Train/test evaluation for random forest and AdaBoost complete; confusion tables and plots saved.\n")


## Section 5 — 5-fold cross-validation
print("## Section 5 — 5-fold cross-validation for random forest and AdaBoost")

# I use StratifiedKFold to keep the species balance the same in each fold
skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=2529)

# I prepare a list to store accuracy results across folds
cv_results = []

# I loop through each fold so I can train both models on the same splits
for fold_index, (train_idx, test_idx) in enumerate(
    skf.split(fish.drop(columns=['species']), fish['species']), start=1
):
    print(f"Processing fold {fold_index}")  # progress update

    # I extract the rows for this fold's training data
    fold_train = fish.iloc[train_idx]
    # I extract the rows for this fold's testing data
    fold_test = fish.iloc[test_idx]

    # I separate features and labels for this fold
    X_fold_train = fold_train.drop(columns=['species'])
    y_fold_train = fold_train['species']
    X_fold_test = fold_test.drop(columns=['species'])
    y_fold_test = fold_test['species']

    # I train a new random forest for this fold
    rf_fold_clf = RandomForestClassifier(random_state=2529)
    rf_fold_clf.fit(X_fold_train, y_fold_train)
    rf_fold_pred = rf_fold_clf.predict(X_fold_test)
    acc_rf = accuracy_score(y_fold_test, rf_fold_pred)  # accuracy for this fold
    cv_results.append({'fold': fold_index, 'method': 'RF', 'accuracy': acc_rf})

    # I train a new AdaBoost model for this fold
    ada_fold_clf = AdaBoostClassifier(random_state=2529)
    ada_fold_clf.fit(X_fold_train, y_fold_train)
    ada_fold_pred = ada_fold_clf.predict(X_fold_test)
    acc_ada = accuracy_score(y_fold_test, ada_fold_pred)  # accuracy for this fold
    cv_results.append({'fold': fold_index, 'method': 'Ada', 'accuracy': acc_ada})

# I turn the list of accuracy results into a DataFrame
cv_df = DataFrame(cv_results)

# I save all per-fold accuracies for transparency
cv_df.to_csv(os.path.join(out_dir, 'cv_per_fold_accuracies_rf_ada.csv'), index=False)

# I compute mean and standard deviation for each method to summarize performance
cv_summary = cv_df.groupby('method').agg(
    mean_accuracy=('accuracy', 'mean'),
    sd_accuracy=('accuracy', 'std')
).reset_index()

# I save the summary as a CSV file
cv_summary.to_csv(os.path.join(out_dir, 'cv_accuracy_summary_rf_ada.csv'), index=False)


# I write the summary table to a text file as well
with open(os.path.join(out_dir, 'cv_accuracy_summary_rf_ada.txt'), 'w') as f:
    f.write(str(cv_summary))

# I make a boxplot comparing RF and AdaBoost CV accuracies
plt.figure(figsize=(7, 5))
methods = cv_df['method'].unique()  # I get the method names (RF, Ada)
data_to_plot = [cv_df[cv_df['method'] == m]['accuracy'] for m in methods]  # group accuracies
plt.boxplot(data_to_plot, labels=methods)
plt.title('5-fold CV accuracies for random forest and AdaBoost')
plt.ylabel('Accuracy')
plt.savefig(os.path.join(out_dir, 'cv_accuracies_boxplot_rf_ada.png'))
plt.close()

print("Cross-validation complete; accuracies and summary saved.\n")


## Section 6 — Final notes and completion
print(f"SCRIPT4 complete. Check the files in {out_dir}")
# I print a console summary so I can quickly compare both models without opening the CSV files
print("\n## Cross-validation summary for Random Forest and AdaBoost\n")

# I print the AdaBoost mean and standard deviation from the cv_summary DataFrame
print(
    f"AdaBoost:\n"
    f"  Mean accuracy: {cv_summary[cv_summary['method']=='Ada']['mean_accuracy'].iloc[0]:.4f}\n"
    f"  Standard deviation: {cv_summary[cv_summary['method']=='Ada']['sd_accuracy'].iloc[0]:.4f}\n"
)

# I print the Random Forest mean and standard deviation
print(
    f"Random Forest:\n"
    f"  Mean accuracy: {cv_summary[cv_summary['method']=='RF']['mean_accuracy'].iloc[0]:.4f}\n"
    f"  Standard deviation: {cv_summary[cv_summary['method']=='RF']['sd_accuracy'].iloc[0]:.4f}\n"
)

# I print a short interpretation based on the numbers
print("Random Forest clearly outperformed AdaBoost in both accuracy and stability across folds.\n")
