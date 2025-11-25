## MaximillianBalter_Unit2_Part3_BIOL672.py
# Author: Maximillian Balter
# OS: macOS
# Libraries/modules used: os, sys, subprocess, time, pandas, numpy,
#    sklearn (model_selection, preprocessing, metrics, neural_network),
#    matplotlib, tensorflow, keras
# Input data files used: Unit_2/data/fish_data.csv
# Output files generated: MLP_confusion_table.csv, MLP_correctness_scatter.png,
#    deepNN_performance_layers.csv, deepNN_performance_plot.png,
#    deepNN_speed_cores.csv, deepNN_speed_plot.png
# Script sections:
#   1. Setup and libraries
#   2. Hardcoded data and output paths
#   3. Load data and quick check
#   4. Neural network classification to compare with SVM
#   5. Deep learning on a larger dataset using Keras, as required
#   6. Timing deep learning models with different CPU cores, as required
#   7. Wrap up


## Section 1 — Setup and libraries
print("UNIT 2: SCRIPT3 - NEURAL NETWORKS")
print("====================================================\n")

# I keep a list of module names so I can check that everything I need is available
modules = [
    'os', 'sys', 'subprocess', 'time', 'pandas', 'numpy',
    'sklearn', 'matplotlib', 'tensorflow', 'keras'
]

# I loop through each module name so I can import it or install it if it is missing
for mod in modules:
    try:
        globals()[mod] = __import__(mod)  # I try to import the module and store it in globals so I can use it later
    except ImportError:
        print(f"Module {mod} not found, attempting to install...")  # I let myself know that this module is not installed yet
        try:
            subprocess.check_call([sys.executable, '-m', 'pip', 'install', mod])  # I ask pip to install the missing module
        except Exception as e:
            print(f"Failed to install {mod}: {e}")  # I record if the installation step did not work
        globals()[mod] = __import__(mod)  # I try importing the module again after attempting installation

# I pull in only the parts of pandas that I actually use for data tables and reading CSVs
from pandas import DataFrame, read_csv
# I pull in some numpy helpers in case I need arrays or argmax behavior
from numpy import array, argmax
# I import the train/test splitter so I can split my fish data into training and testing sets
from sklearn.model_selection import train_test_split
# I import tools to scale features and turn labels into numbers for the models
from sklearn.preprocessing import StandardScaler, LabelEncoder
# I import confusion_matrix so I can measure how well the classifier did on each class
from sklearn.metrics import confusion_matrix
# I import the MLPClassifier as my simple artificial neural network model for the SVM comparison
from sklearn.neural_network import MLPClassifier
# I import matplotlib.pyplot so I can make the required plots for the assignment
import matplotlib.pyplot as plt

# I import tensorflow and keras pieces so I can build deeper neural networks for the large dataset part
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
from tensorflow.keras.utils import to_categorical

# I import numpy under its short name so I can set its seed
import numpy
# I set the numpy random seed so my data-related randomness stays the same when I rerun the script
numpy.random.seed(2529)
# I set the tensorflow seed so my deep learning results do not change every time I run the script
tf.random.set_seed(2529)


## Section 2 — Locate data and prepare output dir
print("## Section 2 — Locate data and prepare output dir")

# I import os here so I can work with file paths and directories for reading and writing files
import os
# I build the full path to my fish_data.csv file inside Unit_2/data so this script always knows where to look
data_path = os.path.join(os.getcwd(), 'Unit_2', 'data', 'fish_data.csv')
# I build the path to my Unit_2/output folder where all output files for this script will go
out_dir = os.path.join(os.getcwd(), 'Unit_2', 'output')

# I check if the output directory already exists
if not os.path.isdir(out_dir):
    os.makedirs(out_dir, exist_ok=True)  # I create the output directory so the script can save plots and CSVs there

# I print the paths so I can confirm in the console that the script is looking in the right places
print(f"Data path: {data_path}\nOutput: {out_dir}\n")


## Section 3 — Load data and quick inspection
print("## Section 3 — Load data and quick inspection")

# I read the fish data CSV into a pandas DataFrame so I can work with rows and columns easily
fish = read_csv(data_path)

# I convert species to a category type so that the model and summaries treat it as a group label
fish['species'] = fish['species'].astype('category')
# I convert the length column to floats to make sure calculations and scaling work cleanly
fish['length'] = fish['length'].astype(float)
# I convert the weight column to floats for the same reason
fish['weight'] = fish['weight'].astype(float)

# I open a text file to record the structure (column types, counts) of the fish DataFrame
with open(os.path.join(out_dir, 'fish_structure.txt'), 'w') as f:
    fish.info(buf=f)  # I send the DataFrame info output into that file so I can inspect it later
# I open another text file for a numeric summary of the fish data
with open(os.path.join(out_dir, 'fish_summary.txt'), 'w') as f:
    f.write(str(fish.describe(include='all')))  # I write full summary statistics so I can see ranges and distributions

# I print how many rows and which species levels I have so I can quickly check the data that will be used in the models
print(f"Loaded fish data with {len(fish)} rows and species levels: {list(fish['species'].cat.categories)}\n")


## Section 4 — Neural network classification to compare with SVM
print("## Section 4 — Neural network classification to compare with SVM")

# I split the data into training and testing sets so I can train the neural net and then fairly test it, matching the SVM setup
train_df, test_df = train_test_split(
    fish, test_size=0.3, stratify=fish['species'], random_state=2529
)

# I open a text file to record how many rows went into the training and test sets
with open(os.path.join(out_dir, 'split_counts_mlp.txt'), 'w') as f:
    f.write(f"train n={len(train_df)}; test n={len(test_df)}")  # I store the counts so I can report them in my writeup if needed

# I drop the species column from the training data to create the feature matrix X for training
X_train = train_df.drop(columns=['species'])
# I take the species column from the training data to use as the target labels y for training
y_train = train_df['species']
# I drop the species column from the test data to create the feature matrix X for testing
X_test = test_df.drop(columns=['species'])
# I take the species column from the test data as the target labels y for testing
y_test = test_df['species']

# I set up a scaler so I can standardize numeric features before sending them into the neural network
scaler = StandardScaler()
# I fit the scaler on the training features and transform them so the network sees standardized values during training
X_train_scaled = DataFrame(scaler.fit_transform(X_train), columns=X_train.columns)
# I apply the same scaling to the test features so they are on the same scale as the training data
X_test_scaled = DataFrame(scaler.transform(X_test), columns=X_test.columns)

# I build a small neural network with one hidden layer to act like the simple neuralnet() model requested in the assignment
mlp = MLPClassifier(hidden_layer_sizes=(5,), max_iter=200, random_state=2529)
# I train this neural network on the scaled training data so it can learn the mapping from measurements to species
mlp.fit(X_train_scaled, y_train)

# I use the trained neural network to predict species labels for the scaled test features
mlp_pred = mlp.predict(X_test_scaled)
# I compute a confusion matrix so I can see how often the neural network got each species right or wrong
cm = confusion_matrix(y_test, mlp_pred, labels=mlp.classes_)
# I wrap the confusion matrix in a DataFrame and label the rows and columns with species names for readability
cm_df = DataFrame(cm, index=mlp.classes_, columns=mlp.classes_)
# I insert a column indicating that these results came from the MLP model, which helps if I later combine tables
cm_df.insert(0, 'model', 'MLP')
# I save the confusion table to the output folder so I can compare it to the SVM confusion table outside of Python
cm_df.to_csv(os.path.join(out_dir, 'MLP_confusion_table.csv'))

# I build a boolean array marking which test predictions were correct to use as colors in a scatter plot
correct = (mlp_pred == y_test.to_numpy())
# I start a new figure for the correctness scatter plot
plt.figure(figsize=(6, 4))
# I plot each test fish by its length and weight, coloring points by whether the neural net got them right or wrong
plt.scatter(
    test_df['length'], test_df['weight'], c=correct, alpha=0.7, cmap='coolwarm'
)
# I title the plot to show that True means correct predictions and False means mistakes
plt.title('MLP - correct (True) vs incorrect (False)')
# I label the x-axis with the length measurement
plt.xlabel('length')
# I label the y-axis with the weight measurement
plt.ylabel('weight')
# I save the plot so I can visually compare neural network performance versus the SVM later
plt.savefig(os.path.join(out_dir, 'MLP_correctness_scatter.png'))
# I close the figure to free up memory and keep plots from stacking up
plt.close()

# I print a short message so I know the basic neural network classification part is done
print('Neural network classification complete; confusion table and plot saved.\n')


## Section 5 — Deep learning on a larger dataset using Keras, as required
print('## Section 5 — Deep learning on a larger dataset using Keras, as required')

# I create a LabelEncoder so I can turn species names into integer codes for Keras
label_encoder = LabelEncoder()
# I fit the encoder on training labels and convert them to integer indices
y_train_int = label_encoder.fit_transform(y_train)
# I convert the test labels to the same integer index space using the fitted encoder
y_test_int = label_encoder.transform(y_test)
# I convert the integer training labels into one-hot rows so Keras can treat this as multi-class classification
y_train_onehot = to_categorical(y_train_int)
# I do the same one-hot conversion for the test labels
y_test_onehot = to_categorical(y_test_int)

# I define the different numbers of hidden layers I want to explore for the performance vs layers plot
layer_counts = [1, 2, 3, 4]

# I set up an empty list where I will store accuracy and time for each model depth
layer_results = []

# I loop over each layer count so I can build models with different depths and measure how they behave
for depth in layer_counts:
    # I capture the starting time so I can measure how long training and evaluation take for this depth
    start_time = time.time()
    # I start a new Keras Sequential model to add layers to
    model = Sequential()
    # I add the first hidden layer with 64 units and ReLU, matching input size to the number of scaled features
    model.add(Dense(64, activation='relu', input_shape=(X_train_scaled.shape[1],)))
    # I add extra hidden layers if the chosen depth is greater than one
    for _ in range(depth - 1):
        model.add(Dense(64, activation='relu'))
    # I add the output layer with one unit per species and softmax for multi-class probabilities
    model.add(Dense(len(label_encoder.classes_), activation='softmax'))
    # I compile the model with Adam and categorical crossentropy, which is standard for this kind of task
    model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])
    # I train the model for 10 epochs on the scaled training data and one-hot labels
    model.fit(X_train_scaled, y_train_onehot, epochs=10, batch_size=32, verbose=0)
    # I evaluate the model on the scaled test data to get loss and accuracy for this depth
    loss, accuracy = model.evaluate(X_test_scaled, y_test_onehot, verbose=0)
    # I compute how much time passed during training and evaluation
    elapsed = time.time() - start_time
    # I save the depth, accuracy, and time into my results list for later analysis and plotting
    layer_results.append({'depth': depth, 'accuracy': accuracy, 'time': elapsed})
    # I print a quick summary of how this depth performed and how long it took
    print(f"Trained Keras model with {depth} hidden layer(s): accuracy={accuracy:.4f}, time={elapsed:.2f}s")

# I convert the list of layer results into a DataFrame to make it easy to save and inspect
layer_df = DataFrame(layer_results)
# I save the performance vs depth information as a CSV so I can refer to it in my writeup
layer_df.to_csv(os.path.join(out_dir, 'deepNN_performance_layers.csv'), index=False)

# I start a new figure to make the required performance vs number of layers plot
plt.figure(figsize=(6, 4))
# I plot accuracy on the y-axis and number of hidden layers on the x-axis to match the assignment requirement
plt.plot(layer_df['depth'], layer_df['accuracy'], marker='o')
# I title this figure to clearly show it is accuracy vs hidden layers
plt.title('Accuracy vs number of hidden layers (Keras)')
# I label the x-axis with the number of hidden layers
plt.xlabel('Number of hidden layers')
# I label the y-axis with accuracy
plt.ylabel('Accuracy')
# I add a simple grid so the trend across layers is easier to see
plt.grid(True)
# I save the performance vs layer count plot into my output folder
plt.savefig(os.path.join(out_dir, 'deepNN_performance_plot.png'))
# I close the figure once I am done with it
plt.close()

# I print a message so I know the depth vs performance part of the assignment is finished
print('Layer sweep complete; performance CSV and plot saved.\n')


## Section 6 — Timing deep learning models with different CPU cores, as required
print('## Section 6 — Timing deep learning models with different CPU cores, as required')

# I ask the system how many CPU cores are available, falling back to 1 if the answer is None
available_cores = os.cpu_count() or 1
# I build a list of core counts from 1 up to either 4 or the number of cores, to keep the experiment small but meaningful
core_counts = list(range(1, min(available_cores, 4) + 1))
# I set up a list to store timing and accuracy for each core setting
core_results = []

# I loop over each core count so I can time how long training takes when I change the number of threads
for cores in core_counts:
    try:
        # I tell TensorFlow to limit how many threads it can use inside operations
        tf.config.threading.set_intra_op_parallelism_threads(cores)
        # I also tell TensorFlow how many threads it can use between operations
        tf.config.threading.set_inter_op_parallelism_threads(cores)
    except Exception as e:
        # I log a message if this version of TensorFlow does not let me change the thread settings
        print(f"Unable to set TensorFlow threads: {e}")

    # I mark the start time so I can measure how long this training run takes
    start = time.time()
    # I build a small model that I will use for timing across different core counts
    speed_model = Sequential()
    # I add a first hidden layer with 64 units and ReLU using the scaled feature size as input
    speed_model.add(Dense(64, activation='relu', input_shape=(X_train_scaled.shape[1],)))
    # I add a second hidden layer with 64 units to keep the model shape fixed across core tests
    speed_model.add(Dense(64, activation='relu'))
    # I add the output layer with one node per species class and softmax
    speed_model.add(Dense(len(label_encoder.classes_), activation='softmax'))
    # I compile the model with the same settings as before so only the core count changes
    speed_model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])
    # I train the model for a small number of epochs so that timing differences show up without taking too long
    speed_model.fit(X_train_scaled, y_train_onehot, epochs=5, batch_size=32, verbose=0)
    # I evaluate the model on the test set to record accuracy for this core count
    _, acc = speed_model.evaluate(X_test_scaled, y_test_onehot, verbose=0)
    # I compute how many seconds the training and evaluation took
    elapsed = time.time() - start

    # I store the core count, accuracy, and elapsed time in my results list
    core_results.append({'cores': cores, 'accuracy': acc, 'time': elapsed})
    # I print a summary so I can see how accuracy and time changed with this many cores
    print(f"Cores={cores}: accuracy={acc:.4f}, time={elapsed:.2f}s")

# I convert the core timing results into a DataFrame so I can save and inspect them
core_df = DataFrame(core_results)
# I save the speed vs cores table to a CSV for the assignment
core_df.to_csv(os.path.join(out_dir, 'deepNN_speed_cores.csv'), index=False)

# I start a figure to create the required plot of training time versus number of cores
plt.figure(figsize=(6, 4))
# I plot training time on the y-axis and number of CPU threads on the x-axis as asked in the instructions
plt.plot(core_df['cores'], core_df['time'], marker='o')
# I title the plot so it is clear that it shows training time versus CPU threads
plt.title('Training time vs number of CPU threads (Keras)')
# I label the x-axis with the number of CPU threads
plt.xlabel('Number of CPU threads')
# I label the y-axis with training time in seconds
plt.ylabel('Training time (seconds)')
# I add a grid to make it easier to see changes in time as cores increase
plt.grid(True)
# I save this speed vs cores figure into the output folder for later use in my writeup
plt.savefig(os.path.join(out_dir, 'deepNN_speed_plot.png'))
# I close the figure now that I am done with it
plt.close()

# I print a message so I know the timing experiments part is finished and saved
print('Speed evaluation complete; core timing CSV and plot saved.\n')


## Section 7 — Final notes and completion
# I print a final message so I know the script ran to the end and where to look for all the outputs
print(f'SCRIPT3 complete. Check the files in {out_dir}')