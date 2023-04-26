import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import tensorflow as tf
import matplotlib.pyplot as plt


# Load the dataset from the CSV file
data = pd.read_csv('corn_data_new.csv')

#Clean dataset by removing categorical data like columns 'Week.Ending' and 'State'
data = data.drop(['Week.Ending', 'State'], axis=1)

#Remove rows with missing values
data = data.dropna()

# Split the dataset into training and test sets
train_data, test_data, train_labels, test_labels = train_test_split(
    data.drop('crop_quality', axis=1),
    data['crop_quality'],
    test_size=0.2,
    random_state=42)

# Scale the input features
scaler = StandardScaler()
train_data = scaler.fit_transform(train_data)
test_data = scaler.transform(test_data)

# Build the neural network model
model = tf.keras.models.Sequential([
    tf.keras.layers.Dense(64, activation='relu', input_shape=(train_data.shape[1],)),
    tf.keras.layers.Dense(64, activation='relu'),
    tf.keras.layers.Dense(1, activation='linear')
])

model.compile(optimizer='adam', loss='mean_squared_error')

# Train the model on the training set
history = model.fit(train_data, train_labels, epochs=100, validation_split=0.2)

# Evaluate the model on the test set
test_loss = model.evaluate(test_data, test_labels)

print('Test Loss:', test_loss)

#Caculate training error
train_loss = model.evaluate(train_data, train_labels)

print('Train Loss:', train_loss)

# Get the predicted values on the test set
test_predictions = model.predict(test_data).flatten()

# Plot the predicted values vs actual for the test set
plt.scatter(test_labels, test_predictions)
plt.xlabel('True Values')
plt.ylabel('Predictions')
plt.axis('equal')
plt.xlim(plt.xlim())
plt.ylim(plt.ylim())
plt.plot([-100, 100], [-100, 100], 'k--')
plt.show()

# Plot the loss vs epochs
plt.plot(history.history['loss'], label='loss')
plt.xlabel('Epoch')
plt.ylabel('Loss')
plt.legend()
plt.show()
