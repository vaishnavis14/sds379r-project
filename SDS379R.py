from google.colab import drive
drive.mount('/content/drive')
!pip install scikeras
import numpy as np
import pandas as pd
from keras.models import Sequential
from keras.layers import Dense
from keras.optimizers import Adam
import matplotlib.pyplot as plt
%cd /content/drive/'My Drive'/TensorFlow/
#define function to swap columns. Code from https://www.statology.org/swap-columns-pandas/
def swap_columns(df, col1, col2):
   col_list = list(df.columns)
   x, y = col_list.index(col1), col_list.index(col2)
   col_list[y], col_list[x] = col_list[x], col_list[y]
   df = df[col_list]
   return df
dataframe = pd.read_csv("housing_permits.csv", delimiter=",")
dataframe = swap_columns(dataframe, "avg_price_month_prev_year", "Sold/Lease Price")
dataframe["ln_prev_year"] = np.log(dataframe["avg_price_month_prev_year"])
dataframe = swap_columns(dataframe, "ln_prev_year", "avg_price_month_prev_year")
dataframe = swap_columns(dataframe, "Sqft Total", "List Date")
dataframe = swap_columns(dataframe, "Sqft Total", "natural_log")
dataframe = dataframe.dropna()
# split into input (X) and output (Y) variables
X = dataframe[dataframe.columns[8:53]]
Y = dataframe[dataframe.columns[7]]
from sklearn.model_selection import train_test_split
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=101)
model = Sequential()
model.add(Dense(40, input_dim=10, activation='relu', input_shape = (45,)))
model.add(Dense(30, activation='relu'))
model.add(Dense(20, activation='relu'))
model.add(Dense(10, activation='relu'))
model.add(Dense(1, activation = 'linear'))
model.compile(loss='mse', optimizer='Adam')
model.fit(X_train, Y_train, validation_data=(X_test, Y_test), epochs=150, batch_size = 32)
model.summary()
loss_df = pd.DataFrame(model.history.history)
loss_df
X_Test_2017 = dataframe[dataframe.columns[7:53]]
X_Test_2017 = X_Test_2017[X_Test_2017['Year'] == 2017]
Y_Test_2017 = X_Test_2017[X_Test_2017.columns[0]]
X_Test_2017 = X_Test_2017[X_Test_2017.columns[1:46]]

X_Test_2017_mod = X_Test_2017.copy()
X_Test_2017_mod[X_Test_2017_mod.columns[3:43]] = X_Test_2017_mod[X_Test_2017_mod.columns[3:43]] * 0.5
# average log value = 12.619514. Average home price = 302402.6
y_pred_2017 = model.predict(X_Test_2017)
np.mean(y_pred_2017)
np.exp(np.mean(y_pred_2017))
# average log value = 12.432438. Average home price = 250806.72
y_pred_2017_mod = model.predict(X_Test_2017_mod)
np.mean(y_pred_2017_mod)
np.exp(np.mean(y_pred_2017_mod))

plt.hist(y_pred_2017)
plt.xlabel("Natural Log of Predicted Selling Price")
plt.ylabel("Frequency")
plt.title("2017 Predicted Selling Price with Reduced Permits Data")
plt.show()
y_pred = model.predict(X_test)
from sklearn import metrics
print('MAE:', metrics.mean_absolute_error(Y_test, y_pred)) 
print('MSE:', metrics.mean_squared_error(Y_test, y_pred)) 
print('RMSE:', np.sqrt(metrics.mean_squared_error(Y_test, y_pred)))
print('VarScore:',metrics.explained_variance_score(Y_test,y_pred))
# Visualizing Our predictions
fig = plt.figure(figsize=(10,5))
plt.scatter(Y_test,y_pred)
plt.xlabel("Natural Log of Actual Selling Price")
plt.ylabel("Natural Log of Predicted Selling Price")
# Perfect predictions
plt.plot(Y_test,Y_test,'r')
X = dataframe[["Year Built", "Year", "ln_prev_year", "Sqft Total"]]
Y = dataframe[dataframe.columns[7]]
from sklearn.model_selection import train_test_split
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=101)
model_2 = Sequential()
model_2.add(Dense(40, input_dim=10, activation='relu', input_shape = (4,)))
model_2.add(Dense(30, activation='relu'))
model_2.add(Dense(20, activation='relu'))
model_2.add(Dense(10, activation='relu'))
model_2.add(Dense(1, activation = 'linear'))
model_2.compile(loss='mse', optimizer='Adam')
model_2.fit(X_train, Y_train, validation_data=(X_test, Y_test), epochs=150, batch_size = 32)
model_2.summary()
loss_df = pd.DataFrame(model_2.history.history)
loss_df.plot(figsize=(12,8))
y_pred = model_2.predict(X_test)
from sklearn import metrics
print('MAE:', metrics.mean_absolute_error(Y_test, y_pred)) 
print('MSE:', metrics.mean_squared_error(Y_test, y_pred)) 
print('RMSE:', np.sqrt(metrics.mean_squared_error(Y_test, y_pred)))
print('VarScore:',metrics.explained_variance_score(Y_test,y_pred))
# Visualizing Our predictions
fig = plt.figure(figsize=(10,5))
plt.scatter(Y_test,y_pred)
plt.xlabel("Natural Log of Actual Selling Price")
plt.ylabel("Natural Log of Predicted Selling Price")
# Perfect predictions
plt.plot(Y_test,Y_test,'r')
