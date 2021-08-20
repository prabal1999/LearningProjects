import pandas as pd
import numpy as np
import sklearn 
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_squared_error
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score, f1_score

df = pd.read_csv("FlightDelays.csv")      				#reads the flight delay csv file
df["DEP_DELAY"] = df["DEP_TIME"] - df["CRS_DEP_TIME"]
df["Flight Status"] = np.where(df["Flight Status"] == "ontime",0 ,1)

x1 = pd.get_dummies(df.ORIGIN)
x2 = pd.get_dummies(df.DEST)
x3 = pd.get_dummies(df.CARRIER)
x4 = pd.get_dummies(df.FL_NUM)
x6 = pd.get_dummies(df.Weather)
x7 = pd.get_dummies(df.TAIL_NUM)
x8 = pd.get_dummies(df.DAY_WEEK)
x9 = pd.get_dummies(df.DAY_OF_MONTH)
x10 = df[["DISTANCE","DEP_DELAY"]]

x10 = x10.apply(lambda x: (x - x.min(axis=0))/(x.max(axis=0) - x.min(axis=0)))

x = pd.concat([x1,x2,x3,x4,x6,x7,x8,x9,x10], axis=1)
y = df[["Flight Status"]]

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.4)
model = LogisticRegression()
model.fit(x_train, y_train)

y_pred = model.predict(x_test)
acc = model.score(x_test,y_test)
RMSE = mean_squared_error(y_pred,y_test)
cm = confusion_matrix(y_test,y_pred)
f1 = f1_score(y_test, y_pred)

print("Accuracy of the model is : ",acc)
print("RMSE value is : ",RMSE)
print("f1_score is : ",f1)
print("confusin matrix for the model is : ",cm)
