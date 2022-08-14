# importing staf
import pandas as pd
from sklearn.tree import DecisionTreeClassifier # Import Decision Tree Classifier
from sklearn.model_selection import train_test_split # Import train_test_split function
from sklearn import metrics #Import scikit-learn metrics module for accuracy calculation
# importing the data
df = pd.read_csv("prep_data.csv")
# Spliting into features and target variable
X=df.drop(['completed_ratio'],axis=1)
y=df.completed_ratio
# spliting into train and test
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=1) # 80% training and 20% test
# training the model
clf = DecisionTreeClassifier(max_depth=depth)
clf = clf.fit(X_train,y_train)
# predict for test
y_pred = clf.predict(X_test)
