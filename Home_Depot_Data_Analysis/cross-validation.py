
import pandas as pd
import numpy as np
import cPickle

from sklearn.cross_validation import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression
from sklearn.pipeline import Pipeline
from sklearn.svm import SVR
from sklearn.metrics import mean_squared_error

np.random.seed(1729)

with open('./data/synthesized/train_processed.pkl', 'r') as infile:    
    train = cPickle.load(infile)
    infile.close()

with open('./data/synthesized/test_processed.pkl', 'r') as infile:    
    test = cPickle.load(infile)
    infile.close()
    



X_train, X_test, y_train, y_test = train_test_split(corpus_svd, train.relevance, test_size=0.3,
                                                    random_state=44)


scaler = StandardScaler()
svr = SVR()

pipeline = Pipeline([('scaler', scaler), ('svr', svr)])
pipeline.fit(X_train, y_train)

predsTrain = pipeline.predict(X_train)
predsTest = pipeline.predict(X_test)

print 'RMSE on training examples %f ' %(np.sqrt(mean_squared_error(y_train, predsTrain)))
print 'RMSE on test examples %f ' %(np.sqrt(mean_squared_error(y_test, predsTest)))
