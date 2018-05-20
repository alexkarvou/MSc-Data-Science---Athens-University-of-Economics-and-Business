# -*- coding: utf-8 -*-

import numpy as np
import os
import zipfile
import csv
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.linear_model import LogisticRegression
from scipy.sparse import lil_matrix

# Read subfolders of the "train_webpages" folder (subfolders correspond to categories)
categories = os.listdir('train_webpages')

# Create a dictionary that maps webpages to the categories they belong
train_labels = {}
for category in categories:
	webpages = os.listdir('train_webpages/'+category)
	for webpage in webpages:
		train_labels[webpage] = category

# Create a numpy array containing the categories of the training examples
y_train = np.array(train_labels.values())

# Create a dictionary that maps each category to an integer
label2index = {}
y_unique = np.unique(y_train)
for i in range(y_unique.size):
	label2index[y_unique[i]] = len(label2index)

# Store the textual content of each webpage of the training set into the dictionary "text_train"
text_train = {}
for webpage in train_labels:
	zipfilenames = os.listdir('train_webpages/'+train_labels[webpage]+"/"+webpage)
	for zipfilename in zipfilenames:
		with zipfile.ZipFile('train_webpages/'+train_labels[webpage]+"/"+webpage+"/"+zipfilename) as z:
			text = ''
			for filename in z.namelist():
				if not os.path.isdir(filename):
					with z.open(filename) as f:
						for line in f:
							text += line.decode('utf16')
			
			text_train[webpage] = text
	
# Create the training matrix. Each row corresponds to a webpage and each column to a word present in at least 10 webpages 
# and at most 50 webpages. The value of each entry in a row is equal to the frequency of that word in the corresponding
# webpage		
vec = CountVectorizer(decode_error='ignore', strip_accents='unicode', min_df=10, max_df=50, stop_words='english')
X_train = vec.fit_transform(text_train.values())

# Read webpages of the test set
test_webpages = os.listdir('test_webpages')

# Store the textual content of each webpage of the test set into the dictionary "text_test"
text_test = []
for i in range(len(test_webpages)):
	zipfilenames = os.listdir('test_webpages/'+test_webpages[i])
	for zipfilename in zipfilenames:
		with zipfile.ZipFile('test_webpages/'+test_webpages[i]+"/"+zipfilename) as z:
			text = ''
			for filename in z.namelist():
				if not os.path.isdir(filename):
					with z.open(filename) as f:
						for line in f:
							text += line.decode('utf16')
			
			text_test.append(text)

# Create the test matrix following the same approach as in the case of the training matrix
X_test = vec.transform(text_test)

print "Train matrix dimensionality: ", X_train.shape
print "Test matrix dimensionality: ", X_test.shape

# Use logistic regression to classify the webpages of the test set
clf = LogisticRegression()
clf.fit(X_train, y_train)
y_pred = clf.predict(X_test)

# Write predictions to a file
with open('sample_submission.csv', 'wb') as csvfile:
	writer = csv.writer(csvfile, delimiter=',')
	writer.writerow(["Webpage","Category"])
	for i in range(len(test_webpages)):
		writer.writerow([test_webpages[i], y_pred[i]])

