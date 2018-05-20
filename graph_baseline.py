# -*- coding: utf-8 -*-

import networkx as nx
import numpy as np
import os
import csv
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import f1_score

# Create a weighted directed graph
G = nx.read_weighted_edgelist('edgelist.csv', delimiter='\t', create_using=nx.DiGraph())

print ("Nodes: ", G.number_of_nodes())
print ("Edges: ", G.number_of_edges())

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
cd C:/Users/karvo/Desktop/aueb/data Science Challenge/Proj_data
# Create a dictionary that maps each category to an integer
label2index = {}
y_unique = np.unique(y_train)
for i in range(y_unique.size):
	label2index[y_unique[i]] = len(label2index)

# Create the training matrix. Its rows correspond to the webpages of the training set
# and its columns to the 12 categories. Given a webpage, each column is set equal to the
# number of neighbors (from the graph) of the webpage that belong to the category mapped 
# to that column		
X_train = np.zeros((len(train_labels), len(label2index)))
i = 0
for webpage in train_labels:
	for neighbor in G.neighbors(webpage):
		if neighbor in train_labels:
			X_train[i, label2index[train_labels[neighbor]]] += 1
	i += 1

# Read webpages of the test set
test_webpages = os.listdir('test_webpages')

# Create the test matrix in the same way as in the case of the training matrix
X_test = np.zeros((len(test_webpages), len(label2index)))
for i in range(len(test_webpages)):
	for neighbor in G.neighbors(test_webpages[i]):
		if neighbor in train_labels:
			X_test[i, label2index[train_labels[neighbor]]] += 1

print ("Train matrix dimensionality: ", X_train.shape)
print ("Test matrix dimensionality: ", X_test.shape)

# Use logistic regression to classify the webpages of the test set
clf = LogisticRegression()
clf.fit(X_train, y_train)
y_pred = clf.predict(X_test)

# Write predictions to a file
with open('sample_submission.csv', 'w') as csvfile:
	writer = csv.writer(csvfile, delimiter=',')
	writer.writerow(["Webpage","Category"])
	for i in range(len(test_webpages)):
		writer.writerow([test_webpages[i], y_pred[i]]
