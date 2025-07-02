<h1> Overview: </h1>
This Data Science App takes the dataset (.csv) at https://www.kaggle.com/datasets/rakeshrau/social-network-ads, and allows the end user to run different types of classification algorithms and view the results. It allows the dataset to be split between training and test data and outputs the Classification Graphs and the confusion matrix.

<h1> How to Run </h1>
1.	Use the Training/Test Ratio Slide Bar to determine the amount of the dataset that should be training vs test.

2.	Enter a seed (this is for reproducibility)
  
3.	Pick a Classification Algorithm (Further explanation can be found below)
   
4.	Optional : You can run inference on the algorithm you just trained. Enter Age, Estimated Salary and click “Run Inference”

<h1> Output Panels </h1>
1.	Selected Algorithms Results on Training set. This is the graph of the trained model on the training data. You can see the predicated and the actual values. (Green dots in the predicated class show models incorrectness or confusion

2.	Selected Algorithms Results on Test set. This is the graph of the trained model on the test data. You can see the predicated and the actual values. (Green dots in the predicated class show models incorrectness or confusion
   
3.	Confusion Matrix. This 4 box graph totals the amount of points in each class. You can determine correctness and incorrectness.
   
4.	Raw Data. This is the Social Network Ads.csv file
  
5.	Help: This documentation. the ui.R file provides a link to this Readme

<h1> Type of Classification:</h1>

![image](https://github.com/user-attachments/assets/f2d327a6-bdb3-4bd7-913d-2fb7517e6958)

<h2> Logistic Regression </h2>
1.	Logistic Regression --> (https://en.wikipedia.org/wiki/Logistic_regression). Logistic regression is a supervised machine learning algorithm widely used for binary classification tasks. It will predict a categorical dependent variable from a number of independent variables. 
<br> </br>
R documentation for the glm(family = binomial) command - https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/glm

<h2> K-Nearest Neighbors </h2>
2.	K-Nearest Neighbors  (https://www.geeksforgeeks.org/machine-learning/k-nearest-neighbours/)  In classification, KNN’s goal is to predict the class of a new data point based on the classes of its nearest neighbors. Choose the number K of neighbors, take the K nearest neighbors of the new data point from the Euclidean distance. Among the K nearest neighbors count the number of data points in each category, assign the new data point to the category where you counted the most neighbors.
<br> </br>
R documentation for the KNN command - https://www.rdocumentation.org/packages/class/versions/7.3-23/topics/knn

<h2> SVM </h2>
3.	Support Vector Machines (SVM)  https://www.geeksforgeeks.org/machine-learning/support-vector-machine-algorithm. SVM tries to find the hyperplane that best separates two classes by maximizing the margin between them. This margin is the distance from the hyperplane to the nearest data points on each side. The Kernel SVM (https://www.geeksforgeeks.org/machine-learning/major-kernel-functions-in-support-vector-machine-svm/) allows the SVM to find non-linear decision boundaries. 
<br> </br>
R documentation for the SVM command - https://www.rdocumentation.org/packages/e1071/versions/1.7-16/topics/svm

Kernal Type = Linear, Poly, Radial

Cost -  cost of constraints violation (default: 1)---it is the ‘C’-constant of the regularization term in the Lagrange formulation.

Degree for Polynomial only (parameter needed for kernel of type polynomial (default: 3)

Gamma - spread" or "width" of the influence of a single training data point. A high gamma value means that each training data point has a very limited range of influence. A high gamma value means that each training data point has a very limited range of influence. A low gamma value means that each training data point has a very wide range of influence. Data points that are relatively far away from a support vector can still be significantly affected by it.

<h2> Naive Bayes </h2>
4.	Naïve Bayes  https://www.geeksforgeeks.org/machine-learning/naive-bayes-classifiers/
 Uses Bayes Theorem, The probability of an event, given knowledge of prior conditions related to the event. In a classification, it calculate the probability of a data point belonging to a certain class, given its features.
The formula for Bayes' Theorem is:  P(A∣B)=P(B)P(B∣A)⋅P(A)
<br> </br>
R documentation for the naiveBayes Command in R - https://www.rdocumentation.org/packages/e1071/versions/1.7-16/topics/naiveBayes

<h2> Decision Tree </h2>
5.	Decision Tree  https://www.geeksforgeeks.org/machine-learning/decision-tree/
A tree built recursively splitting the data in features. Rpart comman build a single tree. Start with all possibilities in first node. From there, the tree branches out into different possibilities based on features in the data. In out example the algorithm looks at all features and finds the one that best separates (Buy and No Buy)
<br> </br>
R Documentation for the rpart command in R - https://www.rdocumentation.org/packages/rpart/versions/4.1.24/topics/rpart

<h2> Random Forest </h2>
6.	Random Forest - https://www.geeksforgeeks.org/machine-learning/random-forest-algorithm-in-machine-learning/	- Creates making decision trees to determine classification. 
<br> </br>
R Documentation for the randomForest comment in R - https://www.rdocumentation.org/packages/randomForest/versions/4.7-1.2/topics/randomForest

Ntree - number of trees grown – This is not a large dataset so let’s assume 10-100

Mtry - number of predictors sampled for spliting at each node. Specifies the number of predictor variables that are randomly sampled as candidates at each split when a decision tree is being grown within the forest. Since out data has age and salary the mtry is either 1 or 2.

References : https://www.udemy.com/course/machinelearning/?srsltid=AfmBOor2kUy218t7T7zdmvP_Gbune3CQ4xZEwL64Dr8Q0kIyhDUOs6qD
             https://gemini.google.com/app/
