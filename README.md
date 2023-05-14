# Credit_Card_Fraud_Prediction
The Credit Card Fraud Detection problem is about finding out which credit card transactions are fake and which are real. The aim is to create a computer program that can tell the difference accurately.
The dataset is taken from Kaggle Competition — Credit Card Fraud Detection. The dataset contains transactions made by credit cards in September 2013 by European cardholders. This dataset presents transactions that occurred in two days, where we have 492 frauds out of
284,807 transactions. The dataset is highly unbalanced, the positive class (frauds) account for 0.172% of all transactions.

working:
1.Credit card dataset is read and converted to a data.table in R
2.The dataset's structure and summary are examined, and basic descriptive statistics are calculated
3.Missing values and class imbalance are checked, and class imbalance
4.percentage is calculated
5.Class variable is visualized using a bar plot, and correlations between
6.variables are visualized using a correlation matrix and heatmap
7.Time and amount of transaction are plotted using histograms and box plots,by class
8.Dataset is split into training and testing sets
9.Models are built using resampling techniques such as downsampling, upsampling, and ROSE on the training set
10.A decision tree model is built on downsampled, upsampled, and ROSE data
11.Model performance is evaluated using the area under the ROC curve (AUC)
