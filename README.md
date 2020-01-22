# Can Big Data Predict Revolutions? An Investigation in R
* Public dataset from the Cline Center at the University of Illinois, listing details of all known coups d'etat from the 1940s through 2004
* Data is almost all "one-hot encoded" as binary values (1 or 0) describing categorical variables
* A second dataset consisting of country-by-country global economic history data was merged 
* Begins by constructing new variables, e.g., decades, season
* Exploratory data analysis with correlations and visualizations
* Two R scripts were created using only the Cline Center coup dataset to predict the following outcomes:
 ** The success or failure of attempted coups
 ** The violence or lack thereof of attempted coups
* One additional R script was created to predict these outcomes:
 ** Coup occurence based on country-by-country economic indicators over time
 **  ** Coup violence and coup success, as in the first two scripts, using both economic data and coup features 
* Numerous machine learning algorithms were used to model these outcomes
 ** Logistic regression
 ** Stepwise forward selection of features for logistic regression
 ** Naive Bayes classification
 ** Decision trees classification
 ** Random Forest ensemble learning
* The correlation matrix for the success of coups is visualized as follows:
<img src="Success_Corrs.jpeg" width="500">
