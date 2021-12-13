import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O
import seaborn as sns
import matplotlib.pyplot as plt
from statsmodels.regression.linear_model import OLS
import statsmodels.api as sm
from statsmodels.tools import add_constant
from itertools import combinations


class BMA:

    def __init__(self, y, X, **kwargs):
        self.y = y
        self.X = X
        self.names = list(X.columns)
        self.nRows, self.nCols = np.shape(X)
        self.likelihoods = np.zeros(self.nCols)
        self.coefficients = np.zeros(self.nCols)
        self.probabilities = np.zeros(self.nCols)

        # check max model size (max number of predictor variables in a model)
        # can be used to reduce the runtime but not doing an exhaustive sampling
        if 'MaxVars' in kwargs.keys():
            self.MaxVars = kwargs['MaxVars']
        else:
            self.MaxVars = self.nCols

        # prepare priors (for individual regressor variables) if provided
        # the prior for a model is the product of the priors on the variables in the model
        if 'Priors' in kwargs.keys():
            if np.size(kwargs['Priors']) == self.nCols:
                self.Priors = kwargs['Priors']
            else:
                print("WARNING: Provided priors error.  Using equal priors instead.")
                print("The priors should be a numpy array of length equal tot he number of regressor variables.")
                self.Priors = np.ones(self.nCols)
        else:
            #default uninformative prior
            self.Priors = np.ones(self.nCols)

    def fit(self):
        # the sum of the likelihoods is the 'normalization' denominator in Bayes theorem
        likelihood_sum = 0

        for num_elements in range(1,self.MaxVars+1):
            # make a list of all index sets of models of size 'num_elements'
            model_index_sets = list(combinations(list(range(self.nCols)), num_elements))

            # iterate through all possible models of the given size
            for model_index_set in model_index_sets:
                # linear regression for the given model
                model_X = self.X.iloc[:,list(model_index_set)]
                # model_regr = OLS(self.y, model_X).fit()
                model_regr = sm.GLM(self.y, model_X, family=sm.families.Gamma()).fit()

                # compute likelihood (times the prior) for the model
                model_likelihood = np.exp(-model_regr.bic/2) * np.prod(self.Priors[list(model_index_set)])
                print("Model Variables:", model_index_set, "likelihood=", model_likelihood)
                likelihood_sum = likelihood_sum + model_likelihood

                # incremental computation of P(X_k) and E(\beta_k)
                for idx, i in zip(model_index_set, range(num_elements)):
                    self.likelihoods[idx] = self.likelihoods[idx] + model_likelihood
                    self.coefficients[idx] = self.coefficients[idx] + model_regr.params[i] * model_likelihood

        # divide by the denominator in Bayes theorem to normalize the probabilities sum to one
        self.probabilities = self.likelihoods / likelihood_sum
        self.coefficients = self.coefficients / likelihood_sum

        # BMA object
        return self

    def summary(self):
        # BMA results as a data frame
        df = pd.DataFrame([self.names, list(self.probabilities), list(self.coefficients)],
             ["Variable Name", "Probability", "Avg. Coefficient"]).T
        return df


#df = pd.read_csv('sample_data1.csv')
df = pd.read_csv('test_gen_data.csv')

#print(df.head())

#sns.pairplot(df, x_vars=["Light", "Smoke", "Quality", "Speed"], y_vars=["Dist"])
#plt.show()

#X1 = df[["Spend"]]
#X2 = df[["StuTeaRat"]]

#X = df[["Spend"]]
#y = df["SATT"]
#regr = OLS(y, add_constant(X)).fit()
#print(regr.summary())

#likelihood1 = np.exp(-regr.bic/2)
#print(np.log(likelihood1))
#print(regr.llf)


#X = df[["Spend", "PrcntTake"]]
#y = df["SATT"]
#regr = OLS(y, add_constant(X)).fit()
#likelihood2 = np.exp(-regr.bic/2)
#print(regr.summary())

#print(likelihood2/likelihood1)


#regr = OLS(y, add_constant(X2)).fit()
#print(regr.summary())


X = df[["Light", "Smoke", "Quality", "Speed"]]
y = df["Dist"]
result = BMA(y, add_constant(X)).fit()
print(result.summary())
