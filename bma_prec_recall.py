from mpmath import mp
import numpy as np
import pandas as pd
import statsmodels.api as sm
from geneticalgorithm import geneticalgorithm as ga
from statsmodels.tools import add_constant
from itertools import combinations
from sklearn.metrics import recall_score, precision_score, f1_score

mp.dps = 50

def precision(predictions, oracle):
    #accuracy = np.sum((pred_bma > 0.5) == y)/len(y)
    #true_positives = np.sum((predictions > 0.5) == oracle)
    #false_positives = np.sum((predictions > 0.5) != oracle)
    #return true_positives / (true_positives + false_positives)
    return precision_score(oracle, (predictions > 0.5))

def recall(predictions, oracle):
    #true_positives = np.sum((predictions > 0.5) == oracle)
    #false_negatives = np.sum((predictions < 0.4) != oracle)
    #return true_positives / (true_positives + false_negatives)
    return recall_score(oracle, (predictions > 0.5))

def f_measure(predictions, oracle):
    #p = precision(predictions, oracle)
    #r = recall(predictions, oracle)
    return f1_score(oracle, (predictions > 0.5))

class BMA:

    def __init__(self, y, X, **kwargs):
        self.y = y
        self.X = X
        self.names = list(X.columns)
        self.nRows, self.nCols = np.shape(X)
        self.likelihoods = mp.zeros(self.nCols,1)
        self.likelihoods_all = {}
        self.coefficients_mp = mp.zeros(self.nCols,1)
        self.coefficients = np.zeros(self.nCols)
        self.probabilities = np.zeros(self.nCols)
        # check the max model size (max number of predictor variables to use in a model)
        # this can be used to reduce the runtime but not doing an exhaustive sampling
        if 'MaxVars' in kwargs.keys():
            self.MaxVars = kwargs['MaxVars']
        else:
            self.MaxVars = self.nCols
        # prepare the priors if they are provided
        # the prior for a model is the product of the priors of the variables in the model
        if 'Priors' in kwargs.keys():
            if np.size(kwargs['Priors']) == self.nCols:
                self.Priors = kwargs['Priors']
            else:
                print("WARNING: Provided priors error.  Using equal priors instead.")
                print("The priors should be a numpy array of length equal tot he number of regressor variables.")
                self.Priors = np.ones(self.nCols)
        else:
            self.Priors = np.ones(self.nCols)
        if 'Verbose' in kwargs.keys():
            self.Verbose = kwargs['Verbose']
        else:
            self.Verbose = False
        if 'RegType' in kwargs.keys():
            self.RegType = kwargs['RegType']
        else:
            self.RegType = 'LS'

    def fit(self):
        # this will be the 'normalization' denominator in Bayes Theorem
        likelighood_sum = 0

        # forall number of elements in the model
        max_likelihood = 0
        for num_elements in range(1,self.MaxVars+1):

            if self.Verbose == True:
                print("Computing BMA for models of size: ", num_elements)

            # make a list of all index sets of models of this size
            Models_next = list(combinations(list(range(self.nCols)), num_elements))

            # Occam's window - compute the candidate models to use for the next iteration
            # Models_previous: the set of models from the previous iteration that satisfy (likelihhod > max_likelihood/20)
            # Models_next: the set of candidate models for the next iteration
            # Models_current: the set of models from Models_next that can be consturcted by adding one new variable to a model from Models_previous
            if num_elements == 1:
                Models_current = Models_next
                Models_previous = []
            else:
                idx_keep = np.zeros(len(Models_next))
                for M_new,idx in zip(Models_next,range(len(Models_next))):
                    for M_good in Models_previous:
                        if(all(x in M_new for x in M_good)):
                            idx_keep[idx] = 1
                            break
                        else:
                            pass
                Models_current = np.asarray(Models_next)[np.where(idx_keep==1)].tolist()
                Models_previous = []


            # iterate through all possible models of the given size
            for model_index_set in Models_current:

                # compute the regression for this given model
                model_X = self.X.iloc[:,list(model_index_set)]
                if self.RegType == 'Logit':
                    model_regr = sm.Logit(self.y, model_X).fit(disp=0)
                else:
                    model_regr = OLS(self.y, model_X).fit()

                # compute the likelihood (times the prior) for the model
                model_likelihood = mp.exp(-model_regr.bic/2)*np.prod(self.Priors[list(model_index_set)])

                if (model_likelihood > max_likelihood/20):
                    if self.Verbose == True:
                        print("Model Variables:",model_index_set,"likelihood=",model_likelihood)
                    self.likelihoods_all[str(model_index_set)] = model_likelihood

                    # add this likelihood to the running tally of likelihoods
                    likelighood_sum = mp.fadd(likelighood_sum, model_likelihood)

                    # add this likelihood (times the priors) for each variable in the model
                    for idx, i in zip(model_index_set, range(num_elements)):
                        self.likelihoods[idx] = mp.fadd(self.likelihoods[idx], model_likelihood, prec=1000)
                        self.coefficients_mp[idx] = mp.fadd(self.coefficients_mp[idx], model_regr.params[i]*model_likelihood, prec=1000)
                    Models_previous.append(model_index_set) # add this model to the list of good models
                    max_likelihood = np.max([max_likelihood,model_likelihood]) # get the new max likelihood if it is this model
                else:
                    if self.Verbose == True:
                        print("Model Variables:",model_index_set,"rejected by Occam's window")


        # divide by the denominator in Bayes theorem to normalize the probabilities sum to one
        self.likelighood_sum = likelighood_sum
        for idx in range(self.nCols):
            self.probabilities[idx] = mp.fdiv(self.likelihoods[idx],likelighood_sum, prec=1000)
            self.coefficients[idx] = mp.fdiv(self.coefficients_mp[idx],likelighood_sum, prec=1000)

        # BMA object as output
        return self

    def predict(self, data):
        data = np.asarray(data)
        if self.RegType == 'Logit':
            try:
                result = 1/(1+np.exp(-1*np.dot(self.coefficients,data)))
            except:
                result = 1/(1+np.exp(-1*np.dot(self.coefficients,data.T)))
        else:
            try:
                result = np.dot(self.coefficients,data)
            except:
                result = np.dot(self.coefficients,data.T)

        return result

    def summary(self):
        # Return the BMA results as a data frame for easy viewing.
        df = pd.DataFrame([self.names, list(self.probabilities), list(self.coefficients)],
             ["Variable Name", "Probability", "Avg. Coefficient"]).T
        return df


df = pd.read_csv('CHDdata.csv')
df["firm"] = (df["firm"] == "Yes")*1 # converts the famhit to 0 (no hist) and 1 (has hist)
#df = df.drop(["famhist"], axis=1)
#df.head()

# illuminance,smoke,color,dist,firm,power,band,speed,quality,hazard
DROP = ['color','dist','firm','speed','quality','hazard']
LIMIT = 400
LIMIT1 = 400

X_oracle = df.drop(["hazard"], axis=1)
X_oracle_drop = df.drop(DROP, axis=1)
y_oracle = df["hazard"]
X = df.drop(["hazard"], axis=1)[0:LIMIT]
y = df["hazard"][0:LIMIT]
#X1 = df.drop(DROP, axis=1)[0:LIMIT1]
#y1 = df["hazard"][0:LIMIT1]

# build oracle model
oracle = BMA(y_oracle, add_constant(X_oracle), RegType = 'Logit', Verbose=False).fit()
# 'a priori' model
#log_reg = sm.Logit(y1, add_constant(X1)).fit()
# print(log_reg.summary())
#pred_Logit = log_reg.predict(add_constant(X_oracle_drop))
# bma model
bma_reg = BMA(y, add_constant(X), RegType = 'Logit', Verbose=True).fit()
pred_bma = bma_reg.predict(add_constant(X_oracle))


# mechanical elicitation of all possible models
print( '=== Computation of the scores for each model ===' )
names = list(X.columns)
nRows, nCols = np.shape(X)
maxVars = nCols
for num_elements in range(1, maxVars+1):
    # make a list of all index sets of models of this size
    models_next = list(combinations(list(range(nCols)), num_elements))
    if num_elements == 1:
        models_current = models_next
        models_previous = []
    else:
        idx_keep = np.zeros(len(models_next))
        for m_new,idx in zip(models_next, range(len(models_next))):
            for m_good in models_previous:
                if(all(x in m_new for x in m_good)):
                    idx_keep[idx] = 1
                    break
                else:
                    pass
        models_current = np.asarray(models_next)[np.where(idx_keep==1)].tolist()
        models_previous = []

    # iterate through all possible models of the given size
    for model_index_set in models_current:
        if len(model_index_set) < maxVars:
            # compute the regression for this given model
            model_X = X.iloc[:,list(model_index_set)]
            model_regr = sm.Logit(y, model_X).fit(disp = 0)
            models_previous.append(model_index_set)
            pred_Logit = model_regr.predict(model_X)
            print('Logit ' + str(model_index_set) + ' Vars: ' + str(len(model_index_set)) + ' precision = ' + str(precision(pred_Logit, y)) + ' recall = ' + str(recall(pred_Logit, y)) + ' F1 = ' + str(f_measure(pred_Logit, y)))


print('BMA: precision = ' + str(precision(pred_bma, y_oracle)) + ' recall = ' + str(recall(pred_bma, y_oracle)) + ' F1 = ' + str(f_measure(pred_bma, y_oracle)))
