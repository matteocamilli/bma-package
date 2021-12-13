from mpmath import mp
import numpy as np
import pandas as pd
import statsmodels.api as sm
from geneticalgorithm import geneticalgorithm as ga
from statsmodels.tools import add_constant
from itertools import combinations

mp.dps = 50

class BMA:

    def __init__(self, y, X, **kwargs):
        # Setup the basic variables.
        self.y = y
        self.X = X
        self.names = list(X.columns)
        self.nRows, self.nCols = np.shape(X)
        self.likelihoods = mp.zeros(self.nCols,1)
        self.likelihoods_all = {}
        self.coefficients_mp = mp.zeros(self.nCols,1)
        self.coefficients = np.zeros(self.nCols)
        self.probabilities = np.zeros(self.nCols)
        # Check the max model size. (Max number of predictor variables to use in a model.)
        # This can be used to reduce the runtime but not doing an exhaustive sampling.
        if 'MaxVars' in kwargs.keys():
            self.MaxVars = kwargs['MaxVars']
        else:
            self.MaxVars = self.nCols
        # Prepare the priors if they are provided.
        # The priors are provided for the individual regressor variables.
        # The prior for a model is the product of the priors on the variables in the model.
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
        # Perform the Bayesian Model Averaging

        # Initialize the sum of the likelihoods for all the models to zero.
        # This will be the 'normalization' denominator in Bayes Theorem.
        likelighood_sum = 0

        # To facilitate iterating through all possible models, we start by iterating thorugh
        # the number of elements in the model.
        max_likelihood = 0
        for num_elements in range(1,self.MaxVars+1):

            if self.Verbose == True:
                print("Computing BMA for models of size: ", num_elements)

            # Make a list of all index sets of models of this size.
            Models_next = list(combinations(list(range(self.nCols)), num_elements))

            # Occam's window - compute the candidate models to use for the next iteration
            # Models_previous: the set of models from the previous iteration that satisfy (likelihhod > max_likelihhod/20)
            # Models_next:     the set of candidate models for the next iteration
            # Models_current:  the set of models from Models_next that can be consturcted by adding one new variable
            #                    to a model from Models_previous
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


            # Iterate through all possible models of the given size.
            for model_index_set in Models_current:

                # Compute the linear regression for this given model.
                model_X = self.X.iloc[:,list(model_index_set)]
                if self.RegType == 'Logit':
                    model_regr = sm.Logit(self.y, model_X).fit(disp=0)
                else:
                    model_regr = OLS(self.y, model_X).fit()

                # Compute the likelihood (times the prior) for the model.
                model_likelihood = mp.exp(-model_regr.bic/2)*np.prod(self.Priors[list(model_index_set)])

                if (model_likelihood > max_likelihood/20):
                    if self.Verbose == True:
                        print("Model Variables:",model_index_set,"likelihood=",model_likelihood)
                    self.likelihoods_all[str(model_index_set)] = model_likelihood

                    # Add this likelihood to the running tally of likelihoods.
                    likelighood_sum = mp.fadd(likelighood_sum, model_likelihood)

                    # Add this likelihood (times the priors) to the running tally
                    # of likelihoods for each variable in the model.
                    for idx, i in zip(model_index_set, range(num_elements)):
                        self.likelihoods[idx] = mp.fadd(self.likelihoods[idx], model_likelihood, prec=1000)
                        self.coefficients_mp[idx] = mp.fadd(self.coefficients_mp[idx], model_regr.params[i]*model_likelihood, prec=1000)
                    Models_previous.append(model_index_set) # add this model to the list of good models
                    max_likelihood = np.max([max_likelihood,model_likelihood]) # get the new max likelihood if it is this model
                else:
                    if self.Verbose == True:
                        print("Model Variables:",model_index_set,"rejected by Occam's window")


        # Divide by the denominator in Bayes theorem to normalize the probabilities
        # sum to one.
        self.likelighood_sum = likelighood_sum
        for idx in range(self.nCols):
            self.probabilities[idx] = mp.fdiv(self.likelihoods[idx],likelighood_sum, prec=1000)
            self.coefficients[idx] = mp.fdiv(self.coefficients_mp[idx],likelighood_sum, prec=1000)

        # Return the new BMA object as an output.
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

X = df.drop(["hazard"], axis=1)
X1 = df.drop(["smoke","illuminance","dist","firm","band","hazard"], axis=1)
y = df["hazard"]

# building the model and fitting the data
log_reg = sm.Logit(y, add_constant(X1)).fit()
# print(log_reg.summary())
pred_Logit = log_reg.predict(add_constant(X1))

result = BMA(y, add_constant(X), RegType = 'Logit', Verbose=True).fit()
pred_bma = result.predict(add_constant(X))

bma_coeff = result.coefficients
row_data = df.drop(["hazard"], axis=1)
row_data = add_constant(row_data)[2:3]
prediction = result.predict(add_constant(row_data))
#print(pred)

# print(pred_Logit)
# print(pred_bma)


#print(np.sum((pred_bma > 0.5) == y)/len(y))
#print(np.sum((pred_Logit > 0.5) == y)/len(y))

print(row_data)
print(prediction)

def fitness(X):
    row_data.loc[2, 'power'] = X[0]
    row_data.loc[2, 'band'] = X[1]
    row_data.loc[2, 'speed'] = X[2]
    row_data.loc[2, 'quality'] = X[3]
    prediction = result.predict(row_data)
    if (prediction) < 0.51:
        prediction = prediction / 10
    delta_change = abs(X[0] - 52)/(78-13) + abs(X[1] - 29.14)/(46.58-14.7) + abs(X[2] - 3.81)/(147.19-0) + abs(X[3] - 46)/(64-15)
    return delta_change - prediction[0]

algorithm_param = {'max_num_iteration': 50,\
                   'population_size':100,\
                   'mutation_probability':0.1,\
                   'elit_ratio': 0.01,\
                   'crossover_probability': 0.5,\
                   'parents_portion': 0.3,\
                   'crossover_type':'uniform',\
                   'max_iteration_without_improv':None}

# vars: sbp,tobacco,ldl,adiposity,famhist,typea,obesity,alcohol,age,chd
# e.g., 160,12,5.73,23.11,Present,49,25.3,97.2,52,1
# [101,218], [0,31.2], [0.98,15.33], [6.74,42.49], [0,1], [13,78], [14.7,46.58], [0,147.19], [15,64]

varbound = np.array([[13,78], [14.7,46.58], [0,147.19], [15,64]])
vartype = np.array([['int'], ['real'], ['real'], ['int']])
model = ga(function=fitness,
    dimension=4,
    variable_type_mixed=vartype,
    variable_boundaries=varbound,
    convergence_curve=False,
    algorithm_parameters=algorithm_param)

model.run()

adaptation = model.output_dict['variable']
row_data.loc[2, 'power'] = adaptation[0]
row_data.loc[2, 'band'] = adaptation[1]
row_data.loc[2, 'speed'] = adaptation[2]
row_data.loc[2, 'quality'] = adaptation[3]
prediction = result.predict(row_data)
print(row_data)
print(prediction)
