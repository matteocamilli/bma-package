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

df = pd.read_csv('data/training_rescueRobot_450.csv')
dfv = pd.read_csv('data/validation_rescueRobot_450.csv')
# converts the firm field to 0 (no firm) and 1 (firm)
df["firm"] = (df["firm"] == "Yes") * 1
dfv["firm"] = (dfv["firm"] == "Yes") * 1

# illuminance,smoke,size,distance,firm,power,band,quality,speed,hazard
DROP = ['illuminance','smoke','distance','band','quality','hazard']
LIMIT = 450
LIMIT1 = 450

X_oracle = dfv.drop(["hazard"], axis=1)
y_oracle = dfv["hazard"]
X = df.drop(["hazard"], axis=1)[0:LIMIT]
y = df["hazard"][0:LIMIT]
X1 = df.drop(DROP, axis=1)[0:LIMIT1]
y1 = df["hazard"][0:LIMIT1]

# build oracle model
oracle = BMA(y_oracle, add_constant(X_oracle), RegType = 'Logit', Verbose=True).fit()
# 'a priori' model example
log_reg = sm.Logit(y1, add_constant(X1)).fit()
#print(log_reg.summary())
#row_data = df.drop(DROP, axis=1)
#row_data = add_constant(row_data)[2:3]
#print(row_data)
#print(log_reg.predict(row_data))
#row_data.iloc[0, row_data.columns.get_loc('speed')] = 70
#print(row_data)
#print(log_reg.predict(row_data))
#pred_Logit = log_reg.predict(add_constant(X1))
# bma model
bma_reg = BMA(y, add_constant(X), RegType = 'Logit', Verbose=True).fit()
pred_bma = bma_reg.predict(add_constant(X))

#print(pred_Logit[100:106])
#print(pred_bma[100:106])
#exit()

# print(pred_Logit)
# print(pred_bma)

#accuracy = np.sum((pred_bma > 0.5) == y)/len(y)
#print('BMA: precision = ' + str(precision(pred_bma, y)) + ' recall = ' + str(recall(pred_bma, y)) + ' F = ' + str(f_measure(pred_bma, y)))
#print('Logit: precision = ' + str(precision(pred_Logit, y1)) + ' recall = ' + str(recall(pred_Logit, y1)) + ' F = ' + str(f_measure(pred_Logit, y1)))


def fitness(X):
    i = 0
    for k in tmp_index_set:
        tmp_data.iloc[0, tmp_data.columns.get_loc(tmp_vars[k][0])] = X[i]
        i = i + 1
    if type(tmp_model) is BMA:
        prediction = tmp_model.predict(tmp_data)[0]
    else:
        prediction = tmp_model.predict(tmp_data).values[0]
    if prediction < 0.51:
        prediction = prediction / 10
    delta_change = 0.0
    i = 0
    for k in tmp_index_set:
        delta_change = delta_change + tmp_vars[k][3] * abs(X[i] - tmp_initial_values[i])/(tmp_vars[k][2][1] - tmp_vars[k][2][0])
        i = i + 1
    #delta_change = 0.8 * abs(X[0] - 52)/(78-13) + 0.4 * abs(X[1] - 29.14)/(46.58-14.7) + 0.2 * abs(X[2] - 3.81)/(147.19-0) + 0.1 * abs(X[3] - 46)/(64-15)
    return delta_change - prediction

def run_adaptation(model, vars, index_set, row_data, fitness):
    vartype = np.array([vars[k][1] for k in index_set])
    varbound = np.array([vars[k][2] for k in index_set])
    params = {'max_num_iteration': 50,\
        'population_size': 100,\
        'mutation_probability':0.1,\
        'elit_ratio': 0.01,\
        'crossover_probability': 0.5,\
        'parents_portion': 0.3,\
        'crossover_type': 'uniform',\
        'max_iteration_without_improv': None}
    ga_model = ga(function = fitness,
        dimension = len(index_set),
        variable_type_mixed = vartype,
        variable_boundaries = varbound,
        convergence_curve = False,
        progress_bar = False,
        algorithm_parameters = params)
    ga_model.run()
    assignment = ga_model.output_dict['variable']
    i = 0
    for k in index_set:
        row_data.iloc[0, row_data.columns.get_loc(vars[k][0])] = assignment[i]
        i = i + 1
    return row_data

# vars: illuminance,smoke,color,dist,firm,power,band,speed,quality,hazard
# e.g., 160,12,5.73,23.11,Present,49,25.3,97.2,52,1
# [101,218], [0,31.2], [0.98,15.33], [6.74,42.49], [0,1], [13,78], [14.7,46.58], [0,147.19], [15,64]

vars = {
    5: ('power', ['int'], [13,78], 0.8),\
    6: ('band', ['real'], [14.7,46.58], 0.4),\
    7: ('quality', ['real'], [0,147.19], 0.2),\
    8: ('speed', ['int'], [15,64], 0.1)}

# print('=== Example: adaptation with BMA ===')
#
# row_data = df.drop(["hazard"], axis=1)
# row_data = add_constant(row_data)[2:3]
# prediction = bma_reg.predict(row_data)
# pred_oracle = oracle.predict(row_data)
#
# print(row_data)
# print('Prediction: ' + str(prediction[0]))
# print('Oracle: ' + str(pred_oracle[0]))
#
# tmp_model = bma_reg
# tmp_vars = vars
# tmp_data = row_data
# tmp_index_set = [5,6,7,8]
# tmp_initial_values = [row_data[vars[k][0]].values[0] for k in tmp_index_set]
#
# new_data = run_adaptation(bma_reg, vars, tmp_index_set, row_data, fitness)
# prediction = bma_reg.predict(new_data)
# pred_oracle = oracle.predict(row_data)
#
# print(new_data)
# print('Prediction: ' + str(prediction[0]))
# print('Oracle: ' + str(pred_oracle[0]))
# print('RE: ' + str(abs(prediction[0] - pred_oracle[0])/pred_oracle[0]))
#
# print('=== Example: adaptation with Logit ===')
#
# row_data = df.drop(DROP, axis=1)
# row_data = add_constant(row_data)[2:3]
# prediction = log_reg.predict(row_data)
#
# print(row_data)
# print('Prediction: ' + str(prediction.values[0]))
# reference_data = df.drop(["hazard"], axis=1)
# reference_data = add_constant(reference_data)[2:3]
# print('Oracle: ' + str(pred_oracle[0]))
#
# tmp_model = log_reg
# tmp_vars = vars
# tmp_data = row_data
# tmp_index_set = [5,8]
# tmp_initial_values = [row_data[vars[k][0]].values[0] for k in tmp_index_set]
#
# new_data = run_adaptation(log_reg, vars, tmp_index_set, row_data, fitness)
# prediction = log_reg.predict(new_data)
#
# print(new_data)
# print('Prediction: ' + str(prediction.values[0]))
# for k in new_data:
#     reference_data.loc[2, k] = new_data.loc[2, k]
# pred_oracle = oracle.predict(reference_data)
# print('Oracle: ' + str(pred_oracle[0]))
# print('RE: ' + str(abs(prediction.values[0] - pred_oracle[0])/pred_oracle[0]))
#
# exit()


print('=== Adaptation with BMA ===')

selected_rows = [i for i in df.index if df.loc[i, 'hazard'] == 0]
for r in selected_rows:
    row_data = df.drop(["hazard"], axis=1)
    row_data = add_constant(row_data)[r:(r+1)]
    prediction = bma_reg.predict(row_data)
    pred_oracle = oracle.predict(row_data)

    #print(row_data)
    #print('Prediction: ' + str(prediction[0]))
    #print('Oracle: ' + str(pred_oracle[0]))

    tmp_model = bma_reg
    tmp_vars = vars
    tmp_data = row_data
    tmp_index_set = [5,6,7,8]
    tmp_initial_values = [row_data[vars[k][0]].values[0] for k in tmp_index_set]

    new_data = run_adaptation(bma_reg, vars, tmp_index_set, row_data, fitness)
    prediction = bma_reg.predict(new_data)
    pred_oracle = oracle.predict(row_data)

    #print(new_data)
    #print('Prediction: ' + str(prediction[0]))
    #print('Oracle: ' + str(pred_oracle[0]))
    print('BMA RE: ' + str(abs(prediction[0] - pred_oracle[0])/pred_oracle[0]) + ' Success: ' + str(prediction[0] > 0.5 and pred_oracle[0] > 0.5))


print('=== Systematic comparison between BMA and Logit models ===')

# mechanical elicitation of all possible models
names = list(X.columns)
nRows, nCols = np.shape(X)
maxVars = nCols
for num_elements in range(1, maxVars + 1):
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
        if len(model_index_set) < maxVars and len(np.intersect1d(model_index_set, [int(k) for k in vars.keys()])) > 0:
            # compute the regression for this given model
            model_X = X.iloc[:, list(model_index_set)]
            model_regr = sm.Logit(y, model_X).fit(disp = 0)
            row_data = X.iloc[2:3, list(model_index_set)]
            row_data = add_constant(row_data)
            #print(row_data)
            tmp_model = model_regr
            tmp_vars = vars
            tmp_data = row_data
            tmp_index_set = [x for x in model_index_set if x >=5]
            tmp_initial_values = [row_data[vars[k][0]].values[0] for k in tmp_index_set]
            new_data = run_adaptation(model_regr, vars, tmp_index_set, row_data, fitness)
            #print(new_data)
            prediction = model_regr.predict(new_data)
            reference_data = df.drop(["hazard"], axis=1)
            reference_data = add_constant(reference_data)[2:3]
            for k in tmp_index_set:
                s = reference_data.columns.get_loc(vars[k][0])
                t = new_data.columns.get_loc(vars[k][0])
                reference_data.iloc[0, s] = new_data.iloc[0, t]
            #print(reference_data)
            pred_oracle = oracle.predict(reference_data)
            print('Logit ' + str(model_index_set) + ' Vars: ' + str(len(model_index_set)) + ' RE: ' + str(abs(prediction.values[0] - pred_oracle[0])/pred_oracle[0]) + ' Success: ' + str(prediction.values[0] > 0.5 and pred_oracle[0] > 0.5))
        models_previous.append(model_index_set)
