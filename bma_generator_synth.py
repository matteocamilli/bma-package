import numpy as np
import pandas as pd
from statsmodels.tools import add_constant
import argparse, random

def predict(X):
    #X['firm'] = (X['firm'] == 'Yes') * 1
    # intercept,illuminance,smoke,size,distance,firm,power,band,quality,speed
    coefficients = np.array([-6.112699, 0.000336, 0.073593, 0.137145, -0.000268, 0.913329, 0.033174, -0.002214, 0.0, 0.051443, 0.000336, 0.073593, 0.137145, -0.000268, 0.913329, 0.033174, 0.000336, 0.073593, 0.137145, -0.000268, 0.913329, 0.033174, -0.002214, 0.0, 0.051443, 0.000336, 0.073593, 0.137145, -0.000268, 0.913329, 0.033174, 0.000336, 0.073593, 0.137145, -0.000268, 0.913329, 0.033174, -0.002214, 0.0, 0.051443, 0.000336, 0.073593, 0.137145, -0.000268, 0.913329, 0.033174, 0.000336, 0.073593, 0.137145, -0.000268, 0.913329, 0.033174, -0.002214, 0.0, 0.051443, 0.000336, 0.073593, 0.137145, -0.000268, 0.913329, 0.033174, 0.137145, -0.000268, 0.913329, 0.033174])
    try:
        result = 1/(1+np.exp(-1*np.dot(coefficients, X)))
    except:
        result = 1/(1+np.exp(-1*np.dot(coefficients, X.T)))
    return result

def main():
    parser = argparse.ArgumentParser(description='BMA rescue robot data generator')
    parser.add_argument('limit', metavar='N', type=int,
                    help='number of samples to be genrated')
    #parser.add_argument('vars', metavar='V', type=int,
    #                help='number of explanatory variables (in {2,4,8,9}) to be genrated')
    args = parser.parse_args()
    SAMPLES = args.limit
    VARS = 64

    #print('illuminance,smoke,color,dist,firm,power,band,quality,speed,hazard')
    print(','.join(['x'+str(i) for i in range(1,VARS+1)]))
    for i in range(SAMPLES):
        sample = {}
        for i in range(1,VARS+1):
            sample.update({'x'+str(i) : random.uniform(random.randrange(1, 50), random.randrange(51, 100))})
        randX = pd.DataFrame(data = sample, index=[0])
        # randX = pd.DataFrame(data = {
        #     'illuminance': random.randrange(101, 218),
        #     'smoke': random.uniform(0.0, 31.2),
        #     'size': random.uniform(0.98, 15.33),
        #     'distance': random.uniform(6.74, 42.49),
        #     'firm': random.choice(list(['No', 'Yes'])),
        #     'power': random.randrange(13, 78),
        #     'band': random.uniform(14.7, 46.58),
        #     'quality': random.uniform(0.0, 147.19),
        #     'speed': random.randrange(15, 64) }, index=[0])
        hazard = 0
        if predict(add_constant(randX, has_constant='add')) > 0.5:
            hazard = 1
        row = randX.values[0]
        print(",".join(map(str, np.append(row, hazard))))


if __name__ == "__main__":
    main()
