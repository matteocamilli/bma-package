from subprocess import Popen, PIPE
import pandas as pd

# number of runs for each experiment
N = 100
COMMAND = 'Rscript'
SCRIPT = 'bma_cost.R'

# sample,vars,maxvars,method
df = pd.read_csv('data/cost_input.csv')

print('method vars sample time')

for i in range(N):
    for index, row in df.iterrows():
        sample = str(row['sample'])
        vars = str(row['vars'])
        method = row['method']
        # e.g., Rscript bma_cost.R -s 200 -v 2 -m MCMC
        p = Popen([COMMAND, SCRIPT, '-s ' + sample, '-v ' + vars, '-m ' + method], stdout=PIPE)
        try:
            result = p.communicate(timeout = 200)
            print(result[0].decode('UTF-8'))
        except subprocess.TimeoutExpired:
            p.kill()
            print(method + ' ' + vars + ' ' + sample + ' 200.0')
