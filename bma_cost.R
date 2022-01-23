library(BMA)
library(MASS)
library(survival)
library(BAS)
library(argparse)
library(R.utils)

defaultW <- getOption('warn')
options(warn = -1) 

parser <- ArgumentParser()
parser$add_argument('-n', '--nexec', type='integer', default=100,
                    help='Number of executions [default 100]',
                    metavar='number')
args <- parser$parse_args()
N <- args$nexec

cat('method vars sample time\n')
df <- read.csv('data/training_rescueRobot_25600_64.csv')
#df$firm<- factor(as.character(df$firm))

# method="BAS+MCMC"
# run_bma <- function(df, sample_size = 200, vars = 2, max_cols = 64) {
#   sample <- df[1:sample_size,(max_cols-vars):max_cols]
#   t0 <- Sys.time()
#   bas_reg = bas.glm(hazard ~ ., data=sample,
#                     method="MCMC", MCMC.iterations=5000,
#                     betaprior=bic.prior(), family=binomial(link = "logit"),
#                     modelprior=uniform())
#   t1 <- Sys.time()
#   time_diff <- as.numeric(t1 - t0, units = "secs")
#   cat(paste(vars, sample_size, time_diff, sep = ','), '\n')
# }

bma <- function(sample, selected_method = 'MCMC') {
  reg <- bas.glm(x64 ~ ., data=sample,
          method=selected_method, MCMC.iterations=5000,
          betaprior=bic.prior(), family=binomial(link = "logit"),
          modelprior=uniform())
  return(reg)
}

# method="BAS+MCMC"
run_bma <- function(df, sample_size = 200, vars = 2, max_cols = 10, selected_method = 'MCMC') {
  sample <- df[1:sample_size,(max_cols-vars):max_cols]
  t0 <- Sys.time()
  bas_reg <- withTimeout(
    bma(sample, selected_method),
    timeout = 200.0, elapsed = 200.0, onTimeout = 'silent')
  time_diff <- as.numeric(Sys.time() - t0, units = "secs")
  cat(paste(selected_method, vars, sample_size, time_diff, sep = ' '), '\n')
}

#setTimeLimit(cpu = 10.0, elapsed = 10.0, transient = FALSE)
#quartz()
#dev.off()

for (i in 1:N) {
  # run_bma(df, 200, 2, 64, 'MCMC')
  # run_bma(df, 200, 4, 64, 'MCMC')
  # run_bma(df, 200, 8, 64, 'MCMC')
  # run_bma(df, 200, 16, 64, 'MCMC')
  # run_bma(df, 200, 32, 64, 'MCMC')
  # run_bma(df, 200, 64, 64, 'MCMC')
  # 
  # run_bma(df, 400, 2, 64, 'MCMC')
  # run_bma(df, 400, 4, 64, 'MCMC')
  # run_bma(df, 400, 8, 64, 'MCMC')
  # run_bma(df, 400, 16, 64, 'MCMC')
  # run_bma(df, 400, 32, 64, 'MCMC')
  # run_bma(df, 400, 64, 64, 'MCMC')
  # 
  # run_bma(df, 800, 2, 64, 'MCMC')
  # run_bma(df, 800, 4, 64, 'MCMC')
  # run_bma(df, 800, 8, 64, 'MCMC')
  # run_bma(df, 800, 16, 64, 'MCMC')
  # run_bma(df, 800, 32, 64, 'MCMC')
  # run_bma(df, 800, 64, 64, 'MCMC')
  # 
  # run_bma(df, 1600, 2, 64, 'MCMC')
  # run_bma(df, 1600, 4, 64, 'MCMC')
  # run_bma(df, 1600, 8, 64, 'MCMC')
  # run_bma(df, 1600, 16, 64, 'MCMC')
  # run_bma(df, 1600, 32, 64, 'MCMC')
  # run_bma(df, 1600, 64, 64, 'MCMC')
  # 
  # run_bma(df, 3200, 2, 64, 'MCMC')
  # run_bma(df, 3200, 4, 64, 'MCMC')
  # run_bma(df, 3200, 8, 64, 'MCMC')
  # run_bma(df, 3200, 16, 64, 'MCMC')
  # run_bma(df, 3200, 32, 64, 'MCMC')
  # run_bma(df, 3200, 64, 64, 'MCMC')
  # 
  # run_bma(df, 6400, 2, 64, 'MCMC')
  # run_bma(df, 6400, 4, 64, 'MCMC')
  # run_bma(df, 6400, 8, 64, 'MCMC')
  # run_bma(df, 6400, 16, 64, 'MCMC')
  # run_bma(df, 6400, 32, 64, 'MCMC')
  # run_bma(df, 6400, 64, 64, 'MCMC')
  # 
  # run_bma(df, 12800, 2, 64, 'MCMC')
  # run_bma(df, 12800, 4, 64, 'MCMC')
  # run_bma(df, 12800, 8, 64, 'MCMC')
  # run_bma(df, 12800, 16, 64, 'MCMC')
  # run_bma(df, 12800, 32, 64, 'MCMC')
  # run_bma(df, 12800, 64, 64, 'MCMC')
  # 
  # run_bma(df, 25600, 2, 64, 'MCMC')
  # run_bma(df, 25600, 4, 64, 'MCMC')
  # run_bma(df, 25600, 8, 64, 'MCMC')
  # run_bma(df, 25600, 16, 64, 'MCMC')
  # run_bma(df, 25600, 32, 64, 'MCMC')
  # run_bma(df, 25600, 64, 64, 'MCMC')
  
  # ===
  
  # run_bma(df, 200, 2, 64, 'BAS')
  # run_bma(df, 200, 4, 64, 'BAS')
  # run_bma(df, 200, 8, 64, 'BAS')
  # run_bma(df, 200, 16, 64, 'BAS')
  #run_bma(df, 200, 32, 64, 'BAS') # TO
  #run_bma(df, 200, 64, 64, 'BAS') # TO
  
  # run_bma(df, 400, 2, 64, 'BAS')
  # run_bma(df, 400, 4, 64, 'BAS')
  # run_bma(df, 400, 8, 64, 'BAS')
  # run_bma(df, 400, 16, 64, 'BAS')
  #run_bma(df, 400, 32, 64, 'BAS')  # TO
  #run_bma(df, 400, 64, 64, 'BAS')  # TO
  
  # run_bma(df, 800, 2, 64, 'BAS')
  # run_bma(df, 800, 4, 64, 'BAS')
  # run_bma(df, 800, 8, 64, 'BAS')
  #run_bma(df, 800, 16, 64, 'BAS')  # TO
  #run_bma(df, 800, 32, 64, 'BAS')  # TO
  #run_bma(df, 800, 64, 64, 'BAS')  # TO
  
  # run_bma(df, 1600, 2, 64, 'BAS')
  # run_bma(df, 1600, 4, 64, 'BAS')
  # run_bma(df, 1600, 8, 64, 'BAS')
  #run_bma(df, 1600, 16, 64, 'BAS')  # TO
  #run_bma(df, 1600, 32, 64, 'BAS')  # TO
  #run_bma(df, 1600, 64, 64, 'BAS')  # TO
  
  run_bma(df, 3200, 2, 64, 'BAS')
  run_bma(df, 3200, 4, 64, 'BAS')
  run_bma(df, 3200, 8, 64, 'BAS')
  #run_bma(df, 3200, 16, 64, 'BAS')  # TO
  #run_bma(df, 3200, 32, 64, 'BAS')  # TO
  #run_bma(df, 3200, 64, 64, 'BAS')  # TO
  
  run_bma(df, 6400, 2, 64, 'BAS')
  run_bma(df, 6400, 4, 64, 'BAS')
  run_bma(df, 6400, 8, 64, 'BAS')
  #run_bma(df, 6400, 16, 64, 'BAS')  # TO
  #run_bma(df, 6400, 32, 64, 'BAS')  # TO
  #run_bma(df, 6400, 64, 64, 'BAS')  # TO
  
  run_bma(df, 12800, 2, 64, 'BAS')
  run_bma(df, 12800, 4, 64, 'BAS')
  run_bma(df, 12800, 8, 64, 'BAS')
  #run_bma(df, 12800, 16, 64, 'BAS')  # TO
  #run_bma(df, 12800, 32, 64, 'BAS')  # TO
  #run_bma(df, 12800, 64, 64, 'BAS')  # TO
  
  run_bma(df, 25600, 2, 64, 'BAS')
  run_bma(df, 25600, 4, 64, 'BAS')
  run_bma(df, 25600, 8, 64, 'BAS')
  #run_bma(df, 25600, 16, 64, 'BAS')  # TO
  #run_bma(df, 25600, 32, 64, 'BAS')  # TO
  #run_bma(df, 25600, 64, 64, 'BAS')  # TO
}

options(warn = defaultW)
