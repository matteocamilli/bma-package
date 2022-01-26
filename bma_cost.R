suppressMessages(library(BMA))
suppressMessages(library(MASS))
suppressMessages(library(survival))
suppressMessages(library(BAS))
suppressMessages(library(argparse))
suppressMessages(library(R.utils))

defaultW <- getOption('warn')
options(warn = -1)

parser <- ArgumentParser()
#parser$add_argument('-n', '--nexec', type='integer', default=1,
#                    help='Number of executions [default 100]',
#                    metavar='number')
parser$add_argument('-s', '--sample', type='integer', default=200,
                    help='Number of observations [default 200]',
                    metavar='sample')
parser$add_argument('-v', '--vars', type='integer', default=2,
                    help='Number of variables [default 2]',
                    metavar='vars')
parser$add_argument('-m', '--method', type='character', default='MCMC',
                    help='Sampling method [default MCMC]',
                    metavar='method')
args <- parser$parse_args()
#N <- args$nexec
SAMPLE <- args$sample # in [200, 25600]
VARS <- args$vars # in [2, 64]
METHOD <- args$method # in {MCMC, BAS, MCMC+BAS}

#cat('method vars sample time\n')
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
  cat(selected_method, vars, sample_size, time_diff, sep = ' ')
}

#setTimeLimit(cpu = 10.0, elapsed = 10.0, transient = FALSE)
#quartz()
#dev.off()

run_bma(df, SAMPLE, VARS, 64, METHOD)

options(warn = defaultW)
