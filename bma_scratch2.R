library(BMA)
library(MASS)
library(survival)
library(BAS)

df <- read.csv('sample_data1.csv')
x.demo <- df[,2:5]
y.demo <- df[,6]
reg <- bicreg(x.demo, y=y.demo)
#reg <- bic.glm(x.demo, y=y.demo, glm.family="Gamma", factor.type=FALSE)
imageplot.bma(reg)

summary(reg, digits=2)
plot(reg)

predict1 <- function(light, smoke, quality, speed) {
  (((26.2073926 * speed) + (0.1960962 * quality) + (16.0497700 * smoke) - (0.2117480 * light) - 5.1642082)^(-1)) * 5
}
predict2 <- function(light, smoke, quality, speed) {
  ((26.178891 * speed + 16.136621 * smoke -5.218887)^(-1)) * 5
}

predict_from_avg_model <- function(dataframe, sample_row, regression) {
  prediction = 0.0
  for (lab in names(regression[["postmean"]])) {
    if (lab == '(Intercept)') {
      prediction = prediction + regression[["postmean"]][[lab]]
    } else {
      prediction = prediction + regression[["postmean"]][[lab]] * dataframe[sample_row,][[lab]]
    }
  }
  prediction = (prediction)
  return(prediction)
}

predict_from_model <- function(dataframe, sample_row, regression, k) {
  prediction = 0.0
  for (lab in names(regression[["mle"]][k,])) {
    if (lab == '(Intercept)') {
      prediction = prediction + regression[["mle"]][k,][[lab]]
    } else {
      prediction = prediction + regression[["mle"]][k,][[lab]] * dataframe[sample_row,][[lab]]
    }
  }
  prediction = (prediction)
  return(prediction)
}

oracle <- function(dataframe, lab, sample_row) {
  return(dataframe[sample_row,][[lab]])
}

relative_error <- function(actual, prediction) {
  return(abs(actual - prediction) / actual)
}

compute_error <- function(dataframe, regression, sample_size) {
  nmodels = regression[['n.models']]
  for (i in 1:sample_size) {
    actual <- oracle(df, 'Dist', i)
    r = c(relative_error(actual, predict_from_avg_model(df, i, regression)))
    for (j in 1:nmodels) {
      r = c(r, relative_error(actual, predict_from_model(df, i, regression, j)))
    }
    if (i == 1) {
      m <- matrix(r, ncol = nmodels+1)
    } else {
      m <- rbind(m, r)
    }
  }
  return(m)
}

stat_diff <- function(x, y) {
  wilcox.test(x, y)$p.value
}

effect_size <- function(x, y) {
  m <- length(x)
  n <- length(y)
  r <- sum(rank(c(x, y))[seq_along(x)])
  (r/m - (m + 1)/2)/n
}

m <- compute_error(df, reg, 50)
stat_diff(m[,1], m[,2])
effect_size(m[,1], m[,3])

actual <- oracle(df, 'y', 3)
relative_error(actual, predict_from_avg_model(df, 3, crime.bicreg))
relative_error(actual, predict_from_model(df, 3, crime.bicreg, 1))
relative_error(actual, predict_from_model(df, 3, crime.bicreg, 2))
relative_error(actual, predict_from_model(df, 3, crime.bicreg, 50))


# ====

df <- read.csv('rescueRobotData.csv')

df$firm<- factor(as.character(df$firm))
reg <- bic.glm (hazard ~ illuminance + smoke + color + dist + firm + power + band + speed + quality,
                        data=df, glm.family="binomial", factor.type=T)
summary(reg)
imageplot.bma(reg)
plot(reg)

bas_reg = bas.glm(hazard ~ illuminance + smoke + color + dist + firm + power + band + speed + quality, data=df,
                   method="BAS+MCMC", MCMC.iterations=5000,
                   betaprior=bic.prior(), family=binomial(link = "logit"),
                   modelprior=uniform())
cog_bas = bas.glm(hazard ~ illuminance + smoke + color + dist + firm + power + band + speed + quality, data=df,
                  method="MCMC",
                  betaprior=bic.prior(), family=binomial(link = "logit"),
                  modelprior = uniform())
round(summary(cog_bas), 3)
image(cog_bas, rotate = F)


data(UScrime)

x.crime<- UScrime[,-16]
y.crime<- log(UScrime[,16])
x.crime[,-2]<- log(x.crime[,-2])
crime.bicreg <- bicreg(x.crime, y.crime)

df <- data.frame(x.crime, y = y.crime)

summary(crime.bicreg, digits=2)

plot(crime.bicreg)
imageplot.bma(crime.bicreg)

data(birthwt)

birthwt$race <- as.factor(birthwt$race)
birthwt$ptl <- as.factor(birthwt$ptl)
bwt.bic.glm <- bic.glm (low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
                        data=birthwt, glm.family="binomial", factor.type=T)

summary(bwt.bic.glm,conditional=T,digits=2)
plot(bwt.bic.glm)


data(veteran)
surv.t<- veteran$time
x<- veteran[,-c(3,4)]
x$celltype<- factor(as.character(x$celltype))
sel<- veteran$status == 0
x<- x[!sel,]
surv.t<- surv.t[!sel]
glm.out.va <- bic.glm(x, y=surv.t, glm.family=Gamma(link="inverse"),
                      factor.type=FALSE)
summary(glm.out.va)
imageplot.bma(glm.out.va)
plot(glm.out.va)

