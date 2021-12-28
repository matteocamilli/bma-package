library(ggplot2)
library(gridExtra)


# ==== RQ1: accuracy of predictions ====

df <- read.csv('precision_recall.log', header = T, dec = '.', sep = '')
bma_precision <- 0.664179104477612 
bma_recall <- 0.55625 
bma_F1 <- 0.6054421768707483

g1 <- ggplot(data = df, aes(x=model, y=precision, fill=model)) +
  geom_boxplot(alpha=0.5) +
  geom_hline(yintercept = bma_precision, color="red", linetype="dashed") +
  scale_y_continuous(limits=c(0,0.7), breaks = seq(0, 0.7, by = 0.1)) +
  annotate(geom="text", x=7, y=0.7, label="BMA precision", color="red") +
  theme(legend.position="none", axis.text.x = element_text(angle = 35, hjust=1))
#g1

g2 <- ggplot(data = df, aes(x=model, y=recall, fill=model)) +
  geom_boxplot(alpha=0.5) +
  geom_hline(yintercept = bma_recall, color="red", linetype="dashed") +
  scale_y_continuous(limits=c(0,0.7), breaks = seq(0, 0.7, by = 0.1)) +
  annotate(geom="text", x=7, y=0.6, label="BMA recall", color="red") +
  theme(legend.position="none",  axis.text.x = element_text(angle = 35, hjust=1))
#g2

g3 <- ggplot(data = df, aes(x=model, y=F1, fill=model)) +
  geom_boxplot(alpha=0.5) +
  geom_hline(yintercept = bma_F1, color="red", linetype="dashed") +
  scale_y_continuous(limits=c(0,0.7), breaks = seq(0, 0.7, by = 0.1)) +
  annotate(geom="text", x=7, y=0.65, label="BMA F1", color="red") +
  theme(legend.position="none",  axis.text.x = element_text(angle = 35, hjust=1))
#g3

par(mfrow=c(1,3))
grid.arrange(g1, g2, g3, ncol=3)

# ==== RQ2: effectiveness of adaptation ====

df <- read.csv('re_bma_logit.log', header = T, dec = '.', sep = '')
options(scipen = 999)
par(mfrow=c(1,2))

g1 <- ggplot(data = df, aes(x=type, y=RE, fill=type)) +
  geom_boxplot(alpha=0.5) +
  scale_y_continuous(trans='log10', limits=c(0.00001,10.0))
  #geom_hline(yintercept = 0.016129293631564, color="red", linetype="dashed")
#g1

bma_success_rate <- nrow(subset(df, type=='BMA' & success)) / nrow(subset(df, type=='BMA'))
logit_success_rate <- nrow(subset(df, type=='Logit' & success)) / nrow(subset(df, type=='Logit'))
success_rate_df <- data.frame (type  = c('BMA', 'Logit'), rate = c(bma_success_rate, logit_success_rate))

g2 <- ggplot(data = success_rate_df, aes(x=type, y=rate, fill=type)) +
  geom_bar(stat="identity", width=0.5, alpha=0.8) +
  scale_y_continuous(limits=c(0,0.8), breaks = seq(0, 0.8, by = 0.2))
#g2

grid.arrange(g1, g2, ncol=2)

