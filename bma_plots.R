library(ggplot2)
library(gridExtra)


# ==== RQ1: accuracy of predictions ====

df <- read.csv('precision_recall.log', header = T, dec = '.', sep = '')
bma_precision <- 0.664179104477612 
bma_recall <- 0.55625 
bma_F1 <- 0.6054421768707483

pdf("plot_tmp.pdf")
g1 <- ggplot(data = df, aes(x=model, y=precision, fill=model)) +
  geom_boxplot(alpha=0.5) +
  geom_hline(yintercept = bma_precision, color="red", linetype="dashed") +
  scale_y_continuous(limits=c(0,0.7), breaks = seq(0, 0.7, by = 0.1)) +
  annotate(geom="text", x=7, y=0.7, label="BMA precision", color="red", size=6) +
  theme(legend.position="none", axis.text.x = element_text(angle = 35, hjust=1), text = element_text(size=25))
#g1
print(g1)
dev.off()
system("pdfcrop --margins '0 0 0 0' plot_tmp.pdf bma_precision.pdf")

pdf("plot_tmp.pdf")
g2 <- ggplot(data = df, aes(x=model, y=recall, fill=model)) +
  geom_boxplot(alpha=0.5) +
  geom_hline(yintercept = bma_recall, color="red", linetype="dashed") +
  scale_y_continuous(limits=c(0,0.7), breaks = seq(0, 0.7, by = 0.1)) +
  annotate(geom="text", x=7, y=0.6, label="BMA recall", color="red", size=6) +
  theme(legend.position="none",  axis.text.x = element_text(angle = 35, hjust=1), text = element_text(size=25))
#g2
print(g2)
dev.off()
system("pdfcrop --margins '0 0 0 0' plot_tmp.pdf bma_recall.pdf")

pdf("plot_tmp.pdf")
g3 <- ggplot(data = df, aes(x=model, y=F1, fill=model)) +
  geom_boxplot(alpha=0.5) +
  geom_hline(yintercept = bma_F1, color="red", linetype="dashed") +
  scale_y_continuous(limits=c(0,0.7), breaks = seq(0, 0.7, by = 0.1)) +
  annotate(geom="text", x=7, y=0.65, label="BMA F1", color="red", size = 6) +
  theme(legend.position="none",  axis.text.x = element_text(angle = 35, hjust=1), text = element_text(size=25))
#g3
print(g3)
dev.off()
system("pdfcrop --margins '0 0 0 0' plot_tmp.pdf bma_f1.pdf")

system("rm *_tmp.pdf")
#par(mfrow=c(1,3))
#grid.arrange(g1, g2, g3, ncol=3)


# ==== RQ2: effectiveness of adaptation ====

df <- read.csv('re_bma_logit.log', header = T, dec = '.', sep = '')
options(scipen = 999)
#par(mfrow=c(1,2))

pdf("plot_tmp.pdf")
g1 <- ggplot(data = df, aes(x=type, y=RE, fill=type)) +
  geom_boxplot(alpha=0.5) +
  scale_y_continuous(trans='log10', limits=c(0.00001,10.0)) +
  theme(legend.position="none", text = element_text(size=25), aspect.ratio=1.5)
  #geom_hline(yintercept = 0.016129293631564, color="red", linetype="dashed")
#g1
print(g1)
dev.off()
system("pdfcrop --margins '0 0 0 0' plot_tmp.pdf relative_error.pdf")

bma_success_rate <- nrow(subset(df, type=='BMA' & success)) / nrow(subset(df, type=='BMA'))
logit_success_rate <- nrow(subset(df, type=='Logit' & success)) / nrow(subset(df, type=='Logit'))
success_rate_df <- data.frame (type  = c('BMA', 'Logit'), rate = c(bma_success_rate, logit_success_rate))

pdf("plot_tmp.pdf")
g2 <- ggplot(data = success_rate_df, aes(x=type, y=rate, fill=type)) +
  geom_bar(stat="identity", width=0.5, alpha=0.8) +
  scale_y_continuous(limits=c(0,0.8), breaks = seq(0, 0.8, by = 0.2)) +
  theme(legend.position="none", text = element_text(size=25), aspect.ratio=1.5)
#g2
print(g2)
dev.off()
system("pdfcrop --margins '0 0 0 0' plot_tmp.pdf success_rate.pdf")

system("rm *_tmp.pdf")
#grid.arrange(g1, g2, ncol=2)


# ==== RQ3: cost of BMA estimates ====

df <- read.csv('bma_cost.log', header = T, dec = '.', sep = '')
tiles <- aggregate(df[, 3:3], list(df$vars, df$sample), mean)
colnames(tiles) <- c('variables','sample','time')

pdf("plot_tmp.pdf")
g1 <- ggplot(tiles, aes(factor(variables), factor(sample), fill = time)) + 
  geom_tile(color = "black") +
  geom_text(aes(label=ifelse(time=='200', 'TO', round(time, 2))), colour="black", size=5) +
  scale_fill_gradient(low = "white", high = "orangered2",
      limits = c(0,200), breaks = c(50, 100, 150),
      guide = guide_colourbar(barwidth = 0.5, barheight = 10)) +
  xlab('#variables') +
  ylab('#observations') +
  labs(fill = "time (s)") +
  theme(text = element_text(size=20), aspect.ratio=0.8)
#g1
print(g1)
dev.off()
system("pdfcrop --margins '0 0 0 0' plot_tmp.pdf cost_mcmc.pdf")


df <- read.csv('bma_cost2.log', header = T, dec = '.', sep = '')
tiles <- aggregate(df[, 4:4], list(df$vars, df$sample), mean)
colnames(tiles) <- c('variables','sample','time')

pdf("plot_tmp.pdf")
g2 <- ggplot(tiles, aes(factor(variables), factor(sample), fill = time)) + 
  geom_tile(color = "black") +
  geom_text(aes(label=ifelse(time=='200', 'TO', round(time, 2))), colour="black", size=5) +
  scale_fill_gradient(low = "white", high = "orangered2",
                      limits = c(0,200), breaks = c(50, 100, 150),
                      guide = guide_colourbar(barwidth = 0.5, barheight = 10)) +
  xlab('#variables') +
  ylab('#observations') +
  labs(fill = "time (s)") +
  theme(text = element_text(size=20), aspect.ratio=0.8)
#g2
print(g2)
dev.off()
system("pdfcrop --margins '0 0 0 0' plot_tmp.pdf cost_bas.pdf")

system("rm *_tmp.pdf")
#par(mfrow=c(1,2))
#grid.arrange(g1, g2, ncol=2)



