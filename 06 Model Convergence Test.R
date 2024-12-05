rm(list = ls())
load("05 Model Fit_model.Rdata")

set.seed(10)

N <- nrow(samp[[2]])
p <- ncol(samp[[2]])

beta.samp.ind <- sample(1:357, 1)
theta.samp.ind <- sample(1:p, 1)
Sigma.samp.ind <- sample(1:p,4, replace = T)

# ------------------------------------------------------------------------------
# Trace plots:

library(ggplot2)
library(latex2exp)
library(glue)

y.label.sub <- paste0(beta.samp.ind,", ",1)
p1 <- ggplot() +
  geom_line(aes(x = 1:N, y = samp$beta[,beta.samp.ind,1]), col = "slateblue2") +
  labs(x = "Iteration",
       y = bquote(beta[.(y.label.sub)])) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

y.label.sub <- paste0(beta.samp.ind,", ",2)
p2 <- ggplot() +
  geom_line(aes(x = 1:N, y = samp$beta[,beta.samp.ind,2]), col = "slateblue2") +
  labs(x = "Iteration",
       y = bquote(beta[.(y.label.sub)])) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

p3 <- ggplot() +
  geom_line(aes(x = 1:N, y = samp$sigma.sq), col = "darkcyan") +
  labs(x = "Iteration",
       y = TeX(r"($\sigma^2$)")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

p4 <- ggplot() +
  geom_line(aes(x = 1:N, y = samp$theta[,theta.samp.ind[1]]), col = "steelblue3") +
  labs(x = "Iteration",
       y = bquote(theta[.(theta.samp.ind[1])])) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

y.label.sub <- paste0(Sigma.samp.ind[1],Sigma.samp.ind[2])
p5 <- ggplot() +
  geom_line(aes(x = 1:N, y = samp$Sigma[,Sigma.samp.ind[1],Sigma.samp.ind[2]]),
            col = "paleturquoise4") +
  labs(x = "Iteration",
       y = bquote(Sigma[.(y.label.sub)])) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

y.label.sub <- paste0(Sigma.samp.ind[3],Sigma.samp.ind[4])
p6 <- ggplot() +
  geom_line(aes(x = 1:N, y = samp$Sigma[,Sigma.samp.ind[3],Sigma.samp.ind[4]]),
            col = "paleturquoise4") +
  labs(x = "Iteration",
       y = bquote(Sigma[.(y.label.sub)])) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

library(patchwork)

p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 3)

# ------------------------------------------------------------------------------
# Effective Sample Sizes:

library(coda)

ESS.beta <- matrix(0, ncol = p, nrow = 357)
for (i in 1:357){
  for (j in 1:p){
    ESS.beta[i,j] <- unname(effectiveSize(samp$beta[,i,j]))
  }
}
ESS.beta <- as.vector(ESS.beta)
ESS.sigma.sq <- unname(effectiveSize(samp$sigma.sq))
ESS.theta <- numeric(length = p)
for (i in 1:p){
  ESS.theta[i] <- unname(effectiveSize(samp$theta[,p]))
}
ESS.Sigma <- matrix(0, ncol = p, nrow = p)
for (i in 1:p){
  for (j in 1:p){
    ESS.Sigma[i,j] <- unname(effectiveSize(samp$Sigma[,i,j]))
  }
}
ESS.Sigma <- as.vector(ESS.Sigma)

summary(ESS.beta)
ESS.sigma.sq
ESS.theta
summary(ESS.Sigma)

# ------------------------------------------------------------------------------
# ACF plots:

library(ggplot2)
library(forecast)

acf.beta.1 <- Acf(samp$beta[,beta.samp.ind,1], plot = FALSE)
acf.beta.2 <- Acf(samp$beta[,beta.samp.ind,2], plot = FALSE)

acf.sigma.sq <- Acf(samp$sigma.sq, plot = FALSE)

acf.theta <- Acf(samp$theta[,theta.samp.ind[1]], plot = FALSE)

acf.Sigma.1 <- Acf(samp$Sigma[,Sigma.samp.ind[1],Sigma.samp.ind[2]], plot = FALSE)
acf.Sigma.2 <- Acf(samp$Sigma[,Sigma.samp.ind[3],Sigma.samp.ind[4]], plot = FALSE)

acf.plot.df <- data.frame(Lag = as.numeric(acf.beta.1$lag[1:16]),
                          acf.beta.1 = as.numeric(acf.beta.1$acf[1:16]),
                          acf.beta.2 = as.numeric(acf.beta.2$acf[1:16]),
                          acf.sigma.sq = as.numeric(acf.sigma.sq$acf[1:16]),
                          acf.theta = as.numeric(acf.theta$acf[1:16]),
                          acf.Sigma.1 = as.numeric(acf.Sigma.1$acf[1:16]),
                          acf.Sigma.2 = as.numeric(acf.Sigma.2$acf[1:16]))

y.label.sub <- paste0(beta.samp.ind,", ",1)
p1 <- ggplot(acf.plot.df, aes(x = Lag, y = acf.beta.1)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_segment(aes(xend = Lag, yend = 0), lwd = 1, color = "slateblue2") + 
  geom_point(color = "slateblue2", size = 1) +
  labs(x = "Lag", y = "ACF", title = bquote(beta[.(y.label.sub)])) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 13))

y.label.sub <- paste0(beta.samp.ind,", ",2)
p2 <- ggplot(acf.plot.df, aes(x = Lag, y = acf.beta.2)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_segment(aes(xend = Lag, yend = 0), lwd = 1, color = "slateblue2") + 
  geom_point(color = "slateblue2", size = 1) +
  labs(x = "Lag", y = "ACF", title = bquote(beta[.(y.label.sub)])) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 13))

p3 <- ggplot(acf.plot.df, aes(x = Lag, y = acf.sigma.sq)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_segment(aes(xend = Lag, yend = 0), lwd = 1, color = "darkcyan") + 
  geom_point(color = "darkcyan", size = 1) +
  labs(x = "Lag", y = "ACF", title = TeX(r"($\sigma^2$)")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 13))

p4 <- ggplot(acf.plot.df, aes(x = Lag, y = acf.theta)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_segment(aes(xend = Lag, yend = 0), lwd = 1, color = "steelblue3") + 
  geom_point(color = "steelblue3", size = 1) +
  labs(x = "Lag", y = "ACF", title = bquote(theta[.(theta.samp.ind[1])])) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 13))

y.label.sub <- paste0(Sigma.samp.ind[1],Sigma.samp.ind[2])
p5 <- ggplot(acf.plot.df, aes(x = Lag, y = acf.Sigma.1)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_segment(aes(xend = Lag, yend = 0), lwd = 1, color = "paleturquoise4") + 
  geom_point(color = "paleturquoise4", size = 1) +
  labs(x = "Lag", y = "ACF", title = bquote(Sigma[.(y.label.sub)])) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 13))

y.label.sub <- paste0(Sigma.samp.ind[3],Sigma.samp.ind[4])
p6 <- ggplot(acf.plot.df, aes(x = Lag, y = acf.Sigma.2)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_segment(aes(xend = Lag, yend = 0), lwd = 1, color = "paleturquoise4") + 
  geom_point(color = "paleturquoise4", size = 1) +
  labs(x = "Lag", y = "ACF", title = bquote(Sigma[.(y.label.sub)])) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 13))


library(patchwork)

p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 3)
