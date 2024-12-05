rm(list = ls())
set.seed(15)

load("05 Model Fit_model.Rdata")

reg.ind <- sample(1:357,6)

## -----------------------------------------------------------------------------
## Parameter estimates:

sigma.sq <- mean(samp$sigma.sq)
Sigma <- apply(samp$Sigma, c(2, 3), mean)
theta <- colMeans(samp$theta)
beta <- apply(samp$beta, c(2, 3), mean)

## -----------------------------------------------------------------------------
## Prediction:

load("03 Data for model.test.Rdata")

n.s <- ncol(Y.ij.test)
n.t <- nrow(Y.ij.test)

Y.ij.test.hat <- matrix(0, ncol = n.s, nrow = n.t)

for (j in 1:n.s){
  beta.j <- beta[j, ]
  X.j <- matrix(c(Y.bar.ij.1.test[,j], Y.bar.ij.2.test[,j], Y.bar.i.1.test,
                  Y.bar.i.2.test), ncol = 4)
  Y.ij.test.hat[,j] <- X.j %*% beta.j
}

# plot of actual value with predicted value -- -- --

df.list <- list()

for (i in 1:length(reg.ind)){
  df.list[[i]] <- data.frame(year = c(2001:2022, 2001:2022),
                             vals = c(Y.ij.test[,reg.ind[i]],Y.ij.test.hat[,reg.ind[i]]),
                             type = c(rep("Actual Value", n.t), rep("Predicted Value", n.t)))
}

library(ggplot2)

p <- 1
p1 <- ggplot(data = df.list[[p]]) +
  geom_line(aes(x = year, y = vals, col = type), lwd = 0.8) +
  labs(x = "Year",
       y = "Rainfall",
       col = "",
       title = paste0("Location: (",S[reg.ind,][p,1], "°E,", S[reg.ind,][p,2], "°N)")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

p <- 2
p2 <- ggplot(data = df.list[[p]]) +
  geom_line(aes(x = year, y = vals, col = type), lwd = 0.8) +
  labs(x = "Year",
       y = "Rainfall",
       col = "",
       title = paste0("Location: (",S[reg.ind,][p,1], "°E,", S[reg.ind,][p,2], "°N)")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

p <- 3
p3 <- ggplot(data = df.list[[p]]) +
  geom_line(aes(x = year, y = vals, col = type), lwd = 0.8) +
  labs(x = "Year",
       y = "Rainfall",
       col = "",
       title = paste0("Location: (",S[reg.ind,][p,1], "°E,", S[reg.ind,][p,2], "°N)")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

p <- 4
p4 <- ggplot(data = df.list[[p]]) +
  geom_line(aes(x = year, y = vals, col = type), lwd = 0.8) +
  labs(x = "Year",
       y = "Rainfall",
       col = "",
       title = paste0("Location: (",S[reg.ind,][p,1], "°E,", S[reg.ind,][p,2], "°N)")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

p <- 5
p5 <- ggplot(data = df.list[[p]]) +
  geom_line(aes(x = year, y = vals, col = type), lwd = 0.8) +
  labs(x = "Year",
       y = "Rainfall",
       col = "",
       title = paste0("Location: (",S[reg.ind,][p,1], "°E,", S[reg.ind,][p,2], "°N)")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

p <- 6
p6 <- ggplot(data = df.list[[p]]) +
  geom_line(aes(x = year, y = vals, col = type), lwd = 0.8) +
  labs(x = "Year",
       y = "Rainfall",
       col = "",
       title = paste0("Location: (",S[reg.ind,][p,1], "°E,", S[reg.ind,][p,2], "°N)")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

library(patchwork)

p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 2) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))
