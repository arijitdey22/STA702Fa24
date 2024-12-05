rm(list = ls())

load("01 Monsoon_rain_imd.Rdata")

n.t <- dim(all.data)[1]
n.s <- dim(all.data)[2]

## =============================================================================
## function for choosing 1 and 2 nearest point

func.near <- function(ind){
  loc <- S[ind, ]
  ind.lat.1 <- (S[, 1] >= (loc[1] - 1)) & (S[, 1] <= (loc[1] + 1))
  ind.lon.1 <- (S[, 2] >= (loc[2] - 1)) & (S[, 2] <= (loc[2] + 1))
  ind.loc.1 <- which(ind.lat.1 & ind.lon.1 == T)
  loc.1.near <- setdiff(ind.loc.1, ind)
  
  ind.lat.2 <- (S[, 1] >= (loc[1] - 2)) & (S[, 1] <= (loc[1] + 2))
  ind.lon.2 <- (S[, 2] >= (loc[2] - 2)) & (S[, 2] <= (loc[2] + 2))
  ind.loc.2 <- which(ind.lat.2 & ind.lon.2 == T)
  loc.2.near <- setdiff(ind.loc.2, ind.loc.1)
  
  return(list(loc.1.near, loc.2.near))
}

## =============================================================================
## Plot of rainfall for the first five locations (for ppt)

plot.df <- data.frame(rainfall = as.vector(all.data[,1:5]),
                      type = rep(1:5,each = 122),
                      year = rep(1901:2022, 5))
plot.df$type <- as.factor(plot.df$type)

library(ggplot2)

ggplot(data = plot.df) +
  geom_line(aes(x = year, y = rainfall, col = type), lwd = 0.8) +
  labs(x = "Year", y = "Rainall") +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14))

## =============================================================================
## Plot of all pixels and 1-near locations and 2-near location

S.plot <- as.data.frame(S)
S.plot$type <- "All Pixel  "

# Chosen pixels: 258,220,80

loc.near <- func.near(258)
S.plot[loc.near[[1]],3] <- "Near-1-Neighbour  "
S.plot[loc.near[[2]],3] <- "Near-2-Neighbour"

loc.near <- func.near(220)
S.plot[loc.near[[1]],3] <- "Near-1-Neighbour  "
S.plot[loc.near[[2]],3] <- "Near-2-Neighbour"

loc.near <- func.near(80)
S.plot[loc.near[[1]],3] <- "Near-1-Neighbour  "
S.plot[loc.near[[2]],3] <- "Near-2-Neighbour"

ggplot() +
  geom_point(data = S.plot, aes(x = x, y = y, col = type), pch = 15, cex = 4) +
  labs(x = "Longitude",
       y = "Latitude",
       col = "") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 14))

## =============================================================================
## Plot to show relation between locations

# plot 1: 

library(tidyverse)

reg.ind <- 200 : 203
plot.df <- as.data.frame(all.data[, reg.ind])

plot.df$Year <- 1:nrow(plot.df) 

plot.df.long <- plot.df %>%
  pivot_longer(cols = -Year, names_to = "Place", values_to = "Rainfall")

ggplot(plot.df.long, aes(x = Year, y = Rainfall, color = Place)) +
  geom_line(lwd = 0.8) +
  labs(x = "Year",
       y = "Rainfall",
       color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 13)) + 
  scale_color_discrete(labels = c(paste0("(",S[reg.ind,][1,1], "°E,", S[reg.ind,][1,2], "°N)"),
                                  paste0("(",S[reg.ind,][2,1], "°E,", S[reg.ind,][2,2], "°N)"),
                                  paste0("(",S[reg.ind,][3,1], "°E,", S[reg.ind,][3,2], "°N)"),
                                  paste0("(",S[reg.ind,][4,1], "°E,", S[reg.ind,][4,2], "°N)")))


# Plot 2:

cor.vec.1 <- cor.vec.2 <- numeric(length = n.s)

for(i in 1:n.s){
  foo <- func.near(i)
  loc.1 <- foo[[1]]
  loc.2 <- foo[[2]]
  
  cor.vec.1.cur <- numeric(length = length(loc.1))
  for (j in 1:length(loc.1)){
    cor.vec.1.cur[j] <- cor(all.data[,i], all.data[,loc.1[j]])
  }
  cor.vec.1[i] <- mean(cor.vec.1.cur)
  
  cor.vec.2.cur <- numeric(length = length(loc.2))
  for (j in 1:length(loc.2)){
    cor.vec.2.cur[j] <- cor(all.data[,i], all.data[,loc.2[j]])
  }
  cor.vec.2[i] <- mean(cor.vec.2.cur)
  
}

df.plot <- data.frame(val = c(cor.vec.1, cor.vec.2),
                      type = c(rep("1-Near Location", n.s),
                               rep("2-Near Location", n.s)))

ggplot(data = df.plot) +
  geom_boxplot(aes(x = val, col = type)) +
  labs(x = "Correlation value", col = "") +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 13))

## =============================================================================
## Plots to show normality over time and stationary

# plotting for same 4 chosen places

# normality test -- -- -- -- 

set.seed(1)
reg.ind <- sample(1:357, 4)

data <- data.frame(sample.1 = all.data[,reg.ind[1]],
                   sample.2 = all.data[,reg.ind[2]],
                   sample.3 = all.data[,reg.ind[3]],
                   sample.4 = all.data[,reg.ind[4]])

p1 <- ggplot(data) +
  stat_qq(aes(sample = sample.1), color = "turquoise3", size = 1) +
  stat_qq_line(aes(sample = sample.1),color = "red", linetype = "dashed", size = 0.5) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
       title = paste0("Location: (",S[reg.ind,][1,1], "°E,", S[reg.ind,][1,2], "°N)")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5))

p2 <- ggplot(data) +  
  stat_qq(aes(sample = sample.2), color = "steelblue3", size = 1) +
  stat_qq_line(aes(sample = sample.2),color = "red", linetype = "dashed", size = 0.5) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
       title = paste0("Location: (",S[reg.ind,][2,1], "°E,", S[reg.ind,][2,2], "°N)")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5))

p3 <- ggplot(data) +  
  stat_qq(aes(sample = sample.3), color = "paleturquoise4", size = 1) +
  stat_qq_line(aes(sample = sample.3),color = "red", linetype = "dashed", size = 0.5) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
       title = paste0("Location: (",S[reg.ind,][3,1], "°E,", S[reg.ind,][3,2], "°N)")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5))

p4 <- ggplot(data) +  
  stat_qq(aes(sample = sample.4), color = "slateblue2", size = 1) +
  stat_qq_line(aes(sample = sample.4),color = "red", linetype = "dashed", size = 0.5) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
       title = paste0("Location: (",S[reg.ind,][4,1], "°E,", S[reg.ind,][4,2], "°N)")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5))

library(patchwork)

p1 + p2 + p3 + p4 + plot_layout(ncol = 2)

# stationarity test -- -- -- -- 

ser.1 <- all.data[,reg.ind[1]]
acf.1 <- acf(ser.1, plot = F, lag.max = 30)
sig.1 <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(ser.1)))
p1 <- ggplot() +
  geom_segment(aes(x = acf.1$lag, xend = acf.1$lag,
                   y = acf.1$acf, yend = 0)) +
  geom_point(aes(x = acf.1$lag, y = acf.1$acf), col = "red", size = 0.8) +
  geom_hline(aes(yintercept = sig.1), col = "blue", lty = 2) + 
  geom_hline(aes(yintercept = - sig.1), col = "blue", lty = 2) + 
  geom_hline(aes(yintercept = 0)) + 
  labs(x = "Lag", y = "ACF",
       title = paste0("Location : (",S[reg.ind,][1,1],
                      "°E,", S[reg.ind,][1,2], "°N)")) + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))

ser.2 <- all.data[,reg.ind[2]]
acf.2 <- acf(ser.2, plot = F, lag.max = 30)
sig.2 <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(ser.2)))
p2 <- ggplot() +
  geom_segment(aes(x = acf.2$lag, xend = acf.2$lag,
                   y = acf.2$acf, yend = 0)) +
  geom_point(aes(x = acf.2$lag, y = acf.2$acf), col = "red", size = 0.8) +
  geom_hline(aes(yintercept = sig.2), col = "blue", lty = 2) + 
  geom_hline(aes(yintercept = - sig.2), col = "blue", lty = 2) + 
  geom_hline(aes(yintercept = 0)) + 
  labs(x = "Lag", y = "ACF",
       title = paste0("Location : (",S[reg.ind,][2,1],
                      "°E,", S[reg.ind,][2,2], "°N)")) + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))

ser.3 <- all.data[,reg.ind[3]]
acf.3 <- acf(ser.3, plot = F, lag.max = 30)
sig.3 <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(ser.3)))
p3 <- ggplot() +
  geom_segment(aes(x = acf.3$lag, xend = acf.3$lag,
                   y = acf.3$acf, yend = 0)) +
  geom_point(aes(x = acf.3$lag, y = acf.3$acf), col = "red", size = 0.8) +
  geom_hline(aes(yintercept = sig.3), col = "blue", lty = 2) + 
  geom_hline(aes(yintercept = - sig.3), col = "blue", lty = 2) + 
  geom_hline(aes(yintercept = 0)) + 
  labs(x = "Lag", y = "ACF",
       title = paste0("Location : (",S[reg.ind,][3,1],
                      "°E,", S[reg.ind,][3,2], "°N)")) + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))

ser.4 <- all.data[,reg.ind[4]]
acf.4 <- acf(ser.4, plot = F, lag.max = 30)
sig.4 <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(ser.4)))
p4 <- ggplot() +
  geom_segment(aes(x = acf.4$lag, xend = acf.4$lag,
                   y = acf.4$acf, yend = 0)) +
  geom_point(aes(x = acf.4$lag, y = acf.4$acf), col = "red", size = 0.8) +
  geom_hline(aes(yintercept = sig.4), col = "blue", lty = 2) + 
  geom_hline(aes(yintercept = - sig.4), col = "blue", lty = 2) + 
  geom_hline(aes(yintercept = 0)) + 
  labs(x = "Lag", y = "ACF",
       title = paste0("Location : (",S[reg.ind,][4,1],
                      "°E,", S[reg.ind,][4,2], "°N)")) + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))
  
p1 + p2 + p3 + p4

## =============================================================================
## Creating the spatial covariates

# -- 
# y.bar.ij.1 and y.bar.ij.2

near.loc.store <- list()
near.loc.1.count <- numeric(length = n.s)
near.loc.2.count <- numeric(length = n.s)
for (j in 1:n.s){
  near.loc.store[[j]] <- func.near(j)
  near.loc.1.count[j] <- length(near.loc.store[[j]][[1]])
  near.loc.2.count[j] <- length(near.loc.store[[j]][[2]])
}

# -- -- -- -- (Part of EDA) -- -- -- --
# justification for taking overall means of near-1 and near-2  locations

ggplot(mapping = aes(x = near.loc.1.count, y = ..prop..)) +
  geom_bar(fill = "darkcyan") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..),
            stat = "count", vjust = -0.5, size = 3.5) +
  labs(y = "Percenntage",
       x = "Near-1-Locations") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggplot(mapping = aes(x = near.loc.2.count, y = ..prop..)) +
  geom_bar(fill = "darkcyan") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..),
            stat = "count", vjust = -0.5, size = 2.5) +
  labs(y = "Percenntage",
       x = "Near-2-Locations") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# -- -- -- -- -- -- -- -- -- 
# spatial covariates

# Y.bar.ij.1 and Y.bar.ij.2

Y.bar.ij.1 <- matrix(0, ncol = n.s, nrow = n.t)
Y.bar.ij.2 <- matrix(0, ncol = n.s, nrow = n.t)

for (i in 1:n.t){
  Y.i <- all.data[i,]
  for (j in 1:n.s){
    loc.1.j <- near.loc.store[[j]][[1]]
    loc.2.j <- near.loc.store[[j]][[2]]
    
    Y.bar.ij.1[i,j] <- mean(Y.i[loc.1.j])
    Y.bar.ij.2[i,j] <- mean(Y.i[loc.2.j])
  }
}

# -- 
# y.bar.i.1 and y.bar.i.1

Y.bar.i.1 <- apply(Y.bar.ij.1, 1, mean)
Y.bar.i.2 <- apply(Y.bar.ij.2, 1, mean)

## =============================================================================
## storing data for model

ind.train <- 1:100

Y.ij.train <- all.data[ind.train, ]
Y.bar.ij.1.train <- Y.bar.ij.1[ind.train,]
Y.bar.ij.2.train <- Y.bar.ij.2[ind.train,]
Y.bar.i.1.train <- Y.bar.i.1[ind.train]
Y.bar.i.2.train <- Y.bar.i.2[ind.train]

save(Y.ij.train, Y.bar.ij.1.train, Y.bar.ij.2.train, Y.bar.i.1.train, 
     Y.bar.i.2.train, file = "03 Data for model.train.Rdata")

# -- --

ind.test <- 101:122

Y.ij.test <- all.data[ind.test, ]
Y.bar.ij.1.test <- Y.bar.ij.1[ind.test,]
Y.bar.ij.2.test <- Y.bar.ij.2[ind.test,]
Y.bar.i.1.test <- Y.bar.i.1[ind.test]
Y.bar.i.2.test <- Y.bar.i.2[ind.test]

save(S, Y.ij.test, Y.bar.ij.1.test, Y.bar.ij.2.test, Y.bar.i.1.test, 
     Y.bar.i.2.test, file = "03 Data for model.test.Rdata")
