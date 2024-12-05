rm(list = ls())
load("03 Data for model.train.Rdata")

set.seed(100)

library(LaplacesDemon)
library(mvtnorm)

n.s <- ncol(Y.ij.train)
n.t <- nrow(Y.ij.train)
p <- 4

# creating X.j and Y.j
X.j <- list()
Y.j <- list()
df.j <- list()
for (j in 1:n.s){
  Y.j[[j]] <- Y.ij.train[,j]
  X.j[[j]] <- matrix(c(Y.bar.ij.1.train[,j],Y.bar.ij.2.train[,j],
                       Y.bar.i.1.train, Y.bar.i.2.train), ncol = p)
  foo <- cbind(Y.j[[j]], X.j[[j]])
  df.j[[j]] <- as.data.frame(foo)
}

library(Matrix)
X.all.bdiag <- as.matrix(bdiag(X.j))

# getting hyper parameters
beta.j.store <- matrix(0, n.s, p)
res.j.store <- numeric(length = n.s)
for (j in 1:n.s){
  lm.j <- lm(data = df.j[[j]], V1 ~ 0 + .)
  beta.j.store[j,] <- unname(lm.j$coefficients)
  res.j.store[j] <- anova(lm.j)['Residuals','Mean Sq']
}

theta.hat <- colMeans(beta.j.store)
Sigma.hat <- cov(beta.j.store)
sigma.sq.hat <- mean(res.j.store)

mu.0 <- theta.hat
Lambda.0 <- Sigma.hat
eta.0 <- p + 2
S.0 <- Sigma.hat
nu.0 <- 2
sigma.sq.0 <- sigma.sq.hat

Lambda.0.inv <- solve(Lambda.0)
Lambda.0.inv.times.mu.0 <- Lambda.0.inv %*% mu.0

# initial values
theta <- as.vector(rmvnorm(1, mu.0, Lambda.0))
Sigma <- rinvwishart(eta.0, S.0)
beta <- rmvnorm(n.s, theta, Sigma)
sigma.sq <- 1 / rgamma(1, nu.0/2, nu.0 * sigma.sq.0 / 2)

# MCMC length and burn-in
iter <- 1e4
burn <- 4e3

# storage for parameters
store.beta <- array(0, dim = c(iter, n.s, p))
store.theta <- matrix(0, ncol = p, nrow = iter)
store.Sigma <- array(0,  dim = c(iter, p, p))
store.sigma.sq <- numeric(length = iter)
store.DIC <- numeric(length = iter)

for (i in 1:iter){

  Sigma.inv <- solve(Sigma)

  #updating beta

  for (j in 1:n.s){
    beta.j.par.2 <- solve(Lambda.0.inv + t(X.j[[j]]) %*% X.j[[j]] / sigma.sq)
    beta.j.par.1 <- beta.j.par.2 %*% ( Sigma.inv %*% theta + t(X.j[[j]]) %*% Y.j[[j]] / sigma.sq)

    beta[j, ] <- rmvnorm(1, beta.j.par.1, beta.j.par.2)
  }

  #updating theta
  beta.bar <- colMeans(beta)

  theta.par.2 <- solve(Lambda.0.inv + n.s * Sigma.inv)
  theta.par.1 <- theta.par.2 %*% (Lambda.0.inv.times.mu.0 + n.s * Sigma.inv %*% beta.bar)

  theta <- as.vector(rmvnorm(1, theta.par.1, theta.par.2))

  #updating Sigma
  diff <- beta - matrix(rep(theta, each = nrow(beta)), nrow = nrow(beta))
  S.theta <- t(diff) %*% diff

  Sigma <- rinvwishart(eta.0 + n.s, S.0 + S.theta)

  #updating sigma.sq
  foo <- matrix(X.all.bdiag %*% as.vector(t(beta)), ncol = 357)
  SSR <- sum( (Y.ij.train - foo)^2)
  sigma.sq <- 1 / rgamma(1, (nu.0 + n.t * n.s) / 2, (nu.0 * sigma.sq.hat + SSR)/2)

  #storing
  store.beta[i, , ] <- beta
  store.theta[i, ] <- theta
  store.Sigma[i, , ] <- Sigma
  store.sigma.sq[i] <- sigma.sq
  store.DIC[i] <- SSR / sigma.sq

  print(paste0("Iteration ", i, " of Model 1."))
}

keep.ind <- (burn+1) : iter
samp <- list(beta = store.beta[keep.ind, , ],
             theta = store.theta[keep.ind, ],
             Sigma = store.Sigma[keep.ind, , ],
             sigma.sq = store.sigma.sq[keep.ind],
             DIC = store.DIC[keep.ind])

save(samp, file = "05 Model Fit_model.Rdata")
