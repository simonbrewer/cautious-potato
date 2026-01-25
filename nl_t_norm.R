## GRF with treatment in features (second stage)
## Normally distributed W

library(grf)
library(hstats)

set.seed(42)
n <- 10000 ## n samples
p <- 4 ## p features

## Make random features
X <- matrix(rnorm(n * p, sd = 0.1), n, p)
# X <- matrix(0, n, p)

## Needed for predictions
X_test0 <- matrix(0, 101, p)
X_test1 <- cbind(X_test0, seq(-3, 3, length.out = 101))
# X_test[, 1] <- seq(-2, 2, length.out = 101)

## Make random treatment (cont)
W <- rnorm(n, mean = 0)
# W <- (W - min(W)) / (max(W) - min(W))
# W <- runif(n)

## Make outcome: nonlinear treatment effect only
w_eff <- 2
Y <- pmax(W, 0) * w_eff

# Y <- W * w_eff
# Y <- 0

## Noise on Y
Y_e <- rnorm(n, sd = 0.1)
Y <- Y + Y_e 

## Plot treatment 'effect'
plot(W, Y)

## ---------------------------------------------
## Fit causal forest (usual)
## ---------------------------------------------
## In stages
colnames(X) <- c("X0", "X1", "X2", "X3")

f_W <- regression_forest(X, W)
W_hat <- predict(f_W)$predictions

f_Y <- regression_forest(X, Y)
Y_hat <- predict(f_Y)$predictions

f_tau0 <- causal_forest(X, Y, W,
                            W.hat = W_hat, Y.hat = Y_hat)
f_tau0
average_treatment_effect(f_tau0)

## importance
imp <- sort(setNames(variable_importance(f_tau0), colnames(X)))
par(mai = c(0.7, 2, 0.2, 0.2))
barplot(imp, horiz = TRUE, las = 1, col = "orange")

## pdp
plot(partial_dep(f_tau0, "X3", X = X))

## ---------------------------------------------
## Fit causal forest (W in X)
## ---------------------------------------------
X_W = cbind(X, W)
colnames(X_W) <- c("X0", "X1", "X2", "X3", "W")
# X_W = as.matrix(W)
# colnames(X_W) <- c("W")

f_tau1 <- causal_forest(X_W, Y, W,
                        W.hat = W_hat, Y.hat = Y_hat)
f_tau1
average_treatment_effect(f_tau1)

## importance
imp <- sort(setNames(variable_importance(f_tau1), colnames(X_W)))
par(mai = c(0.7, 2, 0.2, 0.2))
barplot(imp, horiz = TRUE, las = 1, col = "orange")

## pdp
plot(partial_dep(f_tau1, "W", X = X_W)) +
  scale_y_continuous(limits = c(0, 2)) + 
  theme_minimal()

## 
tau_hat = predict(f_tau1)$predictions

## Reconstruct
tau_hat = predict(f_tau1, X_test1)$predictions
y_hat = tau_hat * X_test1[,5] 

## Plot
plot(W, Y)
lines(X_test1[,5], y_hat, lwd = 3, col = "orange")

# high_effect = tau_hat > median (tau_hat)
# ate_high = average_treatment_effect(f_tau1, subset = high_effect)
# ate_high
# ate_low = average_treatment_effect(f_tau1, subset = !high_effect)
# ate_low
