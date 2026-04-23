set.seed(42)
library(mgcv)
library(gratia)
library(ggeffects)

n_samples = 10000

n_X = 4

## Features
X = matrix(rnorm((n_samples * n_X)), ncol = n_X)

## Treatments
T = rnorm(n_samples)

## ---------------------------------------------
## Make outcome: linear treatment effect only
w_eff = 2
y = w_eff * T # * (T > 0)

## Noise on Y
y_e = rnorm(n_samples, 0.0, 0.1)
y = y + y_e 

plot(T, y)

## ---------------------------------------------
## Make outcome: nonlinear treatment effect
## Linear X[,1]

w_eff = 2
x_eff = 1.5
# y = w_eff * T * (T > 0) + X[,1] * x_eff
y = w_eff * T + X[,1] * x_eff


## Noise on Y
y_e = rnorm(n_samples, 0.0, 0.1)
y = y + y_e 

plot(X[,1], y)
plot(T, y)

## ---------------------------------------------
## Put all data together
df = data.frame(X, T, y)

## ---------------------------------------------
## FWL approach
mod_t = lm(T ~ X1 + X2 + X3 + X4, df)
df$e_t = residuals(mod_t)

mod_y = lm(y ~ X1 + X2 + X3 + X4, df)
df$e_y = residuals(mod_y)

mod_final_lm1 = lm(e_y ~ e_t, df)
mod_final_gam1 = gam(e_y ~ s(e_t), gaussian(), df)

plot(ggpredict(mod_final_gam1))

# Estimate derivatives of smooths
derivs1 <- derivatives(mod_final_gam1, type = "central")

# Plot the derivatives
draw(derivs1)

## ---------------------------------------------
## Full approach
mod_final_lm2 = lm(y ~ T + X1 + X2 + X3 + X4, df)
mod_final_gam2 = gam(y ~ s(T) + X1 + X2 + X3 + X4, gaussian(), df)

plot(ggpredict(mod_final_gam2))

# Estimate derivatives of smooths
derivs2 <- derivatives(mod_final_gam2, type = "central")

# Plot the derivatives
draw(derivs2)

plot_df = data.frame(model = c(derivs1$.smooth, derivs2$.smooth),
                     T = c(derivs1$e_t, derivs2$T),
                     derivative = c(derivs1$.derivative, derivs2$.derivative),
                     ci_lo = c(derivs1$.lower_ci, derivs2$.lower_ci),
                     ci_hi = c(derivs1$.upper_ci, derivs2$.upper_ci))

library(ggplot2)
ggplot(plot_df, aes(x = T)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = model), alpha = 0.5) +
  geom_line(aes(y = derivative, col = model)) + 
  theme_minimal()
