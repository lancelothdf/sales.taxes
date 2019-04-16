#' Author: John Bonney
#'
#' I am testing the Freyaldenhoven, Hansen, and Shapiro (2019) method for
#'    controlling for event study pre-trends.

library(data.table)
library(lfe)
library(ggplot2)

rm(list=ls())
set.seed(1001)
N_i <- 1000

## define primitives, following Section 3.1 DGP
lambda <- 1
rho <- 1
beta <- 1
zeta_sd <- 1
u_sd <- 4
eta_star <- 4

## skeleton
dt <- data.table(expand.grid(
  year = 1:20,
  unit = 1:N_i
), key = "unit")

## add fixed effects for state
alphas <- data.table(unit = 1:N_i, alpha = rnorm(N_i), key = "unit")
dt <- dt[alphas]

dt[, zeta := rnorm(nrow(dt), mean = 0, sd = zeta_sd)]
dt[year == 1, eta := 0]
setkey(dt, unit, year)
for (yr in 2:20) {
  dt[, eta := ifelse(year == yr, shift(eta, type="lag") * rho + zeta, eta), by = unit]
}
dt[, z := as.integer(eta > eta_star)]
dt[, z := cummax(z), by = unit]
dt[, u := rnorm(nrow(dt), mean = 0, sd = u_sd)]
dt[, x := lambda * eta + u]

dt[, epsilon := rnorm(nrow(dt), mean = 0, sd = 1)]

dt[, y := beta * z + 0.25 * eta + 0.2 * year + alpha + epsilon]

## Preparing for estimation
dt[, `:=` (
  l6.z = shift(z, n=6, type="lag"),
  l5.z = shift(z, n=5, type="lag"),
  l4.z = shift(z, n=4, type="lag"),
  l3.z = shift(z, n=3, type="lag"),
  l2.z = shift(z, n=2, type="lag"),
  l1.z = shift(z, n=1, type="lag"),
  f1.z = shift(z, n=1, type="lead"),
  f2.z = shift(z, n=2, type="lead"),
  f3.z = shift(z, n=3, type="lead"),
  f4.z = shift(z, n=4, type="lead"),
  f5.z = shift(z, n=5, type="lead")
), by = unit]

dt[, `:=` (
  D.l5.z = l5.z - l6.z,
  D.l4.z = l4.z - l5.z,
  D.l3.z = l3.z - l4.z,
  D.l2.z = l2.z - l3.z,
  D.l1.z = l1.z - l2.z,
  D.event.z = z - l1.z,
  D.f1.z = f1.z - z,
  D.f2.z = f2.z - f1.z,
  D.f3.z = f3.z - f2.z,
  D.f4.z = f4.z - f3.z,
  D.f5.z = f5.z - f4.z,
  D.f6.z = 1 - f5.z
), by = unit]

## Estimation, following equation (13) and including eta (infeasible)

fhs_formula13 <- y ~ l6.z + D.l5.z + D.l4.z + D.l3.z + D.l2.z + D.l1.z + D.event.z +
  D.f2.z + D.f3.z + D.f4.z + D.f5.z + D.f6.z + eta | year + unit | 0 | unit

res <- felm(data = dt[year %in% 6:15], formula = as.formula(fhs_formula13))

coefs.13 <- data.table(coef(summary(res)), keep.rownames = T)
coefs.13 <- merge(coefs.13, data.table(
  rn = c("l6.z", "D.l5.z", "D.l4.z", "D.l3.z", "D.l2.z", "D.event.z",
    "D.l1.z", "D.f2.z", "D.f3.z", "D.f4.z", "D.f5.z", "D.f6.z"
  ),
  tt_event = c(6, 5, 4, 3, 2, 0, 1, -2, -3, -4, -5, -6)))

ggplot(data = coefs.13, mapping = aes(x = tt_event, y = Estimate)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(aes(ymax = Estimate + 1.96 * `Cluster s.e.`,
                    ymin = Estimate - 1.96 * `Cluster s.e.`),
                width = .8) +
  geom_point(aes(x = -1, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 14) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-2.5, 2.5), breaks = seq(-2.5, 2.5, .5)) +
  scale_x_continuous(breaks = -6:6, labels = c("-6+", as.character(-5:-1), "event", as.character(1:5), "6+")) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.minor = element_blank())

## Estimation, excluding eta

fhs_formula13.2 <- y ~ l6.z + D.l5.z + D.l4.z + D.l3.z + D.l2.z + D.l1.z + D.event.z +
  D.f2.z + D.f3.z + D.f4.z + D.f5.z + D.f6.z | year + unit | 0 | unit

res2 <- felm(data = dt[year %in% 6:15], formula = fhs_formula13.2)

coefs.13.2 <- data.table(coef(summary(res2)), keep.rownames = T)
coefs.13.2 <- merge(coefs.13.2, data.table(
  rn = c("l6.z", "D.l5.z", "D.l4.z", "D.l3.z", "D.l2.z", "D.event.z",
         "D.l1.z", "D.f2.z", "D.f3.z", "D.f4.z", "D.f5.z", "D.f6.z"
  ),
  tt_event = c(6, 5, 4, 3, 2, 0, 1, -2, -3, -4, -5, -6)))

ggplot(data = coefs.13.2, mapping = aes(x = tt_event, y = Estimate)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(aes(ymax = Estimate + 1.96 * `Cluster s.e.`,
                    ymin = Estimate - 1.96 * `Cluster s.e.`),
                width = .8) +
  geom_point(aes(x = -1, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 14) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-2.5, 2.5), breaks = seq(-2.5, 2.5, .5)) +
  scale_x_continuous(breaks = -6:6, labels = c("-6+", as.character(-5:-1), "event", as.character(1:5), "6+")) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.minor = element_blank())

## Estimation, adding x as a proxy for eta

fhs_formula13.3 <- y ~ l6.z + D.l5.z + D.l4.z + D.l3.z + D.l2.z + D.l1.z + D.event.z +
  D.f2.z + D.f3.z + D.f4.z + D.f5.z + D.f6.z + x | year + unit | 0 | unit

res3 <- felm(data = dt[year %in% 6:15], formula = fhs_formula13.3)

coefs.13.3 <- data.table(coef(summary(res3)), keep.rownames = T)
coefs.13.3 <- merge(coefs.13.3, data.table(
  rn = c("l6.z", "D.l5.z", "D.l4.z", "D.l3.z", "D.l2.z", "D.event.z",
         "D.l1.z", "D.f2.z", "D.f3.z", "D.f4.z", "D.f5.z", "D.f6.z"
  ),
  tt_event = c(6, 5, 4, 3, 2, 0, 1, -2, -3, -4, -5, -6)))

ggplot(data = coefs.13.3, mapping = aes(x = tt_event, y = Estimate)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(aes(ymax = Estimate + 1.96 * `Cluster s.e.`,
                    ymin = Estimate - 1.96 * `Cluster s.e.`),
                width = .8) +
  geom_point(aes(x = -1, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 14) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-2.5, 2.5), breaks = seq(-2.5, 2.5, .5)) +
  scale_x_continuous(breaks = -6:6, labels = c("-6+", as.character(-5:-1), "event", as.character(1:5), "6+")) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.minor = element_blank())

## Estimation via proposed 2SLS estimator (with closest lead of z as excluded instrument)

fhs_formula13.4 <- y ~ l6.z + D.l5.z + D.l4.z + D.l3.z + D.l2.z + D.l1.z + D.event.z +
  D.f3.z + D.f4.z + D.f5.z + D.f6.z | year + unit | (x ~ f1.z) | unit

res4 <- felm(data = dt[year %in% 6:15], formula = fhs_formula13.4)

coefs.13.4 <- data.table(coef(summary(res4)), keep.rownames = T)
coefs.13.4 <- merge(coefs.13.4, data.table(
  rn = c("l6.z", "D.l5.z", "D.l4.z", "D.l3.z", "D.l2.z", "D.event.z",
         "D.l1.z", "D.f2.z", "D.f3.z", "D.f4.z", "D.f5.z", "D.f6.z"
  ),
  tt_event = c(6, 5, 4, 3, 2, 0, 1, -2, -3, -4, -5, -6)))

ggplot(data = coefs.13.4, mapping = aes(x = tt_event, y = Estimate)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(aes(ymax = Estimate + 1.96 * `Cluster s.e.`,
                    ymin = Estimate - 1.96 * `Cluster s.e.`),
                width = .8) +
  geom_point(aes(x = -1, y = 0), size = 2, color = "black") +
  geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 14) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-2.5, 2.5), breaks = seq(-2.5, 2.5, .5)) +
  scale_x_continuous(breaks = -6:6, labels = c("-6+", as.character(-5:-1), "event", as.character(1:5), "6+")) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.minor = element_blank())
