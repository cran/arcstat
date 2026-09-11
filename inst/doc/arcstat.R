## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(arcstat)

## -----------------------------------------------------------------------------
o <- arcq(coef = c(0.5, -0.3), mu = 0, sigma = 1)
arclength(o)

## -----------------------------------------------------------------------------
c(at_zero = arclength(arcq(0.5, mu = 0)), at_seventeen = arclength(arcq(0.5, mu = 17)))

## -----------------------------------------------------------------------------
vapply(c(8L, 16L, 32L, 64L), function(k) arclength(o, nodes = k), 0)

## -----------------------------------------------------------------------------
set.seed(1)
al_test(runif(200))$p.value                       # null holds
al_test(c(runif(100), rbeta(100, 8, 8)))$p.value  # a clustered middle

## -----------------------------------------------------------------------------
c(normal = cf_arclength_family("normal"),
  exponential = cf_arclength_family("exponential", lambda = 1),
  pi = pi)

## -----------------------------------------------------------------------------
c(rate_1 = cf_arclength_family("exponential", lambda = 1),
  rate_2 = cf_arclength_family("exponential", lambda = 2))

## -----------------------------------------------------------------------------
set.seed(3)
cf_arclength(rnorm(50000))

## -----------------------------------------------------------------------------
al <- -0.60; be <- -0.35
ub <- eq_ub_quad(al, be)[1]
gp  <- al / ub - be / (1 - ub)
gpp <- -al / ub^2 - be / (1 - ub)^2
c(shoulder = ub, residual_of_original_equation = abs(2 * gp^2 - gpp))

## -----------------------------------------------------------------------------
als <- seq(-0.62, -0.54, by = 0.02)
bss <- vapply(als, eq_bstar, 0)
rbind(alpha = als, beta_star = round(bss, 6))
c(discrepancy_on_the_curve = eq_E(-0.60, eq_bstar(-0.60)))

## -----------------------------------------------------------------------------
u <- (seq_len(20000) - 0.5) / 20000
sample_lmoments(u, 4L)[1:2]

