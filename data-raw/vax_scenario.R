## compute vaccination scenarios
require(data.table)
## TODO require(arg.parse) or similar, use below as defaults but otherwise allow custom input

baseline <- c(
  "0-4" = 0.38,
  "5-18" = 0.26,
  "19-29" = 0.09,
  "30-64" = 0.18,
  "65+" = 0.50
)
## from cite what?

baseline_tiv_rate <- .6 ## from cite what?

baseline_tiv <- c(rep(baseline_tiv_rate, 4), 1)*baseline
baseline_laiv <- c(rep(1-baseline_tiv_rate, 4), 0)*baseline

background <- data.table(
  AgeGroup = factor(names(baseline[-2]), levels=names(baseline)),
  LAIV=baseline_laiv[-2],
  TIV=baseline_tiv[-2],
  key="AgeGroup"
)

prop_LAIV <- 10/11 ## as SLIV rate increases, what proportion is LAIV vs TIV.
## from cite what?

SLIV_min <- .3
SLIV_max <- .8
SLIV_by <- 0.05

SLIV_seq <- seq(SLIV_min, SLIV_max, SLIV_by)

rate_laiv <- c(0, (SLIV_seq-baseline[2])*prop_LAIV) + baseline_laiv[2]
rate_tiv <- c(0, (SLIV_seq-baseline[2])*(1-prop_LAIV)) + baseline_tiv[2]

scenarios <- data.table(scenario=0:length(SLIV_seq), LAIV=rate_laiv, TIV=rate_tiv,key = "scenario")

stopifnot(all.equal(scenarios[,LAIV + TIV], c(baseline[2], SLIV_seq), tol=10^-10, check.names=F))

require(devtools)
devtools::use_data(background, scenarios, overwrite = T)
