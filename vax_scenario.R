## compute vaccination scenarios

require(data.table)

baseline <- c("0-4"=.38, "5-18"=.26, "19-29"=.09, "30-64"=.18, "65+"=.5) ## from cite what?
baseline_iiv_rate <- .6 ## from cite what?

baseline_iiv <- c(rep(baseline_iiv_rate, 4), 1)*baseline
baseline_laiv <- c(rep(1-baseline_iiv_rate, 4), 0)*baseline

background <- data.table(AgeGroup = names(baseline[-2]), LAIV=baseline_laiv[-2], IIV=baseline_iiv[-2])

prop_LAIV <- 10/11 ## from cite what?

SLIV_min <- .3
SLIV_max <- .85
SLIV_by <- 0.05

SLIV_seq <- seq(SLIV_min, SLIV_max, SLIV_by)

rate_laiv <- c(0, (SLIV_seq-baseline[2])*prop_LAIV) + baseline_laiv[2]
rate_iiv <- c(0, (SLIV_seq-baseline[2])*(1-prop_LAIV)) + baseline_iiv[2]

scenarios <- data.table(LAIV=rate_laiv, IIV=rate_iiv, total = rate_laiv + rate_iiv)

seniorDiscount <- .70 ## from cite what?

sen_VE_hom <- c(sp=.80, s = .40, p = .67)
sen_VE_het <- c(sp=.40, s = .30, p = .14)

alpha <- function(VE_sp = .80, VE_s = .40, VE_p = .67, s.d = seniorDiscount) {
  qa <- -VE_s*VE_p; qb <- VE_s+VE_p; qc <- -s.d*VE_sp
  (-qb+c(-1,1)*sqrt(qb^2 - 4*qa*qc))/(2*qa)
}

hom <- alpha()[2]*sen_VE_hom
het <- alpha(.40, .30, .14)[2]*sen_VE_het

