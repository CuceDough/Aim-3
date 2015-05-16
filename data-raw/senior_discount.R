## compute vaccination scenarios

require(data.table)

senior_sp_reduction <- .70 ## from cite what?

VE_hom <- c(
  sp=.80,
  s = .40,
  p = .67
) ## from cite what?

VE_het <- c(
  sp=.40,
  s = .30,
  p = .14
) ## from cite what?

s_and_p_reduction <- function(VE, s.d = senior_sp_reduction) {
  ## assume reduction for VE_s, VE_p the same, solve quadratic eqn
  a <- -VE["s"]*VE["p"]; b <- VE["s"]+VE["p"]; c <- -s.d*VE["sp"]
  (-b + sqrt(b^2 - 4*a*c))/(2*a)
}

hom <- s_and_p_reduction(VE_hom)*VE_hom[-1]
het <- s_and_p_reduction(VE_het)*VE_het[-1]

senior_VE = data.table(
  scenario=factor(c('homologous','heterologous')),
  VE_s = c(hom["s"],het["s"]),
  VE_p = c(hom["p"],het["p"]),
  key = "scenario"
)

require(devtools)

devtools::use_data(senior_VE, overwrite = T)
write.csv(senior_VE, file="./data-raw/senior_VE.csv", row.names = F)
