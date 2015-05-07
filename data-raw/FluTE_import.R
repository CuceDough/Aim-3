require(data.table); require(reshape2)

src.dt <- fread("./data-raw//runs200.csv")
src.dt[, run_id := as.numeric(gsub(".+-(\\d+)$", "\\1", label))]
src.dt$file <- src.dt$label <- src.dt$int <- src.dt$delay <- src.dt$antiviralsused <- src.dt$seed <- NULL
src.dt[, SLIVlvl := gsub("(hom|het)","", scenario) ]
src.dt[, scenario := gsub("\\d+","", scenario) ]
src.dt[, scenario := factor(ifelse(scenario == "hom","homologous","heterologous"))]

overview.dt <- src.dt[,
  list(ill, peak_ill = peakill, peak_day = peaktime),
  keyby = list(scenario, r, SLIVlvl, run_id)
]
