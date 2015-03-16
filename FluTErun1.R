##################################### 2.26.15
# Dennis' FluTE data fom "Run file, email 2.26.15
# Cost Effective Paper Aim 3
############################################################################
# turn off scientific notation
options(scipen=999)

#####
# SET WORKING DIRECTORY CONDITIONAL TO SYSTEM
#####
# if ( Sys.info()["sysname"] == "Linux" ){
#   cf <- "/home/joebrew/Documents/controlflu"
#   linux_path <- "/media/joebrew/JB/fdoh/private/"
#   acps_path <-  "/media/joebrew/JB/fdoh/private/acps/" #"E:/fdoh/private/acps/"
# } else if( Sys.info()["user"] == "BrewJR"){
#   cf <- "C:/Users/BrewJR/Documents/controlflu"
#   linux_path <- "E:/fdoh/private/"
#   acps_path <-  "E:/fdoh/private/acps/"
# } else if( Sys.info()["user"] == "C"){
#   cf <- "C:/Users/C/Documents/controlflu"
#   linux_path <- NULL
#   acps_path <-  NULL
# }
# setwd(cf) 

#####################################################################################################
#####################################################################################################

# #Weycker param
# dzweyker <- read.csv("data/Cost Effect/dzparamweycker.csv")
# dzweyker.dt <- data.table(dzweyker) 

require(data.table)
require(ggplot2)
require(reshape2)

# Reading in raw data 

#2013 population data 
loadDemoData <- function(target = "Agegroupop.csv") {
  agedata <- data.table(read.csv(target))
  setnames(agedata, c("AgeGroup","Population"))
  newLevels <- sub("to","-", levels(agedata$AgeGroup))
  newLevels <- sub("^(\\d+)$","\\1+", newLevels)
  levels(agedata$AgeGroup) <- newLevels
  newOrder <- newLevels[order(as.numeric(sub("^(\\d+).+","\\1", newLevels)))]
  agedata[,AgeGroup := factor(AgeGroup, levels=newOrder)]
  high_risk_proportion <- c(.052, .106, .149, .330, .512) ## by age categories
  temp <- rbind(
    agedata[,list(AgeGroup,
                  population = Population*high_risk_proportion, 
                  risk=factor("high",levels=c("normal","high"))
    )],
    agedata[,list(AgeGroup,
                  population = Population*(1-high_risk_proportion),
                  risk=factor("normal",levels=c("normal","high"))
    )]
  )
  setkey(temp, AgeGroup, risk)
  temp
}

agegroup <- loadDemoData() 

## to recover Population: agegroup[,list(Population = sum(population)),by=AgeGroup]


#FluTE Run 2.27.15
loadSimData <- function(
  target = "runs.csv", referenceDemo = agegroup, scenarioNames = c("hom","het","base"),
  no_sliv_rate = 0.26, min_sliv_rate = 0.3, max_sliv_rate = 0.8
) {
  run.melt <- melt(data.table(read.csv("runs.csv")), variable.name="AgeGroup", value.name="AttackRate", id.vars=c("scenario","r"))
  ## might want to realign levels for certainty in this next step, but in this case we know they are attack0, attack1, etc
  levels(run.melt$AgeGroup) <- levels(referenceDemo$AgeGroup)
  ignore <- paste0("(",paste(scenarioNames,collapse="|"),")")
  run.melt[, vax := as.numeric(sub(ignore,"",as.character(scenario)))]
  run.melt[, scenario := factor(sub("\\d+","", as.character(scenario)))]
  

  vax_sequence <- c(no_sliv_rate, seq(from=min_sliv_rate, to=max_sliv_rate, length.out = max(run.melt$vax, na.rm=T)))
  
  run.melt[!is.na(vax), vax_rate := vax_sequence[vax+1]]
  run.melt[is.na(vax), vax_rate := 0]
  run.melt$vax <- NULL
  setkey(run.melt, scenario, r, vax_rate, AgeGroup)
  run.melt
}

run.melt <- loadSimData()

# run.melt[scenario == "base" & r == 1.2, list(AgeGroup,1:10),by=c("scenario","r","vax_rate")]

createAttackTable <- function(demoData = agegroup, runData = run.melt) {
  res <- merge(run.melt, agegroup, by="AgeGroup", allow.cartesian=T)
  res[,bulk_cases := population*AttackRate]
  setkeyv(res, key(run.melt))
}

attack.dt <- createAttackTable()

target_confidence <- 0.95
lbound <- (1-target_confidence)/2
ubound <- 1 - lbound

measures.dt <- run.melt[, ## n.b., this will be a lot better when we have reasonable sample sizes
  list(
    lower = quantile(AttackRate, lbound),
    median = quantile(AttackRate, 0.5),
    upper = quantile(AttackRate, ubound)
  ), by=c("scenario","r","vax_rate","AgeGroup")
]

write.csv (measures.dt, file= "measures.1.csv")

measures.dttest <- merge(agegroup, measures.dt, by="AgeGroup", allow.cartesian = T) #agegroup[measures.dt, list(pop = AveAttackRate*population), by=c("scenario", "vax", "r", "risk")]
measures.dttest[, pop := population*AveAttackRate]
measures.dttest$Population <- NULL
measures.dttest$population <- NULL
# making similiar data set and subsetting 
play.dt <- measures.dttest 

## to decruft CT code:
## play.dt\[\s*measure\s*==\s*"attack[0-5]"\s*,\s*(\w+)\s*:=\s*([\.\d]+)\] replace w/ $2
## play.dt\$(\w+)\s*=\s*0 replace w/ play.dt[,$1 :=c() ]

## CABP ASIDE: WHERE DO THESE #s COME FROM?

##### PARAMETERS COST of one of the following: 

#OTC (all same price ) 
agegroup$otc = 4.58
agegroup$otc_SD = 3.05
#oupt low risk 
agegroup[risk == "low", oupt := c(255, 145, 191, 229, 369) ]
agegroup[risk == "low", oupt_SD := c(468, 393, 688, 1167, 2353) ]
agegroup[risk == "high", oupt := c(875, 989, 1105, 1117, 725) ]
agegroup[risk == "high", oupt_SD := c(1929, 2274, 2617, 1992, 1724) ]
agegroup[risk == "low", hosp :=c(16580, 22880, 28972, 33989, 17450) ]
agegroup[risk == "low", hosp_SD :=c(55148, 34867, 68022, 145878, 35235) ]
agegroup[risk == "high", hosp :=c(124344, 312736, 72723, 62951, 25525) ]
agegroup[risk == "high", hosp_SD :=c(188393, 76794, 130512, 113984, 48903) ]
agegroup[risk == "low", death :=c(43916, 43916, 116328, 180695, 63924) ]
agegroup[risk == "low", death_SD :=c(37241, 37241, 139671, 508796, 147005) ]
agegroup[risk == "high", death :=c(408333, 408333, 115648, 181102, 50305) ]
agegroup[risk == "high", death_SD :=c(336978, 336978, 99460, 527225, 94335) ]

agegroup[risk == "low", oupt_para := c(0.455, 0.318, 0.313, 0.313, 0.620) ]
agegroup[risk == "high", oupt_para := c(0.910, 0.635, 0.625, 0.625, 0.820) ]
agegroup[risk == "low", hosp_para :=c(0.0141, 0.0006, 0.0042, 0.0193, 0.0421) ]
agegroup[risk == "high", hosp_para :=c(0.0141, 0.0006, 0.0042, 0.0193, 0.0421) ]
agegroup[risk == "low", death_para :=c(0.00004, 0.00001, 0.00009, 0.00134, 0.01170) ]
agegroup[risk == "high", death_para :=c(0.00004, 0.00001, 0.00009, 0.00134, 0.01170) ]
agegroup[risk == "low", otc_para := 1 - oupt_para - hosp_para - death_para ]
agegroup[risk == "high", otc_para := 1 - oupt_para - hosp_para - death_para ]

## n.b., this does not allow for "no treatment"
## also, no analysis of lost productivity?

############################
# Calculations for cases

cases.dt <- merge(agegroup, play.dt, allow.cartesian = T, by=c("AgeGroup","risk"))[, list(scenario, r, vax, AgeGroup, risk,
  otc = otc_para*pop, 
  oupt = oupt_para *pop,
  hosp = hosp_para *pop,
  death = death_para *pop
)]

cases.melt <- melt(cases.dt, id.vars = c("AgeGroup","scenario","r","vax","risk"), variable.name = "event" )

plotdata.dt <- cases.melt[scenario == "hom" & r == 1.4, list(total = sum(value)), by=c("AgeGroup","event","r","vax") ]
plotdata.dt[, vax_rate := c(0.26, seq(0.30, 0.80, by=0.05))[as.numeric(vax)+1]]
plotdata.dt$r <- as.numeric(as.character(plotdata.dt$r))
levels(plotdata.dt$event) <- c("OTC","Outpatient","Hospitalization","Death")

baseplot <- ggplot(plotdata.dt) + theme_bw() + aes(x=vax_rate, y = total/10000)

baseplot + aes(color=r, group=r) +
  facet_grid(event ~ AgeGroup, scales = "free_y") + geom_line() + scale_y_continuous(name="10k cases") +
  scale_color_continuous(low="blue", high="red", name=expression(R[0])) + xlab("Vaccination Rate in 5-18 year olds")

## or

baseplot + aes(fill=AgeGroup, group=r) + 
  facet_grid(event ~ r, scales = "free_y") + geom_bar(stat="identity") + ylab("10k cases") + xlab("Vaccination Rate in 5-18 year olds")


##########################################################
#######################################################

baseplot + aes(fill=AgeGroup) + 
  facet_grid(event ~ ., scales = "free_y") + geom_bar(stat="identity", position = "dodge") + ylab("10k cases") + xlab("Vaccination Rate in 5-18 year olds")


baseplot + aes(fill=AgeGroup) + 
  facet_grid(event ~ r, scales = "free_y") + geom_bar(stat="identity") +
  ylab("10k outcomes") + xlab("Vacc. Proportion in 5-18 year olds")


################################

### Calculations for cost
costs.dt <- merge(agegroup, cases.dt, by=c("AgeGroup","risk"), allow.cartesian = T, suffixes = c(".cost",".cases"))[, list(
  otc = sum(otc.cases * otc.cost), #OTC low risk
  oupt = sum(oupt.cases * oupt.cost), #outpt low risk
  hosp = sum(hosp.cases * hosp.cost), #hosp low risk 
  death = sum(death.cases * death.cost) # death low risk
), by=c("scenario","vax","r","AgeGroup")]

costs.melt <- melt(costs.dt, id.vars = c("AgeGroup","scenario","r","vax"), variable.name = "event", value.name = "cost" )

costplotdata.dt <- costs.melt[scenario == "hom" & r == 1.4]
costplotdata.dt[, vax_rate := c(0.26, seq(0.30, 0.80, by=0.05))[as.numeric(vax)+1]]
costplotdata.dt$r <- as.numeric(as.character(costplotdata.dt$r))
levels(costplotdata.dt$event) <- c("OTC","Outpatient","Hospitalization","Death")

#costs.dt[, medsumIN := oupt_cost + oupt_cost_hr + hosp_cost + hosp_cost_hr +
#           death_cost +  death_cost_hr]

#costs.dt[, medsum := otc_cost + otc_cost_hr + medsumIN]

costbaseplot <- ggplot(costplotdata.dt) + theme_bw() + aes(x=vax_rate, y = cost/1e6) 

costbaseplot + aes(fill=AgeGroup) + 
  facet_grid(r ~ event) + geom_bar(stat="identity", position = "stack") + ylab("1M $") + xlab("Vaccination Rate in 5-18 year olds") + coord_flip()


#########################################################
## Exporting to files to make barcharts

#write.csv (play.dt, file= "play.LA.csv")

temp <- subset(play.dt, group == 1 )
subsetplay<- subset(temp, r == 1.4,  
                   select=c (vax, scenario, measure, otc_cases_t, oupt_cases_t, hosp_cases_t, death_cases_t, medsum, medsumIN))

write.csv(subsetplay, file = "1.4analhom.csv")

#########################
#########################
#Over the counter 
## Need to subset for homo only 

temp <- subset(play.dt, group ==1)

homo_otc_t <- subset(temp, vax <= 11,  
                       select=c (vax, r, measure, otc_cases_t))


## Need to subset for homo only 

temp2 <- subset(play.dt, group ==2)

het_otc_t <- subset(temp2, vax <= 11,  
                     select=c (vax, r, measure, otc_cases_t))

#########################
#########################
#Over the counter 
## Need to subset for homo only 

temp <- subset(play.dt, group ==1)

homo_otc_t <- subset(temp, vax <= 11,  
                     select=c (vax, r, measure, otc_cases_t))


## Need to subset for heter only 

temp2 <- subset(play.dt, group ==2)

het_otc_t <- subset(temp2, vax <= 11,  
                    select=c (vax, r, measure, otc_cases_t))

#############################################################
#Outpt

#homo
temp <- subset(play.dt, group ==1)

homo_oup_t <- subset(temp, vax <= 11,  
                     select=c (vax, r, measure, oupt_cases_t))


## Need to subset for heter only 

temp2 <- subset(play.dt, group ==2)

het_oup_t <- subset(temp2, vax <= 11,  
                    select=c (vax, r, measure, oupt_cases_t))


#################################################################
#Hosp

#homo
temp <- subset(play.dt, group ==1)

homo_hosp_t <- subset(temp, vax <= 11,  
                     select=c (vax, r, measure, hosp_cases_t))


## Need to subset for heter only 

temp2 <- subset(play.dt, group ==2)

het_hosp_t <- subset(temp2, vax <= 11,  
                    select=c (vax, r, measure, hosp_cases_t))

##################################################################
#Death

#homo
temp <- subset(play.dt, group ==1)

homo_death_t <- subset(temp, vax <= 11,  
                     select=c (vax, r, measure, death_cases_t))


## Need to subset for heter only 

temp2 <- subset(play.dt, group ==2)

het_death_t <- subset(temp2, vax <= 11,  
                    select=c (vax, r, measure, death_cases_t))


##############################################################################
#############################################################################
# Illness Burden Graph
## Code to graph panels using lattice, stacked 

# open packages
#library(MASS)
library(lattice)
library(RColorBrewer)


## calibrating the plot and going step by step  
barchart(otc_cases_t ~ measure | r, groups=vax,
         data=homo_otc_t, origin=0, auto.key=TRUE)

######### fixing the legend 
barchart(otc_cases_t ~ measure | r, groups=vax,
         data=homo_otc_t, main="Over the Counter Medicine", 
         auto.key=list(space="top", columns=4, 
                       title="Vax Coverage", cex.title=1))

########## making legend look neater
xyplot(otc_cases_t ~ measure | r, groups=vax,
       data=homo_otc_t, t="l", main="Over the Counter Medicine",
       auto.key=list(space="top", columns=4, 
                     title="Vax Coverage", cex.title=1,
                     lines=TRUE, points=FALSE))

####### colors
myColours <- brewer.pal(12,"Paired")

my.settings <- list(
  superpose.polygon=list(col=myColours[1:12], border="transparent"),
  strip.background=list(col=myColours[8]), # this is the background color bar where R is
  strip.border=list(col="black")
)

#######################################################################
## OTC
###### Whole HomO

barchart(otc_cases_t ~ measure | r, groups=vax,
         data=homo_otc_t, origin=0, 
         main="Over the Counter Medicine", 
         xlab="Age", ylab="Number",
         scales=list(alternating=1),                  
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Vax Coverage", cex.title= 1),
         par.settings = my.settings,
         par.strip.text=list(col="black", font=3),
         panel=function(x,y,...){
           panel.grid(h=-1, v=0); 
           panel.barchart(x,y,...)
         }
)
#######################################################  
#######################################################
# OTC Heter 

barchart(otc_cases_t ~ measure | r, groups=vax,
         data=het_otc_t, origin=0, 
         main="Over the Counter Medicine - Heter", 
         xlab="Age", ylab="Number",
         scales=list(alternating=1),                  
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Vax Coverage", cex.title= 1),
         par.settings = my.settings,
         par.strip.text=list(col="black", font=3),
         panel=function(x,y,...){
           panel.grid(h=-1, v=0); 
           panel.barchart(x,y,...)
         }
)

#############################################################
############################################################
## Outpatient 
###### Whole HomO

barchart(oupt_cases_t ~ measure | r, groups=vax,
         data=homo_oup_t, origin=0, 
         main="Outpatient Visits", 
         xlab="Age", ylab="Number",
         scales=list(alternating=1),                  
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Vax Coverage", cex.title= 1),
         par.settings = my.settings,
         par.strip.text=list(col="black", font=3),
         panel=function(x,y,...){
           panel.grid(h=-1, v=0); 
           panel.barchart(x,y,...)
         }
)

#######################################################
# Out Heter 

barchart(oupt_cases_t ~ measure | r, groups=vax,
         data=het_oup_t, origin=0, 
         main="Outpatient Visits Heter", 
         xlab="Age", ylab="Number",
         scales=list(alternating=1),                  
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Vax Coverage", cex.title= 1),
         par.settings = my.settings,
         par.strip.text=list(col="black", font=3),
         panel=function(x,y,...){
           panel.grid(h=-1, v=0); 
           panel.barchart(x,y,...)
         }
)

##############################################################
###############################################################
#############################################################
############################################################
## Hosp
###### Whole HomO

barchart(hosp_cases_t ~ measure | r, groups=vax,
         data=homo_hosp_t, origin=0, 
         main="Hospitalization Visits", 
         xlab="Age", ylab="Number",
         scales=list(alternating=1),                  
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Vax Coverage", cex.title= 1),
         par.settings = my.settings,
         par.strip.text=list(col="black", font=3),
         panel=function(x,y,...){
           panel.grid(h=-1, v=0); 
           panel.barchart(x,y,...)
         }
)

#######################################################
# Hosp Heter 

barchart(hosp_cases_t ~ measure | r, groups=vax,
         data=het_hosp_t, origin=0, 
         main="Hospitalization Visits Heter", 
         xlab="Age", ylab="Number",
         scales=list(alternating=1),                  
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Vax Coverage", cex.title= 1),
         par.settings = my.settings,
         par.strip.text=list(col="black", font=3),
         panel=function(x,y,...){
           panel.grid(h=-1, v=0); 
           panel.barchart(x,y,...)
         }
)

################################################
################################################

## death
###### Whole HomO

barchart(death_cases_t ~ measure | r, groups=vax,
         data=homo_death_t, origin=0, 
         main="Deaths", 
         xlab="Age", ylab="Number",
         scales=list(alternating=1),                  
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Vax Coverage", cex.title= 1),
         par.settings = my.settings,
         par.strip.text=list(col="black", font=3),
         panel=function(x,y,...){
           panel.grid(h=-1, v=0); 
           panel.barchart(x,y,...)
         }
)

#######################################################
# Death Heter 

barchart(death_cases_t ~ measure | r, groups=vax,
         data=het_death_t, origin=0, 
         main="Death Heter", 
         xlab="Age", ylab="Number",
         scales=list(alternating=1),                  
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Vax Coverage", cex.title= 1),
         par.settings = my.settings,
         par.strip.text=list(col="black", font=3),
         panel=function(x,y,...){
           panel.grid(h=-1, v=0); 
           panel.barchart(x,y,...)
         }
)

############################################################################
######################################
##Medical Costs

temp <- subset(play.dt, r==1.2)
Med1.2temp2 <- subset(temp, vax <= 11,  
                      select=c (vax, r, measure, medsum))


barchart(medsum/100000~ measure | r, groups=vax,
         data=Med1.2temp2, origin=0, 
         main="Medical Cost - R1.2", 
         xlab="Age", ylab="Costs",
         scales=list(alternating=1),                  
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Vax Coverage", cex.title= 1),
         par.settings = my.settings,
         par.strip.text=list(col="black", font=3),
         panel=function(x,y,...){
           panel.grid(h=-1, v=0); 
           panel.barchart(x,y,...)
         }
)

temp <- subset(play.dt, r==1.3)
Med1.2temp2 <- subset(temp, vax <= 11,  
                      select=c (vax, r, measure, medsum))


barchart(medsum/100000~ measure | r, groups=vax,
         data=Med1.2temp2, origin=0, 
         main="Medical Cost - R1.3", 
         xlab="Age", ylab="Costs",
         scales=list(alternating=1),                  
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Vax Coverage", cex.title= 1),
         par.settings = my.settings,
         par.strip.text=list(col="black", font=3),
         panel=function(x,y,...){
           panel.grid(h=-1, v=0); 
           panel.barchart(x,y,...)
         }
)

##1.4 RO
temp <- subset(play.dt, r==1.4)
Med1.2temp2 <- subset(temp, vax <= 11,  
                      select=c (vax, r, measure, medsum))


R1.4<- barchart(medsum~ measure | r, groups=vax,
         data=Med1.2temp2, origin=0, 
         main="Medical Cost - R1.4", 
         xlab="Age", ylab="Costs",
         scales=list(alternating=1),                  
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="Vax Coverage", cex.title= 1),
         par.settings = my.settings,
         par.strip.text=list(col="black", font=3),
         panel=function(x,y,...){
           panel.grid(h=-1, v=0); 
           panel.barchart(x,y,...)
         }
)
print(R1.4)

CostSav <- 








##########################################################################################

##SLIV Cost 
### Calculating COST  of SLIV programs 

#read in vax cov data (gave to Dennis)
Vaxcov <-read.csv("data/Cost Effect/Vaxcov.csv",
                  stringsAsFactors = FALSE)

#make new column with pop LAIV 
Vaxcov$Student <- Vaxcov$LAIV_p * 58480960 # total US pop 5-18
#############################################################

################## SLIV COST 
## average from 6 implementation studies, and 95% confidence from that. ($25.37, +/- $5.81)

#make new column for avg admin cost  
Vaxcov$Avg_Adm <-Vaxcov$Student * 25.37 #avg admin cost  

#make new column for low admin cost 
Vaxcov$Avg_Low <-Vaxcov$Student * 19.56 #avg admin cost  

#make new column for high admin cost
Vaxcov$Avg_Hi <-Vaxcov$Student * 31.17 #avg admin cost  

# ad = 80#25.37+18.88
# ad_l = 50 #19.56
# ad_h = 100#31.17
# pop= 58480960

ad = 25.37+18.88
SD = 44.19+18.88
#ad_l = 19.56 +18.88
#ad_h = 31.17 +18.88
pop= 58480960
#########
SLIV30<- (pop*.3) * ad
SLIV30sd <- (pop*.3) * SD
# SLIV30L<- (pop*.3) * ad_l
# SLIV30H<- (pop*.3) * ad_h

SLIV40<- (pop*.4) * ad
SLIV40sd <- (pop*.4) * SD
# SLIV40L<- (pop*.4) * ad_l
# SLIV40H<- (pop*.4) * ad_h

SLIV50<- (pop*.5) * ad
SLIV50sd <- (pop*.5) * SD
# SLIV50L<- (pop*.5) * ad_l
# SLIV50H<- (pop*.5) * ad_h

SLIV60<- (pop*.6) * ad
SLIV60sd <- (pop*.6) * SD
# SLIV60L<- (pop*.6) * ad_l
# SLIV60H<- (pop*.6) * ad_h

SLIV70<- (pop*.7) * ad
SLIV70sd <- (pop*.7) * SD
# SLIV70L<- (pop*.7) * ad_l
# SLIV70H<- (pop*.7) * ad_h

SLIV80<- (pop*.8) * ad
SLIV80sd <- (pop*.8) * SD
# SLIV80L<- (pop*.8) * ad_l
# SLIV80H<- (pop*.8) * ad_h


# temp
temp1 <- data.frame (SLIV30, SLIV40, SLIV50, SLIV60, SLIV70, SLIV80, SLIV30sd, SLIV40sd, SLIV50sd, SLIV60sd, SLIV70sd, SLIV80sd)

write.csv(temp1, file = "SLIVcostprivate.csv")
#######################################################################















############## Excess Codes
###############################################################################
###############################################################################
#Summing to get total medical cost (aggregate by R0 and vaccination coverage)

#creating table and subsetting
G0hom1.2.dt<- subset(play.dt, group == "0" & r == 1.2)
G1hom1.2.dt<- subset(play.dt, group == "1" & r == 1.2)
G2hom1.2.dt<- subset(play.dt, group == "2" & r == 1.2)
G3hom1.2.dt<- subset(play.dt, group == "3" & r == 1.2)
G4hom1.2.dt<- subset(play.dt, group == "4" & r == 1.2)
G5hom1.2.dt<- subset(play.dt, group == "5" & r == 1.2)
G6hom1.2.dt<- subset(play.dt, group == "6" & r == 1.2)
G7hom1.2.dt<- subset(play.dt, group == "7" & r == 1.2)
G8hom1.2.dt<- subset(play.dt, group == "8" & r == 1.2)
G9hom1.2.dt<- subset(play.dt, group == "9" & r == 1.2)
G10hom1.2.dt<- subset(play.dt, group == "10" & r == 1.2)
G11hom1.2.dt<- subset(play.dt, group == "11" & r == 1.2)

#Adding ## Total medical cost for homo vaccine at 1.2 R0 
Current1.2<-sum(G0hom1.2.dt$medsum)
Vax401.2 <- sum(G3hom1.2.dt$medsum)
Vax501.2 <- sum(G5hom1.2.dt$medsum)
Vax601.2 <- sum(G7hom1.2.dt$medsum)                               
Vax701.2 <- sum(G9hom1.2.dt$medsum)                                    
Vax801.2 <- sum(G11hom1.2.dt$medsum)                                       

# 1 data set 

homo1.2 <- data.frame (Current1.2, Vax401.2, Vax501.2, Vax601.2,                               
                       Vax701.2, Vax801.2)

#####################################################################################
#R1.3

G0hom1.3.dt<- subset(play.dt, group == "0" & r == 1.3)
G1hom1.3.dt<- subset(play.dt, group == "1" & r == 1.3)
G2hom1.3.dt<- subset(play.dt, group == "2" & r == 1.3)
G3hom1.3.dt<- subset(play.dt, group == "3" & r == 1.3)
G4hom1.3.dt<- subset(play.dt, group == "4" & r == 1.3)
G5hom1.3.dt<- subset(play.dt, group == "5" & r == 1.3)
G6hom1.3.dt<- subset(play.dt, group == "6" & r == 1.3)
G7hom1.3.dt<- subset(play.dt, group == "7" & r == 1.3)
G8hom1.3.dt<- subset(play.dt, group == "8" & r == 1.3)
G9hom1.3.dt<- subset(play.dt, group == "9" & r == 1.3)
G10hom1.3.dt<- subset(play.dt, group == "10" & r == 1.3)
G11hom1.3.dt<- subset(play.dt, group == "11" & r == 1.3)

#Adding ## Total medical cost for homo vaccine at 1.3 R0 
Current1.3<- sum(G0hom1.3.dt$medsum)
Vax401.3 <- sum(G4hom1.3.dt$medsum)
Vax501.3 <- sum(G5hom1.3.dt$medsum)
Vax601.3 <- sum(G7hom1.3.dt$medsum)                               
Vax701.3 <- sum(G9hom1.3.dt$medsum)                                    
Vax801.3 <- sum(G11hom1.3.dt$medsum)                                       

# 1 data set 

homo1.3 <- data.frame (Current1.3, Vax401.3, Vax501.3, Vax601.3,                               
                       Vax701.3, Vax801.3)

###############################################################
#R1.4

G0hom1.4.dt<- subset(play.dt, group == "0" & r == 1.4)
G1hom1.4.dt<- subset(play.dt, group == "1" & r == 1.4)
G2hom1.4.dt<- subset(play.dt, group == "2" & r == 1.4)
G3hom1.4.dt<- subset(play.dt, group == "3" & r == 1.4)
G4hom1.4.dt<- subset(play.dt, group == "4" & r == 1.4)
G5hom1.4.dt<- subset(play.dt, group == "5" & r == 1.4)
G6hom1.4.dt<- subset(play.dt, group == "6" & r == 1.4)
G7hom1.4.dt<- subset(play.dt, group == "7" & r == 1.4)
G8hom1.4.dt<- subset(play.dt, group == "8" & r == 1.4)
G9hom1.4.dt<- subset(play.dt, group == "9" & r == 1.4)
G10hom1.4.dt<- subset(play.dt, group == "10" & r == 1.4)
G11hom1.4.dt<- subset(play.dt, group == "11" & r == 1.4)

#Adding ## Total medical cost for homo vaccine at 1.4 R0 
Current1.4<- sum(G0hom1.4.dt$medsum)
Vax401.4 <- sum(G3hom1.4.dt$medsum)
Vax501.4 <- sum(G5hom1.4.dt$medsum)
Vax601.4 <- sum(G7hom1.4.dt$medsum)                               
Vax701.4 <- sum(G9hom1.4.dt$medsum)                                    
Vax801.4 <- sum(G11hom1.4.dt$medsum)                                       

# 1 data set 

homo1.4 <- data.frame (Current1.4, Vax401.4, Vax501.4, Vax601.4,                               
                       Vax701.4, Vax801.4)
###############################################################
# making all in one data table

#subsetting so I can have in plot
temphom <- data.frame (homo1.2, homo1.3, homo1.4)

#export 
write.csv(temphom, file = "homo12medsumv2.csv")

############################################################################
#############################################################################
#Heter 

###############################################################################
###############################################################################
#Summing to get total medical cost (aggregate by R0 and vaccination coverage)

#creating table and subsetting
G0het1.2.dt<- subset(play.dt, group == "9999" & r == 1.2)
G1het1.2.dt<- subset(play.dt, group == "111" & r == 1.2)
G2het1.2.dt<- subset(play.dt, group == "222" & r == 1.2)
G3het1.2.dt<- subset(play.dt, group == "333" & r == 1.2)
G4het1.2.dt<- subset(play.dt, group == "444" & r == 1.2)
G5het1.2.dt<- subset(play.dt, group == "555" & r == 1.2)
G6het1.2.dt<- subset(play.dt, group == "666" & r == 1.2)
G7het1.2.dt<- subset(play.dt, group == "777" & r == 1.2)
G8het1.2.dt<- subset(play.dt, group == "888" & r == 1.2)
G9het1.2.dt<- subset(play.dt, group == "999" & r == 1.2)
G10het1.2.dt<- subset(play.dt, group == "1010" & r == 1.2)
G11het1.2.dt<- subset(play.dt, group == "11111" & r == 1.2)

#Adding ## Total medical cost for heto vaccine at 1.2 R0 
Current1.2<-sum(G0het1.2.dt$medsum)
Vax401.2 <- sum(G3het1.2.dt$medsum)
Vax501.2 <- sum(G5het1.2.dt$medsum)
Vax601.2 <- sum(G7het1.2.dt$medsum)                               
Vax701.2 <- sum(G9het1.2.dt$medsum)                                    
Vax801.2 <- sum(G11het1.2.dt$medsum)                                       

# 1 data set 

heto1.2 <- data.frame (Current1.2, Vax401.2, Vax501.2, Vax601.2,                               
                       Vax701.2, Vax801.2)

#####################################################################################
#R1.3

G0het1.3.dt<- subset(play.dt, group == "9999" & r == 1.3)
G1het1.3.dt<- subset(play.dt, group == "111" & r == 1.3)
G2het1.3.dt<- subset(play.dt, group == "222" & r == 1.3)
G3het1.3.dt<- subset(play.dt, group == "333" & r == 1.3)
G4het1.3.dt<- subset(play.dt, group == "444" & r == 1.3)
G5het1.3.dt<- subset(play.dt, group == "555" & r == 1.3)
G6het1.3.dt<- subset(play.dt, group == "666" & r == 1.3)
G7het1.3.dt<- subset(play.dt, group == "777" & r == 1.3)
G8het1.3.dt<- subset(play.dt, group == "888" & r == 1.3)
G9het1.3.dt<- subset(play.dt, group == "999" & r == 1.3)
G10het1.3.dt<- subset(play.dt, group == "1010" & r == 1.3)
G11het1.3.dt<- subset(play.dt, group == "11111" & r == 1.3)

#Adding ## Total medical cost for heto vaccine at 1.3 R0 
Current1.3<- sum(G0het1.3.dt$medsum)
Vax401.3 <- sum(G4het1.3.dt$medsum)
Vax501.3 <- sum(G5het1.3.dt$medsum)
Vax601.3 <- sum(G7het1.3.dt$medsum)                               
Vax701.3 <- sum(G9het1.3.dt$medsum)                                    
Vax801.3 <- sum(G11het1.3.dt$medsum)                                       

# 1 data set 

heto1.3 <- data.frame (Current1.3, Vax401.3, Vax501.3, Vax601.3,                               
                       Vax701.3, Vax801.3)

###############################################################
#R1.4

G0het1.4.dt<- subset(play.dt, group == "9999" & r == 1.4)
G1het1.4.dt<- subset(play.dt, group == "111" & r == 1.4)
G2het1.4.dt<- subset(play.dt, group == "222" & r == 1.4)
G3het1.4.dt<- subset(play.dt, group == "333" & r == 1.4)
G4het1.4.dt<- subset(play.dt, group == "444" & r == 1.4)
G5het1.4.dt<- subset(play.dt, group == "555" & r == 1.4)
G6het1.4.dt<- subset(play.dt, group == "666" & r == 1.4)
G7het1.4.dt<- subset(play.dt, group == "777" & r == 1.4)
G8het1.4.dt<- subset(play.dt, group == "888" & r == 1.4)
G9het1.4.dt<- subset(play.dt, group == "999" & r == 1.4)
G10het1.4.dt<- subset(play.dt, group == "1010" & r == 1.4)
G11het1.4.dt<- subset(play.dt, group == "11111" & r == 1.4)

#Adding ## Total medical cost for heto vaccine at 1.4 R0 
Current1.4<- sum(G0het1.4.dt$medsum)
Vax401.4 <- sum(G3het1.4.dt$medsum)
Vax501.4 <- sum(G5het1.4.dt$medsum)
Vax601.4 <- sum(G7het1.4.dt$medsum)                               
Vax701.4 <- sum(G9het1.4.dt$medsum)                                    
Vax801.4 <- sum(G11het1.4.dt$medsum)                                       

# 1 data set 

heto1.4 <- data.frame (Current1.4, Vax401.4, Vax501.4, Vax601.4,                               
                       Vax701.4, Vax801.4)
###############################################################
# making all in one data table

#subsetting so I can have in plot
temphet <- data.frame (heto1.2, heto1.3, heto1.4)

#export 
write.csv(temphet, file = "heto12medsumv2.csv")



###################################

#subsetting for 1.2Hom0 1-11 
hom_1.2.dt <- subset(measures.dttest, scenario == "hom0" & r == 1.2)

# Calaculating to make datasets for R 1.2 hom0 (current)

otc_0 <- hom_1.2.dt$V1 * #POP at risk 
  dzweyker.dt$otc #POP at risk * Prob of X  

ill_0<- hom_1.2.dt$V1 * #POP at risk 
  dzweyker.dt$ill #POP at risk * Prob of X 

outpt_0 <- hom_1.2.dt$V1 * #POP at risk 
  dzweyker.dt$oupt #POP at risk * Prob of X 

hosp_0 <- hom_1.2.dt$V1 * #POP at risk 
  dzweyker.dt$hosp #POP at risk * Prob of X 

death_0 <- hom_1.2.dt$V1 * #POP at risk 
  dzweyker.dt$death #POP at risk * Prob of X 

age <- c("x0to4", "x5to18", "x19to29", "x30to64", "x65")

DZ_0<- data.frame(otc_0, ill_0, outpt_0, hosp_0, death_0, age)






##########################################################################################
data.frame(run)
lapply(run, as.numeric)


runs <- NA
runs$hom1.2<- NA 
for (i in 2:ncol(run)){
  runs$hom1.2[i] <-
    sum (run[i, 2:10], na.rm = TRUE)
}




census$total6.18 <-  NA
for (i in 1:nrow(census)){
  census$total6.18[i] <- 
    sum(census[i, 8:20], na.rm = TRUE)








#subsetting 
run1.2 <- run[131:250,]
r_1.2_h0 <- run1.2 [1:10,]
as.numeric(run1.2)


r_1.2_h0b <-mean(r_1.2_h0)
