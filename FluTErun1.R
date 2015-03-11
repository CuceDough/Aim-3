##################################### 2.26.15
# Dennis' FluTE data fom "Run file, email 2.26.15
# Cost Effective Paper Aim 3
############################################################################
# turn off scientific notation
options(scipen=999)

#####
# SET WORKING DIRECTORY CONDITIONAL TO SYSTEM
#####
if ( Sys.info()["sysname"] == "Linux" ){
  cf <- "/home/joebrew/Documents/controlflu"
  linux_path <- "/media/joebrew/JB/fdoh/private/"
  acps_path <-  "/media/joebrew/JB/fdoh/private/acps/" #"E:/fdoh/private/acps/"
} else if( Sys.info()["user"] == "BrewJR"){
  cf <- "C:/Users/BrewJR/Documents/controlflu"
  linux_path <- "E:/fdoh/private/"
  acps_path <-  "E:/fdoh/private/acps/"
} else if( Sys.info()["user"] == "C"){
  cf <- "C:/Users/C/Documents/controlflu"
  linux_path <- NULL
  acps_path <-  NULL
}
setwd(cf) 

#####################################################################################################
#####################################################################################################

# #Weycker param
# dzweyker <- read.csv("data/Cost Effect/dzparamweycker.csv")
# dzweyker.dt <- data.table(dzweyker) 


# Reading in raw data 

#2013 population data 
agegroup <-read.csv("data/Cost Effect/Agegroupop.csv",
                    stringsAsFactors = FALSE)
#FluTE Run 2.27.15
run <- read.csv("data/Cost Effect/runs.csv")#, stringsAsFactors = FALSE)
#run$r <- factor(run$r) #makes something into a factor 
#subset(run, scenario =="base" & r == 1.2) # this is to call it 
# subset give part of data frame (run) that matches criteria (e.g., scenario = base AND r = 1.2)

### Carl's example of a for loop 
#for (scen in levels(run$scenario)) for (r0 in levels(run$r))
#{
#  target <- subset(run, scenario == scen & r == r0)
#}

######### Carl's example of using a data table 
#require(data.table); require(reshape2)

#run.dt <- data.table(run)
#analysis.dt <- run.dt[r != 1.2, list(mean.a0 = mean(attack0), mean.a1 = mean(attack1)), by=c("scenario","r")]
# within the [ a,b ], a is what you are filtering; b is what you are listing 

#############################################################################
#############################################################################
##### Another way (Carl's quicker Way - probably best way )
# Essentially you are melting the data table and subsetting the from long to wide 
#call in packages
require(data.table); require(reshape2) 

# make it into a data table
run.dt <- data.table(run) 

# melt the data 
run.melt <- melt(run.dt, variable.name="measure", id.vars=c("scenario","r"))
measures.dt <- run.melt[,list(mean=mean(value), sd=sd(value)),by=c("scenario","r","measure")]
write.csv (measures.dt, file= "measures.1.csv")

## POPULATION VARIABLE 
## this is to make another column of data. For this instance, using population 
measures.dt$pop = 0
measures.dt[measure == "attack0", pop:=20201362]
measures.dt[measure == "attack1", pop:=58480960]
measures.dt[measure == "attack2", pop:=47273082]
measures.dt[measure == "attack3", pop:=142522150]
measures.dt[measure == "attack4", pop:=40267984]

## High Risk Popluation 
#adding high risk 
measures.dt$h_risk = 0
measures.dt[measure == "attack0", h_risk:=.052]
measures.dt[measure == "attack1", h_risk:=.106]
measures.dt[measure == "attack2", h_risk:=.149]
measures.dt[measure == "attack3", h_risk:=.330]
measures.dt[measure == "attack4", h_risk:=.512]

##groups them via Scenario 
measures.dt$group = 0
measures.dt[scenario == "hom0", group:= "1"]
measures.dt[scenario == "hom1", group:= "1"]
measures.dt[scenario == "hom2", group:= "1"]
measures.dt[scenario == "hom3", group:= "1"]
measures.dt[scenario == "hom4", group:= "1"]
measures.dt[scenario == "hom5", group:= "1"]
measures.dt[scenario == "hom6", group:= "1"]
measures.dt[scenario == "hom7", group:= "1"]
measures.dt[scenario == "hom8", group:= "1"]
measures.dt[scenario == "hom9", group:= "1"]
measures.dt[scenario == "hom10", group:= "1"]
measures.dt[scenario == "hom11", group:= "1"]
measures.dt[scenario == "base", group:= "0"]
measures.dt[scenario == "het0", group:= "2"]
measures.dt[scenario == "het1", group:= "2"]
measures.dt[scenario == "het2", group:= "2"]
measures.dt[scenario == "het3", group:= "2"]
measures.dt[scenario == "het4", group:= "2"]
measures.dt[scenario == "het5", group:= "2"]
measures.dt[scenario == "het6", group:= "2"]
measures.dt[scenario == "het7", group:= "2"]
measures.dt[scenario == "het8", group:= "2"]
measures.dt[scenario == "het9", group:= "2"]
measures.dt[scenario == "het10", group:= "2"]
measures.dt[scenario == "het11", group:= "2"]

as.factor(measures.dt$group)

#Vax %
##groups them via Scenario- Making scenario into a factor 
measures.dt$vax = 0
measures.dt[scenario == "hom0", vax:= "0"]
measures.dt[scenario == "hom1", vax:= "1"]
measures.dt[scenario == "hom2", vax:= "2"]
measures.dt[scenario == "hom3", vax:= "3"]
measures.dt[scenario == "hom4", vax:= "4"]
measures.dt[scenario == "hom5", vax:= "5"]
measures.dt[scenario == "hom6", vax:= "6"]
measures.dt[scenario == "hom7", vax:= "7"]
measures.dt[scenario == "hom8", vax:= "8"]
measures.dt[scenario == "hom9", vax:= "9"]
measures.dt[scenario == "hom10",vax:= "10"]
measures.dt[scenario == "hom11",vax:= "11"]
measures.dt[scenario == "base", vax:= "999"]
measures.dt[scenario == "het0", vax:= "0"]
measures.dt[scenario == "het1", vax:= "1"]
measures.dt[scenario == "het2", vax:= "2"]
measures.dt[scenario == "het3", vax:= "3"]
measures.dt[scenario == "het4", vax:= "4"]
measures.dt[scenario == "het5", vax:= "5"]
measures.dt[scenario == "het6", vax:= "6"]
measures.dt[scenario == "het7", vax:= "7"]
measures.dt[scenario == "het8", vax:= "8"]
measures.dt[scenario == "het9", vax:= "9"]
measures.dt[scenario == "het10", vax:= "10"]
measures.dt[scenario == "het11", vax:= "11"]

#made into an orrdered factor 
as.ordered(measures.dt$vax) 

as.ordered(measures.dt$r)

#measures.dt1 <- measures.dt[, sum(mean*pop), by=c("scenario","r")] #carls orginal code. But I need also by measure (which is age group also)
#measures.dt$SD <- measures.dt [,(sd*pop)] *Ignore for now 
measures.dttest <- measures.dt[, sum(mean*pop), by=c("scenario","r","measure", "group", "h_risk", "vax")]

## calculating high risk 
measures.dttest$hrisk <- measures.dttest[, (V1*h_risk)]

## calculating low risk 
measures.dttest$lrisk <- measures.dttest[, (V1*(1-h_risk))]

# making similiar data set and subsetting 
play.dt <- measures.dttest 


##### PARAMETERS COST of one of the following: 

#OTC (all same price ) 
play.dt$otc = 4.58
play.dt$otc_SD = 3.05
                     

#oupt low risk 
play.dt$oupt= 0
play.dt[measure == "attack0" , oupt:=255]
play.dt[measure == "attack1" , oupt:=145]
play.dt[measure == "attack2" , oupt:=191]
play.dt[measure == "attack3" , oupt:=229]
play.dt[measure == "attack4" , oupt:=369]  

#oupt low risk SD
play.dt$oupt_SE= 0
play.dt[measure == "attack0" , oupt_SE:=468]
play.dt[measure == "attack1" , oupt_SE:=393]
play.dt[measure == "attack2" , oupt_SE:=688]
play.dt[measure == "attack3" , oupt_SE:=1167]
play.dt[measure == "attack4" , oupt_SE:=2353] 

#oupt high risk 
play.dt$oupt_hr= 0
play.dt[measure == "attack0" , oupt_hr:=875]
play.dt[measure == "attack1" , oupt_hr:=989]
play.dt[measure == "attack2" , oupt_hr:=1105]
play.dt[measure == "attack3" , oupt_hr:=1117]
play.dt[measure == "attack4" , oupt_hr:=725]  

#oupt high risk SE
play.dt$oupt_hr_SE= 0
play.dt[measure == "attack0" , oupt_hr_SE:=1929]
play.dt[measure == "attack1" , oupt_hr_SE:=2274]
play.dt[measure == "attack2" , oupt_hr_SE:=2617]
play.dt[measure == "attack3" , oupt_hr_SE:=1992]
play.dt[measure == "attack4" , oupt_hr_SE:=1724] 

#hosp low risk
play.dt$hosp= 0
play.dt[measure == "attack0" , hosp:=16580]
play.dt[measure == "attack1" , hosp:=22880]
play.dt[measure == "attack2" , hosp:=28972]
play.dt[measure == "attack3" , hosp:=33989]
play.dt[measure == "attack4" , hosp:=17450]  

#hosp low risk
play.dt$hosp_SE= 0
play.dt[measure == "attack0" , hosp_SE:=55148]
play.dt[measure == "attack1" , hosp_SE:=34867]
play.dt[measure == "attack2" , hosp_SE:=68022]
play.dt[measure == "attack3" , hosp_SE:=145878]
play.dt[measure == "attack4" , hosp_SE:=35235] 

#hosp high risk
play.dt$hosp_hr = 0
play.dt[measure == "attack0" , hosp_hr:=124344]
play.dt[measure == "attack1" , hosp_hr:=312736]
play.dt[measure == "attack2" , hosp_hr:=72723]
play.dt[measure == "attack3" , hosp_hr:=62951]
play.dt[measure == "attack4" , hosp_hr:=25525]  

#hosp high risk
play.dt$hosp_hr_SE = 0
play.dt[measure == "attack0" , hosp_hr_SE:=188393]
play.dt[measure == "attack1" , hosp_hr_SE:=76794]
play.dt[measure == "attack2" , hosp_hr_SE:=130512]
play.dt[measure == "attack3" , hosp_hr_SE:=113984]
play.dt[measure == "attack4" , hosp_hr_SE:=48903]

#death low risk
play.dt$death= 0
play.dt[measure == "attack0" , death:=43916]
play.dt[measure == "attack1" , death:=43916]
play.dt[measure == "attack2" , death:=116328]
play.dt[measure == "attack3" , death:=180695]
play.dt[measure == "attack4" , death:=63924]  

#death low risk
play.dt$death_SE= 0
play.dt[measure == "attack0" , death_SE:=37241]
play.dt[measure == "attack1" , death_SE:=37241]
play.dt[measure == "attack2" , death_SE:=139671]
play.dt[measure == "attack3" , death_SE:=508796]
play.dt[measure == "attack4" , death_SE:=147005]  

#death high risk 
play.dt$death_hr= 0
play.dt[measure == "attack0" , death_hr:= 408333]
play.dt[measure == "attack1" , death_hr:= 408333]
play.dt[measure == "attack2" , death_hr:= 115648]
play.dt[measure == "attack3" , death_hr:= 181102]
play.dt[measure == "attack4" , death_hr:= 50305]  

#death high risk 
play.dt$death_hr_SE = 0
play.dt[measure == "attack0" , death_hr_SE :=336978]
play.dt[measure == "attack1" , death_hr_SE :=336978]
play.dt[measure == "attack2" , death_hr_SE :=99460]
play.dt[measure == "attack3" , death_hr_SE :=527225]
play.dt[measure == "attack4" , death_hr_SE :=94335]  



### Calculations

# play.dt$otc_hr_c <- play.dt$hrisk * (play.dt$otc)
# play.dt$ill_hr_c <- play.dt$hriisk * (play.dt$ill)
# play.dt$oupt_hr_c <- play.dt$hriisk * (play.dt$oupt)
# play.dt$hosp_hr_c <- play.dt$hriisk * (play.dt$hosp)
# play.dt$death_hr_c <- play.dt$hriisk * (play.dt$death)
##############################################

##### PARAMETERS of one of the following: 

# OTC
play.dt$otc_para = 0
play.dt[measure == "attack0", otc_para:=.5309]
play.dt[measure == "attack1", otc_para:=.6184]
play.dt[measure == "attack2", otc_para:=.6827]
play.dt[measure == "attack3", otc_para:=.6745]
play.dt[measure == "attack4", otc_para:=.0209]                     

# OTC
play.dt$otc_para_hr = 0
play.dt[measure == "attack0", otc_para_hr:=.0759]
play.dt[measure == "attack1", otc_para_hr:=.3644]
play.dt[measure == "attack2", otc_para_hr:=.3708]
play.dt[measure == "attack3", otc_para_hr:=.3625]
play.dt[measure == "attack4", otc_para_hr:=.0209] 

# #ill****
# play.dt$ill_para = 0
# play.dt[measure == "attack0", ill_para:=254.49]
# play.dt[measure == "attack1", ill_para:=144.77]
# play.dt[measure == "attack2", ill_para:=190.49]
# play.dt[measure == "attack3", ill_para:=228.58]
# play.dt[measure == "attack4", ill_para:=368.78]  

#oupt
play.dt$oupt_para = 0
play.dt[measure == "attack0", oupt_para:=.455]
play.dt[measure == "attack1", oupt_para:=.318]
play.dt[measure == "attack2", oupt_para:=.313]
play.dt[measure == "attack3", oupt_para:=.313]
play.dt[measure == "attack4", oupt_para:=.620]  

#oupt
play.dt$oupt_para_hr= 0
play.dt[measure == "attack0", oupt_para_hr:=.910]
play.dt[measure == "attack1", oupt_para_hr:=.635]
play.dt[measure == "attack2", oupt_para_hr:=.625]
play.dt[measure == "attack3", oupt_para_hr:=.625]
play.dt[measure == "attack4", oupt_para_hr:=.820]  


#hosp
play.dt$hosp_para = 0
play.dt[measure == "attack0", hosp_para:=.0141]
play.dt[measure == "attack1", hosp_para:=.0006]
play.dt[measure == "attack2", hosp_para:=.0042]
play.dt[measure == "attack3", hosp_para:=.0193]
play.dt[measure == "attack4", hosp_para:=.0421]  

#hosp
play.dt$hosp_para_hr = 0
play.dt[measure == "attack0", hosp_para_hr:=.0141]
play.dt[measure == "attack1", hosp_para_hr:=.0006]
play.dt[measure == "attack2", hosp_para_hr:=.0042]
play.dt[measure == "attack3", hosp_para_hr:=.0193]
play.dt[measure == "attack4", hosp_para_hr:=.0421]  

#death_para
play.dt$death_para = 0
play.dt[measure == "attack0", death_para:=.00001]
play.dt[measure == "attack1", death_para:=.00001]
play.dt[measure == "attack2", death_para:=.00009]
play.dt[measure == "attack3", death_para:=.00134]
play.dt[measure == "attack4", death_para:=.01170]  

#death_para
play.dt$death_para_hr = 0
play.dt[measure == "attack0", death_para_hr:=.00004]
play.dt[measure == "attack1", death_para_hr:=.00001]
play.dt[measure == "attack2", death_para_hr:=.00009]
play.dt[measure == "attack3", death_para_hr:=.00134]
play.dt[measure == "attack4", death_para_hr:=.01170]  

## renaming "attack" to age groups
play.dt[measure == "attack0", measure:= "0to4"]
play.dt[measure == "attack1", measure:= "5to18"]
play.dt[measure == "attack2", measure:= "19to29"]
play.dt[measure == "attack3", measure:= "30to64"]
play.dt[measure == "attack4", measure:= "65"]

############################
# Calculations for cases
#OTC low risk
play.dt$otc_cases <- play.dt$otc_para * play.dt$lrisk 

#outpt high risk 
play.dt$otc_cases_hr <- play.dt$otc_para_hr * play.dt$hrisk 

# OTC total
play.dt$otc_cases_t <- play.dt$otc_cases + play.dt$otc_cases_hr

###############
#outpt low risk 
play.dt$oupt_cases <- play.dt$oupt_para * play.dt$lrisk

#outpt high risk 
play.dt$oupt_cases_hr <- play.dt$oupt_para_hr * play.dt$hrisk

# Out total 
play.dt$oupt_cases_t <- play.dt$oupt_cases + play.dt$oupt_cases_hr

##############
#hosp low risk 
play.dt$hosp_cases <- play.dt$hosp_para * play.dt$lrisk 

#hosp high risk 
play.dt$hosp_cases_hr <- play.dt$hosp_para_hr * play.dt$hrisk 

#total
play.dt$hosp_cases_t <- play.dt$hosp_cases + play.dt$hosp_cases_hr

################
# death low risk 
play.dt$death_cases <- play.dt$death_para * play.dt$lrisk 

# death high risk 
play.dt$death_cases_hr <- play.dt$death_para_hr * play.dt$hrisk 

# death total 
play.dt$death_cases_t <- play.dt$death_cases + play.dt$death_cases_hr
################################

### Calculations for cost
#OTC low risk
play.dt$otc_cost <- play.dt$otc_para * play.dt$lrisk * play.dt$otc

#outpt high risk 
play.dt$otc_cost_hr <- play.dt$otc_para_hr * play.dt$hrisk * play.dt$otc

#outpt low risk 
play.dt$oupt_cost <- play.dt$oupt_para * play.dt$lrisk* play.dt$oupt

#outpt high risk 
play.dt$oupt_cost_hr <- play.dt$oupt_para_hr * play.dt$hrisk* play.dt$oupt_hr

#hosp low risk 
play.dt$hosp_cost <- play.dt$hosp_para * play.dt$lrisk * play.dt$hosp

#hosp high risk 
play.dt$hosp_cost_hr <- play.dt$hosp_para_hr * play.dt$hrisk * play.dt$hosp_hr

# death low risk 
play.dt$death_cost <- play.dt$death_para * play.dt$lrisk * play.dt$death

# hosp high risk 
play.dt$death_cost_hr <- play.dt$death_para_hr * play.dt$hrisk * play.dt$death_hr


#### adding all the costs but individual 
play.dt$medsum = play.dt$otc_cost + play.dt$otc_cost_hr +  
  play.dt$oupt_cost + play.dt$oupt_cost_hr + play.dt$hosp_cost + play.dt$hosp_cost_hr +
  play.dt$death_cost +  play.dt$death_cost_hr

## without OTC
play.dt$medsumIN = play.dt$oupt_cost + play.dt$oupt_cost_hr + play.dt$hosp_cost + play.dt$hosp_cost_hr +
  play.dt$death_cost +  play.dt$death_cost_hr

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
