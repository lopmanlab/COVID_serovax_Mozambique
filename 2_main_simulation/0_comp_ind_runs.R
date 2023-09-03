file_name <- list.files(path= "/projects/blopman/vger/cliu/0_interpol_wanehi_int_firstvar/",pattern=c("sw_run"))

mod_scen_int <- list()
for(i in 1:400){
#for(i in 1:length(file_name)){
  mod_scen_int[[i]]<-readRDS(paste("/projects/blopman/vger/cliu/0_interpol_wanehi_int_firstvar/",file_name[i],sep=""))
  print(paste(i, "iteration complete"))
}


setwd("/home/cliu369/0_interim")

## Load libraries
library(dplyr)
library(deSolve)
library(ggplot2)
library(tidyr)


tot_pop <- 30066648   ## Took population for 2020 from Mozambique INE (Institute of statistics)
p_urban <- 0.34 ##From INE
p_rural <- 1-p_urban
start.Ns <- c(10884513, 4883969, 7958844, 4900719, 992149, 446454)
dist <- start.Ns/tot_pop

pop_dist = data.frame(age_ur = c("cr","cu","ar","au","er","eu"),
                      pop = start.Ns)
sweep_int <- readRDS("1_sweep_int_firstvar.RDS")[1:400,]
sweep_int$scenarios <- rep(c("ann_late","bi_late"),times=200)

###NNTs from interval vaccination
n1<- 400
nnt1 <- data.frame(
  num_dose = rep(0,n1),
  num_campaigns = rep(0,n1),
  num_deaths = rep(0,n1),
  deaths_averted=rep(0,n1),
  num_deaths_post2=rep(0,n1),
  num_deaths_c = rep(0, n1),
  num_deaths_a = rep(0, n1),
  num_deaths_e = rep(0, n1)
)

times =data.frame(num1 = seq(from=0, to=9, by=1),
                  num2 = seq(from=0, to=4, by=1))%>%
  mutate(ann_late_st = 300+365*num1,
         ann_late_en = 330+365*num1,
         bi_late_st =  300+730*num2,
         bi_late_en = 300+730*num2,
         ann_early_st = 120+365*num1,
         ann_early_en = 150+365*num1,
         bi_early_st = 120+730*num2,
         bi_early_en = 150+730*num2)


vax_times=list()
vax_times[["ann_late"]] <- unique(unlist(times$ann_late_st))
vax_times[["bi_late"]] <- unique(unlist(times$bi_late_st))
vax_times[["ann_early"]]<-unique(unlist(times$ann_early_st))
vax_times[["bi_early"]]<-unique(unlist(times$bi_early_st))



##NUmber of vaccine doses administered
##Need to get the number vaccine eligible at the start of the vax rounds and then multiply by exponential Pert thing for each round

for(i in 1:n1){
  ## get the vax time scenario from scenario runs list
  scen <- sweep_int$scenarios[[i]]
  vax_times_vec <- vax_times[[which(names(vax_times)== scen)]]
  delta3_er <- 0.02
  
  vax_dose <- mod_scen_int[[i]]$vax_elig[1:3650,] %>%
    select(time:vax_elig_a)%>%
    filter(time %in% unlist(vax_times_vec))%>%
    mutate(est_vax_doses = vax_elig_e - vax_elig_e*2.718^(-delta3_er*30))
  
  nnt1$num_dose[i]<- sum(vax_dose$est_vax_doses)
  nnt1$num_campaigns[i] <- nrow(vax_dose)
  nnt1$firstvax[i] <- vax_dose$time[1]
  #vax_dose_post2 <- vax_dose %>%filter(time>730)
  #nnt1$num_dose_post2[i] <- sum(vax_dose_post2$est_vax_doses)
  nnt1$scenarios[i] <- sweep_int$scenarios[i]
  nnt1$sweep_unique[i] <- sweep_int$sweep_unique[i]
  
}

#Number of deaths
for(i in 1:n1){
  nnt1$num_deaths[[i]] <- sum(mod_scen_int[[i]]$pop_num$new_Deaths_tot[1:3650], na.rm=T)
  #post2 <- mod_scen_int[[i]]$pop_num %>%filter(time>730)
  nnt1$num_deaths_post2[[i]] <- sum(mod_scen_int[[i]]$pop_num$new_Deaths_tot[730:3650], na.rm=T)
  nnt1$num_deaths_c[[i]] <- sum(mod_scen_int[[i]]$pop_num$new_Deaths_c[1:3650], na.rm=T)
  nnt1$num_deaths_a[[i]] <- sum(mod_scen_int[[i]]$pop_num$new_Deaths_a[1:3650], na.rm=T)
  nnt1$num_deaths_e[[i]] <- sum(mod_scen_int[[i]]$pop_num$new_Deaths_e[1:3650], na.rm=T)
}



saveRDS(nnt1,"nnt_int_firstvar_1_400.RDS")



#file_name <- list.files(path= "sw_int_hi/",pattern=c("sw_run"))

#mod_scen <- list()
#for(i in 1:length(file_name)){
#  mod_scen[[i]]<-readRDS(paste("sw_int_hi/",file_name[i],sep=""))
#}

#saveRDS(mod_scen,"sw_int_hi.RDS")

