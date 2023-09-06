
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

setwd("/projects/blopman/vger/cliu/0_combined/")

#mod_scen1<- readRDS("sw_wanehi_thresh_1_200.RDS")
#mod_scen2<- readRDS("sw_wanehi_thresh_200_2000.RDS")
#mod_scen3<- readRDS("sw_wanehi_thresh_2001_4000.RDS")
mod_scen <- list()
mod_scen <- readRDS("sw_wanehi_thresh_1_4000.RDS")
sweep<- readRDS("0_sweep_sero.RDS")

mod_scen_int <- readRDS("sw_wanehi_int_1_1000.RDS")
sweep_int <- readRDS("1_sweep_int.RDS")
sweep_int$scenarios <- rep(c("ann_late","bi_late"),times=500)

## NNTs for serovax
n<- 4000
#nnt <- data.frame(
#  num_dose = rep(0,n),
#  num_campaigns = rep(0,n),
#  num_deaths = rep(0,n),
#  deaths_averted=rep(0,n),
#  num_deaths_post2=rep(0,n),
#  num_deaths_c = rep(0,n),
#  num_deaths_a = rep(0,n),
#  num_deaths_e = rep(0,n)
#)

nnt_yr <- list()
##NUmber of vaccine doses administered         
for(i in 1:n){
  vax <- mod_scen[[i]]$vax_elig %>%
    mutate(yr = floor(time/365))
  
  vax_dose <- vax %>% 
    mutate(flag = ifelse(delta3_er == 0.02 & lag(delta3_er==0),1,0))%>%
    filter(flag==1)%>%
    mutate(est_vax_doses = vax_elig_e - vax_elig_e*2.718^(-delta3_er*30))
  
  vax_dose <- vax_dose %>%
              group_by(yr)%>%
              dplyr::summarise(num_dose = sum(est_vax_doses),
                               num_campaigns = n())
  
  deaths <- mod_scen[[i]]$pop_num%>%
    mutate(yr = floor(time/365))
  
  deaths<- deaths %>%
          select(new_Deaths_tot, new_Deaths_c, new_Deaths_a, new_Deaths_e, yr)%>%
          group_by(yr)%>%
          dplyr::summarise(new_Deaths_tot = sum(new_Deaths_tot, na.rm=T),
                 new_Deaths_c = sum(new_Deaths_c, na.rm=T),
                 new_Deaths_a = sum(new_Deaths_a, na.rm=T),
                 new_Deaths_e = sum(new_Deaths_e, na.rm=T))
          
  nnt_yr[[i]]<- deaths %>% left_join(vax_dose, by = c("yr"="yr")) %>%
            mutate(num_dose_roll = cumsum(num_dose),
                   new_Deaths_e_roll = cumsum(new_Deaths_e),
                   new_Deaths_tot_roll = cumsum(new_Deaths_tot),
                   scenarios = sweep$sero_thresh[i],
                   sweep_unique = sweep$sweep_unique[i])
  
}

nnt_yr <- do.call(rbind,nnt_yr)


###NNTs from interval vaccination
n1<- 1000
times =data.frame(num1 = seq(from=0, to=9, by=1),
                  num2 = seq(from=0, to=4, by=1))%>%
  mutate(ann_late_st = 300+365*num1,
         ann_late_en = 330+365*num1,
         bi_late_st =  300+730*num2,
         bi_late_en = 300+730*num2)


vax_times=list()
vax_times[["ann_late"]] <- unique(unlist(times$ann_late_st))
vax_times[["bi_late"]] <- unique(unlist(times$bi_late_st))

nnt1_yr <- list()
##NUmber of vaccine doses administered         
for(i in 1:n1){
          scen <- sweep_int$scenarios[[i]]
          vax_times_vec <- vax_times[[which(names(vax_times)== scen)]]
          delta3_er <- 0.02
          
          vax <- mod_scen_int[[i]]$vax_elig %>%
            mutate(yr = floor(time/365))  
          
          vax_dose <- vax %>%
              select(time:vax_elig_a, yr)%>%
              filter(time %in% unlist(vax_times_vec))%>%
              mutate(est_vax_doses = vax_elig_e - vax_elig_e*2.718^(-delta3_er*30)) %>%
              group_by(yr)%>%
              dplyr::summarise(num_dose = sum(est_vax_doses),
                               num_campaigns = n())  
          
          deaths <- mod_scen[[i]]$pop_num%>%
            mutate(yr = floor(time/365))
          
          deaths<- deaths %>%
            select(new_Deaths_tot, new_Deaths_c, new_Deaths_a, new_Deaths_e, yr)%>%
            group_by(yr)%>%
            dplyr::summarise(new_Deaths_tot = sum(new_Deaths_tot, na.rm=T),
                             new_Deaths_c = sum(new_Deaths_c, na.rm=T),
                             new_Deaths_a = sum(new_Deaths_a, na.rm=T),
                             new_Deaths_e = sum(new_Deaths_e, na.rm=T))
  
            nnt1_yr[[i]]<- deaths %>%left_join(vax_dose, by = c("yr"="yr")) %>%
              mutate(num_dose_roll = cumsum(num_dose),
                     new_Deaths_e_roll = cumsum(new_Deaths_e),
                     new_Deaths_tot_roll = cumsum(new_Deaths_tot),
                     scenarios = sweep_int$scenarios[i],
                     sweep_unique = sweep_int$sweep_unique[i])
}

nnt1_yr <- do.call(rbind, nnt1_yr)


##NUmber of vaccine doses administered
##Need to get the number vaccine eligible at the start of the vax rounds and then multiply by exponential Pert thing for each round

#for(i in 1:n1){
#  ## get the vax time scenario from scenario runs list
#  scen <- sweep_int$scenarios[[i]]
#  vax_times_vec <- vax_times[[which(names(vax_times)== scen)]]
#  delta3_er <- 0.02
  
#  vax_dose <- mod_scen_int[[i]]$vax_elig[1:3650,] %>%
#    select(time:vax_elig_a)%>%
#    filter(time %in% unlist(vax_times_vec))%>%
#    mutate(est_vax_doses = vax_elig_e - vax_elig_e*2.718^(-delta3_er*30))
  
#  nnt1$num_dose[i]<- sum(vax_dose$est_vax_doses)
#  nnt1$num_campaigns[i] <- nrow(vax_dose)
#  nnt1$firstvax[i] <- vax_dose$time[1]
  #vax_dose_post2 <- vax_dose %>%filter(time>730)
  #nnt1$num_dose_post2[i] <- sum(vax_dose_post2$est_vax_doses)
#  nnt1$scenarios[i] <- sweep_int$scenarios[i]
#  nnt1$sweep_unique[i] <- sweep_int$sweep_unique[i]
  

#}



##Summary of NNTs
#nnt_main <- plyr::rbind.fill(
#  nnt,nnt1)

#nnt_main <- nnt
#nnt_main <- nnt_main %>%
#  mutate(scen = 
#           case_when(sero_thresh==0~"No vax",
#                     is.na(sero_thresh)~scenarios,                                   
#                     TRUE ~ paste("serothresh-",sero_thresh,sep="")))
#nnt_main<- nnt_main%>%
#  left_join(no_vax, by="sweep_unique")%>%
#  mutate(
#    deaths_averted = num_deaths_novax - num_deaths,
#    deaths_averted_e = num_deaths_e_novax - num_deaths_e,
#    nnt=num_dose/deaths_averted,
#    nnt_e = num_dose/deaths_averted_e)

saveRDS(nnt_yr, "nnt_main_wanehi_yr.RDS")
saveRDS(nnt1_yr, "nnt1_main_wanehi_yr.RDS")



