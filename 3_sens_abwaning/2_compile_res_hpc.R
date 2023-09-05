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


mod_scen <- readRDS("sw_sens_kappa_sero2.RDS")
sweep<- readRDS("0_sweep_sero.RDS")[2501:3500,]

#mod_scen_int <- readRDS("sw_sens_kappa_int.RDS")
#sweep_int <- readRDS("1_sweep_int.RDS")
#sweep_int$scenarios <- rep(c("ann_late","bi_late"),times=500)

## NNTs for serovax
n<- 1000
nnt <- data.frame(
  num_dose = rep(0,n),
  num_campaigns = rep(0,n),
  num_deaths = rep(0,n),
  deaths_averted=rep(0,n),
  num_deaths_post2=rep(0,n),
  num_deaths_c = rep(0,n),
  num_deaths_a = rep(0,n),
  num_deaths_e = rep(0,n)
)

##NUmber of vaccine doses administered         
for(i in 1:n){

  vax_dose <- mod_scen[[i]]$vax_elig[1:3650,] %>%
    mutate(flag = ifelse(delta3_er == 0.02 & lag(delta3_er==0),1,0))%>%
    filter(flag==1)%>%
    mutate(est_vax_doses = vax_elig_e - vax_elig_e*2.718^(-delta3_er*30))
  nnt$num_dose[i]<- sum(vax_dose$est_vax_doses)
  nnt$num_campaigns[i] <- nrow(vax_dose)
  nnt$firstvax[i] <- vax_dose$time[1]
  #vax_dose_post2 <- vax_dose %>%filter(time>730)
  #nnt$num_dose_post2[i] <- sum(vax_dose_post2$est_vax_doses)
  nnt$sero_thresh[i]<- sweep$sero_thresh[i]
  nnt$sweep_unique[i] <- sweep$sweep_unique[i]
  nnt$kappa[i]<- sweep$kappa1[i]
  nnt$kappa_lab[i]<-paste0("1/",1/sweep$kappa1[i])
  
}

#Number of deaths
for(i in 1:n){
  nnt$num_deaths[[i]] <- sum(mod_scen[[i]]$pop_num$new_Deaths_tot[1:3650], na.rm=T)
  #nnt$num_deaths_post2[[i]]<- sum(mod_scen[[i]]$pop_num$new_Deaths_tot[730:3650], na.rm=T)
  nnt$num_deaths_c[[i]] <- sum(mod_scen[[i]]$pop_num$new_Deaths_c[1:3650], na.rm=T)
  nnt$num_deaths_a[[i]] <- sum(mod_scen[[i]]$pop_num$new_Deaths_a[1:3650], na.rm=T)
  nnt$num_deaths_e[[i]] <- sum(mod_scen[[i]]$pop_num$new_Deaths_e[1:3650], na.rm=T)
}

no_vax <- nnt%>%filter(sero_thresh==0)%>%
  group_by(sweep_unique,kappa_lab)%>%
  slice(n=1)%>%
  select(num_deaths,num_deaths_e, sweep_unique)%>%
  dplyr::rename(.,num_deaths_novax=num_deaths)%>%
  dplyr::rename(.,num_deaths_e_novax=num_deaths_e)

#no_vax_post1 <- nnt%>%filter(sero_thresh==0)%>%
#  group_by(sweep_unique)%>%
#  slice(n=1)%>%
#  select(num_deaths_post2, sweep_unique)%>%
#  dplyr::rename(.,num_deaths_novax_post2=num_deaths_post2)

#no_vax <- no_vax%>%left_join(
#  no_vax_post1
#)

###NNTs from interval vaccination
n1<- 1000
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
  nnt1$kappa[i]<- sweep_int$kappa1[i]
  nnt1$kappa_lab[i]<-paste0("1/",1/sweep_int$kappa1[i])
  
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

##Summary of NNTs
nnt_main <- plyr::rbind.fill(
  nnt,nnt1)

#nnt_main <- nnt
nnt_main <- nnt_main %>%
  mutate(scen = 
           case_when(sero_thresh==0~"No vax",
                     is.na(sero_thresh)~scenarios,                                   
                     TRUE ~ paste("serothresh-",sero_thresh,sep="")))
nnt_main<- nnt_main%>%
  left_join(no_vax, by=c("sweep_unique"="sweep_unique","kappa_lab"="kappa_lab"))%>%
  mutate(
    deaths_averted = num_deaths_novax - num_deaths,
    deaths_averted_e = num_deaths_e_novax - num_deaths_e,
    nnt=num_dose/deaths_averted,
    nnt_e = num_dose/deaths_averted_e)


saveRDS(nnt_main, "1_nnt_main_sens.RDS")

##Compile cases
#mod_scen <- mod_scen[1601:2400]
#sweep<- sweep[1601:2400,]

#mod_scen_int <- mod_scen_int[401:600]
#sweep_int <-sweep_int[401:600,]
sweep_int$sero_thresh <- rep(c("Annual","Biennial"),times=500)


inc <- list()
##serothresh piece
for(i in 1:2500){
  inc[[i]] <- data.frame(val=rep(0,times=3650),
                         time=rep(0, times=3650),
                         date=rep(0, times=3650),
                         sweep_unique=rep(0,times=3650),
                         sero_thresh=rep(0, times=3650))
  inc[[i]]$val<- (mod_scen[[i]]$mod_inc[,"new_I"][2:3651]/tot_pop)*100
  inc[[i]]$time <- seq(from=1, to =3650, by=1)
  inc[[i]]$date <- seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1)
  inc[[i]]$sweep_unique <- sweep$sweep_unique[i]
  inc[[i]]$sero_thresh <- sweep$sero_thresh[i]
  inc[[i]]$kappa_lab <- paste0("1/",1/sweep$kappa1[i])
  inc[[i]]$kappa <- sweep$kappa1[i]
}
## vaxint piece
for(i in 1:1000){
  inc[[2500+i]]<-  data.frame(val=rep(0,times=3650),
                             time=rep(0, times=3650),
                             date=rep(0, times=3650),
                             sweep_unique=rep(0,times=3650),
                             sero_thresh=rep(0, times=3650))
  inc[[2500+i]]$val<- (mod_scen_int[[i]]$mod_inc[,"new_I"][2:3651]/tot_pop)*100
  inc[[2500+i]]$time <- seq(from=1, to =3650, by=1)
  inc[[2500+i]]$date <- seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1)
  inc[[2500+i]]$sweep_unique <- sweep_int$sweep_unique[i]
  inc[[2500+i]]$sero_thresh <- sweep_int$sero_thresh[i]
  inc[[2500+i]]$kappa_lab <- paste0("1/",1/sweep_int$kappa1[i])
  inc[[2500+i]]$kappa <- sweep_int$kappa1[i]
}

inc<- do.call(rbind, inc)
split_inc <- split(inc,inc$kappa)
allNames <- names(split_inc)
for(i in allNames){
  saveName = paste0("inc_",i,".RDS")
  saveRDS(split_inc[[i]], file = saveName)
}


## Compile sero
sero <- list()
##serothresh piece
for(i in 1:2500){
  sero[[i]] <- data.frame(sero_c=rep(0,times=3650),
                          sero_a=rep(0,times=3650),
                          sero_e=rep(0,times=3650),
                          time=rep(0, times=3650),
                          date=rep(0, times=3650),
                          sweep_unique=rep(0,times=3650),
                          sero_thresh=rep(0, times=3650))
  sero[[i]]$sero_c<- (mod_scen[[i]]$seroprev[,"seroprev_c"][2:3651])*100
  sero[[i]]$sero_a<- (mod_scen[[i]]$seroprev[,"seroprev_a"][2:3651])*100
  sero[[i]]$sero_e<- (mod_scen[[i]]$seroprev[,"seroprev_e"][2:3651])*100
  sero[[i]]$time <- seq(from=1, to =3650, by=1)
  sero[[i]]$date <- seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1)
  sero[[i]]$sweep_unique <- sweep$sweep_unique[i]
  sero[[i]]$sero_thresh <- sweep$sero_thresh[i]
  sero[[i]]$kappa_lab <- paste0("1/",1/sweep$kappa1[i])
  sero[[i]]$kappa <- sweep$kappa1[i]
}
## vaxint piece
for(i in 1:1000){
  sero[[2500+i]]<-  data.frame(sero_c=rep(0,times=3650),
                              sero_a=rep(0,times=3650),
                              sero_e=rep(0,times=3650),
                              time=rep(0, times=3650),
                              date=rep(0, times=3650),
                              sweep_unique=rep(0,times=3650),
                              sero_thresh=rep(0, times=3650))
  sero[[2500+i]]$sero_c<- (mod_scen_int[[i]]$seroprev[,"seroprev_c"][2:3651])*100
  sero[[2500+i]]$sero_a<- (mod_scen_int[[i]]$seroprev[,"seroprev_a"][2:3651])*100
  sero[[2500+i]]$sero_e<- (mod_scen_int[[i]]$seroprev[,"seroprev_e"][2:3651])*100
  sero[[2500+i]]$time <- seq(from=1, to =3650, by=1)
  sero[[2500+i]]$date <- seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1)
  sero[[2500+i]]$sweep_unique <- sweep_int$sweep_unique[i]
  sero[[2500+i]]$sero_thresh <- sweep_int$sero_thresh[i]
  sero[[2500+i]]$kappa_lab <- paste0("1/",1/sweep_int$kappa1[i])
  sero[[2500+i]]$kappa <- sweep_int$kappa1[i]
}

sero<- do.call(rbind, sero)
split_sero <- split(sero,sero$kappa)
allNames <- names(split_sero)
for(i in allNames){
  saveName = paste0("sero_",i,".RDS")
  saveRDS(split_sero[[i]], file = saveName)
}


## Compile immune
##Immune tiers taking the medians across scenarios?
imm <- list()
list1 <- which(sweep$sero_thresh %in% c(0, 0.5,0.6,0.7,0.8))


for(i in 1:length(list1)){
  imm_tmp <- data.frame(time=rep(0, times=3650),
                        date=rep(0, times=3650),
                        sweep_unique=rep(0,times=3650),
                        sero_thresh=rep(0, times=3650))
  
  imm_tmp$time <- seq(from=1, to =3650, by=1)
  imm_tmp$date <- seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1)
  imm_tmp$sweep_unique <- sweep$sweep_unique[list1[i]]
  imm_tmp$sero_thresh <- sweep$sero_thresh[list1[i]]
  imm_tmp$kappa_lab <- paste0("1/",1/sweep$kappa1[i])
  imm_tmp$kappa <- sweep$kappa1[i]
  
  imm_sing <- mod_scen[[list1[i]]]$imm_class%>%filter(time!=0)%>%
    mutate(s0v2_a = s0v1_a+s0v2_a,
           s1v2_a = s1v1_a+s1v2_a,
           s2v2_a = s2v1_a+s2v2_a,
           s0v2_e = s0v1_e+s0v2_e,
           s1v2_e = s1v1_e+s1v2_e,
           s2v2_e = s2v1_e+s2v2_e,
           imm1_c = rec_c+vac_c,
           imm1_a = rec_a+vac_a,
           imm1_e = rec_e+vac_e)%>%
    select(time, s0v0_c, s1v0_c, s2v0_c, 
           s0v0_a, s1v0_a, s2v0_a, s0v2_a, s1v2_a, s2v2_a, s0v3_a, s1v3_a, s2v3_a,
           s0v0_e, s1v0_e, s2v0_e, s0v2_e, s1v2_e, s2v2_e, s0v3_e, s1v3_e, s2v3_e,
           imm1_c:imm1_e)
  imm[[i]]<- imm_tmp %>%left_join(imm_sing, by= c("time"="time"))
  # pivot_longer(cols =s0v0_c:imm1_e, names_to = "var", values_to="val")%>%
  #mutate(age_grp = substr(var, 6,6),
  #       imm = substr(var, 1,4),
  #       imm = factor(imm, levels = c("imm1","s0v0","s1v0","s2v0", "s0v2","s1v2","s2v2", "s0v3","s1v3","s2v3")),
  #       age_grp = factor(age_grp, levels=c("c","a","e")))
  
}

for(i in 1:1000){
  imm_tmp <- data.frame(time=rep(0, times=3650),
                        date=rep(0, times=3650),
                        sweep_unique=rep(0,times=3650),
                        sero_thresh=rep(0, times=3650))
  
  imm_tmp$time <- seq(from=1, to =3650, by=1)
  imm_tmp$date <- seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1)
  imm_tmp$sweep_unique <- sweep_int$sweep_unique[i]
  imm_tmp$sero_thresh <- sweep_int$sero_thresh[i] 
  imm_tmp$kappa_lab <- paste0("1/",1/sweep_int$kappa1[i])
  imm_tmp$kappa <- sweep_int$kappa1[i]
  
  imm_sing <- mod_scen_int[[i]]$imm_class%>%filter(time!=0)%>%
    mutate(s0v2_a = s0v1_a+s0v2_a,
           s1v2_a = s1v1_a+s1v2_a,
           s2v2_a = s2v1_a+s2v2_a,
           s0v2_e = s0v1_e+s0v2_e,
           s1v2_e = s1v1_e+s1v2_e,
           s2v2_e = s2v1_e+s2v2_e,
           imm1_c = rec_c+vac_c,
           imm1_a = rec_a+vac_a,
           imm1_e = rec_e+vac_e)%>%
    select(time, s0v0_c, s1v0_c, s2v0_c, 
           s0v0_a, s1v0_a, s2v0_a, s0v2_a, s1v2_a, s2v2_a, s0v3_a, s1v3_a, s2v3_a,
           s0v0_e, s1v0_e, s2v0_e, s0v2_e, s1v2_e, s2v2_e, s0v3_e, s1v3_e, s2v3_e,
           imm1_c:imm1_e)
  imm[[length(list1)+i]]<- imm_tmp %>%left_join(imm_sing, by= c("time"="time")) 
  # pivot_longer(cols =s0v0_c:imm1_e, names_to = "var", values_to="val")%>%
  #  mutate(age_grp = substr(var, 6,6),
  #         imm = substr(var, 1,4),
  #         imm = factor(imm, levels = c("imm1","s0v0","s1v0","s2v0", "s0v2","s1v2","s2v2", "s0v3","s1v3","s2v3")),
  #         age_grp = factor(age_grp, levels=c("c","a","e")))
  
}

imm<- do.call(rbind, imm)
split_imm <- split(imm,imm$kappa)
allNames <- names(split_imm)
for(i in allNames){
  saveName = paste0("imm_",i,".RDS")
  saveRDS(split_imm[[i]], file = saveName)
}

##Compile time-series of deaths

deaths <- list()

#mod_scen_int<-list()
#mod_scen_int[[1]]<-sw_run_1000
##serothresh piece
for(i in 1:2500){
  deaths[[i]] <- data.frame(val=rep(0,times=3650),
                         time=rep(0, times=3650),
                         date=rep(0, times=3650),
                         sweep_unique=rep(0,times=3650),
                         sero_thresh=rep(0, times=3650))
  deaths[[i]]$val<- (mod_scen[[i]]$pop_num[,"new_Deaths_tot"][2:3651]/tot_pop)*100
  deaths[[i]]$time <- seq(from=1, to =3650, by=1)
  deaths[[i]]$date <- seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1)
  deaths[[i]]$sweep_unique <- sweep$sweep_unique[i]
  deaths[[i]]$sero_thresh <- sweep$sero_thresh[i]
  deaths[[i]]$kappa_lab <- paste0("1/",1/sweep$kappa1[i])
  deaths[[i]]$kappa <- sweep$kappa1[i]
}
## vaxint piece
for(i in 1:1000){
  deaths[[2500+i]]<-  data.frame(val=rep(0,times=3650),
                             time=rep(0, times=3650),
                             date=rep(0, times=3650),
                             sweep_unique=rep(0,times=3650),
                             sero_thresh=rep(0, times=3650))
  deaths[[2500+i]]$val<- (mod_scen_int[[i]]$pop_num[,"new_Deaths_tot"][2:3651]/tot_pop)*100
  deaths[[2500+i]]$time <- seq(from=1, to =3650, by=1)
  deaths[[2500+i]]$date <- seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1)
  deaths[[2500+i]]$sweep_unique <- sweep_int$sweep_unique[i]
  deaths[[2500+i]]$sero_thresh <- sweep_int$sero_thresh[i]
  deaths[[2500+i]]$kappa_lab <- paste0("1/",1/sweep_int$kappa1[i])
  deaths[[2500+i]]$kappa <- sweep_int$kappa1[i]
}

deaths<- do.call(rbind, deaths)
split_deaths <- split(deaths,deaths$kappa)
allNames <- names(split_deaths)
for(i in allNames){
  saveName = paste0("deaths_",i,".RDS")
  saveRDS(split_deaths[[i]], file = saveName)
}

