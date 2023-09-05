##Remove scientific notation
options(scipen=999)
library(dplyr)
library(deSolve)
## The purpose of this script is to calculate target statistics from the calibration sweeps
## 

library(ggplot2)
library(tidyverse)

#read in information from sweeps

sweep<- readRDS("0_calibration_sweep12.RDS")
sweep_list <- readRDS("sweep_calib_res.RDS")
##read in simulation batch data

## Periods
dt_end_wave2 <- as.Date("2021-05-24")
dt_end_wave3 <- as.Date("2021-10-30")
dt_end_wave4 <- as.Date("2022-03-10")
dt_end_wave4b <-as.Date("2022-9-1")

##Undereporting guesses
case_underrep1 <- 120
case_underrep2 <- 140
case_underrep3 <-140

tot_pop <- 30066648   ## Took population for 2020 from Mozambique INE (Institute of statistics)

## Read in case data
cases <- read.csv("1_moz_cases_owid.csv")
cases <- cases %>%
  select(date, total_cases, new_cases, new_cases_smoothed, total_deaths, new_deaths, new_deaths_smoothed)%>%
  mutate(date = as.Date(date, format= "%m/%d/%Y")) %>%
  dplyr::rename(rep_tot_cases = total_cases)%>%
  dplyr::rename(rep_new_cases = new_cases)%>%
  dplyr::rename(rep_new_cases_sm = new_cases_smoothed) %>%
  dplyr::rename(rep_tot_deaths = total_deaths) %>%
  dplyr::rename(rep_new_deaths = new_deaths) %>%
  dplyr::rename(rep_new_deaths_sm =new_deaths_smoothed) %>%
  mutate(underrep = case_when(
    #date <= as.Date("2021-7-1")~ case_underrep1,
    #date <= as.Date("2021-12-20")~case_underrep2,
    date <= dt_end_wave2~ case_underrep1,
    date <= dt_end_wave3~case_underrep2,
    TRUE ~case_underrep3
  ),
  periods = case_when(
    date<=dt_end_wave2 ~"Wave1_2",
    date<=dt_end_wave3 ~"Wave3_delta",
    date<=dt_end_wave4 ~"Wave4_omi",
    TRUE ~ "Wave4_post"
  ),
  exp_new_infect = rep_new_cases_sm *underrep,
  exp_tot_infect = rep_tot_cases *underrep)



## Read in historical seroprevalence
sero_hist <- read.csv("1_seroprev_sum.csv")


## Functions below take model outputs and use them to calculate different target statistics, returns dataframe comparisons
##Age-specific seroprevalence target
seroprev_targ<- function(x, waves="nofour"){
  sero_hist <- read.csv("1_seroprev_sum.csv")
  ### Age-specific seroprevalence
  s1_cu <- x$seroprev_cu[which(x$date==as.Date("2020-11-21"))]
  s1_au <- x$seroprev_au[which(x$date==as.Date("2020-11-21"))]
  s1_eu <- x$seroprev_eu[which(x$date==as.Date("2020-11-21"))]
  s1_ul <- x$seroprev_ul[which(x$date==as.Date("2020-11-21"))]
  
  #Urban in the middle/after wave2
  s2_ul <- x$seroprev_ul[which(x$date==as.Date("2021-03-01"))]
  
  #Rural after wave wave2
  s3_cr <- x$seroprev_cr[which(x$date==as.Date("2021-6-18"))]
  s3_ar <- x$seroprev_ar[which(x$date==as.Date("2021-6-18"))]
  s3_er <- x$seroprev_er[which(x$date==as.Date("2021-6-18"))]
  s3_rl <- x$seroprev_rl[which(x$date==as.Date("2021-6-18"))]
  
  #Urban after wave2
  s4_ul <- x$seroprev_ul[which(x$date==as.Date("2021-07-01"))]
  
  #Rural after wave 3 delta
  ##Chose the midpoint of the data collection period
  s5_cr <- x$seroprev_cr[which(x$date==as.Date("2021-10-31"))]
  s5_ar <- x$seroprev_ar[which(x$date==as.Date("2021-10-31"))]
  s5_er <- x$seroprev_er[which(x$date==as.Date("2021-10-31"))]
  s5_rl <- x$seroprev_rl[which(x$date==as.Date("2021-10-31"))]
  
  #Rural after wave 4 omicron
  ## Chose the midpoint of the data collection period
  s6_cr <- x$seroprev_cr[which(x$date==as.Date("2022-1-31"))]
  s6_ar <- x$seroprev_ar[which(x$date==as.Date("2022-1-31"))]
  s6_er <- x$seroprev_er[which(x$date==as.Date("2022-1-31"))]
  s6_rl <- x$seroprev_rl[which(x$date==as.Date("2022-1-31"))]
  
  s7_cr <- x$seroprev_cr[which(x$date==as.Date("2022-5-28"))]
  s7_ar <- x$seroprev_ar[which(x$date==as.Date("2022-5-28"))]
  s7_er <- x$seroprev_er[which(x$date==as.Date("2022-5-28"))]
  s7_rl <- x$seroprev_rl[which(x$date==as.Date("2022-5-28"))]
  
  s8_c <- x$seroprev_c[which(x$date==as.Date("2022-9-1"))]
  s8_a <- x$seroprev_a[which(x$date==as.Date("2022-9-1"))]
  s8_e <- x$seroprev_e[which(x$date==as.Date("2022-9-1"))]
  s8_to <- x$seroprev_to[which(x$date==as.Date("2022-9-1"))]
  s8_ul <- x$seroprev_ul[which(x$date==as.Date("2022-9-1"))]
  s8_rl <- x$seroprev_rl[which(x$date==as.Date("2022-9-1"))]
  
  if(waves=="nofour"){
      seroprev_mod <- data.frame(prev_mod = c(s1_cu, s1_au, s1_eu, s1_ul,            #put point estimates into dataframe
                                              s2_ul, s3_cr, s3_ar, s3_er, s3_rl, 
                                              s4_ul, s5_cr, s5_ar, s5_er, s5_rl),
                                 
                                 period = c(rep("post_wave1",4),"wave2",rep("post_wave2",5),rep("post_wave3",4)),   ##identify the point estimates by wave/period, urban/rural and age group
                                 urb_rural=c(rep("urban",5),rep("rural",4),"urban",rep("rural",4)),
                                 age_group=c("c","a","e","all","all","c","a","e","all","all",rep(c("c","a","e","all"),times=1)))
  
      seroprev_comp<- seroprev_mod %>%left_join(
                          sero_hist%>%select(period, urb_rural, age_group, prev), 
                          by =c("period"="period","urb_rural"="urb_rural","age_group"="age_group"))
  } else {
      seroprev_mod <- data.frame(prev_mod = c(s1_cu, s1_au, s1_eu, s1_ul,           
                                              s2_ul, s3_cr, s3_ar, s3_er, s3_rl,
                                              s4_ul, s5_cr, s5_ar, s5_er, s5_rl,
                                              s6_cr, s6_ar, s6_er, s6_rl), 
                                              #s7_cr, s7_ar, s7_er, s7_rl),
                                 
                               period = c(rep("post_wave1",4),"wave2",rep("post_wave2",5),rep("post_wave3",4),rep("post_wave4",4)),
                               urb_rural=c(rep("urban",5),rep("rural",4),"urban",rep("rural",8)),
                               age_group=c("c","a","e","all","all","c","a","e","all","all",rep(c("c","a","e","all"),times=2)))
    
      seroprev_comp <- seroprev_mod %>%left_join(
                            sero_hist%>%select(period, urb_rural, age_group, prev), 
                            by =c("period"="period","urb_rural"="urb_rural","age_group"="age_group"))
    
  }

  seroprev_comp<- seroprev_comp%>%
                  mutate(square_diff = (prev_mod-prev)^2,
                         prop_diff = prev_mod-prev)
  
  seroprev_comp<- rbind(seroprev_comp,
                   data.frame(prev_mod = c(s7_cr, s7_ar, s7_er, s7_rl, s8_c, s8_a, s8_e, s8_ul,s8_rl,s8_to),
                         period = c(rep("post_wave4_2",4),rep("end_calib",6)),
                         urb_rural = c(rep("rural",4), rep("all", 3), "urban", "rural", "all"),
                         age_group =c("c","a","e","all","c","a","e","all","all","all"),
                         square_diff = rep(0),
                         prop_diff = rep(0),
                         prev=rep(NA)))
  print(seroprev_comp)
}

## Seroprev after wave 2 in rural = 0.27, seroprev after wave 2 in urban =0.21, no vax at that time
## With 70622 reported cases in this period, back of hand estimate is that underreporting is between 80-120
## For Wave 3 delta, seroprevalence increased from 0.21/0.27 to 0.54, if no one seroreverted and we don't account for vaccinations
## Underreporting between 100-120, substantial vax would decrease underreporting and more seroreversion would increase underreporting

target_cum_case <- cases %>%group_by(periods)%>%
  dplyr::summarise(tot_rep_cases = sum(rep_new_cases,na.rm=T),
                   tot_rep_cases_sm=sum(rep_new_cases_sm,na.rm=T),
                   exp_new_infect = sum(exp_new_infect,na.rm=T),
                   peak_cases = max(rep_new_cases_sm, na.rm=T))

cum_case_targ <- function(x){
                 options(scipen=999)
                 t1<- x%>%mutate(
                    periods = case_when(
                      date<=dt_end_wave2 ~"Wave1_2",
                      date<=dt_end_wave3 ~"Wave3_delta",
                      date<=dt_end_wave4 ~"Wave4_omi",
                      date<=dt_end_wave4b ~"Wave4_omi2",
                      TRUE ~ "Wave4_post")) %>%
                      
                      group_by(periods)%>%
                      dplyr::summarise(tot_mod_infections = sum(new_E, na.rm=T),
                                       peak_inf = max(new_E, na.rm=T)) %>%
                        
                        left_join(target_cum_case)%>%
                        mutate(prop_mod_to_exp = tot_mod_infections/exp_new_infect,
                               underep_cum = round(tot_mod_infections/tot_rep_cases_sm, digits=0),
                               underep_peak = round(peak_inf/peak_cases, digits=0))
                 print(t1)
}

distr_sus_last <- function(x){
              t2 <- x %>%
                mutate(
                  Sus = rowSums(select(., contains('Sp')|contains('Sn')|contains('Scr')|contains('Scu')|contains('Sar')|contains('Sau')|contains('Ser')|contains('Seu'))),
                  Rec = rowSums(select(., contains('Rp')|contains('Rn'))),
                  Vac = rowSums(select(., contains('Vp')|(contains('Vn')))),
                  
                  S0v0 = rowSums(select(.,contains("0v0")&contains("S"))),
                  S0v1 = rowSums(select(.,contains("0v1")&contains("S"))),
                  S0v2 = rowSums(select(.,contains("0v2")&contains("S"))),
                  S0v3 = rowSums(select(.,contains("0v3")&contains("S"))),
                  S1v0 = rowSums(select(.,contains("1v0")&contains("S"))),
                  S1v1 = rowSums(select(.,contains("1v1")&contains("S"))),
                  S1v2 = rowSums(select(.,contains("1v2")&contains("S"))),
                  S1v3 = rowSums(select(.,contains("1v3")&contains("S"))),
                  S2v0 = rowSums(select(.,contains("2v0")&contains("S"))),
                  S2v1 = rowSums(select(.,contains("2v1")&contains("S"))),
                  S2v2 = rowSums(select(.,contains("2v2")&contains("S"))),
                  S2v3 = rowSums(select(.,contains("2v3")&contains("S"))),
                  
                  R1v0 = rowSums(select(.,contains("1v0")&(contains("Rp")|contains("Rn")))),
                  R1v1 = rowSums(select(.,contains("1v1")&(contains("Rp")|contains("Rn")))),
                  R1v2 = rowSums(select(.,contains("1v2")&(contains("Rp")|contains("Rn")))),
                  R1v3 = rowSums(select(.,contains("1v3")&(contains("Rp")|contains("Rn")))),
                  R2v0 = rowSums(select(.,contains("2v0")&(contains("Rp")|contains("Rn")))),
                  R2v1 = rowSums(select(.,contains("2v1")&(contains("Rp")|contains("Rn")))),
                  R2v2 = rowSums(select(.,contains("2v2")&(contains("Rp")|contains("Rn")))),
                  R2v3 = rowSums(select(.,contains("2v3")&(contains("Rp")|contains("Rn")))),
                  R3v0 = rowSums(select(.,contains("3v0")&(contains("Rp")|contains("Rn")))),
                  R3v1 = rowSums(select(.,contains("3v1")&(contains("Rp")|contains("Rn")))),
                  R3v2 = rowSums(select(.,contains("3v2")&(contains("Rp")|contains("Rn")))),
                  R3v3 = rowSums(select(.,contains("3v3")&(contains("Rp")|contains("Rn"))))
                ) %>%
                mutate(
                  prop_immune = (Rec+Vac)/(Rec+Vac+Sus)
                )
              print(t2)
              
}
  
##Cumulative cases after each wave target

for (i in 1:length(sweep_list)){
  sweep_list[[i]]$seroprev_targ <- seroprev_targ(sweep_list[[i]]$seroprev, waves="four")
  sweep_list[[i]]$cum_case_targ <- cum_case_targ(sweep_list[[i]]$mod_inc)
  sweep_list[[i]]$last_t <- distr_sus_last(sweep_list[[i]]$last_t)
}

##compiling all the target stats for each run
##input number of total simulations
n<-10000
global_targ <- data.frame(sweep = seq(1:n),
                          seroprev_sumsq_age =rep(0,n), 
                          w2_ru_c_diff = rep(0,n),
                          w2_ru_a_diff = rep(0,n),
                          w2_ru_e_diff = rep(0,n),
                          w2_urb_all_diff = rep(0,n),
                          w3_ru_c_diff = rep(0,n),
                          w3_ru_a_diff = rep(0,n),
                          w3_ru_e_diff = rep(0,n),
                          w4_ru_c_diff = rep(0,n),
                          w4_ru_a_diff = rep(0,n),
                          w4_ru_e_diff = rep(0,n),
                          
                          w12_unrep=rep(0,n),
                          w3_unrep=rep(0,n),
                          w4_unrep=rep(0,n),
                          w2_peak_unrep=rep(0,n),
                          w3_peak_unrep=rep(0,n))

## Compile into data frame of a bunch of calculated target statistics
for(i in 1:length(sweep_list)){
  age_specific <- which(sweep_list[[1]]$seroprev_targ$age_group!="all")
  ## Overall sum square diff 
  global_targ$seroprev_sumsq_age[i] <- sum(sweep_list[[i]]$seroprev_targ$square_diff[age_specific])
  
  ## Last urban seroprev estimate and three rural seroprev estimate can not be more than 10 point different
  global_targ$w2_ru_c_diff[i] <- sweep_list[[i]]$seroprev_targ$prop_diff[6]
  global_targ$w2_ru_a_diff[i] <- sweep_list[[i]]$seroprev_targ$prop_diff[7]
  global_targ$w2_ru_e_diff[i] <- sweep_list[[i]]$seroprev_targ$prop_diff[8]
  global_targ$w2_urb_all_diff[i] <- sweep_list[[i]]$seroprev_targ$prop_diff[10]
  global_targ$w3_ru_c_diff[i] <- sweep_list[[i]]$seroprev_targ$prop_diff[11]
  global_targ$w3_ru_a_diff[i] <- sweep_list[[i]]$seroprev_targ$prop_diff[12]
  global_targ$w3_ru_e_diff[i] <- sweep_list[[i]]$seroprev_targ$prop_diff[13]
  global_targ$w4_ru_c_diff[i] <- sweep_list[[i]]$seroprev_targ$prop_diff[15]
  global_targ$w4_ru_a_diff[i] <- sweep_list[[i]]$seroprev_targ$prop_diff[16]
  global_targ$w4_ru_e_diff[i] <- sweep_list[[i]]$seroprev_targ$prop_diff[17]
  
  global_targ$w12_unrep[i]<- sweep_list[[i]]$cum_case_targ$underep_cum[1]
  global_targ$w3_unrep[i]<- sweep_list[[i]]$cum_case_targ$underep_cum[2]
  global_targ$w4_unrep[i]<-sweep_list[[i]]$cum_case_targ$underep_cum[3]
  
 
  global_targ$w2_peak_unrep[i]<- sweep_list[[i]]$cum_case_targ$underep_peak[1]  ## W2 peak underrep
  global_targ$w3_peak_unrep[i]<- sweep_list[[i]]$cum_case_targ$underep_peak[2]  ## W3 peak underrep
  
  global_targ$w42_peak_inf[i]<- sweep_list[[i]]$cum_case_targ$peak_inf[4]
  global_targ$end_calib_all[i]<-sweep_list[[i]]$seroprev_targ$prev_mod[28]
  
  global_targ$last_prop_imm[i]<-sweep_list[[i]]$last_t$prop_immune[1]

}
sweep$sweep<-seq(1:n)
global_targ <- global_targ%>%
                left_join(sweep %>% select(sweep, kappa1, relbeta_c, relbeta_a, bl, rel_delta, imm_esc_factor_t1, r0, r0_hyp,rel_omi, imm_esc_factor_omi))%>%
                arrange(seroprev_sumsq_age)

filter <- global_targ%>% 
  mutate(sero_top10 = ifelse(seroprev_sumsq_age <= quantile(global_targ$seroprev_sumsq_age, probs=0.1), 1,0),
         w2_urb_all10 = ifelse(abs(w2_urb_all_diff)<=0.1,1,0),
         w2_ru_c10 = ifelse(abs(w2_ru_c_diff)<=0.1,1,0),
         w2_ru_a10 = ifelse(abs(w2_ru_a_diff)<=0.1,1,0),
         w2_ru_e10 = ifelse(abs(w2_ru_e_diff)<=0.1,1,0),
         w3_ru_c10 = ifelse(abs(w3_ru_c_diff)<=0.1,1,0),
         w3_ru_a10 = ifelse(abs(w3_ru_a_diff)<=0.1,1,0),
         w3_ru_e10 = ifelse(abs(w3_ru_e_diff)<=0.1,1,0),
         w4_ru_c10 = ifelse(abs(w4_ru_c_diff)<=0.05,1,0),
         w4_ru_a10 = ifelse(abs(w4_ru_a_diff)<=0.05,1,0),
         w4_ru_e10 = ifelse(abs(w4_ru_e_diff)<=0.05,1,0))%>%
  filter(w2_urb_all10==1&w2_ru_c10==1&w2_ru_a10==1&w2_ru_e10==1&w3_ru_c10==1&w3_ru_a10==1&w3_ru_e10==1&w4_ru_c10==1&w4_ru_a10==1&w4_ru_e10==1)%>%
  arrange(seroprev_sumsq_age)

filter2<- filter%>%filter(end_calib_all>0.72&w42_peak_inf<=80000)

#filter2<- filter%>%filter(w42_peak_inf<=50000)
#filter2 <- filter[1:500,]


##Code to visualize what the waves look like
list <-unlist(filter2$sweep)
p <- list()
for (i in 1:length(list)){
  p[[i]] <- sweep_list[[list[[i]]]]$mod_inc %>%
  mutate(date =  seq(from = as.Date("2020-05-06"), to=as.Date("2023-9-1"), by =1))%>%
  left_join(cases) %>% select(date,new_E, exp_new_infect)%>%
  pivot_longer(cols = new_E:exp_new_infect, names_to = "var") %>%
  mutate(cases_percap = round(value/tot_pop,digits=4))%>%
  ggplot()+
  geom_line(aes(x=date, y = cases_percap, col = var, linetype=var), size=1)  +  
  scale_color_manual(values = c("#3182BD","#9ECAE1"),
                     labels = c("Reported new cases", "Modeled cases")) +
  scale_linetype_manual(values = c("solid","dashed"), labels=c("",""))+
  theme(legend.position = "bottom",legend.title=element_blank(),
        axis.text.x=element_text(size=10),
        axis.text.y = element_text(size=10))+xlab("")+ylab("Per cap infections")+
  ggtitle("")
  
}



####



