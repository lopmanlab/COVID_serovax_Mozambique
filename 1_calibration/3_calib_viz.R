##Remove scientific notation
options(scipen=999)
library(dplyr)
library(deSolve)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

##Comparing modeled cases to reported
sweep_list<-readRDS("H:/aim3_sweep_results/sweep_list12comb.RDS")
global_targ<- readRDS("global_targ12.RDS")
sweep_input <- readRDS("0_calibration_sweep12.RDS")
sweep_input<- sweep_input%>%mutate(sweep = seq(from=1, to=10000, by=1))
cases <- read.csv("1_moz_cases_owid.csv")

###VIsualizing calibrated values
## Periods
dt_end_wave2 <- as.Date("2021-05-24")
dt_end_wave3 <- as.Date("2021-10-30")
dt_end_wave4 <- as.Date("2022-03-10")
dt_end_wave4b <-as.Date("2022-9-1")

##Undereporting guesses
case_underrep1 <- 90
case_underrep2 <- 120
case_underrep3 <-100

tot_pop <- 30066648   ## Took population for 2020 from Mozambique INE (Institute of statistics)

## Read in case data

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


filter <- global_targ%>% 
  ##Sum of squared difference in minimum top 10%
  mutate(sero_top10 = ifelse(seroprev_sumsq_age <= quantile(global_targ$seroprev_sumsq_age, probs=0.1), 1,0),
         ##Each seroprevalence estimate meets specific thresholds
         w2_urb_all10 = ifelse(abs(w2_urb_all_diff)<=0.05,1,0),
         w2_ru_c10 = ifelse(abs(w2_ru_c_diff)<=0.15,1,0),
         w2_ru_a10 = ifelse(abs(w2_ru_a_diff)<=0.15,1,0),
         w2_ru_e10 = ifelse(abs(w2_ru_e_diff)<=0.15,1,0),
         w3_ru_c10 = ifelse(abs(w3_ru_c_diff)<=0.15,1,0),
         w3_ru_a10 = ifelse(abs(w3_ru_a_diff)<=0.15,1,0),
         w3_ru_e10 = ifelse(abs(w3_ru_e_diff)<=0.15,1,0),
         w4_ru_c10 = ifelse(abs(w4_ru_c_diff)<=0.15,1,0),
         w4_ru_a10 = ifelse(abs(w4_ru_a_diff)<=0.15,1,0),
         w4_ru_e10 = ifelse(abs(w4_ru_e_diff)<=0.15,1,0))%>%
  filter(w2_urb_all10==1&w2_ru_c10==1&w2_ru_a10==1&w2_ru_e10==1&w3_ru_c10==1&w3_ru_a10==1&w3_ru_e10==1&w4_ru_c10==1&w4_ru_a10==1&
           w4_ru_e10==1)%>%
  arrange(seroprev_sumsq_age)%>%
  filter(end_calib_all>0.65&w42_peak_inf<=50000)

## Select inputs from calibration
global_filter <- filter%>% group_by(bl)%>%
  slice(n=1)

##Plot outputs from calibrated parameters of the 500
list <-unlist(global_filter$sweep)
inc <- as.data.frame(matrix(data=0, nrow=1214, ncol=length(list)))


sweep_filter <- sweep_list[unlist(list)]
for(i in 1:length(list)){
  inc[,i]<- ((sweep_filter[[i]]$mod_inc[,"new_I"]/tot_pop)*100)
}

inc <- inc %>% 
  mutate(date = seq(from = as.Date("2020-5-6"), to = as.Date("2023-9-1"), by = 1))%>%
  filter(!is.na(V1))%>%
  rowwise()%>%
  mutate(med = median(V1:V100))
med <-inc%>%select(date,med)

exp_inf <- cases %>% select(date,exp_new_infect)%>%
  mutate(cases_per100 = round((exp_new_infect/tot_pop)*100*0.6,digits=4))

p1 <- inc%>%
  filter(date <=as.Date("2022-9-1"))%>%
  select(date, V1:V200)%>%
  pivot_longer(cols=V1:V200, names_to = "run", values_to="val")%>%
  ggplot(aes(x=date, y=val))+
  geom_line(aes(x=date, y=val), col="gray78")+
  #geom_line(data=med, aes(x=time,y=med), col="darkblue", size=0.5, alpha=0.5)+
  geom_line(data=exp_inf, aes(x=date, y=cases_per100), col="#3182BD", linetype="dashed",size=1)+theme_bw()+
  ggtitle("Modeled epidemic vs reported cases")+
  xlab("Date") + ylab("Cases per 100")+
  theme(plot.title = element_text(size=14),
        axis.text = element_text(size=9),
        axis.title = element_text(size=9),
        strip.text = element_text(size=9),
        legend.position = "none")

png("calib_cases.png", width = 7,height = 5,units="in",res=400)
p1
dev.off()


### Comparing modeled serology to estimates
sero_hist <- read.csv("1_seroprev_sum.csv")


##Plot age-specific seroprevalence
sero_cr <- as.data.frame(matrix(data=0, nrow=849, ncol=length(sweep_list)))
sero_ar <- as.data.frame(matrix(data=0, nrow=849, ncol=length(sweep_list)))
sero_er <- as.data.frame(matrix(data=0, nrow=849, ncol=length(sweep_list)))
sero_rl <- as.data.frame(matrix(data=0, nrow=849, ncol=length(sweep_list)))
sero_ul <- as.data.frame(matrix(data=0, nrow=849, ncol=length(sweep_list)))


for(i in 1:length(sweep_list)){
  sero_cr[,i]<- ((sweep_list[[i]]$seroprev[,"seroprev_cr"]))
  sero_ar[,i]<- ((sweep_list[[i]]$seroprev[,"seroprev_ar"]))
  sero_er[,i]<-((sweep_list[[i]]$seroprev[,"seroprev_er"]))
  sero_rl[,i]<-((sweep_list[[i]]$seroprev[,"seroprev_rl"]))
  sero_ul[,i]<-((sweep_list[[i]]$seroprev[,"seroprev_ul"]))
  
}

sero_cr1 <- sero_cr%>%
  filter(!is.na(V1))%>%
  mutate(date = seq(from=as.Date("2020-5-6"), to =as.Date("2020-5-6")+848, by=1))%>%
  rowwise()%>%
  mutate(med = median(V1:V10000),
         age_grp="Rural child")

sero_ar1 <- sero_ar%>%
  filter(!is.na(V1))%>%
  mutate(date = seq(from=as.Date("2020-5-6"), to =as.Date("2020-5-6")+848, by=1))%>%
  rowwise()%>%
  mutate(med = median(V1:V10000),
         age_grp="Rural adult")

sero_er1 <- sero_er%>%
  filter(!is.na(V1))%>%
  mutate(date = seq(from=as.Date("2020-5-6"), to =as.Date("2020-5-6")+848, by=1))%>%
  rowwise()%>%
  mutate(med = median(V1:V10000),
         age_grp="Rural older adult")

sero_rl1 <- sero_rl%>%
  filter(!is.na(V1))%>%
  mutate(date = seq(from=as.Date("2020-5-6"), to =as.Date("2020-5-6")+848, by=1))%>%
  rowwise()%>%
  mutate(med = median(V1:V10000),
         age_grp="Rural all ages")

sero_ul1 <- sero_ul%>%
  filter(!is.na(V1))%>%
  mutate(date = seq(from=as.Date("2020-5-6"), to =as.Date("2020-5-6")+848, by=1))%>%
  rowwise()%>%
  mutate(med = median(V1:V10000),
         age_grp="Urban all ages")

sero <- rbind(sero_cr1,sero_ar1,sero_er1, sero_rl1, sero_ul1)
med <- rbind(sero_cr1[,c("date","med","age_grp")], 
             sero_ar1[,c("date","med","age_grp")], 
             sero_er1[,c("date","med","age_grp")],
             sero_rl1[,c("date","med","age_grp")],
             sero_ul1[,c("date","med","age_grp")])

med <- med %>%ungroup()%>%
  mutate(age_grp=factor(age_grp, levels = c("Rural child",
                                            "Rural adult",
                                            "Rural older adult",
                                            "Rural all ages",
                                            "Urban all ages")))%>%
  arrange(age_grp)%>%
  filter(age_grp !="Rural all ages")%>%
  mutate(age_grp=factor(age_grp, levels = c("Rural child","Rural adult","Rural older adult", "Urban all ages")))

sero <- sero%>%
  select(date, age_grp, V1:V10000)%>%
  pivot_longer(cols=V1:V10000, names_to = "run", values_to="val") %>%
  filter(age_grp !="Rural all ages")%>%
  mutate(age_grp=factor(age_grp, levels = c("Rural child","Rural adult","Rural older adult", "Urban all ages")))%>%
  arrange(age_grp)

sero_hist_simp <- sero_hist%>%
                  mutate(age_urb = paste(age_group,urb_rural, sep="_"),
                         date=as.Date(dt_end))%>%
                  left_join(data.frame(age_urb = c("a_urban","c_urban","e_urban","all_urban",
                                                   "a_rural","c_rural","e_rural","all_rural"),
                                       age_grp = c("Urban adult","Urban child","Urban older adult","Urban all ages",
                                                 "Rural adult", "Rural child","Rural older adult","Rural all ages")),
                            by="age_urb")%>%
          
                  filter(age_urb %in% c("all_urban","c_rural","a_rural","e_rural"))%>%
                  filter(period != "post_wave4_r2")%>%
  mutate(age_grp=factor(age_grp, levels = c("Rural child","Rural adult","Rural older adult", "Urban all ages")))


p2 <- sero%>% 
  ggplot(aes(x=date, y=val))+
  geom_line(aes(x=date, y=val, lty = 'range'), col="gray78", alpha=0.5)+
  geom_line(data=med, aes(x=date, y = med, col = age_grp, lty="median"),size=1, linetype="dashed") +
  geom_point(data=sero_hist_simp, aes(x=date, y=prev, shape = "survey estimates"),color="red", alpha=0.5, size=3, shape=18)+ylim(0,1)+
  scale_color_manual(values = c("#9ECAE1","#4292C6","#08306B","#4D004B"))+
  theme_bw()+
  theme(plot.title = element_text(size=9),
        axis.text.x = element_text(size=9, angle = 10),
        axis.text.y = element_text(size=9),
        axis.title = element_text(size=9),
        strip.text = element_text(size=9),
        legend.position = "none")+
  xlab("Date") + ylab("Seroprevalence")+
  facet_wrap(~age_grp, ncol=2)

png("calib_sero.png", width = 7,height = 5,units="in",res=400)
p2
dev.off()



png("calib_viz.png", width = 10,height = 5,units="in",res=400)
ggarrange(p1,p2,ncol=2, align="h",widths=c(1, 1))
dev.off()

##serology median and ranges
##Plot age-specific seroprevalence
sero_last <-as.data.frame(matrix(data=0, nrow=10000, ncol=13))



for(i in 1:length(sweep_list)){
  
  sero_last[i,] <- sweep_list[[i]]$seroprev[849,]
  
}

colnames(sero_last)<-colnames(sweep_list[[1]]$seroprev)

sero_last %>% select(seroprev_c:seroprev_e, seroprev_to)%>%
  pivot_longer(cols=seroprev_c:seroprev_to, names_to = "var", values_to = "val")%>%
  group_by(var)%>%
  summarise(median=median(val),
            prob_lo = min(val),
            prob_25 = quantile(val, probs=0.25),
            prob_75 = quantile(val, probs=0.75),
            prob_hi = max(val)
            )

##Immunity distributions for median scenario
last <- readRDS("../0_forward/0_Rrand_inctrans/0_last_Rrand.RDS")
last<- last %>% select(-contains("foi")) %>%
  select(-(contains("Ecum")|contains("Icum")))

pop_num <- last %>% mutate(
  
  Nchildr = rowSums(select(.,contains('cr')&(-starts_with("D")))),    ## Total in the population (exclude deaths)
  Nchildu = rowSums(select(.,contains('cu')&(-starts_with("D")))),
  Nadultr = rowSums(select(.,contains('ar')&(-starts_with("D")))),
  Nadultu = rowSums(select(.,contains('au')&(-starts_with("D")))),
  Noldr =   rowSums(select(.,contains('er')&(-starts_with("D")))),
  Noldu =   rowSums(select(.,contains('eu')&(-starts_with("D")))),
  NTot = Nchildr + Nchildu +Nadultr +Nadultu +Noldr+Noldu,  
  Nchild = Nchildr+Nchildu,
  Nadult = Nadultr+Nadultu,
  Nold = Noldr+Noldu)

imm_class <- last %>%
  mutate(
    s0v0_c = rowSums(select(., contains('Scr')|contains('Scu'))),
    s0v0_a = rowSums(select(., contains('Sar')|contains('Sau'))),
    s0v0_e = rowSums(select(., contains('Ser')|contains('Seu'))),
    
    s1v0_c = rowSums(select(.,Spcr1v0,Sncr1v0, Spcu1v0, Spcu1v0)),
    s1v0_a = rowSums(select(.,Spar1v0,Snar1v0, Spau1v0, Snau1v0)),
    s1v0_e = rowSums(select(.,Sper1v0,Sner1v0, Speu1v0, Sneu1v0)),
    
    s2v0_c = rowSums(select(.,Spcr2v0,Sncr2v0, Spcu2v0, Spcu2v0)),
    s2v0_a = rowSums(select(.,Spar2v0,Snar2v0, Spau2v0, Snau2v0)),
    s2v0_e = rowSums(select(.,Sper2v0,Sner2v0, Speu2v0, Sneu2v0)),
    
    s0v1_c = rowSums(select(.,Spcr0v1,Sncr0v1, Spcu0v1, Spcu0v1)),
    s0v1_a = rowSums(select(.,Spar0v1,Snar0v1, Spau0v1, Snau0v1)),
    s0v1_e = rowSums(select(.,Sper0v1,Sner0v1, Speu0v1, Sneu0v1)),
    
    s1v1_c = rowSums(select(.,Spcr1v1,Sncr1v1, Spcu1v1, Spcu1v1)),
    s1v1_a = rowSums(select(.,Spar1v1,Snar1v1, Spau1v1, Snau1v1)),
    s1v1_e = rowSums(select(.,Sper1v1,Sner1v1, Speu1v1, Sneu1v1)),
    
    s2v1_c = rowSums(select(.,Spcr2v1,Sncr2v1, Spcu2v1, Spcu2v1)),
    s2v1_a = rowSums(select(.,Spar2v1,Snar2v1, Spau2v1, Snau2v1)),
    s2v1_e = rowSums(select(.,Sper2v1,Sner2v1, Speu2v1, Sneu2v1)),
    
    s0v2_c = rowSums(select(.,Spcr0v2,Sncr0v2, Spcu0v2, Spcu0v2)),
    s0v2_a = rowSums(select(.,Spar0v2,Snar0v2, Spau0v2, Snau0v2)),
    s0v2_e = rowSums(select(.,Sper0v2,Sner0v2, Speu0v2, Sneu0v2)),
    
    s1v2_c = rowSums(select(.,Spcr1v2,Sncr1v2, Spcu1v2, Spcu1v2)),
    s1v2_a = rowSums(select(.,Spar1v2,Snar1v2, Spau1v2, Snau1v2)),
    s1v2_e = rowSums(select(.,Sper1v2,Sner1v2, Speu1v2, Sneu1v2)),
    
    s2v2_c = rowSums(select(.,Spcr2v2,Sncr2v2, Spcu2v2, Spcu2v2)),
    s2v2_a = rowSums(select(.,Spar2v2,Snar2v2, Spau2v2, Snau2v2)),
    s2v2_e = rowSums(select(.,Sper2v2,Sner2v2, Speu2v2, Sneu2v2)),
    
    s0v3_c = rowSums(select(.,Spcr0v3,Sncr0v3, Spcu0v3, Spcu0v3)),
    s0v3_a = rowSums(select(.,Spar0v3,Snar0v3, Spau0v3, Snau0v3)),
    s0v3_e = rowSums(select(.,Sper0v3,Sner0v3, Speu0v3, Sneu0v3)),
    
    s1v3_c = rowSums(select(.,Spcr1v3,Sncr1v3, Spcu1v3, Spcu1v3)),
    s1v3_a = rowSums(select(.,Spar1v3,Snar1v3, Spau1v3, Snau1v3)),
    s1v3_e = rowSums(select(.,Sper1v3,Sner1v3, Speu1v3, Sneu1v3)),
    
    s2v3_c = rowSums(select(.,Spcr2v3,Sncr2v3, Spcu2v3, Spcu2v3)),
    s2v3_a = rowSums(select(.,Spar2v3,Snar2v3, Spau2v3, Snau2v3)),
    s2v3_e = rowSums(select(.,Sper2v3,Sner2v3, Speu2v3, Sneu2v3)),
    
    sus_c = rowSums(select(., ((contains('cu')|contains('cr'))&(contains('Sp')|contains('Sn')))|contains('Scr')|contains('Scu'))),
    sus_a = rowSums(select(., ((contains('au')|contains('ar'))&(contains('Sp')|contains('Sn')))|contains('Sar')|contains('Sau'))),
    sus_e = rowSums(select(., ((contains('eu')|contains('er'))&(contains('Sp')|contains('Sn')))|contains('Ser')|contains('Seu'))),
    
    rec_c = rowSums(select(., (contains('cu')|contains('cr'))&(contains('Rp')|contains('Rn')))),
    rec_a = rowSums(select(., (contains('au')|contains('ar'))&(contains('Rp')|contains('Rn')))),
    rec_e = rowSums(select(., (contains('eu')|contains('er'))&(contains('Rp')|contains('Rn')))),
    
    vac_c = rowSums(select(., (contains('cu')|contains('cr'))&(contains('Vp')|contains('Vn')))), 
    vac_a = rowSums(select(., (contains('au')|contains('ar'))&(contains('Vp')|contains('Vn')))), 
    vac_e = rowSums(select(., (contains('eu')|contains('er'))&(contains('Vp')|contains('Vn'))))) %>% 
  
  select(s0v0_c:vac_e)

imm_class <- imm_class %>%
  mutate(partsus_c = sus_c -s0v0_c,
         partsus_a = sus_a -s0v0_a,
         partsus_e = sus_e -s0v0_e,
         imm_c = rec_c+vac_c,
         imm_a = rec_a+vac_a,
         imm_e = rec_e+vac_e)%>%
  pivot_longer(cols = s0v0_c:imm_e, names_to = "var", values_to="val")%>%
  mutate(cat = gsub("_.*$","",var),
         age_grp=sub(".*_", "", var),
         exp = substr(cat, 1,2),
         vax = substr(cat,3,4))

imm_class <- imm_class%>%
  left_join(data.frame(age_grp = c("c","a","e"),
                        pop = c(15767921,12815562,1416282)))

imm_class <- imm_class%>%mutate(prop=val/pop)

