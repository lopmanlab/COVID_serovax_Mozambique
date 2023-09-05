##This script will take the sweep_list.RDS from the targetstat_calc.R and convert it into imports to the forward simulation
##First read in sweep_list and the data frame of target statistics

##Remove scientific notation
options(scipen=999)
library(dplyr)
library(deSolve)
library(ggplot2)
library(tidyverse)
#sweep_list<-readRDS("sweep_list12comb.RDS")
global_targ<- readRDS("global_targ12.RDS")
sweep_input <- readRDS("0_calibration_sweep12.RDS")
sweep_input<- sweep_input%>%mutate(sweep = seq(from=1, to=10000, by=1))

#sweep_redo <- sweep_input[which(sweep_input$sweep %in% ]

##Filtered scenarios
filter <- global_targ%>% 
  ##Sum of squared difference in minimum top 10%
  mutate(sero_top10 = ifelse(seroprev_sumsq_age <= quantile(global_targ$seroprev_sumsq_age, probs=0.1), 1,0),
  ##Each seroprevalence estimate meets specific thresholds
         w2_urb_all10 = ifelse(abs(w2_urb_all_diff)<=0.1,1,0),
         w2_ru_c10 = ifelse(abs(w2_ru_c_diff)<=0.1,1,0),
         w2_ru_a10 = ifelse(abs(w2_ru_a_diff)<=0.1,1,0),
         w2_ru_e10 = ifelse(abs(w2_ru_e_diff)<=0.1,1,0),
         w3_ru_c10 = ifelse(abs(w3_ru_c_diff)<=0.1,1,0),
         w3_ru_a10 = ifelse(abs(w3_ru_a_diff)<=0.1,1,0),
         w3_ru_e10 = ifelse(abs(w3_ru_e_diff)<=0.1,1,0),
         w4_ru_c10 = ifelse(abs(w4_ru_c_diff)<=0.1,1,0),
         w4_ru_a10 = ifelse(abs(w4_ru_a_diff)<=0.1,1,0),
         w4_ru_e10 = ifelse(abs(w4_ru_e_diff)<=0.1,1,0))%>%
  filter(w2_urb_all10==1&w2_ru_c10==1&w2_ru_a10==1&w2_ru_e10==1&w3_ru_c10==1&w3_ru_a10==1&w3_ru_e10==1&w4_ru_c10==1&w4_ru_a10==1&
          w4_ru_e10==1)%>%
  arrange(seroprev_sumsq_age)%>%
  filter(end_calib_all>0.65&w42_peak_inf<=50000)



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
 
## Set up sweep inputs
## Select inputs from calibration
sweep <- filter[1:400,]%>%select(bl, relbeta_c, relbeta_a, rel_delta, imm_esc_factor_t1, rel_omi, imm_esc_factor_omi, 
                              kappa1, r0, r0_hyp, sweep)

sweep<- sweep %>% mutate(
        omega_pc = 1/150,
        omega2_pc = 1/180,
        kappa2 = 1/1600,
        kappa3 = 1/2500,
        red_inf_1 = 0.35,
        rel_red_inf_2 = 0.43,
        inc_trans =1,
        sero_thresh=0
)

## Get the starting values
last_list<-list()
for(i in 1:nrow(sweep)){
  last_list[[i]] <- sweep_list[[filter$sweep[i]]]$last_t[1:858]

}

##SAve to forward simulation work space
saveRDS(sweep, "../0_forward/0_sweep_test.RDS")
saveRDS(last_list,"../0_forward/0_last_test.RDS")

##
saveRDS(sweep, "../0_forward/0_sweep_range.RDS")
saveRDS(last_list,"../0_forward/0_last_range.RDS")

##Get calibrated ranges
quantile(sweep$bl,probs=c(0, 0.25, 0.5, 0.75,1))
quantile(sweep$relbeta_c,probs=c(0, 0.25, 0.5, 0.75,1))
quantile(sweep$relbeta_a,probs=c(0, 0.25, 0.5, 0.75,1))
quantile(sweep$r0, probs=c(0, 0.25, 0.5, 0.75,1))
quantile(sweep$kappa1, probs=c(0, 0.25, 0.5, 0.75,1))
quantile(sweep$rel_delta, probs=c(0, 0.25, 0.5, 0.75,1))
quantile(sweep$imm_esc_factor_t1, probs=c(0, 0.25, 0.5, 0.75,1))
quantile(sweep$rel_omi, probs=c(0, 0.25, 0.5, 0.75,1))
quantile(sweep$imm_esc_factor_omi, probs=c(0, 0.25, 0.5, 0.75,1))

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


sweep <- filter[1:400,]%>%select(bl, relbeta_c, relbeta_a, rel_delta, imm_esc_factor_t1, rel_omi, imm_esc_factor_omi, 
                                 kappa1, r0, r0_hyp, sweep)

## Read in historical seroprevalence
sero_hist <- read.csv("1_seroprev_sum.csv")

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
  geom_line(data=exp_inf, aes(x=date, y=cases_per100), col="#3182BD", linetype="dashed",size=0.7)+theme_bw()+
  ggtitle("Modeled historical epidemic")+
  xlab("Date") + ylab("Cases per 100")+
  theme(plot.title = element_text(size=14),
        axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        strip.text = element_text(size=12),
        legend.position = "none")

png("calib_cases.png", width = 10,height = 5,units="in",res=400)
p1
dev.off()

###Seroprevalence visualization
##Plot age-specific seroprevalence
sero <- list()
for(i in 1:length(list)){
  sero[[i]]<-sweep_filter[[i]]$seroprev_targ
  sero[[i]]$sweep<-i
}

sero <- do.call(rbind, sero)

sero_tab <- sero %>%group_by(period, urb_rural, age_group)%>%
  summarise(med = median(prev_mod, na.rm=T),
            prob_25 = quantile(prev_mod, probs=0, na.rm=T),
            prob_75 = quantile(prev_mod,probs=1, na.rm=T))

period = c(rep("post_wave1",4),"wave2",rep("post_wave2",5),rep("post_wave3",4),rep("post_wave4",4))
sero_dates <- data.frame(period= c(unique(period), "post_wave4_2","end_calib"), 
                         date = c(as.Date("2020-11-21"),
                                  as.Date("2021-03-01"),
                                  as.Date("2021-06-18"),
                                  as.Date("2021-10-31"),
                                  as.Date("2022-1-31"),
                                  as.Date("2022-5-28"),
                                  as.Date("2022-9-1")))
sero_est<- sero_hist%>%select(period, urb_rural, age_group, prev)%>%
            left_join(sero_dates)%>%
            mutate(date = ifelse(is.na(date),NA,as.Date(date) -30),
                   date=as.Date(date,origin="1970-01-01"))

sero_tab <- sero_tab%>%
            left_join(sero_est, by=c("period"="period","urb_rural"="urb_rural","age_group"="age_group"))
write.csv(sero_tab, "9_sero_tab.csv")

## Plot of seroprevalence
sero_tab%>%mutate(data="model")%>%left_join(sero_est, by=c("period"="period","urb_rural"="urb_rural","age_group"="age_group"))%>%
  left_join(sero_dates)%>%
  filter(date < as.Date("2022-05-01"))%>%
  filter(urb_rural !="all")%>%
  filter((urb_rural=="urban"&age_group=="all")|(urb_rural=="rural"&age_group!="all"))%>%
  ggplot(aes(x=date, y=med))+
  geom_point(aes(x=date,y=med, col=age_group), size=2)+
  geom_errorbar(aes(ymin=prob_25, ymax=prob_75,col=age_group),width=20)+
  geom_point(data=sero_est, aes(x=date, y=prev, col=age_group))+
  
  facet_wrap(~urb_rural)


p2 <- sero%>% 
  ggplot(aes(x=time, y=val))+
  geom_line(aes(x=time, y=val), col="gray78")+
  geom_line(data=med, aes(x=time, y = med, col = age_grp),size=1.2) +ylim(0,1)+
  scale_color_manual(values = c("#C6DBEF","#4292C6","#08306B"))+
  theme_bw()+
  theme(plot.title = element_text(size=9),
        axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        strip.text = element_text(size=12),
        legend.position = "none")+
  xlab("Time") + ylab("Seroprevalence")+
  facet_wrap(~age_grp, ncol=3)


##Code to visualize what the waves look like
list <-unlist(filter$sweep)

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
