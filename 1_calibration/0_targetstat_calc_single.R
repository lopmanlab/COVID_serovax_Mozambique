##Cumulative cases after each wave target
sweep<- readRDS("0_calibration_sweep12.RDS")


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
  
    seroprev_mod <- data.frame(prev_mod = c(s1_cu, s1_au, s1_eu, s1_ul,            #put point estimates into dataframe
                                            s2_ul, s3_cr, s3_ar, s3_er, s3_rl, 
                                            s4_ul),
                               
                               period = c(rep("post_wave1",4),"wave2",rep("post_wave2",5)),   ##identify the point estimates by wave/period, urban/rural and age group
                               urb_rural=c(rep("urban",5),rep("rural",4),"urban"),
                               age_group=c("c","a","e","all","all","c","a","e","all","all"))
    
    seroprev_comp<- seroprev_mod %>%left_join(
      sero_hist%>%select(period, urb_rural, age_group, prev), 
      by =c("period"="period","urb_rural"="urb_rural","age_group"="age_group"))
   
  seroprev_comp<- seroprev_comp%>%
    mutate(square_diff = (prev_mod-prev)^2,
           prop_diff = prev_mod-prev)
  print(seroprev_comp)
}


for (i in 1:length(mod_scenarios)){
  mod_scenarios[[i]]$seroprev_targ <- seroprev_targ(mod_scenarios[[i]]$seroprev,waves="four")
  mod_scenarios[[i]]$cum_case_targ <- cum_case_targ(mod_scenarios[[i]]$mod_inc)
}

##compiling all the target stats for each run
##input number of total simulations
n<-4
global_targ1 <- data.frame(sweep = seq(1:n),
                          seroprev_sumsq_age =rep(0,n), 
                          w2_urb_all_diff = rep(0,n),
                          w3_ru_c_diff = rep(0,n),
                          w3_ru_a_diff = rep(0,n),
                          w3_ru_e_diff = rep(0,n),
                          
                          w12_unrep=rep(0,n),
                          w3_unrep=rep(0,n),
                          w4_unrep=rep(0,n),
                          w2_peak_unrep=rep(0,n),
                          w3_peak_unrep=rep(0,n))

## Compile into data frame of a bunch of targets
for(i in 1:length(mod_scenarios)){
  age_specific <- which(mod_scenarios[[1]]$seroprev_targ$age_group!="all")
  ## Overall sum square diff 
  global_targ1$seroprev_sumsq_age[i] <- sum(mod_scenarios[[i]]$seroprev_targ$square_diff[age_specific])
  
  ## Last urban seroprev estimate and three rural seroprev estimate can not be more than 10 point different
  global_targ1$w2_ru_c_diff[i] <- mod_scenarios[[i]]$seroprev_targ$prop_diff[6]
  global_targ1$w2_ru_a_diff[i] <- mod_scenarios[[i]]$seroprev_targ$prop_diff[7]
  global_targ1$w2_ru_e_diff[i] <- mod_scenarios[[i]]$seroprev_targ$prop_diff[8]
  global_targ1$w2_urb_all_diff[i] <- mod_scenarios[[i]]$seroprev_targ$prop_diff[10]
  global_targ1$w3_ru_c_diff[i] <- mod_scenarios[[i]]$seroprev_targ$prop_diff[11]
  global_targ1$w3_ru_a_diff[i] <- mod_scenarios[[i]]$seroprev_targ$prop_diff[12]
  global_targ1$w3_ru_e_diff[i] <- mod_scenarios[[i]]$seroprev_targ$prop_diff[13]
  global_targ1$w4_ru_c_diff[i] <- mod_scenarios[[i]]$seroprev_targ$prop_diff[15]
  global_targ1$w4_ru_a_diff[i] <- mod_scenarios[[i]]$seroprev_targ$prop_diff[16]
  global_targ1$w4_ru_e_diff[i] <- mod_scenarios[[i]]$seroprev_targ$prop_diff[17]
  
  global_targ1$w12_unrep[i]<- mod_scenarios[[i]]$cum_case_targ$underep_cum[1]
  global_targ1$w3_unrep[i]<- mod_scenarios[[i]]$cum_case_targ$underep_cum[2]
  
  ## W2 peak underrep
  global_targ1$w2_peak_unrep[i]<- mod_scenarios[[i]]$cum_case_targ$underep_peak[1]
  global_targ1$w3_peak_unrep[i]<- mod_scenarios[[i]]$cum_case_targ$underep_peak[2]
  ## W3 peak underrep
}


mod_scenarios[[1]]$mod_inc %>%
  mutate(date =  seq(from = as.Date("2020-05-06"), to=as.Date("2022-8-1"), by =1))%>%
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

##Prop 2021-7-1, no data from MoH, OWID reflects low vax
mod_scenarios[[1]]$mod_op$p_d1_ar[420]
mod_scenarios[[1]]$mod_op$p_d1_au[420]
mod_scenarios[[1]]$mod_op$p_d1_er[420]
mod_scenarios[[1]]$mod_op$p_d1_eu[420]

mod_scenarios[[2]]$mod_op$p_d2_ar[420]
mod_scenarios[[2]]$mod_op$p_d2_au[420]
mod_scenarios[[2]]$mod_op$p_d2_er[420]
mod_scenarios[[2]]$mod_op$p_d2_eu[420]

##Prop 2021-10-31
mod_scenarios[[1]]$mod_op$p_d1_ar[542]
mod_scenarios[[1]]$mod_op$p_d1_au[542]
mod_scenarios[[1]]$mod_op$p_d1_er[542]
mod_scenarios[[1]]$mod_op$p_d1_eu[542]

mod_scenarios[[2]]$mod_op$p_d2_ar[542]
mod_scenarios[[2]]$mod_op$p_d2_au[542]
mod_scenarios[[2]]$mod_op$p_d2_er[542]
mod_scenarios[[2]]$mod_op$p_d2_eu[542]

##Prop 2022 1-31
mod_scenarios[[1]]$mod_op$p_d1_ar[634]
mod_scenarios[[1]]$mod_op$p_d1_au[634]
mod_scenarios[[1]]$mod_op$p_d1_er[634]
mod_scenarios[[1]]$mod_op$p_d1_eu[634]

mod_scenarios[[2]]$mod_op$p_d2_ar[634]
mod_scenarios[[2]]$mod_op$p_d2_au[634]
mod_scenarios[[2]]$mod_op$p_d2_er[634]
mod_scenarios[[2]]$mod_op$p_d2_eu[634]


##Prop 2022 4-1
mod_scenarios[[1]]$mod_op$p_d1_ar[694]
mod_scenarios[[1]]$mod_op$p_d1_au[694]
mod_scenarios[[1]]$mod_op$p_d1_er[694]
mod_scenarios[[1]]$mod_op$p_d1_eu[694]

mod_scenarios[[2]]$mod_op$p_d2_ar[694]
mod_scenarios[[2]]$mod_op$p_d2_au[694]
mod_scenarios[[2]]$mod_op$p_d2_er[694]
mod_scenarios[[2]]$mod_op$p_d2_eu[694]

## Prop 2022-8-1
mod_scenarios[[1]]$mod_op$p_d1_ar[815]
mod_scenarios[[1]]$mod_op$p_d1_au[815]
mod_scenarios[[1]]$mod_op$p_d1_er[815]
mod_scenarios[[1]]$mod_op$p_d1_eu[815]

mod_scenarios[[2]]$mod_op$p_d2_ar[815]
mod_scenarios[[2]]$mod_op$p_d2_au[815]
mod_scenarios[[2]]$mod_op$p_d2_er[815]
mod_scenarios[[2]]$mod_op$p_d2_eu[815]
