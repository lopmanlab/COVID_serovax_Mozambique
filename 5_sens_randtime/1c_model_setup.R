library(dplyr)
library(deSolve)
library(ggplot2)
source("1_model_code_int_v8.R")
sweep<- readRDS("0_sweep_int.RDS")
spec_humid <- read.csv("9_spec_humid_nyc.csv")[,c("day","avg_sm3")]
start <- readRDS("9_last_Rrand.RDS")
#start <- readRDS("0_last_reset_0s.R")  ##Reset to initial initials to better assess impact of swapping out the matrices..
#mixing_matrix <- readRDS("0_mixing_matrix_gmix.R")
trigger<-F
sero_e1 <-rep(0,5)
yroot1 <- rep(0,5)
yroot2 <- rep(0,5)



########################## 
##Model initiation stuff##
##########################
model_sims <- function(i){
  
  print(paste(Sys.time(),"iteration start", i, sep=" "))
  ##Future pandemic
  r00<- 4.5
  r01<- 4.5
  r02<- 5
  r03<- 5
  r04<- 5
  r05<- 5
  r06<- 5
  r07<- 5
  r08<- 5
  r09<- 5
  
  st0<-sweep$st0[i]
  st1<-sweep$st1[i]
  st2<-sweep$st2[i]
  st3<-sweep$st3[i]
  st4<-sweep$st4[i]
  st5<-sweep$st5[i]
  st6<-sweep$st6[i]
  st7<-sweep$st7[i]
  st8<-sweep$st8[i]
  st9<-sweep$st9[i]
  
  fac1 <- sweep$fac1[i]
  fac2 <- sweep$fac2[i]
  
  total_time=3650         
  t = seq(0,total_time,1)
  
  tt=3650
  tt2= seq(0,tt,1)
  signal <- data.frame(t = tt2, 
                       yr = floor(tt2/365),
                       day = c(rep(c(seq(from=0, to=364, by=1)),times=10),364),
                       fac = rep(0, length(tt2)),
                       #r0_hyp = rep(0,length(times)),
                       r0t = rep(0, length(tt2)))
  
  signal<- merge(signal, spec_humid, by.x="day", by.y="day")
  
  r0hyp_list <- data.frame(yr = seq(from=0, to=10, by=1),
                           r0_hyp = c(r00,r01,r02,r03,r04,r05,r06,r07,r08,r09,r09))
  
  
  signal<- merge(signal,r0hyp_list, by.x= "yr", by.y = "yr")
  signal <- signal[order(signal$t),]
  
  r0min<-sweep$r0_base[i]
  
  rand_time_seq <- c(0,st0:365,1:(st0-1),
                     st1:365,1:(st1-1),
                     st2:365,1:(st2-1),
                     st3:365,1:(st3-1),
                     st4:365,1:(st4-1),
                     st5:365,1:(st5-1),
                     st6:365,1:(st6-1),
                     st7:365,1:(st7-1),
                     st8:365,1:(st8-1),
                     st9:365,1:(st9-1))
  
  signal  <- cbind(signal, rand_time_seq)%>%
    mutate(trand= rand_time_seq+yr*365)%>%arrange(trand)%>%
    mutate(t=tt2)

  signal$r0t= exp(fac1*signal$avg_sm3 + log(signal$r0_hyp*fac2-r0min*0.85))+r0min
  signal$import <- signal$r0t/sweep$r0[i]
  
  signal <-signal[,c("t","import")]
  
  
  input <<- approxfun(signal, rule = 2)
  trigger <<-F

  ##Matrix
  #CM <<- mixing_matrix[[1]]
  
  ####Scenario sweeps####
  sweep <- sweep
  num_sweep <- nrow(sweep)
  trigger<-F
  ##########################
  ##Model initiation stuff##
  ##########################
  ##Initial state values##
  
  #start = unlist(start_all[i,])
  start = unlist(start)

  ##Transmission parameters
  ## Beta is the probability of transmission when contacted with susceptible of clases c, a, e
  bl <- sweep$bl[i]
  rel_c <- sweep$relbeta_c[i]
  rel_a <- sweep$relbeta_a[i]
  
  beta_c <- bl*rel_c
  beta_a <- bl*rel_a    ##Guesses
  beta_e <- bl*1    ##Guesses
  r0 <- sweep$r0[i]
  r0_hyp <- sweep$r0_hyp[i]
  inc_trans<-sweep$inc_trans[i]
  
  ##alpha is relative infectiousness of asymptomatic
  alpha <- 0.6
  
  
  # Relative infectiousness of omicron
  rel_delta <- sweep$rel_delta[i]    ## Delta increase
  rel_omi <-sweep$rel_omi[i] ##Omicron increase
  #sd1<-sweep$sd1[i]    ##Omicron decrease step 1
  #sd2 <- sweep$sd2[i]  ## Omicron decrease step 2
  #rel_newvar <- sweep$rel_newvar[i]
  
  #mu_c <-0
  #mu_a <-0
  #mu_e <-0
  ## Vaccine parameters
  vei1<-0.50 ## VE against infection
  vep1<-0.40 ## VE against hospitalization
  vei2<-0.60 ## VE against infection 
  vep2<-0.67 ## VE against hospitalization
  vei3<-0.7 ## VE against infection
  vep3<-0.9 ## VE against hospitalization
  
  
  ##Natural history parameters
  sigma <- 1/5.5  #latent period
  
  gamma_I <-1/7      #duration of infection for symptomatically infectious
  gamma_A <-1/7      #duration of infection for asymptomatically infectious
  gamma_H <-1/5      #hospital length of stay
  
  nu_c<- 0.45         #Probability of symptomatic infection for children
  nu_a<- 0.55         #Probability of symptomatic infection for adults
  nu_e<- 0.65         #Probability of symptomatic infection for elderly
  
  phi_c<- 0.004     #Prob of hospitalization (children) among unvaxed
  phi_a<- 0.03       #Prob of hospitalization (adults) among unvaxed
  phi_e<- 0.2       #Prob of hospitalization (elderly) among unvaxed
  
  phi_cv1 <- phi_c*(1-vep1)
  phi_av1 <- phi_a*(1-vep1)
  phi_ev1 <- phi_e*(1-vep1)
  
  phi_cv2 <- phi_c*(1-vep2)
  phi_av2 <- phi_a*(1-vep2)
  phi_ev2 <- phi_e*(1-vep2)
  
  phi_cv3 <- phi_c*(1-vep3)
  phi_av3 <- phi_a*(1-vep3)
  phi_ev3 <- phi_e*(1-vep3)
  
  
  
  
  mu_c<-0.005            #Prob of death (children)
  mu_a<-0.0365       #Prob of death (adults)
  mu_e<-0.15        #Prob of death (elderly)
  
  ##Protection post-infection
  #red_inf_1 <- 0.35  ## FOI among susceptible individuals after period of immunity is 35%%
  #red_inf_2 <- 0.15
  #red_inf_1<-0.5
  #red_inf_2<-0.4
  red_inf_1 <- sweep$red_inf_1[i]
  red_inf_2 <- red_inf_1*sweep$rel_red_inf_2[i]
  #red_inf_1 <- 0.45
  #red_inf_2 <- red_inf_1*0.7
 
  
  ## Waning immunity after infection
  ##Among seropositives
  omega_pc <- sweep$omega_pc[i]
  omega_pa <- omega_pc
  omega_pe <- omega_pc
  
  ##Among seronegatives
  omega_nc <- omega_pc
  omega_na <- omega_nc
  omega_ne <- omega_nc
  
  ##Waning immunity after vaccination
  omegav_pc <- omega_pc
  omegav_pa <- omegav_pc
  omegav_pe <- omegav_pc
  
  omegav_nc <- omega_pc
  omegav_na <- omegav_nc
  omegav_ne <- omegav_nc
  
  ## After third infection (two prior exposure class), wane from R3 to S2
  omega2_pc <-sweep$omega2_pc[i]
  omega2_pa <-omega2_pc
  omega2_pe <-omega2_pc
  
  omega2_nc <-omega2_pc
  omega2_na <-omega2_nc
  omega2_ne <-omega2_nc
  
  ##After third vaccination, wane from V3 to V2
  omega3_pc <-sweep$omega2_pc[i]   ##Keep cyclical waning from V3 to V2 the same as cyclical waning from immune third infection to susc
  omega3_pa <-omega3_pc
  omega3_pe <-omega3_pc
  
  omega3_nc <-omega3_pc    ##Keep waning of seronegative the same as seropositive
  omega3_na <-omega3_nc
  omega3_ne <-omega3_nc
  
  ## Additional waning from S3 -> S2
  omega4_pc <- 1/365
  omega4_pa <- omega4_pc
  omega4_pe <- omega4_pc
  
  omega4_nc <- omega4_pc
  omega4_na <- omega4_pc
  omega4_ne <- omega4_pc
  
  
  ## immune escape factor
  imm_esc_factor_t1 <- sweep$imm_esc_factor_t1[i]
  imm_esc_factor_omi<- sweep$imm_esc_factor_omi[i]
  add_imm <- sweep$add_imm[i]
  #imm_esc_factor_newvar <- sweep$imm_esc_factor_newvar[i]
  
  ##Serology
  # Probability of seroconversion after infection
  pi <- 0.9
  
  # Prob of seroconversion after vaccination
  rho_v1 <- 0.85
  rho_v2 <- 0.6 ## Among those seronegative after first dose
  rho_v3 <-0.9  ##Among those seronegative after second dose, or waned
  ##Seroreversion
  #kappa_c <- 1/250
  #kappa_a <- 1/250
  #kappa_e <- 1/250
  
  kappa1 <- sweep$kappa1[i]
  kappa2 <- sweep$kappa2[i]
  kappa3 <- sweep$kappa3[i]
  
  sero_thresh <- sweep$sero_thresh[i]
  
  
  ## Vax interval
  vax_int <- sweep$vax_int[i]
  vax_start <- sweep$vax_start[i]
  vax_end <- sweep$vax_end[i]

  
  beta1<-sweep$beta1[i]
  w <- sweep$w[i]
  #kappa_vax <-1/250
  params<-c('beta_c'= beta_c, 'beta_a' = beta_a, 'beta_e' = beta_e,
            'alpha' = alpha, 'rel_delta'=rel_delta,'rel_omi' = rel_omi, 
            'vei1'=vei1, 'vei2'=vei2, 'vei3'=vei3, 'vep1'=vep1, 'vep2'=vep2, 'vep3' =vep3,
            'sigma' = sigma,
            'gamma_I'=gamma_I, 'gamma_A'=gamma_A, 'gamma_H'=gamma_H,
            'nu_c'=nu_c, 'nu_a'=nu_a, 'nu_e'=nu_e,
            'phi_c'=phi_c, 'phi_a'=phi_a, 'phi_e'=phi_e,
            'phi_cv1'=phi_cv1, 'phi_av1'=phi_av1, 'phi_ev1'=phi_ev1,
            'phi_cv2'=phi_cv2, 'phi_av2'=phi_av2, 'phi_ev2'=phi_ev2,
            'phi_cv3'=phi_cv3, 'phi_av3'=phi_av3, 'phi_ev3'=phi_ev3,
            
            'mu_c'=mu_c, 'mu_a'=mu_a, 'mu_e'=mu_e,'red_inf_1' = red_inf_1, 'red_inf_2'=red_inf_2,
            'omega_pc'=omega_pc, 'omega_pa'=omega_pa, 'omega_pe'=omega_pe,
            'omega_nc'=omega_nc, 'omega_na'=omega_na, 'omega_ne'=omega_ne,
            
            'omega2_pc'=omega2_pc, 'omega2_pa'=omega2_pa, 'omega2_pe'=omega2_pe,
            'omega2_nc'=omega2_nc, 'omega2_na'=omega2_na, 'omega2_ne'=omega2_ne,
            
            'omegav_pc'=omegav_pc, 'omegav_pa'=omegav_pa, 'omegav_pe' =omegav_pe,
            'omegav_nc'=omegav_nc, 'omegav_na'=omegav_na, 'omegav_ne' =omegav_ne,
            
            'omega3_pc'=omega3_pc, 'omega3_pa'=omega3_pa, 'omega3_pe'=omega3_pe,
            'omega3_nc'=omega3_nc, 'omega3_na'=omega3_na, 'omega3_ne'=omega3_ne,
            
            
            'omega4_pc'=omega4_pc, 'omega4_pa'=omega4_pa, 'omega4_pe'=omega4_pe,
            'omega4_nc'=omega4_nc, 'omega4_na'=omega4_na, 'omega4_ne'=omega4_ne,
            
            'imm_esc_factor_t1'=imm_esc_factor_t1, 'imm_esc_factor_omi'=imm_esc_factor_omi, 'add_imm'=add_imm,
            'pi'=pi, 'rho_v1'=rho_v1, 'rho_v2' = rho_v2, 'rho_v3' =rho_v3,
            'kappa1'=kappa1, 'kappa2'=kappa2, 'kappa3'=kappa3,
            'r0'=r0, 'r0_hyp'=r0_hyp, 'inc_trans'=inc_trans,
            
            'r00'=r00, 'r01'=r01, 'r02'=r02, 'r03'=r03, 'r04'=r04, 'r05'=r05, 'r06'=r06, 'r07'=r07,'r08'=r08, 'r09'=r09,
            
             'beta1'=beta1, 'w'=w,
            'sero_thresh' =sero_thresh,
            
            'vax_int'=vax_int, 'vax_start'=vax_start, 'vax_end'=vax_end
            #'sd1'=sd1,'sd2'=sd2,'rel_newvar'=rel_newvar,
  )
  
  
  model_out <- as.data.frame(ode(y = start, times = t, fun = COVID_sero_vax, parms = params))
  mod_foi <- model_out %>% select(time| contains("foi"))
  
  
  mod_inc <- model_out %>% select(time| contains("Ecum")|contains("Icum")) %>% mutate(
    Ecum_cr = rowSums(select(.,contains("Ecum")&(contains("_cr")))),
    Ecum_cu = rowSums(select(.,contains("Ecum")&(contains("_cu")))),
    Ecum_ar = rowSums(select(.,contains("Ecum")&(contains("_ar")))),
    Ecum_au = rowSums(select(.,contains("Ecum")&(contains("_au")))),
    Ecum_er = rowSums(select(.,contains("Ecum")&(contains("_er")))),
    Ecum_eu = rowSums(select(.,contains("Ecum")&(contains("_eu")))),
    
    Ecum_c = Ecum_cr+Ecum_cu,
    Ecum_a = Ecum_ar+Ecum_au,
    Ecum_e = Ecum_er+Ecum_eu,
    
    Icum_c = rowSums(select(.,contains("Icum")&(contains("_cu")|contains("_cr")))),
    Icum_a = rowSums(select(.,contains("Icum")&(contains("_au")|contains("_ar")))),
    Icum_e = rowSums(select(.,contains("Icum")&(contains("_eu")|contains("_er")))),
    
    new_E_c = Ecum_c - lag(Ecum_c),          
    new_E_a = Ecum_a - lag(Ecum_a),
    new_E_e = Ecum_e - lag(Ecum_e),
    
    new_I_c = Icum_c - lag(Icum_c),
    new_I_a = Icum_a - lag(Icum_a),
    new_I_e = Icum_e - lag(Icum_e),  
    
    Ecum = Ecum_c+Ecum_a+Ecum_e,
    Icum = Icum_c+Icum_a+Icum_e,
    new_E = new_E_c+new_E_a+new_E_e,
    new_I = new_I_c+new_I_a+new_I_e
  ) 
    
    #mutate(date =  seq(from = as.Date("2020-05-06"), to=as.Date(dt_end), by =1)) %>%
    #select(time, Ecum_cr:date)
  
  model_out <- model_out %>% select(-contains("foi")) %>%
    select(-(contains("Ecum")|contains("Icum")))
  
  ## Population totals and deaths
  pop_num <- model_out %>% mutate(
    
    Nchildr = rowSums(select(.,contains('cr')&(-starts_with("D")))),    ## Total in the population (exclude deaths)
    Nchildu = rowSums(select(.,contains('cu')&(-starts_with("D")))),
    Nadultr = rowSums(select(.,contains('ar')&(-starts_with("D")))),
    Nadultu = rowSums(select(.,contains('au')&(-starts_with("D")))),
    Noldr =   rowSums(select(.,contains('er')&(-starts_with("D")))),
    Noldu =   rowSums(select(.,contains('eu')&(-starts_with("D")))),
    NTot = Nchildr + Nchildu +Nadultr +Nadultu +Noldr+Noldu,           ## Total alive in compartments
    
    
    Deaths_c = rowSums(select(.,contains('Dc'))),
    Deaths_a = rowSums(select(.,contains('Da'))),
    Deaths_e = rowSums(select(.,contains('De'))),
    
    Deaths_tot = Deaths_c+Deaths_a+Deaths_e,
    
    new_Deaths_c = Deaths_c -lag(Deaths_c),
    new_Deaths_a = Deaths_a - lag(Deaths_a),
    new_Deaths_e = Deaths_e - lag(Deaths_e),
    
    new_Deaths_tot = Deaths_tot - lag(Deaths_tot)) %>%
    
    select(time, Nchildr:new_Deaths_tot)
  
  
  seroprev<- model_out %>% left_join(pop_num%>%select(time,Nchildr:NTot), by= "time")%>%
    
    mutate(
    
    SpRp_cr = rowSums(select(., contains('cr')& (contains('Sp')|contains('Rp')|contains('Vp')))),
    SpRp_cu = rowSums(select(., contains('cu')& (contains('Sp')|contains('Rp')|contains('Vp')))),
    SpRp_ar = rowSums(select(., contains('ar')& (contains('Sp')|contains('Rp')|contains('Vp')))),
    SpRp_au = rowSums(select(., contains('au')& (contains('Sp')|contains('Rp')|contains('Vp')))),
    SpRp_er = rowSums(select(., contains('er')& (contains('Sp')|contains('Rp')|contains('Vp')))),
    SpRp_eu = rowSums(select(., contains('eu')& (contains('Sp')|contains('Rp')|contains('Vp')))),
    
    
    active_cr = rowSums(select(.,(contains('Ecr')|contains('Acr')|contains('Icr')|contains('Hcr')), -ends_with('1v0'))),
    active_cu = rowSums(select(.,(contains('Ecu')|contains('Acu')|contains('Icu')|contains('Hcu')), -ends_with('1v0'))),
    active_ar = rowSums(select(.,(contains('Ear')|contains('Aar')|contains('Iar')|contains('Har')), -ends_with('1v0'))),
    active_au = rowSums(select(.,(contains('Eau')|contains('Aau')|contains('Iau')|contains('Hau')), -ends_with('1v0'))),
    active_er = rowSums(select(.,(contains('Eer')|contains('Aer')|contains('Ier')|contains('Her')), -ends_with('1v0'))),
    active_eu = rowSums(select(.,(contains('Eeu')|contains('Aeu')|contains('Ieu')|contains('Heu')), -ends_with('1v0'))),
    
    seropos_cr = SpRp_cr + active_cr,
    seropos_cu = SpRp_cu + active_cu,
    seropos_ar = SpRp_ar + active_ar,
    seropos_au = SpRp_au + active_au,
    seropos_er = SpRp_er + active_er,
    seropos_eu = SpRp_eu + active_eu,
    
    seroprev_cr = seropos_cr/Nchildr,
    seroprev_cu = seropos_cu/Nchildu,
    seroprev_ar = seropos_ar/Nadultr,
    seroprev_au = seropos_au/Nadultu,
    seroprev_er = seropos_er/Noldr,
    seroprev_eu = seropos_eu/Noldu,
    
    seroprev_c = (seropos_cr+seropos_cu)/(Nchildr+Nchildu),
    seroprev_a = (seropos_ar+seropos_au)/(Nadultr+Nadultu),
    seroprev_e = (seropos_er+seropos_eu)/(Noldr+Noldu),
    
    seroprev_ul = (seropos_cu+seropos_au+seropos_eu)/(Nchildu+Nadultu+Noldu),
    seroprev_rl = (seropos_cr+seropos_ar+seropos_er)/(Nchildr+Nadultr+Noldr),
    
    seroprev_to = (seropos_cr+seropos_cu+seropos_ar+seropos_au+seropos_er+seropos_eu)/(NTot))%>%
    select(time,seroprev_cr:seroprev_to)

  
  vax_dist <- model_out%>% mutate(
    V0cum_c = rowSums(select(.,contains("v0")&(contains("cr")|contains("cu"))&(-starts_with("D")))),
    V1cum_c = rowSums(select(.,contains("v1")&(contains("cr")|contains("cu"))&(-starts_with("D")))),
    V2cum_c = rowSums(select(.,contains("v2")&(contains("cr")|contains("cu"))&(-starts_with("D")))),
    V3cum_c = rowSums(select(.,contains("v3")&(contains("cr")|contains("cu"))&(-starts_with("D")))),
    
    V0cum_a = rowSums(select(.,contains("v0")&(contains("ar")|contains("au"))&(-starts_with("D")))),
    V1cum_a = rowSums(select(.,contains("v1")&(contains("ar")|contains("au"))&(-starts_with("D")))),
    V2cum_a = rowSums(select(.,contains("v2")&(contains("ar")|contains("au"))&(-starts_with("D")))),
    V3cum_a = rowSums(select(.,contains("v3")&(contains("ar")|contains("au"))&(-starts_with("D")))),
    
    V0cum_e = rowSums(select(.,contains("v0")&(contains("er")|contains("eu"))&(-starts_with("D")))),
    V1cum_e = rowSums(select(.,contains("v1")&(contains("er")|contains("eu"))&(-starts_with("D")))),
    V2cum_e = rowSums(select(.,contains("v2")&(contains("er")|contains("eu"))&(-starts_with("D")))),
    V3cum_e = rowSums(select(.,contains("v3")&(contains("er")|contains("eu"))&(-starts_with("D")))))%>%
    
    select(time, V0cum_c:V3cum_e)
  
  exp_dist <- model_out %>%mutate(
        e0cum_c = rowSums(select(.,contains("0v")&(contains("cr")|contains("cu"))&(-starts_with("D")))),
        e1cum_c = rowSums(select(.,contains("1v")&(contains("cr")|contains("cu"))&(-starts_with("D")))),
        e2cum_c = rowSums(select(.,contains("2v")&(contains("cr")|contains("cu"))&(-starts_with("D")))),
        
        e0cum_a = rowSums(select(.,contains("0v")&(contains("ar")|contains("au"))&(-starts_with("D")))),
        e1cum_a = rowSums(select(.,contains("1v")&(contains("ar")|contains("au"))&(-starts_with("D")))),
        e2cum_a = rowSums(select(.,contains("2v")&(contains("ar")|contains("au"))&(-starts_with("D")))),
        
        e0cum_e = rowSums(select(.,contains("0v")&(contains("er")|contains("eu"))&(-starts_with("D")))),
        e1cum_e = rowSums(select(.,contains("1v")&(contains("er")|contains("eu"))&(-starts_with("D")))),
        e2cum_e = rowSums(select(.,contains("2v")&(contains("er")|contains("eu"))&(-starts_with("D"))))) %>%
      select(time,  e0cum_c:e2cum_e)
  
  imm_class <- model_out %>%
    mutate(
      s0v0_c = rowSums(select(., contains('Scr')|contains('Scu'))),
      s0v0_a = rowSums(select(., contains('Sar')|contains('Sau'))),
      s0v0_e = rowSums(select(., contains('Ser')|contains('Seu'))),
      
      s1v0_c = rowSums(select(.,Spcr1v0,Sncr1v0, Spcu1v0, Sncu1v0)),
      s1v0_a = rowSums(select(.,Spar1v0,Snar1v0, Spau1v0, Snau1v0)),
      s1v0_e = rowSums(select(.,Sper1v0,Sner1v0, Speu1v0, Sneu1v0)),
      
      s2v0_c = rowSums(select(.,Spcr2v0,Sncr2v0, Spcu2v0, Sncu2v0)),
      s2v0_a = rowSums(select(.,Spar2v0,Snar2v0, Spau2v0, Snau2v0)),
      s2v0_e = rowSums(select(.,Sper2v0,Sner2v0, Speu2v0, Sneu2v0)),
      
      s0v1_c = rowSums(select(.,Spcr0v1,Sncr0v1, Spcu0v1, Sncu0v1)),
      s0v1_a = rowSums(select(.,Spar0v1,Snar0v1, Spau0v1, Snau0v1)),
      s0v1_e = rowSums(select(.,Sper0v1,Sner0v1, Speu0v1, Sneu0v1)),
      
      s1v1_c = rowSums(select(.,Spcr1v1,Sncr1v1, Spcu1v1, Sncu1v1)),
      s1v1_a = rowSums(select(.,Spar1v1,Snar1v1, Spau1v1, Snau1v1)),
      s1v1_e = rowSums(select(.,Sper1v1,Sner1v1, Speu1v1, Sneu1v1)),
      
      s2v1_c = rowSums(select(.,Spcr2v1,Sncr2v1, Spcu2v1, Sncu2v1)),
      s2v1_a = rowSums(select(.,Spar2v1,Snar2v1, Spau2v1, Snau2v1)),
      s2v1_e = rowSums(select(.,Sper2v1,Sner2v1, Speu2v1, Sneu2v1)),
      
      s0v2_c = rowSums(select(.,Spcr0v2,Sncr0v2, Spcu0v2, Sncu0v2)),
      s0v2_a = rowSums(select(.,Spar0v2,Snar0v2, Spau0v2, Snau0v2)),
      s0v2_e = rowSums(select(.,Sper0v2,Sner0v2, Speu0v2, Sneu0v2)),
      
      s1v2_c = rowSums(select(.,Spcr1v2,Sncr1v2, Spcu1v2, Sncu1v2)),
      s1v2_a = rowSums(select(.,Spar1v2,Snar1v2, Spau1v2, Snau1v2)),
      s1v2_e = rowSums(select(.,Sper1v2,Sner1v2, Speu1v2, Sneu1v2)),
      
      s2v2_c = rowSums(select(.,Spcr2v2,Sncr2v2, Spcu2v2, Sncu2v2)),
      s2v2_a = rowSums(select(.,Spar2v2,Snar2v2, Spau2v2, Snau2v2)),
      s2v2_e = rowSums(select(.,Sper2v2,Sner2v2, Speu2v2, Sneu2v2)),
      
      s0v3_c = rowSums(select(.,Spcr0v3,Sncr0v3, Spcu0v3, Sncu0v3)),
      s0v3_a = rowSums(select(.,Spar0v3,Snar0v3, Spau0v3, Snau0v3)),
      s0v3_e = rowSums(select(.,Sper0v3,Sner0v3, Speu0v3, Sneu0v3)),
      
      s1v3_c = rowSums(select(.,Spcr1v3,Sncr1v3, Spcu1v3, Sncu1v3)),
      s1v3_a = rowSums(select(.,Spar1v3,Snar1v3, Spau1v3, Snau1v3)),
      s1v3_e = rowSums(select(.,Sper1v3,Sner1v3, Speu1v3, Sneu1v3)),
      
      s2v3_c = rowSums(select(.,Spcr2v3,Sncr2v3, Spcu2v3, Sncu2v3)),
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
    
    select(time, s0v0_c:vac_e)
  
    vax_elig <- model_out %>%
    ##Calculate number of individuals eligible for vaccination
    mutate(vax_elig_er = rowSums(select(.,contains("er")&(-starts_with("D")&-starts_with("I")&-starts_with("H")))),
           vax_elig_eu = rowSums(select(.,contains("eu")&(-starts_with("D")&-starts_with("I")&-starts_with("H")))),
           vax_elig_ar = rowSums(select(.,contains("ar")&(-starts_with("D")&-starts_with("I")&-starts_with("H")))),
           vax_elig_au = rowSums(select(.,contains("au")&(-starts_with("D")&-starts_with("I")&-starts_with("H")))),
           
           vax_elig_e = vax_elig_er + vax_elig_eu,
           vax_elig_a = vax_elig_ar + vax_elig_au)%>%
    select(time, vax_elig_er:vax_elig_a) %>%
    
    left_join(model_out %>% select(time, delta1_cr:delta3_eu))
    
    sd<-model_out$sd
  
  model_out$sweepnum <- i
  # model_summary$sweepnum <- i
  print(paste(i, "iteration complete"))
  
  res_out <- list(pop_num = pop_num, start = start, params = params,
              #model_out=model_out,
              mod_inc=mod_inc, 
              mod_foi=mod_foi, imm_class=imm_class,
              seroprev=seroprev, vax_elig=vax_elig,
              sd=sd)
  #saveRDS(res_out,paste("sw_run_",i,".RDS",sep=""))
  
  num <- if(nchar(i)==1){paste("000",i,sep="")} else if(nchar(i)==2){paste("00",i,sep="")}else if(nchar(i)==3){paste("0",i,sep="")}else{paste(i)}
  saveRDS(res_out,paste("/projects/blopman/vger/cliu/0_sens_int/sw_run_",num,".RDS",sep=""))
  return(res_out)

}