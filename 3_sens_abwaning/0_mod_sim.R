library(dplyr)
library(deSolve)
library(ggplot2)
source("0_model_code_v8.R")
sweep<- readRDS("0_sweep_sero.RDS")
spec_humid <- read.csv("9_spec_humid_nyc.csv")[,c("day","avg_sm2")]
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
  inc_trans<-sweep$inc_trans[i]
  r00<- sweep$r00[i]*inc_trans^0
  r01<- sweep$r01[i]*inc_trans^0
  r02<- sweep$r02[i]*inc_trans^1
  r03<- sweep$r03[i]*inc_trans^2
  r04<- sweep$r04[i]*inc_trans^3
  r05<- sweep$r05[i]*inc_trans^4
  r06<- sweep$r06[i]*inc_trans^5
  r07<- sweep$r07[i]*inc_trans^6
  r08<- sweep$r08[i]*inc_trans^6
  r09<- sweep$r09[i]*inc_trans^6
  
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
  #signal$r0t= exp(-227.5*signal$avg_sm2 + log(signal$r0_hyp-r0min))+r0min
  #signal$r0t= exp(-227.5*signal$avg_sm2 + log((signal$r0_hyp)+4-r0min))+r0min
  signal$r0t= exp(fac1*signal$avg_sm2 + log(signal$r0_hyp*fac2-r0min*0.85))+r0min
  signal$import <- signal$r0t/sweep$r0[i]
  
  signal <-signal[,c("t","import")]
  
  #signal%>%
  #  ggplot(aes(x=t, y=r0t))+
  #  geom_line(aes(x=t,y=r0t))
  
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
  start<- c(unlist(start), vswitch=1, delta1_cr=0, delta1_cu=0, delta3_ar=0, delta3_au=0, delta3_er=0, delta3_eu=0)
  
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
            'sero_thresh' =sero_thresh
            #'sd1'=sd1,'sd2'=sd2,'rel_newvar'=rel_newvar,
  )
  
  
  
  rootFun <- function(t,start,params){
    with(as.list(c(start, params)),{
      #Seropositive sums
      SpRp_crv0 = sum(Rpcr1v0, W1Rpcr1v0,W2Rpcr1v0,W3Rpcr1v0,Spcr1v0,Rpcr2v0,Spcr2v0,Rpcr3v0)
      SpRp_crv1 = sum(Spcr0v1,Vpcr0v1,W1Vpcr0v1,W2Vpcr0v1,W3Vpcr0v1,Rpcr1v1,Spcr1v1,Rpcr2v1, Spcr2v1,Rpcr3v1)
      SpRp_crv2 = sum(Spcr0v2,Vpcr0v2,Rpcr1v2,Spcr1v2,Rpcr2v2,Spcr2v2,Rpcr3v2)
      SpRp_crv3 = sum(Spcr0v3,Vpcr0v3,Rpcr1v3,Spcr1v3,Rpcr2v3,Spcr2v3,Rpcr3v3)
      
      SpRp_cuv0 = sum(Rpcu1v0, W1Rpcu1v0,W2Rpcu1v0,W3Rpcu1v0,Spcu1v0,Rpcu2v0,Spcu2v0,Rpcu3v0)
      SpRp_cuv1 = sum(Spcu0v1,Vpcu0v1,W1Vpcu0v1,W2Vpcu0v1,W3Vpcu0v1,Rpcu1v1,Spcu1v1,Rpcu2v1, Spcu2v1,Rpcu3v1)
      SpRp_cuv2 = sum(Spcu0v2,Vpcu0v2,Rpcu1v2,Spcu1v2,Rpcu2v2,Spcu2v2,Rpcu3v2)
      SpRp_cuv3 = sum(Spcu0v3,Vpcu0v3,Rpcu1v3,Spcu1v3,Rpcu2v3,Spcu2v3,Rpcu3v3)
      
      SpRp_arv0 = sum(Rpar1v0, W1Rpar1v0,W2Rpar1v0,W3Rpar1v0,Spar1v0,Rpar2v0,Spar2v0,Rpar3v0)
      SpRp_arv1 = sum(Spar0v1,Vpar0v1,W1Vpar0v1,W2Vpar0v1,W3Vpar0v1,Rpar1v1,Spar1v1,Rpar2v1, Spar2v1,Rpar3v1)
      SpRp_arv2 = sum(Spar0v2,Vpar0v2,Rpar1v2,Spar1v2,Rpar2v2,Spar2v2,Rpar3v2)
      SpRp_arv3 = sum(Spar0v3,Vpar0v3,Rpar1v3,Spar1v3,Rpar2v3,Spar2v3,Rpar3v3)
      
      SpRp_auv0 = sum(Rpau1v0, W1Rpau1v0,W2Rpau1v0,W3Rpau1v0,Spau1v0,Rpau2v0,Spau2v0,Rpau3v0)
      SpRp_auv1 = sum(Spau0v1,Vpau0v1,W1Vpau0v1,W2Vpau0v1,W3Vpau0v1,Rpau1v1,Spau1v1,Rpau2v1, Spau2v1,Rpau3v1)
      SpRp_auv2 = sum(Spau0v2,Vpau0v2,Rpau1v2,Spau1v2,Rpau2v2,Spau2v2,Rpau3v2)
      SpRp_auv3 = sum(Spau0v3,Vpau0v3,Rpau1v3,Spau1v3,Rpau2v3,Spau2v3,Rpau3v3)
      
      SpRp_erv0 = sum(Rper1v0, W1Rper1v0,W2Rper1v0,W3Rper1v0,Sper1v0,Rper2v0,Sper2v0,Rper3v0)
      SpRp_erv1 = sum(Sper0v1,Vper0v1,W1Vper0v1,W2Vper0v1,W3Vper0v1,Rper1v1,Sper1v1,Rper2v1, Sper2v1,Rper3v1)
      SpRp_erv2 = sum(Sper0v2,Vper0v2,Rper1v2,Sper1v2,Rper2v2,Sper2v2,Rper3v2)
      SpRp_erv3 = sum(Sper0v3,Vper0v3,Rper1v3,Sper1v3,Rper2v3,Sper2v3,Rper3v3)
      
      SpRp_euv0 = sum(Rpeu1v0, W1Rpeu1v0,W2Rpeu1v0,W3Rpeu1v0,Speu1v0,Rpeu2v0,Speu2v0,Rpeu3v0)
      SpRp_euv1 = sum(Speu0v1,Vpeu0v1,W1Vpeu0v1,W2Vpeu0v1,W3Vpeu0v1,Rpeu1v1,Speu1v1,Rpeu2v1, Speu2v1,Rpeu3v1)
      SpRp_euv2 = sum(Speu0v2,Vpeu0v2,Rpeu1v2,Speu1v2,Rpeu2v2,Speu2v2,Rpeu3v2)
      SpRp_euv3 = sum(Speu0v3,Vpeu0v3,Rpeu1v3,Speu1v3,Rpeu2v3,Speu2v3,Rpeu3v3)
      
      #Active infection sums
      active_crv0 =sum(Ecr2v0, Acr2v0, Icr2v0, Hcr2v0,Ecr3v0, Acr3v0, Icr3v0, Hcr3v0)
      active_crv1 =sum(Ecr1v1, Acr1v1, Icr1v1, Hcr1v1,Ecr2v1, Acr2v1, Icr2v1, Hcr2v1,Ecr3v1, Acr3v1, Icr3v1, Hcr3v1)
      active_crv2 =sum(Ecr1v2, Acr1v2, Icr1v2, Hcr1v2,Ecr2v2, Acr2v2, Icr2v2, Hcr2v2,Ecr3v2, Acr3v2, Icr3v2, Hcr3v2)
      active_crv3 =sum(Ecr1v3, Acr1v3, Icr1v3, Hcr1v3,Ecr2v3, Acr2v3, Icr2v3, Hcr2v3,Ecr3v3, Acr3v3, Icr3v3, Hcr3v3)
      
      active_cuv0 =sum(Ecu2v0, Acu2v0, Icu2v0, Hcu2v0,Ecu3v0, Acu3v0, Icu3v0, Hcu3v0)
      active_cuv1 =sum(Ecu1v1, Acu1v1, Icu1v1, Hcu1v1,Ecu2v1, Acu2v1, Icu2v1, Hcu2v1,Ecu3v1, Acu3v1, Icu3v1, Hcu3v1)
      active_cuv2 =sum(Ecu1v2, Acu1v2, Icu1v2, Hcu1v2,Ecu2v2, Acu2v2, Icu2v2, Hcu2v2,Ecu3v2, Acu3v2, Icu3v2, Hcu3v2)
      active_cuv3 =sum(Ecu1v3, Acu1v3, Icu1v3, Hcu1v3,Ecu2v3, Acu2v3, Icu2v3, Hcu2v3,Ecu3v3, Acu3v3, Icu3v3, Hcu3v3)
      
      active_arv0 =sum(Ear2v0, Aar2v0, Iar2v0, Har2v0,Ear3v0, Aar3v0, Iar3v0, Har3v0)
      active_arv1 =sum(Ear1v1, Aar1v1, Iar1v1, Har1v1,Ear2v1, Aar2v1, Iar2v1, Har2v1,Ear3v1, Aar3v1, Iar3v1, Har3v1)
      active_arv2 =sum(Ear1v2, Aar1v2, Iar1v2, Har1v2,Ear2v2, Aar2v2, Iar2v2, Har2v2,Ear3v2, Aar3v2, Iar3v2, Har3v2)
      active_arv3 =sum(Ear1v3, Aar1v3, Iar1v3, Har1v3,Ear2v3, Aar2v3, Iar2v3, Har2v3,Ear3v3, Aar3v3, Iar3v3, Har3v3)
      
      active_auv0 =sum(Eau2v0, Aau2v0, Iau2v0, Hau2v0,Eau3v0, Aau3v0, Iau3v0, Hau3v0)
      active_auv1 =sum(Eau1v1, Aau1v1, Iau1v1, Hau1v1,Eau2v1, Aau2v1, Iau2v1, Hau2v1,Eau3v1, Aau3v1, Iau3v1, Hau3v1)
      active_auv2 =sum(Eau1v2, Aau1v2, Iau1v2, Hau1v2,Eau2v2, Aau2v2, Iau2v2, Hau2v2,Eau3v2, Aau3v2, Iau3v2, Hau3v2)
      active_auv3 =sum(Eau1v3, Aau1v3, Iau1v3, Hau1v3,Eau2v3, Aau2v3, Iau2v3, Hau2v3,Eau3v3, Aau3v3, Iau3v3, Hau3v3)
      
      active_erv0 =sum(Eer2v0, Aer2v0, Ier2v0, Her2v0,Eer3v0, Aer3v0, Ier3v0, Her3v0)
      active_erv1 =sum(Eer1v1, Aer1v1, Ier1v1, Her1v1,Eer2v1, Aer2v1, Ier2v1, Her2v1,Eer3v1, Aer3v1, Ier3v1, Her3v1)
      active_erv2 =sum(Eer1v2, Aer1v2, Ier1v2, Her1v2,Eer2v2, Aer2v2, Ier2v2, Her2v2,Eer3v2, Aer3v2, Ier3v2, Her3v2)
      active_erv3 =sum(Eer1v3, Aer1v3, Ier1v3, Her1v3,Eer2v3, Aer2v3, Ier2v3, Her2v3,Eer3v3, Aer3v3, Ier3v3, Her3v3)
      
      active_euv0 =sum(Eeu2v0, Aeu2v0, Ieu2v0, Heu2v0,Eeu3v0, Aeu3v0, Ieu3v0, Heu3v0)
      active_euv1 =sum(Eeu1v1, Aeu1v1, Ieu1v1, Heu1v1,Eeu2v1, Aeu2v1, Ieu2v1, Heu2v1,Eeu3v1, Aeu3v1, Ieu3v1, Heu3v1)
      active_euv2 =sum(Eeu1v2, Aeu1v2, Ieu1v2, Heu1v2,Eeu2v2, Aeu2v2, Ieu2v2, Heu2v2,Eeu3v2, Aeu3v2, Ieu3v2, Heu3v2)
      active_euv3 =sum(Eeu1v3, Aeu1v3, Ieu1v3, Heu1v3,Eeu2v3, Aeu2v3, Ieu2v3, Heu2v3,Eeu3v3, Aeu3v3, Ieu3v3, Heu3v3)
      
      #Population
      Ncrv0 =sum(SpRp_crv0,active_crv0,Scr0v0,Ecr1v0, Acr1v0, Icr1v0, Hcr1v0,Rncr1v0,Sncr1v0,Rncr2v0,Sncr2v0, Rncr3v0)
      Ncrv1 =sum(SpRp_crv1,active_crv1,Vncr0v1, Sncr0v1, Rncr1v1,Sncr1v1,Rncr2v1,Sncr2v1, Rncr3v1)
      Ncrv2 =sum(SpRp_crv2,active_crv2,Vncr0v2, Sncr0v2, Rncr1v2,Sncr1v2,Rncr2v2,Sncr2v2, Rncr3v2)
      Ncrv3 =sum(SpRp_crv3,active_crv3,Vncr0v3, Sncr0v3, Rncr1v3,Sncr1v3,Rncr2v3,Sncr2v3, Rncr3v3)
      
      Ncuv0 =sum(SpRp_cuv0,active_cuv0,Scu0v0,Ecu1v0, Acu1v0, Icu1v0, Hcu1v0,Rncu1v0,Sncu1v0,Rncu2v0,Sncu2v0, Rncu3v0)
      Ncuv1 =sum(SpRp_cuv1,active_cuv1,Vncu0v1, Sncu0v1, Rncu1v1,Sncu1v1,Rncu2v1,Sncu2v1, Rncu3v1)
      Ncuv2 =sum(SpRp_cuv2,active_cuv2,Vncu0v2, Sncu0v2, Rncu1v2,Sncu1v2,Rncu2v2,Sncu2v2, Rncu3v2)
      Ncuv3 =sum(SpRp_cuv3,active_cuv3,Vncu0v3, Sncu0v3, Rncu1v3,Sncu1v3,Rncu2v3,Sncu2v3, Rncu3v3)
      
      Narv0 =sum(SpRp_arv0,active_arv0,Sar0v0,Ear1v0, Aar1v0, Iar1v0, Har1v0,Rnar1v0,Snar1v0,Rnar2v0,Snar2v0, Rnar3v0)
      Narv1 =sum(SpRp_arv1,active_arv1,Vnar0v1, Snar0v1, Rnar1v1,Snar1v1,Rnar2v1,Snar2v1, Rnar3v1)
      Narv2 =sum(SpRp_arv2,active_arv2,Vnar0v2, Snar0v2, Rnar1v2,Snar1v2,Rnar2v2,Snar2v2, Rnar3v2)
      Narv3 =sum(SpRp_arv3,active_arv3,Vnar0v3, Snar0v3, Rnar1v3,Snar1v3,Rnar2v3,Snar2v3, Rnar3v3)
      
      Nauv0 =sum(SpRp_auv0,active_auv0,Sau0v0,Eau1v0, Aau1v0, Iau1v0, Hau1v0,Rnau1v0,Snau1v0,Rnau2v0,Snau2v0, Rnau3v0)
      Nauv1 =sum(SpRp_auv1,active_auv1,Vnau0v1, Snau0v1, Rnau1v1,Snau1v1,Rnau2v1,Snau2v1, Rnau3v1)
      Nauv2 =sum(SpRp_auv2,active_auv2,Vnau0v2, Snau0v2, Rnau1v2,Snau1v2,Rnau2v2,Snau2v2, Rnau3v2)
      Nauv3 =sum(SpRp_auv3,active_auv3,Vnau0v3, Snau0v3, Rnau1v3,Snau1v3,Rnau2v3,Snau2v3, Rnau3v3)
      
      Nerv0 =sum(SpRp_erv0,active_erv0,Ser0v0,Eer1v0, Aer1v0, Ier1v0, Her1v0,Rner1v0,Sner1v0,Rner2v0,Sner2v0, Rner3v0)
      Nerv1 =sum(SpRp_erv1,active_erv1,Vner0v1, Sner0v1, Rner1v1,Sner1v1,Rner2v1,Sner2v1, Rner3v1)
      Nerv2 =sum(SpRp_erv2,active_erv2,Vner0v2, Sner0v2, Rner1v2,Sner1v2,Rner2v2,Sner2v2, Rner3v2)
      Nerv3 =sum(SpRp_erv3,active_erv3,Vner0v3, Sner0v3, Rner1v3,Sner1v3,Rner2v3,Sner2v3, Rner3v3)
      
      Neuv0 =sum(SpRp_euv0,active_euv0,Seu0v0,Eeu1v0, Aeu1v0, Ieu1v0, Heu1v0,Rneu1v0,Sneu1v0,Rneu2v0,Sneu2v0, Rneu3v0)
      Neuv1 =sum(SpRp_euv1,active_euv1,Vneu0v1, Sneu0v1, Rneu1v1,Sneu1v1,Rneu2v1,Sneu2v1, Rneu3v1)
      Neuv2 =sum(SpRp_euv2,active_euv2,Vneu0v2, Sneu0v2, Rneu1v2,Sneu1v2,Rneu2v2,Sneu2v2, Rneu3v2)
      Neuv3 =sum(SpRp_euv3,active_euv3,Vneu0v3, Sneu0v3, Rneu1v3,Sneu1v3,Rneu2v3,Sneu2v3, Rneu3v3)
      
      
      ##Population sums
      Nchildr = sum(Ncrv0,Ncrv1,Ncrv2,Ncrv3)
      Nadultr = sum(Narv0,Narv1,Narv2,Narv3)
      Noldr   = sum(Nerv0,Nerv1,Nerv2,Nerv3)
      
      Nchildu = sum(Ncuv0,Ncuv1,Ncuv2,Ncuv3)
      Nadultu = sum(Nauv0,Nauv1,Nauv2,Nauv3)
      Noldu   = sum(Neuv0,Neuv1,Neuv2,Neuv3)
      
      seropos_cr = sum(SpRp_crv0, SpRp_crv1, SpRp_crv2, SpRp_crv3, active_crv0, active_crv1, active_crv2, active_crv3)
      seropos_cu = sum(SpRp_cuv0, SpRp_cuv1, SpRp_cuv2, SpRp_cuv3, active_cuv0, active_cuv1, active_cuv2, active_cuv3)
      seropos_ar = sum(SpRp_arv0, SpRp_arv1, SpRp_arv2, SpRp_arv3, active_arv0, active_arv1, active_arv2, active_arv3)
      seropos_au = sum(SpRp_auv0, SpRp_auv1, SpRp_auv2, SpRp_auv3, active_auv0, active_auv1, active_auv2, active_auv3)
      seropos_er = sum(SpRp_erv0, SpRp_erv1, SpRp_erv2, SpRp_erv3, active_erv0, active_erv1, active_erv2, active_erv3)
      seropos_eu = sum(SpRp_euv0, SpRp_euv1, SpRp_euv2, SpRp_euv3, active_euv0, active_euv1, active_euv2, active_euv3)
      
      seroprev_cr = seropos_cr/Nchildr
      seroprev_cu = seropos_cu/Nchildu
      seroprev_ar = seropos_ar/Nadultr
      seroprev_au = seropos_au/Nadultu
      seroprev_er = seropos_er/Noldr
      seroprev_eu = seropos_eu/Noldu
      
      seroprev_c = (seropos_cr+seropos_cu)/(Nchildr+Nchildu)
      seroprev_a = (seropos_ar+seropos_au)/(Nadultr+Nadultu)
      seroprev_e = (seropos_er+seropos_eu)/(Noldr+Noldu)
      
      seroprev_to = (seropos_cr+seropos_cu+seropos_ar+seropos_au+seropos_er+seropos_eu)/(Nchildr+Nchildu+Nadultr+Nadultu+Noldr+Noldu)
      
      sero_e1[t]<<-seroprev_e

      #yroot <-c(seroprev_e-0.6,start[859])
      yroot <- c(seroprev_e-sero_thresh, start[859])
      
      #trigger <<- if(abs(yroot[2])<0.6) TRUE else FALSE
      trigger <<- if(abs(yroot[2])<sero_thresh) TRUE else FALSE
      #trigger1[t] <<- if(abs(yroot[2])<0.6) TRUE else FALSE
      yroot1[t]<<-yroot[1]
      yroot2[t]<<-yroot[2]
      #if(abs(start[18])<1) trigger<<-TRUE
      return(yroot)    
      #sero_cr-0.005
    })
  }
  
  eventFun <- function(t,start,params){
    with(as.list(start, params),{
      start[859] <- -30
      start[862] <- 0   #ar delta 3_vax
      start[863] <- 0   #au delta 3_vax
      start[864] <- 0.02    #er delta 3_vax
      start[865] <- 0.02    #eu delta 3_vax
      
      #start[19] <- if(abs(start[18] < 1e-6)) 0 else 0.00001
      #start[18] <- if(abs(start[18] < 1e-6)) 1 else start[18]
      
      #whichroot <- which(abs(yroot) < 1e-6) # specify tolerance
      #start[19] <- if(whichroot == 2) 0 else 0.00001
      #if(start[18]<=1&start[18]>=1) trigger <<-TRUE
      start[862] <- if(trigger) 0 else 0
      start[863] <- if(trigger) 0 else 0
      start[864] <- if(trigger) 0 else 0.02
      start[865] <- if(trigger) 0 else 0.02      
      start[859] <- if(trigger) 0 else start[859]
      #if(trigger) trigger<<-FALSE
      return(start)
    })
  }
  
  model_out <- as.data.frame(ode(y = start, times = t, fun = COVID_sero_vax, parms = params, rootfun=rootFun, events=list(func=eventFun, root=T)))
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
  saveRDS(res_out,paste("/projects/blopman/vger/cliu/0_sens_kappa/sw_run_",num,".RDS",sep=""))
  return(res_out)

}