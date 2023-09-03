library(dplyr)
library(deSolve)
library(ggplot2)
library(tidyverse)
source("1_model_code.R")
source("0_getR0_func.R")
sweep<- readRDS("0_calib_sweep.RDS")


dt_end=as.Date("2022-9-1")
tot_pop <- 30066648   ## Took population for 2020 from Mozambique INE (Institute of statistics)
p_urban <- 0.34 ##From INE
p_rural <- 1-p_urban
start.Ns <- c(10884513, 4883969, 7958844, 4900719, 992149, 446454)
dist <- start.Ns/tot_pop

pop_dist = data.frame(age_ur = c("cr","cu","ar","au","er","eu"),
                      pop = start.Ns)

model_sims <- function(i){
  
  #library(tidyverse)
  dt_start = as.Date("2020-05-07")
  dt_end = dt_end
  total_time = length(seq(from=dt_start, to=dt_end, by=1))
  
  t = seq(0,total_time,1)      
  
  ####Scenario sweeps####
  sweep <- sweep
  num_sweep <- nrow(sweep)
  
  ##########################
  ##Model initiation stuff##
  ##########################
  ##Initial state values##
  
  
  distI<-c(0.354*p_rural, 0.354*p_urban, 0.586*p_rural, 0.586*p_urban, 0.059*p_rural, 0.059*p_urban) ###Guessed age/rural dist of infections
  distH<-c(0.023*0.069, 0.023*0.931, 0.751*0.357, 0.751*0.643, 0.227*0.763, 0.227*0.237) #Guessed age/rural dist of hospitalizations
  #obtained by applying hospitalization rates to distI and assuming equal probability by risk group within age strata
  reporting <- 1/5   ##Guessed
  cum_I <- 7/reporting ## Started May 7th, 2020
  start.Is<-distI*cum_I 
  start.As<-(1/0.6)*(1-0.6)*start.Is #1/p gives total infections and (1-p)*infections gives asymptomatic infections 
  start.Es<-start.Is+start.As
  
  ##FIX BASELINE H##
  start.Hs<-distH*sum(start.Is*(0.117)) #Just tried a weighted average of the proportion of symptomatic cases
  #hospitalized
  start.Rs<-c(0,0,0,0,0,0)*start.Ns #Upper bound
  start.Ds<- c(0,0,0,0,0,0)
  
  start.Ss<-start.Ns-(start.Is+start.Es+start.As+start.Hs+start.Rs+start.Ds)
  #For now, assuming no starting vaccination.  In follow-up simulations, we should take 
  #Svp0 and Sv0 from S0 mostly, with some small proportion of the population being in S10.
  
  
  start = c(
    #Unvaccinated and unexposed
    Scr0v0=start.Ss[1], Scu0v0=start.Ss[2], Sar0v0=start.Ss[3], Sau0v0=start.Ss[4], Ser0v0=start.Ss[5], Seu0v0=start.Ss[6], #S0
    Ecr1v0=start.Es[1], Ecu1v0=start.Es[2], Ear1v0=start.Es[3], Eau1v0=start.Es[4], Eer1v0=start.Es[5], Eeu1v0=start.Es[6], #E1
    Acr1v0=start.As[1], Acu1v0=start.As[2], Aar1v0=start.As[3], Aau1v0=start.As[4], Aer1v0=start.As[5], Aeu1v0=start.As[6], #A1
    Icr1v0=start.Is[1], Icu1v0=start.Is[2], Iar1v0=start.Is[3], Iau1v0=start.Is[4], Ier1v0=start.Is[5], Ieu1v0=start.Is[6], #I1
    Hcr1v0=start.Hs[1], Hcu1v0=start.Hs[2], Har1v0=start.Hs[3], Hau1v0=start.Hs[4], Her1v0=start.Hs[5], Heu1v0=start.Hs[6], #H1
    Rpcr1v0=start.Rs[1],Rpcu1v0=start.Rs[2],Rpar1v0=start.Rs[3],Rpau1v0=start.Rs[4],Rper1v0=start.Rs[5],Rpeu1v0=start.Rs[6], #R1
    Rncr1v0=start.Rs[1],Rncu1v0=start.Rs[2],Rnar1v0=start.Rs[3],Rnau1v0=start.Rs[4],Rner1v0=start.Rs[5],Rneu1v0=start.Rs[6], #R1
    Dcr1v0=start.Ds[1], Dcu1v0=start.Ds[2], Dar1v0=start.Ds[3], Dau1v0=start.Ds[4], Der1v0=start.Ds[5], Deu1v0=start.Ds[6], #M1
    
    #Unvaccinated one exposure
    Spcr1v0=0, Spcu1v0=0, Spar1v0=0, Spau1v0=0, Sper1v0=0,  Speu1v0=0, #S1 part sussceptible after first exposure & seropositive
    Sncr1v0=0, Sncu1v0=0, Snar1v0=0, Snau1v0=0, Sner1v0=0,  Sneu1v0=0, #S1 part sussceptible after first exposure & seronegative
    Ecr2v0 =0, Ecu2v0 =0, Ear2v0 =0, Eau2v0 =0, Eer2v0 =0,  Eeu2v0 =0, #E2 infection among those with first prior exposure
    Acr2v0 =0, Acu2v0 =0, Aar2v0 =0, Aau2v0 =0, Aer2v0 =0,  Aeu2v0 =0, #A2
    Icr2v0 =0, Icu2v0 =0, Iar2v0 =0, Iau2v0 =0, Ier2v0 =0,  Ieu2v0 =0, #I2
    Hcr2v0 =0, Hcu2v0 =0, Har2v0 =0, Hau2v0 =0, Her2v0 =0,  Heu2v0 =0, #H2
    Rpcr2v0=0, Rpcu2v0=0, Rpar2v0=0, Rpau2v0=0, Rper2v0=0,  Rpeu2v0=0, #Rp2 Immune after second exposure and seropos
    Rncr2v0=0, Rncu2v0=0, Rnar2v0=0, Rnau2v0=0, Rner2v0=0,  Rneu2v0=0, #Rn2 Immune after second exposure and seroneg
    Dcr2v0 =0, Dcu2v0 =0, Dar2v0 =0, Dau2v0 =0, Der2v0 =0,  Deu2v0 =0, #D2
    
    #Unvaccinated two exposure
    Spcr2v0=0, Spcu2v0=0, Spar2v0=0, Spau2v0=0, Sper2v0=0,  Speu2v0=0, #
    Sncr2v0=0, Sncu2v0=0, Snar2v0=0, Snau2v0=0, Sner2v0=0,  Sneu2v0=0, #
    Ecr3v0 =0, Ecu3v0 =0, Ear3v0 =0, Eau3v0 =0, Eer3v0 =0,  Eeu3v0 =0, 
    Acr3v0 =0, Acu3v0 =0, Aar3v0 =0, Aau3v0 =0, Aer3v0 =0,  Aeu3v0 =0, #
    Icr3v0 =0, Icu3v0 =0, Iar3v0 =0, Iau3v0 =0, Ier3v0 =0,  Ieu3v0 =0, #
    Hcr3v0 =0, Hcu3v0 =0, Har3v0 =0, Hau3v0 =0, Her3v0 =0,  Heu3v0 =0, #
    Rpcr3v0=0, Rpcu3v0=0, Rpar3v0=0, Rpau3v0=0, Rper3v0=0,  Rpeu3v0=0, #
    Rncr3v0=0, Rncu3v0=0, Rnar3v0=0, Rnau3v0=0, Rner3v0=0,  Rneu3v0=0, 
    Dcr3v0 =0, Dcu3v0 =0, Dar3v0 =0, Dau3v0 =0, Der3v0 =0,  Deu3v0 =0, #
    
    ##One vax unexposured
    Spcr0v1 =0,  Spcu0v1 =0,Spar0v1 =0, Spau0v1 =0, Sper0v1 =0,  Speu0v1 =0, #
    Sncr0v1 =0,  Sncu0v1 =0,Snar0v1 =0, Snau0v1 =0, Sner0v1 =0,  Sneu0v1 =0, #
    Ecr1v1 =0, Ecu1v1 =0, Ear1v1 =0, Eau1v1 =0, Eer1v1 =0,  Eeu1v1 =0, 
    Acr1v1 =0, Acu1v1 =0, Aar1v1 =0, Aau1v1 =0, Aer1v1 =0,  Aeu1v1 =0, #
    Icr1v1 =0, Icu1v1 =0, Iar1v1 =0, Iau1v1 =0, Ier1v1 =0,  Ieu1v1 =0, #
    Hcr1v1 =0, Hcu1v1 =0, Har1v1 =0, Hau1v1 =0, Her1v1 =0,  Heu1v1 =0, #
    Rpcr1v1=0, Rpcu1v1=0, Rpar1v1=0, Rpau1v1=0, Rper1v1=0,  Rpeu1v1=0, #
    Rncr1v1=0, Rncu1v1=0, Rnar1v1=0, Rnau1v1=0, Rner1v1=0,  Rneu1v1=0, 
    Dcr1v1 =0, Dcu1v1 =0, Dar1v1 =0, Dau1v1 =0, Der1v1 =0,  Deu1v1 =0, #
    
    #One vax one exposure
    Spcr1v1=0, Spcu1v1=0, Spar1v1=0, Spau1v1=0, Sper1v1=0,  Speu1v1=0, #
    Sncr1v1=0, Sncu1v1=0, Snar1v1=0, Snau1v1=0, Sner1v1=0,  Sneu1v1=0, #
    Ecr2v1 =0, Ecu2v1 =0, Ear2v1 =0, Eau2v1 =0, Eer2v1 =0,  Eeu2v1 =0, 
    Acr2v1 =0, Acu2v1 =0, Aar2v1 =0, Aau2v1 =0, Aer2v1 =0,  Aeu2v1 =0, #
    Icr2v1 =0, Icu2v1 =0, Iar2v1 =0, Iau2v1 =0, Ier2v1 =0,  Ieu2v1 =0, #
    Hcr2v1 =0, Hcu2v1 =0, Har2v1 =0, Hau2v1 =0, Her2v1 =0,  Heu2v1 =0, #
    Rpcr2v1=0, Rpcu2v1=0, Rpar2v1=0, Rpau2v1=0, Rper2v1=0,  Rpeu2v1=0, #
    Rncr2v1=0, Rncu2v1=0, Rnar2v1=0, Rnau2v1=0, Rner2v1=0,  Rneu2v1=0, 
    Dcr2v1 =0, Dcu2v1 =0, Dar2v1 =0, Dau2v1 =0, Der2v1 =0,  Deu2v1 =0, #
    
    # One vax two exposure
    Spcr2v1=0, Spcu2v1=0, Spar2v1=0, Spau2v1=0, Sper2v1=0,  Speu2v1=0, #
    Sncr2v1=0, Sncu2v1=0, Snar2v1=0, Snau2v1=0, Sner2v1=0,  Sneu2v1=0, #
    Ecr3v1 =0, Ecu3v1 =0, Ear3v1 =0, Eau3v1 =0, Eer3v1 =0,  Eeu3v1 =0, 
    Acr3v1 =0, Acu3v1 =0, Aar3v1 =0, Aau3v1 =0, Aer3v1 =0,  Aeu3v1 =0, #
    Icr3v1 =0, Icu3v1 =0, Iar3v1 =0, Iau3v1 =0, Ier3v1 =0,  Ieu3v1 =0, #
    Hcr3v1 =0, Hcu3v1 =0, Har3v1 =0, Hau3v1 =0, Her3v1 =0,  Heu3v1 =0, #
    Rpcr3v1=0, Rpcu3v1=0, Rpar3v1=0, Rpau3v1=0, Rper3v1=0,  Rpeu3v1=0, #
    Rncr3v1=0, Rncu3v1=0, Rnar3v1=0, Rnau3v1=0, Rner3v1=0,  Rneu3v1=0, 
    Dcr3v1 =0, Dcu3v1 =0, Dar3v1 =0, Dau3v1 =0, Der3v1 =0,  Deu3v1 =0, #
    
    ##Two vax unexposured
    Spcr0v2 =0, Spcu0v2 =0, Spar0v2 =0, Spau0v2 =0, Sper0v2 =0,  Speu0v2 =0, #
    Sncr0v2 =0, Sncu0v2 =0, Snar0v2 =0, Snau0v2 =0, Sner0v2 =0,  Sneu0v2 =0, #
    Ecr1v2 =0, Ecu1v2 =0, Ear1v2 =0, Eau1v2 =0, Eer1v2 =0,  Eeu1v2 =0, 
    Acr1v2 =0, Acu1v2 =0, Aar1v2 =0, Aau1v2 =0, Aer1v2 =0,  Aeu1v2 =0, #
    Icr1v2 =0, Icu1v2 =0, Iar1v2 =0, Iau1v2 =0, Ier1v2 =0,  Ieu1v2 =0, #
    Hcr1v2 =0, Hcu1v2 =0, Har1v2 =0, Hau1v2 =0, Her1v2 =0,  Heu1v2 =0, #
    Rpcr1v2=0, Rpcu1v2=0, Rpar1v2=0, Rpau1v2=0, Rper1v2=0,  Rpeu1v2=0, #
    Rncr1v2=0, Rncu1v2=0, Rnar1v2=0, Rnau1v2=0, Rner1v2=0,  Rneu1v2=0, 
    Dcr1v2 =0, Dcu1v2 =0, Dar1v2 =0, Dau1v2 =0, Der1v2 =0,  Deu1v2 =0, #
    
    #Two vax one exposure
    Spcr1v2=0, Spcu1v2=0, Spar1v2=0, Spau1v2=0, Sper1v2=0,  Speu1v2=0, #
    Sncr1v2=0, Sncu1v2=0, Snar1v2=0, Snau1v2=0, Sner1v2=0,  Sneu1v2=0, #
    Ecr2v2 =0, Ecu2v2 =0, Ear2v2 =0, Eau2v2 =0, Eer2v2 =0,  Eeu2v2 =0, 
    Acr2v2 =0, Acu2v2 =0, Aar2v2 =0, Aau2v2 =0, Aer2v2 =0,  Aeu2v2 =0, #
    Icr2v2 =0, Icu2v2 =0, Iar2v2 =0, Iau2v2 =0, Ier2v2 =0,  Ieu2v2 =0, #
    Hcr2v2 =0, Hcu2v2 =0, Har2v2 =0, Hau2v2 =0, Her2v2 =0,  Heu2v2 =0, #
    Rpcr2v2=0, Rpcu2v2=0, Rpar2v2=0, Rpau2v2=0, Rper2v2=0,  Rpeu2v2=0, #
    Rncr2v2=0, Rncu2v2=0, Rnar2v2=0, Rnau2v2=0, Rner2v2=0,  Rneu2v2=0, 
    Dcr2v2 =0, Dcu2v2 =0, Dar2v2 =0, Dau2v2 =0, Der2v2 =0,  Deu2v2 =0, #
    
    # Two vax two exposure
    Spcr2v2=0, Spcu2v2=0, Spar2v2=0, Spau2v2=0, Sper2v2=0,  Speu2v2=0, #
    Sncr2v2=0, Sncu2v2=0, Snar2v2=0, Snau2v2=0, Sner2v2=0,  Sneu2v2=0, #
    Ecr3v2 =0, Ecu3v2 =0, Ear3v2 =0, Eau3v2 =0, Eer3v2 =0,  Eeu3v2 =0, 
    Acr3v2 =0, Acu3v2 =0, Aar3v2 =0, Aau3v2 =0, Aer3v2 =0,  Aeu3v2 =0, #
    Icr3v2 =0, Icu3v2 =0, Iar3v2 =0, Iau3v2 =0, Ier3v2 =0,  Ieu3v2 =0, #
    Hcr3v2 =0, Hcu3v2 =0, Har3v2 =0, Hau3v2 =0, Her3v2 =0,  Heu3v2 =0, #
    Rpcr3v2=0, Rpcu3v2=0, Rpar3v2=0, Rpau3v2=0, Rper3v2=0,  Rpeu3v2=0, #
    Rncr3v2=0, Rncu3v2=0, Rnar3v2=0, Rnau3v2=0, Rner3v2=0,  Rneu3v2=0, 
    Dcr3v2 =0, Dcu3v2 =0, Dar3v2 =0, Dau3v2 =0, Der3v2 =0,  Deu3v2 =0, #
    
    ##Three vax unexposured
    Spcr0v3 =0, Spcu0v3 =0, Spar0v3 =0, Spau0v3 =0, Sper0v3 =0,  Speu0v3 =0, #
    Sncr0v3 =0, Sncu0v3 =0, Snar0v3 =0, Snau0v3 =0, Sner0v3 =0,  Sneu0v3 =0, #
    Ecr1v3 =0, Ecu1v3 =0, Ear1v3 =0, Eau1v3 =0, Eer1v3 =0,  Eeu1v3 =0, 
    Acr1v3 =0, Acu1v3 =0, Aar1v3 =0, Aau1v3 =0, Aer1v3 =0,  Aeu1v3 =0, #
    Icr1v3 =0, Icu1v3 =0, Iar1v3 =0, Iau1v3 =0, Ier1v3 =0,  Ieu1v3 =0, #
    Hcr1v3 =0, Hcu1v3 =0, Har1v3 =0, Hau1v3 =0, Her1v3 =0,  Heu1v3 =0, #
    Rpcr1v3=0, Rpcu1v3=0, Rpar1v3=0, Rpau1v3=0, Rper1v3=0,  Rpeu1v3=0, #
    Rncr1v3=0, Rncu1v3=0, Rnar1v3=0, Rnau1v3=0, Rner1v3=0,  Rneu1v3=0, 
    Dcr1v3 =0, Dcu1v3 =0, Dar1v3 =0, Dau1v3 =0, Der1v3 =0,  Deu1v3 =0, #
    
    #Three vax one exposure
    Spcr1v3=0, Spcu1v3=0, Spar1v3=0, Spau1v3=0, Sper1v3=0,  Speu1v3=0, #
    Sncr1v3=0, Sncu1v3=0, Snar1v3=0, Snau1v3=0, Sner1v3=0,  Sneu1v3=0, #
    Ecr2v3 =0, Ecu2v3 =0, Ear2v3 =0, Eau2v3 =0, Eer2v3 =0,  Eeu2v3 =0, 
    Acr2v3 =0, Acu2v3 =0, Aar2v3 =0, Aau2v3 =0, Aer2v3 =0,  Aeu2v3 =0, #
    Icr2v3 =0, Icu2v3 =0, Iar2v3 =0, Iau2v3 =0, Ier2v3 =0,  Ieu2v3 =0, #
    Hcr2v3 =0, Hcu2v3 =0, Har2v3 =0, Hau2v3 =0, Her2v3 =0,  Heu2v3 =0, #
    Rpcr2v3=0, Rpcu2v3=0, Rpar2v3=0, Rpau2v3=0, Rper2v3=0,  Rpeu2v3=0, #
    Rncr2v3=0, Rncu2v3=0, Rnar2v3=0, Rnau2v3=0, Rner2v3=0,  Rneu2v3=0, 
    Dcr2v3 =0, Dcu2v3 =0, Dar2v3 =0, Dau2v3 =0, Der2v3 =0,  Deu2v3 =0, #
    
    # Three vax two exposure
    Spcr2v3=0, Spcu2v3=0, Spar2v3=0, Spau2v3=0, Sper2v3=0,  Speu2v3=0, #
    Sncr2v3=0, Sncu2v3=0, Snar2v3=0, Snau2v3=0, Sner2v3=0,  Sneu2v3=0, #
    Ecr3v3 =0, Ecu3v3 =0, Ear3v3 =0, Eau3v3 =0, Eer3v3 =0,  Eeu3v3 =0, 
    Acr3v3 =0, Acu3v3 =0, Aar3v3 =0, Aau3v3 =0, Aer3v3 =0,  Aeu3v3 =0, #
    Icr3v3 =0, Icu3v3 =0, Iar3v3 =0, Iau3v3 =0, Ier3v3 =0,  Ieu3v3 =0, #
    Hcr3v3 =0, Hcu3v3 =0, Har3v3 =0, Hau3v3 =0, Her3v3 =0,  Heu3v3 =0, #
    Rpcr3v3=0, Rpcu3v3=0, Rpar3v3=0, Rpau3v3=0, Rper3v3=0,  Rpeu3v3=0, #
    Rncr3v3=0, Rncu3v3=0, Rnar3v3=0, Rnau3v3=0, Rner3v3=0,  Rneu3v3=0, 
    Dcr3v3 =0, Dcu3v3 =0, Dar3v3 =0, Dau3v3 =0, Der3v3 =0,  Deu3v3 =0, #
    
    Ecum1v0_cr =0, Ecum1v0_cu =0, Ecum1v0_ar=0, Ecum1v0_au=0, Ecum1v0_er=0, Ecum1v0_eu=0,
    Ecum2v0_cr =0, Ecum2v0_cu =0, Ecum2v0_ar=0, Ecum2v0_au=0, Ecum2v0_er=0, Ecum2v0_eu=0,
    Ecum3v0_cr =0, Ecum3v0_cu =0, Ecum3v0_ar=0, Ecum3v0_au=0, Ecum3v0_er=0, Ecum3v0_eu=0,
    
    Ecum1v1_cr=0, Ecum1v1_cu=0, Ecum1v1_ar=0, Ecum1v1_au=0, Ecum1v1_er=0, Ecum1v1_eu=0,
    Ecum2v1_cr=0, Ecum2v1_cu=0, Ecum2v1_ar=0, Ecum2v1_au=0, Ecum2v1_er=0, Ecum2v1_eu=0,
    Ecum3v1_cr=0, Ecum3v1_cu=0, Ecum3v1_ar=0, Ecum3v1_au=0, Ecum3v1_er=0, Ecum3v1_eu=0,
    
    Ecum1v2_cr=0, Ecum1v2_cu=0, Ecum1v2_ar=0, Ecum1v2_au=0, Ecum1v2_er=0, Ecum1v2_eu=0,
    Ecum2v2_cr=0, Ecum2v2_cu=0, Ecum2v2_ar=0, Ecum2v2_au=0, Ecum2v2_er=0, Ecum2v2_eu=0,
    Ecum3v2_cr=0, Ecum3v2_cu=0, Ecum3v2_ar=0, Ecum3v2_au=0, Ecum3v2_er=0, Ecum3v2_eu=0,
    
    Ecum1v3_cr=0, Ecum1v3_cu=0, Ecum1v3_ar=0, Ecum1v3_au=0, Ecum1v3_er=0, Ecum1v3_eu=0,
    Ecum2v3_cr=0, Ecum2v3_cu=0, Ecum2v3_ar=0, Ecum2v3_au=0, Ecum2v3_er=0, Ecum2v3_eu=0,
    Ecum3v3_cr=0, Ecum3v3_cu=0, Ecum3v3_ar=0, Ecum3v3_au=0, Ecum3v3_er=0, Ecum3v3_eu=0,
    
    Icum1v0_cr=0, Icum1v0_cu=0, Icum1v0_ar=0, Icum1v0_au=0, Icum1v0_er=0, Icum1v0_eu=0,
    Icum2v0_cr=0, Icum2v0_cu=0, Icum2v0_ar=0, Icum2v0_au=0, Icum2v0_er=0, Icum2v0_eu=0,
    Icum3v0_cr=0, Icum3v0_cu=0, Icum3v0_ar=0, Icum3v0_au=0, Icum3v0_er=0, Icum3v0_eu=0,
    
    Icum1v1_cr=0, Icum1v1_cu=0, Icum1v1_ar=0, Icum1v1_au=0, Icum1v1_er=0, Icum1v1_eu=0,
    Icum2v1_cr=0, Icum2v1_cu=0, Icum2v1_ar=0, Icum2v1_au=0, Icum2v1_er=0, Icum2v1_eu=0,
    Icum3v1_cr=0, Icum3v1_cu=0, Icum3v1_ar=0, Icum3v1_au=0, Icum3v1_er=0, Icum3v1_eu=0,
    
    Icum1v2_cr=0, Icum1v2_cu=0, Icum1v2_ar=0, Icum1v2_au=0, Icum1v2_er=0, Icum1v2_eu=0,
    Icum2v2_cr=0, Icum2v2_cu=0, Icum2v2_ar=0, Icum2v2_au=0, Icum2v2_er=0, Icum2v2_eu=0,
    Icum3v2_cr=0, Icum3v2_cu=0, Icum3v2_ar=0, Icum3v2_au=0, Icum3v2_er=0, Icum3v2_eu=0,
    
    Icum1v3_cr=0, Icum1v3_cu=0, Icum1v3_ar=0, Icum1v3_au=0, Icum1v3_er=0, Icum1v3_eu=0,
    Icum2v3_cr=0, Icum2v3_cu=0, Icum2v3_ar=0, Icum2v3_au=0, Icum2v3_er=0, Icum2v3_eu=0,
    Icum3v3_cr=0, Icum3v3_cu=0, Icum3v3_ar=0, Icum3v3_au=0, Icum3v3_er=0, Icum3v3_eu=0,
    
    W1Rpcr1v0=0, W2Rpcr1v0=0,W3Rpcr1v0=0,W1Rpcu1v0=0,W2Rpcu1v0=0,W3Rpcu1v0=0,
    W1Rpar1v0=0, W2Rpar1v0=0,W3Rpar1v0=0,W1Rpau1v0=0,W2Rpau1v0=0,W3Rpau1v0=0,
    W1Rper1v0=0, W2Rper1v0=0,W3Rper1v0=0,W1Rpeu1v0=0,W2Rpeu1v0=0,W3Rpeu1v0=0,
    
    
    Vpcr0v1 = 0, Vpcu0v1 = 0, Vpar0v1 = 0, Vpau0v1 = 0, Vper0v1 = 0, Vpeu0v1 = 0,
    Vncr0v1 = 0, Vncu0v1 = 0, Vnar0v1 = 0, Vnau0v1 = 0, Vner0v1 = 0, Vneu0v1 = 0,
    Vpcr0v2 = 0, Vpcu0v2 = 0, Vpar0v2 = 0, Vpau0v2 = 0, Vper0v2 = 0, Vpeu0v2 = 0,
    Vncr0v2 = 0, Vncu0v2 = 0, Vnar0v2 = 0, Vnau0v2 = 0, Vner0v2 = 0, Vneu0v2 = 0,
    Vpcr0v3 = 0, Vpcu0v3 = 0, Vpar0v3 = 0, Vpau0v3 = 0, Vper0v3 = 0, Vpeu0v3 = 0,
    Vncr0v3 = 0, Vncu0v3 = 0, Vnar0v3 = 0, Vnau0v3 = 0, Vner0v3 = 0, Vneu0v3 = 0,
    
    ##Gamma waning trackers
    W1Vpcr0v1 = 0,  W2Vpcr0v1 = 0, W3Vpcr0v1 = 0,
    W1Vpcu0v1 = 0,  W2Vpcu0v1 = 0, W3Vpcu0v1 = 0,
    W1Vpar0v1 = 0,  W2Vpar0v1 = 0, W3Vpar0v1 = 0,
    W1Vpau0v1 = 0,  W2Vpau0v1 = 0, W3Vpau0v1 = 0,
    W1Vper0v1 = 0,  W2Vper0v1 = 0, W3Vper0v1 = 0,
    W1Vpeu0v1 = 0,  W2Vpeu0v1 = 0, W3Vpeu0v1 = 0
    
    
  )
  
  
  ##Transmission parameters
  ## Beta is the probability of transmission when contacted with susceptible of clases c, a, e
  bl <- sweep$bl[i]
  rel_c <- sweep$relbeta_c[i]
  rel_a <- sweep$relbeta_a[i]
  
  beta_c <- bl*rel_c
  beta_a <- bl*rel_a    ##Guesses
  beta_e <- bl*1    ##Guesses
  r0 <- getr0(bl=bl, rel_c=rel_c, rel_a=rel_a,1)
  
  ##alpha is relative infectiousness of asymptomatic
  alpha <- 0.6
  
  
  # Relative infectiousness of omicron
  rel_delta <- sweep$rel_delta[i]    ## Delta increase
  rel_omi <-sweep$rel_omi[i] ##Omicron increase
  sd1<-sweep$sd1[i]    ##Omicron decrease step 1
  sd2 <- sweep$sd2[i]  ## Omicron decrease step 2
  rel_newvar <- sweep$rel_newvar[i]
  
  #mu_c <-0
  #mu_a <-0
  #mu_e <-0
  ## Vaccine parameters
  vei1<-0.50 ## VE against infection
  vep1<-0.40 ## VE against hospitalization
  vei2<-0.60 ## VE against infection 
  vep2<-0.67 ## VE against hospitalization
  vei3<-0.7 ## VE against infection
  vep3<-0.7 ## VE against hospitalization
  
  
  ##Natural history parameters
  sigma <- 1/5.5  #latent period
  
  gamma_I <-1/7      #duration of infection for symptomatically infectious
  gamma_A <-1/7      #duration of infection for asymptomatically infectious
  gamma_H <-1/5      #hospital length of stay
  
  nu_c<- 0.45         #Probability of symptomatic infection for children
  nu_a<- 0.55         #Probability of symptomatic infection for adults
  nu_e<- 0.65         #Probability of symptomatic infection for elderly
  
  phi_c<- 0.0075     #Prob of hospitalization (children) among unvaxed
  phi_a<- 0.15       #Prob of hospitalization (adults) among unvaxed
  phi_e<- 0.35       #Prob of hospitalization (elderly) among unvaxed
  
  phi_cv1 <- phi_c*(1-vep1)
  phi_av1 <- phi_a*(1-vep1)
  phi_ev1 <- phi_e*(1-vep1)
  
  phi_cv2 <- phi_c*(1-vep2)
  phi_av2 <- phi_a*(1-vep2)
  phi_ev2 <- phi_e*(1-vep2)
  
  phi_cv3 <- phi_c*(1-vep3)
  phi_av3 <- phi_a*(1-vep3)
  phi_ev3 <- phi_e*(1-vep3)
  
  
  
  
  mu_c<-0.01            #Prob of death (children)
  mu_a<-0.0465       #Prob of death (adults)
  mu_e<-0.1        #Prob of death (elderly)
  
  ##Protection post-infection
  #red_inf_1 <- 0.35  ## FOI among susceptible individuals after period of immunity is 35%%
  #red_inf_2 <- 0.15
  #red_inf_1<-0.5
  #red_inf_2<-0.4
  red_inf_1 <- sweep$red_inf_1[i]
  red_inf_2 <- red_inf_1*sweep$rel_red_inf_2[i]
  
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
  
  ## After third infection (two prior exposure class)
  omega2_pc <-0
  omega2_pa <-omega2_pc
  omega2_pe <-omega2_pc
  
  omega2_nc <-0
  omega2_na <-omega2_nc
  omega2_ne <-omega2_nc
  
  
  ## immune escape factor
  imm_esc_factor_t1 <- 1.2
  
  
  imm_esc_factor_omi<- sweep$imm_esc_factor_omi[i]
  imm_esc_factor_newvar <- sweep$imm_esc_factor_newvar[i]
  r0_hyp = sweep$r0_hyp[i]
  
  ##Serology
  # Probability of seroconversion after infection
  pi <- 0.9
  
  # Prob of seroconversion after vaccination
  rho_v1 <- 0.85
  rho_v2 <- 0.6 ## Among those seronegative after first dose
  rho_v3 <-0.6  ##Among those seronegative after second dose, 
  ##Seroreversion
  #kappa_c <- 1/250
  #kappa_a <- 1/250
  #kappa_e <- 1/250
  
  kappa1 <- sweep$kappa1[i]
  kappa2 <- sweep$kappa2[i]
  kappa3 <- sweep$kappa3[i]
  
  
  #kappa_vax <-1/250
  params<<-c('beta_c'= beta_c, 'beta_a' = beta_a, 'beta_e' = beta_e,
             'alpha' = alpha, 'rel_delta'=rel_delta,'rel_omi' = rel_omi, 'sd1'=sd1,'sd2'=sd2,'rel_newvar'=rel_newvar,
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
             
             'imm_esc_factor_t1'=imm_esc_factor_t1, 'imm_esc_factor_omi'=imm_esc_factor_omi,'imm_esc_factor_newvar'=imm_esc_factor_newvar,
             'pi'=pi, 'rho_v1'=rho_v1, 'rho_v2' = rho_v2, 'rho_v3' =rho_v3,
             'kappa1'=kappa1, 'kappa2'=kappa2, 'kappa3'=kappa3,
             'r0'=r0, 'r0_hyp'=r0_hyp
  ) 
  
  
  
  # system.time(as.data.frame(ode(y = start, times = t, fun = COVID_vax_2strain_India, parms = params, method='ode45')))
  #model_out <- as.data.frame(ode(y = start, times = t, fun = COVID_sero_vax, parms = params, method='ode45',events=list(func=posfun, time=c(0:total_time))))
  #model_out <- as.data.frame(ode(y = start, times = t, fun = COVID_sero_vax, parms = params, method='ode45'))
  model_out <- as.data.frame(lsodar(y=start, times = t, fun = COVID_sero_vax, parms=params))
  
  #last_t <- tail(model_out, n=1)[2:859] ##get numbers in each compartment at the last time 
  last_t <- model_out[848, 2:859]
  #mod_foi <- model_out %>% select(time| contains("foi"))
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
  ) %>% 
   
    
  mutate(date =  seq(from = as.Date("2020-05-06"), to=as.Date(dt_end), by =1)) %>%
  select(time, Ecum_cr:date)

  model_out <- model_out %>% select(-contains("foi")) %>%
    select(-(contains("Ecum")|contains("Icum")))
  
  
  seroprev<- model_out %>% mutate(
    Nchildr = rowSums(select(.,contains('cr')&(-starts_with("D")))),    ## Total in the population (exclude deaths)
    Nchildu = rowSums(select(.,contains('cu')&(-starts_with("D")))),
    Nadultr = rowSums(select(.,contains('ar')&(-starts_with("D")))),
    Nadultu = rowSums(select(.,contains('au')&(-starts_with("D")))),
    Noldr =   rowSums(select(.,contains('er')&(-starts_with("D")))),
    Noldu =   rowSums(select(.,contains('eu')&(-starts_with("D")))),
    NTot = Nchildr + Nchildu +Nadultr +Nadultu +Noldr+Noldu,           ## Total alive in compartments
    
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
    
    seroprev_to = (seropos_cr+seropos_cu+seropos_ar+seropos_au+seropos_er+seropos_eu)/(NTot)) %>%
    mutate(date =  seq(from = as.Date("2020-05-06"), to=as.Date(dt_end), by =1))%>%
    select(seroprev_cr:date)
  
  seroprev_filt <- seroprev%>%filter(date%in%c(as.Date("2020-11-21"),as.Date("2021-3-1"), 
                                               as.Date("2021-6-18"),as.Date("2021-7-1"),
                                               as.Date("2021-10-31"), as.Date("2022-1-31"), 
                                               as.Date("2022-5-28"),as.Date("2022-9-1")))
    
  
  
  model_out$sweepnum <- i
  # model_summary$sweepnum <- i
  print(paste(i, "iteration complete"))
  #return(list(mod_op = model_out, start = start, params = params, last_t = last_t, mod_inc=mod_inc, seroprev=seroprev))
  return(list(start = start, params = params,last_t=last_t, seroprev=seroprev))
  
}