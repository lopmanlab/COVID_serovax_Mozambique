library(dplyr)
library(ggplot2)

mod_res_list <- list()
mod_inc_list<- list()

##Model initials
dt_end=as.Date("2022-4-1")
tot_pop <- 30066648   ## Took population for 2020 from Mozambique INE (Institute of statistics)
p_urban <- 0.34 ##From INE
p_rural <- 1-p_urban
start.Ns <- c(10884513, 4883969, 7958844, 4900719, 992149, 446454)
dist <- start.Ns/tot_pop

pop_dist = data.frame(age_ur = c("cr","cu","ar","au","er","eu"),
                      pop = start.Ns)


#for (i in 1:length(list_file)){

for(i in 1:length(mod_scenarios)){
  #Read in the model outputs
  
 mod_res <- mod_scenarios[[i]]$mod_op
 mod_parms <- mod_scenarios[[i]]$params
 mod_res <- mod_res%>%select(-date)
 mod_foi <- mod_scenarios[[i]]$mod_foi
 mod_inc <- mod_scenarios[[i]]$mod_inc
 
 
  #mod_res <- mod_scenarios[[1]]
  #mod_foi <- mod_res %>% select(time| contains("foi"))
  #mod_inc <- mod_res %>% select(time| contains("Ecum")|contains("Icum"))
  
  #mod_res <- mod_res %>% select(-contains("foi")) %>%
  #  select(-(contains("Ecum")|contains("Icum")))
  
  ## Sum different compartments
  mod_res <- mod_res %>% mutate(
    
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
    
    new_Deaths_tot = Deaths_tot - lag(Deaths_tot),
    
    
    Deaths_v0 = rowSums(select(.,starts_with("D")&ends_with("v0"))),
    Deaths_v1 = rowSums(select(.,starts_with("D")&ends_with("v1"))),
    Deaths_v2 = rowSums(select(.,starts_with("D")&ends_with("v2"))),
    Deaths_v3 = rowSums(select(.,starts_with("D")&ends_with("v3"))),
    
    Deaths_e1 = rowSums(select(.,contains("D")&contains("1v"))),
    Deaths_e2 = rowSums(select(.,contains("D")&contains("2v"))),
    Deaths_e3 = rowSums(select(.,contains("D")&contains("3v"))),
    
    V0cum = rowSums(select(.,contains("v0")&(-starts_with("D")))),
    V1cum = rowSums(select(.,contains("v1")&(-starts_with("D")))),
    V2cum = rowSums(select(.,contains("v2")&(-starts_with("D")))),
    V3cum = rowSums(select(.,contains("v3")&(-starts_with("D")))),
  
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
    V3cum_e = rowSums(select(.,contains("v3")&(contains("er")|contains("eu"))&(-starts_with("D")))),

    V0cum_cr = rowSums(select(.,contains("v0")&(contains("cr"))&(-starts_with("D")))),
    V1cum_cr = rowSums(select(.,contains("v1")&(contains("cr"))&(-starts_with("D")))),
    V2cum_cr = rowSums(select(.,contains("v2")&(contains("cr"))&(-starts_with("D")))),
    V3cum_cr = rowSums(select(.,contains("v3")&(contains("cr"))&(-starts_with("D")))),
    
    V0cum_cu = rowSums(select(.,contains("v0")&(contains("cu"))&(-starts_with("D")))),
    V1cum_cu = rowSums(select(.,contains("v1")&(contains("cu"))&(-starts_with("D")))),
    V2cum_cu = rowSums(select(.,contains("v2")&(contains("cu"))&(-starts_with("D")))),
    V3cum_cu = rowSums(select(.,contains("v3")&(contains("cu"))&(-starts_with("D")))),
    
    V0cum_ar = rowSums(select(.,contains("v0")&(contains("ar"))&(-starts_with("D")))),
    V1cum_ar = rowSums(select(.,contains("v1")&(contains("ar"))&(-starts_with("D")))),
    V2cum_ar = rowSums(select(.,contains("v2")&(contains("ar"))&(-starts_with("D")))),
    V3cum_ar = rowSums(select(.,contains("v3")&(contains("ar"))&(-starts_with("D")))),
    
    V0cum_er = rowSums(select(.,contains("v0")&(contains("er"))&(-starts_with("D")))),
    V1cum_er = rowSums(select(.,contains("v1")&(contains("er"))&(-starts_with("D")))),
    V2cum_er = rowSums(select(.,contains("v2")&(contains("er"))&(-starts_with("D")))),
    V3cum_er = rowSums(select(.,contains("v3")&(contains("er"))&(-starts_with("D")))),
    
    V0cum_au = rowSums(select(.,contains("v0")&(contains("au"))&(-starts_with("D")))),
    V1cum_au = rowSums(select(.,contains("v1")&(contains("au"))&(-starts_with("D")))),
    V2cum_au = rowSums(select(.,contains("v2")&(contains("au"))&(-starts_with("D")))),
    V3cum_au = rowSums(select(.,contains("v3")&(contains("au"))&(-starts_with("D")))),
    
    V0cum_eu = rowSums(select(.,contains("v0")&(contains("eu"))&(-starts_with("D")))),
    V1cum_eu = rowSums(select(.,contains("v1")&(contains("eu"))&(-starts_with("D")))),
    V2cum_eu = rowSums(select(.,contains("v2")&(contains("eu"))&(-starts_with("D")))),
    V3cum_eu = rowSums(select(.,contains("v3")&(contains("eu"))&(-starts_with("D"))))
    
    
  )
  
  ## Seroprevalence
  #mod_res <- mod_res %>% mutate(
  #  SpRp_cr = rowSums(select(., contains('cr')& (contains('Sp')|contains('Rp')|contains('Vp')))),
  #  SpRp_cu = rowSums(select(., contains('cu')& (contains('Sp')|contains('Rp')|contains('Vp')))),
  #  SpRp_ar = rowSums(select(., contains('ar')& (contains('Sp')|contains('Rp')|contains('Vp')))),
  #  SpRp_au = rowSums(select(., contains('au')& (contains('Sp')|contains('Rp')|contains('Vp')))),
  #  SpRp_er = rowSums(select(., contains('er')& (contains('Sp')|contains('Rp')|contains('Vp')))),
  #  SpRp_eu = rowSums(select(., contains('eu')& (contains('Sp')|contains('Rp')|contains('Vp')))),
    
  #  seroneg = rowSums(select(., contains('Sn')|contains('Rn'))),
    
  #  active_cr = rowSums(select(.,(contains('Ecr')|contains('Acr')|contains('Icr')|contains('Hcr')), -ends_with('1v0'))),
  #  active_cu = rowSums(select(.,(contains('Ecu')|contains('Acu')|contains('Icu')|contains('Hcu')), -ends_with('1v0'))),
  #  active_ar = rowSums(select(.,(contains('Ear')|contains('Aar')|contains('Iar')|contains('Har')), -ends_with('1v0'))),
  #  active_au = rowSums(select(.,(contains('Eau')|contains('Aau')|contains('Iau')|contains('Hau')), -ends_with('1v0'))),
  #  active_er = rowSums(select(.,(contains('Eer')|contains('Aer')|contains('Ier')|contains('Her')), -ends_with('1v0'))),
  #  active_eu = rowSums(select(.,(contains('Eeu')|contains('Aeu')|contains('Ieu')|contains('Heu')), -ends_with('1v0'))),
    
  #  seropos_cr = SpRp_cr + active_cr,
  #  seropos_cu = SpRp_cu + active_cu,
  #  seropos_ar = SpRp_ar + active_ar,
  #  seropos_au = SpRp_au + active_au,
  #  seropos_er = SpRp_er + active_er,
  #  seropos_eu = SpRp_eu + active_eu,
    
  #  seroprev_cr = seropos_cr/pop_dist$pop[1],
  #  seroprev_cu = seropos_cu/pop_dist$pop[2],
  #  seroprev_ar = seropos_ar/pop_dist$pop[3],
  #  seroprev_au = seropos_au/pop_dist$pop[4],
  #  seroprev_er = seropos_er/pop_dist$pop[5],
  #  seroprev_eu = seropos_eu/pop_dist$pop[6],
    
  #  seroprev_c = (seropos_cr+seropos_cu)/(pop_dist$pop[1]+pop_dist$pop[2]),
  #  seroprev_a = (seropos_ar+seropos_au)/(pop_dist$pop[3]+pop_dist$pop[4]),
  #  seroprev_e = (seropos_er+seropos_eu)/(pop_dist$pop[5]+pop_dist$pop[6]),
    
  #  seroprev_ul = (seropos_cu+seropos_au+seropos_eu)/(pop_dist$pop[2]+pop_dist$pop[4]+pop_dist$pop[6]),
  #  seroprev_rl = (seropos_cr+seropos_ar+seropos_er)/(pop_dist$pop[1]+pop_dist$pop[3]+pop_dist$pop[5]),
    
  #  seroprev_to = (seropos_cr+seropos_cu+seropos_ar+seropos_au+seropos_er+seropos_eu)/(sum(pop_dist$pop))
    
  #)
  
  
  
  mod_res <- mod_res %>%mutate(
    omega_pc = mod_parms[["omega_pc"]], ##Get parm values from a named vector
    kappa1 = mod_parms[["kappa1"]],
    kappa2 =mod_parms[["kappa2"]],
    kappa3 = mod_parms[["kappa3"]])
  
  mod_res_list[[i]] <- mod_res %>% left_join(mod_inc, by = "time") %>%
                        left_join(mod_foi, by ="time")%>%
                        mutate(date =  seq(from = as.Date("2020-05-06"), to=as.Date(dt_end), by =1)) 
  
}


