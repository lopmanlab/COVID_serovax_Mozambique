#The code below uses Latin hypercube sampling to randomly sample from the pre-defined parameter space for each run
#R0 from the sampled betas were calculated by identifying the dominant eigenvalue of the next generation matrix that 
#incorporates both age-specific mixing patterns and the age-specific probabilities of transmission

library(lhs) ##Needs the right RJAGs version and may not work on in the hpc for prevalence library
library(dplyr)
library(prevalence)
source("0_getR0_func.R")
set.seed(12345)
total.set.size <- 20000  ##set to large number so we can filter on those that give R0 in a reasonable range
l <- randomLHS(total.set.size, 20)

##We get the upper and lower bounds by making sure that the lower bl and upper rel infectiosness can give an R0 of 2.2
## And similarly that the upper bl and lower rel infectiousness can give an R0 of 2.2
##Effectively allowing varying relative infectiousness between the three age groups that can produce an R of between 1.8-2.2
getr0(0.011,4,4,1)
getr0(0.12,0.25,0.25,1)


#Setting bounds for drawing relative age-specific infectiousness from uniform distribution
relbeta_c_parms<- c(0.35,0.39) # relative infectiousness of children compared to older adults
relbeta_a_parms<- c(0.40,0.53) # relative infectiousness of adults compared to older adults
bl_parms <- c(0.071,0.077)     # beta for older adults

# Setting bounds for rate of waning immunity and anitbody waning
#omega_pc_parms = betaExpert(1/150,1/300,1/90, p=0.95, method="mean") #beta
#kappa1_parms = betaExpert(1/400,1/3000,1/100, p=0.95, method="mean") #beta
#kappa2_parms = betaExpert(1/1600,1/3000,1/150, p=0.95, method="mean") #beta
#kappa3_parms = betaExpert(1/2500,1/5000,1/300, p=0.95, method="mean") #beta

# Setting bounds for reduced susceptibility after 1/2 exposures
red_inf_1_parms<-c(0.3,0.6)
rel_red_inf_2_parms<-c(0.3,0.6)

#Setting bounds for increased transmissibility and immune escape of delta
rel_delta_parms = c(1.45,1.65)
imm_esc_factor_t1_parms = c(1.1,1.45)

#Setting bounds for increased transmissibility and immune escape of omicron
rel_omi_parms = c(2.8,3.8)
imm_esc_factor_omi_parms = c(1.4,1.8)

#Setting bounds for underreporting
unrep1_parms = c(40,100)
unrep2_parms = c(70,150)


relbeta_c <- round((l[,1]*(relbeta_c_parms[2]-relbeta_c_parms[1]))+relbeta_c_parms[1],5)
relbeta_a <- round((l[,2]*(relbeta_a_parms[2]-relbeta_a_parms[1]))+relbeta_a_parms[1],5)
bl        <- round((l[,3]*(bl_parms[2]-bl_parms[1]))+bl_parms[1],5)

rel_delta <- round((l[,4]*(rel_delta_parms[2]-rel_delta_parms[1]))+rel_delta_parms[1],5)
imm_esc_factor_t1 <- round((l[,5]*(imm_esc_factor_t1_parms[2]-imm_esc_factor_t1_parms[1]))+imm_esc_factor_t1_parms[1],5)

rel_omi <- round((l[,6]*(rel_omi_parms[2]-rel_omi_parms[1]))+rel_omi_parms[1],5)
imm_esc_factor_omi <- round((l[,7]*(imm_esc_factor_omi_parms[2]-imm_esc_factor_omi_parms[1]))+imm_esc_factor_omi_parms[1],5)

unrep1 <- round((l[,8]*(unrep1_parms[2]-unrep1_parms[1]))+unrep1_parms[1],5)
unrep2 <- round((l[,9]*(unrep2_parms[2]-unrep2_parms[1]))+unrep2_parms[1],5)

#omega_pc <- qbeta(l[,4],omega_pc_parms$alpha,omega_pc_parms$beta)
#kappa1 <- qbeta(l[,5],kappa1_parms$alpha,kappa1_parms$beta)
#kappa2 <- qbeta(l[,6],kappa2_parms$alpha,kappa2_parms$beta)
#kappa3 <- qbeta(l[,7],kappa3_parms$alpha,kappa3_parms$beta)

#red_inf_1 <- round((l[,8]*(red_inf_1_parms[2]-red_inf_1_parms[1]))+red_inf_1_parms[1],5)
#rel_red_inf_2 <- round((l[,9]*(rel_red_inf_2_parms[2]-rel_red_inf_2_parms[1]))+rel_red_inf_2_parms[1],5)

sweep <- data.frame(relbeta_c=relbeta_c,relbeta_a=relbeta_a, bl=bl,
                    rel_delta = rel_delta, imm_esc_factor_t1=imm_esc_factor_t1,
                    rel_omi = rel_omi, imm_esc_factor_omi = imm_esc_factor_omi,
                    unrep1=unrep1, unrep2=unrep2,
                    omega_pc = rep(1/150),
                    kappa1 = rep(1/500), kappa2=rep(1/1600), kappa3=rep(1/2500),
                    red_inf_1=0.35, rel_red_inf_2=0.43,sd1=rep(3.5),sd2=rep(3),rel_newvar=rep(3),
                    imm_esc_factor_newvar=rep(1.7))

##Figure out the estimated r0 for each combination
for(i in 1:nrow(sweep)){
  sweep$r0[i]<-getr0(bl=sweep$bl[i],rel_c = sweep$relbeta_c[i],rel_a = sweep$relbeta_a[i],rel_e=1)
}

##Select those with R0 between 1.7 and 2.2
sweep1<- sweep%>%filter(r0>=2 & r0<=2.3)
sweep1$r0_hyp<-rep(3.8)
sweep1<-sweep1%>%sample_n(4000)


##save out
saveRDS(sweep1, "0_calib_sweep.RDS")
##These leave as dummies so we don't need to change model, but really won't be used b/c time switch not turned on
