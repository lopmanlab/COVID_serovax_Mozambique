
##Code below is Rscript that is executed through the bash script calib_bash.sh

library(deSolve)
library(tidyverse)
require(plyr)
library(here)
library(foreach)
library(progressr)
library(doParallel)
library(tictoc)

#tic()

rm(list=ls())
#cl <- makeCluster(8)
#registerDoParallel(cl)

##For the HPC
registerDoParallel(cores=32)
#options(scipen=999)

##Edit beginning and end of sweep to execute the sweeps across the four cores

sweep_beg <- 727
sweep_end <- 800

mod_scenarios <- foreach(i = sweep_beg:sweep_end) %dopar% {
  #mod_scenarios <- for(i in 1:num_sweep){
  library(deSolve)
  library(tidyverse)
  library(plyr)
  
  sweep<-sweep
  source("0_mod_sim.R")
  
  loop<- model_sims(i)
}
#toc()

#saveRDS(mod_scenarios,"/projects/blopman/vger/cliu/sw_int_hi_adjr0_inctrans106.RDS")

