##Code below is Rscript that is executed through the bash script calib_bash.sh
##This helps us run multiple simulations in parallel, speeding up computing time

library(deSolve)
library(tidyverse)
require(plyr)
library(here)
library(foreach)
library(progressr)
library(doParallel)


rm(list=ls())
#cl <- makeCluster(8)
#registerDoParallel(cl)

##For the HPC
registerDoParallel(cores=32)
options(scipen=999)

##Edit beginning and end of sweep to execute the sweeps across the four cores

sweep_beg <-1
sweep_end <-10000

mod_scenarios <- foreach(i = sweep_beg:sweep_end) %dopar% {
#mod_scenarios <- for(i in 1:num_sweep){
  library(deSolve)
  library(tidyverse)
  library(plyr)
  
  sweep<-sweep
  source("1_model_setup.R")
  
  loop<- model_sims(i)
}


saveRDS(mod_scenarios,"sweep_calib_res.RDS")
