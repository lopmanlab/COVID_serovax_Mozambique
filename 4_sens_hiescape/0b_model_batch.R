
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

sweep_beg <- 1
sweep_end <- 4000

mod_scenarios <- foreach(i = sweep_beg:sweep_end) %dopar% {
  #mod_scenarios <- for(i in 1:num_sweep){
  library(deSolve)
  library(tidyverse)
  library(plyr)
  
  sweep<-sweep
  source("0c_model_setup.R")
  
  loop<- model_sims(i)
}
#toc()

saveRDS(mod_scenarios,"sw_hi_thresh.RDS")

