file_name <- list.files(path= "/projects/blopman/vger/cliu/0_interpol_wanehi_int/",pattern=c("sw_run"))

mod_scen <- list()
#for(i in 1:length(file_name)){
for(i in 1:1000){
  mod_scen[[i]]<-readRDS(paste("/projects/blopman/vger/cliu/0_interpol_wanehi_int/",file_name[i],sep=""))
  print(paste(i, "iteration complete"))
  
}

saveRDS(mod_scen,"/projects/blopman/vger/cliu/0_combined/sw_wanehi_int_1_1000.RDS")



#file_name <- list.files(path= "sw_int_hi/",pattern=c("sw_run"))

#mod_scen <- list()
#for(i in 1:length(file_name)){
#  mod_scen[[i]]<-readRDS(paste("sw_int_hi/",file_name[i],sep=""))
#}

#saveRDS(mod_scen,"sw_int_hi.RDS")

