file_name <- list.files(path= "/projects/blopman/vger/cliu/0_interpol_wanehi/",pattern=c("sw_run"))

mod_scen <- list()
for(i in 601:3800){
#for(i in 1:length(file_name)){
  mod_scen[[i]]<-readRDS(paste("/projects/blopman/vger/cliu/0_interpol_wanehi/",file_name[i],sep=""))
  print(paste(i, "iteration complete"))
}

saveRDS(mod_scen,"/projects/blopman/vger/cliu/0_combined/sw_hi_thresh_800_3800.RDS")



#file_name <- list.files(path= "sw_int_hi/",pattern=c("sw_run"))

#mod_scen <- list()
#for(i in 1:length(file_name)){
#  mod_scen[[i]]<-readRDS(paste("sw_int_hi/",file_name[i],sep=""))
#}

#saveRDS(mod_scen,"sw_int_hi.RDS")

