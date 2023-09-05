file_name <- list.files(path= "/projects/blopman/vger/cliu/0_sens_kappa/",pattern=c("sw_run"))

mod_scen <- list()
for(i in 1:1000){
#for(i in 1:length(file_name)){
  mod_scen[[i]]<-readRDS(paste("/projects/blopman/vger/cliu/0_sens_kappa/",file_name[i],sep=""))
  print(paste(i, "iteration complete"))
}
saveRDS(mod_scen,"/projects/blopman/vger/cliu/0_combined/sw_sens_kappa_sero2.RDS")


