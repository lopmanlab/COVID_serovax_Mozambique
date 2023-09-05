file_name <- list.files(path= "/projects/blopman/vger/cliu/0_sens/",pattern=c("sw_run"))

mod_scen <- list()
for(i in 1:800){
#for(i in 1:length(file_name)){
  mod_scen[[i]]<-readRDS(paste("/projects/blopman/vger/cliu/0_sens/",file_name[i],sep=""))
  print(paste(i, "iteration complete"))
}
saveRDS(mod_scen,"/projects/blopman/vger/cliu/0_combined/sw_sens_sero.RDS")


