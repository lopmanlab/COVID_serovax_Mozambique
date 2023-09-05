sweep<- readRDS("0_sweep_R_rand_inctrans_thresh.RDS")
sweep$sero_thresh<-0
sweep1 <- unique(sweep)
sweep1<- sweep1[1:8,]

sweep1$beta1 <-0.1
sweep1$w <- 1
saveRDS(sweep1, "0_sweep1.RDS")

sweep1 <- rbind(sweep1,sweep1,sweep1,sweep1,sweep1,sweep1,sweep1,sweep1)
sweep1$w <-c(0,(2*pi)/8,(2*pi)/4,(3/8)*(2*pi),(2*pi)/2,(5/8)*(2*pi),(6/8)*(2*pi),(7/8)*(2*pi))
saveRDS(sweep1,"0_sweep.RDS")

sweep1<-sweep[1:700,]
sweep1<- sweep1%>%select(-sero_thresh)
sweep1 <- unique(sweep1)
sweep2 <- rbind(sweep1,sweep1,sweep1,sweep1,sweep1,sweep1,sweep1,sweep1)
sweep2$sero_thresh <- rep(c(0,0.5,0.55,0.6,0.65,0.7,0.75,0.8),each=500)

sweep2 <- sweep2%>%arrange(sweep_unique, sero_thresh)
sweep2 <- readRDS("0_sweep_standard.RDS")

sweep2 <- rbind(sweep1,sweep1)
sweep2$fac1 <--275.5
sweep2$fac2 <- 1.4
sweep2$inc_trans2<-1.075
sweep2$vax_int <- rep(c(365, 730), each=500)
sweep2$vax_start <- rep(c(300/365, 300/730), each=500)
sweep2$vax_end <- rep(c(330/365, 330/730), each=500)

saveRDS(sweep2, "1_sweep_standard_int.RDS")

saveRDS(sweep2,"0_sweep_standard.RDS")

sweep<-readRDS("0_sweep_seas_hi.RDS")
sweep1 <- sweep[1:100,]
sweep2 <- rbind(sweep1,sweep1)
sweep2$vax_int <- rep(c(365,730),each=100)
sweep2$vax_start <- rep(c(300/365,300/730), each=100)
sweep2$vax_end <- rep(c(330/365,330/730), each=100)

saveRDS(sweep2,"0_sweep_seas_hi_int.RDS")
sweep <- readRDS("0_sweep_sing_omega.RDS")
sweep$inc_trans <- 1.075

saveRDS(sweep, "0_sweep_sing_omega.RDS")
sweep<- readRDS("0_sweep_sing_omega.RDS")

sweep2 <- sweep[1:8,]
sweep3 <- rbind(sweep2,sweep2,sweep2,sweep2)
sweep4 <- rbind(sweep3,sweep3,sweep3)
sweep4$fac1 <-rep(c(-227.5,-250.5,-275.5,-300.5),each=24)

fac2 <-  rep(c(1.4,1.5,1.6), each=8)
fac2 <- c(fac2,fac2,fac2,fac2)
sweep4$fac2 <- fac2
vals <-expand.grid(fac1=c(-227.5,-250.5,-275.5,-300.5),
            fac2 =c(1.4,1.5,1.6))  

saveRDS(sweep4,"0_sweep_adj_R0.RDS")
sweep4 <- readRDS("0_sweep_adj_r0.RDS")
sweep4$inc_trans <- 1.065

saveRDS(sweep4, "0_sweep_adj_R0_inctrans1065.RDS")

sweep1<- readRDS("0_sweep_adj_R0_inctrans1065.RDS")
sweep4$inc_trans<- 1.07
saveRDS(sweep4,"0_sweep_adj_R0_inctrans107.RDS")

sweep4$inc_trans<- 1.06
saveRDS(sweep4,"0_sweep_adj_R0_inctrans106.RDS")