library(dplyr)

sweep<- readRDS("0_sweep_standard.RDS")
sweep1 <- sweep%>%select(-sero_thresh)%>%unique()
sweep1 <- sweep1[1:25,]

sweep2 <- rbind(sweep1,sweep1,sweep1,sweep1)
sweep2$sero_thresh <- rep(c(0,0.5,0.55,0.6), each=25)
sweep2 <- sweep2%>%arrange(sweep_unique)
sweep2$r0_base <- 2.00
sweep2$trial_scen <- "hi_base200"

sweep3<- sweep2
sweep3$inc_trans<- 1
sweep3$trial_scen<-"lo_base200"


sweep4 <- sweep2
sweep4$r0_base <- 2.025
sweep4$trial_scen <- "hi_base202"

sweep5<-sweep4
sweep5$inc_trans <- 1
sweep5$trial_scen <- "lo_base202"

sweep10 <- rbind(sweep2,sweep3,sweep4,sweep5)

sweep6 <- rbind(sweep3,sweep3,sweep3,sweep3)
sweep6$inc_trans<- rep(c(1.01,1.02,1.03,1.04),each=100)
sweep6$trial_scen <- rep(c("lo101_base200","lo102_base200", "lo103_base200","lo104_base200"), each=100)

sweep10<- rbind(sweep10,sweep6)

saveRDS(sweep10, "0_sweep_recalib.RDS")

sweep<- readRDS("0_sweep_standard.RDS")
sweep1 <- sweep%>%select(-sero_thresh)%>%unique()
sweep1 <- sweep1[1:25,]

sweep2 <- rbind(sweep1,sweep1,sweep1,sweep1)
sweep2$sero_thresh <- rep(c(0,0.5,0.55,0.6), each=25)
sweep2 <- sweep2%>%arrange(sweep_unique)
sweep2$r0_base <- 2.05
sweep2$trial_scen <- "hi_base205"

saveRDS(sweep2, "0_sweep_inctrans_explo.RDS")


### reduce the first r0 for trial to help "stabilize" epidemic first
sweep<-readRDS("0_sweep_inctrans_explo.RDS")
sweep1 <- sweep%>%select(-sero_thresh)%>%unique()
sweep2 <- sweep1 %>%mutate(r00= rnorm(25, mean =2.5, sd=0.2))
sweep3 <- sweep1 %>% mutate(r00=rnorm(25, mean=3.5, sd=0.2))
sweep4 <- sweep1 %>%mutate(r00 =rnorm(25, mean =4.5, sd=0.2))

sweep5 <- rbind(sweep2,sweep3,sweep4)
sweep6 <- rbind(sweep5,sweep5)%>%
          mutate(inc_trans=rep(c(1, 1.075), each=75),
                 trial_scen = rep(c("r0025_lo","r0035_lo","r0045_lo","r0025_hi","r0035_hi","r0045_hi"), each=25))
sweep6$sero_thresh<-0

saveRDS(sweep6,"0_sweep_r00.RDS")

sweep <- readRDS("0_sweep_r00.RDS")
sweep1 <- sweep%>% filter(trial_scen%in%c("r0025_lo", "r0035_lo","r0045_lo"))
sweep2 <- sweep1 %>%
          mutate(r01 = rep(r01_1, times=3))
sweep3 <- rbind(sweep1,sweep2)
saveRDS(sweep3,"0_sweep_r00_r2.RDS")

r01_1 <- rnorm(25,4.5,0.2)

sweep3 <- readRDS("0_sweep_r00_r2.RDS")
sweep8<-sweep3[51:75,]
sweep8$scen_trial<- rep("r045_r155")
sweep10 <- rbind(sweep8,sweep8,sweep8,sweep8,sweep8,sweep8,sweep8)
sweep10$sero_thresh<- rep(c(0.5,0.55,0.6,0.65,0.7,0.75,0.8),each=25)

sweep9<-sweep3[76:100,]
sweep9$scen_trial<- rep("r025_r045")
sweep11 <- rbind(sweep9,sweep9,sweep9,sweep9,sweep9,sweep9,sweep9)
sweep11$sero_thresh<- rep(c(0.5,0.55,0.6,0.65,0.7,0.75,0.8),each=25)

sweep12 <- rbind(sweep10,sweep11)
saveRDS(sweep12, "0_sweep_r0_r1_vax.RDS")

sweep9<-sweep3[126:150,]
sweep9$scen_trial<- rep("r045_r045")
sweep11 <- rbind(sweep9,sweep9,sweep9,sweep9,sweep9,sweep9,sweep9)
sweep11$sero_thresh<- rep(c(0.5,0.55,0.6,0.65,0.7,0.75,0.8),each=25)
saveRDS(sweep11, "0_sweep_r0_r1_vax2.RDS")

sweep99<-readRDS("0_sweep_r0_r1_vax2.RDS")

sweep99<- sweep99%>%select(-sero_thresh)%>%unique()

sweep98<-readRDS("0_sweep_r0_r1_vax.RDS")
sweep98<- sweep98%>%select(-sero_thresh)%>%unique()

sweep1<- rbind(sweep98,sweep99)
sweep2 <- rbind(sweep1,sweep1)
sweep2$vax_int <- rep(c(365,730), each=75)
sweep2$vax_start<-rep(c(300/365,300/730),each=75)
sweep2$vax_end <- rep(c(330/365,330/730),each=75)

saveRDS(sweep2, "3_sweep_vax.RDS")

###Third round
sweep3 <- readRDS("0_sweep_r00_r2.RDS")
sweep4 <- sweep3[126:150,]
sweep4<- sweep4 %>%mutate(
         r02 =rnorm(25, mean =4.5, sd=0.2),
         r03 =rnorm(25, mean =4.5, sd=0.2),
         r04 =rnorm(25, mean =4.5, sd=0.2),
         r05 =rnorm(25, mean =4.5, sd=0.2),
         r06 =rnorm(25, mean =4.5, sd=0.2),
         r07 =rnorm(25, mean =4.5, sd=0.2),
         r08 =rnorm(25, mean =4.5, sd=0.2),
         r09 =rnorm(25, mean =4.5, sd=0.2))

sweep4all <- rbind(sweep4, sweep4,sweep4,sweep4,sweep4,sweep4,sweep4,sweep4)
sweep4all$sero_thresh <- rep(c(0,0.5,0.55,0.6,0.65,0.7,0.75,0.8),each=25)

sweep5<- sweep3[126:150,]
sweep5<- sweep5 %>%mutate(
              r02 =rnorm(25, mean =5, sd=0.2),
              r03 =rnorm(25, mean =5, sd=0.2),
              r04 =rnorm(25, mean =5, sd=0.2),
              r05 =rnorm(25, mean =5, sd=0.2),
              r06 =rnorm(25, mean =5, sd=0.2),
              r07 =rnorm(25, mean =5, sd=0.2),
              r08 =rnorm(25, mean =5, sd=0.2),
              r09 =rnorm(25, mean =5, sd=0.2))
sweep5all <- rbind(sweep5,sweep5,sweep5,sweep5,sweep5,sweep5,sweep5,sweep5)
sweep5all$sero_thresh <- rep(c(0,0.5,0.55,0.6,0.65,0.7,0.75,0.8),each=25)

sweep99 <- rbind(sweep4all,sweep5all)
sweep99$trial_scen<- rep(c("r02_45","r02_50"),each=200)
sweep99<- sweep99%>%arrange(sweep_unique)

saveRDS(sweep99, "0_sweep_r3.RDS")

sweep<- readRDS("0_sweep_r3.RDS")
sweep1<- sweep%>%select(-sero_thresh)%>%unique()
sweep2 <- rbind(sweep1,sweep1)
sweep2$vax_int <- rep(c(365,730), each=50)
sweep2$vax_start <- rep(c(300/365,300/730), each=50)
sweep2$vax_end <- rep(c(330/365,330/730),each=50)
sweep2<- sweep2%>%arrange(trial_scen)
saveRDS(sweep2, "3_sweep_r3_int.RDS")

sweep <- readRDS("0_sweep_r3.RDS")
sweep1 <- sweep%>%filter(trial_scen=="r02_50")
sweep2<- sweep1 %>%select(-sero_thresh)%>%unique()

sweep3 <- rbind(sweep2,sweep2,sweep2)
sweep4 <- rbind(sweep2,sweep2,sweep2,sweep2)
sweep5<- rbind(sweep4,sweep4,sweep4,sweep4)
rm(sweep4)
sweep5 <- rbind(sweep3,sweep5)
sweep5<- sweep5 %>%mutate(
              r00 =rnorm(475, mean =4.5, sd=0.2),
              r01 =rnorm(475, mean=4.5, sd=0.2),
              r02 =rnorm(475, mean =5, sd=0.2),
              r03 =rnorm(475, mean =5, sd=0.2),
              r04 =rnorm(475, mean =5, sd=0.2),
              r05 =rnorm(475, mean =5, sd=0.2),
              r06 =rnorm(475, mean =5, sd=0.2),
              r07 =rnorm(475, mean =5, sd=0.2),
              r08 =rnorm(475, mean =5, sd=0.2),
              r09 =rnorm(475, mean =5, sd=0.2))
sweep5$sweep_unique <- seq(from=26, to=500, by=1)

sweep6 <- rbind(sweep5,sweep5,sweep5,sweep5,sweep5,sweep5,sweep5,sweep5)%>%
          mutate(sero_thresh = rep(c(0,0.5,0.55,0.6,0.65,0.7,0.75,0.8), each=475))

sweep7<-rbind(sweep1,sweep6)
sweep7$kappa2 <-1/2500
sweep7<- sweep7%>%arrange(sweep_unique)
saveRDS(sweep7, "0_sweep_sero.RDS")

sweep1<- sweep7%>%select(-sero_thresh)%>%unique()
sweep2 <- rbind(sweep1,sweep1)
sweep2$vax_int <- rep(c(365,730), each=500)
sweep2$vax_start <- rep(c(300/365,300/730), each=500)
sweep2$vax_end <- rep(c(330/365,330/730),each=500)
sweep2<- sweep2%>%arrange(sweep_unique)
saveRDS(sweep2, "1_sweep_int.RDS")


sweep_int <- readRDS("1_sweep_int.RDS")
sweep_int1 <- rbind(sweep_int,sweep_int,sweep_int,sweep_int,sweep_int,sweep_int,sweep_int)
sweep_int1$vax_first <- rep(c(1667,1356,1132,822,436,255,109),each=1000)
sweep_int1$sero_match <- rep(c("50%","55%","60%","65%","70%","75%","80%"), each =1000)
sweep_int1$vax_start <- rep(c(206/365,260/365,36/365,91/365,70/365,254/365,108/265), each=1000)
sweep_int1$vax_end <- rep(c(236/365,290/365,66/365,121/365,100/365,284/365,138/265), each=1000)

saveRDS(sweep_int1, "1_sweep_int_firstvar.RDS")

sweep<- readRDS("0_sweep_sero.RDS")
sweep<- sweep%>%select(c(bl:rel_red_inf_2,add_imm:r0_base))

st <-as.data.frame(matrix(sample(2:365, size=5000, replace=T),ncol=10,nrow=500))
colnames(st)<-c("st0","st1","st2","st3","st4","st5","st6","st7","st8","st9")
st <- st%>%mutate(sweep_unique=1:500)
st_rep <- rbind(st,st,st,st,st,st,st,st)
sweep2<- cbind(sweep, st_rep)
sweep2<- sweep2%>%mutate(sero_thresh = rep(c(0,0.5,0.55,0.6,0.65,0.7,0.75,0.8), each =500))
sweep2<-sweep2%>%arrange(sweep_unique,sero_thresh)

saveRDS(sweep2, "0_sweep_sero.RDS")


sweep<- readRDS("0_sweep_sero.RDS")
##Get the first 100 runs
sweep1<- sweep%>%select(-kappa1,-kappa2,-kappa3)
sweep1<- sweep1%>%unique()%>%arrange(sero_thresh)
sweep2<-rbind(sweep1,sweep1)
sweep2$kappa1<-rep(c(1/3000,1/3500),each =100)
sweep2$kappa2<-rep(c(1/3000,1/3500),each =100)
sweep2$kappa3<-rep(c(1/3000,1/3500),each =100)

sweep3<-rbind(sweep,sweep2)
saveRDS(sweep3, "0_sweep_sero.RDS")
sweep4<-rbind(sweep2,sweep2)

sweep4$vax_int <-rep(c(365,730),each =600)
sweep4$vax_start <- rep(c(300/365,300/730),each=600)
sweep4$vax_end <- rep(c(330/365,330/730),each=600)
