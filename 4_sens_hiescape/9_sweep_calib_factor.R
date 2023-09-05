library(dplyr)
library(tidyr)

sweep<- readRDS("0_sweep_sing_omega.RDS")
spec_humid <- read.csv("9_spec_humid_nyc.csv")[,c("day","avg_sm2")]

i<-1

inc_trans<-sweep$inc_trans[i]
r00<- sweep$r00[i]*inc_trans^0
r01<- sweep$r01[i]*inc_trans^1
r02<- sweep$r02[i]*inc_trans^2
r03<- sweep$r03[i]*inc_trans^3
r04<- sweep$r04[i]*inc_trans^4
r05<- sweep$r05[i]*inc_trans^5
r06<- sweep$r06[i]*inc_trans^6
r07<- sweep$r07[i]*inc_trans^7
r08<- sweep$r08[i]*inc_trans^8
r09<- sweep$r09[i]*inc_trans^9

total_time=3650            
t = seq(0,total_time,1)

tt=3650
tt2= seq(0,tt,1)
signal <- data.frame(t = tt2, 
                     yr = floor(tt2/365),
                     day = c(rep(c(seq(from=0, to=364, by=1)),times=10),364),
                     fac = rep(0, length(tt2)),
                     #r0_hyp = rep(0,length(times)),
                     r0t = rep(0, length(tt2)))

signal<- merge(signal, spec_humid, by.x="day", by.y="day")

r0hyp_list <- data.frame(yr = seq(from=0, to=10, by=1),
                         r0_hyp = c(r00,r01,r02,r03,r04,r05,r06,r07,r08,r09,r09))
#r0_hyp = c(sweep$r00[i],sweep$r01[i],sweep$r02[i],sweep$r03[i],sweep$r04[i],
#          sweep$r05[i],sweep$r06[i],sweep$r07[i],sweep$r08[i],sweep$r09[i],sweep$r09[i]))

signal<- merge(signal,r0hyp_list, by.x= "yr", by.y = "yr")
signal <- signal[order(signal$t),]

#signal$r0t <- ifelse(signal$cyc<=270/365,2,signal$r0_hyp)
r0min<-2.05
#signal$r0t= (exp(-227.5*signal$avg_sm2 + log(signal$r0_hyp-r0min))+r0min)
signal$r0t= (exp(-227.5*signal$avg_sm2 + log(signal$r0_hyp*1.35-r0min*0.9))+r0min)
#signal$r0t= exp(-227.5*signal$avg_sm2 + log((signal$r0_hyp)+4-r0min))+r0min

signal$import <- signal$r0t/sweep$r0[i]
signal%>%group_by(yr)%>%summarise(min_r0t=min(r0t),max_r0t = max(r0t),r0_hyp = max(r0_hyp))

(exp(-227.5*0.002236376 + log(5.54*1.35-2.05*0.9))+r0min)
(exp(-227.5*0.013296096 + log(5.54*1.35-2.05*0.9))+r0min)

exp(-227.5*0.002236376 + log(5.54-2.05))
exp(-227.5*0.013296096 + log(5.54-2.05))

exp(-300.5*0.002236376 + log(5.54*1.5-2.05*0.85))+2.05
exp(-300.5*0.013296096 + log(5.54*1.5-2.05*0.85))+2.05

exp(-300.5*0.002236376 + log(5.96*1.5-2.05*0.85))+2.05
exp(-300.5*0.013296096 + log(5.96*1.5-2.05*0.85))+2.05

exp(-350.5*0.002236376 + log(8.913*1.6-2.05*0.85))+2.05
exp(-350.5*0.013296096 + log(8.913*1.6-2.05*0.85))+2.05

exp(-300.5*0.002236376 + log(11.32268*1.5-2.05*0.8))+2.05
exp(-300.5*0.013296096 + log(11.32268*1.5-2.05*0.8))+2.05

exp(-227.5*0.002236376 + log(11.32268*1.5-2.05*0.8))+2.05
exp(-227.5*0.013296096 + log(11.32268*1.5-2.05*0.8))+2.05

exp(-227.5*0.002236376 + log(2-1.2))+1.2
exp(-227.5*0.013296096 + log(2-1.5))+1.2
