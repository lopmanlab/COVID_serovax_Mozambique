## Mixing matrix
## The columns are contact age group (infectee) whereas the rows are participant age groups (infector)
## Flipped from the visualizations
#CM_u <- matrix(c(3.99,3.204,1.603,2.384, 4.316, 2.410,0.651,1.316,1.472 ),
#               nrow = 3,
#               dimnames = list(x=c("0-17","18-59","50+"), y=c("0-17","18-59","50+")))

#CM_r <- matrix(c(6.889, 4.328, 2.313, 2.766, 5.812,3.179,0.724,1.558, 2.112), 
#               nrow = 3, 
#               dimnames = list(x=c("0-17","18-59","50+"), y=c("0-17","18-59","50+")))

CM_u <- matrix(c(4.369811, 3.579099, 3.581121,
                2.689841, 3.910767, 4.198945,
                 0.6558402,1.0232138,1.5153399),
               nrow = 3,
               dimnames = list(x=c("0-17","18-49","50+"), y=c("0-17","18-49","50+")))

CM_r <- matrix(c(5.718741, 3.954180, 4.162091,
                 2.971730, 4.720997, 5.519682,
                 0.7622381,1.3450557,2.2524737), 
               nrow = 3, 
               dimnames = list(x=c("0-17","18-49","50+"), y=c("0-17","18-49","50+")))



## Travel probabilities
p_ru <- 0.05 # Prop of urban contact made by rural people/all contacts made by rural
p_ur <- 0.01 # Prop of rural contact made by urban people/all contacts made by urban
#p_ru <- 0.008 # Daily prob of urban contact made by rural people/all contacts made by rural
#p_ur <- 0.005 # Daily ptob of rural contact made by urban people/all contacts made by urban
#Attempt to create a 6x6 matrix incorporating a dimension of urban/rural

CM_rr <- (1-p_ru)*CM_r
CM_ru <- (p_ru)*CM_r
CM_uu <- (1-p_ur)*CM_u
CM_ur <- p_ur*CM_u

CM <- cbind(rbind(CM_rr, CM_ur), rbind(CM_ru,CM_uu))
colnames(CM) <- c("Rural 0-17","Rural 18-49","Rural 50+","Urban 0-17","Urban 18-49","Urban 50+")
rownames(CM) <-c("Rural 0-17","Rural 18-49","Rural 50+","Urban 0-17","Urban 18-49","Urban 50+")


##Visualize the matrix

theme<-theme_classic()+
  theme(plot.title = element_text(hjust=0.5, face="bold",size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        panel.border=element_blank(),
        axis.text=element_text(size=11),
        axis.text.x=element_text(angle=45,hjust=1))

#png("code/figs/contact_matrix.png",width = 9,height = 7,units="in",res=200)
#as.data.frame(CM) %>%
#  mutate(from_var = row.names(CM)) %>%
#  pivot_longer(cols='Rural 0-17':'Urban 50+', names_to= "to_var") %>%
#  ggplot(aes(x = from_var, y = to_var, fill = value)) + 
#  scale_fill_gradient2(low = "white", high = "#273871", mid = "#7FABD3", midpoint = 4, limit =c(0,8),name="Mean contact rate")+
#  xlab("Age/urbanicity of infectious")+ylab("Age/urbanicity of susceptible")+
#  geom_tile()+ 
#  geom_text(aes(label=round(value, digits=2)), colour = "black", check_overlap = TRUE)+
#  theme(legend.position = "bottom") + theme + ggtitle("")
#dev.off()