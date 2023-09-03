##Calculating r0 from next generation matrix
getr0 <- function(bl,rel_c,rel_a,rel_e){
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
  #p_ru <- 0.008 # Daily prob of urban contact made by rural people/all contacts made by rural
  #p_ur <- 0.005 # Daily ptob of rural contact made by urban people/all contacts made by urban
  p_ru <- 0.05 # Prop of urban contact made by rural people/all contacts made by rural
  p_ur <- 0.01 # Prop of rural contact made by urban people/all contacts made by urban
  #Attempt to create a 6x6 matrix incorporating a dimension of urban/rural
  
  CM_rr <- (1-p_ru)*CM_r
  CM_ru <- (p_ru)*CM_r
  CM_uu <- (1-p_ur)*CM_u
  CM_ur <- p_ur*CM_u
  
  CM <- cbind(rbind(CM_rr, CM_ur), rbind(CM_ru,CM_uu))
  colnames(CM) <- c("sus_r_0_17","sus_r_18_49","sus_r_50","sus_u_0_17","sus_u_18_49","sus_u_50")
  rownames(CM) <- c("inf_r_0_17","inf_r_18_49","inf_r_50","inf_u_0_17","inf_u_18_49","inf_u_50")
  
  ##population
  start.Ns <- c(10884513, 4883969, 7958844, 4900719, 992149, 446454)
  
  pop_dist = data.frame(age_ur = c("cr","cu","ar","au","er","eu"),
                        pop = start.Ns,
                        sus_pop = c("sus_r_0_17","sus_u_0_17","sus_r_18_49","sus_u_18_49","sus_r_50","sus_u_50"))
  
  alpha <- 0.6
  nu_c<- 0.45         #Probability of symptomatic infection for children
  nu_a<- 0.55         #Probability of symptomatic infection for adults
  nu_e<- 0.65         #Probability of symptomatic infection for elderly
  
  prop_inf_c = nu_c+(1-nu_c)*alpha
  prop_inf_a = nu_a + (1-nu_a)*alpha
  prop_inf_e = nu_e +(1-nu_e)*alpha
  
  #rel_c<-1
  #rel_a<-1
  #rel_e<-1
  
  beta_c <- bl*rel_c
  beta_a <- bl*rel_a    
  beta_e <- bl*rel_e    
  ## vector of infectiousness
  v_sus<- data.frame(var = c("sus_r_0_17","sus_r_18_49","sus_r_50","sus_u_0_17","sus_u_18_49","sus_u_50"),
                     beta = c(beta_c, beta_a, beta_e, beta_c, beta_a, beta_e))
  v_inf<- data.frame(inf = row.names(CM),
                     prop_infector = c(prop_inf_c, prop_inf_a, prop_inf_e,prop_inf_c,prop_inf_a,prop_inf_e))
  
  ngm <- as.data.frame(CM)%>% mutate(inf=row.names(.))%>%tidyr::pivot_longer(cols=sus_r_0_17:sus_u_50, names_to = "sus") %>%
    left_join(v_sus, by = c("sus"="var")) %>%
    left_join(v_inf, by= c("inf"="inf")) %>%
    left_join(pop_dist %>% select(sus_pop,pop), by = c("sus"="sus_pop"))%>%
    mutate(r0 = value*beta*prop_infector*7)
  
  m_ngm<-ngm %>% select(-value,-beta,-prop_infector,-pop)%>%tidyr::pivot_wider(names_from="sus",values_from="r0")%>%select(-inf)
  
  ##Dominant eigenvalue =1.29
  eigen<-max(eigen(as.matrix(m_ngm))$values)
  print(eigen)
}
