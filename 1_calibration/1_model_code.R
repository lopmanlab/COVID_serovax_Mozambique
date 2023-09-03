source("0_mixing_matrix_gmix.R")

## Model flows
## The subsaripts denote the number of times someone as been exposed through either infection or vaccination
COVID_sero_vax <- function(t, x, parms){
  #####Unvaccinated####
  # Unvaccinated and unexposed
  Scr0v0 =x[1];  Scu0v0 =x[2];  Sar0v0 =x[3];  Sau0v0 =x[4];  Ser0v0 =x[5];  Seu0v0 =x[6] #S0
  Ecr1v0 =x[7];  Ecu1v0 =x[8];  Ear1v0 =x[9];  Eau1v0 =x[10]; Eer1v0 =x[11]; Eeu1v0 =x[12] #E1
  Acr1v0 =x[13]; Acu1v0 =x[14]; Aar1v0 =x[15]; Aau1v0 =x[16]; Aer1v0 =x[17]; Aeu1v0 =x[18] #A1
  Icr1v0 =x[19]; Icu1v0 =x[20]; Iar1v0 =x[21]; Iau1v0 =x[22]; Ier1v0 =x[23]; Ieu1v0 =x[24] #I1
  Hcr1v0 =x[25]; Hcu1v0 =x[26]; Har1v0 =x[27]; Hau1v0 =x[28]; Her1v0 =x[29]; Heu1v0 =x[30] #H1
  Rpcr1v0=x[31]; Rpcu1v0=x[32]; Rpar1v0=x[33]; Rpau1v0=x[34]; Rper1v0=x[35]; Rpeu1v0=x[36] #Immune after first exposure and seropositive
  Rncr1v0=x[37]; Rncu1v0=x[38]; Rnar1v0=x[39]; Rnau1v0=x[40]; Rner1v0=x[41]; Rneu1v0=x[42] #Immune after first exposure and seronegative
  Dcr1v0 =x[43]; Dcu1v0 =x[44]; Dar1v0 =x[45]; Dau1v0 =x[46]; Der1v0 =x[47]; Deu1v0 =x[48] #D1
  
  # Unvaccinated and exposed once through infection
  Spcr1v0=x[49]; Spcu1v0=x[50]; Spar1v0=x[51]; Spau1v0=x[52]; Sper1v0=x[53];  Speu1v0=x[54] #S1 part sussceptible after first exposure & seropositive
  Sncr1v0=x[55]; Sncu1v0=x[56]; Snar1v0=x[57]; Snau1v0=x[58]; Sner1v0=x[59];  Sneu1v0=x[60] #S1 part sussceptible after first exposure & seronegative
  Ecr2v0 =x[61]; Ecu2v0 =x[62]; Ear2v0 =x[63]; Eau2v0 =x[64]; Eer2v0 =x[65];  Eeu2v0 =x[66] #E2 infection among those with first prior exposure
  Acr2v0 =x[67]; Acu2v0 =x[68]; Aar2v0 =x[69]; Aau2v0 =x[70]; Aer2v0 =x[71];  Aeu2v0 =x[72] #A2
  Icr2v0 =x[73]; Icu2v0 =x[74]; Iar2v0 =x[75]; Iau2v0 =x[76]; Ier2v0 =x[77];  Ieu2v0 =x[78] #I2
  Hcr2v0 =x[79]; Hcu2v0 =x[80]; Har2v0 =x[81]; Hau2v0 =x[82]; Her2v0 =x[83];  Heu2v0 =x[84] #H2
  Rpcr2v0=x[85]; Rpcu2v0=x[86]; Rpar2v0=x[87]; Rpau2v0=x[88]; Rper2v0=x[89];  Rpeu2v0=x[90] #Rp2 Immune after second exposure and seropos
  Rncr2v0=x[91]; Rncu2v0=x[92]; Rnar2v0=x[93]; Rnau2v0=x[94]; Rner2v0=x[95];  Rneu2v0=x[96] #Rn2 Immune after second exposure and seroneg
  Dcr2v0 =x[97]; Dcu2v0 =x[98]; Dar2v0 =x[99]; Dau2v0 =x[100];Der2v0 =x[101]; Deu2v0=x[102] #D2
  
  #Unvaccinated and exposed twice through infection
  Spcr2v0=x[103]; Spcu2v0=x[104]; Spar2v0=x[105]; Spau2v0=x[106]; Sper2v0=x[107]; Speu2v0=x[108] #S2 part sussceptible after second exposure & seropositive
  Sncr2v0=x[109]; Sncu2v0=x[110]; Snar2v0=x[111]; Snau2v0=x[112]; Sner2v0=x[113]; Sneu2v0=x[114] #S2 part sussceptible after second exposure & seronegative
  Ecr3v0 =x[115]; Ecu3v0 =x[116]; Ear3v0 =x[117]; Eau3v0 =x[118]; Eer3v0 =x[119]; Eeu3v0 =x[120] #E3 infection among those with second prior exposure
  Acr3v0 =x[121]; Acu3v0 =x[122]; Aar3v0 =x[123]; Aau3v0 =x[124]; Aer3v0 =x[125]; Aeu3v0 =x[126] #A3
  Icr3v0 =x[127]; Icu3v0 =x[128]; Iar3v0 =x[129]; Iau3v0 =x[130]; Ier3v0 =x[131]; Ieu3v0 =x[132] #I3
  Hcr3v0 =x[133]; Hcu3v0 =x[134]; Har3v0 =x[135]; Hau3v0 =x[136]; Her3v0 =x[137]; Heu3v0 =x[138] #H3
  Rpcr3v0=x[139]; Rpcu3v0=x[140]; Rpar3v0=x[141]; Rpau3v0=x[142]; Rper3v0=x[143]; Rpeu3v0=x[144] #Rp3 Immune after third exposure and seropos
  Rncr3v0=x[145]; Rncu3v0=x[146]; Rnar3v0=x[147]; Rnau3v0=x[148]; Rner3v0=x[149]; Rneu3v0=x[150] #Rn3 Immune after third exposure and seoneg
  Dcr3v0 =x[151]; Dcu3v0 =x[152]; Dar3v0 =x[153]; Dau3v0 =x[154]; Der3v0 =x[155]; Deu3v0 =x[156] #D3
  
  #####One dose of vaccination####
  # Vaccinated once and unexposed
  Spcr0v1 =x[157]; Spcu0v1 =x[158];Spar0v1 =x[159];Spau0v1 =x[160];Sper0v1 =x[161];Speu0v1 =x[162] #Sp0v1
  Sncr0v1 =x[163]; Sncu0v1 =x[164];Snar0v1 =x[165];Snau0v1 =x[166];Sner0v1 =x[167];Sneu0v1 =x[168] #Sp0v1
  Ecr1v1 =x[169];  Ecu1v1 =x[170]; Ear1v1 =x[171]; Eau1v1 =x[172]; Eer1v1 =x[173]; Eeu1v1 =x[174] #E1
  Acr1v1 =x[175];  Acu1v1 =x[176]; Aar1v1 =x[177]; Aau1v1 =x[178]; Aer1v1 =x[179]; Aeu1v1 =x[180] #A1Good morning, is this still available?
  Icr1v1 =x[181];  Icu1v1 =x[182]; Iar1v1 =x[183]; Iau1v1 =x[184]; Ier1v1 =x[185]; Ieu1v1 =x[186] #I1
  Hcr1v1 =x[187];  Hcu1v1 =x[188]; Har1v1 =x[189]; Hau1v1 =x[190]; Her1v1 =x[191]; Heu1v1 =x[192] #H1
  Rpcr1v1=x[193];  Rpcu1v1=x[194]; Rpar1v1=x[195]; Rpau1v1=x[196]; Rper1v1=x[197]; Rpeu1v1=x[198] #Immune after first exposure and seropositive
  Rncr1v1=x[199];  Rncu1v1=x[200]; Rnar1v1=x[201]; Rnau1v1=x[202]; Rner1v1=x[203]; Rneu1v1=x[204] #Immune after first exposure and seronegative
  Dcr1v1 =x[205];  Dcu1v1 =x[206]; Dar1v1 =x[207]; Dau1v1 =x[208]; Der1v1 =x[209]; Deu1v1 =x[210] #D1
  
  # Vaccinated once and exposed once through infection
  Spcr1v1=x[211]; Spcu1v1=x[212]; Spar1v1=x[213]; Spau1v1=x[214]; Sper1v1=x[215];  Speu1v1=x[216] #S1 part sussceptible after first exposure & seropositive
  Sncr1v1=x[217]; Sncu1v1=x[218]; Snar1v1=x[219]; Snau1v1=x[220]; Sner1v1=x[221];  Sneu1v1=x[222] #S1 part sussceptible after first exposure & seronegative
  Ecr2v1 =x[223]; Ecu2v1 =x[224]; Ear2v1 =x[225]; Eau2v1 =x[226]; Eer2v1 =x[227];  Eeu2v1 =x[228] #E2 infection among those with first prior exposure
  Acr2v1 =x[229]; Acu2v1 =x[230]; Aar2v1 =x[231]; Aau2v1 =x[232]; Aer2v1 =x[233];  Aeu2v1 =x[234] #A2
  Icr2v1 =x[235]; Icu2v1 =x[236]; Iar2v1 =x[237]; Iau2v1 =x[238]; Ier2v1 =x[239];  Ieu2v1 =x[240] #I2
  Hcr2v1 =x[241]; Hcu2v1 =x[242]; Har2v1 =x[243]; Hau2v1 =x[244]; Her2v1 =x[245];  Heu2v1 =x[246] #H2
  Rpcr2v1=x[247]; Rpcu2v1=x[248]; Rpar2v1=x[249]; Rpau2v1=x[250]; Rper2v1=x[251];  Rpeu2v1=x[252] #Rp2 Immune after second exposure and seropos
  Rncr2v1=x[253]; Rncu2v1=x[254]; Rnar2v1=x[255]; Rnau2v1=x[256]; Rner2v1=x[257];  Rneu2v1=x[258] #Rn2 Immune after second exposure and seroneg
  Dcr2v1 =x[259]; Dcu2v1 =x[260]; Dar2v1 =x[261]; Dau2v1 =x[262]; Der2v1 =x[263];  Deu2v1=x[264] #D2
  
  #Vaccinated once and exposed twice through infection
  Spcr2v1=x[265]; Spcu2v1=x[266]; Spar2v1=x[267]; Spau2v1=x[268]; Sper2v1=x[269]; Speu2v1=x[270] #S2 part sussceptible after second exposure & seropositive
  Sncr2v1=x[271]; Sncu2v1=x[272]; Snar2v1=x[273]; Snau2v1=x[274]; Sner2v1=x[275]; Sneu2v1=x[276] #S2 part sussceptible after second exposure & seronegative
  Ecr3v1 =x[277]; Ecu3v1 =x[278]; Ear3v1 =x[279]; Eau3v1 =x[280]; Eer3v1 =x[281]; Eeu3v1 =x[282] #E3 infection among those with second prior exposure
  Acr3v1 =x[283]; Acu3v1 =x[284]; Aar3v1 =x[285]; Aau3v1 =x[286]; Aer3v1 =x[287]; Aeu3v1 =x[288] #A3
  Icr3v1 =x[289]; Icu3v1 =x[290]; Iar3v1 =x[291]; Iau3v1 =x[292]; Ier3v1 =x[293]; Ieu3v1 =x[294] #I3
  Hcr3v1 =x[295]; Hcu3v1 =x[296]; Har3v1 =x[297]; Hau3v1 =x[298]; Her3v1 =x[299]; Heu3v1 =x[300] #H3
  Rpcr3v1=x[301]; Rpcu3v1=x[302]; Rpar3v1=x[303]; Rpau3v1=x[304]; Rper3v1=x[305]; Rpeu3v1=x[306] #Rp3 Immune after third exposure and seropos
  Rncr3v1=x[307]; Rncu3v1=x[308]; Rnar3v1=x[309]; Rnau3v1=x[310]; Rner3v1=x[311]; Rneu3v1=x[312] #Rn3 Immune after third exposure and seoneg
  Dcr3v1 =x[313]; Dcu3v1 =x[314]; Dar3v1 =x[315]; Dau3v1 =x[316]; Der3v1 =x[317]; Deu3v1 =x[318] #D3
  
  #####Two dose of vaccination####
  # Vaccinated twice and unexposed
  Spcr0v2 =x[319];Spcu0v2 =x[320];Spar0v2 =x[321];Spau0v2 =x[322];Sper0v2 =x[323];Speu0v2 =x[324] #S0
  Sncr0v2 =x[325];Sncu0v2 =x[326];Snar0v2 =x[327];Snau0v2 =x[328];Sner0v2 =x[329];Sneu0v2 =x[330] #S0
  Ecr1v2 =x[331]; Ecu1v2 =x[332]; Ear1v2 =x[333]; Eau1v2 =x[334]; Eer1v2 =x[335]; Eeu1v2 =x[336] #E1
  Acr1v2 =x[337]; Acu1v2 =x[338]; Aar1v2 =x[339]; Aau1v2 =x[340]; Aer1v2 =x[341]; Aeu1v2 =x[342] #A1
  Icr1v2 =x[343]; Icu1v2 =x[344]; Iar1v2 =x[345]; Iau1v2 =x[346]; Ier1v2 =x[347]; Ieu1v2 =x[348] #I1
  Hcr1v2 =x[349]; Hcu1v2 =x[350]; Har1v2 =x[351]; Hau1v2 =x[352]; Her1v2 =x[353]; Heu1v2 =x[354] #H1
  Rpcr1v2=x[355]; Rpcu1v2=x[356]; Rpar1v2=x[357]; Rpau1v2=x[358]; Rper1v2=x[359]; Rpeu1v2=x[360] #Immune after first exposure and seropositive
  Rncr1v2=x[361]; Rncu1v2=x[362]; Rnar1v2=x[363]; Rnau1v2=x[364]; Rner1v2=x[365]; Rneu1v2=x[366] #Immune after first exposure and seronegative
  Dcr1v2 =x[367]; Dcu1v2 =x[368]; Dar1v2 =x[369]; Dau1v2 =x[370]; Der1v2 =x[371]; Deu1v2 =x[372] #D1
  
  # Vaccinated twice and exposed once through infection
  Spcr1v2=x[373]; Spcu1v2=x[374]; Spar1v2=x[375]; Spau1v2=x[376]; Sper1v2=x[377];  Speu1v2=x[378] #S1 part sussceptible after first exposure & seropositive
  Sncr1v2=x[379]; Sncu1v2=x[380]; Snar1v2=x[381]; Snau1v2=x[382]; Sner1v2=x[383];  Sneu1v2=x[384] #S1 part sussceptible after first exposure & seronegative
  Ecr2v2 =x[385]; Ecu2v2 =x[386]; Ear2v2 =x[387]; Eau2v2 =x[388]; Eer2v2 =x[389];  Eeu2v2 =x[390] #E2 infection among those with first prior exposure
  Acr2v2 =x[391]; Acu2v2 =x[392]; Aar2v2 =x[393]; Aau2v2 =x[394]; Aer2v2 =x[395];  Aeu2v2 =x[396] #A2
  Icr2v2 =x[397]; Icu2v2 =x[398]; Iar2v2 =x[399]; Iau2v2 =x[400]; Ier2v2 =x[401];  Ieu2v2 =x[402] #I2
  Hcr2v2 =x[403]; Hcu2v2 =x[404]; Har2v2 =x[405]; Hau2v2 =x[406]; Her2v2 =x[407];  Heu2v2 =x[408] #H2
  Rpcr2v2=x[409]; Rpcu2v2=x[410]; Rpar2v2=x[411]; Rpau2v2=x[412]; Rper2v2=x[413];  Rpeu2v2=x[414] #Rp2 Immune after second exposure and seropos
  Rncr2v2=x[415]; Rncu2v2=x[416]; Rnar2v2=x[417]; Rnau2v2=x[418]; Rner2v2=x[419];  Rneu2v2=x[420] #Rn2 Immune after second exposure and seroneg
  Dcr2v2 =x[421]; Dcu2v2 =x[422]; Dar2v2 =x[423]; Dau2v2 =x[424]; Der2v2 =x[425];  Deu2v2=x[426] #D2
  
  #Vaccinated twice and exposed twice through infection
  Spcr2v2=x[427]; Spcu2v2=x[428]; Spar2v2=x[429]; Spau2v2=x[430]; Sper2v2=x[431]; Speu2v2=x[432] #S2 part sussceptible after second exposure & seropositive
  Sncr2v2=x[433]; Sncu2v2=x[434]; Snar2v2=x[435]; Snau2v2=x[436]; Sner2v2=x[437]; Sneu2v2=x[438] #S2 part sussceptible after second exposure & seronegative
  Ecr3v2 =x[439]; Ecu3v2 =x[440]; Ear3v2 =x[441]; Eau3v2 =x[442]; Eer3v2 =x[443]; Eeu3v2 =x[444] #E3 infection among those with second prior exposure
  Acr3v2 =x[445]; Acu3v2 =x[446]; Aar3v2 =x[447]; Aau3v2 =x[448]; Aer3v2 =x[449]; Aeu3v2 =x[450] #A3
  Icr3v2 =x[451]; Icu3v2 =x[452]; Iar3v2 =x[453]; Iau3v2 =x[454]; Ier3v2 =x[455]; Ieu3v2 =x[456] #I3
  Hcr3v2 =x[457]; Hcu3v2 =x[458]; Har3v2 =x[459]; Hau3v2 =x[460]; Her3v2 =x[461]; Heu3v2 =x[462] #H3
  Rpcr3v2=x[463]; Rpcu3v2=x[464]; Rpar3v2=x[465]; Rpau3v2=x[466]; Rper3v2=x[467]; Rpeu3v2=x[468] #Rp3 Immune after third exposure and seropos
  Rncr3v2=x[469]; Rncu3v2=x[470]; Rnar3v2=x[471]; Rnau3v2=x[472]; Rner3v2=x[473]; Rneu3v2=x[474] #Rn3 Immune after third exposure and seoneg
  Dcr3v2 =x[475]; Dcu3v2 =x[476]; Dar3v2 =x[477]; Dau3v2 =x[478]; Der3v2 =x[479]; Deu3v2 =x[480] #D3
  
  #####Three dose of vaccination####
  # Vaccinated three times and unexposed
  Spcr0v3 =x[481];Spcu0v3 =x[482];Spar0v3 =x[483];Spau0v3 =x[484];Sper0v3 =x[485];Speu0v3 =x[486] #S0
  Sncr0v3 =x[487];Sncu0v3 =x[488];Snar0v3 =x[489];Snau0v3 =x[490];Sner0v3 =x[491];Sneu0v3 =x[492] #S0
  Ecr1v3 =x[493]; Ecu1v3 =x[494];  Ear1v3 =x[495];Eau1v3 =x[496]; Eer1v3 =x[497]; Eeu1v3 =x[498] #E1
  Acr1v3 =x[499]; Acu1v3 =x[500]; Aar1v3 =x[501]; Aau1v3 =x[502]; Aer1v3 =x[503]; Aeu1v3 =x[504] #A1
  Icr1v3 =x[505]; Icu1v3 =x[506]; Iar1v3 =x[507]; Iau1v3 =x[508]; Ier1v3 =x[509]; Ieu1v3 =x[510] #I1
  Hcr1v3 =x[511]; Hcu1v3 =x[512]; Har1v3 =x[513]; Hau1v3 =x[514]; Her1v3 =x[515]; Heu1v3 =x[516] #H1
  Rpcr1v3=x[517]; Rpcu1v3=x[518]; Rpar1v3=x[519]; Rpau1v3=x[520]; Rper1v3=x[521]; Rpeu1v3=x[522] #Immune after first exposure and seropositive
  Rncr1v3=x[523]; Rncu1v3=x[524]; Rnar1v3=x[525]; Rnau1v3=x[526]; Rner1v3=x[527]; Rneu1v3=x[528] #Immune after first exposure and seronegative
  Dcr1v3 =x[529]; Dcu1v3 =x[530]; Dar1v3 =x[531]; Dau1v3 =x[532]; Der1v3 =x[533]; Deu1v3 =x[534] #D1
  
  # Vaccinated three times and exposed once through infection
  Spcr1v3=x[535]; Spcu1v3=x[536]; Spar1v3=x[537]; Spau1v3=x[538]; Sper1v3=x[539];  Speu1v3=x[540] #S1 part sussceptible after first exposure & seropositive
  Sncr1v3=x[541]; Sncu1v3=x[542]; Snar1v3=x[543]; Snau1v3=x[544]; Sner1v3=x[545];  Sneu1v3=x[546] #S1 part sussceptible after first exposure & seronegative
  Ecr2v3 =x[547]; Ecu2v3 =x[548]; Ear2v3 =x[549]; Eau2v3 =x[550]; Eer2v3 =x[551];  Eeu2v3 =x[552] #E2 infection among those with first prior exposure
  Acr2v3 =x[553]; Acu2v3 =x[554]; Aar2v3 =x[555]; Aau2v3 =x[556]; Aer2v3 =x[557];  Aeu2v3 =x[558] #A2
  Icr2v3 =x[559]; Icu2v3 =x[560]; Iar2v3 =x[561]; Iau2v3 =x[562]; Ier2v3 =x[563];  Ieu2v3 =x[564] #I2
  Hcr2v3 =x[565]; Hcu2v3 =x[566]; Har2v3 =x[567]; Hau2v3 =x[568]; Her2v3 =x[569];  Heu2v3 =x[570] #H2
  Rpcr2v3=x[571]; Rpcu2v3=x[572]; Rpar2v3=x[573]; Rpau2v3=x[574]; Rper2v3=x[575];  Rpeu2v3=x[576] #Rp2 Immune after second exposure and seropos
  Rncr2v3=x[577]; Rncu2v3=x[578]; Rnar2v3=x[579]; Rnau2v3=x[580]; Rner2v3=x[581];  Rneu2v3=x[582] #Rn2 Immune after second exposure and seroneg
  Dcr2v3 =x[583]; Dcu2v3 =x[584]; Dar2v3 =x[585]; Dau2v3 =x[586]; Der2v3 =x[587];  Deu2v3=x[588] #D2
  
  #Vaccinated three times and exposed twice through infection
  Spcr2v3=x[589]; Spcu2v3=x[590]; Spar2v3=x[591]; Spau2v3=x[592]; Sper2v3=x[593]; Speu2v3=x[594] #S2 part sussceptible after second exposure & seropositive
  Sncr2v3=x[595]; Sncu2v3=x[596]; Snar2v3=x[597]; Snau2v3=x[598]; Sner2v3=x[599]; Sneu2v3=x[600] #S2 part sussceptible after second exposure & seronegative
  Ecr3v3 =x[601]; Ecu3v3 =x[602]; Ear3v3 =x[603]; Eau3v3 =x[604]; Eer3v3 =x[605]; Eeu3v3 =x[606] #E3 infection among those with second prior exposure
  Acr3v3 =x[607]; Acu3v3 =x[608]; Aar3v3 =x[609]; Aau3v3 =x[610]; Aer3v3 =x[611]; Aeu3v3 =x[612] #A3
  Icr3v3 =x[613]; Icu3v3 =x[614]; Iar3v3 =x[615]; Iau3v3 =x[616]; Ier3v3 =x[617]; Ieu3v3 =x[618] #I3
  Hcr3v3 =x[619]; Hcu3v3 =x[620]; Har3v3 =x[621]; Hau3v3 =x[622]; Her3v3 =x[623]; Heu3v3 =x[624] #H3
  Rpcr3v3=x[625]; Rpcu3v3=x[626]; Rpar3v3=x[627]; Rpau3v3=x[628]; Rper3v3=x[629]; Rpeu3v3=x[630] #Rp3 Immune after third exposure and seropos
  Rncr3v3=x[631]; Rncu3v3=x[632]; Rnar3v3=x[633]; Rnau3v3=x[634]; Rner3v3=x[635]; Rneu3v3=x[636] #Rn3 Immune after third exposure and seoneg
  Dcr3v3 =x[637]; Dcu3v3 =x[638]; Dar3v3 =x[639]; Dau3v3 =x[640]; Der3v3 =x[641]; Deu3v3 =x[642] #D3
  
  ##Cumulative trackers
  Ecum1v0_cr=x[643]; Ecum1v0_cu=x[644]; Ecum1v0_ar=x[645]; Ecum1v0_au=x[646]; Ecum1v0_er=x[647]; Ecum1v0_eu=x[648]
  Ecum2v0_cr=x[649]; Ecum2v0_cu=x[650]; Ecum2v0_ar=x[651]; Ecum2v0_au=x[652]; Ecum2v0_er=x[653]; Ecum2v0_eu=x[654]
  Ecum3v0_cr=x[655]; Ecum3v0_cu=x[656]; Ecum3v0_ar=x[657]; Ecum3v0_au=x[658]; Ecum3v0_er=x[659]; Ecum3v0_eu=x[660]
  
  Ecum1v1_cr=x[661]; Ecum1v1_cu=x[662]; Ecum1v1_ar=x[663]; Ecum1v1_au=x[664]; Ecum1v1_er=x[665]; Ecum1v1_eu=x[666]
  Ecum2v1_cr=x[667]; Ecum2v1_cu=x[668]; Ecum2v1_ar=x[669]; Ecum2v1_au=x[670]; Ecum2v1_er=x[671]; Ecum2v1_eu=x[672]
  Ecum3v1_cr=x[673]; Ecum3v1_cu=x[674]; Ecum3v1_ar=x[675]; Ecum3v1_au=x[676]; Ecum3v1_er=x[677]; Ecum3v1_eu=x[678]
  
  Ecum1v2_cr=x[679]; Ecum1v2_cu=x[680]; Ecum1v2_ar=x[681]; Ecum1v2_au=x[682]; Ecum1v2_er=x[683]; Ecum1v2_eu=x[684]
  Ecum2v2_cr=x[685]; Ecum2v2_cu=x[686]; Ecum2v2_ar=x[687]; Ecum2v2_au=x[688]; Ecum2v2_er=x[689]; Ecum2v2_eu=x[690]
  Ecum3v2_cr=x[691]; Ecum3v2_cu=x[692]; Ecum3v2_ar=x[693]; Ecum3v2_au=x[694]; Ecum3v2_er=x[695]; Ecum3v2_eu=x[696]
  
  Ecum1v3_cr=x[697]; Ecum1v3_cu=x[698]; Ecum1v3_ar=x[699]; Ecum1v3_au=x[700]; Ecum1v3_er=x[701]; Ecum1v3_eu =x[702]
  Ecum2v3_cr=x[703]; Ecum2v3_cu=x[704]; Ecum2v3_ar=x[705]; Ecum2v3_au=x[706]; Ecum2v3_er=x[707]; Ecum2v3_eu=x[708]
  Ecum3v3_cr=x[709]; Ecum3v3_cu=x[710]; Ecum3v3_ar=x[711]; Ecum3v3_au=x[712]; Ecum3v3_er=x[713]; Ecum3v3_eu=x[714]
  
  Icum1v0_cr=x[715]; Icum1v0_cu=x[716]; Icum1v0_ar=x[717]; Icum1v0_au=x[718]; Icum1v0_er=x[719]; Icum1v0_eu=x[720]
  Icum2v0_cr=x[721]; Icum2v0_cu=x[722]; Icum2v0_ar=x[723]; Icum2v0_au=x[724]; Icum2v0_er=x[725]; Icum2v0_eu=x[726]
  Icum3v0_cr=x[727]; Icum3v0_cu=x[728]; Icum3v0_ar=x[729]; Icum3v0_au=x[730]; Icum3v0_er=x[731]; Icum3v0_eu=x[732]
  
  Icum1v1_cr=x[733]; Icum1v1_cu=x[734]; Icum1v1_ar=x[735]; Icum1v1_au=x[736]; Icum1v1_er=x[737]; Icum1v1_eu=x[738]
  Icum2v1_cr=x[739]; Icum2v1_cu=x[740]; Icum2v1_ar=x[741]; Icum2v1_au=x[742]; Icum2v1_er=x[743]; Icum2v1_eu=x[744]
  Icum3v1_cr=x[745]; Icum3v1_cu=x[746]; Icum3v1_ar=x[747]; Icum3v1_au=x[748]; Icum3v1_er=x[749]; Icum3v1_eu=x[750]
  
  Icum1v2_cr=x[751]; Icum1v2_cu=x[752]; Icum1v2_ar=x[753]; Icum1v2_au=x[754]; Icum1v2_er=x[755]; Icum1v2_eu=x[756]
  Icum2v2_cr=x[757]; Icum2v2_cu=x[758]; Icum2v2_ar=x[759]; Icum2v2_au=x[760]; Icum2v2_er=x[761]; Icum2v2_eu=x[762]
  Icum3v2_cr=x[763]; Icum3v2_cu=x[764]; Icum3v2_ar=x[765]; Icum3v2_au=x[766]; Icum3v2_er=x[767]; Icum3v2_eu=x[768]
  
  Icum1v3_cr=x[769]; Icum1v3_cu=x[770]; Icum1v3_ar=x[771]; Icum1v3_au=x[772]; Icum1v3_er=x[773]; Icum1v3_eu=x[774]
  Icum2v3_cr=x[775]; Icum2v3_cu=x[776]; Icum2v3_ar=x[777]; Icum2v3_au=x[778]; Icum2v3_er=x[779]; Icum2v3_eu=x[780]
  Icum3v3_cr=x[781]; Icum3v3_cu=x[782]; Icum3v3_ar=x[783]; Icum3v3_au=x[784]; Icum3v3_er=x[785]; Icum3v3_eu=x[786]
  
  ##Gamma waning trackers
  W1Rpcr1v0 = x[787];  W2Rpcr1v0 = x[788];  W3Rpcr1v0 = x[789]
  W1Rpcu1v0 = x[790]; W2Rpcu1v0 = x[791]; W3Rpcu1v0 = x[792]
  W1Rpar1v0 = x[793]; W2Rpar1v0 = x[794]; W3Rpar1v0 = x[795]
  W1Rpau1v0 = x[796]; W2Rpau1v0 = x[797]; W3Rpau1v0 = x[798]
  W1Rper1v0 = x[799]; W2Rper1v0 = x[800]; W3Rper1v0 = x[801]
  W1Rpeu1v0 = x[802]; W2Rpeu1v0 = x[803]; W3Rpeu1v0 = x[804]
  
  ##Vax and temporarily immune
  Vpcr0v1 = x[805]; Vpcu0v1 = x[806]; Vpar0v1 = x[807]; Vpau0v1 = x[808]; Vper0v1 = x[809]; Vpeu0v1 = x[810]
  Vncr0v1 = x[811]; Vncu0v1 = x[812]; Vnar0v1 = x[813]; Vnau0v1 = x[814]; Vner0v1 = x[815]; Vneu0v1 = x[816]
  Vpcr0v2 = x[817]; Vpcu0v2 = x[818]; Vpar0v2 = x[819]; Vpau0v2 = x[820]; Vper0v2 = x[821]; Vpeu0v2 = x[822]
  Vncr0v2 = x[823]; Vncu0v2 = x[824]; Vnar0v2 = x[825]; Vnau0v2 = x[826]; Vner0v2 = x[827]; Vneu0v2 = x[828]
  Vpcr0v3 = x[829]; Vpcu0v3 = x[830]; Vpar0v3 = x[831]; Vpau0v3 = x[832]; Vper0v3 = x[833]; Vpeu0v3 = x[834]
  Vncr0v3 = x[835]; Vncu0v3 = x[836]; Vnar0v3 = x[837]; Vnau0v3 = x[838]; Vner0v3 = x[839]; Vneu0v3 = x[840]
  
  ##Gamma waning trackers
  W1Vpcr0v1 = x[841];  W2Vpcr0v1 = x[842];  W3Vpcr0v1 = x[843]
  W1Vpcu0v1 = x[844];  W2Vpcu0v1 = x[845];  W3Vpcu0v1 = x[846]
  W1Vpar0v1 = x[847];  W2Vpar0v1 = x[848];  W3Vpar0v1 = x[849]
  W1Vpau0v1 = x[850];  W2Vpau0v1 = x[851];  W3Vpau0v1 = x[852]
  W1Vper0v1 = x[853];  W2Vper0v1 = x[854];  W3Vper0v1 = x[855]
  W1Vpeu0v1 = x[856];  W2Vpeu0v1 = x[857];  W3Vpeu0v1 = x[858]
  
  
  with(as.list(params),{
    # Derived variables
    #Rural child 
    Ncr0v0 = sum(Scr0v0, Ecr1v0, Acr1v0, Icr1v0, Hcr1v0, Rpcr1v0, W1Rpcr1v0,W2Rpcr1v0,W3Rpcr1v0,Rncr1v0) #Unvax and no prior exposure
    Ncr1v0 = sum(Spcr1v0, Sncr1v0, Ecr2v0, Acr2v0, Icr2v0, Hcr2v0, Rpcr2v0, Rncr2v0) #Unvax and one prior
    Ncr2v0 = sum(Spcr2v0, Sncr2v0, Ecr3v0, Acr3v0, Icr3v0, Hcr3v0, Rpcr3v0, Rncr3v0) #Unvax and two prior
    
    Ncr0v1 = sum(Spcr0v1,Vpcr0v1, W1Vpcr0v1,W2Vpcr0v1,W3Vpcr0v1,Vncr0v1,Sncr0v1, Ecr1v1, Acr1v1, Icr1v1, Hcr1v1, Rpcr1v1, Rncr1v1) #vax 1 dose and no prior exposure
    Ncr1v1 = sum(Spcr1v1, Sncr1v1, Ecr2v1, Acr2v1, Icr2v1, Hcr2v1, Rpcr2v1, Rncr2v1) #vax 1 dose and one prior
    Ncr2v1 = sum(Spcr2v1, Sncr2v1, Ecr3v1, Acr3v1, Icr3v1, Hcr3v1, Rpcr3v1, Rncr3v1) #vax 1 dose and two prior
    
    Ncr0v2 = sum(Spcr0v2, Vpcr0v2, Vncr0v2,Sncr0v2, Ecr1v2, Acr1v2, Icr1v2, Hcr1v2, Rpcr1v2, Rncr1v2) #vax 2 dose and no prior exposure
    Ncr1v2 = sum(Spcr1v2, Sncr1v2, Ecr2v2, Acr2v2, Icr2v2, Hcr2v2, Rpcr2v2, Rncr2v2) #vax 2 dose and one prior
    Ncr2v2 = sum(Spcr2v2, Sncr2v2, Ecr3v2, Acr3v2, Icr3v2, Hcr3v2, Rpcr3v2, Rncr3v2) #vax 2 doseand two prior
    
    Ncr0v3 = sum(Spcr0v3, Vpcr0v3, Vncr0v3,Sncr0v3, Ecr1v3, Acr1v3, Icr1v3, Hcr1v3, Rpcr1v3, Rncr1v3) #vax 3 dose and no prior exposure
    Ncr1v3 = sum(Spcr1v3, Sncr1v3, Ecr2v3, Acr2v3, Icr2v3, Hcr2v3, Rpcr2v3, Rncr2v3) #vax 3 dose and one prior
    Ncr2v3 = sum(Spcr2v3, Sncr2v3, Ecr3v3, Acr3v3, Icr3v3, Hcr3v3, Rpcr3v3, Rncr3v3) #vax 3 dose and two prior
    
    #Urban child
    Ncu0v0 = sum(Scu0v0, Ecu1v0, Acu1v0, Icu1v0, Hcu1v0, Rpcu1v0,W1Rpcu1v0,W2Rpcu1v0,W3Rpcu1v0, Rncu1v0) #Unvax and no prior exposure
    Ncu1v0 = sum(Spcu1v0, Sncu1v0, Ecu2v0, Acu2v0, Icu2v0, Hcu2v0, Rpcu2v0, Rncu2v0) #Unvax and one prior
    Ncu2v0 = sum(Spcu2v0, Sncu2v0, Ecu3v0, Acu3v0, Icu3v0, Hcu3v0, Rpcu3v0, Rncu3v0) #Unvax and two prior
    
    Ncu0v1 = sum(Spcu0v1,Vpcu0v1, W1Vpcu0v1,W2Vpcu0v1,W3Vpcu0v1,Vncu0v1,Sncu0v1, Ecu1v1, Acu1v1, Icu1v1, Hcu1v1, Rpcu1v1, Rncu1v1) #vax 1 dose and no prior exposure
    Ncu1v1 = sum(Spcu1v1, Sncu1v1, Ecu2v1, Acu2v1, Icu2v1, Hcu2v1, Rpcu2v1, Rncu2v1) #vax 1 dose and one prior
    Ncu2v1 = sum(Spcu2v1, Sncu2v1, Ecu3v1, Acu3v1, Icu3v1, Hcu3v1, Rpcu3v1, Rncu3v1) #vax 1 dose and two prior
    
    Ncu0v2 = sum(Spcu0v2, Vpcu0v2, Vncu0v2,Sncu0v2,Ecu1v2, Acu1v2, Icu1v2, Hcu1v2, Rpcu1v2, Rncu1v2) #vax 2 dose and no prior exposure
    Ncu1v2 = sum(Spcu1v2, Sncu1v2, Ecu2v2, Acu2v2, Icu2v2, Hcu2v2, Rpcu2v2, Rncu2v2) #vax 2 dose and one prior
    Ncu2v2 = sum(Spcu2v2, Sncu2v2, Ecu3v2, Acu3v2, Icu3v2, Hcu3v2, Rpcu3v2, Rncu3v2) #vax 2 doseand two prior
    
    Ncu0v3 = sum(Spcu0v3, Vpcu0v3, Vncu0v3,Sncu0v3,Ecu1v3, Acu1v3, Icu1v3, Hcu1v3, Rpcu1v3, Rncu1v3) #vax 3 dose and no prior exposure
    Ncu1v3 = sum(Spcu1v3, Sncu1v3, Ecu2v3, Acu2v3, Icu2v3, Hcu2v3, Rpcu2v3, Rncu2v3) #vax 3 dose and one prior
    Ncu2v3 = sum(Spcu2v3, Sncu2v3, Ecu3v3, Acu3v3, Icu3v3, Hcu3v3, Rpcu3v3, Rncu3v3) #vax 3 dose and two prior
    
    #Rural adult
    Nar0v0 = sum(Sar0v0, Ear1v0, Aar1v0, Iar1v0, Har1v0, Rpar1v0,W1Rpar1v0,W2Rpar1v0,W3Rpar1v0,  Rnar1v0) #Unvax and no prior exposure
    Nar1v0 = sum(Spar1v0, Snar1v0, Ear2v0, Aar2v0, Iar2v0, Har2v0, Rpar2v0, Rnar2v0) #Unvax and one prior
    Nar2v0 = sum(Spar2v0, Snar2v0, Ear3v0, Aar3v0, Iar3v0, Har3v0, Rpar3v0, Rnar3v0) #Unvax and two prior
    
    Nar0v1 = sum(Spar0v1,Vpar0v1, W1Vpar0v1,W2Vpar0v1,W3Vpar0v1,Vnar0v1, Snar0v1, Ear1v1, Aar1v1, Iar1v1, Har1v1, Rpar1v1, Rnar1v1) #vax 1 dose and no prior exposure
    Nar1v1 = sum(Spar1v1, Snar1v1, Ear2v1, Aar2v1, Iar2v1, Har2v1, Rpar2v1, Rnar2v1) #vax 1 dose and one prior
    Nar2v1 = sum(Spar2v1, Snar2v1, Ear3v1, Aar3v1, Iar3v1, Har3v1, Rpar3v1, Rnar3v1) #vax 1 dose and two prior
    
    Nar0v2 = sum(Spar0v2, Vpar0v2, Vnar0v2, Snar0v2, Ear1v2, Aar1v2, Iar1v2, Har1v2, Rpar1v2, Rnar1v2) #vax 2 dose and no prior exposure
    Nar1v2 = sum(Spar1v2, Snar1v2, Ear2v2, Aar2v2, Iar2v2, Har2v2, Rpar2v2, Rnar2v2) #vax 2 dose and one prior
    Nar2v2 = sum(Spar2v2, Snar2v2, Ear3v2, Aar3v2, Iar3v2, Har3v2, Rpar3v2, Rnar3v2) #vax 2 doseand two prior
    
    Nar0v3 = sum(Spar0v3, Vpar0v3, Vnar0v3,Snar0v3, Ear1v3, Aar1v3, Iar1v3, Har1v3, Rpar1v3, Rnar1v3) #vax 3 dose and no prior exposure
    Nar1v3 = sum(Spar1v3, Snar1v3, Ear2v3, Aar2v3, Iar2v3, Har2v3, Rpar2v3, Rnar2v3) #vax 3 dose and one prior
    Nar2v3 = sum(Spar2v3, Snar2v3, Ear3v3, Aar3v3, Iar3v3, Har3v3, Rpar3v3, Rnar3v3) #vax 3 dose and two prior
    
    #Urban adult
    Nau0v0 = sum(Sau0v0, Eau1v0, Aau1v0, Iau1v0, Hau1v0, Rpau1v0, W1Rpau1v0,W2Rpau1v0,W3Rpau1v0, Rnau1v0) #Unvax and no prior exposure
    Nau1v0 = sum(Spau1v0, Snau1v0, Eau2v0, Aau2v0, Iau2v0, Hau2v0, Rpau2v0, Rnau2v0) #Unvax and one prior
    Nau2v0 = sum(Spau2v0, Snau2v0, Eau3v0, Aau3v0, Iau3v0, Hau3v0, Rpau3v0, Rnau3v0) #Unvax and two prior
    
    Nau0v1 = sum(Spau0v1,Vpau0v1, W1Vpau0v1,W2Vpau0v1,W3Vpau0v1,Vnau0v1,Snau0v1, Eau1v1, Aau1v1, Iau1v1, Hau1v1, Rpau1v1, Rnau1v1) #vax 1 dose and no prior exposure
    Nau1v1 = sum(Spau1v1, Snau1v1, Eau2v1, Aau2v1, Iau2v1, Hau2v1, Rpau2v1, Rnau2v1) #vax 1 dose and one prior
    Nau2v1 = sum(Spau2v1, Snau2v1, Eau3v1, Aau3v1, Iau3v1, Hau3v1, Rpau3v1, Rnau3v1) #vax 1 dose and two prior
    
    Nau0v2 = sum(Spau0v2, Vpau0v2, Vnau0v2,Snau0v2, Eau1v2, Aau1v2, Iau1v2, Hau1v2, Rpau1v2, Rnau1v2) #vax 2 dose and no prior exposure
    Nau1v2 = sum(Spau1v2, Snau1v2, Eau2v2, Aau2v2, Iau2v2, Hau2v2, Rpau2v2, Rnau2v2) #vax 2 dose and one prior
    Nau2v2 = sum(Spau2v2, Snau2v2, Eau3v2, Aau3v2, Iau3v2, Hau3v2, Rpau3v2, Rnau3v2) #vax 2 doseand two prior
    
    Nau0v3 = sum(Spau0v3, Vpau0v3, Vnau0v3,Snau0v3, Eau1v3, Aau1v3, Iau1v3, Hau1v3, Rpau1v3, Rnau1v3) #vax 3 dose and no prior exposure
    Nau1v3 = sum(Spau1v3, Snau1v3, Eau2v3, Aau2v3, Iau2v3, Hau2v3, Rpau2v3, Rnau2v3) #vax 3 dose and one prior
    Nau2v3 = sum(Spau2v3, Snau2v3, Eau3v3, Aau3v3, Iau3v3, Hau3v3, Rpau3v3, Rnau3v3) #vax 3 dose and two prior
    
    #Rural elderly
    Ner0v0 = sum(Ser0v0, Eer1v0, Aer1v0, Ier1v0, Her1v0, Rper1v0,W1Rper1v0,W2Rper1v0,W3Rper1v0, Rner1v0) #Unvax and no prior exposure
    Ner1v0 = sum(Sper1v0, Sner1v0, Eer2v0, Aer2v0, Ier2v0, Her2v0, Rper2v0, Rner2v0) #Unvax and one prior
    Ner2v0 = sum(Sper2v0, Sner2v0, Eer3v0, Aer3v0, Ier3v0, Her3v0, Rper3v0, Rner3v0) #Unvax and two prior
    
    Ner0v1 = sum(Sper0v1,Vper0v1, W1Vper0v1,W2Vper0v1,W3Vper0v1,Vner0v1, Sner0v1, Eer1v1, Aer1v1, Ier1v1, Her1v1, Rper1v1, Rner1v1) #vax 1 dose and no prior exposure
    Ner1v1 = sum(Sper1v1, Sner1v1, Eer2v1, Aer2v1, Ier2v1, Her2v1, Rper2v1, Rner2v1) #vax 1 dose and one prior
    Ner2v1 = sum(Sper2v1, Sner2v1, Eer3v1, Aer3v1, Ier3v1, Her3v1, Rper3v1, Rner3v1) #vax 1 dose and two prior
    
    Ner0v2 = sum(Sper0v2, Vper0v2, Vner0v2,Sner0v2, Eer1v2, Aer1v2, Ier1v2, Her1v2, Rper1v2, Rner1v2) #vax 2 dose and no prior exposure
    Ner1v2 = sum(Sper1v2, Sner1v2, Eer2v2, Aer2v2, Ier2v2, Her2v2, Rper2v2, Rner2v2) #vax 2 dose and one prior
    Ner2v2 = sum(Sper2v2, Sner2v2, Eer3v2, Aer3v2, Ier3v2, Her3v2, Rper3v2, Rner3v2) #vax 2 doseand two prior
    
    Ner0v3 = sum(Sper0v3, Vper0v3, Vner0v3,Sner0v3, Eer1v3, Aer1v3, Ier1v3, Her1v3, Rper1v3, Rner1v3) #vax 3 dose and no prior exposure
    Ner1v3 = sum(Sper1v3, Sner1v3, Eer2v3, Aer2v3, Ier2v3, Her2v3, Rper2v3, Rner2v3) #vax 3 dose and one prior
    Ner2v3 = sum(Sper2v3, Sner2v3, Eer3v3, Aer3v3, Ier3v3, Her3v3, Rper3v3, Rner3v3) #vax 3 dose and two prior
    
    #Urban elderly
    Neu0v0 = sum(Seu0v0, Eeu1v0, Aeu1v0, Ieu1v0, Heu1v0, Rpeu1v0, W1Rpeu1v0,W2Rpeu1v0,W3Rpeu1v0, Rneu1v0) #Unvax and no prior exposure
    Neu1v0 = sum(Speu1v0, Sneu1v0, Eeu2v0, Aeu2v0, Ieu2v0, Heu2v0, Rpeu2v0, Rneu2v0) #Unvax and one prior
    Neu2v0 = sum(Speu2v0, Sneu2v0, Eeu3v0, Aeu3v0, Ieu3v0, Heu3v0, Rpeu3v0, Rneu3v0) #Unvax and two prior
    
    Neu0v1 = sum(Speu0v1,Vpeu0v1, W1Vpeu0v1,W2Vpeu0v1,W3Vpeu0v1,Vneu0v1, Sneu0v1, Eeu1v1, Aeu1v1, Ieu1v1, Heu1v1, Rpeu1v1, Rneu1v1) #vax 1 dose and no prior exposure
    Neu1v1 = sum(Speu1v1, Sneu1v1, Eeu2v1, Aeu2v1, Ieu2v1, Heu2v1, Rpeu2v1, Rneu2v1) #vax 1 dose and one prior
    Neu2v1 = sum(Speu2v1, Sneu2v1, Eeu3v1, Aeu3v1, Ieu3v1, Heu3v1, Rpeu3v1, Rneu3v1) #vax 1 dose and two prior
    
    Neu0v2 = sum(Speu0v2, Vpeu0v2, Vneu0v2,Sneu0v2, Eeu1v2, Aeu1v2, Ieu1v2, Heu1v2, Rpeu1v2, Rneu1v2) #vax 2 dose and no prior exposure
    Neu1v2 = sum(Speu1v2, Sneu1v2, Eeu2v2, Aeu2v2, Ieu2v2, Heu2v2, Rpeu2v2, Rneu2v2) #vax 2 dose and one prior
    Neu2v2 = sum(Speu2v2, Sneu2v2, Eeu3v2, Aeu3v2, Ieu3v2, Heu3v2, Rpeu3v2, Rneu3v2) #vax 2 doseand two prior
    
    Neu0v3 = sum(Speu0v3, Vpeu0v3, Vneu0v3, Sneu0v3, Eeu1v3, Aeu1v3, Ieu1v3, Heu1v3, Rpeu1v3, Rneu1v3) #vax 3 dose and no prior exposure
    Neu1v3 = sum(Speu1v3, Sneu1v3, Eeu2v3, Aeu2v3, Ieu2v3, Heu2v3, Rpeu2v3, Rneu2v3) #vax 3 dose and one prior
    Neu2v3 = sum(Speu2v3, Sneu2v3, Eeu3v3, Aeu3v3, Ieu3v3, Heu3v3, Rpeu3v3, Rneu3v3) #vax 3 dose and two prior
    
    ##Population sums
    Nchildr = sum(Ncr0v0,Ncr1v0,Ncr2v0,Ncr0v1,Ncr1v1,Ncr2v1,Ncr0v2,Ncr1v2,Ncr2v2,Ncr0v3,Ncr1v3,Ncr2v3)
    Nadultr = sum(Nar0v0,Nar1v0,Nar2v0,Nar0v1,Nar1v1,Nar2v1,Nar0v2,Nar1v2,Nar2v2,Nar0v3,Nar1v3,Nar2v3)
    Noldr   = sum(Ner0v0,Ner1v0,Ner2v0,Ner0v1,Ner1v1,Ner2v1,Ner0v2,Ner1v2,Ner2v2,Ner0v3,Ner1v3,Ner2v3)
    
    Nchildu = sum(Ncu0v0,Ncu1v0,Ncu2v0,Ncu0v1,Ncu1v1,Ncu2v1,Ncu0v2,Ncu1v2,Ncu2v2,Ncu0v3,Ncu1v3,Ncu2v3)
    Nadultu = sum(Nau0v0,Nau1v0,Nau2v0,Nau0v1,Nau1v1,Nau2v1,Nau0v2,Nau1v2,Nau2v2,Nau0v3,Nau1v3,Nau2v3)
    Noldu   = sum(Neu0v0,Neu1v0,Neu2v0,Neu0v1,Neu1v1,Neu2v1,Neu0v2,Neu1v2,Neu2v2,Neu0v3,Neu1v3,Neu2v3)
    
    ## Vax proportions in each of the 6 population stratas
    ## Assumptions until we can get true vax over time by age group
    p_d1_cr = (Ncr0v1+Ncr1v1+Ncr2v1+Ncr0v2+Ncr1v2+Ncr2v2+Ncr0v3+Ncr1v3+Ncr2v3)/Nchildr # Proportion of rural children receiving at least one dose of vaccine
    p_d1_cu = (Ncu0v1+Ncu1v1+Ncu2v1+Ncu0v2+Ncu1v2+Ncu2v2+Ncu0v3+Ncu1v3+Ncu2v3)/Nchildu # Prop urban children receiving at least one dose
    p_d1_ar = (Nar0v1+Nar1v1+Nar2v1+Nar0v2+Nar1v2+Nar2v2+Nar0v3+Nar1v3+Nar2v3)/Nadultr
    p_d1_au = (Nau0v1+Nau1v1+Nau2v1+Nau0v2+Nau1v2+Nau2v2+Nau0v3+Nau1v3+Nau2v3)/Nadultu
    p_d1_er = (Ner0v1+Ner1v1+Ner2v1+Ner0v2+Ner1v2+Ner2v2+Ner0v3+Ner1v3+Ner2v3)/Noldr
    p_d1_eu = (Neu0v1+Neu1v1+Neu2v1+Neu0v2+Neu1v2+Neu2v2+Neu0v3+Neu1v3+Neu2v3)/Noldu
    
    p_d2_cr = (Ncr0v2+Ncr1v2+Ncr2v2+Ncr0v3+Ncr1v3+Ncr2v3)/Nchildr # Proportion of rural children receiving at least two dose of vaccine
    p_d2_cu = (Ncu0v2+Ncu1v2+Ncu2v2+Ncu0v3+Ncu1v3+Ncu2v3)/Nchildu # Prop urban children receiving at least two dose
    p_d2_ar = (Nar0v2+Nar1v2+Nar2v2+Nar0v3+Nar1v3+Nar2v3)/Nadultr
    p_d2_au = (Nau0v2+Nau1v2+Nau2v2+Nau0v3+Nau1v3+Nau2v3)/Nadultu
    p_d2_er = (Ner0v2+Ner1v2+Ner2v2+Ner0v3+Ner1v3+Ner2v3)/Noldr
    p_d2_eu = (Neu0v2+Neu1v2+Neu2v2+Neu0v3+Neu1v3+Neu2v3)/Noldu   
    
    p_d1_c = (Ncr0v1+Ncr1v1+Ncr2v1+Ncr0v2+Ncr1v2+Ncr2v2+Ncr0v3+Ncr1v3+Ncr2v3+Ncu0v1+Ncu1v1+Ncu2v1+Ncu0v2+Ncu1v2+Ncu2v2+Ncu0v3+Ncu1v3+Ncu2v3)/(Nchildr+Nchildu)
    p_d1_a = (Nar0v1+Nar1v1+Nar2v1+Nar0v2+Nar1v2+Nar2v2+Nar0v3+Nar1v3+Nar2v3+Nau0v1+Nau1v1+Nau2v1+Nau0v2+Nau1v2+Nau2v2+Nau0v3+Nau1v3+Nau2v3)/(Nadultr+Nadultu)
    p_d1_e = (Ner0v1+Ner1v1+Ner2v1+Ner0v2+Ner1v2+Ner2v2+Ner0v3+Ner1v3+Ner2v3+Neu0v1+Neu1v1+Neu2v1+Neu0v2+Neu1v2+Neu2v2+Neu0v3+Neu1v3+Neu2v3)/(Noldr + Noldu)
    
    p_d2_c = (Ncr0v2+Ncr1v2+Ncr2v2+Ncr0v3+Ncr1v3+Ncr2v3+Ncu0v2+Ncu1v2+Ncu2v2+Ncu0v3+Ncu1v3+Ncu2v3)/(Nchildr+Nchildu)
    p_d2_a = (Nar0v2+Nar1v2+Nar2v2+Nar0v3+Nar1v3+Nar2v3+Nau0v2+Nau1v2+Nau2v2+Nau0v3+Nau1v3+Nau2v3)/(Nadultr+Nadultu)
    p_d2_e = (Ner0v2+Ner1v2+Ner2v2+Ner0v3+Ner1v3+Ner2v3+Neu0v2+Neu1v2+Neu2v2+Neu0v3+Neu1v3+Neu2v3)/(Noldr+Noldu)
    
    ##Seroprev in each of the 6 population stratas
    #sero_cr <<-
    #sero_cu
    #sero_ar
    #sero_au
    #sero_er
    #sero_eu
    
    
    if(t<306){     ##2021-3-9
      delta1_cr<-0; delta1_cu<-0; delta1_ar<-0; delta1_au<-0; delta1_er <- 0; delta1_eu <-0   
      delta2_cr<-0; delta2_cu<-0; delta2_ar<-0; delta2_au<-0; delta2_er <- 0; delta2_eu <-0 
    } else if (t<322){   ##2021-3-9 to 2021-3-25
      delta1_cr<-0; delta1_cu<-0; delta1_ar<-0.0003; delta1_au<-0.0004; delta1_er <- 0.0004; delta1_eu <-0.0004   
      delta2_cr<-0; delta2_cu<-0; delta2_ar<-0; delta2_au<-0; delta2_er <- 0; delta2_eu <-0 
    } else if (t<542){   ##2021-3-25 to 2021-10-31
      delta1_cr<-0; delta1_cu<-0; delta1_ar<-0.0003; delta1_au<-0.0008; delta1_er <- 0.0006; delta1_eu <-0.0008  
      delta2_cr<-0; delta2_cu<-0; delta2_ar<-0.001; delta2_au<-0.001; delta2_er <- 0.001; delta2_eu <-0.01 
    } else if (t<694){   ##2021-10-31 to 2022-4-1
      delta1_cr<-0; delta1_cu<-0; delta1_ar<-0.013; delta1_au<-0.011; delta1_er <- 0.012; delta1_eu <-0.011  
      delta2_cr<-0; delta2_cu<-0; delta2_ar<-0.022; delta2_au<-0.022; delta2_er <- 0.022; delta2_eu <-0.022 
    } else if (t<=847){  ## 2022-4-1 to 2022-9-1
      delta1_cr<-0; delta1_cu<-0; delta1_ar<-0.007; delta1_au<-0.007; delta1_er <- 0.007; delta1_eu <-0.007  
      delta2_cr<-0; delta2_cu<-0; delta2_ar<-0.012; delta2_au<-0.012; delta2_er <- 0.012; delta2_eu <-0.012
    } else {
      delta1_cr<-0; delta1_cu<-0; delta1_ar<-0; delta1_au<-0; delta1_er <- 0; delta1_eu <-0  
      delta2_cr<-0; delta2_cu<-0; delta2_ar<-0; delta2_au<-0; delta2_er <- 0; delta2_eu <-0
    }
    
    
    #if(t<306){     ##2021-3-9
    #  delta1_cr<-0; delta1_cu<-0; delta1_ar<-0; delta1_au<-0; delta1_er <- 0; delta1_eu <-0   
    #  delta2_cr<-0; delta2_cu<-0; delta2_ar<-0; delta2_au<-0; delta2_er <- 0; delta2_eu <-0 
    #} else if (t<322){   ##2021-3-9 to 2021-3-25
    #  delta1_cr<-0; delta1_cu<-0; delta1_ar<-0.000074; delta1_au<-0.000279; delta1_er <- 0.0003; delta1_eu <-0.0003   
    #  delta2_cr<-0; delta2_cu<-0; delta2_ar<-0; delta2_au<-0; delta2_er <- 0; delta2_eu <-0 
    #} else if (t<372){    ##2021-3-25 to 2021-5-14
    #  delta1_cr<-0; delta1_cu<-0; delta1_ar<-0.0000694; delta1_au<-0.0008; delta1_er <- 0.0002; delta1_eu <-0.0002  
    #  delta2_cr<-0; delta2_cu<-0; delta2_ar<-0.00768; delta2_au<-0.009747; delta2_er <- 0.00767; delta2_eu <-0.007677 
    #} else if (t<477){    ##2021-5-14 to 2021-8-27
    #  delta1_cr<-0; delta1_cu<-0; delta1_ar<-0.000171; delta1_au<-0.005; delta1_er <- 0.002; delta1_eu <-0.006   
    #  delta2_cr<-0; delta2_cu<-0; delta2_ar<-0.0062202; delta2_au<-0.012202; delta2_er <- 0.0122; delta2_eu <-0.012202 
    #} else if (t<540){   ##2021-8-27 to 2021-10-29
    #  delta1_cr<-0; delta1_cu<-0; delta1_ar<-0.00053; delta1_au<-0.00082; delta1_er <- 0.002; delta1_eu <-0.001  
    #  delta2_cr<-0; delta2_cu<-0; delta2_ar<-0.01; delta2_au<-0.012; delta2_er <- 0.015; delta2_eu <-0.016 
    #} else if (t<639){   ##2021-10-29 to 2022-2-5
    #  delta1_cr<-0; delta1_cu<-0; delta1_ar<-0.02; delta1_au<-0.025; delta1_er <- 0.016; delta1_eu <-0.016  
    #  delta2_cr<-0; delta2_cu<-0; delta2_ar<-0.018; delta2_au<-0.01; delta2_er <- 0.02; delta2_eu <-0.016 
    #} else if (t<=833){
    #  delta1_cr<-0; delta1_cu<-0; delta1_ar<-0.01; delta1_au<-0.0001; delta1_er <- 0.01; delta1_eu <-0.01  
    #  delta2_cr<-0; delta2_cu<-0; delta2_ar<-0.016; delta2_au<-0.02; delta2_er <- 0.015; delta2_eu <-0.01
    #} else {
    #  delta1_cr<-0; delta1_cu<-0; delta1_ar<-0; delta1_au<-0; delta1_er <- 0; delta1_eu <-0  
    #  delta2_cr<-0; delta2_cu<-0; delta2_ar<-0; delta2_au<-0; delta2_er <- 0; delta2_eu <-0
    #}
    
    
    #delta1_cr<-0; delta1_cu<-0; delta1_ar<-0; delta1_au<-0; delta1_er <- 0; delta1_eu <-0   
    #delta2_cr<-0; delta2_cu<-0; delta2_ar<-0; delta2_au<-0; delta2_er <- 0; delta2_eu <-0   
    delta3_cr<-0; delta3_cu<-0; delta3_ar<-0; delta3_au<-0; delta3_er <- 0; delta3_eu <-0    
    delta4_cr<-0; delta4_cu<-0; delta4_ar<-0; delta4_au<-0; delta4_er <- 0; delta4_eu <-0    
    
    ##Social distancing stuff
    if (t>=0 & t<=100){      ## R0 115=2020-8-30
      sd<-1
    } else if(t>100 & t<=232){  ## 115=2020-8-30; 239=2021-1-1
      #sd <- 0.5   
      sd <- 0.95/r0
    } else if(t>232 & t<=262){  ## 239=2021-1-1; 267=2021-1-29
      sd <- 2.5/r0
    } else if (t>262& t<=288){  ## 257=2021-1-29; 288=2021-2-19
      sd <- 1.4/r0
    } else if (t>288& t<=396){  ##288=2021-2-19; 405=2021-6-6
      sd<-0.9/r0
    } else if (t>396& t<=430){  ##405=2021-6-6; 430=2021-7-11
      sd <- rel_delta
    }else if(t>430& t<=454){   #430=2021-7-11; 454=2021-8-4
      sd <- 2.5/r0
    }  else if(t>454 &t<=565){ #454=2021-8-4; 565=2021-11-23
      sd <-1.16/r0
    } else if(t>565 & t<=611){ #565=2021-11-23; #611
      sd<-rel_omi
       #sd <-3.8
    } else if(t>611 & t<= 620){
      sd<- rel_omi/1.7
    } else if(t>620 & t<=710){
      sd <- 1.5/r0
    } else if(t>710 & t<=814){
      sd <- r0_hyp/r0   ##Ratio of hypothetical r0 to r0 to be inputted as a scaling factor into the model
    } else {
      
      if (((t-814)/365)%%1>(270/365)){
        sd <- (r0_hyp*1.2)/r0
      } else {
        sd <- 1.8/r0
      }
      #sd <- rel_newvar ##keep on for now and see what happens
    }
    
    ### imunne escape factor for those with either prior exposur or prior vaccination
    if(t>=0 & t<=405){
      imm_esc_factor <- 1
    } else if (t>405 & t<=565){
      imm_esc_factor <- imm_esc_factor_t1
    } else if (t>565 & t<= 922){
      imm_esc_factor <- imm_esc_factor_omi ##omicron
    } else {
      imm_esc_factor <- imm_esc_factor_omi
    }
    
    #StartODES
    # Force of infection
    # Foi_cr: Interpreted as FOI exerted on child rural susceptibles
    foi_cr_0v0 =  beta_c * ((CM[1,1]*(Icr1v0 +Icr2v0 +Icr3v0 +Icr1v1 +Icr2v1 +Icr3v1 + Icr1v2 +Icr2v2 +Icr3v2 +Icr1v3 +Icr2v3 +Icr3v3 + alpha*(Acr1v0 +Acr2v0 +Acr3v0 +Acr1v1 +Acr2v1 +Acr3v1 + Acr1v2 +Acr2v2 +Acr3v2 +Acr1v3 +Acr2v3 +Acr3v3))/Nchildr) + # first part is rural infectious -> rural suscepible
                              (CM[2,1]*(Iar1v0 +Iar2v0 +Iar3v0 +Iar1v1 +Iar2v1 +Iar3v1 + Iar1v2 +Iar2v2 +Iar3v2 +Iar1v3 +Iar2v3 +Iar3v3 + alpha*(Aar1v0 +Aar2v0 +Aar3v0 +Aar1v1 +Aar2v1 +Aar3v1 + Aar1v2 +Aar2v2 +Aar3v2 +Aar1v3 +Aar2v3 +Aar3v3))/Nadultr) +
                              (CM[3,1]*(Ier1v0 +Ier2v0 +Ier3v0 +Ier1v1 +Ier2v1 +Ier3v1 + Ier1v2 +Ier2v2 +Ier3v2 +Ier1v3 +Ier2v3 +Ier3v3 + alpha*(Aer1v0 +Aer2v0 +Aer3v0 +Aer1v1 +Aer2v1 +Aer3v1 + Aer1v2 +Aer2v2 +Aer3v2 +Aer1v3 +Aer2v3 +Aer3v3))/Noldr) +
                              #second part is urban infectious -> rural susceptible
                              (CM[4,1]*(Icu1v0 +Icu2v0 +Icu3v0 +Icu1v1 +Icu2v1 +Icu3v1 + Icu1v2 +Icu2v2 +Icu3v2 +Icu1v3 +Icu2v3 +Icu3v3 + alpha*(Acu1v0 +Acu2v0 +Acu3v0 +Acu1v1 +Acu2v1 +Acu3v1 + Acu1v2 +Acu2v2 +Acu3v2 +Acu1v3 +Acu2v3 +Acu3v3))/Nchildu) + 
                              (CM[5,1]*(Iau1v0 +Iau2v0 +Iau3v0 +Iau1v1 +Iau2v1 +Iau3v1 + Iau1v2 +Iau2v2 +Iau3v2 +Iau1v3 +Iau2v3 +Iau3v3 + alpha*(Aau1v0 +Aau2v0 +Aau3v0 +Aau1v1 +Aau2v1 +Aau3v1 + Aau1v2 +Aau2v2 +Aau3v2 +Aau1v3 +Aau2v3 +Aau3v3))/Nadultu) +
                              (CM[6,1]*(Ieu1v0 +Ieu2v0 +Ieu3v0 +Ieu1v1 +Ieu2v1 +Ieu3v1 + Ieu1v2 +Ieu2v2 +Ieu3v2 +Ieu1v3 +Ieu2v3 +Ieu3v3 + alpha*(Aeu1v0 +Aeu2v0 +Aeu3v0 +Aeu1v1 +Aeu2v1 +Aeu3v1 + Aeu1v2 +Aeu2v2 +Aeu3v2 +Aeu1v3 +Aeu2v3 +Aeu3v3))/Noldu)) 
    
    foi_ar_0v0 = beta_a * ((CM[1,2]*(Icr1v0 +Icr2v0 +Icr3v0 +Icr1v1 +Icr2v1 +Icr3v1 + Icr1v2 +Icr2v2 +Icr3v2 +Icr1v3 +Icr2v3 +Icr3v3 + alpha*(Acr1v0 +Acr2v0 +Acr3v0 +Acr1v1 +Acr2v1 +Acr3v1 + Acr1v2 +Acr2v2 +Acr3v2 +Acr1v3 +Acr2v3 +Acr3v3))/Nchildr) + # first part is rural infectious -> rural suscepible
                             (CM[2,2]*(Iar1v0 +Iar2v0 +Iar3v0 +Iar1v1 +Iar2v1 +Iar3v1 + Iar1v2 +Iar2v2 +Iar3v2 +Iar1v3 +Iar2v3 +Iar3v3 + alpha*(Aar1v0 +Aar2v0 +Aar3v0 +Aar1v1 +Aar2v1 +Aar3v1 + Aar1v2 +Aar2v2 +Aar3v2 +Aar1v3 +Aar2v3 +Aar3v3))/Nadultr) +
                             (CM[3,2]*(Ier1v0 +Ier2v0 +Ier3v0 +Ier1v1 +Ier2v1 +Ier3v1 + Ier1v2 +Ier2v2 +Ier3v2 +Ier1v3 +Ier2v3 +Ier3v3 + alpha*(Aer1v0 +Aer2v0 +Aer3v0 +Aer1v1 +Aer2v1 +Aer3v1 + Aer1v2 +Aer2v2 +Aer3v2 +Aer1v3 +Aer2v3 +Aer3v3))/Noldr) +
                             #second part is urban infectious -> rural susceptible
                             (CM[4,2]*(Icu1v0 +Icu2v0 +Icu3v0 +Icu1v1 +Icu2v1 +Icu3v1 + Icu1v2 +Icu2v2 +Icu3v2 +Icu1v3 +Icu2v3 +Icu3v3 + alpha*(Acu1v0 +Acu2v0 +Acu3v0 +Acu1v1 +Acu2v1 +Acu3v1 + Acu1v2 +Acu2v2 +Acu3v2 +Acu1v3 +Acu2v3 +Acu3v3))/Nchildu) + 
                             (CM[5,2]*(Iau1v0 +Iau2v0 +Iau3v0 +Iau1v1 +Iau2v1 +Iau3v1 + Iau1v2 +Iau2v2 +Iau3v2 +Iau1v3 +Iau2v3 +Iau3v3 + alpha*(Aau1v0 +Aau2v0 +Aau3v0 +Aau1v1 +Aau2v1 +Aau3v1 + Aau1v2 +Aau2v2 +Aau3v2 +Aau1v3 +Aau2v3 +Aau3v3))/Nadultu) +
                             (CM[6,2]*(Ieu1v0 +Ieu2v0 +Ieu3v0 +Ieu1v1 +Ieu2v1 +Ieu3v1 + Ieu1v2 +Ieu2v2 +Ieu3v2 +Ieu1v3 +Ieu2v3 +Ieu3v3 + alpha*(Aeu1v0 +Aeu2v0 +Aeu3v0 +Aeu1v1 +Aeu2v1 +Aeu3v1 + Aeu1v2 +Aeu2v2 +Aeu3v2 +Aeu1v3 +Aeu2v3 +Aeu3v3))/Noldu)) 
    
    foi_er_0v0 = beta_e * ((CM[1,3]*(Icr1v0 +Icr2v0 +Icr3v0 +Icr1v1 +Icr2v1 +Icr3v1 + Icr1v2 +Icr2v2 +Icr3v2 +Icr1v3 +Icr2v3 +Icr3v3 + alpha*(Acr1v0 +Acr2v0 +Acr3v0 +Acr1v1 +Acr2v1 +Acr3v1 + Acr1v2 +Acr2v2 +Acr3v2 +Acr1v3 +Acr2v3 +Acr3v3))/Nchildr) + # first part is rural infectious -> rural suscepible
                             (CM[2,3]*(Iar1v0 +Iar2v0 +Iar3v0 +Iar1v1 +Iar2v1 +Iar3v1 + Iar1v2 +Iar2v2 +Iar3v2 +Iar1v3 +Iar2v3 +Iar3v3 + alpha*(Aar1v0 +Aar2v0 +Aar3v0 +Aar1v1 +Aar2v1 +Aar3v1 + Aar1v2 +Aar2v2 +Aar3v2 +Aar1v3 +Aar2v3 +Aar3v3))/Nadultr) +
                             (CM[3,3]*(Ier1v0 +Ier2v0 +Ier3v0 +Ier1v1 +Ier2v1 +Ier3v1 + Ier1v2 +Ier2v2 +Ier3v2 +Ier1v3 +Ier2v3 +Ier3v3 + alpha*(Aer1v0 +Aer2v0 +Aer3v0 +Aer1v1 +Aer2v1 +Aer3v1 + Aer1v2 +Aer2v2 +Aer3v2 +Aer1v3 +Aer2v3 +Aer3v3))/Noldr) +
                             #second part is urban infectious -> rural susceptible
                             (CM[4,3]*(Icu1v0 +Icu2v0 +Icu3v0 +Icu1v1 +Icu2v1 +Icu3v1 + Icu1v2 +Icu2v2 +Icu3v2 +Icu1v3 +Icu2v3 +Icu3v3 + alpha*(Acu1v0 +Acu2v0 +Acu3v0 +Acu1v1 +Acu2v1 +Acu3v1 + Acu1v2 +Acu2v2 +Acu3v2 +Acu1v3 +Acu2v3 +Acu3v3))/Nchildu) + 
                             (CM[5,3]*(Iau1v0 +Iau2v0 +Iau3v0 +Iau1v1 +Iau2v1 +Iau3v1 + Iau1v2 +Iau2v2 +Iau3v2 +Iau1v3 +Iau2v3 +Iau3v3 + alpha*(Aau1v0 +Aau2v0 +Aau3v0 +Aau1v1 +Aau2v1 +Aau3v1 + Aau1v2 +Aau2v2 +Aau3v2 +Aau1v3 +Aau2v3 +Aau3v3))/Nadultu) +
                             (CM[6,3]*(Ieu1v0 +Ieu2v0 +Ieu3v0 +Ieu1v1 +Ieu2v1 +Ieu3v1 + Ieu1v2 +Ieu2v2 +Ieu3v2 +Ieu1v3 +Ieu2v3 +Ieu3v3 + alpha*(Aeu1v0 +Aeu2v0 +Aeu3v0 +Aeu1v1 +Aeu2v1 +Aeu3v1 + Aeu1v2 +Aeu2v2 +Aeu3v2 +Aeu1v3 +Aeu2v3 +Aeu3v3))/Noldu)) 
    
    foi_cu_0v0 = beta_c * ((CM[1,4]*(Icr1v0 +Icr2v0 +Icr3v0 +Icr1v1 +Icr2v1 +Icr3v1 + Icr1v2 +Icr2v2 +Icr3v2 +Icr1v3 +Icr2v3 +Icr3v3 + alpha*(Acr1v0 +Acr2v0 +Acr3v0 +Acr1v1 +Acr2v1 +Acr3v1 + Acr1v2 +Acr2v2 +Acr3v2 +Acr1v3 +Acr2v3 +Acr3v3))/Nchildr) + # first part is rural infectious -> rural suscepible
                             (CM[2,4]*(Iar1v0 +Iar2v0 +Iar3v0 +Iar1v1 +Iar2v1 +Iar3v1 + Iar1v2 +Iar2v2 +Iar3v2 +Iar1v3 +Iar2v3 +Iar3v3 + alpha*(Aar1v0 +Aar2v0 +Aar3v0 +Aar1v1 +Aar2v1 +Aar3v1 + Aar1v2 +Aar2v2 +Aar3v2 +Aar1v3 +Aar2v3 +Aar3v3))/Nadultr) +
                             (CM[3,4]*(Ier1v0 +Ier2v0 +Ier3v0 +Ier1v1 +Ier2v1 +Ier3v1 + Ier1v2 +Ier2v2 +Ier3v2 +Ier1v3 +Ier2v3 +Ier3v3 + alpha*(Aer1v0 +Aer2v0 +Aer3v0 +Aer1v1 +Aer2v1 +Aer3v1 + Aer1v2 +Aer2v2 +Aer3v2 +Aer1v3 +Aer2v3 +Aer3v3))/Noldr) +
                             #second part is urban infectious -> urban susceptible
                             (CM[4,4]*(Icu1v0 +Icu2v0 +Icu3v0 +Icu1v1 +Icu2v1 +Icu3v1 + Icu1v2 +Icu2v2 +Icu3v2 +Icu1v3 +Icu2v3 +Icu3v3 + alpha*(Acu1v0 +Acu2v0 +Acu3v0 +Acu1v1 +Acu2v1 +Acu3v1 + Acu1v2 +Acu2v2 +Acu3v2 +Acu1v3 +Acu2v3 +Acu3v3))/Nchildu) + 
                             (CM[5,4]*(Iau1v0 +Iau2v0 +Iau3v0 +Iau1v1 +Iau2v1 +Iau3v1 + Iau1v2 +Iau2v2 +Iau3v2 +Iau1v3 +Iau2v3 +Iau3v3 + alpha*(Aau1v0 +Aau2v0 +Aau3v0 +Aau1v1 +Aau2v1 +Aau3v1 + Aau1v2 +Aau2v2 +Aau3v2 +Aau1v3 +Aau2v3 +Aau3v3))/Nadultu) +
                             (CM[6,4]*(Ieu1v0 +Ieu2v0 +Ieu3v0 +Ieu1v1 +Ieu2v1 +Ieu3v1 + Ieu1v2 +Ieu2v2 +Ieu3v2 +Ieu1v3 +Ieu2v3 +Ieu3v3 + alpha*(Aeu1v0 +Aeu2v0 +Aeu3v0 +Aeu1v1 +Aeu2v1 +Aeu3v1 + Aeu1v2 +Aeu2v2 +Aeu3v2 +Aeu1v3 +Aeu2v3 +Aeu3v3))/Noldu)) 
    
    foi_au_0v0 = beta_a * ((CM[1,5]*(Icr1v0 +Icr2v0 +Icr3v0 +Icr1v1 +Icr2v1 +Icr3v1 + Icr1v2 +Icr2v2 +Icr3v2 +Icr1v3 +Icr2v3 +Icr3v3 + alpha*(Acr1v0 +Acr2v0 +Acr3v0 +Acr1v1 +Acr2v1 +Acr3v1 + Acr1v2 +Acr2v2 +Acr3v2 +Acr1v3 +Acr2v3 +Acr3v3))/Nchildr) + # first part is rural infectious -> rural suscepible
                             (CM[2,5]*(Iar1v0 +Iar2v0 +Iar3v0 +Iar1v1 +Iar2v1 +Iar3v1 + Iar1v2 +Iar2v2 +Iar3v2 +Iar1v3 +Iar2v3 +Iar3v3 + alpha*(Aar1v0 +Aar2v0 +Aar3v0 +Aar1v1 +Aar2v1 +Aar3v1 + Aar1v2 +Aar2v2 +Aar3v2 +Aar1v3 +Aar2v3 +Aar3v3))/Nadultr) +
                             (CM[3,5]*(Ier1v0 +Ier2v0 +Ier3v0 +Ier1v1 +Ier2v1 +Ier3v1 + Ier1v2 +Ier2v2 +Ier3v2 +Ier1v3 +Ier2v3 +Ier3v3 + alpha*(Aer1v0 +Aer2v0 +Aer3v0 +Aer1v1 +Aer2v1 +Aer3v1 + Aer1v2 +Aer2v2 +Aer3v2 +Aer1v3 +Aer2v3 +Aer3v3))/Noldr) +
                             #second part is urban infectious -> urban susceptible
                             (CM[4,5]*(Icu1v0 +Icu2v0 +Icu3v0 +Icu1v1 +Icu2v1 +Icu3v1 + Icu1v2 +Icu2v2 +Icu3v2 +Icu1v3 +Icu2v3 +Icu3v3 + alpha*(Acu1v0 +Acu2v0 +Acu3v0 +Acu1v1 +Acu2v1 +Acu3v1 + Acu1v2 +Acu2v2 +Acu3v2 +Acu1v3 +Acu2v3 +Acu3v3))/Nchildu) + 
                             (CM[5,5]*(Iau1v0 +Iau2v0 +Iau3v0 +Iau1v1 +Iau2v1 +Iau3v1 + Iau1v2 +Iau2v2 +Iau3v2 +Iau1v3 +Iau2v3 +Iau3v3 + alpha*(Aau1v0 +Aau2v0 +Aau3v0 +Aau1v1 +Aau2v1 +Aau3v1 + Aau1v2 +Aau2v2 +Aau3v2 +Aau1v3 +Aau2v3 +Aau3v3))/Nadultu) +
                             (CM[6,5]*(Ieu1v0 +Ieu2v0 +Ieu3v0 +Ieu1v1 +Ieu2v1 +Ieu3v1 + Ieu1v2 +Ieu2v2 +Ieu3v2 +Ieu1v3 +Ieu2v3 +Ieu3v3 + alpha*(Aeu1v0 +Aeu2v0 +Aeu3v0 +Aeu1v1 +Aeu2v1 +Aeu3v1 + Aeu1v2 +Aeu2v2 +Aeu3v2 +Aeu1v3 +Aeu2v3 +Aeu3v3))/Noldu)) 
    
    foi_eu_0v0 = beta_e *  ((CM[1,6]*(Icr1v0 +Icr2v0 +Icr3v0 +Icr1v1 +Icr2v1 +Icr3v1 + Icr1v2 +Icr2v2 +Icr3v2 +Icr1v3 +Icr2v3 +Icr3v3 + alpha*(Acr1v0 +Acr2v0 +Acr3v0 +Acr1v1 +Acr2v1 +Acr3v1 + Acr1v2 +Acr2v2 +Acr3v2 +Acr1v3 +Acr2v3 +Acr3v3))/Nchildr) + # first part is rural infectious -> rural suscepible
                              (CM[2,6]*(Iar1v0 +Iar2v0 +Iar3v0 +Iar1v1 +Iar2v1 +Iar3v1 + Iar1v2 +Iar2v2 +Iar3v2 +Iar1v3 +Iar2v3 +Iar3v3 + alpha*(Aar1v0 +Aar2v0 +Aar3v0 +Aar1v1 +Aar2v1 +Aar3v1 + Aar1v2 +Aar2v2 +Aar3v2 +Aar1v3 +Aar2v3 +Aar3v3))/Nadultr) +
                              (CM[3,6]*(Ier1v0 +Ier2v0 +Ier3v0 +Ier1v1 +Ier2v1 +Ier3v1 + Ier1v2 +Ier2v2 +Ier3v2 +Ier1v3 +Ier2v3 +Ier3v3 + alpha*(Aer1v0 +Aer2v0 +Aer3v0 +Aer1v1 +Aer2v1 +Aer3v1 + Aer1v2 +Aer2v2 +Aer3v2 +Aer1v3 +Aer2v3 +Aer3v3))/Noldr) +
                              #second part is urban infectious -> urban susceptible
                              (CM[4,6]*(Icu1v0 +Icu2v0 +Icu3v0 +Icu1v1 +Icu2v1 +Icu3v1 + Icu1v2 +Icu2v2 +Icu3v2 +Icu1v3 +Icu2v3 +Icu3v3 + alpha*(Acu1v0 +Acu2v0 +Acu3v0 +Acu1v1 +Acu2v1 +Acu3v1 + Acu1v2 +Acu2v2 +Acu3v2 +Acu1v3 +Acu2v3 +Acu3v3))/Nchildu) + 
                              (CM[5,6]*(Iau1v0 +Iau2v0 +Iau3v0 +Iau1v1 +Iau2v1 +Iau3v1 + Iau1v2 +Iau2v2 +Iau3v2 +Iau1v3 +Iau2v3 +Iau3v3 + alpha*(Aau1v0 +Aau2v0 +Aau3v0 +Aau1v1 +Aau2v1 +Aau3v1 + Aau1v2 +Aau2v2 +Aau3v2 +Aau1v3 +Aau2v3 +Aau3v3))/Nadultu) +
                              (CM[6,6]*(Ieu1v0 +Ieu2v0 +Ieu3v0 +Ieu1v1 +Ieu2v1 +Ieu3v1 + Ieu1v2 +Ieu2v2 +Ieu3v2 +Ieu1v3 +Ieu2v3 +Ieu3v3 + alpha*(Aeu1v0 +Aeu2v0 +Aeu3v0 +Aeu1v1 +Aeu2v1 +Aeu3v1 + Aeu1v2 +Aeu2v2 +Aeu3v2 +Aeu1v3 +Aeu2v3 +Aeu3v3))/Noldu)) 
    
    foi_cr_0v1 = foi_cr_0v0 * (1-vei1) *imm_esc_factor
    foi_cu_0v1 = foi_cu_0v0 * (1-vei1) *imm_esc_factor
    foi_ar_0v1 = foi_ar_0v0 * (1-vei1) *imm_esc_factor
    foi_au_0v1 = foi_au_0v0 * (1-vei1) *imm_esc_factor
    foi_er_0v1 = foi_er_0v0 * (1-vei1) *imm_esc_factor
    foi_eu_0v1 = foi_eu_0v0 * (1-vei1) *imm_esc_factor
    
    foi_cr_0v2 = foi_cr_0v0 * (1-vei2) *imm_esc_factor
    foi_cu_0v2 = foi_cu_0v0 * (1-vei2) *imm_esc_factor
    foi_ar_0v2 = foi_ar_0v0 * (1-vei2) *imm_esc_factor
    foi_au_0v2 = foi_au_0v0 * (1-vei2) *imm_esc_factor
    foi_er_0v2 = foi_er_0v0 * (1-vei2) *imm_esc_factor
    foi_eu_0v2 = foi_eu_0v0 * (1-vei2) *imm_esc_factor
    
    foi_cr_0v3 = foi_cr_0v0 * (1-vei3) *imm_esc_factor
    foi_cu_0v3 = foi_cu_0v0 * (1-vei3) *imm_esc_factor
    foi_ar_0v3 = foi_ar_0v0 * (1-vei3) *imm_esc_factor
    foi_au_0v3 = foi_au_0v0 * (1-vei3) *imm_esc_factor
    foi_er_0v3 = foi_er_0v0 * (1-vei3) *imm_esc_factor
    foi_eu_0v3 = foi_eu_0v0 * (1-vei3) *imm_esc_factor
    
    ### After first exposure
    foi_cr_1v0 = foi_cr_0v0 * red_inf_1 *imm_esc_factor
    foi_cu_1v0 = foi_cu_0v0 * red_inf_1 *imm_esc_factor
    foi_ar_1v0 = foi_ar_0v0 * red_inf_1 *imm_esc_factor
    foi_au_1v0 = foi_au_0v0 * red_inf_1 *imm_esc_factor
    foi_er_1v0 = foi_er_0v0 * red_inf_1 *imm_esc_factor
    foi_eu_1v0 = foi_eu_0v0 * red_inf_1 *imm_esc_factor
    
    foi_cr_1v1 = foi_cr_0v1 * red_inf_1 *imm_esc_factor
    foi_cu_1v1 = foi_cu_0v1 * red_inf_1 *imm_esc_factor
    foi_ar_1v1 = foi_ar_0v1 * red_inf_1 *imm_esc_factor
    foi_au_1v1 = foi_au_0v1 * red_inf_1 *imm_esc_factor
    foi_er_1v1 = foi_er_0v1 * red_inf_1 *imm_esc_factor
    foi_eu_1v1 = foi_eu_0v1 * red_inf_1 *imm_esc_factor
    
    foi_cr_1v2 = foi_cr_0v2 * red_inf_1 *imm_esc_factor
    foi_cu_1v2 = foi_cu_0v2 * red_inf_1 *imm_esc_factor
    foi_ar_1v2 = foi_ar_0v2 * red_inf_1 *imm_esc_factor
    foi_au_1v2 = foi_au_0v2 * red_inf_1 *imm_esc_factor
    foi_er_1v2 = foi_er_0v2 * red_inf_1 *imm_esc_factor
    foi_eu_1v2 = foi_eu_0v2 * red_inf_1 *imm_esc_factor
    
    foi_cr_1v3 = foi_cr_0v3 * red_inf_1 *imm_esc_factor 
    foi_cu_1v3 = foi_cu_0v3 * red_inf_1 *imm_esc_factor
    foi_ar_1v3 = foi_ar_0v3 * red_inf_1 *imm_esc_factor
    foi_au_1v3 = foi_au_0v3 * red_inf_1 *imm_esc_factor
    foi_er_1v3 = foi_er_0v3 * red_inf_1 *imm_esc_factor
    foi_eu_1v3 = foi_eu_0v3 * red_inf_1 *imm_esc_factor
    
    ### After second exposure
    foi_cr_2v0 = foi_cr_0v0 * red_inf_2 *imm_esc_factor
    foi_cu_2v0 = foi_cu_0v0 * red_inf_2*imm_esc_factor
    foi_ar_2v0 = foi_ar_0v0 * red_inf_2*imm_esc_factor
    foi_au_2v0 = foi_au_0v0 * red_inf_2*imm_esc_factor
    foi_er_2v0 = foi_er_0v0 * red_inf_2*imm_esc_factor
    foi_eu_2v0 = foi_eu_0v0 * red_inf_2*imm_esc_factor
    
    foi_cr_2v1 = foi_cr_0v1 * red_inf_2 *imm_esc_factor
    foi_cu_2v1 = foi_cu_0v1 * red_inf_2*imm_esc_factor
    foi_ar_2v1 = foi_ar_0v1 * red_inf_2*imm_esc_factor
    foi_au_2v1 = foi_au_0v1 * red_inf_2*imm_esc_factor
    foi_er_2v1 = foi_er_0v1 * red_inf_2*imm_esc_factor
    foi_eu_2v1 = foi_eu_0v1 * red_inf_2*imm_esc_factor
    
    foi_cr_2v2 = foi_cr_0v2 * red_inf_2*imm_esc_factor
    foi_cu_2v2 = foi_cu_0v2 * red_inf_2*imm_esc_factor
    foi_ar_2v2 = foi_ar_0v2 * red_inf_2*imm_esc_factor
    foi_au_2v2 = foi_au_0v2 * red_inf_2*imm_esc_factor
    foi_er_2v2 = foi_er_0v2 * red_inf_2*imm_esc_factor
    foi_eu_2v2 = foi_eu_0v2 * red_inf_2*imm_esc_factor
    
    foi_cr_2v3 = foi_cr_0v3 * red_inf_2*imm_esc_factor
    foi_cu_2v3 = foi_cu_0v3 * red_inf_2*imm_esc_factor
    foi_ar_2v3 = foi_ar_0v3 * red_inf_2*imm_esc_factor
    foi_au_2v3 = foi_au_0v3 * red_inf_2*imm_esc_factor
    foi_er_2v3 = foi_er_0v3 * red_inf_2*imm_esc_factor
    foi_eu_2v3 = foi_eu_0v3 * red_inf_2*imm_esc_factor
    
    ################################
    ##Unvaccinated and unexposed####
    ################################
    #Susceptible
    dScr0v0  = -Scr0v0 * sd * foi_cr_0v0 - delta1_cr * Scr0v0  
    dScu0v0  = -Scu0v0 * sd * foi_cu_0v0 - delta1_cu * Scu0v0 
    dSar0v0  = -Sar0v0 * sd * foi_ar_0v0 - delta1_ar * Sar0v0 
    dSau0v0  = -Sau0v0 * sd * foi_au_0v0 - delta1_au * Sau0v0 
    dSer0v0  = -Ser0v0 * sd * foi_er_0v0 - delta1_er * Ser0v0 
    dSeu0v0  = -Seu0v0 * sd * foi_eu_0v0 - delta1_eu * Seu0v0 
    
    #Exposed
    dEcr1v0  =  Scr0v0 * sd * foi_cr_0v0 -  sigma*Ecr1v0  - delta1_cr*Ecr1v0
    dEcu1v0  =  Scu0v0 * sd * foi_cu_0v0 -  sigma*Ecu1v0  - delta1_cu*Ecu1v0
    dEar1v0  =  Sar0v0 * sd * foi_ar_0v0 -  sigma*Ear1v0  - delta1_ar*Ear1v0
    dEau1v0  =  Sau0v0 * sd * foi_au_0v0 -  sigma*Eau1v0  - delta1_au*Eau1v0
    dEer1v0  =  Ser0v0 * sd * foi_er_0v0 -  sigma*Eer1v0  - delta1_er*Eer1v0
    dEeu1v0  =  Seu0v0 * sd * foi_eu_0v0 -  sigma*Eeu1v0  - delta1_eu*Eeu1v0
    
    # Asymptomatic
    dAcr1v0  =  (1-nu_c)*sigma*Ecr1v0 - gamma_A * Acr1v0 - delta1_cr*Acr1v0
    dAcu1v0  =  (1-nu_c)*sigma*Ecu1v0 - gamma_A * Acu1v0 - delta1_cu*Acu1v0
    dAar1v0  =  (1-nu_a)*sigma*Ear1v0 - gamma_A * Aar1v0 - delta1_ar*Aar1v0
    dAau1v0  =  (1-nu_a)*sigma*Eau1v0 - gamma_A * Aau1v0 - delta1_au*Aau1v0
    dAer1v0  =  (1-nu_e)*sigma*Eer1v0 - gamma_A * Aer1v0 - delta1_er*Aer1v0
    dAeu1v0  =  (1-nu_e)*sigma*Eeu1v0 - gamma_A * Aeu1v0 - delta1_eu*Aeu1v0  
    
    #Symptomatic 
    dIcr1v0  =  nu_c*sigma*Ecr1v0 - gamma_I * Icr1v0 
    dIcu1v0  =  nu_c*sigma*Ecu1v0 - gamma_I * Icu1v0 
    dIar1v0  =  nu_a*sigma*Ear1v0 - gamma_I * Iar1v0 
    dIau1v0  =  nu_a*sigma*Eau1v0 - gamma_I * Iau1v0 
    dIer1v0  =  nu_e*sigma*Eer1v0 - gamma_I * Ier1v0
    dIeu1v0  =  nu_e*sigma*Eeu1v0 - gamma_I * Ieu1v0
    
    # Hospitalized
    dHcr1v0  =  phi_c*gamma_I*Icr1v0 - gamma_H * Hcr1v0
    dHcu1v0  =  phi_c*gamma_I*Icu1v0 - gamma_H * Hcu1v0
    dHar1v0  =  phi_a*gamma_I*Iar1v0 - gamma_H * Har1v0
    dHau1v0  =  phi_a*gamma_I*Iau1v0 - gamma_H * Hau1v0
    dHer1v0  =  phi_e*gamma_I*Ier1v0 - gamma_H * Her1v0
    dHeu1v0  =  phi_e*gamma_I*Ieu1v0 - gamma_H * Heu1v0
    
    # Recovered and seroconverted
    dRpcr1v0  =  pi*(1-phi_c)*gamma_I*Icr1v0 + pi*gamma_A*Acr1v0 + pi*(1-mu_c)*gamma_H*Hcr1v0 -4*kappa1*Rpcr1v0 - omega_pc*Rpcr1v0 - delta1_cr*Rpcr1v0
    dRpcu1v0  =  pi*(1-phi_c)*gamma_I*Icu1v0 + pi*gamma_A*Acu1v0 + pi*(1-mu_c)*gamma_H*Hcu1v0 -4*kappa1*Rpcu1v0 - omega_pc*Rpcu1v0 - delta1_cu*Rpcu1v0
    dRpar1v0  =  pi*(1-phi_a)*gamma_I*Iar1v0 + pi*gamma_A*Aar1v0 + pi*(1-mu_a)*gamma_H*Har1v0 -4*kappa1*Rpar1v0 - omega_pa*Rpar1v0 - delta1_ar*Rpar1v0 
    dRpau1v0  =  pi*(1-phi_a)*gamma_I*Iau1v0 + pi*gamma_A*Aau1v0 + pi*(1-mu_a)*gamma_H*Hau1v0 -4*kappa1*Rpau1v0 - omega_pa*Rpau1v0 - delta1_au*Rpau1v0
    dRper1v0  =  pi*(1-phi_e)*gamma_I*Ier1v0 + pi*gamma_A*Aer1v0 + pi*(1-mu_e)*gamma_H*Her1v0 -4*kappa1*Rper1v0 - omega_pe*Rper1v0 - delta1_er*Rper1v0
    dRpeu1v0  =  pi*(1-phi_e)*gamma_I*Ieu1v0 + pi*gamma_A*Aeu1v0 + pi*(1-mu_e)*gamma_H*Heu1v0 -4*kappa1*Rpeu1v0 - omega_pe*Rpeu1v0 - delta1_eu*Rpeu1v0
    
    #Recovered and not seropositive
    dRncr1v0  =  (1-pi)*(1-phi_c)*gamma_I*Icr1v0 + (1-pi)*gamma_A*Acr1v0 + (1-pi)*(1-mu_c)*gamma_H*Hcr1v0 + 4*kappa1*W3Rpcr1v0 - omega_nc*Rncr1v0 - delta1_cr*Rncr1v0
    dRncu1v0  =  (1-pi)*(1-phi_c)*gamma_I*Icu1v0 + (1-pi)*gamma_A*Acu1v0 + (1-pi)*(1-mu_c)*gamma_H*Hcu1v0 + 4*kappa1*W3Rpcu1v0 - omega_nc*Rncu1v0 - delta1_cu*Rncu1v0
    dRnar1v0  =  (1-pi)*(1-phi_a)*gamma_I*Iar1v0 + (1-pi)*gamma_A*Aar1v0 + (1-pi)*(1-mu_a)*gamma_H*Har1v0 + 4*kappa1*W3Rpar1v0 - omega_na*Rnar1v0 - delta1_ar*Rnar1v0 
    dRnau1v0  =  (1-pi)*(1-phi_a)*gamma_I*Iau1v0 + (1-pi)*gamma_A*Aau1v0 + (1-pi)*(1-mu_a)*gamma_H*Hau1v0 + 4*kappa1*W3Rpau1v0 - omega_na*Rnau1v0 - delta1_au*Rnau1v0 
    dRner1v0  =  (1-pi)*(1-phi_e)*gamma_I*Ier1v0 + (1-pi)*gamma_A*Aer1v0 + (1-pi)*(1-mu_e)*gamma_H*Her1v0 + 4*kappa1*W3Rper1v0 - omega_ne*Rner1v0 - delta1_er*Rner1v0 
    dRneu1v0  =  (1-pi)*(1-phi_e)*gamma_I*Ieu1v0 + (1-pi)*gamma_A*Aeu1v0 + (1-pi)*(1-mu_e)*gamma_H*Heu1v0 + 4*kappa1*W3Rpeu1v0 - omega_ne*Rneu1v0 - delta1_eu*Rneu1v0 
    
    #Deaths 
    dDcr1v0  =  mu_c*gamma_H*Hcr1v0
    dDcu1v0  =  mu_c*gamma_H*Hcu1v0
    dDar1v0  =  mu_a*gamma_H*Har1v0
    dDau1v0  =  mu_a*gamma_H*Hau1v0
    dDer1v0  =  mu_e*gamma_H*Her1v0
    dDeu1v0  =  mu_e*gamma_H*Heu1v0
    
    ################################
    ##Unvaccinated and exposed once####
    ################################
    #Susceptible
    dSpcr1v0  = omega_pc*Rpcr1v0 + omega_pc*W1Rpcr1v0 +omega_pc*W2Rpcr1v0 +omega_pc*W3Rpcr1v0 -kappa1*Spcr1v0 - Spcr1v0 * sd * foi_cr_1v0 - delta1_cr * Spcr1v0   ##Note vaccination rate here doesnt differ by previous vax status
    dSpcu1v0  = omega_pc*Rpcu1v0 + omega_pc*W1Rpcu1v0 +omega_pc*W2Rpcu1v0 +omega_pc*W3Rpcu1v0-kappa1*Spcu1v0 - Spcu1v0 * sd * foi_cu_1v0 - delta1_cu * Spcu1v0 
    dSpar1v0  = omega_pa*Rpar1v0 + omega_pa*W1Rpar1v0 +omega_pa*W2Rpar1v0 +omega_pa*W3Rpar1v0 -kappa1*Spar1v0 - Spar1v0 * sd * foi_ar_1v0 - delta1_ar * Spar1v0 
    dSpau1v0  = omega_pa*Rpau1v0 + omega_pa*W1Rpau1v0 +omega_pa*W2Rpau1v0 +omega_pa*W3Rpau1v0 -kappa1*Spau1v0 - Spau1v0 * sd * foi_au_1v0 - delta1_au * Spau1v0 
    dSper1v0  = omega_pe*Rper1v0 + omega_pe*W1Rper1v0 +omega_pe*W2Rper1v0 +omega_pe*W3Rper1v0 -kappa1*Sper1v0 - Sper1v0 * sd * foi_er_1v0 - delta1_er * Sper1v0 
    dSpeu1v0  = omega_pe*Rpeu1v0 + omega_pe*W1Rpeu1v0 +omega_pe*W2Rpeu1v0 +omega_pe*W3Rpeu1v0 -kappa1*Speu1v0 - Speu1v0 * sd * foi_eu_1v0 - delta1_eu * Speu1v0 
    
    dSncr1v0  = omega_nc*Rncr1v0 + kappa1*Spcr1v0 -Sncr1v0 * sd * foi_cr_1v0 - delta1_cr * Sncr1v0   ##Note vaccination rate here doesnt differ by previous vax status
    dSncu1v0  = omega_nc*Rncu1v0 + kappa1*Spcu1v0 -Sncu1v0 * sd * foi_cu_1v0 - delta1_cu * Sncu1v0 
    dSnar1v0  = omega_na*Rnar1v0 + kappa1*Spar1v0 -Snar1v0 * sd * foi_ar_1v0 - delta1_ar * Snar1v0 
    dSnau1v0  = omega_na*Rnau1v0 + kappa1*Spau1v0 -Snau1v0 * sd * foi_au_1v0 - delta1_au * Snau1v0 
    dSner1v0  = omega_ne*Rner1v0 + kappa1*Sper1v0 -Sner1v0 * sd * foi_er_1v0 - delta1_er * Sner1v0 
    dSneu1v0  = omega_ne*Rneu1v0 + kappa1*Speu1v0 -Sneu1v0 * sd * foi_eu_1v0 - delta1_eu * Sneu1v0 
    
    #Exposed
    dEcr2v0  =  Spcr1v0 * sd * foi_cr_1v0 + Sncr1v0 * sd * foi_cr_1v0 -  sigma * Ecr2v0  - delta1_cr*Ecr2v0
    dEcu2v0  =  Spcu1v0 * sd * foi_cu_1v0 + Sncu1v0 * sd * foi_cu_1v0 -  sigma * Ecu2v0  - delta1_cu*Ecu2v0
    dEar2v0  =  Spar1v0 * sd * foi_ar_1v0 + Snar1v0 * sd * foi_ar_1v0 -  sigma * Ear2v0  - delta1_ar*Ear2v0
    dEau2v0  =  Spau1v0 * sd * foi_au_1v0 + Snau1v0 * sd * foi_au_1v0 -  sigma * Eau2v0  - delta1_au*Eau2v0
    dEer2v0  =  Sper1v0 * sd * foi_er_1v0 + Sner1v0 * sd * foi_er_1v0 -  sigma * Eer2v0  - delta1_er*Eer2v0
    dEeu2v0  =  Speu1v0 * sd * foi_eu_1v0 + Sneu1v0 * sd * foi_eu_1v0 -  sigma * Eeu2v0  - delta1_eu*Eeu2v0
    
    # Asymptomatic
    dAcr2v0  =  (1-nu_c)*sigma*Ecr2v0 - gamma_A * Acr2v0 - delta1_cr*Acr2v0
    dAcu2v0  =  (1-nu_c)*sigma*Ecu2v0 - gamma_A * Acu2v0 - delta1_cu*Acu2v0
    dAar2v0  =  (1-nu_a)*sigma*Ear2v0 - gamma_A * Aar2v0 - delta1_ar*Aar2v0
    dAau2v0  =  (1-nu_a)*sigma*Eau2v0 - gamma_A * Aau2v0 - delta1_au*Aau2v0
    dAer2v0  =  (1-nu_e)*sigma*Eer2v0 - gamma_A * Aer2v0 - delta1_er*Aer2v0
    dAeu2v0  =  (1-nu_e)*sigma*Eeu2v0 - gamma_A * Aeu2v0 - delta1_eu*Aeu2v0  
    
    #Symptomatic 
    dIcr2v0  =  nu_c*sigma*Ecr2v0 - gamma_I * Icr2v0 
    dIcu2v0  =  nu_c*sigma*Ecu2v0 - gamma_I * Icu2v0 
    dIar2v0  =  nu_a*sigma*Ear2v0 - gamma_I * Iar2v0 
    dIau2v0  =  nu_a*sigma*Eau2v0 - gamma_I * Iau2v0 
    dIer2v0  =  nu_e*sigma*Eer2v0 - gamma_I * Ier2v0
    dIeu2v0  =  nu_e*sigma*Eeu2v0 - gamma_I * Ieu2v0
    
    # Hospitalized
    dHcr2v0  =  phi_c*gamma_I*Icr2v0 - gamma_H * Hcr2v0
    dHcu2v0  =  phi_c*gamma_I*Icu2v0 - gamma_H * Hcu2v0
    dHar2v0  =  phi_a*gamma_I*Iar2v0 - gamma_H * Har2v0
    dHau2v0  =  phi_a*gamma_I*Iau2v0 - gamma_H * Hau2v0
    dHer2v0  =  phi_e*gamma_I*Ier2v0 - gamma_H * Her2v0
    dHeu2v0  =  phi_e*gamma_I*Ieu2v0 - gamma_H * Heu2v0
    
    # Recovered and seroconverted
    dRpcr2v0  =  pi*(1-phi_c)*gamma_I*Icr2v0 + pi*gamma_A*Acr2v0 + pi*(1-mu_c)*gamma_H*Hcr2v0 -kappa2*Rpcr2v0 - omega_pc*Rpcr2v0 - delta1_cr*Rpcr2v0
    dRpcu2v0  =  pi*(1-phi_c)*gamma_I*Icu2v0 + pi*gamma_A*Acu2v0 + pi*(1-mu_c)*gamma_H*Hcu2v0 -kappa2*Rpcu2v0 - omega_pc*Rpcu2v0 - delta1_cu*Rpcu2v0
    dRpar2v0  =  pi*(1-phi_a)*gamma_I*Iar2v0 + pi*gamma_A*Aar2v0 + pi*(1-mu_a)*gamma_H*Har2v0 -kappa2*Rpar2v0 - omega_pa*Rpar2v0 - delta1_ar*Rpar2v0 
    dRpau2v0  =  pi*(1-phi_a)*gamma_I*Iau2v0 + pi*gamma_A*Aau2v0 + pi*(1-mu_a)*gamma_H*Hau2v0 -kappa2*Rpau2v0 - omega_pa*Rpau2v0 - delta1_au*Rpau2v0
    dRper2v0  =  pi*(1-phi_e)*gamma_I*Ier2v0 + pi*gamma_A*Aer2v0 + pi*(1-mu_e)*gamma_H*Her2v0 -kappa2*Rper2v0 - omega_pe*Rper2v0 - delta1_er*Rper2v0
    dRpeu2v0  =  pi*(1-phi_e)*gamma_I*Ieu2v0 + pi*gamma_A*Aeu2v0 + pi*(1-mu_e)*gamma_H*Heu2v0 -kappa2*Rpeu2v0 - omega_pe*Rpeu2v0 - delta1_eu*Rpeu2v0
    
    #Recovered and not seropositive
    dRncr2v0  =  (1-pi)*(1-phi_c)*gamma_I*Icr2v0 + (1-pi)*gamma_A*Acr2v0 + (1-pi)*(1-mu_c)*gamma_H*Hcr2v0 + kappa2*Rpcr2v0 - omega_nc*Rncr2v0 - delta1_cr*Rncr2v0
    dRncu2v0  =  (1-pi)*(1-phi_c)*gamma_I*Icu2v0 + (1-pi)*gamma_A*Acu2v0 + (1-pi)*(1-mu_c)*gamma_H*Hcu2v0 + kappa2*Rpcu2v0 - omega_nc*Rncu2v0 - delta1_cu*Rncu2v0
    dRnar2v0  =  (1-pi)*(1-phi_a)*gamma_I*Iar2v0 + (1-pi)*gamma_A*Aar2v0 + (1-pi)*(1-mu_a)*gamma_H*Har2v0 + kappa2*Rpar2v0 - omega_na*Rnar2v0 - delta1_ar*Rnar2v0 
    dRnau2v0  =  (1-pi)*(1-phi_a)*gamma_I*Iau2v0 + (1-pi)*gamma_A*Aau2v0 + (1-pi)*(1-mu_a)*gamma_H*Hau2v0 + kappa2*Rpau2v0 - omega_na*Rnau2v0 - delta1_au*Rnau2v0 
    dRner2v0  =  (1-pi)*(1-phi_e)*gamma_I*Ier2v0 + (1-pi)*gamma_A*Aer2v0 + (1-pi)*(1-mu_e)*gamma_H*Her2v0 + kappa2*Rper2v0 - omega_ne*Rner2v0 - delta1_er*Rner2v0 
    dRneu2v0  =  (1-pi)*(1-phi_e)*gamma_I*Ieu2v0 + (1-pi)*gamma_A*Aeu2v0 + (1-pi)*(1-mu_e)*gamma_H*Heu2v0 + kappa2*Rpeu2v0 - omega_ne*Rneu2v0 - delta1_eu*Rneu2v0 
    
    #Deaths 
    dDcr2v0  =  mu_c*gamma_H*Hcr2v0
    dDcu2v0  =  mu_c*gamma_H*Hcu2v0
    dDar2v0  =  mu_a*gamma_H*Har2v0
    dDau2v0  =  mu_a*gamma_H*Hau2v0
    dDer2v0  =  mu_e*gamma_H*Her2v0
    dDeu2v0  =  mu_e*gamma_H*Heu2v0
    
    ################################
    ##Unvaccinated and exposed twice####
    ################################
    #Susceptible
    dSpcr2v0  = omega_pc*Rpcr2v0 -kappa2*Spcr2v0 - Spcr2v0 * sd * foi_cr_2v0 - delta1_cr * Spcr2v0 + omega2_pc*Rpcr3v0   ##Note vaccination rate here doesnt differ by previous vax status
    dSpcu2v0  = omega_pc*Rpcu2v0 -kappa2*Spcu2v0 - Spcu2v0 * sd * foi_cu_2v0 - delta1_cu * Spcu2v0 + omega2_pc*Rpcu3v0
    dSpar2v0  = omega_pa*Rpar2v0 -kappa2*Spar2v0 - Spar2v0 * sd * foi_ar_2v0 - delta1_ar * Spar2v0 + omega2_pa*Rpar3v0
    dSpau2v0  = omega_pa*Rpau2v0 -kappa2*Spau2v0 - Spau2v0 * sd * foi_au_2v0 - delta1_au * Spau2v0 + omega2_pa*Rpau3v0
    dSper2v0  = omega_pe*Rper2v0 -kappa2*Sper2v0 - Sper2v0 * sd * foi_er_2v0 - delta1_er * Sper2v0 + omega2_pe*Rper3v0
    dSpeu2v0  = omega_pe*Rpeu2v0 -kappa2*Speu2v0 - Speu2v0 * sd * foi_eu_2v0 - delta1_eu * Speu2v0 + omega2_pe*Rpeu3v0
    
    dSncr2v0  = omega_nc*Rncr2v0 + kappa2*Spcr2v0 -Sncr2v0 * sd * foi_cr_2v0 - delta1_cr * Sncr2v0 + omega2_nc*Rncr3v0
    dSncu2v0  = omega_nc*Rncu2v0 + kappa2*Spcu2v0 -Sncu2v0 * sd * foi_cu_2v0 - delta1_cu * Sncu2v0 + omega2_nc*Rncu3v0
    dSnar2v0  = omega_na*Rnar2v0 + kappa2*Spar2v0 -Snar2v0 * sd * foi_ar_2v0 - delta1_ar * Snar2v0 + omega2_na*Rnar3v0
    dSnau2v0  = omega_na*Rnau2v0 + kappa2*Spau2v0 -Snau2v0 * sd * foi_au_2v0 - delta1_au * Snau2v0 + omega2_na*Rnau3v0
    dSner2v0  = omega_ne*Rner2v0 + kappa2*Sper2v0 -Sner2v0 * sd * foi_er_2v0 - delta1_er * Sner2v0 + omega2_ne*Rner3v0
    dSneu2v0  = omega_ne*Rneu2v0 + kappa2*Speu2v0 -Sneu2v0 * sd * foi_eu_2v0 - delta1_eu * Sneu2v0 + omega2_ne*Rneu3v0
    
    #Exposed
    dEcr3v0  =  Spcr2v0 * sd * foi_cr_2v0 + Sncr2v0 * sd * foi_cr_2v0 -  sigma * Ecr3v0  - delta1_cr*Ecr3v0
    dEcu3v0  =  Spcu2v0 * sd * foi_cu_2v0 + Sncu2v0 * sd * foi_cu_2v0 -  sigma * Ecu3v0  - delta1_cu*Ecu3v0
    dEar3v0  =  Spar2v0 * sd * foi_ar_2v0 + Snar2v0 * sd * foi_ar_2v0 -  sigma * Ear3v0  - delta1_ar*Ear3v0
    dEau3v0  =  Spau2v0 * sd * foi_au_2v0 + Snau2v0 * sd * foi_au_2v0 -  sigma * Eau3v0  - delta1_au*Eau3v0
    dEer3v0  =  Sper2v0 * sd * foi_er_2v0 + Sner2v0 * sd * foi_er_2v0 -  sigma * Eer3v0  - delta1_er*Eer3v0
    dEeu3v0  =  Speu2v0 * sd * foi_eu_2v0 + Sneu2v0 * sd * foi_eu_2v0 -  sigma * Eeu3v0  - delta1_eu*Eeu3v0
    
    # Asymptomatic
    dAcr3v0  =  (1-nu_c)*sigma*Ecr3v0 - gamma_A * Acr3v0 - delta1_cr*Acr3v0
    dAcu3v0  =  (1-nu_c)*sigma*Ecu3v0 - gamma_A * Acu3v0 - delta1_cu*Acu3v0
    dAar3v0  =  (1-nu_a)*sigma*Ear3v0 - gamma_A * Aar3v0 - delta1_ar*Aar3v0
    dAau3v0  =  (1-nu_a)*sigma*Eau3v0 - gamma_A * Aau3v0 - delta1_au*Aau3v0
    dAer3v0  =  (1-nu_e)*sigma*Eer3v0 - gamma_A * Aer3v0 - delta1_er*Aer3v0
    dAeu3v0  =  (1-nu_e)*sigma*Eeu3v0 - gamma_A * Aeu3v0 - delta1_eu*Aeu3v0  
    
    #Symptomatic 
    dIcr3v0  =  nu_c*sigma*Ecr3v0 - gamma_I * Icr3v0 
    dIcu3v0  =  nu_c*sigma*Ecu3v0 - gamma_I * Icu3v0 
    dIar3v0  =  nu_a*sigma*Ear3v0 - gamma_I * Iar3v0 
    dIau3v0  =  nu_a*sigma*Eau3v0 - gamma_I * Iau3v0 
    dIer3v0  =  nu_e*sigma*Eer3v0 - gamma_I * Ier3v0
    dIeu3v0  =  nu_e*sigma*Eeu3v0 - gamma_I * Ieu3v0
    
    # Hospitalized
    dHcr3v0  =  phi_c*gamma_I*Icr3v0 - gamma_H * Hcr3v0
    dHcu3v0  =  phi_c*gamma_I*Icu3v0 - gamma_H * Hcu3v0
    dHar3v0  =  phi_a*gamma_I*Iar3v0 - gamma_H * Har3v0
    dHau3v0  =  phi_a*gamma_I*Iau3v0 - gamma_H * Hau3v0
    dHer3v0  =  phi_e*gamma_I*Ier3v0 - gamma_H * Her3v0
    dHeu3v0  =  phi_e*gamma_I*Ieu3v0 - gamma_H * Heu3v0
    
    # Recovered and seroconverted
    dRpcr3v0  =  pi*(1-phi_c)*gamma_I*Icr3v0 + pi*gamma_A*Acr3v0 + pi*(1-mu_c)*gamma_H*Hcr3v0 -kappa3*Rpcr3v0 - omega2_pc*Rpcr3v0 - delta1_cr*Rpcr3v0
    dRpcu3v0  =  pi*(1-phi_c)*gamma_I*Icu3v0 + pi*gamma_A*Acu3v0 + pi*(1-mu_c)*gamma_H*Hcu3v0 -kappa3*Rpcu3v0 - omega2_pc*Rpcu3v0 - delta1_cu*Rpcu3v0
    dRpar3v0  =  pi*(1-phi_a)*gamma_I*Iar3v0 + pi*gamma_A*Aar3v0 + pi*(1-mu_a)*gamma_H*Har3v0 -kappa3*Rpar3v0 - omega2_pa*Rpar3v0 - delta1_ar*Rpar3v0 
    dRpau3v0  =  pi*(1-phi_a)*gamma_I*Iau3v0 + pi*gamma_A*Aau3v0 + pi*(1-mu_a)*gamma_H*Hau3v0 -kappa3*Rpau3v0 - omega2_pa*Rpau3v0 - delta1_au*Rpau3v0
    dRper3v0  =  pi*(1-phi_e)*gamma_I*Ier3v0 + pi*gamma_A*Aer3v0 + pi*(1-mu_e)*gamma_H*Her3v0 -kappa3*Rper3v0 - omega2_pe*Rper3v0 - delta1_er*Rper3v0
    dRpeu3v0  =  pi*(1-phi_e)*gamma_I*Ieu3v0 + pi*gamma_A*Aeu3v0 + pi*(1-mu_e)*gamma_H*Heu3v0 -kappa3*Rpeu3v0 - omega2_pe*Rpeu3v0 - delta1_eu*Rpeu3v0
    
    #Recovered and not seropositive
    dRncr3v0  =  (1-pi)*(1-phi_c)*gamma_I*Icr3v0 + (1-pi)*gamma_A*Acr3v0 + (1-pi)*(1-mu_c)*gamma_H*Hcr3v0 + kappa3*Rpcr3v0 - omega2_nc*Rncr3v0 - delta1_cr*Rncr3v0
    dRncu3v0  =  (1-pi)*(1-phi_c)*gamma_I*Icu3v0 + (1-pi)*gamma_A*Acu3v0 + (1-pi)*(1-mu_c)*gamma_H*Hcu3v0 + kappa3*Rpcu3v0 - omega2_nc*Rncu3v0 - delta1_cu*Rncu3v0
    dRnar3v0  =  (1-pi)*(1-phi_a)*gamma_I*Iar3v0 + (1-pi)*gamma_A*Aar3v0 + (1-pi)*(1-mu_a)*gamma_H*Har3v0 + kappa3*Rpar3v0 - omega2_na*Rnar3v0 - delta1_ar*Rnar3v0 
    dRnau3v0  =  (1-pi)*(1-phi_a)*gamma_I*Iau3v0 + (1-pi)*gamma_A*Aau3v0 + (1-pi)*(1-mu_a)*gamma_H*Hau3v0 + kappa3*Rpau3v0 - omega2_na*Rnau3v0 - delta1_au*Rnau3v0 
    dRner3v0  =  (1-pi)*(1-phi_e)*gamma_I*Ier3v0 + (1-pi)*gamma_A*Aer3v0 + (1-pi)*(1-mu_e)*gamma_H*Her3v0 + kappa3*Rper3v0 - omega2_ne*Rner3v0 - delta1_er*Rner3v0 
    dRneu3v0  =  (1-pi)*(1-phi_e)*gamma_I*Ieu3v0 + (1-pi)*gamma_A*Aeu3v0 + (1-pi)*(1-mu_e)*gamma_H*Heu3v0 + kappa3*Rpeu3v0 - omega2_ne*Rneu3v0 - delta1_eu*Rneu3v0 
    
    #Deaths 
    dDcr3v0  =  mu_c*gamma_H*Hcr3v0
    dDcu3v0  =  mu_c*gamma_H*Hcu3v0
    dDar3v0  =  mu_a*gamma_H*Har3v0
    dDau3v0  =  mu_a*gamma_H*Hau3v0
    dDer3v0  =  mu_e*gamma_H*Her3v0
    dDeu3v0  =  mu_e*gamma_H*Heu3v0
    
    ################################
    ##Vaccinated one dose and unexposed####
    ################################
    #Susceptible and seropositive with S-spike and total IGg
    dSpcr0v1  =  omegav_pc *Vpcr0v1+ omegav_pc*W1Vpcr0v1+ omegav_pc*W2Vpcr0v1+ omegav_pc*W3Vpcr0v1 -Spcr0v1 * sd * foi_cr_0v1 - delta2_cr * Spcr0v1  -kappa1 * Spcr0v1   
    dSpcu0v1  =  omegav_pc *Vpcu0v1+ omegav_pc*W1Vpcu0v1+ omegav_pc*W2Vpcu0v1+ omegav_pc*W3Vpcu0v1 -Spcu0v1 * sd * foi_cu_0v1 - delta2_cu * Spcu0v1  -kappa1 * Spcu0v1  
    dSpar0v1  =  omegav_pa *Vpar0v1+ omegav_pc*W1Vpar0v1+ omegav_pc*W2Vpar0v1+ omegav_pc*W3Vpar0v1 -Spar0v1 * sd * foi_ar_0v1 - delta2_ar * Spar0v1  -kappa1 * Spar0v1  
    dSpau0v1  =  omegav_pa *Vpau0v1+ omegav_pc*W1Vpau0v1+ omegav_pc*W2Vpau0v1+ omegav_pc*W3Vpau0v1 -Spau0v1 * sd * foi_au_0v1 - delta2_au * Spau0v1  -kappa1 * Spau0v1  
    dSper0v1  =  omegav_pe *Vper0v1+ omegav_pc*W1Vper0v1+ omegav_pc*W2Vper0v1+ omegav_pc*W3Vper0v1 -Sper0v1 * sd * foi_er_0v1 - delta2_er * Sper0v1  -kappa1 * Sper0v1  
    dSpeu0v1  =  omegav_pe *Vpeu0v1+ omegav_pc*W1Vpeu0v1+ omegav_pc*W2Vpeu0v1+ omegav_pc*W3Vpeu0v1 -Speu0v1 * sd * foi_eu_0v1 - delta2_eu * Speu0v1  -kappa1 * Speu0v1  
    
    # Susceptible and seronegative for S-spike and total IGg
    dSncr0v1  = omegav_nc *Vncr0v1 - Sncr0v1 * sd * foi_cr_0v1 - delta2_cr * Sncr0v1 +kappa1 * Spcr0v1 
    dSncu0v1  = omegav_nc *Vncu0v1 - Sncu0v1 * sd * foi_cu_0v1 - delta2_cu * Sncu0v1 +kappa1 * Spcu0v1 
    dSnar0v1  = omegav_na *Vnar0v1 - Snar0v1 * sd * foi_ar_0v1 - delta2_ar * Snar0v1 +kappa1 * Spar0v1 
    dSnau0v1  = omegav_na *Vnau0v1 - Snau0v1 * sd * foi_au_0v1 - delta2_au * Snau0v1 +kappa1 * Spau0v1 
    dSner0v1  = omegav_ne *Vner0v1 - Sner0v1 * sd * foi_er_0v1 - delta2_er * Sner0v1 +kappa1 * Sper0v1 
    dSneu0v1  = omegav_ne *Vneu0v1 - Sneu0v1 * sd * foi_eu_0v1 - delta2_eu * Sneu0v1 +kappa1 * Speu0v1 
    
    #Exposed
    dEcr1v1  =  Spcr0v1*sd*foi_cr_0v1+Sncr0v1 * sd * foi_cr_0v1 -  sigma * Ecr1v1  - delta2_cr*Ecr1v1 +delta1_cr*Ecr1v0
    dEcu1v1  =  Spcu0v1*sd*foi_cu_0v1+Sncu0v1 * sd * foi_cu_0v1- sigma * Ecu1v1  - delta2_cu*Ecu1v1 +delta1_cu*Ecu1v0
    dEar1v1  =  Spar0v1*sd*foi_ar_0v1+Snar0v1 * sd * foi_ar_0v1-  sigma * Ear1v1  - delta2_ar*Ear1v1 +delta1_ar*Ear1v0
    dEau1v1  =  Spau0v1*sd*foi_au_0v1+Snau0v1 * sd * foi_au_0v1-  sigma * Eau1v1  - delta2_au*Eau1v1 +delta1_au*Eau1v0
    dEer1v1  =  Sper0v1*sd*foi_er_0v1+Sner0v1 * sd * foi_er_0v1-  sigma * Eer1v1  - delta2_er*Eer1v1 +delta1_er*Eer1v0
    dEeu1v1  =  Speu0v1*sd*foi_eu_0v1+Sneu0v1 * sd * foi_eu_0v1-  sigma * Eeu1v1  - delta2_eu*Eeu1v1 +delta1_eu*Eeu1v0
    
    # Asymptomatic
    dAcr1v1  =  (1-nu_c)*sigma*Ecr1v1 - gamma_A * Acr1v1 - delta2_cr*Acr1v1 +delta1_cr*Acr1v0
    dAcu1v1  =  (1-nu_c)*sigma*Ecu1v1 - gamma_A * Acu1v1 - delta2_cu*Acu1v1 +delta1_cu*Acu1v0
    dAar1v1  =  (1-nu_a)*sigma*Ear1v1 - gamma_A * Aar1v1 - delta2_ar*Aar1v1 +delta1_ar*Aar1v0
    dAau1v1  =  (1-nu_a)*sigma*Eau1v1 - gamma_A * Aau1v1 - delta2_au*Aau1v1 +delta1_au*Aau1v0
    dAer1v1  =  (1-nu_e)*sigma*Eer1v1 - gamma_A * Aer1v1 - delta2_er*Aer1v1 +delta1_er*Aer1v0
    dAeu1v1  =  (1-nu_e)*sigma*Eeu1v1 - gamma_A * Aeu1v1 - delta2_eu*Aeu1v1 +delta1_eu*Aeu1v0  
    
    #Symptomatic 
    dIcr1v1  =  nu_c*sigma*Ecr1v1 - gamma_I * Icr1v1 
    dIcu1v1  =  nu_c*sigma*Ecu1v1 - gamma_I * Icu1v1 
    dIar1v1  =  nu_a*sigma*Ear1v1 - gamma_I * Iar1v1 
    dIau1v1  =  nu_a*sigma*Eau1v1 - gamma_I * Iau1v1 
    dIer1v1  =  nu_e*sigma*Eer1v1 - gamma_I * Ier1v1
    dIeu1v1  =  nu_e*sigma*Eeu1v1 - gamma_I * Ieu1v1
    
    # Hospitalized
    dHcr1v1  =  phi_cv1*gamma_I*Icr1v1 - gamma_H * Hcr1v1
    dHcu1v1  =  phi_cv1*gamma_I*Icu1v1 - gamma_H * Hcu1v1
    dHar1v1  =  phi_av1*gamma_I*Iar1v1 - gamma_H * Har1v1
    dHau1v1  =  phi_av1*gamma_I*Iau1v1 - gamma_H * Hau1v1
    dHer1v1  =  phi_ev1*gamma_I*Ier1v1 - gamma_H * Her1v1
    dHeu1v1  =  phi_ev1*gamma_I*Ieu1v1 - gamma_H * Heu1v1
    
    # Recovered and seroconverted
    dRpcr1v1  =  pi*(1-phi_cv1)*gamma_I*Icr1v1 + pi*gamma_A*Acr1v1 + pi*(1-mu_c)*gamma_H*Hcr1v1 -kappa2*Rpcr1v1 - omega_pc*Rpcr1v1 - delta2_cr*Rpcr1v1 +delta1_cr*Rpcr1v0+delta1_cr*W1Rpcr1v0+delta1_cr*W2Rpcr1v0+delta1_cr*W3Rpcr1v0+delta1_cr*Rncr1v0*rho_v1 +delta1_cr*Spcr1v0+delta1_cr*Sncr1v0*rho_v1 
    dRpcu1v1  =  pi*(1-phi_cv1)*gamma_I*Icu1v1 + pi*gamma_A*Acu1v1 + pi*(1-mu_c)*gamma_H*Hcu1v1 -kappa2*Rpcu1v1 - omega_pc*Rpcu1v1 - delta2_cu*Rpcu1v1 +delta1_cu*Rpcu1v0+delta1_cu*W1Rpcu1v0+delta1_cu*W2Rpcu1v0+delta1_cu*W3Rpcu1v0+delta1_cu*Rncu1v0*rho_v1 +delta1_cu*Spcu1v0+delta1_cu*Sncu1v0*rho_v1 
    dRpar1v1  =  pi*(1-phi_av1)*gamma_I*Iar1v1 + pi*gamma_A*Aar1v1 + pi*(1-mu_a)*gamma_H*Har1v1 -kappa2*Rpar1v1 - omega_pa*Rpar1v1 - delta2_ar*Rpar1v1 +delta1_ar*Rpar1v0+delta1_ar*W1Rpar1v0+delta1_ar*W2Rpar1v0+delta1_ar*W3Rpar1v0+delta1_ar*Rnar1v0*rho_v1 +delta1_ar*Spar1v0+delta1_ar*Snar1v0*rho_v1 
    dRpau1v1  =  pi*(1-phi_av1)*gamma_I*Iau1v1 + pi*gamma_A*Aau1v1 + pi*(1-mu_a)*gamma_H*Hau1v1 -kappa2*Rpau1v1 - omega_pa*Rpau1v1 - delta2_au*Rpau1v1 +delta1_au*Rpau1v0+delta1_au*W1Rpau1v0+delta1_au*W2Rpau1v0+delta1_au*W3Rpau1v0+delta1_au*Rnau1v0*rho_v1 +delta1_au*Spau1v0+delta1_au*Snau1v0*rho_v1 
    dRper1v1  =  pi*(1-phi_ev1)*gamma_I*Ier1v1 + pi*gamma_A*Aer1v1 + pi*(1-mu_e)*gamma_H*Her1v1 -kappa2*Rper1v1 - omega_pe*Rper1v1 - delta2_er*Rper1v1 +delta1_er*Rper1v0+delta1_er*W1Rper1v0+delta1_er*W2Rper1v0+delta1_er*W3Rper1v0+delta1_er*Rner1v0*rho_v1 +delta1_er*Sper1v0+delta1_er*Sner1v0*rho_v1
    dRpeu1v1  =  pi*(1-phi_ev1)*gamma_I*Ieu1v1 + pi*gamma_A*Aeu1v1 + pi*(1-mu_e)*gamma_H*Heu1v1 -kappa2*Rpeu1v1 - omega_pe*Rpeu1v1 - delta2_eu*Rpeu1v1 +delta1_eu*Rpeu1v0+delta1_eu*W1Rpeu1v0+delta1_eu*W2Rpeu1v0+delta1_eu*W3Rpeu1v0+delta1_eu*Rneu1v0*rho_v1 +delta1_eu*Speu1v0+delta1_eu*Sneu1v0*rho_v1 
    
    #Recovered and not seropositive
    dRncr1v1  =  (1-pi)*(1-phi_cv1)*gamma_I*Icr1v1 + (1-pi)*gamma_A*Acr1v1 + (1-pi)*(1-mu_c)*gamma_H*Hcr1v1 + kappa2*Rpcr1v1 - omega_nc*Rncr1v1 - delta2_cr*Rncr1v1 +delta1_cr*Rncr1v0*(1-rho_v1)+delta1_cr*Sncr1v0*(1-rho_v1)
    dRncu1v1  =  (1-pi)*(1-phi_cv1)*gamma_I*Icu1v1 + (1-pi)*gamma_A*Acu1v1 + (1-pi)*(1-mu_c)*gamma_H*Hcu1v1 + kappa2*Rpcu1v1 - omega_nc*Rncu1v1 - delta2_cu*Rncu1v1 +delta1_cu*Rncu1v0*(1-rho_v1)+delta1_cu*Sncu1v0*(1-rho_v1)
    dRnar1v1  =  (1-pi)*(1-phi_av1)*gamma_I*Iar1v1 + (1-pi)*gamma_A*Aar1v1 + (1-pi)*(1-mu_a)*gamma_H*Har1v1 + kappa2*Rpar1v1 - omega_na*Rnar1v1 - delta2_ar*Rnar1v1 +delta1_ar*Rnar1v0*(1-rho_v1)+delta1_ar*Snar1v0*(1-rho_v1)
    dRnau1v1  =  (1-pi)*(1-phi_av1)*gamma_I*Iau1v1 + (1-pi)*gamma_A*Aau1v1 + (1-pi)*(1-mu_a)*gamma_H*Hau1v1 + kappa2*Rpau1v1 - omega_na*Rnau1v1 - delta2_au*Rnau1v1 +delta1_au*Rnau1v0*(1-rho_v1)+delta1_au*Snau1v0*(1-rho_v1)
    dRner1v1  =  (1-pi)*(1-phi_ev1)*gamma_I*Ier1v1 + (1-pi)*gamma_A*Aer1v1 + (1-pi)*(1-mu_e)*gamma_H*Her1v1 + kappa2*Rper1v1 - omega_ne*Rner1v1 - delta2_er*Rner1v1 +delta1_er*Rner1v0*(1-rho_v1)+delta1_er*Sner1v0*(1-rho_v1)
    dRneu1v1  =  (1-pi)*(1-phi_ev1)*gamma_I*Ieu1v1 + (1-pi)*gamma_A*Aeu1v1 + (1-pi)*(1-mu_e)*gamma_H*Heu1v1 + kappa2*Rpeu1v1 - omega_ne*Rneu1v1 - delta2_eu*Rneu1v1 +delta1_eu*Rneu1v0*(1-rho_v1)+delta1_eu*Sneu1v0*(1-rho_v1)
    
    #Deaths 
    dDcr1v1  =  mu_c*gamma_H*Hcr1v1
    dDcu1v1  =  mu_c*gamma_H*Hcu1v1
    dDar1v1  =  mu_a*gamma_H*Har1v1
    dDau1v1  =  mu_a*gamma_H*Hau1v1
    dDer1v1  =  mu_e*gamma_H*Her1v1
    dDeu1v1  =  mu_e*gamma_H*Heu1v1
    
    ################################
    ##Vaccinated one dose and exposed once####
    ################################
    #Susceptible
    dSpcr1v1  = omega_pc*Rpcr1v1 -kappa2*Spcr1v1 - Spcr1v1 * sd * foi_cr_1v1 - delta2_cr * Spcr1v1 
    dSpcu1v1  = omega_pc*Rpcu1v1 -kappa2*Spcu1v1 - Spcu1v1 * sd * foi_cu_1v1 - delta2_cu * Spcu1v1 
    dSpar1v1  = omega_pa*Rpar1v1 -kappa2*Spar1v1 - Spar1v1 * sd * foi_ar_1v1 - delta2_ar * Spar1v1 
    dSpau1v1  = omega_pa*Rpau1v1 -kappa2*Spau1v1 - Spau1v1 * sd * foi_au_1v1 - delta2_au * Spau1v1 
    dSper1v1  = omega_pe*Rper1v1 -kappa2*Sper1v1 - Sper1v1 * sd * foi_er_1v1 - delta2_er * Sper1v1  
    dSpeu1v1  = omega_pe*Rpeu1v1 -kappa2*Speu1v1 - Speu1v1 * sd * foi_eu_1v1 - delta2_eu * Speu1v1 
    
    dSncr1v1  = omega_nc*Rncr1v1 + kappa2*Spcr1v1 -Sncr1v1 * sd * foi_cr_1v1 - delta2_cr * Sncr1v1  
    dSncu1v1  = omega_nc*Rncu1v1 + kappa2*Spcu1v1 -Sncu1v1 * sd * foi_cu_1v1 - delta2_cu * Sncu1v1 
    dSnar1v1  = omega_na*Rnar1v1 + kappa2*Spar1v1 -Snar1v1 * sd * foi_ar_1v1 - delta2_ar * Snar1v1 
    dSnau1v1  = omega_na*Rnau1v1 + kappa2*Spau1v1 -Snau1v1 * sd * foi_au_1v1 - delta2_au * Snau1v1 
    dSner1v1  = omega_ne*Rner1v1 + kappa2*Sper1v1 -Sner1v1 * sd * foi_er_1v1 - delta2_er * Sner1v1 
    dSneu1v1  = omega_ne*Rneu1v1 + kappa2*Speu1v1 -Sneu1v1 * sd * foi_eu_1v1 - delta2_eu * Sneu1v1 
    
    #Exposed
    dEcr2v1  =  Spcr1v1 * sd * foi_cr_1v1 + Sncr1v1 * sd * foi_cr_1v1 -  sigma * Ecr2v1  - delta2_cr*Ecr2v1 +delta1_cr*Ecr2v0
    dEcu2v1  =  Spcu1v1 * sd * foi_cu_1v1 + Sncu1v1 * sd * foi_cu_1v1 -  sigma * Ecu2v1  - delta2_cu*Ecu2v1 +delta1_cu*Ecu2v0
    dEar2v1  =  Spar1v1 * sd * foi_ar_1v1 + Snar1v1 * sd * foi_ar_1v1 -  sigma * Ear2v1  - delta2_ar*Ear2v1 +delta1_ar*Ear2v0
    dEau2v1  =  Spau1v1 * sd * foi_au_1v1 + Snau1v1 * sd * foi_au_1v1 -  sigma * Eau2v1  - delta2_au*Eau2v1 +delta1_au*Eau2v0
    dEer2v1  =  Sper1v1 * sd * foi_er_1v1 + Sner1v1 * sd * foi_er_1v1 -  sigma * Eer2v1  - delta2_er*Eer2v1 +delta1_er*Eer2v0
    dEeu2v1  =  Speu1v1 * sd * foi_eu_1v1 + Sneu1v1 * sd * foi_eu_1v1 -  sigma * Eeu2v1  - delta2_eu*Eeu2v1 +delta1_eu*Eeu2v0
    
    # Asymptomatic
    dAcr2v1  =  (1-nu_c)*sigma*Ecr2v1 - gamma_A * Acr2v1 - delta2_cr*Acr2v1 + delta1_cr*Acr2v0
    dAcu2v1  =  (1-nu_c)*sigma*Ecu2v1 - gamma_A * Acu2v1 - delta2_cu*Acu2v1 + delta1_cu*Acu2v0
    dAar2v1  =  (1-nu_a)*sigma*Ear2v1 - gamma_A * Aar2v1 - delta2_ar*Aar2v1 + delta1_ar*Aar2v0
    dAau2v1  =  (1-nu_a)*sigma*Eau2v1 - gamma_A * Aau2v1 - delta2_au*Aau2v1 + delta1_au*Aau2v0
    dAer2v1  =  (1-nu_e)*sigma*Eer2v1 - gamma_A * Aer2v1 - delta2_er*Aer2v1 + delta1_er*Aer2v0
    dAeu2v1  =  (1-nu_e)*sigma*Eeu2v1 - gamma_A * Aeu2v1 - delta2_eu*Aeu2v1 + delta1_eu*Aeu2v0
    
    #Symptomatic 
    dIcr2v1  =  nu_c*sigma*Ecr2v1 - gamma_I * Icr2v1 
    dIcu2v1  =  nu_c*sigma*Ecu2v1 - gamma_I * Icu2v1 
    dIar2v1  =  nu_a*sigma*Ear2v1 - gamma_I * Iar2v1 
    dIau2v1  =  nu_a*sigma*Eau2v1 - gamma_I * Iau2v1 
    dIer2v1  =  nu_e*sigma*Eer2v1 - gamma_I * Ier2v1
    dIeu2v1  =  nu_e*sigma*Eeu2v1 - gamma_I * Ieu2v1
    
    # Hospitalized
    dHcr2v1  =  phi_cv1*gamma_I*Icr2v1 - gamma_H * Hcr2v1
    dHcu2v1  =  phi_cv1*gamma_I*Icu2v1 - gamma_H * Hcu2v1
    dHar2v1  =  phi_av1*gamma_I*Iar2v1 - gamma_H * Har2v1
    dHau2v1  =  phi_av1*gamma_I*Iau2v1 - gamma_H * Hau2v1
    dHer2v1  =  phi_ev1*gamma_I*Ier2v1 - gamma_H * Her2v1
    dHeu2v1  =  phi_ev1*gamma_I*Ieu2v1 - gamma_H * Heu2v1
    
    # Recovered and seroconverted
    dRpcr2v1  =  pi*(1-phi_cv1)*gamma_I*Icr2v1 + pi*gamma_A*Acr2v1 + pi*(1-mu_c)*gamma_H*Hcr2v1 -kappa3*Rpcr2v1 - omega_pc*Rpcr2v1 - delta2_cr*Rpcr2v1 +delta1_cr*Rpcr2v0 + delta1_cr*Rncr2v0*(rho_v2)+ delta1_cr*Spcr2v0+ delta1_cr*Sncr2v0*(rho_v2)
    dRpcu2v1  =  pi*(1-phi_cv1)*gamma_I*Icu2v1 + pi*gamma_A*Acu2v1 + pi*(1-mu_c)*gamma_H*Hcu2v1 -kappa3*Rpcu2v1 - omega_pc*Rpcu2v1 - delta2_cu*Rpcu2v1 +delta1_cu*Rpcu2v0+ delta1_cu*Rncu2v0*(rho_v2)+ delta1_cu*Spcu2v0+ delta1_cu*Sncu2v0*(rho_v2)
    dRpar2v1  =  pi*(1-phi_av1)*gamma_I*Iar2v1 + pi*gamma_A*Aar2v1 + pi*(1-mu_a)*gamma_H*Har2v1 -kappa3*Rpar2v1 - omega_pa*Rpar2v1 - delta2_ar*Rpar2v1 +delta1_ar*Rpar2v0+ delta1_ar*Rnar2v0*(rho_v2)+ delta1_ar*Spar2v0+ delta1_ar*Snar2v0*(rho_v2)
    dRpau2v1  =  pi*(1-phi_av1)*gamma_I*Iau2v1 + pi*gamma_A*Aau2v1 + pi*(1-mu_a)*gamma_H*Hau2v1 -kappa3*Rpau2v1 - omega_pa*Rpau2v1 - delta2_au*Rpau2v1 +delta1_au*Rpau2v0+ delta1_au*Rnau2v0*(rho_v2)+ delta1_au*Spau2v0+ delta1_au*Snau2v0*(rho_v2)
    dRper2v1  =  pi*(1-phi_ev1)*gamma_I*Ier2v1 + pi*gamma_A*Aer2v1 + pi*(1-mu_e)*gamma_H*Her2v1 -kappa3*Rper2v1 - omega_pe*Rper2v1 - delta2_er*Rper2v1 +delta1_er*Rper2v0+ delta1_er*Rner2v0*(rho_v2)+ delta1_er*Sper2v0+ delta1_er*Sner2v0*(rho_v2)
    dRpeu2v1  =  pi*(1-phi_ev1)*gamma_I*Ieu2v1 + pi*gamma_A*Aeu2v1 + pi*(1-mu_e)*gamma_H*Heu2v1 -kappa3*Rpeu2v1 - omega_pe*Rpeu2v1 - delta2_eu*Rpeu2v1 +delta1_eu*Rpeu2v0+ delta1_eu*Rneu2v0*(rho_v2)+ delta1_eu*Speu2v0+ delta1_eu*Sneu2v0*(rho_v2)
    
    #Recovered and not seropositive
    dRncr2v1  =  (1-pi)*(1-phi_cv1)*gamma_I*Icr2v1 + (1-pi)*gamma_A*Acr2v1 + (1-pi)*(1-mu_c)*gamma_H*Hcr2v1 + kappa3*Rpcr2v1 - omega_nc*Rncr2v1 - delta2_cr*Rncr2v1 + delta1_cr*Rncr2v0*(1-rho_v2)+ delta1_cr*Sncr2v0*(1-rho_v2)
    dRncu2v1  =  (1-pi)*(1-phi_cv1)*gamma_I*Icu2v1 + (1-pi)*gamma_A*Acu2v1 + (1-pi)*(1-mu_c)*gamma_H*Hcu2v1 + kappa3*Rpcu2v1 - omega_nc*Rncu2v1 - delta2_cu*Rncu2v1 + delta1_cu*Rncu2v0*(1-rho_v2)+ delta1_cu*Sncu2v0*(1-rho_v2)
    dRnar2v1  =  (1-pi)*(1-phi_av1)*gamma_I*Iar2v1 + (1-pi)*gamma_A*Aar2v1 + (1-pi)*(1-mu_a)*gamma_H*Har2v1 + kappa3*Rpar2v1 - omega_na*Rnar2v1 - delta2_ar*Rnar2v1 + delta1_ar*Rnar2v0*(1-rho_v2)+ delta1_ar*Snar2v0*(1-rho_v2)
    dRnau2v1  =  (1-pi)*(1-phi_av1)*gamma_I*Iau2v1 + (1-pi)*gamma_A*Aau2v1 + (1-pi)*(1-mu_a)*gamma_H*Hau2v1 + kappa3*Rpau2v1 - omega_na*Rnau2v1 - delta2_au*Rnau2v1 + delta1_au*Rnau2v0*(1-rho_v2)+ delta1_au*Snau2v0*(1-rho_v2)
    dRner2v1  =  (1-pi)*(1-phi_ev1)*gamma_I*Ier2v1 + (1-pi)*gamma_A*Aer2v1 + (1-pi)*(1-mu_e)*gamma_H*Her2v1 + kappa3*Rper2v1 - omega_ne*Rner2v1 - delta2_er*Rner2v1 + delta1_er*Rner2v0*(1-rho_v2)+ delta1_er*Sner2v0*(1-rho_v2)
    dRneu2v1  =  (1-pi)*(1-phi_ev1)*gamma_I*Ieu2v1 + (1-pi)*gamma_A*Aeu2v1 + (1-pi)*(1-mu_e)*gamma_H*Heu2v1 + kappa3*Rpeu2v1 - omega_ne*Rneu2v1 - delta2_eu*Rneu2v1 + delta1_eu*Rneu2v0*(1-rho_v2)+ delta1_eu*Sneu2v0*(1-rho_v2)
    
    #Deaths 
    dDcr2v1  =  mu_c*gamma_H*Hcr2v1
    dDcu2v1  =  mu_c*gamma_H*Hcu2v1
    dDar2v1  =  mu_a*gamma_H*Har2v1
    dDau2v1  =  mu_a*gamma_H*Hau2v1
    dDer2v1  =  mu_e*gamma_H*Her2v1
    dDeu2v1  =  mu_e*gamma_H*Heu2v1
    
    ################################
    ##Vaccinated one dose and exposed twice####
    ################################
    #Susceptible
    dSpcr2v1  = omega_pc*Rpcr2v1 -kappa3*Spcr2v1 - Spcr2v1 * sd * foi_cr_2v1 - delta2_cr * Spcr2v1  + omega2_pc*Rpcr3v1
    dSpcu2v1  = omega_pc*Rpcu2v1 -kappa3*Spcu2v1 - Spcu2v1 * sd * foi_cu_2v1 - delta2_cu * Spcu2v1  + omega2_pc*Rpcu3v1
    dSpar2v1  = omega_pa*Rpar2v1 -kappa3*Spar2v1 - Spar2v1 * sd * foi_ar_2v1 - delta2_ar * Spar2v1  + omega2_pa*Rpar3v1
    dSpau2v1  = omega_pa*Rpau2v1 -kappa3*Spau2v1 - Spau2v1 * sd * foi_au_2v1 - delta2_au * Spau2v1  + omega2_pa*Rpau3v1
    dSper2v1  = omega_pe*Rper2v1 -kappa3*Sper2v1 - Sper2v1 * sd * foi_er_2v1 - delta2_er * Sper2v1  + omega2_pe*Rper3v1
    dSpeu2v1  = omega_pe*Rpeu2v1 -kappa3*Speu2v1 - Speu2v1 * sd * foi_eu_2v1 - delta2_eu * Speu2v1  + omega2_pe*Rpeu3v1
    
    dSncr2v1  = omega_pc*Rncr2v1 + kappa3*Spcr2v1 -Sncr2v1 * sd * foi_cr_2v1 - delta2_cr * Sncr2v1 + omega2_nc*Rncr3v1
    dSncu2v1  = omega_pc*Rncu2v1 + kappa3*Spcu2v1 -Sncu2v1 * sd * foi_cu_2v1 - delta2_cu * Sncu2v1 + omega2_nc*Rncu3v1
    dSnar2v1  = omega_pa*Rnar2v1 + kappa3*Spar2v1 -Snar2v1 * sd * foi_ar_2v1 - delta2_ar * Snar2v1 + omega2_na*Rnar3v1
    dSnau2v1  = omega_pa*Rnau2v1 + kappa3*Spau2v1 -Snau2v1 * sd * foi_au_2v1 - delta2_au * Snau2v1 + omega2_na*Rnau3v1
    dSner2v1  = omega_pe*Rner2v1 + kappa3*Sper2v1 -Sner2v1 * sd * foi_er_2v1 - delta2_er * Sner2v1 + omega2_ne*Rner3v1
    dSneu2v1  = omega_pe*Rneu2v1 + kappa3*Speu2v1 -Sneu2v1 * sd * foi_eu_2v1 - delta2_eu * Sneu2v1 + omega2_ne*Rneu3v1
    
    #Exposed
    dEcr3v1  =  Spcr2v1 * sd * foi_cr_2v1 + Sncr2v1 * sd * foi_cr_2v1 -  sigma * Ecr3v1  - delta2_cr*Ecr3v1 + delta1_cr*Ecr3v0
    dEcu3v1  =  Spcu2v1 * sd * foi_cu_2v1 + Sncu2v1 * sd * foi_cu_2v1 -  sigma * Ecu3v1  - delta2_cu*Ecu3v1 + delta1_cu*Ecu3v0
    dEar3v1  =  Spar2v1 * sd * foi_ar_2v1 + Snar2v1 * sd * foi_ar_2v1 -  sigma * Ear3v1  - delta2_ar*Ear3v1 + delta1_ar*Ear3v0
    dEau3v1  =  Spau2v1 * sd * foi_au_2v1 + Snau2v1 * sd * foi_au_2v1 -  sigma * Eau3v1  - delta2_au*Eau3v1 + delta1_au*Eau3v0
    dEer3v1  =  Sper2v1 * sd * foi_er_2v1 + Sner2v1 * sd * foi_er_2v1 -  sigma * Eer3v1  - delta2_er*Eer3v1 + delta1_er*Eer3v0
    dEeu3v1  =  Speu2v1 * sd * foi_eu_2v1 + Sneu2v1 * sd * foi_eu_2v1 -  sigma * Eeu3v1  - delta2_eu*Eeu3v1 + delta1_eu*Eeu3v0
    
    # Asymptomatic
    dAcr3v1  =  (1-nu_c)*sigma*Ecr3v1 - gamma_A * Acr3v1 - delta2_cr*Acr3v1+ delta1_cr*Acr3v0
    dAcu3v1  =  (1-nu_c)*sigma*Ecu3v1 - gamma_A * Acu3v1 - delta2_cu*Acu3v1+ delta1_cu*Acu3v0
    dAar3v1  =  (1-nu_a)*sigma*Ear3v1 - gamma_A * Aar3v1 - delta2_ar*Aar3v1+ delta1_ar*Aar3v0
    dAau3v1  =  (1-nu_a)*sigma*Eau3v1 - gamma_A * Aau3v1 - delta2_au*Aau3v1+ delta1_au*Aau3v0
    dAer3v1  =  (1-nu_e)*sigma*Eer3v1 - gamma_A * Aer3v1 - delta2_er*Aer3v1+ delta1_er*Aer3v0
    dAeu3v1  =  (1-nu_e)*sigma*Eeu3v1 - gamma_A * Aeu3v1 - delta2_eu*Aeu3v1+ delta1_eu*Aeu3v0
    
    #Symptomatic 
    dIcr3v1  =  nu_c*sigma*Ecr3v1 - gamma_I * Icr3v1 
    dIcu3v1  =  nu_c*sigma*Ecu3v1 - gamma_I * Icu3v1 
    dIar3v1  =  nu_a*sigma*Ear3v1 - gamma_I * Iar3v1 
    dIau3v1  =  nu_a*sigma*Eau3v1 - gamma_I * Iau3v1 
    dIer3v1  =  nu_e*sigma*Eer3v1 - gamma_I * Ier3v1
    dIeu3v1  =  nu_e*sigma*Eeu3v1 - gamma_I * Ieu3v1
    
    # Hospitalized
    dHcr3v1  =  phi_cv1*gamma_I*Icr3v1 - gamma_H * Hcr3v1
    dHcu3v1  =  phi_cv1*gamma_I*Icu3v1 - gamma_H * Hcu3v1
    dHar3v1  =  phi_av1*gamma_I*Iar3v1 - gamma_H * Har3v1
    dHau3v1  =  phi_av1*gamma_I*Iau3v1 - gamma_H * Hau3v1
    dHer3v1  =  phi_ev1*gamma_I*Ier3v1 - gamma_H * Her3v1
    dHeu3v1  =  phi_ev1*gamma_I*Ieu3v1 - gamma_H * Heu3v1
    
    # Recovered and seroconverted
    dRpcr3v1  =  pi*(1-phi_cv1)*gamma_I*Icr3v1 + pi*gamma_A*Acr3v1 + pi*(1-mu_c)*gamma_H*Hcr3v1 -kappa3*Rpcr3v1 - omega2_pc*Rpcr3v1 - delta2_cr*Rpcr3v1 + delta1_cr*Rpcr3v0+ delta1_cr*Rncr3v0*(rho_v2)
    dRpcu3v1  =  pi*(1-phi_cv1)*gamma_I*Icu3v1 + pi*gamma_A*Acu3v1 + pi*(1-mu_c)*gamma_H*Hcu3v1 -kappa3*Rpcu3v1 - omega2_pc*Rpcu3v1 - delta2_cu*Rpcu3v1 + delta1_cu*Rpcu3v0+ delta1_cu*Rncu3v0*(rho_v2)
    dRpar3v1  =  pi*(1-phi_av1)*gamma_I*Iar3v1 + pi*gamma_A*Aar3v1 + pi*(1-mu_a)*gamma_H*Har3v1 -kappa3*Rpar3v1 - omega2_pa*Rpar3v1 - delta2_ar*Rpar3v1 + delta1_ar*Rpar3v0 + delta1_ar*Rnar3v0*(rho_v2)
    dRpau3v1  =  pi*(1-phi_av1)*gamma_I*Iau3v1 + pi*gamma_A*Aau3v1 + pi*(1-mu_a)*gamma_H*Hau3v1 -kappa3*Rpau3v1 - omega2_pa*Rpau3v1 - delta2_au*Rpau3v1 + delta1_au*Rpau3v0+ delta1_au*Rnau3v0*(rho_v2)
    dRper3v1  =  pi*(1-phi_ev1)*gamma_I*Ier3v1 + pi*gamma_A*Aer3v1 + pi*(1-mu_e)*gamma_H*Her3v1 -kappa3*Rper3v1 - omega2_pe*Rper3v1 - delta2_er*Rper3v1 + delta1_er*Rper3v0+ delta1_er*Rner3v0*(rho_v2)
    dRpeu3v1  =  pi*(1-phi_ev1)*gamma_I*Ieu3v1 + pi*gamma_A*Aeu3v1 + pi*(1-mu_e)*gamma_H*Heu3v1 -kappa3*Rpeu3v1 - omega2_pe*Rpeu3v1 - delta2_eu*Rpeu3v1 + delta1_eu*Rpeu3v0+ delta1_eu*Rneu3v0*(rho_v2)
    
    #Recovered and not seropositive
    dRncr3v1  =  (1-pi)*(1-phi_cv1)*gamma_I*Icr3v1 + (1-pi)*gamma_A*Acr3v1 + (1-pi)*(1-mu_c)*gamma_H*Hcr3v1 + kappa3*Rpcr3v1 - omega2_nc*Rncr3v1 - delta2_cr*Rncr3v1 + delta1_cr*Rncr3v0*(1-rho_v2)
    dRncu3v1  =  (1-pi)*(1-phi_cv1)*gamma_I*Icu3v1 + (1-pi)*gamma_A*Acu3v1 + (1-pi)*(1-mu_c)*gamma_H*Hcu3v1 + kappa3*Rpcu3v1 - omega2_nc*Rncu3v1 - delta2_cu*Rncu3v1 + delta1_cu*Rncu3v0*(1-rho_v2)
    dRnar3v1  =  (1-pi)*(1-phi_av1)*gamma_I*Iar3v1 + (1-pi)*gamma_A*Aar3v1 + (1-pi)*(1-mu_a)*gamma_H*Har3v1 + kappa3*Rpar3v1 - omega2_na*Rnar3v1 - delta2_ar*Rnar3v1 + delta1_ar*Rnar3v0*(1-rho_v2)
    dRnau3v1  =  (1-pi)*(1-phi_av1)*gamma_I*Iau3v1 + (1-pi)*gamma_A*Aau3v1 + (1-pi)*(1-mu_a)*gamma_H*Hau3v1 + kappa3*Rpau3v1 - omega2_na*Rnau3v1 - delta2_au*Rnau3v1 + delta1_au*Rnau3v0*(1-rho_v2)
    dRner3v1  =  (1-pi)*(1-phi_ev1)*gamma_I*Ier3v1 + (1-pi)*gamma_A*Aer3v1 + (1-pi)*(1-mu_e)*gamma_H*Her3v1 + kappa3*Rper3v1 - omega2_ne*Rner3v1 - delta2_er*Rner3v1 + delta1_er*Rner3v0*(1-rho_v2)
    dRneu3v1  =  (1-pi)*(1-phi_ev1)*gamma_I*Ieu3v1 + (1-pi)*gamma_A*Aeu3v1 + (1-pi)*(1-mu_e)*gamma_H*Heu3v1 + kappa3*Rpeu3v1 - omega2_ne*Rneu3v1 - delta2_eu*Rneu3v1 + delta1_eu*Rneu3v0*(1-rho_v2)
    
    #Deaths 
    dDcr3v1  =  mu_c*gamma_H*Hcr3v1
    dDcu3v1  =  mu_c*gamma_H*Hcu3v1
    dDar3v1  =  mu_a*gamma_H*Har3v1
    dDau3v1  =  mu_a*gamma_H*Hau3v1
    dDer3v1  =  mu_e*gamma_H*Her3v1
    dDeu3v1  =  mu_e*gamma_H*Heu3v1
    
    ################################
    ##Vaccinated two dose and unexposed####
    ################################
    #Susceptible and seropositive with S-spike and total IGg
    dSpcr0v2  = omegav_pc*Vpcr0v2 -Spcr0v2 * sd * foi_cr_0v2 - delta3_cr * Spcr0v2  -kappa2 * Spcr0v2
    dSpcu0v2  = omegav_pc*Vpcu0v2 -Spcu0v2 * sd * foi_cu_0v2 - delta3_cu * Spcu0v2  -kappa2 * Spcu0v2
    dSpar0v2  = omegav_pa*Vpar0v2 -Spar0v2 * sd * foi_ar_0v2 - delta3_ar * Spar0v2  -kappa2 * Spar0v2
    dSpau0v2  = omegav_pa*Vpau0v2 -Spau0v2 * sd * foi_au_0v2 - delta3_au * Spau0v2  -kappa2 * Spau0v2
    dSper0v2  = omegav_pe*Vper0v2 -Sper0v2 * sd * foi_er_0v2 - delta3_er * Sper0v2  -kappa2 * Sper0v2
    dSpeu0v2  = omegav_pe*Vpeu0v2 -Speu0v2 * sd * foi_eu_0v2 - delta3_eu * Speu0v2  -kappa2 * Speu0v2
    
    #Susceptible
    dSncr0v2  = omegav_nc*Vncr0v2 -Sncr0v2 * sd * foi_cr_0v2 - delta3_cr * Sncr0v2 +kappa2 * Spcr0v2
    dSncu0v2  = omegav_nc*Vncu0v2 -Sncu0v2 * sd * foi_cu_0v2 - delta3_cu * Sncu0v2 +kappa2 * Spcu0v2
    dSnar0v2  = omegav_na*Vnar0v2 -Snar0v2 * sd * foi_ar_0v2 - delta3_ar * Snar0v2 +kappa2 * Spar0v2
    dSnau0v2  = omegav_na*Vnau0v2 -Snau0v2 * sd * foi_au_0v2 - delta3_au * Snau0v2 +kappa2 * Spau0v2
    dSner0v2  = omegav_ne*Vner0v2 -Sner0v2 * sd * foi_er_0v2 - delta3_er * Sner0v2 +kappa2 * Sper0v2
    dSneu0v2  = omegav_ne*Vneu0v2 -Sneu0v2 * sd * foi_eu_0v2 - delta3_eu * Sneu0v2 +kappa2 * Speu0v2
    
    #Exposed
    dEcr1v2  =  Spcr0v2 * sd * foi_cr_0v2 + Sncr0v2 * sd * foi_cr_0v2-  sigma * Ecr1v2  - delta3_cr*Ecr1v2 +delta2_cr*Ecr1v1
    dEcu1v2  =  Spcu0v2 * sd * foi_cu_0v2 + Sncu0v2 * sd * foi_cu_0v2-  sigma * Ecu1v2  - delta3_cu*Ecu1v2 +delta2_cu*Ecu1v1
    dEar1v2  =  Spar0v2 * sd * foi_ar_0v2 + Snar0v2 * sd * foi_ar_0v2-  sigma * Ear1v2  - delta3_ar*Ear1v2 +delta2_ar*Ear1v1
    dEau1v2  =  Spau0v2 * sd * foi_au_0v2 + Snau0v2 * sd * foi_au_0v2-  sigma * Eau1v2  - delta3_au*Eau1v2 +delta2_au*Eau1v1
    dEer1v2  =  Sper0v2 * sd * foi_er_0v2 + Sner0v2 * sd * foi_er_0v2-  sigma * Eer1v2  - delta3_er*Eer1v2 +delta2_er*Eer1v1
    dEeu1v2  =  Speu0v2 * sd * foi_eu_0v2 + Sneu0v2 * sd * foi_eu_0v2-  sigma * Eeu1v2  - delta3_eu*Eeu1v2 +delta2_eu*Eeu1v1
    
    # Asymptomatic
    dAcr1v2  =  (1-nu_c)*sigma*Ecr1v2 - gamma_A * Acr1v2 - delta3_cr*Acr1v2 +delta2_cr*Acr1v1
    dAcu1v2  =  (1-nu_c)*sigma*Ecu1v2 - gamma_A * Acu1v2 - delta3_cu*Acu1v2 +delta2_cu*Acu1v1
    dAar1v2  =  (1-nu_a)*sigma*Ear1v2 - gamma_A * Aar1v2 - delta3_ar*Aar1v2 +delta2_ar*Aar1v1
    dAau1v2  =  (1-nu_a)*sigma*Eau1v2 - gamma_A * Aau1v2 - delta3_au*Aau1v2 +delta2_au*Aau1v1
    dAer1v2  =  (1-nu_e)*sigma*Eer1v2 - gamma_A * Aer1v2 - delta3_er*Aer1v2 +delta2_er*Aer1v1
    dAeu1v2  =  (1-nu_e)*sigma*Eeu1v2 - gamma_A * Aeu1v2 - delta3_eu*Aeu1v2 +delta2_eu*Aeu1v1  
    
    #Symptomatic 
    dIcr1v2  =  nu_c*sigma*Ecr1v2 - gamma_I * Icr1v2 
    dIcu1v2  =  nu_c*sigma*Ecu1v2 - gamma_I * Icu1v2 
    dIar1v2  =  nu_a*sigma*Ear1v2 - gamma_I * Iar1v2 
    dIau1v2  =  nu_a*sigma*Eau1v2 - gamma_I * Iau1v2 
    dIer1v2  =  nu_e*sigma*Eer1v2 - gamma_I * Ier1v2
    dIeu1v2  =  nu_e*sigma*Eeu1v2 - gamma_I * Ieu1v2
    
    # Hospitalized
    dHcr1v2  =  phi_cv2*gamma_I*Icr1v2 - gamma_H * Hcr1v2
    dHcu1v2  =  phi_cv2*gamma_I*Icu1v2 - gamma_H * Hcu1v2
    dHar1v2  =  phi_av2*gamma_I*Iar1v2 - gamma_H * Har1v2
    dHau1v2  =  phi_av2*gamma_I*Iau1v2 - gamma_H * Hau1v2
    dHer1v2  =  phi_ev2*gamma_I*Ier1v2 - gamma_H * Her1v2
    dHeu1v2  =  phi_ev2*gamma_I*Ieu1v2 - gamma_H * Heu1v2
    
    # Recovered and seroconverted
    dRpcr1v2  =  pi*(1-phi_cv2)*gamma_I*Icr1v2 + pi*gamma_A*Acr1v2 + pi*(1-mu_c)*gamma_H*Hcr1v2 -kappa3*Rpcr1v2 - omega_pc*Rpcr1v2 - delta3_cr*Rpcr1v2 +delta2_cr*Rpcr1v1+delta2_cr*Rncr1v1*(rho_v2)+delta2_cr*Spcr1v1 +delta2_cr*Sncr1v1*(rho_v2)
    dRpcu1v2  =  pi*(1-phi_cv2)*gamma_I*Icu1v2 + pi*gamma_A*Acu1v2 + pi*(1-mu_c)*gamma_H*Hcu1v2 -kappa3*Rpcu1v2 - omega_pc*Rpcu1v2 - delta3_cu*Rpcu1v2 +delta2_cu*Rpcu1v1+delta2_cu*Rncu1v1*(rho_v2)+delta2_cu*Spcu1v1+delta2_cu*Sncu1v1*(rho_v2)
    dRpar1v2  =  pi*(1-phi_av2)*gamma_I*Iar1v2 + pi*gamma_A*Aar1v2 + pi*(1-mu_a)*gamma_H*Har1v2 -kappa3*Rpar1v2 - omega_pa*Rpar1v2 - delta3_ar*Rpar1v2 +delta2_ar*Rpar1v1+delta2_ar*Rnar1v1*(rho_v2)+delta2_ar*Spar1v1+delta2_ar*Snar1v1*(rho_v2)
    dRpau1v2  =  pi*(1-phi_av2)*gamma_I*Iau1v2 + pi*gamma_A*Aau1v2 + pi*(1-mu_a)*gamma_H*Hau1v2 -kappa3*Rpau1v2 - omega_pa*Rpau1v2 - delta3_au*Rpau1v2 +delta2_au*Rpau1v1+delta2_au*Rnau1v1*(rho_v2)+delta2_au*Spau1v1+delta2_au*Snau1v1*(rho_v2)
    dRper1v2  =  pi*(1-phi_ev2)*gamma_I*Ier1v2 + pi*gamma_A*Aer1v2 + pi*(1-mu_e)*gamma_H*Her1v2 -kappa3*Rper1v2 - omega_pe*Rper1v2 - delta3_er*Rper1v2 +delta2_er*Rper1v1+delta2_er*Rner1v1*(rho_v2)+delta2_er*Sper1v1+delta2_er*Sner1v1*(rho_v2)
    dRpeu1v2  =  pi*(1-phi_ev2)*gamma_I*Ieu1v2 + pi*gamma_A*Aeu1v2 + pi*(1-mu_e)*gamma_H*Heu1v2 -kappa3*Rpeu1v2 - omega_pe*Rpeu1v2 - delta3_eu*Rpeu1v2 +delta2_eu*Rpeu1v1+delta2_eu*Rneu1v1*(rho_v2)+delta2_eu*Speu1v1+delta2_eu*Sneu1v1*(rho_v2)
    
    #Recovered and not seropositive
    dRncr1v2  =  (1-pi)*(1-phi_cv2)*gamma_I*Icr1v2 + (1-pi)*gamma_A*Acr1v2 + (1-pi)*(1-mu_c)*gamma_H*Hcr1v2 + kappa3*Rpcr1v2 - omega_nc*Rncr1v2 - delta3_cr*Rncr1v2 +delta2_cr*Rncr1v1*(1-rho_v2)+delta2_cr*Sncr1v1*(1-rho_v2)
    dRncu1v2  =  (1-pi)*(1-phi_cv2)*gamma_I*Icu1v2 + (1-pi)*gamma_A*Acu1v2 + (1-pi)*(1-mu_c)*gamma_H*Hcu1v2 + kappa3*Rpcu1v2 - omega_nc*Rncu1v2 - delta3_cu*Rncu1v2 +delta2_cu*Rncu1v1*(1-rho_v2)+delta2_cu*Sncu1v1*(1-rho_v2)
    dRnar1v2  =  (1-pi)*(1-phi_av2)*gamma_I*Iar1v2 + (1-pi)*gamma_A*Aar1v2 + (1-pi)*(1-mu_a)*gamma_H*Har1v2 + kappa3*Rpar1v2 - omega_na*Rnar1v2 - delta3_ar*Rnar1v2 +delta2_ar*Rnar1v1*(1-rho_v2)+delta2_ar*Snar1v1*(1-rho_v2)
    dRnau1v2  =  (1-pi)*(1-phi_av2)*gamma_I*Iau1v2 + (1-pi)*gamma_A*Aau1v2 + (1-pi)*(1-mu_a)*gamma_H*Hau1v2 + kappa3*Rpau1v2 - omega_na*Rnau1v2 - delta3_au*Rnau1v2 +delta2_au*Rnau1v1*(1-rho_v2)+delta2_au*Snau1v1*(1-rho_v2)
    dRner1v2  =  (1-pi)*(1-phi_ev2)*gamma_I*Ier1v2 + (1-pi)*gamma_A*Aer1v2 + (1-pi)*(1-mu_e)*gamma_H*Her1v2 + kappa3*Rper1v2 - omega_ne*Rner1v2 - delta3_er*Rner1v2 +delta2_er*Rner1v1*(1-rho_v2)+delta2_er*Sner1v1*(1-rho_v2)
    dRneu1v2  =  (1-pi)*(1-phi_ev2)*gamma_I*Ieu1v2 + (1-pi)*gamma_A*Aeu1v2 + (1-pi)*(1-mu_e)*gamma_H*Heu1v2 + kappa3*Rpeu1v2 - omega_ne*Rneu1v2 - delta3_eu*Rneu1v2 +delta2_eu*Rneu1v1*(1-rho_v2)+delta2_eu*Sneu1v1*(1-rho_v2)
    
    #Deaths 
    dDcr1v2  =  mu_c*gamma_H*Hcr1v2
    dDcu1v2  =  mu_c*gamma_H*Hcu1v2
    dDar1v2  =  mu_a*gamma_H*Har1v2
    dDau1v2  =  mu_a*gamma_H*Hau1v2
    dDer1v2  =  mu_e*gamma_H*Her1v2
    dDeu1v2  =  mu_e*gamma_H*Heu1v2
    
    ################################
    ##Vaccinated two dose and exposed once####
    ################################
    #Susceptible
    dSpcr1v2  = omega_pc*Rpcr1v2 -kappa3*Spcr1v2 - Spcr1v2 * sd * foi_cr_1v2 - delta3_cr * Spcr1v2 
    dSpcu1v2  = omega_pc*Rpcu1v2 -kappa3*Spcu1v2 - Spcu1v2 * sd * foi_cu_1v2 - delta3_cu * Spcu1v2 
    dSpar1v2  = omega_pa*Rpar1v2 -kappa3*Spar1v2 - Spar1v2 * sd * foi_ar_1v2 - delta3_ar * Spar1v2 
    dSpau1v2  = omega_pa*Rpau1v2 -kappa3*Spau1v2 - Spau1v2 * sd * foi_au_1v2 - delta3_au * Spau1v2 
    dSper1v2  = omega_pe*Rper1v2 -kappa3*Sper1v2 - Sper1v2 * sd * foi_er_1v2 - delta3_er * Sper1v2 
    dSpeu1v2  = omega_pe*Rpeu1v2 -kappa3*Speu1v2 - Speu1v2 * sd * foi_eu_1v2 - delta3_eu * Speu1v2 
    
    dSncr1v2  = omega_nc*Rncr1v2 + kappa3*Spcr1v2 -Sncr1v2 * sd * foi_cr_1v2 - delta3_cr * Sncr1v2  
    dSncu1v2  = omega_nc*Rncu1v2 + kappa3*Spcu1v2 -Sncu1v2 * sd * foi_cu_1v2 - delta3_cu * Sncu1v2 
    dSnar1v2  = omega_na*Rnar1v2 + kappa3*Spar1v2 -Snar1v2 * sd * foi_ar_1v2 - delta3_ar * Snar1v2 
    dSnau1v2  = omega_na*Rnau1v2 + kappa3*Spau1v2 -Snau1v2 * sd * foi_au_1v2 - delta3_au * Snau1v2 
    dSner1v2  = omega_ne*Rner1v2 + kappa3*Sper1v2 -Sner1v2 * sd * foi_er_1v2 - delta3_er * Sner1v2 
    dSneu1v2  = omega_ne*Rneu1v2 + kappa3*Speu1v2 -Sneu1v2 * sd * foi_eu_1v2 - delta3_eu * Sneu1v2
    
    #Exposed
    dEcr2v2  =  Spcr1v2 * sd * foi_cr_1v2 + Sncr1v2 * sd * foi_cr_1v2 -  sigma * Ecr2v2  - delta3_cr*Ecr2v2 +delta2_cr*Ecr2v1
    dEcu2v2  =  Spcu1v2 * sd * foi_cu_1v2 + Sncu1v2 * sd * foi_cu_1v2 -  sigma * Ecu2v2  - delta3_cu*Ecu2v2 +delta2_cu*Ecu2v1
    dEar2v2  =  Spar1v2 * sd * foi_ar_1v2 + Snar1v2 * sd * foi_ar_1v2 -  sigma * Ear2v2  - delta3_ar*Ear2v2 +delta2_ar*Ear2v1
    dEau2v2  =  Spau1v2 * sd * foi_au_1v2 + Snau1v2 * sd * foi_au_1v2 -  sigma * Eau2v2  - delta3_au*Eau2v2 +delta2_au*Eau2v1
    dEer2v2  =  Sper1v2 * sd * foi_er_1v2 + Sner1v2 * sd * foi_er_1v2 -  sigma * Eer2v2  - delta3_er*Eer2v2 +delta2_er*Eer2v1
    dEeu2v2  =  Speu1v2 * sd * foi_eu_1v2 + Sneu1v2 * sd * foi_eu_1v2 -  sigma * Eeu2v2  - delta3_eu*Eeu2v2 +delta2_eu*Eeu2v1
    
    # Asymptomatic
    dAcr2v2  =  (1-nu_c)*sigma*Ecr2v2 - gamma_A * Acr2v2 - delta3_cr*Acr2v2 + delta2_cr*Acr2v1
    dAcu2v2  =  (1-nu_c)*sigma*Ecu2v2 - gamma_A * Acu2v2 - delta3_cu*Acu2v2 + delta2_cu*Acu2v1
    dAar2v2  =  (1-nu_a)*sigma*Ear2v2 - gamma_A * Aar2v2 - delta3_ar*Aar2v2 + delta2_ar*Aar2v1
    dAau2v2  =  (1-nu_a)*sigma*Eau2v2 - gamma_A * Aau2v2 - delta3_au*Aau2v2 + delta2_au*Aau2v1
    dAer2v2  =  (1-nu_e)*sigma*Eer2v2 - gamma_A * Aer2v2 - delta3_er*Aer2v2 + delta2_er*Aer2v1
    dAeu2v2  =  (1-nu_e)*sigma*Eeu2v2 - gamma_A * Aeu2v2 - delta3_eu*Aeu2v2 + delta2_eu*Aeu2v1
    
    #Symptomatic 
    dIcr2v2  =  nu_c*sigma*Ecr2v2 - gamma_I * Icr2v2 
    dIcu2v2  =  nu_c*sigma*Ecu2v2 - gamma_I * Icu2v2 
    dIar2v2  =  nu_a*sigma*Ear2v2 - gamma_I * Iar2v2 
    dIau2v2  =  nu_a*sigma*Eau2v2 - gamma_I * Iau2v2 
    dIer2v2  =  nu_e*sigma*Eer2v2 - gamma_I * Ier2v2
    dIeu2v2  =  nu_e*sigma*Eeu2v2 - gamma_I * Ieu2v2
    
    # Hospitalized
    dHcr2v2  =  phi_cv2*gamma_I*Icr2v2 - gamma_H * Hcr2v2
    dHcu2v2  =  phi_cv2*gamma_I*Icu2v2 - gamma_H * Hcu2v2
    dHar2v2  =  phi_av2*gamma_I*Iar2v2 - gamma_H * Har2v2
    dHau2v2  =  phi_av2*gamma_I*Iau2v2 - gamma_H * Hau2v2
    dHer2v2  =  phi_ev2*gamma_I*Ier2v2 - gamma_H * Her2v2
    dHeu2v2  =  phi_ev2*gamma_I*Ieu2v2 - gamma_H * Heu2v2
    
    # Recovered and seroconverted
    dRpcr2v2  =  pi*(1-phi_cv2)*gamma_I*Icr2v2 + pi*gamma_A*Acr2v2 + pi*(1-mu_c)*gamma_H*Hcr2v2 -kappa3*Rpcr2v2 - omega_pc*Rpcr2v2 - delta3_cr*Rpcr2v2 +delta2_cr*Rpcr2v1+ delta2_cr*Rncr2v1*rho_v2
    dRpcu2v2  =  pi*(1-phi_cv2)*gamma_I*Icu2v2 + pi*gamma_A*Acu2v2 + pi*(1-mu_c)*gamma_H*Hcu2v2 -kappa3*Rpcu2v2 - omega_pc*Rpcu2v2 - delta3_cu*Rpcu2v2 +delta2_cu*Rpcu2v1+ delta2_cu*Rncu2v1*rho_v2
    dRpar2v2  =  pi*(1-phi_av2)*gamma_I*Iar2v2 + pi*gamma_A*Aar2v2 + pi*(1-mu_a)*gamma_H*Har2v2 -kappa3*Rpar2v2 - omega_pa*Rpar2v2 - delta3_ar*Rpar2v2 +delta2_ar*Rpar2v1+ delta2_ar*Rnar2v1*rho_v2
    dRpau2v2  =  pi*(1-phi_av2)*gamma_I*Iau2v2 + pi*gamma_A*Aau2v2 + pi*(1-mu_a)*gamma_H*Hau2v2 -kappa3*Rpau2v2 - omega_pa*Rpau2v2 - delta3_au*Rpau2v2 +delta2_au*Rpau2v1+ delta2_au*Rnau2v1*rho_v2
    dRper2v2  =  pi*(1-phi_ev2)*gamma_I*Ier2v2 + pi*gamma_A*Aer2v2 + pi*(1-mu_e)*gamma_H*Her2v2 -kappa3*Rper2v2 - omega_pe*Rper2v2 - delta3_er*Rper2v2 +delta2_er*Rper2v1+ delta2_er*Rner2v1*rho_v2
    dRpeu2v2  =  pi*(1-phi_ev2)*gamma_I*Ieu2v2 + pi*gamma_A*Aeu2v2 + pi*(1-mu_e)*gamma_H*Heu2v2 -kappa3*Rpeu2v2 - omega_pe*Rpeu2v2 - delta3_eu*Rpeu2v2 +delta2_eu*Rpeu2v1+ delta2_eu*Rneu2v1*rho_v2
    
    #Recovered and not seropositive
    dRncr2v2  =  (1-pi)*(1-phi_cv2)*gamma_I*Icr2v2 + (1-pi)*gamma_A*Acr2v2 + (1-pi)*(1-mu_c)*gamma_H*Hcr2v2 + kappa3*Rpcr2v2 - omega_nc*Rncr2v2 - delta3_cr*Rncr2v2 + delta2_cr*Rncr2v1*(1-rho_v2)
    dRncu2v2  =  (1-pi)*(1-phi_cv2)*gamma_I*Icu2v2 + (1-pi)*gamma_A*Acu2v2 + (1-pi)*(1-mu_c)*gamma_H*Hcu2v2 + kappa3*Rpcu2v2 - omega_nc*Rncu2v2 - delta3_cu*Rncu2v2 + delta2_cu*Rncu2v1*(1-rho_v2)
    dRnar2v2  =  (1-pi)*(1-phi_av2)*gamma_I*Iar2v2 + (1-pi)*gamma_A*Aar2v2 + (1-pi)*(1-mu_a)*gamma_H*Har2v2 + kappa3*Rpar2v2 - omega_na*Rnar2v2 - delta3_ar*Rnar2v2 + delta2_ar*Rnar2v1*(1-rho_v2)
    dRnau2v2  =  (1-pi)*(1-phi_av2)*gamma_I*Iau2v2 + (1-pi)*gamma_A*Aau2v2 + (1-pi)*(1-mu_a)*gamma_H*Hau2v2 + kappa3*Rpau2v2 - omega_na*Rnau2v2 - delta3_au*Rnau2v2 + delta2_au*Rnau2v1*(1-rho_v2)
    dRner2v2  =  (1-pi)*(1-phi_ev2)*gamma_I*Ier2v2 + (1-pi)*gamma_A*Aer2v2 + (1-pi)*(1-mu_e)*gamma_H*Her2v2 + kappa3*Rper2v2 - omega_ne*Rner2v2 - delta3_er*Rner2v2 + delta2_er*Rner2v1*(1-rho_v2)
    dRneu2v2  =  (1-pi)*(1-phi_ev2)*gamma_I*Ieu2v2 + (1-pi)*gamma_A*Aeu2v2 + (1-pi)*(1-mu_e)*gamma_H*Heu2v2 + kappa3*Rpeu2v2 - omega_ne*Rneu2v2 - delta3_eu*Rneu2v2 + delta2_eu*Rneu2v1*(1-rho_v2)
    
    #Deaths 
    dDcr2v2  =  mu_c*gamma_H*Hcr2v2
    dDcu2v2  =  mu_c*gamma_H*Hcu2v2
    dDar2v2  =  mu_a*gamma_H*Har2v2
    dDau2v2  =  mu_a*gamma_H*Hau2v2
    dDer2v2  =  mu_e*gamma_H*Her2v2
    dDeu2v2  =  mu_e*gamma_H*Heu2v2
    
    ################################
    ##Vaccinated two dose and exposed twice####
    ################################
    #Susceptible
    dSpcr2v2  = omega_pc*Rpcr2v2 -kappa3*Spcr2v2 - Spcr2v2 * sd * foi_cr_2v2 - delta3_cr * Spcr2v2 + delta2_cr*Spcr2v1+ delta2_cr*Sncr2v1*(rho_v2)+ omega2_pc*Rpcr3v2  
    dSpcu2v2  = omega_pc*Rpcu2v2 -kappa3*Spcu2v2 - Spcu2v2 * sd * foi_cu_2v2 - delta3_cu * Spcu2v2 + delta2_cu*Spcu2v1+ delta2_cu*Sncu2v1*(rho_v2)+ omega2_pc*Rpcu3v2  
    dSpar2v2  = omega_pa*Rpar2v2 -kappa3*Spar2v2 - Spar2v2 * sd * foi_ar_2v2 - delta3_ar * Spar2v2 + delta2_ar*Spar2v1+ delta2_ar*Snar2v1*(rho_v2)+ omega2_pa*Rpar3v2  
    dSpau2v2  = omega_pa*Rpau2v2 -kappa3*Spau2v2 - Spau2v2 * sd * foi_au_2v2 - delta3_au * Spau2v2 + delta2_au*Spau2v1+ delta2_au*Snau2v1*(rho_v2)+ omega2_pa*Rpau3v2  
    dSper2v2  = omega_pe*Rper2v2 -kappa3*Sper2v2 - Sper2v2 * sd * foi_er_2v2 - delta3_er * Sper2v2 + delta2_er*Sper2v1+ delta2_er*Sner2v1*(rho_v2)+ omega2_pe*Rper3v2  
    dSpeu2v2  = omega_pe*Rpeu2v2 -kappa3*Speu2v2 - Speu2v2 * sd * foi_eu_2v2 - delta3_eu * Speu2v2 + delta2_eu*Speu2v1+ delta2_eu*Sneu2v1*(rho_v2)+ omega2_pe*Rpeu3v2  
    
    dSncr2v2  = omega_nc*Rncr2v2 + kappa3*Spcr2v2 -Sncr2v2 * sd * foi_cr_2v2 - delta3_cr * Sncr2v2 + delta2_cr*Sncr2v1*(1-rho_v2) + omega2_nc*Rncr3v2
    dSncu2v2  = omega_nc*Rncu2v2 + kappa3*Spcu2v2 -Sncu2v2 * sd * foi_cu_2v2 - delta3_cu * Sncu2v2 + delta2_cu*Sncu2v1*(1-rho_v2) + omega2_nc*Rncu3v2
    dSnar2v2  = omega_na*Rnar2v2 + kappa3*Spar2v2 -Snar2v2 * sd * foi_ar_2v2 - delta3_ar * Snar2v2 + delta2_ar*Snar2v1*(1-rho_v2) + omega2_na*Rnar3v2
    dSnau2v2  = omega_na*Rnau2v2 + kappa3*Spau2v2 -Snau2v2 * sd * foi_au_2v2 - delta3_au * Snau2v2 + delta2_au*Snau2v1*(1-rho_v2) + omega2_na*Rnau3v2
    dSner2v2  = omega_ne*Rner2v2 + kappa3*Sper2v2 -Sner2v2 * sd * foi_er_2v2 - delta3_er * Sner2v2 + delta2_er*Sner2v1*(1-rho_v2) + omega2_ne*Rner3v2
    dSneu2v2  = omega_ne*Rneu2v2 + kappa3*Speu2v2 -Sneu2v2 * sd * foi_eu_2v2 - delta3_eu * Sneu2v2 + delta2_eu*Sneu2v1*(1-rho_v2) + omega2_ne*Rneu3v2
    
    #Exposed
    dEcr3v2  =  Spcr2v2 * sd * foi_cr_2v2 + Sncr2v2 * sd * foi_cr_2v2 -  sigma * Ecr3v2  - delta3_cr*Ecr3v2 + delta2_cr*Ecr3v1
    dEcu3v2  =  Spcu2v2 * sd * foi_cu_2v2 + Sncu2v2 * sd * foi_cu_2v2 -  sigma * Ecu3v2  - delta3_cu*Ecu3v2 + delta2_cu*Ecu3v1
    dEar3v2  =  Spar2v2 * sd * foi_ar_2v2 + Snar2v2 * sd * foi_ar_2v2 -  sigma * Ear3v2  - delta3_ar*Ear3v2 + delta2_ar*Ear3v1
    dEau3v2  =  Spau2v2 * sd * foi_au_2v2 + Snau2v2 * sd * foi_au_2v2 -  sigma * Eau3v2  - delta3_au*Eau3v2 + delta2_au*Eau3v1
    dEer3v2  =  Sper2v2 * sd * foi_er_2v2 + Sner2v2 * sd * foi_er_2v2 -  sigma * Eer3v2  - delta3_er*Eer3v2 + delta2_er*Eer3v1
    dEeu3v2  =  Speu2v2 * sd * foi_eu_2v2 + Sneu2v2 * sd * foi_eu_2v2 -  sigma * Eeu3v2  - delta3_eu*Eeu3v2 + delta2_eu*Eeu3v1
    
    # Asymptomatic
    dAcr3v2  =  (1-nu_c)*sigma*Ecr3v2 - gamma_A * Acr3v2 - delta3_cr*Acr3v2+ delta2_cr*Acr3v1
    dAcu3v2  =  (1-nu_c)*sigma*Ecu3v2 - gamma_A * Acu3v2 - delta3_cu*Acu3v2+ delta2_cu*Acu3v1
    dAar3v2  =  (1-nu_a)*sigma*Ear3v2 - gamma_A * Aar3v2 - delta3_ar*Aar3v2+ delta2_ar*Aar3v1
    dAau3v2  =  (1-nu_a)*sigma*Eau3v2 - gamma_A * Aau3v2 - delta3_au*Aau3v2+ delta2_au*Aau3v1
    dAer3v2  =  (1-nu_e)*sigma*Eer3v2 - gamma_A * Aer3v2 - delta3_er*Aer3v2+ delta2_er*Aer3v1
    dAeu3v2  =  (1-nu_e)*sigma*Eeu3v2 - gamma_A * Aeu3v2 - delta3_eu*Aeu3v2+ delta2_eu*Aeu3v1
    
    #Symptomatic 
    dIcr3v2  =  nu_c*sigma*Ecr3v2 - gamma_I * Icr3v2 
    dIcu3v2  =  nu_c*sigma*Ecu3v2 - gamma_I * Icu3v2 
    dIar3v2  =  nu_a*sigma*Ear3v2 - gamma_I * Iar3v2 
    dIau3v2  =  nu_a*sigma*Eau3v2 - gamma_I * Iau3v2 
    dIer3v2  =  nu_e*sigma*Eer3v2 - gamma_I * Ier3v2
    dIeu3v2  =  nu_e*sigma*Eeu3v2 - gamma_I * Ieu3v2
    
    # Hospitalized
    dHcr3v2  =  phi_cv2*gamma_I*Icr3v2 - gamma_H * Hcr3v2
    dHcu3v2  =  phi_cv2*gamma_I*Icu3v2 - gamma_H * Hcu3v2
    dHar3v2  =  phi_av2*gamma_I*Iar3v2 - gamma_H * Har3v2
    dHau3v2  =  phi_av2*gamma_I*Iau3v2 - gamma_H * Hau3v2
    dHer3v2  =  phi_ev2*gamma_I*Ier3v2 - gamma_H * Her3v2
    dHeu3v2  =  phi_ev2*gamma_I*Ieu3v2 - gamma_H * Heu3v2
    
    # Recovered and seroconverted
    dRpcr3v2  =  pi*(1-phi_cv2)*gamma_I*Icr3v2 + pi*gamma_A*Acr3v2 + pi*(1-mu_c)*gamma_H*Hcr3v2 -kappa3*Rpcr3v2 - omega2_pc*Rpcr3v2 - delta3_cr*Rpcr3v2 + delta2_cr*Rpcr3v1+ delta2_cr*Rncr3v1*(rho_v2) 
    dRpcu3v2  =  pi*(1-phi_cv2)*gamma_I*Icu3v2 + pi*gamma_A*Acu3v2 + pi*(1-mu_c)*gamma_H*Hcu3v2 -kappa3*Rpcu3v2 - omega2_pc*Rpcu3v2 - delta3_cu*Rpcu3v2 + delta2_cu*Rpcu3v1+ delta2_cu*Rncu3v1*(rho_v2) 
    dRpar3v2  =  pi*(1-phi_av2)*gamma_I*Iar3v2 + pi*gamma_A*Aar3v2 + pi*(1-mu_a)*gamma_H*Har3v2 -kappa3*Rpar3v2 - omega2_pa*Rpar3v2 - delta3_ar*Rpar3v2 + delta2_ar*Rpar3v1+ delta2_ar*Rnar3v1*(rho_v2)  
    dRpau3v2  =  pi*(1-phi_av2)*gamma_I*Iau3v2 + pi*gamma_A*Aau3v2 + pi*(1-mu_a)*gamma_H*Hau3v2 -kappa3*Rpau3v2 - omega2_pa*Rpau3v2 - delta3_au*Rpau3v2 + delta2_au*Rpau3v1+ delta2_au*Rnau3v1*(rho_v2) 
    dRper3v2  =  pi*(1-phi_ev2)*gamma_I*Ier3v2 + pi*gamma_A*Aer3v2 + pi*(1-mu_e)*gamma_H*Her3v2 -kappa3*Rper3v2 - omega2_pe*Rper3v2 - delta3_er*Rper3v2 + delta2_er*Rper3v1+ delta2_er*Rner3v1*(rho_v2) 
    dRpeu3v2  =  pi*(1-phi_ev2)*gamma_I*Ieu3v2 + pi*gamma_A*Aeu3v2 + pi*(1-mu_e)*gamma_H*Heu3v2 -kappa3*Rpeu3v2 - omega2_pe*Rpeu3v2 - delta3_eu*Rpeu3v2 + delta2_eu*Rpeu3v1+ delta2_eu*Rneu3v1*(rho_v2) 
    
    #Recovered and not seropositive
    dRncr3v2  =  (1-pi)*(1-phi_cv2)*gamma_I*Icr3v2 + (1-pi)*gamma_A*Acr3v2 + (1-pi)*(1-mu_c)*gamma_H*Hcr3v2 + kappa3*Rpcr3v2 - omega2_nc*Rncr3v2 - delta3_cr*Rncr3v2 + delta2_cr*Rncr3v1*(1-rho_v2) 
    dRncu3v2  =  (1-pi)*(1-phi_cv2)*gamma_I*Icu3v2 + (1-pi)*gamma_A*Acu3v2 + (1-pi)*(1-mu_c)*gamma_H*Hcu3v2 + kappa3*Rpcu3v2 - omega2_nc*Rncu3v2 - delta3_cu*Rncu3v2 + delta2_cu*Rncu3v1*(1-rho_v2) 
    dRnar3v2  =  (1-pi)*(1-phi_av2)*gamma_I*Iar3v2 + (1-pi)*gamma_A*Aar3v2 + (1-pi)*(1-mu_a)*gamma_H*Har3v2 + kappa3*Rpar3v2 - omega2_na*Rnar3v2 - delta3_ar*Rnar3v2 + delta2_ar*Rnar3v1*(1-rho_v2) 
    dRnau3v2  =  (1-pi)*(1-phi_av2)*gamma_I*Iau3v2 + (1-pi)*gamma_A*Aau3v2 + (1-pi)*(1-mu_a)*gamma_H*Hau3v2 + kappa3*Rpau3v2 - omega2_na*Rnau3v2 - delta3_au*Rnau3v2 + delta2_au*Rnau3v1*(1-rho_v2) 
    dRner3v2  =  (1-pi)*(1-phi_ev2)*gamma_I*Ier3v2 + (1-pi)*gamma_A*Aer3v2 + (1-pi)*(1-mu_e)*gamma_H*Her3v2 + kappa3*Rper3v2 - omega2_ne*Rner3v2 - delta3_er*Rner3v2 + delta2_er*Rner3v1*(1-rho_v2) 
    dRneu3v2  =  (1-pi)*(1-phi_ev2)*gamma_I*Ieu3v2 + (1-pi)*gamma_A*Aeu3v2 + (1-pi)*(1-mu_e)*gamma_H*Heu3v2 + kappa3*Rpeu3v2 - omega2_ne*Rneu3v2 - delta3_eu*Rneu3v2 + delta2_eu*Rneu3v1*(1-rho_v2) 
    
    #Deaths 
    dDcr3v2  =  mu_c*gamma_H*Hcr3v2
    dDcu3v2  =  mu_c*gamma_H*Hcu3v2
    dDar3v2  =  mu_a*gamma_H*Har3v2
    dDau3v2  =  mu_a*gamma_H*Hau3v2
    dDer3v2  =  mu_e*gamma_H*Her3v2
    dDeu3v2  =  mu_e*gamma_H*Heu3v2
    
    ################################
    ##Vaccinated three dose and unexposed####
    ################################
    #Susceptible seropositive
    dSpcr0v3  = omegav_pc*Vpcr0v3 -Spcr0v3 * sd * foi_cr_0v3 - kappa3*Spcr0v3 - delta4_cr * Spcr0v3
    dSpcu0v3  = omegav_pc*Vpcu0v3 -Spcu0v3 * sd * foi_cu_0v3 - kappa3*Spcu0v3 - delta4_cu * Spcu0v3
    dSpar0v3  = omegav_pa*Vpar0v3 -Spar0v3 * sd * foi_ar_0v3 - kappa3*Spar0v3 - delta4_ar * Spar0v3
    dSpau0v3  = omegav_pa*Vpau0v3 -Spau0v3 * sd * foi_au_0v3 - kappa3*Spau0v3 - delta4_au * Spau0v3
    dSper0v3  = omegav_pe*Vper0v3 -Sper0v3 * sd * foi_er_0v3 - kappa3*Sper0v3 - delta4_er * Sper0v3
    dSpeu0v3  = omegav_pe*Vpeu0v3 -Speu0v3 * sd * foi_eu_0v3 - kappa3*Speu0v3 - delta4_eu * Speu0v3
    
    
    #Susceptible seronegative
    dSncr0v3  = omegav_nc*Vncr0v3 -Sncr0v3*sd*foi_cr_0v3 + kappa3*Spcr0v3 - delta4_cr*Sncr0v3 
    dSncu0v3  = omegav_nc*Vncu0v3 -Sncu0v3*sd*foi_cu_0v3 + kappa3*Spcu0v3 - delta4_cu*Sncu0v3 
    dSnar0v3  = omegav_na*Vnar0v3 -Snar0v3*sd*foi_ar_0v3 + kappa3*Spar0v3 - delta4_ar*Snar0v3 
    dSnau0v3  = omegav_na*Vnau0v3 -Snau0v3*sd*foi_au_0v3 + kappa3*Spau0v3 - delta4_au*Snau0v3 
    dSner0v3  = omegav_ne*Vner0v3 -Sner0v3*sd*foi_er_0v3 + kappa3*Sper0v3 - delta4_er*Sner0v3 
    dSneu0v3  = omegav_ne*Vneu0v3 -Sneu0v3*sd*foi_eu_0v3 + kappa3*Speu0v3 - delta4_eu*Sneu0v3 
    
    #Exposed
    dEcr1v3  =  Spcr0v3 * sd * foi_cr_0v3 + Sncr0v3 * sd * foi_cr_0v3-  sigma * Ecr1v3 +delta3_cr*Ecr1v2
    dEcu1v3  =  Spcu0v3 * sd * foi_cu_0v3 + Sncu0v3 * sd * foi_cu_0v3-  sigma * Ecu1v3 +delta3_cu*Ecu1v2
    dEar1v3  =  Spar0v3 * sd * foi_ar_0v3 + Snar0v3 * sd * foi_ar_0v3-  sigma * Ear1v3 +delta3_ar*Ear1v2
    dEau1v3  =  Spau0v3 * sd * foi_au_0v3 + Snau0v3 * sd * foi_au_0v3-  sigma * Eau1v3 +delta3_au*Eau1v2
    dEer1v3  =  Sper0v3 * sd * foi_er_0v3 + Sner0v3 * sd * foi_er_0v3-  sigma * Eer1v3 +delta3_er*Eer1v2
    dEeu1v3  =  Speu0v3 * sd * foi_eu_0v3 + Sneu0v3 * sd * foi_eu_0v3-  sigma * Eeu1v3 +delta3_eu*Eeu1v2
    
    # Asymptomatic
    dAcr1v3  =  (1-nu_c)*sigma*Ecr1v3 - gamma_A * Acr1v3 +delta3_cr*Acr1v2
    dAcu1v3  =  (1-nu_c)*sigma*Ecu1v3 - gamma_A * Acu1v3 +delta3_cu*Acu1v2
    dAar1v3  =  (1-nu_a)*sigma*Ear1v3 - gamma_A * Aar1v3 +delta3_ar*Aar1v2
    dAau1v3  =  (1-nu_a)*sigma*Eau1v3 - gamma_A * Aau1v3 +delta3_au*Aau1v2
    dAer1v3  =  (1-nu_e)*sigma*Eer1v3 - gamma_A * Aer1v3 +delta3_er*Aer1v2
    dAeu1v3  =  (1-nu_e)*sigma*Eeu1v3 - gamma_A * Aeu1v3 +delta3_eu*Aeu1v2  
    
    #Symptomatic 
    dIcr1v3  =  nu_c*sigma*Ecr1v3 - gamma_I * Icr1v3 
    dIcu1v3  =  nu_c*sigma*Ecu1v3 - gamma_I * Icu1v3 
    dIar1v3  =  nu_a*sigma*Ear1v3 - gamma_I * Iar1v3 
    dIau1v3  =  nu_a*sigma*Eau1v3 - gamma_I * Iau1v3 
    dIer1v3  =  nu_e*sigma*Eer1v3 - gamma_I * Ier1v3
    dIeu1v3  =  nu_e*sigma*Eeu1v3 - gamma_I * Ieu1v3
    
    # Hospitalized
    dHcr1v3  =  phi_cv3*gamma_I*Icr1v3 - gamma_H * Hcr1v3
    dHcu1v3  =  phi_cv3*gamma_I*Icu1v3 - gamma_H * Hcu1v3
    dHar1v3  =  phi_av3*gamma_I*Iar1v3 - gamma_H * Har1v3
    dHau1v3  =  phi_av3*gamma_I*Iau1v3 - gamma_H * Hau1v3
    dHer1v3  =  phi_ev3*gamma_I*Ier1v3 - gamma_H * Her1v3
    dHeu1v3  =  phi_ev3*gamma_I*Ieu1v3 - gamma_H * Heu1v3
    
    # Recovered and seroconverted
    dRpcr1v3  =  pi*(1-phi_cv3)*gamma_I*Icr1v3 + pi*gamma_A*Acr1v3 + pi*(1-mu_c)*gamma_H*Hcr1v3 -kappa3*Rpcr1v3 - omega_pc*Rpcr1v3 - delta4_cr*Rpcr1v3 +delta3_cr*Rpcr1v2+delta3_cr*Rncr1v2*(rho_v3)+delta3_cr*Spcr1v2+delta3_cr*Sncr1v2*(rho_v3)   
    dRpcu1v3  =  pi*(1-phi_cv3)*gamma_I*Icu1v3 + pi*gamma_A*Acu1v3 + pi*(1-mu_c)*gamma_H*Hcu1v3 -kappa3*Rpcu1v3 - omega_pc*Rpcu1v3 - delta4_cu*Rpcu1v3 +delta3_cu*Rpcu1v2+delta3_cu*Rncu1v2*(rho_v3)+delta3_cu*Spcu1v2+delta3_cu*Sncu1v2*(rho_v3)
    dRpar1v3  =  pi*(1-phi_av3)*gamma_I*Iar1v3 + pi*gamma_A*Aar1v3 + pi*(1-mu_a)*gamma_H*Har1v3 -kappa3*Rpar1v3 - omega_pa*Rpar1v3 - delta4_ar*Rpar1v3 +delta3_ar*Rpar1v2+delta3_ar*Rnar1v2*(rho_v3)+delta3_ar*Spar1v2+delta3_ar*Snar1v2*(rho_v3)
    dRpau1v3  =  pi*(1-phi_av3)*gamma_I*Iau1v3 + pi*gamma_A*Aau1v3 + pi*(1-mu_a)*gamma_H*Hau1v3 -kappa3*Rpau1v3 - omega_pa*Rpau1v3 - delta4_au*Rpau1v3 +delta3_au*Rpau1v2+delta3_au*Rnau1v2*(rho_v3)+delta3_au*Spau1v2+delta3_au*Snau1v2*(rho_v3)
    dRper1v3  =  pi*(1-phi_ev3)*gamma_I*Ier1v3 + pi*gamma_A*Aer1v3 + pi*(1-mu_e)*gamma_H*Her1v3 -kappa3*Rper1v3 - omega_pe*Rper1v3 - delta4_er*Rper1v3 +delta3_er*Rper1v2+delta3_er*Rner1v2*(rho_v3)+delta3_er*Sper1v2+delta3_er*Sner1v2*(rho_v3)
    dRpeu1v3  =  pi*(1-phi_ev3)*gamma_I*Ieu1v3 + pi*gamma_A*Aeu1v3 + pi*(1-mu_e)*gamma_H*Heu1v3 -kappa3*Rpeu1v3 - omega_pe*Rpeu1v3 - delta4_eu*Rpeu1v3 +delta3_eu*Rpeu1v2+delta3_eu*Rneu1v2*(rho_v3)+delta3_eu*Speu1v2+delta3_eu*Sneu1v2*(rho_v3)
    
    
    #Recovered and not seropositive
    dRncr1v3  =  (1-pi)*(1-phi_cv3)*gamma_I*Icr1v3 + (1-pi)*gamma_A*Acr1v3 + (1-pi)*(1-mu_c)*gamma_H*Hcr1v3 + kappa3*Rpcr1v3 - omega_nc*Rncr1v3 - delta4_cr*Rncr1v3 +delta3_cr*Rncr1v2*(1-rho_v3)+delta3_cr*Sncr1v2*(1-rho_v3)  
    dRncu1v3  =  (1-pi)*(1-phi_cv3)*gamma_I*Icu1v3 + (1-pi)*gamma_A*Acu1v3 + (1-pi)*(1-mu_c)*gamma_H*Hcu1v3 + kappa3*Rpcu1v3 - omega_nc*Rncu1v3 - delta4_cu*Rncu1v3 +delta3_cu*Rncu1v2*(1-rho_v3)+delta3_cu*Sncu1v2*(1-rho_v3)
    dRnar1v3  =  (1-pi)*(1-phi_av3)*gamma_I*Iar1v3 + (1-pi)*gamma_A*Aar1v3 + (1-pi)*(1-mu_a)*gamma_H*Har1v3 + kappa3*Rpar1v3 - omega_na*Rnar1v3 - delta4_ar*Rnar1v3 +delta3_ar*Rnar1v2*(1-rho_v3)+delta3_ar*Snar1v2*(1-rho_v3)
    dRnau1v3  =  (1-pi)*(1-phi_av3)*gamma_I*Iau1v3 + (1-pi)*gamma_A*Aau1v3 + (1-pi)*(1-mu_a)*gamma_H*Hau1v3 + kappa3*Rpau1v3 - omega_na*Rnau1v3 - delta4_au*Rnau1v3 +delta3_au*Rnau1v2*(1-rho_v3)+delta3_au*Snau1v2*(1-rho_v3)
    dRner1v3  =  (1-pi)*(1-phi_ev3)*gamma_I*Ier1v3 + (1-pi)*gamma_A*Aer1v3 + (1-pi)*(1-mu_e)*gamma_H*Her1v3 + kappa3*Rper1v3 - omega_ne*Rner1v3 - delta4_er*Rner1v3 +delta3_er*Rner1v2*(1-rho_v3)+delta3_er*Sner1v2*(1-rho_v3)
    dRneu1v3  =  (1-pi)*(1-phi_ev3)*gamma_I*Ieu1v3 + (1-pi)*gamma_A*Aeu1v3 + (1-pi)*(1-mu_e)*gamma_H*Heu1v3 + kappa3*Rpeu1v3 - omega_ne*Rneu1v3 - delta4_eu*Rneu1v3 +delta3_eu*Rneu1v2*(1-rho_v3)+delta3_eu*Sneu1v2*(1-rho_v3)
    
    #Deaths 
    dDcr1v3  =  mu_c*gamma_H*Hcr1v3
    dDcu1v3  =  mu_c*gamma_H*Hcu1v3
    dDar1v3  =  mu_a*gamma_H*Har1v3
    dDau1v3  =  mu_a*gamma_H*Hau1v3
    dDer1v3  =  mu_e*gamma_H*Her1v3
    dDeu1v3  =  mu_e*gamma_H*Heu1v3
    
    ################################
    ##Vaccinated three dose and exposed once####
    ################################
    #Susceptible
    dSpcr1v3  = omega_pc*Rpcr1v3 -kappa3*Spcr1v3 - Spcr1v3 * sd * foi_cr_1v3 - delta4_cr * Spcr1v3 
    dSpcu1v3  = omega_pc*Rpcu1v3 -kappa3*Spcu1v3 - Spcu1v3 * sd * foi_cu_1v3 - delta4_cu * Spcu1v3 
    dSpar1v3  = omega_pc*Rpar1v3 -kappa3*Spar1v3 - Spar1v3 * sd * foi_ar_1v3 - delta4_ar * Spar1v3 
    dSpau1v3  = omega_pc*Rpau1v3 -kappa3*Spau1v3 - Spau1v3 * sd * foi_au_1v3 - delta4_au * Spau1v3 
    dSper1v3  = omega_pc*Rper1v3 -kappa3*Sper1v3 - Sper1v3 * sd * foi_er_1v3 - delta4_er * Sper1v3 
    dSpeu1v3  = omega_pc*Rpeu1v3 -kappa3*Speu1v3 - Speu1v3 * sd * foi_eu_1v3 - delta4_eu * Speu1v3 
    
    dSncr1v3  = omega_pc*Rncr1v3 + kappa3*Spcr1v3 -Sncr1v3 * sd * foi_cr_1v3 - delta4_cr * Sncr1v3 
    dSncu1v3  = omega_pc*Rncu1v3 + kappa3*Spcu1v3 -Sncu1v3 * sd * foi_cu_1v3 - delta4_cu * Sncu1v3 
    dSnar1v3  = omega_pc*Rnar1v3 + kappa3*Spar1v3 -Snar1v3 * sd * foi_ar_1v3 - delta4_ar * Snar1v3 
    dSnau1v3  = omega_pc*Rnau1v3 + kappa3*Spau1v3 -Snau1v3 * sd * foi_au_1v3 - delta4_au * Snau1v3 
    dSner1v3  = omega_pc*Rner1v3 + kappa3*Sper1v3 -Sner1v3 * sd * foi_er_1v3 - delta4_er * Sner1v3 
    dSneu1v3  = omega_pc*Rneu1v3 + kappa3*Speu1v3 -Sneu1v3 * sd * foi_eu_1v3 - delta4_eu * Sneu1v3 
    
    #Exposed
    dEcr2v3  =  Spcr1v3 * sd * foi_cr_1v3 + Sncr1v3 * sd * foi_cr_1v3 -  sigma * Ecr2v3  - delta4_cr*Ecr2v3 +delta3_cr*Ecr2v2
    dEcu2v3  =  Spcu1v3 * sd * foi_cu_1v3 + Sncu1v3 * sd * foi_cu_1v3 -  sigma * Ecu2v3  - delta4_cu*Ecu2v3 +delta3_cu*Ecu2v2
    dEar2v3  =  Spar1v3 * sd * foi_ar_1v3 + Snar1v3 * sd * foi_ar_1v3 -  sigma * Ear2v3  - delta4_ar*Ear2v3 +delta3_ar*Ear2v2
    dEau2v3  =  Spau1v3 * sd * foi_au_1v3 + Snau1v3 * sd * foi_au_1v3 -  sigma * Eau2v3  - delta4_au*Eau2v3 +delta3_au*Eau2v2
    dEer2v3  =  Sper1v3 * sd * foi_er_1v3 + Sner1v3 * sd * foi_er_1v3 -  sigma * Eer2v3  - delta4_er*Eer2v3 +delta3_er*Eer2v2
    dEeu2v3  =  Speu1v3 * sd * foi_eu_1v3 + Sneu1v3 * sd * foi_eu_1v3 -  sigma * Eeu2v3  - delta4_eu*Eeu2v3 +delta3_eu*Eeu2v2
    
    # Asymptomatic
    dAcr2v3  =  (1-nu_c)*sigma*Ecr2v3 - gamma_A * Acr2v3 - delta4_cr*Acr2v3 + delta3_cr*Acr2v2
    dAcu2v3  =  (1-nu_c)*sigma*Ecu2v3 - gamma_A * Acu2v3 - delta4_cu*Acu2v3 + delta3_cu*Acu2v2
    dAar2v3  =  (1-nu_a)*sigma*Ear2v3 - gamma_A * Aar2v3 - delta4_ar*Aar2v3 + delta3_ar*Aar2v2
    dAau2v3  =  (1-nu_a)*sigma*Eau2v3 - gamma_A * Aau2v3 - delta4_au*Aau2v3 + delta3_au*Aau2v2
    dAer2v3  =  (1-nu_e)*sigma*Eer2v3 - gamma_A * Aer2v3 - delta4_er*Aer2v3 + delta3_er*Aer2v2
    dAeu2v3  =  (1-nu_e)*sigma*Eeu2v3 - gamma_A * Aeu2v3 - delta4_eu*Aeu2v3 + delta3_eu*Aeu2v2
    
    #Symptomatic 
    dIcr2v3  =  nu_c*sigma*Ecr2v3 - gamma_I * Icr2v3 
    dIcu2v3  =  nu_c*sigma*Ecu2v3 - gamma_I * Icu2v3 
    dIar2v3  =  nu_a*sigma*Ear2v3 - gamma_I * Iar2v3 
    dIau2v3  =  nu_a*sigma*Eau2v3 - gamma_I * Iau2v3 
    dIer2v3  =  nu_e*sigma*Eer2v3 - gamma_I * Ier2v3
    dIeu2v3  =  nu_e*sigma*Eeu2v3 - gamma_I * Ieu2v3
    
    # Hospitalized
    dHcr2v3  =  phi_cv3*gamma_I*Icr2v3 - gamma_H * Hcr2v3
    dHcu2v3  =  phi_cv3*gamma_I*Icu2v3 - gamma_H * Hcu2v3
    dHar2v3  =  phi_av3*gamma_I*Iar2v3 - gamma_H * Har2v3
    dHau2v3  =  phi_av3*gamma_I*Iau2v3 - gamma_H * Hau2v3
    dHer2v3  =  phi_ev3*gamma_I*Ier2v3 - gamma_H * Her2v3
    dHeu2v3  =  phi_ev3*gamma_I*Ieu2v3 - gamma_H * Heu2v3
    
    # Recovered and seroconverted
    dRpcr2v3  =  pi*(1-phi_cv3)*gamma_I*Icr2v3 + pi*gamma_A*Acr2v3 + pi*(1-mu_c)*gamma_H*Hcr2v3 -kappa3*Rpcr2v3 - omega_pc*Rpcr2v3 - delta4_cr*Rpcr2v3 +delta3_cr*Rpcr2v2+ delta3_cr*Rncr2v2*(rho_v3)+ delta3_cr*Spcr2v2+ delta3_cr*Sncr2v2*(rho_v3)
    dRpcu2v3  =  pi*(1-phi_cv3)*gamma_I*Icu2v3 + pi*gamma_A*Acu2v3 + pi*(1-mu_c)*gamma_H*Hcu2v3 -kappa3*Rpcu2v3 - omega_pc*Rpcu2v3 - delta4_cu*Rpcu2v3 +delta3_cu*Rpcu2v2+ delta3_cu*Rncu2v2*(rho_v3)+ delta3_cu*Spcu2v2+ delta3_cu*Sncu2v2*(rho_v3)
    dRpar2v3  =  pi*(1-phi_av3)*gamma_I*Iar2v3 + pi*gamma_A*Aar2v3 + pi*(1-mu_a)*gamma_H*Har2v3 -kappa3*Rpar2v3 - omega_pa*Rpar2v3 - delta4_ar*Rpar2v3 +delta3_ar*Rpar2v2+ delta3_ar*Rnar2v2*(rho_v3)+ delta3_ar*Spar2v2+ delta3_ar*Snar2v2*(rho_v3)
    dRpau2v3  =  pi*(1-phi_av3)*gamma_I*Iau2v3 + pi*gamma_A*Aau2v3 + pi*(1-mu_a)*gamma_H*Hau2v3 -kappa3*Rpau2v3 - omega_pa*Rpau2v3 - delta4_au*Rpau2v3 +delta3_au*Rpau2v2+ delta3_au*Rnau2v2*(rho_v3)+ delta3_au*Spau2v2+ delta3_au*Snau2v2*(rho_v3)
    dRper2v3  =  pi*(1-phi_ev3)*gamma_I*Ier2v3 + pi*gamma_A*Aer2v3 + pi*(1-mu_e)*gamma_H*Her2v3 -kappa3*Rper2v3 - omega_pe*Rper2v3 - delta4_er*Rper2v3 +delta3_er*Rper2v2+ delta3_er*Rner2v2*(rho_v3)+ delta3_er*Sper2v2+ delta3_er*Sner2v2*(rho_v3)
    dRpeu2v3  =  pi*(1-phi_ev3)*gamma_I*Ieu2v3 + pi*gamma_A*Aeu2v3 + pi*(1-mu_e)*gamma_H*Heu2v3 -kappa3*Rpeu2v3 - omega_pe*Rpeu2v3 - delta4_eu*Rpeu2v3 +delta3_eu*Rpeu2v2+ delta3_eu*Rneu2v2*(rho_v3)+ delta3_eu*Speu2v2+ delta3_eu*Sneu2v2*(rho_v3) 
    
    #Recovered and not seropositive
    dRncr2v3  =  (1-pi)*(1-phi_cv3)*gamma_I*Icr2v3 + (1-pi)*gamma_A*Acr2v3 + (1-pi)*(1-mu_c)*gamma_H*Hcr2v3 + kappa3*Rpcr2v3 - omega_nc*Rncr2v3 - delta4_cr*Rncr2v3 + delta3_cr*Rncr2v2*(1-rho_v3)+ delta3_cr*Sncr2v2*(1-rho_v3)
    dRncu2v3  =  (1-pi)*(1-phi_cv3)*gamma_I*Icu2v3 + (1-pi)*gamma_A*Acu2v3 + (1-pi)*(1-mu_c)*gamma_H*Hcu2v3 + kappa3*Rpcu2v3 - omega_nc*Rncu2v3 - delta4_cu*Rncu2v3 + delta3_cu*Rncu2v2*(1-rho_v3)+ delta3_cu*Sncu2v2*(1-rho_v3)
    dRnar2v3  =  (1-pi)*(1-phi_av3)*gamma_I*Iar2v3 + (1-pi)*gamma_A*Aar2v3 + (1-pi)*(1-mu_a)*gamma_H*Har2v3 + kappa3*Rpar2v3 - omega_na*Rnar2v3 - delta4_ar*Rnar2v3 + delta3_ar*Rnar2v2*(1-rho_v3)+ delta3_ar*Snar2v2*(1-rho_v3)
    dRnau2v3  =  (1-pi)*(1-phi_av3)*gamma_I*Iau2v3 + (1-pi)*gamma_A*Aau2v3 + (1-pi)*(1-mu_a)*gamma_H*Hau2v3 + kappa3*Rpau2v3 - omega_na*Rnau2v3 - delta4_au*Rnau2v3 + delta3_au*Rnau2v2*(1-rho_v3)+ delta3_au*Snau2v2*(1-rho_v3)
    dRner2v3  =  (1-pi)*(1-phi_ev3)*gamma_I*Ier2v3 + (1-pi)*gamma_A*Aer2v3 + (1-pi)*(1-mu_e)*gamma_H*Her2v3 + kappa3*Rper2v3 - omega_ne*Rner2v3 - delta4_er*Rner2v3 + delta3_er*Rner2v2*(1-rho_v3)+ delta3_er*Sner2v2*(1-rho_v3)
    dRneu2v3  =  (1-pi)*(1-phi_ev3)*gamma_I*Ieu2v3 + (1-pi)*gamma_A*Aeu2v3 + (1-pi)*(1-mu_e)*gamma_H*Heu2v3 + kappa3*Rpeu2v3 - omega_ne*Rneu2v3 - delta4_eu*Rneu2v3 + delta3_eu*Rneu2v2*(1-rho_v3)+ delta3_eu*Sneu2v2*(1-rho_v3)
    
    #Deaths 
    dDcr2v3  =  mu_c*gamma_H*Hcr2v3
    dDcu2v3  =  mu_c*gamma_H*Hcu2v3
    dDar2v3  =  mu_a*gamma_H*Har2v3
    dDau2v3  =  mu_a*gamma_H*Hau2v3
    dDer2v3  =  mu_e*gamma_H*Her2v3
    dDeu2v3  =  mu_e*gamma_H*Heu2v3
    
    ################################
    ##Vaccinated three dose and exposed twice####
    ################################
    #Susceptible
    dSpcr2v3  = omega_pc*Rpcr2v3 -kappa3*Spcr2v3 - Spcr2v3 * sd * foi_cr_2v3 - delta4_cr * Spcr2v3  + omega2_pc*Rpcr3v3
    dSpcu2v3  = omega_pc*Rpcu2v3 -kappa3*Spcu2v3 - Spcu2v3 * sd * foi_cu_2v3 - delta4_cu * Spcu2v3  + omega2_pc*Rpcu3v3
    dSpar2v3  = omega_pc*Rpar2v3 -kappa3*Spar2v3 - Spar2v3 * sd * foi_ar_2v3 - delta4_ar * Spar2v3  + omega2_pa*Rpar3v3
    dSpau2v3  = omega_pc*Rpau2v3 -kappa3*Spau2v3 - Spau2v3 * sd * foi_au_2v3 - delta4_au * Spau2v3  + omega2_pa*Rpau3v3
    dSper2v3  = omega_pc*Rper2v3 -kappa3*Sper2v3 - Sper2v3 * sd * foi_er_2v3 - delta4_er * Sper2v3  + omega2_pe*Rper3v3
    dSpeu2v3  = omega_pc*Rpeu2v3 -kappa3*Speu2v3 - Speu2v3 * sd * foi_eu_2v3 - delta4_eu * Speu2v3  + omega2_pe*Rpeu3v3
    
    dSncr2v3  = omega_pc*Rncr2v3 + kappa3*Spcr2v3 -Sncr2v3 * sd * foi_cr_2v3 - delta4_cr * Sncr2v3  + omega2_nc*Rncr3v3
    dSncu2v3  = omega_pc*Rncu2v3 + kappa3*Spcu2v3 -Sncu2v3 * sd * foi_cu_2v3 - delta4_cu * Sncu2v3  + omega2_nc*Rncu3v3
    dSnar2v3  = omega_pc*Rnar2v3 + kappa3*Spar2v3 -Snar2v3 * sd * foi_ar_2v3 - delta4_ar * Snar2v3  + omega2_na*Rnar3v3
    dSnau2v3  = omega_pc*Rnau2v3 + kappa3*Spau2v3 -Snau2v3 * sd * foi_au_2v3 - delta4_au * Snau2v3  + omega2_na*Rnau3v3
    dSner2v3  = omega_pc*Rner2v3 + kappa3*Sper2v3 -Sner2v3 * sd * foi_er_2v3 - delta4_er * Sner2v3  + omega2_ne*Rner3v3
    dSneu2v3  = omega_pc*Rneu2v3 + kappa3*Speu2v3 -Sneu2v3 * sd * foi_eu_2v3 - delta4_eu * Sneu2v3  + omega2_ne*Rneu3v3
    
    #Exposed
    dEcr3v3  =  Spcr2v3 * sd * foi_cr_2v3 + Sncr2v3 * sd * foi_cr_2v3 -  sigma * Ecr3v3  - delta4_cr*Ecr3v3 + delta3_cr*Ecr3v2
    dEcu3v3  =  Spcu2v3 * sd * foi_cu_2v3 + Sncu2v3 * sd * foi_cu_2v3 -  sigma * Ecu3v3  - delta4_cu*Ecu3v3 + delta3_cu*Ecu3v2
    dEar3v3  =  Spar2v3 * sd * foi_ar_2v3 + Snar2v3 * sd * foi_ar_2v3 -  sigma * Ear3v3  - delta4_ar*Ear3v3 + delta3_ar*Ear3v2
    dEau3v3  =  Spau2v3 * sd * foi_au_2v3 + Snau2v3 * sd * foi_au_2v3 -  sigma * Eau3v3  - delta4_au*Eau3v3 + delta3_au*Eau3v2
    dEer3v3  =  Sper2v3 * sd * foi_er_2v3 + Sner2v3 * sd * foi_er_2v3 -  sigma * Eer3v3  - delta4_er*Eer3v3 + delta3_er*Eer3v2
    dEeu3v3  =  Speu2v3 * sd * foi_eu_2v3 + Sneu2v3 * sd * foi_eu_2v3 -  sigma * Eeu3v3  - delta4_eu*Eeu3v3 + delta3_eu*Eeu3v2
    
    # Asymptomatic
    dAcr3v3  =  (1-nu_c)*sigma*Ecr3v3 - gamma_A * Acr3v3 - delta4_cr*Acr3v3+ delta3_cr*Acr3v2
    dAcu3v3  =  (1-nu_c)*sigma*Ecu3v3 - gamma_A * Acu3v3 - delta4_cu*Acu3v3+ delta3_cu*Acu3v2
    dAar3v3  =  (1-nu_a)*sigma*Ear3v3 - gamma_A * Aar3v3 - delta4_ar*Aar3v3+ delta3_ar*Aar3v2
    dAau3v3  =  (1-nu_a)*sigma*Eau3v3 - gamma_A * Aau3v3 - delta4_au*Aau3v3+ delta3_au*Aau3v2
    dAer3v3  =  (1-nu_e)*sigma*Eer3v3 - gamma_A * Aer3v3 - delta4_er*Aer3v3+ delta3_er*Aer3v2
    dAeu3v3  =  (1-nu_e)*sigma*Eeu3v3 - gamma_A * Aeu3v3 - delta4_eu*Aeu3v3+ delta3_eu*Aeu3v2
    
    #Symptomatic 
    dIcr3v3  =  nu_c*sigma*Ecr3v3 - gamma_I * Icr3v3 
    dIcu3v3  =  nu_c*sigma*Ecu3v3 - gamma_I * Icu3v3 
    dIar3v3  =  nu_a*sigma*Ear3v3 - gamma_I * Iar3v3 
    dIau3v3  =  nu_a*sigma*Eau3v3 - gamma_I * Iau3v3 
    dIer3v3  =  nu_e*sigma*Eer3v3 - gamma_I * Ier3v3
    dIeu3v3  =  nu_e*sigma*Eeu3v3 - gamma_I * Ieu3v3
    
    # Hospitalized
    dHcr3v3  =  phi_cv3*gamma_I*Icr3v3 - gamma_H * Hcr3v3
    dHcu3v3  =  phi_cv3*gamma_I*Icu3v3 - gamma_H * Hcu3v3
    dHar3v3  =  phi_av3*gamma_I*Iar3v3 - gamma_H * Har3v3
    dHau3v3  =  phi_av3*gamma_I*Iau3v3 - gamma_H * Hau3v3
    dHer3v3  =  phi_ev3*gamma_I*Ier3v3 - gamma_H * Her3v3
    dHeu3v3  =  phi_ev3*gamma_I*Ieu3v3 - gamma_H * Heu3v3
    
    # Recovered and seroconverted
    dRpcr3v3  =  pi*(1-phi_cv3)*gamma_I*Icr3v3 + pi*gamma_A*Acr3v3 + pi*(1-mu_c)*gamma_H*Hcr3v3 -kappa3*Rpcr3v3 - omega2_pc*Rpcr3v3 - delta4_cr*Rpcr3v3 + delta3_cr*Rpcr3v2+ delta3_cr*Rncr3v2*(rho_v3)
    dRpcu3v3  =  pi*(1-phi_cv3)*gamma_I*Icu3v3 + pi*gamma_A*Acu3v3 + pi*(1-mu_c)*gamma_H*Hcu3v3 -kappa3*Rpcu3v3 - omega2_pc*Rpcu3v3 - delta4_cu*Rpcu3v3 + delta3_cu*Rpcu3v2+ delta3_cu*Rncu3v2*(rho_v3)
    dRpar3v3  =  pi*(1-phi_av3)*gamma_I*Iar3v3 + pi*gamma_A*Aar3v3 + pi*(1-mu_a)*gamma_H*Har3v3 -kappa3*Rpar3v3 - omega2_pa*Rpar3v3 - delta4_ar*Rpar3v3 + delta3_ar*Rpar3v2 + delta3_ar*Rnar3v2*(rho_v3)
    dRpau3v3  =  pi*(1-phi_av3)*gamma_I*Iau3v3 + pi*gamma_A*Aau3v3 + pi*(1-mu_a)*gamma_H*Hau3v3 -kappa3*Rpau3v3 - omega2_pa*Rpau3v3 - delta4_au*Rpau3v3 + delta3_au*Rpau3v2+ delta3_au*Rnau3v2*(rho_v3)
    dRper3v3  =  pi*(1-phi_ev3)*gamma_I*Ier3v3 + pi*gamma_A*Aer3v3 + pi*(1-mu_e)*gamma_H*Her3v3 -kappa3*Rper3v3 - omega2_pe*Rper3v3 - delta4_er*Rper3v3 + delta3_er*Rper3v2+ delta3_er*Rner3v2*(rho_v3)
    dRpeu3v3  =  pi*(1-phi_ev3)*gamma_I*Ieu3v3 + pi*gamma_A*Aeu3v3 + pi*(1-mu_e)*gamma_H*Heu3v3 -kappa3*Rpeu3v3 - omega2_pe*Rpeu3v3 - delta4_eu*Rpeu3v3 + delta3_eu*Rpeu3v2+ delta3_eu*Rneu3v2*(rho_v3)
    
    #Recovered and not seropositive
    dRncr3v3  =  (1-pi)*(1-phi_cv3)*gamma_I*Icr3v3 + (1-pi)*gamma_A*Acr3v3 + (1-pi)*(1-mu_c)*gamma_H*Hcr3v3 + kappa3*Rpcr3v3 - omega2_nc*Rncr3v3 - delta4_cr*Rncr3v3 + delta3_cr*Rncr3v2*(1-rho_v3)
    dRncu3v3  =  (1-pi)*(1-phi_cv3)*gamma_I*Icu3v3 + (1-pi)*gamma_A*Acu3v3 + (1-pi)*(1-mu_c)*gamma_H*Hcu3v3 + kappa3*Rpcu3v3 - omega2_nc*Rncu3v3 - delta4_cu*Rncu3v3 + delta3_cu*Rncu3v2*(1-rho_v3)
    dRnar3v3  =  (1-pi)*(1-phi_av3)*gamma_I*Iar3v3 + (1-pi)*gamma_A*Aar3v3 + (1-pi)*(1-mu_a)*gamma_H*Har3v3 + kappa3*Rpar3v3 - omega2_na*Rnar3v3 - delta4_ar*Rnar3v3 + delta3_ar*Rnar3v2*(1-rho_v3)
    dRnau3v3  =  (1-pi)*(1-phi_av3)*gamma_I*Iau3v3 + (1-pi)*gamma_A*Aau3v3 + (1-pi)*(1-mu_a)*gamma_H*Hau3v3 + kappa3*Rpau3v3 - omega2_na*Rnau3v3 - delta4_au*Rnau3v3 + delta3_au*Rnau3v2*(1-rho_v3)
    dRner3v3  =  (1-pi)*(1-phi_ev3)*gamma_I*Ier3v3 + (1-pi)*gamma_A*Aer3v3 + (1-pi)*(1-mu_e)*gamma_H*Her3v3 + kappa3*Rper3v3 - omega2_ne*Rner3v3 - delta4_er*Rner3v3 + delta3_er*Rner3v2*(1-rho_v3)
    dRneu3v3  =  (1-pi)*(1-phi_ev3)*gamma_I*Ieu3v3 + (1-pi)*gamma_A*Aeu3v3 + (1-pi)*(1-mu_e)*gamma_H*Heu3v3 + kappa3*Rpeu3v3 - omega2_ne*Rneu3v3 - delta4_eu*Rneu3v3 + delta3_eu*Rneu3v2*(1-rho_v3)
    
    #Deaths 
    dDcr3v3  =  mu_c*gamma_H*Hcr3v3
    dDcu3v3  =  mu_c*gamma_H*Hcu3v3
    dDar3v3  =  mu_a*gamma_H*Har3v3
    dDau3v3  =  mu_a*gamma_H*Hau3v3
    dDer3v3  =  mu_e*gamma_H*Her3v3
    dDeu3v3  =  mu_e*gamma_H*Heu3v3
    
    ##Cumulative infections
    ## No vaccine
    dEcum1v0_cr = Scr0v0 * sd * foi_cr_0v0
    dEcum1v0_cu = Scu0v0 * sd * foi_cu_0v0
    dEcum1v0_ar = Sar0v0 * sd * foi_ar_0v0
    dEcum1v0_au = Sau0v0 * sd * foi_au_0v0
    dEcum1v0_er = Ser0v0 * sd * foi_er_0v0
    dEcum1v0_eu = Seu0v0 * sd * foi_eu_0v0
    
    dEcum2v0_cr = Spcr1v0 * sd * foi_cr_1v0 + Sncr1v0 * sd * foi_cr_1v0
    dEcum2v0_cu = Spcu1v0 * sd * foi_cu_1v0 + Sncu1v0 * sd * foi_cu_1v0
    dEcum2v0_ar = Spar1v0 * sd * foi_ar_1v0 + Snar1v0 * sd * foi_ar_1v0
    dEcum2v0_au = Spau1v0 * sd * foi_au_1v0 + Snar1v0 * sd * foi_ar_1v0
    dEcum2v0_er = Sper1v0 * sd * foi_er_1v0 + Sner1v0 * sd * foi_er_1v0
    dEcum2v0_eu = Speu1v0 * sd * foi_eu_1v0 + Sneu1v0 * sd * foi_eu_1v0
    
    dEcum3v0_cr = Spcr2v0 * sd * foi_cr_2v0 + Sncr2v0 * sd * foi_cr_2v0
    dEcum3v0_cu = Spcu2v0 * sd * foi_cu_2v0 + Sncu2v0 * sd * foi_cu_2v0
    dEcum3v0_ar = Spar2v0 * sd * foi_ar_2v0 + Snar2v0 * sd * foi_ar_2v0
    dEcum3v0_au = Spau2v0 * sd * foi_au_2v0 + Snau2v0 * sd * foi_au_2v0
    dEcum3v0_er = Sper2v0 * sd * foi_er_2v0 + Sner2v0 * sd * foi_er_2v0
    dEcum3v0_eu = Speu2v0 * sd * foi_eu_2v0 + Sneu2v0 * sd * foi_eu_2v0
    
    ##One dose vaccine
    dEcum1v1_cr = Spcr0v1 * sd * foi_cr_0v1 +Sncr0v1 * sd * foi_cr_0v1 
    dEcum1v1_cu = Spcu0v1 * sd * foi_cu_0v1 +Sncu0v1 * sd * foi_cu_0v1 
    dEcum1v1_ar = Spar0v1 * sd * foi_ar_0v1 +Snar0v1 * sd * foi_ar_0v1 
    dEcum1v1_au = Spau0v1 * sd * foi_au_0v1 +Snau0v1 * sd * foi_au_0v1 
    dEcum1v1_er = Sper0v1 * sd * foi_er_0v1 +Sner0v1 * sd * foi_er_0v1 
    dEcum1v1_eu = Speu0v1 * sd * foi_eu_0v1 +Sneu0v1 * sd * foi_eu_0v1 
    
    dEcum2v1_cr = Spcr1v1 * sd * foi_cr_1v1 + Sncr1v1 * sd * foi_cr_1v1
    dEcum2v1_cu = Spcu1v1 * sd * foi_cu_1v1 + Sncu1v1 * sd * foi_cu_1v1
    dEcum2v1_ar = Spar1v1 * sd * foi_ar_1v1 + Snar1v1 * sd * foi_ar_1v1
    dEcum2v1_au = Spau1v1 * sd * foi_au_1v1 + Snar1v1 * sd * foi_ar_1v1
    dEcum2v1_er = Sper1v1 * sd * foi_er_1v1 + Sner1v1 * sd * foi_er_1v1
    dEcum2v1_eu = Speu1v1 * sd * foi_eu_1v1 + Sner1v1 * sd * foi_er_1v1
    
    dEcum3v1_cr = Spcr2v1 * sd * foi_cr_2v1 + Sncr2v1 * sd * foi_cr_2v1
    dEcum3v1_cu = Spcu2v1 * sd * foi_cu_2v1 + Sncu2v1 * sd * foi_cu_2v1
    dEcum3v1_ar = Spar2v1 * sd * foi_ar_2v1 + Snar2v1 * sd * foi_ar_2v1
    dEcum3v1_au = Spau2v1 * sd * foi_au_2v1 + Snau2v1 * sd * foi_au_2v1
    dEcum3v1_er = Sper2v1 * sd * foi_er_2v1 + Sner2v1 * sd * foi_er_2v1
    dEcum3v1_eu = Speu2v1 * sd * foi_eu_2v1 + Sneu2v1 * sd * foi_eu_2v1
    
    ##Two dose vaccine
    dEcum1v2_cr = Spcr0v2 * sd * foi_cr_0v2 + Sncr0v2 * sd * foi_cr_0v2 
    dEcum1v2_cu = Spcu0v2 * sd * foi_cu_0v2 + Sncu0v2 * sd * foi_cu_0v2
    dEcum1v2_ar = Spar0v2 * sd * foi_ar_0v2 + Snar0v2 * sd * foi_ar_0v2
    dEcum1v2_au = Spau0v2 * sd * foi_au_0v2 + Snau0v2 * sd * foi_au_0v2
    dEcum1v2_er = Sper0v2 * sd * foi_er_0v2 + Sner0v2 * sd * foi_er_0v2
    dEcum1v2_eu = Speu0v2 * sd * foi_eu_0v2 + Sneu0v2 * sd * foi_eu_0v2
    
    dEcum2v2_cr = Spcr1v2 * sd * foi_cr_1v2 + Sncr1v2 * sd * foi_cr_1v2
    dEcum2v2_cu = Spcu1v2 * sd * foi_cu_1v2 + Sncr1v2 * sd * foi_cr_1v2
    dEcum2v2_ar = Spar1v2 * sd * foi_ar_1v2 + Sncr1v2 * sd * foi_cr_1v2
    dEcum2v2_au = Spau1v2 * sd * foi_au_1v2 + Sncr1v2 * sd * foi_cr_1v2
    dEcum2v2_er = Sper1v2 * sd * foi_er_1v2 + Sncr1v2 * sd * foi_cr_1v2
    dEcum2v2_eu = Speu1v2 * sd * foi_eu_1v2 + Sncr1v2 * sd * foi_cr_1v2
    
    dEcum3v2_cr = Spcr2v2 * sd * foi_cr_2v2 + Sncr2v2 * sd * foi_cr_2v2
    dEcum3v2_cu = Spcu2v2 * sd * foi_cu_2v2 + Sncu2v2 * sd * foi_cu_2v2
    dEcum3v2_ar = Spar2v2 * sd * foi_ar_2v2 + Snar2v2 * sd * foi_ar_2v2
    dEcum3v2_au = Spau2v2 * sd * foi_au_2v2 + Snau2v2 * sd * foi_au_2v2
    dEcum3v2_er = Sper2v2 * sd * foi_er_2v2 + Sner2v2 * sd * foi_er_2v2
    dEcum3v2_eu = Speu2v2 * sd * foi_eu_2v2 + Sneu2v2 * sd * foi_eu_2v2
    
    ##Three dose vaccine
    dEcum1v3_cr = Spcr0v3 * sd * foi_cr_0v3 + Sncr0v3 * sd * foi_cr_0v3
    dEcum1v3_cu = Spcu0v3 * sd * foi_cu_0v3 + Sncu0v3 * sd * foi_cu_0v3
    dEcum1v3_ar = Spar0v3 * sd * foi_ar_0v3 + Snar0v3 * sd * foi_ar_0v3
    dEcum1v3_au = Spau0v3 * sd * foi_au_0v3 + Snau0v3 * sd * foi_au_0v3
    dEcum1v3_er = Sper0v3 * sd * foi_er_0v3 + Sner0v3 * sd * foi_er_0v3
    dEcum1v3_eu = Speu0v3 * sd * foi_eu_0v3 + Sneu0v3 * sd * foi_eu_0v3
    
    dEcum2v3_cr = Spcr1v3 * sd * foi_cr_1v3 + Sncr1v3 * sd * foi_cr_1v3
    dEcum2v3_cu = Spcu1v3 * sd * foi_cu_1v3 + Sncu1v3 * sd * foi_cu_1v3
    dEcum2v3_ar = Spar1v3 * sd * foi_ar_1v3 + Snar1v3 * sd * foi_ar_1v3
    dEcum2v3_au = Spau1v3 * sd * foi_au_1v3 + Snau1v3 * sd * foi_au_1v3
    dEcum2v3_er = Sper1v3 * sd * foi_er_1v3 + Sner1v3 * sd * foi_er_1v3
    dEcum2v3_eu = Speu1v3 * sd * foi_eu_1v3 + Sneu1v3 * sd * foi_eu_1v3
    
    dEcum3v3_cr = Spcr2v3 * sd * foi_cr_2v3 + Sncr2v3 * sd * foi_cr_2v3
    dEcum3v3_cu = Spcu2v3 * sd * foi_cu_2v3 + Sncu2v3 * sd * foi_cu_2v3
    dEcum3v3_ar = Spar2v3 * sd * foi_ar_2v3 + Snar2v3 * sd * foi_ar_2v3
    dEcum3v3_au = Spau2v3 * sd * foi_au_2v3 + Snau2v3 * sd * foi_au_2v3
    dEcum3v3_er = Sper2v3 * sd * foi_er_2v3 + Sner2v3 * sd * foi_er_2v3
    dEcum3v3_eu = Speu2v3 * sd * foi_eu_2v3 + Sneu2v3 * sd * foi_eu_2v3
    
    ##Cumulative symptomatic cases
    dIcum1v0_cr = nu_c*sigma*Ecr1v0
    dIcum1v0_cu = nu_c*sigma*Ecu1v0
    dIcum1v0_ar = nu_c*sigma*Ear1v0
    dIcum1v0_au = nu_c*sigma*Eau1v0
    dIcum1v0_er = nu_c*sigma*Eer1v0
    dIcum1v0_eu = nu_c*sigma*Eeu1v0
    
    dIcum2v0_cr = nu_c*sigma*Ecr2v0
    dIcum2v0_cu = nu_c*sigma*Ecu2v0
    dIcum2v0_ar = nu_c*sigma*Ear2v0
    dIcum2v0_au = nu_c*sigma*Eau2v0
    dIcum2v0_er = nu_c*sigma*Eer2v0
    dIcum2v0_eu = nu_c*sigma*Eeu2v0
    
    dIcum3v0_cr = nu_c*sigma*Ecr3v0
    dIcum3v0_cu = nu_c*sigma*Ecu3v0
    dIcum3v0_ar = nu_c*sigma*Ear3v0
    dIcum3v0_au = nu_c*sigma*Eau3v0
    dIcum3v0_er = nu_c*sigma*Eer3v0
    dIcum3v0_eu = nu_c*sigma*Eeu3v0
    
    dIcum1v1_cr = nu_c*sigma*Ecr1v1
    dIcum1v1_cu = nu_c*sigma*Ecu1v1
    dIcum1v1_ar = nu_c*sigma*Ear1v1
    dIcum1v1_au = nu_c*sigma*Eau1v1
    dIcum1v1_er = nu_c*sigma*Eer1v1
    dIcum1v1_eu = nu_c*sigma*Eeu1v1
    
    dIcum2v1_cr = nu_c*sigma*Ecr2v1
    dIcum2v1_cu = nu_c*sigma*Ecu2v1
    dIcum2v1_ar = nu_c*sigma*Ear2v1
    dIcum2v1_au = nu_c*sigma*Eau2v1
    dIcum2v1_er = nu_c*sigma*Eer2v1
    dIcum2v1_eu = nu_c*sigma*Eeu2v1
    
    dIcum3v1_cr = nu_c*sigma*Ecr3v1
    dIcum3v1_cu = nu_c*sigma*Ecu3v1
    dIcum3v1_ar = nu_c*sigma*Ear3v1
    dIcum3v1_au = nu_c*sigma*Eau3v1
    dIcum3v1_er = nu_c*sigma*Eer3v1
    dIcum3v1_eu = nu_c*sigma*Eeu3v1
    
    dIcum1v2_cr = nu_c*sigma*Ecr1v2
    dIcum1v2_cu = nu_c*sigma*Ecu1v2
    dIcum1v2_ar = nu_c*sigma*Ear1v2
    dIcum1v2_au = nu_c*sigma*Eau1v2
    dIcum1v2_er = nu_c*sigma*Eer1v2
    dIcum1v2_eu = nu_c*sigma*Eeu1v2
    
    dIcum2v2_cr = nu_c*sigma*Ecr2v2
    dIcum2v2_cu = nu_c*sigma*Ecu2v2
    dIcum2v2_ar = nu_c*sigma*Ear2v2
    dIcum2v2_au = nu_c*sigma*Eau2v2
    dIcum2v2_er = nu_c*sigma*Eer2v2
    dIcum2v2_eu = nu_c*sigma*Eeu2v2
    
    dIcum3v2_cr = nu_c*sigma*Ecr3v2
    dIcum3v2_cu = nu_c*sigma*Ecu3v2
    dIcum3v2_ar = nu_c*sigma*Ear3v2
    dIcum3v2_au = nu_c*sigma*Eau3v2
    dIcum3v2_er = nu_c*sigma*Eer3v2
    dIcum3v2_eu = nu_c*sigma*Eeu3v2
    
    dIcum1v3_cr = nu_c*sigma*Ecr1v3
    dIcum1v3_cu = nu_c*sigma*Ecu1v3
    dIcum1v3_ar = nu_c*sigma*Ear1v3
    dIcum1v3_au = nu_c*sigma*Eau1v3
    dIcum1v3_er = nu_c*sigma*Eer1v3
    dIcum1v3_eu = nu_c*sigma*Eeu1v3
    
    dIcum2v3_cr = nu_c*sigma*Ecr2v3
    dIcum2v3_cu = nu_c*sigma*Ecu2v3
    dIcum2v3_ar = nu_c*sigma*Ear2v3
    dIcum2v3_au = nu_c*sigma*Eau2v3
    dIcum2v3_er = nu_c*sigma*Eer2v3
    dIcum2v3_eu = nu_c*sigma*Eeu2v3
    
    dIcum3v3_cr = nu_c*sigma*Ecr3v3
    dIcum3v3_cu = nu_c*sigma*Ecu3v3
    dIcum3v3_ar = nu_c*sigma*Ear3v3
    dIcum3v3_au = nu_c*sigma*Eau3v3
    dIcum3v3_er = nu_c*sigma*Eer3v3
    dIcum3v3_eu = nu_c*sigma*Eeu3v3
    
    ### Gamma waning from first infection
    dW1Rpcr1v0 = 4*kappa1*Rpcr1v0 - 4*kappa1*W1Rpcr1v0 - omega_pc*W1Rpcr1v0 - delta1_cr*W1Rpcr1v0
    dW2Rpcr1v0 = 4*kappa1*W1Rpcr1v0 - 4*kappa1*W2Rpcr1v0- omega_pc*W2Rpcr1v0- delta1_cr*W2Rpcr1v0
    dW3Rpcr1v0 = 4*kappa1*W2Rpcr1v0 - 4*kappa1*W3Rpcr1v0- omega_pc*W3Rpcr1v0- delta1_cr*W3Rpcr1v0
    
    dW1Rpcu1v0 = 4*kappa1*Rpcu1v0 - 4*kappa1*W1Rpcu1v0- omega_pc*W1Rpcu1v0- delta1_cu*W1Rpcu1v0
    dW2Rpcu1v0 = 4*kappa1*W1Rpcu1v0 - 4*kappa1*W2Rpcu1v0- omega_pc*W2Rpcu1v0 - delta1_cu*W2Rpcu1v0
    dW3Rpcu1v0 = 4*kappa1*W2Rpcu1v0 - 4*kappa1*W3Rpcu1v0- omega_pc*W3Rpcu1v0 - delta1_cu*W2Rpcu1v0
    
    dW1Rpar1v0 = 4*kappa1*Rpar1v0 - 4*kappa1*W1Rpar1v0 - omega_pa*W1Rpar1v0 - delta1_ar*W1Rpar1v0
    dW2Rpar1v0 = 4*kappa1*W1Rpar1v0 - 4*kappa1*W2Rpar1v0- omega_pa*W2Rpar1v0- delta1_ar*W2Rpar1v0
    dW3Rpar1v0 = 4*kappa1*W2Rpar1v0 - 4*kappa1*W3Rpar1v0- omega_pa*W3Rpar1v0- delta1_ar*W3Rpar1v0
    
    dW1Rpau1v0 = 4*kappa1*Rpau1v0 - 4*kappa1*W1Rpau1v0- omega_pa*W1Rpau1v0- delta1_au*W1Rpau1v0
    dW2Rpau1v0 = 4*kappa1*W1Rpau1v0 - 4*kappa1*W2Rpau1v0- omega_pa*W2Rpau1v0 - delta1_au*W2Rpau1v0
    dW3Rpau1v0 = 4*kappa1*W2Rpau1v0 - 4*kappa1*W3Rpau1v0- omega_pa*W3Rpau1v0 - delta1_au*W3Rpau1v0
    
    dW1Rper1v0 = 4*kappa1*Rper1v0 - 4*kappa1*W1Rper1v0 - omega_pe*W1Rper1v0 - delta1_er*W1Rper1v0
    dW2Rper1v0 = 4*kappa1*W1Rper1v0 - 4*kappa1*W2Rper1v0- omega_pe*W2Rper1v0- delta1_er*W2Rper1v0
    dW3Rper1v0 = 4*kappa1*W2Rper1v0 - 4*kappa1*W3Rper1v0- omega_pe*W3Rper1v0- delta1_er*W3Rper1v0
    
    dW1Rpeu1v0 = 4*kappa1*Rpeu1v0 - 4*kappa1*W1Rpeu1v0- omega_pe*W1Rpeu1v0- delta1_eu*W1Rpeu1v0
    dW2Rpeu1v0 = 4*kappa1*W1Rpeu1v0 - 4*kappa1*W2Rpeu1v0- omega_pe*W2Rpeu1v0 - delta1_eu*W2Rpeu1v0
    dW3Rpeu1v0 = 4*kappa1*W2Rpeu1v0 - 4*kappa1*W3Rpeu1v0- omega_pe*W3Rpeu1v0 - delta1_eu*W3Rpeu1v0
    

    ##Classes of temporary immunity from those who were vaccinated
    ## First dose
    ## Seropositive
    
    dVpcr0v1 = rho_v1 * delta1_cr * Scr0v0 -4*kappa1 * Vpcr0v1- delta2_cr * Vpcr0v1 - omegav_pc *Vpcr0v1
    dVpcu0v1 = rho_v1 * delta1_cu * Scu0v0 -4*kappa1 * Vpcu0v1- delta2_cu * Vpcu0v1 - omegav_pc *Vpcu0v1
    dVpar0v1 = rho_v1 * delta1_ar * Sar0v0 -4*kappa1 * Vpar0v1- delta2_ar * Vpar0v1 - omegav_pa *Vpar0v1
    dVpau0v1 = rho_v1 * delta1_au * Sau0v0 -4*kappa1 * Vpau0v1- delta2_au * Vpau0v1 - omegav_pa *Vpau0v1
    dVper0v1 = rho_v1 * delta1_er * Ser0v0 -4*kappa1 * Vper0v1- delta2_er * Vper0v1 - omegav_pe *Vper0v1
    dVpeu0v1 = rho_v1 * delta1_eu * Seu0v0 -4*kappa1 * Vpeu0v1- delta2_eu * Vpeu0v1 - omegav_pe *Vpeu0v1
    
    ## First dose and seronegative
    dVncr0v1 = (1-rho_v1) * delta1_cr * Scr0v0 + 4*kappa1 * W3Vpcr0v1 - delta2_cr * Vncr0v1 - omegav_nc *Vncr0v1
    dVncu0v1 = (1-rho_v1) * delta1_cu * Scu0v0 + 4*kappa1 * W3Vpcu0v1 - delta2_cu * Vncu0v1 - omegav_nc *Vncu0v1
    dVnar0v1 = (1-rho_v1) * delta1_ar * Sar0v0 + 4*kappa1 * W3Vpar0v1 - delta2_ar * Vnar0v1 - omegav_na *Vnar0v1
    dVnau0v1 = (1-rho_v1) * delta1_au * Sau0v0 + 4*kappa1 * W3Vpau0v1 - delta2_au * Vnau0v1 - omegav_na *Vnau0v1
    dVner0v1 = (1-rho_v1) * delta1_er * Ser0v0 + 4*kappa1 * W3Vper0v1 - delta2_er * Vner0v1 - omegav_ne *Vner0v1
    dVneu0v1 = (1-rho_v1) * delta1_eu * Seu0v0 + 4*kappa1 * W3Vpeu0v1 - delta2_eu * Vneu0v1 - omegav_ne *Vneu0v1
    
    ##Second dose
    ##Seropositive
    dVpcr0v2 =  delta2_cr*Spcr0v1+(rho_v2)*delta2_cr*Sncr0v1 + delta2_cr*Vpcr0v1+delta2_cr*W1Vpcr0v1+delta2_cr*W2Vpcr0v1+delta2_cr*W3Vpcr0v1+(rho_v2)*delta2_cr*Vncr0v1 - kappa2 * Vpcr0v2- delta3_cr*Vpcr0v2 - omegav_pc *Vpcr0v2
    dVpcu0v2 =  delta2_cu*Spcu0v1+(rho_v2)*delta2_cu*Sncu0v1 + delta2_cu*Vpcu0v1+delta2_cu*W1Vpcu0v1+delta2_cu*W2Vpcu0v1+delta2_cu*W3Vpcu0v1+(rho_v2)*delta2_cu*Vncu0v1 - kappa2 * Vpcu0v2- delta3_cu * Vpcu0v2 - omegav_pc *Vpcu0v2
    dVpar0v2 =  delta2_ar*Spar0v1+(rho_v2)*delta2_ar*Snar0v1 + delta2_ar*Vpar0v1+delta2_ar*W1Vpar0v1+delta2_ar*W2Vpar0v1+delta2_ar*W3Vpar0v1+(rho_v2)*delta2_ar*Vnar0v1 - kappa2 * Vpar0v2- delta3_ar * Vpar0v2 - omegav_pa *Vpar0v2
    dVpau0v2 =  delta2_au*Spau0v1+(rho_v2)*delta2_au*Snau0v1 + delta2_au*Vpau0v1+delta2_au*W1Vpau0v1+delta2_au*W2Vpau0v1+delta2_au*W3Vpau0v1+(rho_v2)*delta2_au*Vnau0v1 - kappa2 * Vpau0v2- delta3_au * Vpau0v2 - omegav_pa *Vpau0v2
    dVper0v2 =  delta2_er*Sper0v1+(rho_v2)*delta2_er*Sner0v1 + delta2_er*Vper0v1+delta2_er*W1Vper0v1+delta2_er*W2Vper0v1+delta2_er*W3Vper0v1+(rho_v2)*delta2_er*Vner0v1 - kappa2 * Vper0v2- delta3_er * Vper0v2 - omegav_pe *Vper0v2
    dVpeu0v2 =  delta2_eu*Speu0v1+(rho_v2)*delta2_eu*Sneu0v1 + delta2_eu*Vpeu0v1+delta2_eu*W1Vpeu0v1+delta2_eu*W2Vpeu0v1+delta2_eu*W3Vpeu0v1+(rho_v2)*delta2_eu*Vneu0v1 - kappa2 * Vpeu0v2- delta3_eu * Vpeu0v2 - omegav_pe *Vpeu0v2
    
    ##Seronegative
    dVncr0v2 = (1-rho_v2) * delta2_cr * Sncr0v1 + (1-rho_v2) *delta2_cr * Vncr0v1+ kappa2 * Vpcr0v2 - delta3_cr * Vncr0v2 - omegav_nc *Vncr0v2
    dVncu0v2 = (1-rho_v2) * delta2_cu * Sncu0v1 + (1-rho_v2) *delta2_cu * Vncu0v1+ kappa2 * Vpcu0v2 - delta3_cu * Vncu0v2 - omegav_nc *Vncu0v2
    dVnar0v2 = (1-rho_v2) * delta2_ar * Snar0v1 + (1-rho_v2) *delta2_ar * Vnar0v1+ kappa2 * Vpar0v2 - delta3_ar * Vnar0v2 - omegav_na *Vnar0v2
    dVnau0v2 = (1-rho_v2) * delta2_au * Snau0v1 + (1-rho_v2) *delta2_au * Vnau0v1+ kappa2 * Vpau0v2 - delta3_au * Vnau0v2 - omegav_na *Vnau0v2
    dVner0v2 = (1-rho_v2) * delta2_er * Sner0v1 + (1-rho_v2) *delta2_er * Vner0v1+ kappa2 * Vper0v2 - delta3_er * Vner0v2 - omegav_ne *Vner0v2
    dVneu0v2 = (1-rho_v2) * delta2_eu * Sneu0v1 + (1-rho_v2) *delta2_eu * Vneu0v1+ kappa2 * Vpeu0v2 - delta3_eu * Vneu0v2 - omegav_ne *Vneu0v2
    
    ##Third dose
    ##Seropositive
    dVpcr0v3 = delta3_cr*Spcr0v2 +delta3_cr*Vpcr0v2 + rho_v3*delta3_cr*Sncr0v2 + rho_v3*delta3_cr*Vncr0v2-kappa3 * Vpcr0v3- delta4_cr * Vpcr0v3 - omegav_pc *Vpcr0v3
    dVpcu0v3 = delta3_cu*Spcu0v2 +delta3_cu*Vpcu0v2 + rho_v3*delta3_cu*Sncu0v2 + rho_v3*delta3_cu*Vncu0v2-kappa3 * Vpcu0v3- delta4_cu * Vpcu0v3 - omegav_pc *Vpcu0v3
    dVpar0v3 = delta3_ar*Spar0v2 +delta3_ar*Vpar0v2 + rho_v3*delta3_ar*Snar0v2 + rho_v3*delta3_ar*Vnar0v2-kappa3 * Vpar0v3- delta4_ar * Vpar0v3 - omegav_pa *Vpar0v3
    dVpau0v3 = delta3_au*Spau0v2 +delta3_au*Vpau0v2 + rho_v3*delta3_au*Snau0v2 + rho_v3*delta3_au*Vnau0v2-kappa3 * Vpau0v3- delta4_au * Vpau0v3 - omegav_pa *Vpau0v3
    dVper0v3 = delta3_er*Sper0v2 +delta3_er*Vper0v2 + rho_v3*delta3_er*Sner0v2 + rho_v3*delta3_er*Vner0v2-kappa3 * Vper0v3- delta4_er * Vper0v3 - omegav_pe *Vper0v3
    dVpeu0v3 = delta3_eu*Speu0v2 +delta3_eu*Vpeu0v2 + rho_v3*delta3_eu*Sneu0v2 + rho_v3*delta3_eu*Vneu0v2-kappa3 * Vpeu0v3- delta4_eu * Vpeu0v3 - omegav_pe *Vpeu0v3
    
    ##Seronegative
    dVncr0v3 = (1-rho_v3)*delta3_cr*Sncr0v2 + (1-rho_v3)*delta3_cr*Vncr0v2 + kappa3 * Vpcr0v3 - delta4_cr * Vncr0v3 - omegav_nc *Vncr0v3
    dVncu0v3 = (1-rho_v3)*delta3_cu*Sncu0v2 + (1-rho_v3)*delta3_cu*Vncu0v2 + kappa3 * Vpcu0v3 - delta4_cu * Vncu0v3 - omegav_nc *Vncu0v3
    dVnar0v3 = (1-rho_v3)*delta3_ar*Snar0v2 + (1-rho_v3)*delta3_ar*Vnar0v2 + kappa3 * Vpar0v3 - delta4_ar * Vnar0v3 - omegav_na *Vnar0v3
    dVnau0v3 = (1-rho_v3)*delta3_au*Snau0v2 + (1-rho_v3)*delta3_au*Vnau0v2 + kappa3 * Vpau0v3 - delta4_au * Vnau0v3 - omegav_na *Vnau0v3
    dVner0v3 = (1-rho_v3)*delta3_er*Sner0v2 + (1-rho_v3)*delta3_er*Vner0v2 + kappa3 * Vper0v3 - delta4_er * Vner0v3 - omegav_ne *Vner0v3
    dVneu0v3 = (1-rho_v3)*delta3_eu*Sneu0v2 + (1-rho_v3)*delta3_eu*Vneu0v2 + kappa3 * Vpeu0v3 - delta4_eu * Vneu0v3 - omegav_ne *Vneu0v3
    
    
    ##Gamma waning from first vaccination- immune vaxed class
    
    dW1Vpcr0v1 = 4*kappa1*Vpcr0v1 - 4*kappa1*W1Vpcr0v1 - delta2_cr*W1Vpcr0v1 -omegav_pc*W1Vpcr0v1
    dW2Vpcr0v1 = 4*kappa1*W1Vpcr0v1-4*kappa1*W2Vpcr0v1 - delta2_cr*W2Vpcr0v1 -omegav_pc*W2Vpcr0v1
    dW3Vpcr0v1 = 4*kappa1*W2Vpcr0v1-4*kappa1*W3Vpcr0v1 - delta2_cr*W3Vpcr0v1 -omegav_pc*W3Vpcr0v1
    
    dW1Vpcu0v1 = 4*kappa1*Vpcu0v1 - 4*kappa1*W1Vpcu0v1 - delta2_cu*W1Vpcu0v1 -omegav_pc*W1Vpcu0v1
    dW2Vpcu0v1 = 4*kappa1*W1Vpcu0v1-4*kappa1*W2Vpcu0v1 - delta2_cu*W2Vpcu0v1 -omegav_pc*W2Vpcu0v1
    dW3Vpcu0v1 = 4*kappa1*W2Vpcu0v1-4*kappa1*W3Vpcu0v1 - delta2_cu*W3Vpcu0v1 -omegav_pc*W3Vpcu0v1
    
    dW1Vpar0v1 = 4*kappa1*Vpar0v1 - 4*kappa1*W1Vpar0v1 - delta2_ar*W1Vpar0v1 -omegav_pa*W1Vpar0v1
    dW2Vpar0v1 = 4*kappa1*W1Vpar0v1-4*kappa1*W2Vpar0v1 - delta2_ar*W2Vpar0v1 -omegav_pa*W2Vpar0v1
    dW3Vpar0v1 = 4*kappa1*W2Vpar0v1-4*kappa1*W3Vpar0v1 - delta2_ar*W3Vpar0v1 -omegav_pa*W3Vpar0v1
    
    dW1Vpau0v1 = 4*kappa1*Vpau0v1 - 4*kappa1*W1Vpau0v1 - delta2_au*W1Vpau0v1 -omegav_pa*W1Vpau0v1
    dW2Vpau0v1 = 4*kappa1*W1Vpau0v1-4*kappa1*W2Vpau0v1 - delta2_au*W2Vpau0v1 -omegav_pa*W2Vpau0v1
    dW3Vpau0v1 = 4*kappa1*W2Vpau0v1-4*kappa1*W3Vpau0v1 - delta2_au*W3Vpau0v1 -omegav_pa*W3Vpau0v1
    
    dW1Vper0v1 = 4*kappa1*Vper0v1 - 4*kappa1*W1Vper0v1 - delta2_er*W1Vper0v1 -omegav_pe*W1Vper0v1
    dW2Vper0v1 = 4*kappa1*W1Vper0v1-4*kappa1*W2Vper0v1 - delta2_er*W2Vper0v1 -omegav_pe*W2Vper0v1
    dW3Vper0v1 = 4*kappa1*W2Vper0v1-4*kappa1*W3Vper0v1 - delta2_er*W3Vper0v1 -omegav_pe*W3Vper0v1
    
    dW1Vpeu0v1 = 4*kappa1*Vpeu0v1 - 4*kappa1*W1Vpeu0v1 - delta2_eu*W1Vpeu0v1 -omegav_pe*W1Vpeu0v1
    dW2Vpeu0v1 = 4*kappa1*W1Vpeu0v1-4*kappa1*W2Vpeu0v1 - delta2_eu*W2Vpeu0v1 -omegav_pe*W2Vpeu0v1
    dW3Vpeu0v1 = 4*kappa1*W2Vpeu0v1-4*kappa1*W3Vpeu0v1 - delta2_eu*W3Vpeu0v1 -omegav_pe*W3Vpeu0v1
    
    
    res = c(dScr0v0, dScu0v0, dSar0v0, dSau0v0, dSer0v0, dSeu0v0,
            dEcr1v0, dEcu1v0, dEar1v0, dEau1v0, dEer1v0, dEeu1v0,
            dAcr1v0, dAcu1v0, dAar1v0, dAau1v0, dAer1v0, dAeu1v0,
            dIcr1v0, dIcu1v0, dIar1v0, dIau1v0, dIer1v0, dIeu1v0,
            dHcr1v0, dHcu1v0, dHar1v0, dHau1v0, dHer1v0, dHeu1v0,
            dRpcr1v0, dRpcu1v0, dRpar1v0, dRpau1v0, dRper1v0, dRpeu1v0,
            dRncr1v0, dRncu1v0, dRnar1v0, dRnau1v0, dRner1v0, dRneu1v0,
            dDcr1v0, dDcu1v0, dDar1v0, dDau1v0, dDer1v0, dDeu1v0, 
            
            #Unvax one prior exposure
            dSpcr1v0, dSpcu1v0, dSpar1v0, dSpau1v0, dSper1v0, dSpeu1v0,
            dSncr1v0, dSncu1v0, dSnar1v0, dSnau1v0, dSner1v0, dSneu1v0,
            dEcr2v0, dEcu2v0, dEar2v0, dEau2v0, dEer2v0, dEeu2v0,
            dAcr2v0, dAcu2v0, dAar2v0, dAau2v0, dAer2v0, dAeu2v0,
            dIcr2v0, dIcu2v0, dIar2v0, dIau2v0, dIer2v0, dIeu2v0,
            dHcr2v0, dHcu2v0, dHar2v0, dHau2v0, dHer2v0, dHeu2v0,
            dRpcr2v0, dRpcu2v0, dRpar2v0, dRpau2v0, dRper2v0, dRpeu2v0,
            dRncr2v0, dRncu2v0, dRnar2v0, dRnau2v0, dRner2v0, dRneu2v0,
            dDcr2v0, dDcu2v0, dDar2v0, dDau2v0, dDer2v0, dDeu2v0, 
            
            #Unvax two prior exposure
            dSpcr2v0, dSpcu2v0, dSpar2v0, dSpau2v0, dSper2v0, dSpeu2v0,
            dSncr2v0, dSncu2v0, dSnar2v0, dSnau2v0, dSner2v0, dSneu2v0,
            dEcr3v0, dEcu3v0, dEar3v0, dEau3v0, dEer3v0, dEeu3v0,
            dAcr3v0, dAcu3v0, dAar3v0, dAau3v0, dAer3v0, dAeu3v0,
            dIcr3v0, dIcu3v0, dIar3v0, dIau3v0, dIer3v0, dIeu3v0,
            dHcr3v0, dHcu3v0, dHar3v0, dHau3v0, dHer3v0, dHeu3v0,
            dRpcr3v0, dRpcu3v0, dRpar3v0, dRpau3v0, dRper3v0, dRpeu3v0,
            dRncr3v0, dRncu3v0, dRnar3v0, dRnau3v0, dRner3v0, dRneu3v0,
            dDcr3v0, dDcu3v0, dDar3v0, dDau3v0, dDer3v0, dDeu3v0, 
            
            ##One vax dose no exposure
            
            dSpcr0v1, dSpcu0v1, dSpar0v1, dSpau0v1, dSper0v1, dSpeu0v1,
            dSncr0v1, dSncu0v1, dSnar0v1, dSnau0v1, dSner0v1, dSneu0v1,
            dEcr1v1, dEcu1v1, dEar1v1, dEau1v1, dEer1v1, dEeu1v1,
            dAcr1v1, dAcu1v1, dAar1v1, dAau1v1, dAer1v1, dAeu1v1,
            dIcr1v1, dIcu1v1, dIar1v1, dIau1v1, dIer1v1, dIeu1v1,
            dHcr1v1, dHcu1v1, dHar1v1, dHau1v1, dHer1v1, dHeu1v1,
            dRpcr1v1, dRpcu1v1, dRpar1v1, dRpau1v1, dRper1v1, dRpeu1v1,
            dRncr1v1, dRncu1v1, dRnar1v1, dRnau1v1, dRner1v1, dRneu1v1,
            dDcr1v1, dDcu1v1, dDar1v1, dDau1v1, dDer1v1, dDeu1v1, 
            
            #One vax dose one prior exposure
            dSpcr1v1, dSpcu1v1, dSpar1v1, dSpau1v1, dSper1v1, dSpeu1v1,
            dSncr1v1, dSncu1v1, dSnar1v1, dSnau1v1, dSner1v1, dSneu1v1,
            dEcr2v1, dEcu2v1, dEar2v1, dEau2v1, dEer2v1, dEeu2v1,
            dAcr2v1, dAcu2v1, dAar2v1, dAau2v1, dAer2v1, dAeu2v1,
            dIcr2v1, dIcu2v1, dIar2v1, dIau2v1, dIer2v1, dIeu2v1,
            dHcr2v1, dHcu2v1, dHar2v1, dHau2v1, dHer2v1, dHeu2v1,
            dRpcr2v1, dRpcu2v1, dRpar2v1, dRpau2v1, dRper2v1, dRpeu2v1,
            dRncr2v1, dRncu2v1, dRnar2v1, dRnau2v1, dRner2v1, dRneu2v1,
            dDcr2v1, dDcu2v1, dDar2v1, dDau2v1, dDer2v1, dDeu2v1, 
            
            #One vax dose two prior exposure
            dSpcr2v1, dSpcu2v1, dSpar2v1, dSpau2v1, dSper2v1, dSpeu2v1,
            dSncr2v1, dSncu2v1, dSnar2v1, dSnau2v1, dSner2v1, dSneu2v1,
            dEcr3v1, dEcu3v1, dEar3v1, dEau3v1, dEer3v1, dEeu3v1,
            dAcr3v1, dAcu3v1, dAar3v1, dAau3v1, dAer3v1, dAeu3v1,
            dIcr3v1, dIcu3v1, dIar3v1, dIau3v1, dIer3v1, dIeu3v1,
            dHcr3v1, dHcu3v1, dHar3v1, dHau3v1, dHer3v1, dHeu3v1,
            dRpcr3v1, dRpcu3v1, dRpar3v1, dRpau3v1, dRper3v1, dRpeu3v1,
            dRncr3v1, dRncu3v1, dRnar3v1, dRnau3v1, dRner3v1, dRneu3v1,
            dDcr3v1, dDcu3v1, dDar3v1, dDau3v1, dDer3v1, dDeu3v1, 
            
            ##Two vax dose no prior exposure
            dSpcr0v2, dSpcu0v2, dSpar0v2, dSpau0v2, dSper0v2, dSpeu0v2,
            dSncr0v2, dSncu0v2, dSnar0v2, dSnau0v2, dSner0v2, dSneu0v2,
            dEcr1v2, dEcu1v2, dEar1v2, dEau1v2, dEer1v2, dEeu1v2,
            dAcr1v2, dAcu1v2, dAar1v2, dAau1v2, dAer1v2, dAeu1v2,
            dIcr1v2, dIcu1v2, dIar1v2, dIau1v2, dIer1v2, dIeu1v2,
            dHcr1v2, dHcu1v2, dHar1v2, dHau1v2, dHer1v2, dHeu1v2,
            dRpcr1v2, dRpcu1v2, dRpar1v2, dRpau1v2, dRper1v2, dRpeu1v2,
            dRncr1v2, dRncu1v2, dRnar1v2, dRnau1v2, dRner1v2, dRneu1v2,
            dDcr1v2, dDcu1v2, dDar1v2, dDau1v2, dDer1v2, dDeu1v2, 
            
            #Two vax dose one prior exposure
            dSpcr1v2, dSpcu1v2, dSpar1v2, dSpau1v2, dSper1v2, dSpeu1v2,
            dSncr1v2, dSncu1v2, dSnar1v2, dSnau1v2, dSner1v2, dSneu1v2,
            dEcr2v2, dEcu2v2, dEar2v2, dEau2v2, dEer2v2, dEeu2v2,
            dAcr2v2, dAcu2v2, dAar2v2, dAau2v2, dAer2v2, dAeu2v2,
            dIcr2v2, dIcu2v2, dIar2v2, dIau2v2, dIer2v2, dIeu2v2,
            dHcr2v2, dHcu2v2, dHar2v2, dHau2v2, dHer2v2, dHeu2v2,
            dRpcr2v2, dRpcu2v2, dRpar2v2, dRpau2v2, dRper2v2, dRpeu2v2,
            dRncr2v2, dRncu2v2, dRnar2v2, dRnau2v2, dRner2v2, dRneu2v2,
            dDcr2v2, dDcu2v2, dDar2v2, dDau2v2, dDer2v2, dDeu2v2, 
            
            #Two vax dose two prior exposure
            dSpcr2v2, dSpcu2v2, dSpar2v2, dSpau2v2, dSper2v2, dSpeu2v2,
            dSncr2v2, dSncu2v2, dSnar2v2, dSnau2v2, dSner2v2, dSneu2v2,
            dEcr3v2, dEcu3v2, dEar3v2, dEau3v2, dEer3v2, dEeu3v2,
            dAcr3v2, dAcu3v2, dAar3v2, dAau3v2, dAer3v2, dAeu3v2,
            dIcr3v2, dIcu3v2, dIar3v2, dIau3v2, dIer3v2, dIeu3v2,
            dHcr3v2, dHcu3v2, dHar3v2, dHau3v2, dHer3v2, dHeu3v2,
            dRpcr3v2, dRpcu3v2, dRpar3v2, dRpau3v2, dRper3v2, dRpeu3v2,
            dRncr3v2, dRncu3v2, dRnar3v2, dRnau3v2, dRner3v2, dRneu3v2,
            dDcr3v2, dDcu3v2, dDar3v2, dDau3v2, dDer3v2, dDeu3v2, 
            
            #Three vax dose no prior exposure
            dSpcr0v3, dSpcu0v3, dSpar0v3, dSpau0v3, dSper0v3, dSpeu0v3,
            dSncr0v3, dSncu0v3, dSnar0v3, dSnau0v3, dSner0v3, dSneu0v3,
            dEcr1v3, dEcu1v3, dEar1v3, dEau1v3, dEer1v3, dEeu1v3,
            dAcr1v3, dAcu1v3, dAar1v3, dAau1v3, dAer1v3, dAeu1v3,
            dIcr1v3, dIcu1v3, dIar1v3, dIau1v3, dIer1v3, dIeu1v3,
            dHcr1v3, dHcu1v3, dHar1v3, dHau1v3, dHer1v3, dHeu1v3,
            dRpcr1v3, dRpcu1v3, dRpar1v3, dRpau1v3, dRper1v3, dRpeu1v3,
            dRncr1v3, dRncu1v3, dRnar1v3, dRnau1v3, dRner1v3, dRneu1v3,
            dDcr1v3, dDcu1v3, dDar1v3, dDau1v3, dDer1v3, dDeu1v3, 
            
            #Three vax dose one prior exposure
            dSpcr1v3, dSpcu1v3, dSpar1v3, dSpau1v3, dSper1v3, dSpeu1v3,
            dSncr1v3, dSncu1v3, dSnar1v3, dSnau1v3, dSner1v3, dSneu1v3,
            dEcr2v3, dEcu2v3, dEar2v3, dEau2v3, dEer2v3, dEeu2v3,
            dAcr2v3, dAcu2v3, dAar2v3, dAau2v3, dAer2v3, dAeu2v3,
            dIcr2v3, dIcu2v3, dIar2v3, dIau2v3, dIer2v3, dIeu2v3,
            dHcr2v3, dHcu2v3, dHar2v3, dHau2v3, dHer2v3, dHeu2v3,
            dRpcr2v3, dRpcu2v3, dRpar2v3, dRpau2v3, dRper2v3, dRpeu2v3,
            dRncr2v3, dRncu2v3, dRnar2v3, dRnau2v3, dRner2v3, dRneu2v3,
            dDcr2v3, dDcu2v3, dDar2v3, dDau2v3, dDer2v3, dDeu2v3, 
            
            #Three vax dose two prior exposure
            dSpcr2v3, dSpcu2v3, dSpar2v3, dSpau2v3, dSper2v3, dSpeu2v3,
            dSncr2v3, dSncu2v3, dSnar2v3, dSnau2v3, dSner2v3, dSneu2v3,
            dEcr3v3, dEcu3v3, dEar3v3, dEau3v3, dEer3v3, dEeu3v3,
            dAcr3v3, dAcu3v3, dAar3v3, dAau3v3, dAer3v3, dAeu3v3,
            dIcr3v3, dIcu3v3, dIar3v3, dIau3v3, dIer3v3, dIeu3v3,
            dHcr3v3, dHcu3v3, dHar3v3, dHau3v3, dHer3v3, dHeu3v3,
            dRpcr3v3, dRpcu3v3, dRpar3v3, dRpau3v3, dRper3v3, dRpeu3v3,
            dRncr3v3, dRncu3v3, dRnar3v3, dRnau3v3, dRner3v3, dRneu3v3,
            dDcr3v3, dDcu3v3, dDar3v3, dDau3v3, dDer3v3, dDeu3v3,
            
            dEcum1v0_cr, dEcum1v0_cu, dEcum1v0_ar, dEcum1v0_au, dEcum1v0_er, dEcum1v0_eu,
            dEcum2v0_cr, dEcum2v0_cu, dEcum2v0_ar, dEcum2v0_au, dEcum2v0_er, dEcum2v0_eu,
            dEcum3v0_cr, dEcum3v0_cu, dEcum3v0_ar, dEcum3v0_au, dEcum3v0_er, dEcum3v0_eu,
            
            dEcum1v1_cr, dEcum1v1_cu, dEcum1v1_ar, dEcum1v1_au, dEcum1v1_er, dEcum1v1_eu,
            dEcum2v1_cr, dEcum2v1_cu, dEcum2v1_ar, dEcum2v1_au, dEcum2v1_er, dEcum2v1_eu,
            dEcum3v1_cr, dEcum3v1_cu, dEcum3v1_ar, dEcum3v1_au, dEcum3v1_er, dEcum3v1_eu,
            
            dEcum1v2_cr, dEcum1v2_cu, dEcum1v2_ar, dEcum1v2_au, dEcum1v2_er, dEcum1v2_eu,
            dEcum2v2_cr, dEcum2v2_cu, dEcum2v2_ar, dEcum2v2_au, dEcum2v2_er, dEcum2v2_eu,
            dEcum3v2_cr, dEcum3v2_cu, dEcum3v2_ar, dEcum3v2_au, dEcum3v2_er, dEcum3v2_eu,
            
            dEcum1v3_cr, dEcum1v3_cu, dEcum1v3_ar, dEcum1v3_au, dEcum1v3_er, dEcum1v3_eu,
            dEcum2v3_cr, dEcum2v3_cu, dEcum2v3_ar, dEcum2v3_au, dEcum2v3_er, dEcum2v3_eu,
            dEcum3v3_cr, dEcum3v3_cu, dEcum3v3_ar, dEcum3v3_au, dEcum3v3_er, dEcum3v3_eu,
            
            dIcum1v0_cr, dIcum1v0_cu, dIcum1v0_ar, dIcum1v0_au, dIcum1v0_er, dIcum1v0_eu,
            dIcum2v0_cr, dIcum2v0_cu, dIcum2v0_ar, dIcum2v0_au, dIcum2v0_er, dIcum2v0_eu,
            dIcum3v0_cr, dIcum3v0_cu, dIcum3v0_ar, dIcum3v0_au, dIcum3v0_er, dIcum3v0_eu,
            
            dIcum1v1_cr, dIcum1v1_cu, dIcum1v1_ar, dIcum1v1_au, dIcum1v1_er, dIcum1v1_eu,
            dIcum2v1_cr, dIcum2v1_cu, dIcum2v1_ar, dIcum2v1_au, dIcum2v1_er, dIcum2v1_eu,
            dIcum3v1_cr, dIcum3v1_cu, dIcum3v1_ar, dIcum3v1_au, dIcum3v1_er, dIcum3v1_eu,
            
            dIcum1v2_cr, dIcum1v2_cu, dIcum1v2_ar, dIcum1v2_au, dIcum1v2_er, dIcum1v2_eu,
            dIcum2v2_cr, dIcum2v2_cu, dIcum2v2_ar, dIcum2v2_au, dIcum2v2_er, dIcum2v2_eu,
            dIcum3v2_cr, dIcum3v2_cu, dIcum3v2_ar, dIcum3v2_au, dIcum3v2_er, dIcum3v2_eu,
            
            dIcum1v3_cr, dIcum1v3_cu, dIcum1v3_ar, dIcum1v3_au, dIcum1v3_er, dIcum1v3_eu,
            dIcum2v3_cr, dIcum2v3_cu, dIcum2v3_ar, dIcum2v3_au, dIcum2v3_er, dIcum2v3_eu,
            dIcum3v3_cr, dIcum3v3_cu, dIcum3v3_ar, dIcum3v3_au, dIcum3v3_er, dIcum3v3_eu,
            
            dW1Rpcr1v0, dW2Rpcr1v0, dW3Rpcr1v0,
            dW1Rpcu1v0, dW2Rpcu1v0, dW3Rpcu1v0,
            dW1Rpar1v0, dW2Rpar1v0, dW3Rpar1v0,
            dW1Rpau1v0, dW2Rpau1v0, dW3Rpau1v0,
            dW1Rper1v0, dW2Rper1v0, dW3Rper1v0,
            dW1Rpeu1v0, dW2Rpeu1v0, dW3Rpeu1v0,

            dVpcr0v1, dVpcu0v1, dVpar0v1, dVpau0v1, dVper0v1, dVpeu0v1,
            dVncr0v1, dVncu0v1, dVnar0v1, dVnau0v1, dVner0v1, dVneu0v1,
            dVpcr0v2, dVpcu0v2, dVpar0v2, dVpau0v2, dVper0v2, dVpeu0v2,
            dVncr0v2, dVncu0v2, dVnar0v2, dVnau0v2, dVner0v2, dVneu0v2,
            dVpcr0v3, dVpcu0v3, dVpar0v3, dVpau0v3, dVper0v3, dVpeu0v3,
            dVncr0v3, dVncu0v3, dVnar0v3, dVnau0v3, dVner0v3, dVneu0v3,
            
            dW1Vpcr0v1,dW2Vpcr0v1,dW3Vpcr0v1, 
            dW1Vpcu0v1,dW2Vpcu0v1,dW3Vpcu0v1, 
            dW1Vpar0v1,dW2Vpar0v1,dW3Vpar0v1, 
            dW1Vpau0v1,dW2Vpau0v1,dW3Vpau0v1, 
            dW1Vper0v1,dW2Vper0v1,dW3Vper0v1, 
            dW1Vpeu0v1,dW2Vpeu0v1,dW3Vpeu0v1
            
    )
    time_varying_pars=c('foi_cr_0v0'=foi_cr_0v0, 'foi_cu_0v0'=foi_cu_0v0, 'foi_ar_0v0'=foi_ar_0v0, 'foi_au_0v0'=foi_au_0v0, 'foi_er_0v0'=foi_er_0v0, 'foi_eu_0v0'=foi_eu_0v0,
                        'foi_cr_0v1'=foi_cr_0v1, 'foi_cu_0v1'=foi_cu_0v1, 'foi_ar_0v1'=foi_ar_0v1, 'foi_au_0v1'= foi_au_0v1, 'foi_er_0v1'=foi_er_0v1, 'foi_eu_0v1'=foi_eu_0v1,
                        'foi_cr_0v2'=foi_cr_0v2, 'foi_cu_0v2'=foi_cu_0v2, 'foi_ar_0v2'=foi_ar_0v2, 'foi_au_0v2'= foi_au_0v2, 'foi_er_0v2'=foi_er_0v2, 'foi_eu_0v2'=foi_eu_0v2,
                        'foi_cr_0v3'=foi_cr_0v3, 'foi_cu_0v3'=foi_cu_0v3, 'foi_ar_0v3'=foi_ar_0v3, 'foi_au_0v3'= foi_au_0v3, 'foi_er_0v3'=foi_er_0v3, 'foi_eu_0v3'=foi_eu_0v3,
                        
                        'foi_cr_1v0'=foi_cr_1v0, 'foi_cu_1v0'=foi_cu_1v0, 'foi_ar_1v0'=foi_ar_1v0, 'foi_au_1v0'=foi_au_1v0, 'foi_er_1v0'=foi_er_1v0, 'foi_eu_1v0'=foi_eu_1v0,
                        'foi_cr_1v1'=foi_cr_1v1, 'foi_cu_1v1'=foi_cu_1v1, 'foi_ar_1v1'=foi_ar_1v1, 'foi_au_1v1'= foi_au_1v1, 'foi_er_1v1'=foi_er_1v1, 'foi_eu_1v1'=foi_eu_1v1,
                        'foi_cr_1v2'=foi_cr_1v2, 'foi_cu_1v2'=foi_cu_1v2, 'foi_ar_1v2'=foi_ar_1v2, 'foi_au_1v2'= foi_au_1v2, 'foi_er_1v2'=foi_er_1v2, 'foi_eu_1v2'=foi_eu_1v2,
                        'foi_cr_1v3'=foi_cr_1v3, 'foi_cu_1v3'=foi_cu_1v3, 'foi_ar_1v3'=foi_ar_1v3, 'foi_au_1v3'= foi_au_1v3, 'foi_er_1v3'=foi_er_1v3, 'foi_eu_1v3'=foi_eu_1v3,
                        
                        'foi_cr_2v0'=foi_cr_2v0, 'foi_cu_2v0'=foi_cu_2v0, 'foi_ar_2v0'=foi_ar_2v0, 'foi_au_2v0'=foi_au_2v0, 'foi_er_2v0'=foi_er_2v0, 'foi_eu_2v0'=foi_eu_2v0,
                        'foi_cr_2v1'=foi_cr_2v1, 'foi_cu_2v1'=foi_cu_2v1, 'foi_ar_2v1'=foi_ar_2v1, 'foi_au_2v1'= foi_au_2v1, 'foi_er_2v1'=foi_er_2v1, 'foi_eu_2v1'=foi_eu_2v1,
                        'foi_cr_2v2'=foi_cr_2v2, 'foi_cu_2v2'=foi_cu_2v2, 'foi_ar_2v2'=foi_ar_2v2, 'foi_au_2v2'= foi_au_2v2, 'foi_er_2v2'=foi_er_2v2, 'foi_eu_2v2'=foi_eu_2v2,
                        'foi_cr_2v3'=foi_cr_2v3, 'foi_cu_2v3'=foi_cu_2v3, 'foi_ar_2v3'=foi_ar_2v3, 'foi_au_2v3'= foi_au_2v3, 'foi_er_2v3'=foi_er_2v3, 'foi_eu_2v3'=foi_eu_2v3,
                        
                        'p_d1_cr'=p_d1_cr, 'p_d1_cu'=p_d1_cu, 'p_d1_ar'=p_d1_ar, 'p_d1_au'=p_d1_au, 'p_d1_er'=p_d1_er, 'p_d1_eu'=p_d1_eu,
                        'p_d2_cr'=p_d2_cr, 'p_d2_cu'=p_d2_cu, 'p_d2_ar'=p_d2_ar, 'p_d2_au'=p_d2_au, 'p_d2_er'=p_d2_er, 'p_d2_eu'=p_d2_eu,
                        #'daily_dose'=daily_dose, 'p_first'=p_first, 'dose_one'=dose_one, 'dose_two'=dose_two,
                        'delta1_cr'=delta1_cr, 'delta1_cu'=delta1_cu, 'delta1_ar'=delta1_ar, 'delta1_au'=delta1_au, 'delta1_er'=delta1_er, 'delta1_eu'=delta1_eu,
                        'delta2_cr'=delta2_cr, 'delta2_cu'=delta2_cu, 'delta2_ar'=delta2_ar, 'delta2_au'=delta2_au, 'delta2_er'=delta2_er, 'delta2_eu'=delta2_eu)
    #'delta3_cr'=delta3_cr, 'delta3_cu'=delta3_cu, 'delta3_ar'=delta3_ar, 'delta3_au'=delta3_au, 'delta3_er'=delta3_er, 'delta3_eu'=delta3_eu,
    #'delta4_cr'=delta4_cr, 'delta4_cu'=delta4_cu, 'delta4_ar'=delta4_ar, 'delta4_au'=delta4_au, 'delta4_er'=delta4_er, 'delta4_eu'=delta4_eu)
    
    #cat("Time=", t, "dSeu0v0=",dSeu0v0,"foi_eu_0v0=", foi_eu_0v0,"dSer0v0=",dSer0v0,"foi_er_0v0=", foi_er_0v0, "dSar0v0=", dSar0v0, "foi_ar_0v0=",foi_ar_0v0,"\n")
    list(res, time_varying_pars)
  }) #closing brackets for as.list(parames) loop
}# closing bracket for function  Icumv1_ar =x[715]; Icumv1_cu =x[716]; Icumv1_ar =x[717]; Icumv1_au =x[718]; Icumv1_er =x[719]; Icumv1_eu =x[720]