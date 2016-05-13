
# Define  psychology and smart home sensor varaibles
# 
# Author: pdawadi
###############################################################################

 

set.seed(100);
#---------------------------------------------------Significance level
SIG_LEVEL =  as.numeric(0.05);  

#---------------------------------------------------Smart Home Activities
#"Work" is excluded from this list since some participants do not "work"
SMARTHOME.ACTITIVIES =c("Sleep","Bed_Toilet_Transition","Leave_Home","Relax","Cook","Eat","Personal_Hygiene","FunctionalHealth")#,
#Standard Clinical Bianual variables
orig.std.var = c("In_PM_ug_m3",	"In_Dylos_small",	"In_Dylos_large",	"In_O3_ppb",	"In_NO2_ppb",	"In_NO_ppb",	"In_Nox_ppb",	"In_CO2_ppm",
                 "In_CH4_ppm",	"In_H2O_ppm",	"In_Temp_C",	"Out_PM25_ug_m3",	"Out_CO2_ppm", "Out_H2O_ppt",	"Out_NO_ppb",	"Out_NO2_ppb",
                 "Out_Nox_ppb",	"Out_O3_ppb",	"Garage_Temp_C",	"WS_pressure_mbar",	"WS_temp_C",	"WS_RH",	"WS_Dew_C",	"WS_WD_true_deg",
                 "WS_Windspeed_m_s", "In_m31_ppb",	"In_m33_ppb",	"In_m42_ppb",	"In_m45_ppb",	"In_m59_ppb",	"In_m69_ppb",	"In_m71_ppb",
                 "In_m73_ppb",	"In_m79_ppb",	"In_m81_ppb",	"In_m93_ppb",	"In_m105_ppb",	"In_m107_ppb",	"In_m121_ppb",	"In_m135_ppb",
                 "In_m137_ppb",	"Out_m31_ppb",	"Out_m33_ppb",	"Out_m42_ppb",	"Outm45_ppb",	"Out_m59_ppb",	"Out_m69_ppb",	"Out_m71_ppb",	
                 "Out_m73_ppb",	"Out_m79_ppb",	"Out_m81_ppb",	"Out_m93_ppb",	"Out_m105_ppb",	"Out_m107_ppb",	"Out_m121_ppb",	"Out_m135_ppb",	"Out_m137_ppb");
#orig.std.var = c("In_CO2_ppm","In_Nox_ppb","tugtrial","DepStotal")#,"rbansisdelmem"
#orig.std.var = c("rbansistor","MMAAtotal","tugtrial","DepStotal")#,"rbansisdelmem"
psych.std.var = c(orig.std.var)

#----------------------------------------------------SH.VAR
#Sleep 
sleep.features = c( "Sleep.duration","Sleep.freqn");
bed.features = c("Bed_Toilet_Transition.duration")#,"Bed_Toilet_Transition.freqn");
#ADL VAR
#Note : Most frequency metrics for ADL are zeros so I removed them from the feature vectors
leave_home.features= c("Leave_Home.duration" )				#, "Leave_Home.freqn");
relax.features=c("Relax.duration")							#,"Relax.freqn"); 
cook.features=c("Cook.duration")							#,"Cook.freqn");
eat.features= c(  "Eat.duration" )							#, "Eat.freqn");
personal_hygiene.features = c("Personal_Hygiene.duration")	#,"Personal_Hygiene.freqn");
adl.features =c(leave_home.features,relax.features,cook.features,eat.features,personal_hygiene.features) 
#SD variables Mobility 
#technically these are #counts and distance covered. I renamed since they are renamed as so in features
mobility.features = c( "FunctionalHealth.duration", "FunctionalHealth.freqn");
SH.features =c(sleep.features,bed.features,adl.features,mobility.features);

 #----------------------------------------------------Change Features
sleep.change =c("sleep_durn.change","sleep_freq.change", "bed.change");
mobility.change =c("mobility.durn.change","mobility.freqn.change");
adl.change = c("leave_home.change","relax.change","cook.change","eat.change","personal_hygiene.change");
sh.change = c(sleep.change,adl.change,mobility.change);

#----------------------------------------------------Skewnewss Features
sleep.sk =c("sleep_durn.sk", "sleep_freq.sk","bed.sk");
mobility.sk =c("mobility.durn.sk","mobility.freqn.sk");
adl.sk = c("leave_home.sk","relax.sk","cook.sk","eat.sk","personal_hygiene.sk");
sh.sk = c(sleep.sk,adl.sk,mobility.sk);



#----------------------------------------------------Autocorrelation features
sleep.acf =c("sleep_durn.acf", "sleep_freq.acf","bed.acf");
mobility.acf =c("mobility.durn.acf","mobility.freqn.acf");
adl.acf = c("leave_home.acf","relax.acf","cook.acf","eat.acf","personal_hygiene.acf");
sh.acf = c(sleep.acf,adl.acf,mobility.acf);



#----------------------------------------------------Standard deviation features
sleep.var =c("sleep.var", "sleep_freq.var", "bed.var");
mobility.var =c("mobility.durn.var","mobility.freqn.var");
adl.var = c("leave_home.var","relax.var","cook.var","eat.var","personal_hygiene.var");
sh.var = c(sleep.var,adl.var,mobility.var);


#----------------------------------------------------Kurtosis features
sleep.kurt =c("sleep.kurt", "sleep_freq.kurt", "bed.kurt");
mobility.kurt =c("mobility.durn.kurt","mobility.freqn.kurt");
adl.kurt = c("leave_home.kurt","relax.kurt","cook.kurt","eat.kurt","personal_hygiene.kurt");
sh.kurt = c(sleep.kurt,adl.kurt,mobility.kurt);


dataset.features = c(sh.change,sh.sk,sh.acf,sh.var)

