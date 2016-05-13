 #Load standard clinical dataset and feature vectors 
# 
# Author: pdawadi
###############################################################################

source("JConst.R");
#source("SHCompareCount.R")

#---COMMENTS Regarding Horizon House Particpant Pool 
#  105 has 1905 date in it be careful!
#  110 moved to 120. Psych data uses 110 as its ID
#  107,121,124 are multiple residents. I am ignoring them 
#  112 has no SR and SH data
#  110 has two months of data then he moved to 120. i am not including it since it was creating me trouble while comparing CLinical data

#116
#114 : has no self report measurements 
#117-- >lots of missng 110 ->difficult to 110,  , 110 
#101 ,102,        110,  116, ,119 
#112 removed because it does't have clinical scores.
#Always put in increasing order nor i cannot combine between pscyh changes and sh changes
TPID=NA
#HH_TPID =  c(101 ,102 ,103,104,105,106,108,109,111,113,114,115, 117,118,119,122,123,125) #18 individuals in total
HH_TPID = c(1)
#HH_TPID = c(1)
#HH_TPID =  c(101,102,103) #18 individuals in total


#,120)#,115,122,123,125)#,122,123,124,125)#,120) 113 110 101 102 ,103,

#ACT_OHSU_TPID =  "
"c(599,  279,  266,  737,  276,  344,  392,  401,  358,  353,      
        352,  362,  347,  357,  354,  356,  363,  685,  408,  383,      
        397,  330,  406,  360,  469,  468,  437,  454,  484,  444,      
        436,  470,  474,  473,  487,  503,  502,  494,  496,  488,      
        434,  508,  683,  687,  525,  564,  557,  589,  584,  595,      
        609,  506,  601,  555,  345,  596,  417,  637,  627,  621,      
        642,  641,  701,  626,  664,  679,  688,  704,  666,  731,      
        675,  982,  1027, 1041)"    
#11088 has one data point  remov it 
#there are negative numbers in 11140,11026
ANN_OHSU_TPID =  
c(11063,11066,11067,11069,11070,11072,11073,
		11078,11079,11080,11091,11094,11099,11105,11112,11113,11116,11120
		,11122,11125,11127,11130,11134,11137,11140,11141,11142,11148,11149,11150,
		11151,11152,11159,11162,11167,11170,11171,11177,11178,11192,11193,11198,
		11208,11209)

c(381 ,6084 ,6349,7621,11000,11003,11004,11011,11012,11017,11018,11019,11020,11021,11022,11026,11027,11029,11033,11034,11039,11040,11043,11044,
		11049,11050,11052,11058,11060)




#TPID =  c(119);
#114 do not have dates in SR data i am ignoring it ,117,115
VERBOSE = TRUE;

OHSU_Data = function(csv.psychpath ="ohsu2014.csv"){
	
	psych.data = read.csv(csv.psychpath,strip.white=TRUE,as.is=TRUE,na.strings=c("","-1","None"," "),sep=",");
	psych.reqd = subset(psych.data,select= c("VSTpatid","VSTseqno","VSTdateEVAL","vstMMSE","GAITSECONDS"))
	#sort the id
	psych.reqd = psych.reqd[order(psych.reqd$VSTpatid),]	
	
	#rename it as the hh data
	colnames(psych.reqd) = c("id","ttimepoint","TIMESTAMP_PST","MMSE","TUG");
	na.date = which(!is.na(psych.reqd$TIMESTAMP_PST));
	psych.reqd= psych.reqd[na.date,]
	#print(psych.reqd) 	
	psych.reqd = filterById(psych.reqd, ANN_OHSU_TPID); 
	psych.reqd$TIMESTAMP_PST = as.Date(strptime(psych.reqd$TIMESTAMP_PST,"%m/%d/%Y"))
	psych.reqd$TIMESTAMP_PST =  strftime(psych.reqd$TIMESTAMP_PST,"%Y-%m"); 	
	
	return(psych.reqd);
}

 


#Load Clinical Assessment Data 
#tested the output looks ok 
stdPsychData = function(csv.psychpath = '../DataFromIAQBuildDataSheet/FromYiBoPST/UsedToRun/TestProcessedH003_median.csv'){
#stdPsychData = function(csv.psychpath = '../DataFromIAQBuildDataSheet/FromYiBoPST/SampleMedianAtmo2.csv'){
#stdPsychData = function(csv.psychpath = '../DataFromIAQBuildDataSheet/ProcessedH002_median.csv'){
#stdPsychData = function(csv.psychpath = '../IndoorAirQuality/smTestData.csv'){
#stdPsychData = function(csv.psychpath = '../PsychData/hh2014.csv'){
	#"~/Dropbox/Research/HHCorrelation/HHData/hhpsych-10-2013PA.csv"){
	#Read csv file	
	psych.data = read.csv(csv.psychpath,strip.white=TRUE,as.is=TRUE,na.strings=c("","-1","None","?"),sep=",");
	#print("HAPPY TESTING LINE 88 JLoadData.R  line 88  Psych data TEST TEST TEST TEST TEST TEST")
	#print("Psych data TEST TEST TEST TEST TEST TEST")
	#print("HAPPY TESTING LINE 89 JLoadData.R  line 89  Psych data TEST TEST TEST TEST TEST TEST")
	#print(psych.data)
	#print("Psych data TEST TEST TEST TEST TEST TEST")
	#print("HAPPY TESTING LINE 93 JLoadData.R  line 93  Psych data TEST TEST TEST TEST TEST TEST")
	#Keep selected columns
	#print("happy testing line 95")
	#print(psych.std.var)
	#print("happy testing line 98")
	psych.reqd = subset(psych.data,select= c("id","ttimepoint","TIMESTAMP_PST",psych.std.var));#i have removed age, edu	
	#print("HAPPY TESTING LINE 98 JLoadData.R  line 98  Psych data TEST TEST TEST TEST TEST TEST")
	#print(psych.reqd)
	#print("HAPPY TESTING LINE 100 JLoadData.R  line 100  Psych data TEST TEST TEST TEST TEST TEST")
	#convert biannualtdate to date format 
	#print("HAPPY TESTING LINE 106 JLoadData.R ");print(psych.reqd$TIMESTAMP_PST); # "8/29/15 7:00"  "8/29/15 8:00"  "8/29/15 9:00"  "8/29/15 10:00"
  na.date = which(!is.na(psych.reqd$TIMESTAMP_PST)); 
	#print("HAPPY TESTING LINE 107 JLoadData.R ");print(na.date);  # 1 2 3 4 5 .... 19 20 ...
	psych.reqd= psych.reqd[na.date,];
  #print("HAPPY TESTING LINE 110 JLoadData.R ");print(psych.reqd);
	#    id ttimepoint TIMESTAMP_PST In_PM_ug_m3 In_Dylos_small In_Dylos_large
	# 1    1          1 8/25/15 15:00          NA      1085000.0       153618.5
	#psych.reqd$TIMESTAMP_PST = as.Date(strptime(psych.reqd$TIMESTAMP_PST,"%d-%b-%y"))
  # ?? not sure: psych.reqd$TIMESTAMP_PST = as.Date(strptime(psych.reqd$TIMESTAMP_PST,"%m/%d/%y %H:%M")); #8/25/2015  3:00:00 PM
	#print("HAPPY TESTING LINE 110 JLoadData.R ");print(psych.reqd$TIMESTAMP_PST);  # [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA N
	#psych.reqd$TIMESTAMP_PST =  strftime(psych.reqd$TIMESTAMP_PST,"%Y-%m"); 
  
	psych.reqd$TIMESTAMP_PST = strftime(strptime(psych.reqd$TIMESTAMP_PST,"%m/%d/%Y %I:%M:%S %p"), "%Y-%m-%d %H:%M");

	#print("HAPPY TESTING LINE 118 JLoadData.R ");print(psych.reqd$TIMESTAMP_PST);  # [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA N
	#psych.reqd$WSUIALDtot = rep(0,nrow(psych.reqd))
	#psych.reqd$WSUIADL=rep(0,nrow(psych.reqd))
 	#only load required id 
	psych.reqd = filterById(psych.reqd,HH_TPID); 
	#print("testing psych.reqd line 114 JLoadData.R happy testing Happy Testing");
	#print(psych.reqd)
	#print("testing psych.reqd line 114 JLoadData.R happy testing Happy Testing");
	
	write("                    Loading Standard Clinical Data                               ",stdout());
	write(paste("\n=============================================================================================="),stdout());
	write("           I have loaded Standard Clinical Data  for following Participants",stdout());
	write(cat(unique(psych.reqd $id)),stdout())
	write(paste("\n=============================================================================================="),stdout());
	write(paste("Total number of participants are : ",length(unique(psych.reqd $id))),stdout()); 
	write(paste("\n=============================================================================================="),stdout());
	write("Following clinical scores are loaded ",stdout());
	a<-"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	print(a)
 	#print(psych.reqd)    	 
	
	#rename the column names as RBANS, MMAA, TUG, GDS
  #colnames(psych.reqd)[which(names(psych.reqd) == "rbansistor")] = "RBANS";
	#colnames(psych.reqd)[which(names(psych.reqd) == "MMAAtotal")]  = "MMAA";
	#colnames(psych.reqd)[which(names(psych.reqd) == "tugtrial")]   = "TUG";
	#colnames(psych.reqd)[which(names(psych.reqd) == "DepStotal")]  = "GDS"; 
 	
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_PM_ug_m3")] = "PM_ug";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_Dylos_small")]  = "DylosS";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_Dylos_large")]   = "DylosL";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_O3_ppb")]  = "O3";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_NO2_ppb")] = "NO2";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_NO_ppb")]  = "NO";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_Nox_ppb")]   = "NOX";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_CO2_ppm")]  = "CO2";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_CH4_ppm")]  = "CH4";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_H2O_ppm")] = "H2O";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_Temp_C")]  = "Temp";
 	
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_PM25_ug_m3")] = "O_PM25";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_CO2_ppm")]  = "O_CO2";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_H2O_ppt")]   = "O_H2O";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_NO_ppb")]  = "O_NO";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_NO2_ppb")] = "O_NO2";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_Nox_ppb")]  = "O_Nox";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_O3_ppb")]   = "O_O3";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Garage_Temp_C")]  = "G_Temp";
 	colnames(psych.reqd)[which(names(psych.reqd) == "WS_pressure_mbar")]  = "WS_press";
 	colnames(psych.reqd)[which(names(psych.reqd) == "WS_temp_C")] = "WS_temp";
 	colnames(psych.reqd)[which(names(psych.reqd) == "WS_RH")] = "WS_RH";
 	colnames(psych.reqd)[which(names(psych.reqd) == "WS_Dew_C")]  = "WS_Dew";
 	colnames(psych.reqd)[which(names(psych.reqd) == "WS_WD_true_deg")]  = "WS_WD";
 	colnames(psych.reqd)[which(names(psych.reqd) == "WS_Windspeed_m_s")]  = "WS_WindSp";
 	
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m31_ppb")] = "m31";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m33_ppb")]  = "m33";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m42_ppb")]   = "m42";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m45_ppb")]  = "m45";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m59_ppb")] = "m59";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m69_ppb")]  = "m69";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m71_ppb")]   = "m71";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m73_ppb")]  = "m73";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m79_ppb")]  = "m79";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m81_ppb")] = "m81";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m93_ppb")]  = "m93";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m105_ppb")]  = "m105";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m107_ppb")]  = "m107";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m121_ppb")] = "m121";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m135_ppb")]  = "m135";
 	colnames(psych.reqd)[which(names(psych.reqd) == "In_m137_ppb")]  = "m137";

 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m31_ppb")] = "O_m31";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m33_ppb")]  = "O_m33";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m42_ppb")]   = "m42";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Outm45_ppb")]  = "O_m45";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m59_ppb")] = "O_m59";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m69_ppb")]  = "O_m69";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m71_ppb")]   = "O_m71";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m73_ppb")]  = "O_m73";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m79_ppb")]  = "O_m79";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m81_ppb")] = "O_m81";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m93_ppb")]  = "O_m93";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m105_ppb")]  = "O_m105";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m107_ppb")]  = "O_m107";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m121_ppb")] = "O_m121";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m135_ppb")]  = "O_m135";
 	colnames(psych.reqd)[which(names(psych.reqd) == "Out_m137_ppb")]  = "O_m137";
 	
 	# variable form: JConst.R: sorig.std.var = c("In_PM_ug_m3", "In_Dylos_small",	"In_Dylos_large",	"In_O3_ppb", "In_NO2_ppb", "In_NO_ppb", "In_Nox_ppb",	"In_CO2_ppm",	"In_CH4_ppm","In_H2O_ppm","In_Temp_C");
	#print(names(psych.reqd))
 	#print("aaaaaaaaabbbbbbbbbbbbbbbbaaaaaaaaabbbbbbbbbbbbbbbbaaaaaaaaabbbbbbbbbbbbbbbb")
 	#print("aaaaaaaaabbbbbbbbbbbbbbbbaaaaaaaaabbbbbbbbbbbbbbbbaaaaaaaaabbbbbbbbbbbbbbbb")
 	#print("aaaaaaaaabbbbbbbbbbbbbbbbaaaaaaaaabbbbbbbbbbbbbbbbaaaaaaaaabbbbbbbbbbbbbbbb")
	return(psych.reqd);
}

 

#Load everyday functioning data 
getShData = function(OHSU=F){
	#shpath =  "/net/files/home/pdawadi/Documents/HorizonHouse/hhdataAug16y2013/Submission/features/hh"){#
	#each participant has his dedicated list
	if (OHSU){ 
			TPID = ANN_OHSU_TPID; #read data from annonymized id but rename it from actual one 
			shpath="/home/pdawadi/Documents/HorizonHouse/OHSU_features_new/"
		}
	else{
		TPID = HH_TPID;
		shpath="/Volumes/Seagate Backup Plus Drive/IAQ/caabChange/ProjectCorr/FeatureExtraction/"
		#shpath="/home/pdawadi/Documents/HorizonHouse/HH_random_features/"
		
	}
 
	data.list <- vector("list", length(TPID));
	#name the list 
	if(OHSU){
	names(data.list)=ANN_OHSU_TPID;
    tag ="OHSU-"}
    else{
		names(data.list) =TPID;
	   #tag ="hh" 
		  tag = "atmo"
	}
	
	write(paste("\n=============================================================================================="),stdout());
	write("                      Loading Smart Home Sensor Data...  ",stdout());
	i=1;
	for (ids in TPID){	
	  #print("happy testing line 167 JLoadData.R  happy testing line 167 JLoadData.R");
		merged.csv=NA;
		for(act in SMARTHOME.ACTITIVIES){  # in JConst.R line 14 
		  #print("happy testing line 170 JLoadData.R  happy testing line 170 JLoadData.R");
		  #print("ids");
		  #print(ids);  ## 1, 2, 3, ....
		  #print(act);  ## e.g. leave_home; cook
		  #print("happy testing line 171 JLoadData.R  happy testing line 171 JLoadData.R");
			r=tryCatch({
			act.path = paste(shpath,tag,ids,".",act,sep="");	 # files in extracFeatures 
			 
			act.csv = read.csv(act.path,na.string="NA",as.is=T);},
			error=function(w){print(paste("Unable to load data",ids,act));
			}
			
			)
			#print("happy testing line 178 JLoadData.R  happy testing line 178 JLoadData.R"); 
		  #print("happy testing line 180 act.csv JLoadData.R  happy testing line 180 act.csv JLoadData.R");
		  #print("happy testing line 180 *****************************************************");
		  #print("happy testing line 180 *****************************************************");
		  #print("happy testing line 180 *****************************************************");
		  #print(act)
		  #print(act.csv)
		  #print(act)
		  #print("happy testing line 207 act.csv JLoadData.R  happy testing line 207 act.csv JLoadData.R");
		  #print("happy testing line 207 act.csv JLoadData.R  happy testing line 207 act.csv JLoadData.R");
		  #print("happy testing line 207 *****************************************************");
		  #print("happy testing line 207 *****************************************************");
		  #print("happy testing line 207 *****************************************************");
		  #print(merged.csv)	
		  #print("happy testing line 207 *****************************************************");
		  #print("happy testing line 207 *****************************************************");
		  #print("happy testing line 207 *****************************************************");
			#print("hello Happy Testing 213 JLoadData.R  happy testing line 213 JLoadData.R")
    		act.csv[,1]=strftime(act.csv[,1], "%Y-%m-%d %H:%M:%S"); 
    		#print(act.csv[,1]);
    		#print(act.csv[,2]);
    		#print(act)
			#print("hellp")
    		
			if (act!="FunctionalHealth"){
				#change start time to date time format 
			  #print("happy testing line 229");
				colnames(act.csv)=c("day",paste(act,".duration",sep=""), paste(act,".freqn",sep=""), 
						paste(act,".count",sep=""), paste(act,".start",sep=""));
				numeric.cols = ncol(act.csv) -1; #except last, all columns are numeric 
				#print("happy testing line 233");
				act.csv[,2:numeric.cols] = apply(act.csv[,2:numeric.cols],2,as.numeric);
				#print("happy testing line 235");
			    #keep only hours for start time
				x=act.csv[,numeric.cols+1];				
				#strip the date first
				date.rem = gsub("\\d+-\\d+-\\d+","",x)
				#strip minutes and second
				min.rem = gsub(":\\d+:\\d+.\\d+","",date.rem);
				#print(min.rem)
				#act.csv[,numeric.cols+1]=as.numeric(min.rem);			   
				#put only hours and ignore the rest
			}
			else{			
			  #print("happy testing line 229");
				colnames(act.csv)=c("day",paste(act,".duration",sep=""), paste(act,".freqn",sep=""))		
				#print("happy testing line 249");
				act.csv[,2:ncol(act.csv)] = apply(act.csv[,2:ncol(act.csv)],2,as.numeric);		
				#print("happy testing line 251");
				#print(act.csv)
			}
    	  	#merge two csv data to a single column 
			if(length(merged.csv)==1){
				merged.csv = act.csv
			}
			else{
				if(nrow(merged.csv)>nrow(act.csv)){
					merged.csv =  merge(merged.csv, act.csv, by ="day",all.x=T);
				}
				else{
					merged.csv =  merge(merged.csv, act.csv, by ="day",all.y=T);
				}
			}			 
 		 	}
		
		#slect the variables 
       
        #print(head(merged.csv))
        sh.data = data.frame(merged.csv[,c("day",SH.features)]);
		#Do the imputation
		#sh.vars = impute(sh.vars,3);#impute by median #replaced by linear interpolation while calculating feature vectors        
		sh.data [,2:ncol( sh.data )] = apply( sh.data [,2:ncol( sh.data )],2,as.numeric);	
    	write(paste("Loading data.. ",ids),stdout());
    	data.list[[i]] = sh.data ;
  	 	i=i+1;	
	}	
 
	write(paste("\n=============================================================================================="),stdout());
	write("                 I loaded smart home data for following participants  ",stdout());
	#write(cat(names(data.list)),stdout());	 	
	write(paste("Number of participants are",length(data.list)),stdout())
	write(paste("\n=============================================================================================="),stdout());
    write("I will analyze following  smart home activities features",stdout());
	#write(paste(colnames(data.list[[1]])),stdout())	
	
	write(paste("\n=============================================================================================="),stdout());
	
	
	
	return(data.list);	
}

#Select the given id 
filterById = function(psych.data,TPID){
	psych.filtered = as.data.frame(matrix(nrow = 0, ncol = length(psych.data),dimnames = list(NULL,names(psych.data))))
	
	for (ids in TPID){
		psych.filtered=rbind(psych.filtered,psych.data[psych.data$id==ids,])
	}
	levels(psych.filtered$id) =unique(psych.filtered$id )
	return(psych.filtered);
}


mergePsychShChanges = function(ch.psych,ch.sh,names=T,xColName = "monthtdate",yColName="date"){
	#print("--inside merge psych changes----------------");
	
	#print(ch.psych);print(ch.sh);print(length(ch.psych));print(length(ch.sh));
	
	
	#--Check if the lengths of the two data frame are equal 
	#print(names(ch.psych));print(names(ch.sh))
	
	#print(length(ch.psych));print(ch.sh)
	if(length(ch.psych)!=length(ch.sh)){
		stop("Error: Length of two lists differ");
	}
	
	#create a vector to hold the merged data frame
    data.list <- vector("list", length(ch.psych))	
	
	#--Does the merged list have same name as the one of the two lists
	if(names)  
		names(data.list)=names(ch.psych);
	
	for (i in 1:length(ch.psych)){
		print("nextone -------------------------")
		nameA=names(ch.sh[i]);
		nameB=names(ch.psych[i])
		
		#--Only merge if name matches, else stop
		if(nameB!=nameA){
			print (nameB)
			print(nameA)
			stop(paste("Error: Cannot combine", nameA," ",nameB,"names of the two data frame differ"));
		}
		#print("inside lop");
		
		#--Is there anything beyond NA 
		is.psych.ok = all(is.na(ch.psych[[i]]));
		is.sh.ok =  all(is.na(ch.sh[[i]]));
		#print(ch.psych[[i]]);	print(ch.psych[[i]])
		#-- If everything looks good merge them 
		if(is.psych.ok | is.sh.ok){			
			data.list[[i]] =NA;
			print("this should not execute")
			
		}else{
			#print(ch.psych[[i]])
			#print(ch.sh[[i]])
			temp.df = merge(x=ch.psych[[i]], y=ch.sh[[i]], by.x = xColName,by.y=yColName,all.x=TRUE );
			#print("temp df");
			#print(temp.df)
			#except date make sure that remaining cols are numeric 
			#print("happy testing line 418 JLoadData.R")
			#print("temp.df[,-1],2,as.numeric"); #print(temp.df[,-1][1]);  #2015-08-29 17:00
			  	#	PM_ug.diff DylosS.diff DylosL.diff   O3.diff NO2.diff NO.diff
				#1   2015-08-25 15:00          NA     45000.0    5297.5       NA   -5.40
				#2   2015-08-26 00:00          NA   -950805.0 -153618.5       NA  -10.70
			#temp.df[,-1][0] = as.POSIXct(temp.df[,-1][0], format="%Y-%m-%d  %H:%M")
			tempx = temp.df[,-1][1];
			tempL = nrow(tempx);
			#print(tempL);  # 186
			#print(tempx$PM_ug.diff[1]); #"2015-08-25 15:00"
			for (i in 1:tempL){
				tx = tempx$PM_ug.diff[1];
				tempx$PM_ug.diff[1] = as.POSIXct(tx, format="%Y-%m-%d  %H:%M");
			}
			#print("happy testing finished line 437");  # temp.df[,-1] is all the data in output csv
			#print(temp.df[]); #"2015-08-25 15:00"
			#firsttdate       PM_ug.diff DylosS.diff DylosL.diff   O3.diff NO2.diff
			#        1 2015-08-25 15:00          NA     45000.0    5297.5       NA
			#       10 2015-08-26 00:00          NA   -950805.0 -153618.5       NA
			num.df =  data.frame(matrix(apply(temp.df[,-1],2,as.numeric),ncol=ncol(temp.df[,-1]) ,dimnames=list(NULL,colnames(temp.df[,-1]))),check.rows=T);
    		
    		df = data.frame(cbind(temp.df[,1],num.df),stringsAsFactors=F)
			colnames(df) = c("date",colnames(num.df));
			#print(df)
			data.list[[i]] =df;
			#print("----------------------after merge---------------------");
			#print(df)
			
 		} 	
	}
	return(data.list);
}

OHSU_MAP = matrix(c(266,6349,
		276,11000,
		279,6084,
		330,11040,
		344,11003,
		345,11140,
		347,11020,
		352,11018,
		353,11017,
		354,11022,
		356,11026,
		357,11021,
		358,11012,
		360,11044,
		362,11019,
		363,11027,
		383,11034,
		392,11004,
		397,11039,
		401,11011,
		406,11043,
		408,11033,
		417,11142,
		434,11091,
		436,11066,
		437,11052,
		444,11063,
		454,11058,
		468,11050,
		469,11049,
		470,11067,
		473,11070,
		474,11069,
		484,11060,
		487,11072,
		488,11088,
		494,11079,
		496,11080,
		502,11078,
		503,11073,
		506,11130,
		508,11094,
		525,11112,
		555,11137,
		557,11116,
		564,11113,
		584,11122,
		589,11120,
		595,11125,
		596,11141,
		599,381,
		601,11134,
		609,11127,
		621,11150,
		626,11162,
		627,11149,
		637,11148,
		641,11152,
		642,11151,
		664,11167,
		666,11178,
		675,11193,
		679,11170,
		683,11099,
		685,11029,
		687,11105,
		688,11171,
		701,11159,
		704,11177,
		731,11192,
		737,7621,
		982,11198,
		1027,11208,
		1041,11209),ncol=2,byrow=TRUE)
		
#OHSU_MAP_LIST = list()
#OHSU_MAP_LIST =OHSU_MAP[,1]
#names(OHSU_MAP_LIST) = OHSU_MAP[,2]

#subj_home = read.csv("OHSU_SUBJ_HOME_MAP.csv");

#OHSU_SUBJ_HOME = list();
#OHSU_SUBJ_HOME =subj_home[,1]
#names(OHSU_SUBJ_HOME) = subj_home[,2]


