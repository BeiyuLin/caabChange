#require(foreign)
library(foreign)
source("JLoadData.R")
source("JConst.R")
source("JPACompareCount.R")
source("extract_features.R")

#constants in the 
LAST_N_HOURS = TRUE
FIRST_N_HOURS = TRUE
WIN_SIZE= 24 # 621 atmo1
# WIN_SIZE=30 
ATMOID = 1

#change this line to run the appropriate 

main=function(){
	first_exp();
	#early_pred();
	#trend_fit_pred();
}

first_exp = function(){
	#win_size = 35#seq(30,30,10)   ## NOT SURE WHY 35???
  win_size = 60
	#print("Happy Testing line 24 HHCorr HAPP");
	#print(win_size)
	
	for(w in win_size){
	  #print("Happy Testing line 27 HHCorr HAPP");
	  #print(w);
	  my.df = run_alg(last_n_HOURS=FALSE,
			first_n_HOURS=FALSE,
			win_size_ALG = w,
			detrending_TP="gaussian",
			no_HOUR=0)
	  #print("Happy Testing line 34 HHCorr HAPP");
	  #saveDF(my.df,paste("repl_dataset/MaureenBaseline",w,"_test",sep=""));
	  saveDF(my.df,paste("repl_dataset/IAQ",ATMOID,"_test",sep=""));
}
}

trend_fit_pred = function(){
	win_size = seq(10,95,5);#generate window sizes at interval of 5
	for (w in win_size){
		my.df = run_alg(last_n_HOURS=FALSE,
				first_n_HOURS=FALSE,
				win_size_ALG=w,
				detrending_TP="gaussian",
				no_HOUR=0)
		
	   saveDF(my.df,paste("repl_dataset/random/trend_gauss_random","_winsize_",w,sep=""));
	}
	
	for (w in win_size){
		my.df=run_alg(last_n_HOURS=FALSE,
				first_n_HOURS=FALSE,
				win_size_ALG=w,
				detrending_TP="linear",
				no_HOUR=0)
		saveDF(my.df,paste("repl_dataset/random/trend_trend_linear_random","_winsize_",w,sep=""));
	}
	
	
	for (w in win_size){
		my.df = run_alg(last_n_HOURS=FALSE,
				first_n_HOURS=FALSE,
				win_size_ALG=w,
				detrending_TP="no",
				no_HOUR=0)
	   saveDF(my.df,paste("repl_dataset/random/trend_trend_none_random","_winsize_",w,sep=""));
	}
}

run_alg=function(last_n_HOURS,
		 first_n_HOURS,
		 win_size_ALG,
		 detrending_TP,
		 no_HOUR){
	#--Load the PA data pwd
	pa.data = stdPsychData();	 
	#--Group them by list
	pa.list = by(pa.data,pa.data$id,identity);	 
	#print("test pa.list pa.list pa.list pa.list pa.list pa.list pa.list")
	#############
	#print("test pa.list pa.list pa.list pa.list pa.list pa.list pa.list"); print(pa.list);
	#pa.data$id: 1
	#id ttimepoint TIMESTAMP_PST      PM_ug    DylosS   DylosL   O3    NO2
	#1    1          1          <NA>         NA 1085000.0 153618.5   NA  -2.35
	#2    1          2          <NA>   8.427920 1130000.0 158916.0   NA  -7.75
	#3    1          3          <NA>   5.927800  476748.0  38846.0 3.90 -13.90
	#4    1          4          <NA>   5.902840  557971.0  49440.0 2.60  -8.20
	###############
	#--Find the changes PA data
	#print("slidewindowPA([,-1,-2]) happy testing line 79 HHCorr_Main.R");
	#print(slidewindowPA(x[,-1,-2]));
	#print("happy testing line 79 HHCorr_Main.R");
	diff.pa= lapply(pa.list,function(x){slidewindowPA(x[,-1,-2]);})  ### modified 4/24/16 5pm 
	# ?? not sure print(diff.pa);
	#diff.pa= lapply(pa.list,function(x){slidewindowPA(x[,-c(1,2)]);})
	#print ("teset diff.pa diff.pa diff.pa diff.pa diff.pa diff.pa diff.pa");print(diff.pa);	
	#$`1`
    #firsttdate PM_ug.diff DylosS.diff DylosL.diff   O3.diff NO2.diff NO.diff
	#       1       <NA>          NA     45000.0    5297.5       NA   -5.40
	#     2       <NA>          NA   -608252.0 -114772.5       NA  -11.55
	#    3       <NA>          NA   -527029.0 -104178.5       NA   -5.85
	#     4       <NA>          NA   -163288.0  -15891.5       NA   -9.90
	#      5       <NA>          NA   -753043.0 -132430.5       NA   -9.30
	#      6       <NA>          NA   -777763.0 -139493.5       NA  -11.35
	#---Load SH data	  
	sh.list = getShData();#of those id that  have  been listed
	atype=1
	print("shdata loaded")
	sh.var.changes=list()
	 
	{
		#--Group SH data by hours
		#lsh.monthly = analyze_monthly(sh.list)		
	  lsh.hourly = analyze_hourly(sh.list)		
		#To align PA dates with SH sensor dates, make few alignments
		#lsh.final = lapply(lsh.monthly, function(x){ if(names(x)[1]=="2011-06") return(x[-1]) else return(x)})
	  lsh.final = lapply(lsh.hourly, function(x){ if(names(x)[1]=="2015-06") return(x[-1]) else return(x)})
		#prefer 		
		for(i  in 1:length(lsh.final)){
		  y=lsh.final[i]
			x=lsh.final[[i]]
			#print("happy testing x happy testing line 114 HHC.R");print(x);
			#print("HAPPY TESTING line 112 HAPPY TESTING line 112 HHCor.R");
			#print("HAPPY TESTING line 112 HAPPY TESTING line 112 HHCor.R");
			#print("HAPPY TESTING line 112 HAPPY TESTING line 112 HHCor.R");
			#print("xxxxxx line 124 HHCorr.Main.R");print(x) ## all the raw data from SH with duration
			#################
			#strftime(x[, 1], "%m/%d/%y %H:%M:%S"): 08/26/15 00:00:00
			#Sleep.duration Sleep.freqn Bed_Toilet_Transition.duration
			#25        0.03333          14                             NA
			#Leave_Home.duration Relax.duration Cook.duration Eat.duration
			#25                  NA             NA            NA           NA
			#Personal_Hygiene.duration FunctionalHealth.duration FunctionalHealth.freqn
			#25                        NA                        36               279.6498
			#################
			ids=names(y)
			#print(ids)			
			#print("happy testing line 112 HHCoor");  # 187 clinical; 216 SH
	    sh.var.changes[[i]] = slidewindowSH(x, slidewindowSH, windowsize= 216, skip= 216, ids,
	                                        last_n_hours=last_n_HOURS,
	                                        first_n_hours=first_n_HOURS,
	                                        win_size_alg1=win_size_ALG,
	                                        detrending=detrending_TP,
	                                        no_hour=no_HOUR)
	    #print("sh.var.changes[[i]] in HHCorr line 134");print(sh.var.changes[[i]]); ## ???? also only have one data on 08/25/15
			#print("happy testing line 119 HHCoor");	
		}
	  #print("sh.var.changes[[i]] in HHCorr line 134");print(sh.var.changes); ## ???? also only have one data on 08/25/15
	  #sleep_durn.change sleep_freq.change bed.change
	  #1                  0                 0                 0          0
	  #2               <NA>              <NA>              <NA>       <NA>
	  #3               <NA>              <NA>              <NA>       <NA>
	  #4               <NA>              <NA>              <NA>       <NA>
	    
		names(sh.var.changes)=HH_TPID	
		##### ??????????????????????  sh.var.changes only have data on 08/25/15
		#print("sh.var.changes"); print(sh.var.changes$'1');  #[1,1]: "08/25/15 00:00:00"
		
		#sh.var.changes$'122'[1,1] = "2013-01"
		#sh.var.changes$'123'[1,1] = "2013-01"
		#sh.var.changes$'123'[2,1] = "2013-07"
		#sh.var.changes$'125'[1,1] = "2013-01"
		#sh.var.changes$'125'[2,1] = "2013-07"

		#print("length of diff.pa at HHCorr_Main.R"); print(length(diff.pa));
		#print("length of sh.var.changes at HHCorr_Main.R"); print(length(sh.var.changes));
		merge.list = mergePsychShChanges(diff.pa,sh.var.changes, xColName="firsttdate", yColName="date");
		
	    
    	#var.list = lapply(merge.list, function(x){sapply(x[,-1], sd)})
		#print(merge.list);
		#-- convert merged variables to data frame 
		#print("testing rbind.data.frame");print(rbind.data.frame);
		#print("merge.list");print(merge.list);  # already same as output. 
		final.df = do.call(rbind.data.frame,merge.list)  # rbind.data.frame is a function
		
		return(final.df)
		

	}	
	#--Count #Changes in SH data 
}

 

#analyze_monthly = function(sh.list){	
#	lsh.monthly = lapply(sh.list, function(x){ by(x, strftime(x[,1],"%Y-%m"),function(x)return(data.frame(identity(x[,-1]))));})	
#	write("I have following number of hours of data",stdout());
#	a=lapply(lsh.monthly,function(x)length(names(x)))
#	print(data.frame(a))
#	write(paste("In average, we have data for X hours, where X = ", mean(as.numeric(a))),stdout());
#	return(lsh.monthly)
#}


analyze_hourly = function(sh.list){	
  lsh.hourly = lapply(sh.list, function(x){ by(x, strftime(x[,1],"%m/%d/%y %H:%M:%S"),function(x)return(data.frame(identity(x[,-1]))));})	
  # strftime(x[, 1], "%m/%d/%y %H:%M:%S"): 08/25/15 00:00:00
  #print("hapy testing line 165 happy testing lien 165 HHCoor_Main.R.")
  #print(lsh.hourly)
	#  $`1`
	#strftime(x[, 1], "%m/%d/%y %H:%M:%S"): 08/25/15 00:00:00
	#  Sleep.duration Sleep.freqn Bed_Toilet_Transition.duration Leave_Home.duration
	#1        0.03333           3                             NA                  NA
	# Relax.duration Cook.duration Eat.duration Personal_Hygiene.duration
	#1        0.01667            NA           NA                        NA

  #print("happt testing line 170 HHCorr_Main.R")
  write("I have following number of hours of data",stdout());
  a=lapply(lsh.hourly,function(x)length(names(x)))
  #print("data.frame(a)"); print(data.frame(a));
  #print("a"); print(a);  [1] "a" $`1` [1] 216
  
  #print("lsh.hourly"); print(lsh.hourly);
  #strftime(x[, 1], "%m/%d/%y %H:%M:%S"): 09/02/15 23:00:00
  #Sleep.duration Sleep.freqn Bed_Toilet_Transition.duration Leave_Home.duration Relax.duration Cook.duration Eat.duration
  #216        0.03333          19                             NA                  NA        0.03333            NA           NA
  #Personal_Hygiene.duration FunctionalHealth.duration FunctionalHealth.freqn
  #216                   0.01667                        60               616.5123
  
  #[1] "data.frame(a)"
  # X1
  # 216
  write(paste("In average, we have data for X hour, where X = ", mean(as.numeric(a))),stdout());
  return(lsh.hourly)
}

saveDF = function(final.df,file_name){	
	csv.path = paste(file_name,"_age.csv",sep="");
	write.csv(final.df,csv.path,na="?",quote=FALSE);
	#just to write in arff append ids
	row.names = rownames(final.df);
	final.df = cbind(row.names,final.df)
	write.arff(final.df,paste(csv.path,".arff",sep=""));
	#write(paste("#","Siglevel = ",SIG_LEVEL ,"ACT=",SH.VAR,"RANDOM=",LIST.IS.RANDOM [RANDOM],sep=" "), csv.path, append=TRUE);
	write(paste("File saved at",csv.path),stdout());
}

main()

