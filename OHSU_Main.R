source("JLoadData.R")
source("JConst.R")
source("OHSUPACompareCount.R")
source("extract_features.R")
require(foreign)

#rsync -avruz   pdawadi@node01.ailab.wsu.edu:/net/files/home/pdawadi/MyResearch/HHCorrelation/OHSU_features_new/   /home/pdawadi/Documents/HorizonHouse/OHSU_features_new/  


main=function(){
	first_exp();
	#early_pred();
	#trend_fit_pred();
	#min_no_month_pred();
	
}

first_exp = function(){
	win_size = seq(20,80,10)
	for(w in win_size){
		my.df = run_alg(last_n_MONTHS=FALSE,
				first_n_MONTHS=FALSE,
				win_size_ALG = w,
				detrending_TP="gaussian",
				no_MONTH=0)
		saveDF(my.df,paste("OHSU_dataset/OHSU_RegularExp_winsize_",w,"_trend_gaussian",sep=""));
	}
}

early_pred = function(){
	early_p = c(1,2,3,4,5);
	win_size = seq(25,100,10);#generate window sizes at interval of 5
	
	for (w in win_size){
		for (m in early_p){		
			my.df = run_alg(last_n_MONTHS=FALSE,
					first_n_MONTHS=TRUE,
					win_size_ALG = w,
					detrending_TP="gaussian",
					no_MONTH=m)
	    	saveDF(my.df,paste("OHSU_dataset/early_pred/early_pred_",m,"_winsize_",w,sep=""));
		}
	}
}


trend_fit_pred = function(){
	win_size = seq(20,100,10);#generate window sizes at interval of 5
	for (w in win_size){
		my.df = run_alg(last_n_MONTHS=FALSE,
				first_n_MONTHS=FALSE,
				win_size_ALG=w,
				detrending_TP="gaussian",
				no_MONTH=0)
		
	    saveDF(my.df,paste("OHSU_dataset/OHSU_trend_gauss","_winsize_",w,sep=""));
	}
	
	for (w in win_size){
		my.df=run_alg(last_n_MONTHS=FALSE,
				first_n_MONTHS=FALSE,
				win_size_ALG=w,
				detrending_TP="linear",
				no_MONTH=0)
		saveDF(my.df,paste("OHSU_dataset/OHSU_trend_linear","_winsize_",w,sep=""));
	}
	
	
	for (w in win_size){
		my.df = run_alg(last_n_MONTHS=FALSE,
				first_n_MONTHS=FALSE,
				win_size_ALG=w,
				detrending_TP="no",
				no_MONTH=0)
	    saveDF(my.df,paste("OHSU_dataset/OHSU_trend_none","_winsize_",w,sep=""));
	}
	
	
	
}

min_no_month_pred = function(){
	min_month = c(1,2,3,4,5);
	win_size = seq(20,100,10);#
	
	for (w in win_size){	
		for (m in min_month){
			my.df = run_alg(last_n_MONTHS=TRUE,
					first_n_MONTHS=FALSE,
					win_size_ALG = w,
					detrending_TP="gaussian",
					no_MONTH=m)
	    	saveDF(my.df,paste("OHSU_dataset/min_no/min_no_",m,"_winsize_",w,sep="")) 
		}
	}
	
	
	
}

 

run_alg=function(last_n_MONTHS,
		first_n_MONTHS,
		win_size_ALG,
		detrending_TP,
		no_MONTH){
	#--Load the PA data pwd
	pa.data = OHSU_Data();		 
	pa.list = by(pa.data,pa.data$id,identity);	
    print("Dfdf");
	print(pa.list)
	

	diff.pa= lapply(pa.list,function(x){slidewindowPA(x[,-c(1,2)]);}) 
	#print(diff.pa)	
	#---Load SH data	 
	sh.list = getShData(OHSU=TRUE);#of those id that  have  been listed
	#print(sh.list)	
	#print(diff.pa)
	lapply(sh.list,function(x){apply(x,2,function(y){mean(as.numeric(y),na.rm=TRUE)})})
	lsh.monthly = analyze_monthly(sh.list)	
	new_lsh.monthly=list();
	new_diff.pa=list();
	for (i in 1:length(lsh.monthly)){
		print(paste("processing ",names(lsh.monthly[i])));
		resulting_list = alignSH_ClinicalMonths(lsh.monthly[[i]],diff.pa[[i]]);
		new_lsh.monthly[[i]] =resulting_list[[1]];
		new_diff.pa[[i]] = resulting_list[[2]];
   } 
   names(new_lsh.monthly) =names(lsh.monthly);
   names(new_diff.pa) =names(diff.pa);
   sh.var.changes=list()
	for(i  in 1: length(lsh.monthly)){
		print(i)
	    y=new_lsh.monthly[i]
		x=new_lsh.monthly[[i]]			 
		ids=names(y)
		#print(ids)	
	    #print(x)
		#print(x)
	    sh.var.changes[[i]] = slidewindowSH(x, slidewindowSH, windowsize=12,skip=1,ids,
				last_n_months=last_n_MONTHS,
				first_n_months=first_n_MONTHS,
				win_size_alg1=win_size_ALG,
				detrending=detrending_TP,
		    	no_month=no_MONTH)
	}
	names(sh.var.changes)= ANN_OHSU_TPID	
	
	print( new_diff.pa)
	print(sh.var.changes)
	print("dfdf")
	print("-------------------")
	#print(sh.var.changes)
	merge.list = mergePsychShChanges(new_diff.pa,sh.var.changes ,xColName="firsttdate",yColName="date");
	
	
    #var.list = lapply(merge.list, function(x){sapply(x[,-1], sd)})
	#print(merge.list);
	#-- convert merged variables to data frame 
	final.df = do.call(rbind.data.frame,merge.list)
	
    return(final.df)
	
}

alignSH_ClinicalMonths = function(sh.monthly,lcl.monthly){
	lcl.monthly = data.frame(lcl.monthly)
	#print(lcl.monthly)
	#print(nrow(lcl.monthly))
	#print(lcl.monthly[4,])
	 
	#print(f_sh.month);
	new_sh.monthly=list();
	new_lcl.monthly=NA; 
	for (i in 1:nrow(lcl.monthly)){
		#print(i)
		#print(lcl.monthly[i,])
		f_cl.month =lcl.monthly[i,1];
		#print("another ");print(f_cl.month)
		for (j in 1:length(sh.monthly)){
			#print("sh.monthly");print(names(sh.monthly[j]))
			if (f_cl.month == names(sh.monthly[j])){
				new_sh.monthly = sh.monthly[j : length(sh.monthly)];
				new_lcl.monthly = lcl.monthly[i : nrow(lcl.monthly),];
				#print("--")
				return(list(new_sh.monthly,new_lcl.monthly));	
			}
		  
		}		
	}
	
}

check_all = function(sh.list){
	lapply(sh.list, function(x){print(mean(x$Sleep.duration,na.rm=T));print(var(x$Sleep.duration, na.rm=T))})
}
analyze_all=function(sh.list){
	for(i in 1:length(sh.list)){
		x=sh.list[i]	
		data.df = do.call(rbind.data.frame,x)
		#print(colnames(data.df))	 
		#print(data.df)
		pidx=names(x)
		print(paste("processing P",pidx))
		#print(data.df[2])
		
		
		df3 = apply(data.df[2],2,extract_features,detrending="gaussian",name=paste(pidx,colnames(data.df[2])))
		df4 = extract_features(as.matrix(data.df[3]),detrending="gaussian",name=paste(pidx,colnames(data.df[3])))
		df5 = apply(data.df[4],2,extract_features,detrending="gaussian",name=paste(pidx,colnames(data.df[4])))
		df6 = apply(data.df[5],2,extract_features,detrending="gaussian",name=paste(pidx,colnames(data.df[5])))
		df7 = apply(data.df[6],2,extract_features,detrending="gaussian",name=paste(pidx,colnames(data.df[6])))
		df8 = apply(data.df[7],2,extract_features,detrending="gaussian",name=paste(pidx,colnames(data.df[7])))		
		df9 = apply(data.df[8],2,extract_features,detrending="gaussian",name=paste(pidx,colnames(data.df[8])))	 
		df10 = apply(data.df[9],2,extract_features,detrending="gaussian",name=paste(pidx,colnames(data.df[9])))	 
		df11 = apply(data.df[10],2,extract_features,detrending="gaussian",name=paste(pidx,colnames(data.df[10])))
	    df12 = apply(data.df[11],2,extract_features,detrending="gaussian",name=paste(pidx,colnames(data.df[11])))
	}
	
}


analyze_monthly = function(sh.list){	
	lsh.monthly = lapply(sh.list, function(x){ by(x, strftime(x[,1],"%Y-%m"),function(x)return(data.frame(identity(x[,-1]))));})	
	write("I have following number of months of data",stdout());
	a=lapply(lsh.monthly,function(x)length(names(x)))
	print(data.frame(a))
	write(paste("In average, we have data for X months, where X = ", mean(as.numeric(a))),stdout());
	return(lsh.monthly)
}

saveDF = function(final.df,file_name){	
	csv.path = paste(file_name,".csv",sep="");
	
	write.csv(final.df,csv.path,na="?",quote=FALSE);
	#just to write in arff append ids
	row.names = rownames(final.df);
	final.df = cbind(row.names,final.df)
	write.arff(final.df,paste(csv.path,".arff",sep=""));
	#write(paste("#","Siglevel = ",SIG_LEVEL ,"ACT=",SH.VAR,"RANDOM=",LIST.IS.RANDOM [RANDOM],sep=" "), csv.path, append=TRUE);
	
	write(paste("File saved at",csv.path),stdout());
	
}


main()