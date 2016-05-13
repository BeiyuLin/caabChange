slidewindowPA = function(pa.df,windowsize=2,skip = 1){
	col_ignore = c(1,2)#,3,4)#c(1)  !
	#print("test pa.df[1,] JPACompareCount.R")
	#print(pa.df[1,])
	#print("test inside of SlideWindwoPA SlideWindwoPA SlideWindwoPA !!!!!")
	no.cols= (ncol(pa.df) -length(col_ignore))*3 + length(col_ignore) 
	#print(pa.df)
	#print("test pa.df pa.df pa.df pa.df pa.df pa.df pa.df pa.df !!!!!")
	len.df  =  nrow(data.frame(pa.df));#number of available months for comparison	
	#print(len.df);
	if(windowsize>len.df){    
		#print(pa.df)
		print(paste("Warning : Window size ",windowsize, " is greater or equal to  data length",len.df));	 
		return(NA);
	}
	
	ptrfirst  = seq(1, (len.df - windowsize+1) ,by=skip) 
	ptrsec  = ptrfirst + (windowsize-1);		
	
	result  = data.frame(matrix(0,nrow=length(ptrfirst),ncol=no.cols));
	#--column names are differences in scores, first score, second score
	colnames(result) = c("firsttdate",#"age","sex","edu","gender",
			paste(colnames(pa.df[,-col_ignore]),".diff",sep=""),
			paste(colnames(pa.df[,-col_ignore]),".first",sep=""),
			paste(colnames(pa.df[,-col_ignore]),".second",sep=""));

	#--ignore a row if all elements are na 
	if(all(is.na(pa.df)))
		pa.df = na.omit(pa.df);
    #print(pa.df)
	#print("happy test JPACompareCount line 33 JPACompareCount line 33 !!!")
	#print("test ptrfirst");
	#print(ptrfirst)
	for( i in 1:length(ptrfirst)){		 
	    #diff.df   =  pa.df[ptrsec[i],-col_ignore]  - pa.df[ptrfirst[i],-col_ignore];
	  #print("happy test lINE 34 JPACompareCount")
	  #print(pa.df[ptrsec[i],-col_ignore]);
	  #print(pa.df[1,-col_ignore])
	  #print(pa.df[ptrsec[i],-col_ignore]  - pa.df[1,-col_ignore]);
		diff.df   =  pa.df[ptrsec[i],-col_ignore]  - pa.df[1,-col_ignore];
		first.df = pa.df[ptrfirst[i],-col_ignore];
		second.df  =  pa.df[ptrsec[i],-col_ignore];		
		#print(diff.df);print(first.df);print(second.df)
	   	#the first column is date so append and return it			   
	    result[i,] = c(as.character(pa.df[ptrfirst[i],col_ignore]), #always put the first date so as to properly align sh and pa data
				diff.df,
				first.df,
				second.df);
		
		#print(result)
	}
	
    return(result);
}

slidewindowSH  = function(var.list, windowsize, skip,rm.outlier,pidx=0,
						last_n_hours,
						first_n_hours,
						win_size_alg1,
						detrending,
						no_hour){ 
	
	len.df  =   length(var.list);	   # 216
  #print("var.list is ****************** line 64 JPACompareCount.R");
	#print(var.list)
	#strftime(x[, 1], "%m/%d/%y %H:%M:%S"): 09/02/15 23:00:00
	#Sleep.duration Sleep.freqn Bed_Toilet_Transition.duration Leave_Home.duration Relax.duration Cook.duration
	#216        0.03333          19                             NA                  NA        0.03333            NA
	#Eat.duration Personal_Hygiene.duration FunctionalHealth.duration FunctionalHealth.freqn
	#216           NA                   0.01667                        60               616.5123
	
	col.names = c("date",sh.change,sh.sk,sh.acf,sh.var,sh.kurt) 
	
	#print("windowsize");print(windowsize);  # 216
	#print("len.df");print(len.df);   # 216
	if(windowsize > len.df){# 
		print("i am here5");
		result  = data.frame(matrix(0,nrow=1,ncol=length(col.names)));	  
    	#print(result);
		
		print(paste("Window size ",windowsize, " is greater or equal to  data length ", len.df));
		result[1,] = c(names(var.list[1]),(rep(NA,length(col.names))));#
		#quick fix (DATE,SLP.CHANGE,ADL.CHANGE,FH.CHANGE) TO DO
		
		colnames(result) =col.names#; c("date","slp.change","fh.change","adl.change","freq.change","slp.ent","fh.ent","adl.ent","freq.ent");
		return(result); 
		
	}
	print("len.df-windowsize+1"); print(len.df); print(windowsize);
	ptrfirst  = seq(1, len.df - windowsize+1 ,by=skip); #start of the window 
	ptrsec  = ptrfirst + (windowsize-1);		        #end of the window
	result  = NULL; 
	print("ptrfirst");print(ptrfirst);
	#print("happy tesitng ptrfirst line 87 JPACompareCount.R");print(ptrfirst);
	#print(ptrsec);
	cnt=1;#only take last 6 smonth of data
	for( i in 1:length(ptrfirst)){  #187
		idx = ptrfirst[i]:ptrsec[i];	 
    	print("data is available for these months ");
		  print(idx);
		win.list = var.list[idx]; 
		print("happy testing line 101 win.list JPAC.r"); print(length(win.list));
		#print("happy testing win.list line 101 JPACompareCount.R"); print(win.list); ## win.list is complete. Nothing missed here.
		#print("happy testing length of win.list line 101 JPACompareCount.R"); print(length(win.list));  # 187
		#print("happy testing line 103 JPACompareCount.R win.lis[1]"); print(win.list[2]);
		
		#print(length(win.list))
		#if(length(win.list)>8){
			#only use last 6 months of data 
		#	cnt = length(win.list) - 6;		
		#}		 

		#print (first_n_hours) # FALSE
		#print("first_n_hours line 110 JPACompareCount.R");print(first_n_hours)
		#print("last_n_hours line 110 JPACompareCount.R");print(last_n_hours)
		
		if( first_n_hours){
			print("Activating last_n_hours mode")
			use.idx = cnt:(length(win.list)-no_hour)
			print(paste("I am using data from these months"));
			print("happy testing Line 103 JPACompareCount.R");
			print(idx[use.idx]);
			
			print(paste("I am using last N months of data, N= ", length(use.idx)));
			res = find_sh_changes( win.list[use.idx],
					i,pidx,
					detrend=detrending,
					win_size_alg=win_size_alg1);
		}
		else if (last_n_hours){
			print("Activating first_n_hours mode") 
			use.idx = (length(win.list)-no_hour):(length(win.list))
			print(paste("I am using data from these months")); print(idx[use.idx]);
		    print(paste("I am using first N months of data, N = ", length(use.idx)))
			res = find_sh_changes( win.list[use.idx],
					i,pidx,
					detrend=detrending,
					win_size_alg=win_size_alg1);
		}
		else{ ## only this case. Ignore the if and elseif. 
		  #print("happy testing line 127 JPACompareCount.R");
			res = find_sh_changes( win.list[cnt:length(win.list)],i,pidx,
					detrend=detrending,
					win_size_alg=win_size_alg1);
		}
		#print("win.list[2]"); print(win.list[2]);
		#$`08/25/15 01:00:00`
		#Sleep.duration Sleep.freqn Bed_Toilet_Transition.duration Leave_Home.duration
		#2        0.03333          12                        0.03333                  NA
		#Relax.duration Cook.duration Eat.duration Personal_Hygiene.duration
		#2             NA            NA           NA                        NA
		#FunctionalHealth.duration FunctionalHealth.freqn
		#2                        34               214.2947
		
	
	  #tempL = dim(win.list);
	  #tempC= c(names(win.list[1]));
	  #for (i in 1: tempL){
	 #   tempC = c(tempC, c(names(win.list[1])));
	  #}
	  
	  #newrow =c(tempC , res);	 #add same prafulla
		tmpLength = 50;
		newrow = c();
		for( i in 1:tmpLength){
		  resstart = (i-1)*tmpLength + 1;
		  temprow = c(names(win.list[i]) , res[resstart:(resstart+tmpLength)]);	 #add same 	08/25/15 00:00:00 
		  newrow = c(c(newrow), c(temprow));
		}
		#print("happy testing line 169 JPAC.r ")  # for loop is okay.
		#print("res");print(res);
		#print("names(win.list[1])");print(names(win.list[1]));
		#[1] "names(win.list[1])"
		#[1] "08/25/15 00:00:00"
		#print("newrow"); print(newrow);
		
		if(is.null(result)){	 #for the first time			  
		  result  = data.frame(matrix(0,nrow=length(216),ncol=(length(newrow))));
		  #result  = data.frame(matrix(0,nrow=length(ptrfirst),ncol=(length(res)+1)));
			#colnames(result) = c("date",names(res));
		  colnames(result) = c(names(newrow));
		}  			 
		print("happy testing line 180 JPAC.r ")
		print("result dim"); print(dim(result[i,]));
		print("newrow"); print(length(newrow));
		
	  #print("col.names"); print(length(col.names));print(col.names);  # 51 
		#print("names of new row"); print(names(newrow));
		#print("result[i,]"); print(result[i,]);
		
		#print(newrow)
		#result[i,] =  newrow;		 
		result[i,] =  newrow;		 
		
	}
	#print(result)
	print("-------------------------------------------------------------------");
	return(result);
}



find_sh_changes = function(sh.list,idx,pidx,detrend,win_size_alg){		
	#Step 1 :each element in  a list is one month  so merge them
	#print("happy testing Line 155 rbind.data.frame sh.list JPACompareCount.R"); print(sh.list);  
	data.df = do.call(rbind.data.frame,sh.list);  # row combine
	#print("rbind.data.frame"); print(rbind.data.frame); ## rbind.data.frame is a function.
	#print("data.df");print(data.df) ## all the raw SH data with durations.
	colnames(data.df) = colnames(sh.list[[1]])  
	#print("happy testing colnames(sh.list[[1]])"); print(colnames(sh.list[[1]])); ## all durations:
	##################
	#[1] "Sleep.duration"                 "Sleep.freqn"                   
	#[3] "Bed_Toilet_Transition.duration" "Leave_Home.duration"           
	#[5] "Relax.duration"                 "Cook.duration"                 
	#[7] "Eat.duration"                   "Personal_Hygiene.duration"     
	#[9] "FunctionalHealth.duration"      "FunctionalHealth.freqn"  
	#################
	#print("happy testing Line 156 data.df JPACompareCount.R"); print(data.df);  
	#apply the algorithm to each coloumns
	#print("happy testing line 162 data.df[1] JPACompareCount.R"); print(data.df[1]);  # all data for slepp duration.
	#print("happy testing line 162 data.df[1]-data.df[10] JPACompareCount.R");print(data.df);
	### note: windowsize= 187, skip= 187 decide how many lines would be output. data.df is the raw SH data. 
	# print("happy testing length of data.df[1], line 203 JPAC.r"); print(length(data.df[,1]));#216
	# print(data.df[,2]);# 216 data for Sleep.freqn
	#3 12  2 11 50 18  3 10  9 23 
	df1 = extract_features(data.df[1],  detrending=detrend,winsize=win_size_alg,
			name=paste(pidx,colnames(data.df[1]),"-",idx))
	print("happy testing df1 line 172 JPACompareCount.R");print(df1);  # 6 which means 6 columns
	print("happy testing length of the number of rows in df1");print(length(df1[,1]));  # 68
	## but why the timeindex is number instead of time.
	df2 = extract_features(data.df[2],  detrending=detrend,winsize=win_size_alg,
			name=paste(pidx,colnames(data.df[2]),"-",idx))
	#print("happy testing df1 line 172 JPACompareCount.R");print(df2);  ## but why the timeindex is number instead of time.
	df3 = extract_features(data.df[3],  detrending=detrend,winsize=win_size_alg,
			name=paste(pidx,colnames(data.df[3]),"-",idx))
	df4 = extract_features(data.df[4],  detrending=detrend,winsize=win_size_alg,
			name=paste(pidx,colnames(data.df[4]),"-",idx))
	df5 = extract_features(data.df[5],  detrending=detrend,winsize=win_size_alg,
			name=paste(pidx,colnames(data.df[5]),"-",idx))
	df6 = extract_features(data.df[6],  detrending=detrend,winsize=win_size_alg,
			name=paste(pidx,colnames(data.df[6]),"-",idx))
	df7 = extract_features(data.df[7],  detrending=detrend,winsize=win_size_alg,
			name=paste(pidx,colnames(data.df[7]),"-",idx))		
	df8 = extract_features(data.df[8],  detrending=detrend,winsize=win_size_alg,
			name=paste(pidx,colnames(data.df[8]),"-",idx))
	df9 = extract_features(data.df[9],  detrending=detrend,winsize=win_size_alg,
			name=paste(pidx,colnames(data.df[9]),"-",idx))
	df10 = extract_features(data.df[10], detrending=detrend,winsize=win_size_alg,
			name=paste(pidx,colnames(data.df[10]),"-",idx))
	#df10=df9;
	 #print(df1)
	 #print(str(df1))
	# print("mean is")
	 #print(mean(df1$acf));
	#print(data.df)
	  
	#print("df1$ch_ratio df1$ch_ratio"); print(df1$ch_ratio);
	# ch.props =c(mean(df1$ch_ratio),mean(df2$ch_ratio),
	# 		 mean(df3$ch_ratio),mean(df4$ch_ratio),mean(df5$ch_ratio),mean(df6$ch_ratio),
	# 		 mean(df7$ch_ratio),mean(df8$ch_ratio),mean(df9$ch_ratio),mean(df10$ch_ratio));
	# 
	#  acf.props=c(mean(df1$acf),mean(df2$acf),mean(df3$acf),mean(df4$acf),mean(df5$acf),mean(df6$acf),mean(df7$acf),mean(df8$acf),mean(df9$acf),mean(df10$acf));
	#  sk.props=c(mean(df1$sk),mean(df2$sk),mean(df3$sk),mean(df4$sk),mean(df5$sk),mean(df6$sk),mean(df7$sk),mean(df8$sk),mean(df9$sk),mean(df10$sk));
	#  var.props =c(mean(df1$sd),mean(df2$sd),mean(df3$sd),mean(df4$sd),mean(df5$sd),mean(df6$sd),mean(df7$sd),mean(df8$sd),mean(df9$sd),mean(df10$sd));
	#  print("var.props happy testing lien 240 JPAC.r");print(var.props)
	#  kurt.props =c(mean(df1$kurt),mean(df2$kurt),mean(df3$kurt),mean(df4$kurt),mean(df5$kurt),mean(df6$kurt),mean(df7$kurt),mean(df8$kurt),mean(df9$kurt),mean(df10$kurt));
	# f_results = c(as.vector(ch.props),as.vector(acf.props),as.vector(sk.props),as.vector(var.props),as.vector(kurt.props));
	# print("Happy testing line 243 JPAC.R f_results before names"); print(f_results);
	ch.props =c(df1$ch_ratio,df2$ch_ratio, df3$ch_ratio,df4$ch_ratio,df5$ch_ratio,df6$ch_ratio, df7$ch_ratio,df8$ch_ratio,df9$ch_ratio,df10$ch_ratio);
	# print("ch.props line 223 JPACompareCount.R"); print(ch.props);
	
	acf.props=c(df1$acf,df2$acf,df3$acf,df4$acf,df5$acf,df6$acf,df7$acf,df8$acf,df9$acf,df10$acf);
	sk.props=c(df1$sk,df2$sk,df3$sk,df4$sk,df5$sk,df6$sk,df7$sk,df8$sk,df9$sk,df10$sk);
	var.props =c(df1$sd,df2$sd,df3$sd,df4$sd,df5$sd,df6$sd,df7$sd,df8$sd,df9$sd,df10$sd);
	#print(var.props)
	kurt.props =c(df1$kurt,df2$kurt,df3$kurt,df4$kurt,df5$kurt,df6$kurt,df7$kurt,df8$kurt,df9$kurt,df10$kurt);
	
	 f_results = c(as.vector(ch.props),as.vector(acf.props),as.vector(sk.props),as.vector(var.props),as.vector(kurt.props));
	 
	 Ttimeindex = 1:dim(data.df[1])[1];
	 #print("Ttimeindex"); print(Ttimeindex);
	 tempC = c();
	 tempC = c(sh.change,sh.acf,sh.sk,sh.var,sh.kurt);
	#print("original length of tempC"); print(length(tempC));
	#print("(length(ch.props) -1)/100"); print((length(ch.props) -1)/100);  # 4.9 
	#print("length(ch.props) -1)"); print(length(ch.props) -1);
	 for (i in seq(1, (length(ch.props)-10)/10, by = 1)){
	   tempC = c(c(tempC), c(sh.change,sh.acf,sh.sk,sh.var,sh.kurt));  #2450
	 }
	print("length for ch.props, acf.props,sk.props, ar.props"); print(c(length(ch.props), length(acf.props), length(sk.props), length(var.props)));  # all four is 493
	print("total length of f_results:"); print(length(ch.props) + length(acf.props) + length(sk.props) + length(var.props));  ## total of 5 is 2465
	#print("as.vector(acf.props)"); print(as.vector(acf.props));
	#print("f_result"); print(f_results);
	 #print("tempC"); print(tempC)
	#print("length of tempC"); print(length(tempC));  # 2450
	#print("tempC"); print(tempC);
	#print("length of r_results"); print(length(f_results));  # 2465
  #print("f_results[1]");print(f_results[5:10][1:5]); print(c(f_results[1], f_results[2], f_results[3])); ## always 1
	 names(f_results) = tempC;
	
	#print(sh.acf)
	#[1] "sleep_durn.acf"       "sleep_freq.acf"       "bed.acf"             
	#[4] "leave_home.acf"       "relax.acf"            "cook.acf"            
	#[7] "eat.acf"              "personal_hygiene.acf" "mobility.durn.acf"   
	#[10] "mobility.freqn.acf" 
	#print("sh.acf[1]");print(sh.acf[1:5]);  #"sleep_durn.acf"
	names(f_results) = c(c(tempC), c(sh.change,sh.acf[1:5]));  #2465
  #print("length(names(f_results))"); print(length(names(f_results)));

	# names(f_results)=c(sh.change,sh.acf,sh.sk,sh.var,sh.kurt)
	
	# print("happy testing lien 211 JPACompareCount.R"); print(f_results); # result is only for one day: 08/25/15 00:00:00
	 #sleep_durn.acf          sleep_freq.acf                 bed.acf 
	 #5.0277656791            5.0379753154            5.0383561348 
	 # leave_home.acf               relax.acf 
	 # 5.0356868253            5.3378762932     ### 2465 
	 #print("happy testing line 166 df1 JPACompareCount.R"); print(df1);
	 #print("f_results is list or not"); print(is.matrix(f_results));  ## not list, not matrix.
	 #print(length(f_results)/5);
	return(f_results);
}

 

 
