slidewindowPA = function(pa.df,windowsize=2,skip = 1){
	
	no.cols= (ncol(pa.df) -1)*3 + 1
	#print(pa.df)
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
	colnames(result) = c("firsttdate",colnames(pa.df[,-1]),
			paste(colnames(pa.df[,-1]),".first",sep=""),
			paste(colnames(pa.df[,-1]),".second",sep=""));
	
	#--ignore a row if all elements are na 
	if(all(is.na(pa.df)))
		pa.df = na.omit(pa.df);
	
 
	
	for( i in 1:length(ptrfirst)){		 
	    diff.df   =  pa.df[ptrsec[i],-1]  - pa.df[ptrfirst[i],-1];
		first.df = pa.df[ptrfirst[i],-1];
		second.df  =  pa.df[ptrsec[i],-1];		
		#print(diff.df);print(first.df);print(second.df)
	   	#the first column is date so append and return it			   
	    result[i,] = c(as.character(pa.df[ptrfirst[i],1]), #always put the first date so as to properly align sh and pa data
				diff.df,
				first.df,
				second.df);
	}
	#print(result)
    return(result);
}

slidewindowSH  = function(var.list, windowsize, skip,rm.outlier,pidx=0,
						last_n_months,
						first_n_months,
						win_size_alg1,
						detrending,
						no_month){ 
	
	len.df  =   length(var.list);	 
	col.names = c("date",sh.change,sh.sk,sh.acf,sh.var,sh.kurt) 
	
	if(windowsize > len.df){# 
		print("i am here5");
		result  = data.frame(matrix(0,nrow=1,ncol=length(col.names)));	  
    	print(result);
		
		print(paste("Window size ",windowsize, " is greater or equal to  data length ", len.df));
		result[1,] = c(names(var.list[1]),(rep(NA,length(col.names))));#
		#quick fix (DATE,SLP.CHANGE,ADL.CHANGE,FH.CHANGE) TO DO
		
		colnames(result) =col.names#; c("date","slp.change","fh.change","adl.change","freq.change","slp.ent","fh.ent","adl.ent","freq.ent");
	    
		return(result); 
		
	}
	
	ptrfirst  = seq(1, len.df - windowsize+1 ,by=skip); #start of the window 
	ptrsec  = ptrfirst + (windowsize-1);		        #end of the window
	result  = NULL; 
	
	print(ptrfirst);
	print(ptrsec);
	cnt=1;#only take last 6 smonth of data
	for( i in 1:length(ptrfirst)){
		idx = ptrfirst[i]:ptrsec[i];	 
    	print("data is available for these months ");print(idx)
		win.list = var.list[idx]   
		 
		 
		if(length(win.list)>8){
			#only use last 6 months of data 
			print("i am using last six months of data");
			cnt = length(win.list) - 6;			 
		}		 
		 
		if( first_n_months){
			print("Activating last_n_months mode")
			use.idx = cnt:(length(win.list)-no_month)
			print(paste("I am using data from these months")); print(idx[use.idx]);			
			print(paste("I am using last N months of data, N= ", length(use.idx)))
			res = find_sh_changes( win.list[use.idx],
					i,pidx,
					detrend=detrending,
					win_size_alg=win_size_alg1);
		}
		else if (last_n_months){
			print("Activating first_n_months mode") 
			use.idx = (length(win.list)-no_month):(length(win.list))
			print(paste("I am using data from these months")); print(idx[use.idx]);
		    print(paste("I am using first N months of data, N = ", length(use.idx)))
			res = find_sh_changes( win.list[use.idx],
					i,pidx,
					detrend=detrending,
					win_size_alg=win_size_alg1);
		}
		else{
			res = find_sh_changes( win.list[cnt:length(win.list)],i,pidx,
					detrend=detrending,
					win_size_alg=win_size_alg1);
			
			}
		newrow =c(names(win.list[1]) , res);	 #and same 	
		if(is.null(result)){	 #for the first time			  
		    result  = data.frame(matrix(0,nrow=length(ptrfirst),ncol=(length(res)+1)));
			colnames(result) = c("date",names(res));
		}  			 
		
		result[i,] =  newrow;		 
	}
	print("-------------------------------------------------------------------");
    return(result);
}



find_sh_changes = function(sh.list,idx,pidx,detrend,win_size_alg){		
	 
	#Step 1 :each element in  a list is one month  so merge them
	data.df = do.call(rbind.data.frame,sh.list);
	 
	#print(data.df)
	colnames(data.df) = colnames(sh.list[[1]])    
	#print(data.df)  
	#apply the algorithm to each coloumns
	df1 = extract_features(data.df[1],  detrending=detrend,winsize=win_size_alg,
			name=paste(pidx,colnames(data.df[1]),"-",idx))
	df2 = extract_features(data.df[2],  detrending=detrend,winsize=win_size_alg,
			name=paste(pidx,colnames(data.df[2]),"-",idx))
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
	#df10 = extract_features(data.df[10], detrending=detrend,winsize=win_size_alg,
	#		name=paste(pidx,colnames(data.df[10]),"-",idx))
	df10=df9;
	 #print(df1)
	 #print(str(df1))
	# print("mean is")
	 #print(mean(df1$acf));
	#print(data.df)
	
	
	
	 ch.props =c(mean(df1$ch_ratio),mean(df2$ch_ratio),
			 mean(df3$ch_ratio),mean(df4$ch_ratio),mean(df5$ch_ratio),mean(df6$ch_ratio),
			 mean(df7$ch_ratio),mean(df8$ch_ratio),mean(df9$ch_ratio),mean(df10$ch_ratio));
	 
	 
	 acf.props=c(mean(df1$acf),mean(df2$acf),mean(df3$acf),mean(df4$acf),mean(df5$acf),mean(df6$acf),mean(df7$acf),mean(df8$acf),mean(df9$acf),mean(df10$acf));
	 sk.props=c(mean(df1$sk),mean(df2$sk),mean(df3$sk),mean(df4$sk),mean(df5$sk),mean(df6$sk),mean(df7$sk),mean(df8$sk),mean(df9$sk),mean(df10$sk));
	 var.props =c(mean(df1$sd),mean(df2$sd),mean(df3$sd),mean(df4$sd),mean(df5$sd),mean(df6$sd),mean(df7$sd),mean(df8$sd),mean(df9$sd),mean(df10$sd));
	 #print(var.props)
	 kurt.props =c(mean(df1$kurt),mean(df2$kurt),mean(df3$kurt),mean(df4$kurt),mean(df5$kurt),mean(df6$kurt),mean(df7$kurt),mean(df8$kurt),mean(df9$kurt),mean(df10$kurt));
	
	 
	 f_results = c(as.vector(ch.props),as.vector(acf.props),as.vector(sk.props),as.vector(var.props),as.vector(kurt.props));
	 
	 
	 names(f_results) = c(sh.change,sh.acf,sh.sk,sh.var,sh.kurt);
	 #print(f_results);
	return(f_results);
}

 

 
