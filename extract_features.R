#Part of the code is based on http://www.early-warning-signals.org/
require(ICSNP);
library(moments);
extract_features=function (timeseries, winsize = 100,#orignianl is 20 
		detrending = c("no", "gaussian", "linear", "first-diff"), 
		bandwidth = NULL,  
    	degree = NULL, 
		logtransform = TRUE,
		na.rm=T,
    	interpolate = F, 
    	AR_n = FALSE, 
		powerspectrum = FALSE,
		plot=FALSE,
		name=""){
    #---remove nas
	#print(timeseries)
  #print("happy testing winsize extract_features.r line 17"); print(winsize);
	
	if(na.rm){
		timeseries = na.omit(timeseries);
	}
	timeseries = data.matrix(timeseries)
	
	#print("timeseries in extract_features.R"); print(timeseries);  ### all the durations. 
	#######
	#Sleep.duration
	#08/25/15 00:00:00        0.03333
	#08/25/15 01:00:00        0.03333
	#08/25/15 02:00:00        0.01667
	#08/25/15 03:00:00        0.03333
	#08/25/15 04:00:00        0.03333
	#08/25/15 05:00:00        0.04167
	######
	#[1] "timeseries in extract_features.R"
	#Sleep.duration
	#08/25/15 00:00:00        0.03333
	#---extract the time signal data
	
	#print("dim(timeseries)[2]");print(dim(timeseries)[2]); # 1 # dim means dimension of the object
	#print("dim(timeseries)[1]");print(1:dim(timeseries)[1]); # 1
    if (dim(timeseries)[2] == 1) {
        Y = timeseries
        timeindex = 1:dim(timeseries)[1]
    }
    else if (dim(timeseries)[2] == 2) {
        Y = timeseries[, 2]
        timeindex = timeseries[, 1]
    }
    else {
        warning("not right format of timeseries input")
    }
    #print("Y"); print(Y)
    #                  Sleep.duration
    #08/25/15 00:00:00        0.03333
    #08/25/15 01:00:00        0.03333
	
	#if(length(timeseries)<25){
	if(length(timeseries)<1){
		print("The length of time series is too low.");
		out = data.frame(NA, NA, 
        		NA, NA, NA, NA,NA)
    	colnames(out) = c("timeindex", "sd", "sk", "kurt", 
        		"acf", "densratio", "ch_ratio") 
    	return(out)
	} 
	
	#--Want to interpolate?,remember with na.rm
    if (interpolate) {
        YY = approx(timeindex, Y, n = length(Y), method = "linear")
        Y = YY$y
    }
	     	
	#--Want log transform?  ## logtransform is TRUE.
    if (logtransform) {
        Y = log(Y + 1)
    }
	
	#--Do some detrending
    detrending = match.arg(detrending)	
    #print("happy testing detrending line 79 extrat_features.R"); print(detrending);  # Guassian. 
    if (detrending == "gaussian") {
        if (is.null(bandwidth)) {
            bw = round(bw.nrd0(timeindex))
        }
        else {
            bw = round(length(Y) * bandwidth/100)
        }
        smYY = ksmooth(timeindex, Y, kernel = "normal", bandwidth = bw, 
            	range.x = range(timeindex), x.points = timeindex)
        nsmY = Y - smYY$y
        smY = smYY$y
    }
    else if (detrending == "linear") {
        nsmY = resid(lm(Y ~ timeindex))
        smY = fitted(lm(Y ~ timeindex))
    }
    else if (detrending == "first-diff") {
        nsmY = diff(Y)
        timeindexdiff = timeindex[1:(length(timeindex) - 1)]
    }
    else if (detrending == "no") {
        smY = Y
        nsmY = Y
    }
	#print(smY);print(nsmY)
	#--Create rolling windows
    mrow =  round(length(as.matrix(Y)) * winsize/100) #if winsize in percentage 
    #print("Y happy testing happy testing"); print(Y); # 08/25/15 00:00:00  to  09/01/15 13:00:00 
    #                  Sleep.duration
    #   08/25/15 00:00:00     0.03278660
    #print("matrix Y happy testing happy testing"); print(matrix(Y));
    #print("mrow");print(mrow); # 219
	  #print("nsmy");print(nsmY)  # 08/25/15 00:00:00 to 09/01/15 13:00:00
    #                 Sleep.duration
    #   08/25/15 00:00:00   3.472103e-03
    #print("nrow(as.matrix(nsmY))"); print(nrow(as.matrix(nsmY)));  #117
	mcol = nrow(as.matrix(nsmY)) - mrow + 1 
    #print()    
    #print("nsmY");print(nsmY);
    
    #Sleep.duration
    #08/25/15 00:00:00   3.472103e-03
    #08/25/15 01:00:00   3.603735e-03
    #print("length(nsmY)");print(nrow(as.matrix(nsmY)));
    #print("mrow"); print(mrow); 
    #print("mcol"); print(mcol);  
	#print(paste("mcol",mcol))
	
    nMR = matrix(data = NA, nrow = mrow, ncol = mcol) #to calculate rest of the metric
	oMR = matrix(data = NA, nrow = mrow, ncol = mcol) #to calculate 2C algorithm
 
    for (i in 1:mcol) {
        Ytw = nsmY[i:(i + mrow - 1)]
        nMR[, i] = Ytw
		oMR[,i]=smY[i:(i + mrow - 1)];
    }
	
	#--Calculate metrics   
    nSD = numeric()
    nSK = numeric()
    nKURT = numeric()
    nACF = numeric()
    
    nSPECT = matrix(0, nrow = mcol, ncol = ncol(nMR))    
	ch_ratio = numeric()
	 
	nSD = apply(nMR, 2, sd, na.rm = TRUE)
	#print("happy testing line 109 extract_features.R");
    for (i in 1:ncol(nMR)) {          	 
        nSK[i] = abs(moments::skewness(nMR[, i], na.rm = TRUE)) 
        nKURT[i] = moments::kurtosis(nMR[, i], na.rm = TRUE) ## although in a loop, but all data is same!!!      
        #print("arc, nMR line 140 extract_features.R'"); print(nMR[, 1]);
        ACF = acf(nMR[, i], lag.max = 1, type = c("correlation"),plot = FALSE); 
        nACF[i] = ACF$acf[2]; 	
		nRow = nrow(oMR);		 
		#split first half as bin1 and second half as bin2
		bin1 = oMR[1:floor(nRow/2),i];
		bin2 = oMR[seq(floor(nRow/2)+1,nRow,1)]
		ch_ratio[i]= cmpHotelling(bin1,bin2) 
	} 
	#print(ch_ratio)
    if(plot){
		plot_func(Y,smY,nsmY,nSD,nACF, nSK,nKURT, ch_ratio,name,detrending,mrow)
	}
	
	out = data.frame(timeindex[mrow:length(nsmY)], nSD, 
        	nSK, nKURT, nACF, ch_ratio)
    colnames(out) = c("timeindex", "sd", "sk", "kurt", 
        	"acf",  "ch_ratio") 
    #print("happy testing out line 169 extract_features.R"); print(out); ## for all SH sensors.
    #timeindex       sd       sk     kurt           acf ch_ratio
    #1         92 1.521223 1.621552 7.189946  0.0628194810        1
    #2         93 1.548508 1.535173 6.673771  0.0533637784        0
    return(out)
}

plot_func =function(Y,smY,nsmY,nSD,nACF, nSK,nKURT,ch_ratio,name,detrending,mw=60){
	timevec = seq(1, length(nSD))   
	timeindex = seq(1,length(Y))
    
    KtACF = cor.test(timevec, nACF, alternative = c("two.sided"), 
        	method = c("kendall"), conf.level = 0.95)
    KtSD = cor.test(timevec, nSD, alternative = c("two.sided"), 
        	method = c("kendall"), conf.level = 0.95)
    KtSK = cor.test(timevec, nSK, alternative = c("two.sided"), 
        	method = c("kendall"), conf.level = 0.95)
    KtKU = cor.test(timevec, nKURT, alternative = c("two.sided"), 
        	method = c("kendall"), conf.level = 0.95)
	#print(timevec);print(ch_ratio)
	KtCH = cor.test(timevec, ch_ratio, alternative = c("two.sided"), 
        	method = c("kendall"), conf.level = 0.95)
 
    
	
	#dev.new()
	#print(name)
	png(paste("/home/pdawadi/Documents/HorizonHouse/new_plots/",name,".png",sep=""));
	par(mar = (c(0, 2, 0, 1) + 0), oma = c(7, 2, 3, 1), mfrow = c(4, 2))   
    plot(timeindex, Y, type = "l", ylab = "", xlab = "", xaxt = "n", 
        	las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
    if (detrending == "gaussian") {
        lines(timeindex, smY, type = "l", ylab = "", xlab = "", 
            	xaxt = "n", col = 2, las = 1, xlim = c(timeindex[1], 
                		timeindex[length(timeindex)]))
    }    
    if (detrending == "no") {
        plot(c(0, 1), c(0, 1), ylab = "", xlab = "", yaxt = "n", 
            	xaxt = "n", type = "n", las = 1)
        text(0.5, 0.5, "no residuals - no detrending")
    }
    else if (detrending == "first-diff") {
        limit = max(c(max(abs(nsmY))))
        plot(timeindexdiff, nsmY, ylab = "", xlab = "", type = "l", 
            	xaxt = "n", las = 1, ylim = c(-limit, limit), xlim = c(timeindexdiff[1], 
                		timeindexdiff[length(timeindexdiff)]))
        legend("topleft", "first-differenced", bty = "n")
    }
    else {
        limit = max(c(max(abs(nsmY))))
        plot(timeindex, nsmY, ylab = "", xlab = "", type = "h", 
            	xaxt = "n", las = 1, ylim = c(-limit, limit), xlim = c(timeindex[1], 
                		timeindex[length(timeindex)]))
        legend("topleft", "residuals", bty = "n")
    }
	 
	#-------PLot ACF trend
    plot(timeindex[mw:length(nsmY)], nACF, ylab = "", xlab = "", 
        	type = "l", xaxt = "n", las = 1, xlim = c(timeindex[1], 
            		timeindex[length(timeindex)]))
    legend("bottomleft", paste("Kendall tau=", round(KtACF$estimate, 
        					digits = 3)), bty = "n")
    legend("topleft", "acf(1)", bty = "n")
	
	 
 
	 
	#-------Plot sd trend    
    plot(timeindex[mw:length(nsmY)], nSD, ylab = "", xlab = "", 
        	type = "l", xaxt = "n", las = 1, xlim = c(timeindex[1], 
            		timeindex[length(timeindex)]))
    legend("bottomleft", paste("Kendall tau=", round(KtSD$estimate, 
        					digits = 3)), bty = "n")
    legend("topleft", "standard deviation", bty = "n")
	 
    #-------Plot Change ratio trend    
    plot(timeindex[mw:length(nsmY)], ch_ratio, ylab = "", xlab = "", 
            type = "l", xaxt = "n", las = 1, xlim = c(timeindex[1], 
                	timeindex[length(timeindex)]))
    legend("bottomleft", paste("Kendall tau=", round(KtCH$estimate, 
            				digits = 3)), bty = "n")	 
    legend("topleft", paste("Change ratio",round(mean(ch_ratio),3)), bty = "n")
    
	#-------Plot skewness Trend
    plot(timeindex[mw:length(nsmY)], nSK, type = "l", ylab = "", 
        	xlab = "", las = 1, cex.lab = 1, xlim = c(timeindex[1], 
            		timeindex[length(timeindex)]))
    legend("topleft", "skewness", bty = "n")
    legend("bottomleft", paste("Kendall tau=", round(KtSK$estimate, 
        					digits = 3)), bty = "n")
    mtext("time", side = 1, line = 2, cex = 0.8)
	 
	#-------Plot kurtosis trend
    plot(timeindex[mw:length(nsmY)], nKURT, type = "l", ylab = "", 
        	xlab = "", las = 1, cex.lab = 1, xlim = c(timeindex[1], 
            		timeindex[length(timeindex)]))
    legend("topleft", "kurtosis", bty = "n")
    legend("bottomleft", paste("Kendall tau=", round(KtKU$estimate, 
        					digits = 3)), bty = "n")
    mtext("time", side = 1, line = 2, cex = 0.8)
    	
	mtext(paste( "Activity ",name), side = 3, line = 0.2, outer = TRUE)
     
     
dev.off()
}

cmpHotelling  = function(bin1, bin2){
	bin1=as.matrix(bin1);bin2=as.matrix(bin2)
	#print(bin1);print(bin2)
	pval=F;
	#print("hoteeling")
	#print(bin1);print(bin2)
	result = tryCatch({
				test = HotellingsT2( as.matrix(bin1), as.matrix(bin2),test="f");
				pval = test$p.value <SIG_LEVEL	
			},error = function(w) {
				#print("errror in hotelling");
				#print(error)
				#print(paste("MY_ERROR:  ",error))
				 
			})
	
	return(pval)		
}

 