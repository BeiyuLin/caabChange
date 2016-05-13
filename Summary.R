#Read the clinical dataset and generate the summary 


get_summary=function (col){
	col = na.omit(col);
	#print(mean(col))
	#print(sd(col))
	#print(range(col))
	
}



path ="../DataFromIAQBuildDataSheet/FromYiBoPST/UsedToRun/TestProcessedH003_median.csv";
#path ="hh2014.csv";
my.csv  = read.csv(path, na.strings=c("","-1","None"," "),sep=",")
 
no_residents = len(my.csv);
#print(table(gender))



get_summary(my.csv$age)
get_summary(my.csv$edu)






path ="ohsu2014.csv"
my.csv  = read.csv(path, na.strings=c("","-1","None"," "),sep=",")

no_residents = len(my.csv);
print(table(gender))



get_summary(my.csv$age)
get_summary(my.csv$edu)




 