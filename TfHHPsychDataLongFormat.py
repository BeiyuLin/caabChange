'''
Created : 20 March 2013
Edited  : 7 May 2013
Edits : Refined the list of the variables that we will be using 
Edited : 7 July 2013
Edits : Added overall health and overall changes and tug trial in the long format data

Read Maureen's psych data file, select  chosen psych variables  for Horizon House Smart Home Longtd data analysis 
Stores the  data in long format 

 List of Psych variables

        Name                                                         Var Name
1.  General Cognitive Status                                      tXrbansistor
2.  Self Report of Everyday Functioning                           tXWSUIADL 
3.  Depression                                                    tXGDStotal 
4.  Performance Based Functional Abilities Measure                tXMMAAtotal
5.  Functional Assessment Questionnaire                           tXWSUIALDtot
6.  Timed Up and Go test                                          tXTUGtrial2

7.  Total Days below Par                                          mXdaysbelowpar
8.  Total days cut down on activities                             moXdecreasedact
9.  Overall Health                                                moXOverallhealth
10. Overall change                                                mMoXoverallchanges
11. Physical Mobility                                             mOx_PHYSICAL_MOBILITY
12. Memory                                                        moXHR_memory                
13. Medication Change                                             moXhr_medicationchange
14. Sleep Quality                                                 moXHR._sleep_quality
15. Health Interference Activities                                moXhr_healthinferenceactivities
16. General Health                                                moXhr_generalhealth
 

COMMENTS for original HH psych data : 
1. I have renamed t1testdate1 with  Time_1_date and Mo1Startdate with Mo1Healthdate to be  consistent with 
the rest of the date labels for for bi-annual data and monthly self reported data 
2. For all the variables, Month 7 was wrongly named I have renamed it correctly Mo\d+Hr#4_think_clearly 
'''

import re
import numpy as np;
#import matplotlib.pyplot as plt;
#from pdb import set_trace as brake; 
import csv;
 
#readfile = "/net/files/home/pdawadi/Dropbox/Research/Longitudinal Data Analysis/HH smart home data/Total data_Merged datasetFinal.csv"; #this one is old location 
#readfile = "/home/pdawadi/Dropbox/Research/HHCorrelation/HHData/HHdatabase8-2013.csv"; #this one is new location 
#readfile ="/Users/cook/home/projects/students/prafulla/caab/ProjectCorr/PsychData/HHdatabase4-11-2014.csv"
#writefile ="/Users/cook/home/projects/students/prafulla/caab/ProjectCorr/PsychData/hhtest.csv"

readfile="/Volumes/Seagate Backup Plus Drive/IAQ/caabChange/ProjectCorr/IndoorAirQuality/atmoCopyfromHH.csv"
writefile="/Volumes/Seagate Backup Plus Drive/IAQ/caabChange/ProjectCorr/IndoorAirQuality/TestAtmoCopyfromHH.csv"
def getSingleColumn(id, colName):     
    findObj = re.findall(colName,csvheader,re.IGNORECASE);
    #print findObj;
    cnt=0;
    mydata = np.empty(len(findObj),dtype="object");
    for f in findObj:         
        #print "f= " , f, "colname=" ,colName,"id=",id;
        mydata[cnt]  = psychdata[id][f];
        #print mydata;
        #if "this month" in psychdata[id][f] :
            #brake();      
        cnt=cnt+1;
    #print mydata;
    return mydata;
    #transform this data and return 
     
#bi annual
fheadersPA = ["id", 
            "ttimepoint", 
            "monthtdate",
            "biannualtdate",
            "age",
            "edu",
            "gender",
            "rbansistor", 
            "rbansisimmmem",
            "rbansisviscon",
            "rbansislang",
            "rbansisattn",
            "rbansisdelmem",
            "WSUIADL",   
            "DepStotal",
            "MMAAtotal",  
            "WSUIALDtot",
            "tugtrial",
            "bmi",
            "speedprocessing",
            "prospmemory",
            "tempordermemory",
          ]

          

hregexPA = [#PA data
          "Mo\d+Healthdate",
          "Time_\d+_date",
          "t1age",
          "t1edu",
          "t1gender",
          "t\d+rbansistot",     #ok #all 5 measures of rbans
          "t\d+rbansisimmmem",
          "t\d+rbansisviscon",
          "t\d+rbansislang",
          "t\d+rbansisattn",
          "t\d+rbansisdelmem",
          "t\d+WSUIADLtot",     #?not ok 
          "t\d+GDStotal",       #ok
          "t\d+MMAAtotal",      #ok
          "t\d+WSUIADLtot",    #?not ok variable has been renamed
          "t\d+TUGtrial2",    #?
          "t\d+BMI", #bmi 
          "t\d+DC47tottime", #speeded processing
          "t\d+TORBANSpmtot",#prospective memory
          "t\d+TORBANSTOcorr", # temporal order memory 
          ]

fheadersSR=["id", 
            "ttimepoint", 
            "monthtdate",
           
          #Mood           
           "feel_depressed",
           "mood",
           
        #Cognitive Health  
            "memory",
            "think_clearly",
          "remember_things",
          
          #Health 
          "physical_mobility",
          "general_health",
          
           #Activity
          "daily_activities",
          "engage_daily_activities",
          
          #Sleep
          "sleep_quality",
          "average_fatigue",
          
           #Other 
          "fallen",
          "medication_change",
          
          "overallhealth",
          "overallchanges",
          
     ]     

hregexSR = [#PA data
          "Mo\d+Healthdate",
         
          #Mood
          "Mo\d+HR_#12_feel_depressed",#{1-24, 7 is misnamed} 
          "Mo\d+HR_#11_mood", #{1-24 , 7 is misnamed}
          
          # Health  
          "Mo\d+HR.3_memory",             # {1-24,  7 is misnamed}
          "Mo\d+Hr#4_think_clearly",  #{1-24 , 7 is misnamed}
          "Mo\d+HR#5_remember_things", #{1-24 , 7 is misnamed}
          
          #Health 
          "Mo\d+HR.2_physical_mobiltiy", #{1-24 , 7 is misnamed}
          "Mo\d+HR_#15_general_health",  #{ONly two available Mo1 and Mo7}
          
          #Activity 
          "Mo\d+Hr.7_daily_Activities",           #{1-24, 7 is misnamed as 
          "Mo\d+Hr#8_engage_daily_Activities",  #{1-24, 7 is misnamed}
          
          #Sleep
          "Mo\d+HR.9_sleep_quality",        # {1-24},7 is misnamed as Mo7Health#9_sleep_quality
          "Mo\d+HR_#10_average_fatigue",   # {1-24 , 7 is misnamed as   Mo7Health_#10_average_fatigue   
           
              
          #Other  
          "Mo\d+Hr#1a_Fallen",#{1-27, 7 is misnamed as Mo7Health#1a_Fallen}
          "Mo\d+HR#6_medication_change", #{1-24,7 misnamed as Mo7Health#6_medication_change others are named as Mo6HR#6_medication_change }
          
          #General Health
          "Mo\d+overallhealth",
          "Mo\d+overallchanges",
          
                   
          ]
 #"Mo\d+Health_.14_health_interfere_activities", 
fheaders=["id","time","var","vs"]
hregex=['[a-z]\d*Var',"[a-z]\d*Vs"]

#Replace the variables here based on if you want to convert clinical data or  self report data
fheaders = fheadersPA;  # replace with fheadersSR for self-report
hregex =hregexPA;       # replace with hregexSR  for self-report

#read CSV file
#Numpy genfrom txt gave me weird error while reading the new file format
#use file reader to create a numpy rec array
psychdata=None;
with open(readfile,'r') as f:
    d_header = f.readline().strip();
    csvheader = d_header; 
   
    name=d_header.split(",");
    print "aaaaaaaaaaaaaaaaaaaaaaaaa"
    #print name
    print "aaaaaaaaaaaaaaaaaaaaaaaaaaa"
    
    name = [entr.strip() for entr in name];
    print "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    #print name[0]
    #print name
    print "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    dtpstr=['|S25']*len(name)

    dtpe=zip(name,dtpstr);
    psychdata = np.array(np.empty(len(name)),dtype=dtpe);
    
    d_data = f.readline().strip()
    cnt=1;
    while d_data:
        #print "count %d"%cnt
        d_data_sp = d_data.split(","); 
        print "d_data_sp test test test test test test"
        print d_data_sp
        print "d_data_sp test test test test test test"
        
        name1 = [entr.strip() for entr in d_data_sp];  
        print "cccccccccccccccccccccccccccc"
        print name1[0]     
        print "cccccccccccccccccccccccccccc"
        psychdata.resize(cnt);# 
        psychdata[cnt-1] = np.rec.fromarrays(name1,dtype=dtpe)#np.array(d_data_sp,dtype=dtpe)
        cnt=cnt+1;
        d_data = f.readline().strip();
        
hhid = psychdata['ID'];
print hhid;
print "TEST LEN HHID, TEST LEN HHID, TEST LEN HHID,TEST LEN HHID!!!!!!!!!!"
print len(hhid)
#print hhid
lghhdata = np.empty((len(hhid)),dtype="object");  #stores data in long format 
print "TEST lghhdata, lghhdata, lghhdata, lghhdata, lghhdata !!!!!!!!!!!!!"
print lghhdata

tempdata = [None]*len(hregex); #  stores arrays corresponding to entries in header 

for id in  np.arange(0,len(hhid)):
     cnt=0;
     for rgx in hregex: 
        tempdata[cnt]=getSingleColumn(id,rgx); #get a column of data( an entry in header)
        #a=  tempdata[cnt];
        cnt=cnt+1;      
     lghhdata[id]=tempdata;  
      
     tempdata = [None]*len(hregex);
    
#store lghhdata(longformat) to a file
 
with open(writefile,'w') as f:
    
    print hhid;
    print >>f, ",".join(fheaders); #write header
    cnt=0;    
    for lst in lghhdata:       
        print "test lst lst lst lst lst"
        print lst
        mrglist = map(None, *lst);
       # mrglist = map(lambda *row:list(row), *lst);  # get nth elements from all the columns 
        listsize = [len(l) for l in lst]
        print "test listsize listsize listsize listsize listsize listsize "
        print listsize
        maxsize = max(listsize);    #get the maximum time
        for time in np.arange(0,maxsize):
            print "test time time time time time time "
            print time
            newlist = mrglist[time];   # ????? split the row by timepoint: t1, t2, ... t6 ???            
            print>>f,str(hhid[cnt]),",",str(time+1),",", str(newlist).strip('[]') # write each entries  in file 
        cnt=cnt+1;
        print "cnt cnt cnt cnt cnt cnt cnt cnt!!!!"
        print cnt

print "Transformation Complete";
print "File saved at ",writefile;


             
'''
They have renamed many variables on the  new dataset Sep 2013. Here are the list of manual edits I did. 
1: Rename t1testdate1 to Time_1_date 
2. -1 for NAN
3. Ingore MMAA score  at time point 1 for participant 100-118  
4. Do the following steps after  you generate the longitudinal file : 
   a. Replace ' ' ( ) with space
   b. replace none with space
   c. -1 indicates missing values, replace it with space
   d. Remove all the MMAA scores at time point 1 
   e. Remove timepoint 1 for 113, it was an outlier
'''
'''
Newer file format 
1. ID of the HH participants starts with 100+. Remove other ids. 
2.Replace #Null! with None
'''

    
    
    
    
