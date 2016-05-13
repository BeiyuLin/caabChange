__author__ = "Prafulla Dawadi"
 
from numpy.core.numeric import seterr 
from SegmentFile import FileSegmentation;
from ActivityDensity import ActDensity;
import numpy;
import numpy as np;
import math;
import re;
import sys;
import optparse;
import os;  
import re;
import datetime; 
from multiprocessing import Pool; 
import tempfile;     
import matplotlib.pyplot as plt 

#rsync -avruz /home/pdawadi/Dropbox/eclipse/workspace/python/ProjectCorr/*    pdawadi@node01.ailab.wsu.edu:/net/files/home/pdawadi/MyResearch/HHCorrelation/ProjectCorr_New

#rsync -avruz pdawadi@node01.ailab.wsu.edu:/net/files/home/pdawadi/MyResearch/HHCorrelation/ProjectCorr_New/act_features  /home/pdawadi/Documents/new_actfeatures
 
#Input: AR annotated sensor data 
#       Remember to extract mobility(Functional health) based features, we need sensor data with motion sensor id (M001 etc, I ignore area sensors) 
#Output: Duration/ #Sensor for the activity 

#Reminder : Make sure that you have changed hhXX.pos file in ActivityDensity.py. NOTE the 
 
#Extract activity features from activity annotated files 
#Extract mobility features from raw files


class ActivityFeatures:   
    currentActivityName=None;
    actDensity = ActDensity();
      
    def _get_datetime(self,str_date,str_time):
        #print str_date,str_time
        str_d=str_date + " " + str_time;
        parts = str_d.split('.');
        dt = datetime.datetime.strptime(parts[0], "%Y-%m-%d %H:%M:%S")
        return dt;
            
    def _getStartTime(self,startLine):         
        act_start=startLine.split(" ");         
        return act_start[0] +"  "+act_start[1];
            
    def _generateHeader(self,actName):
        if actName == "FunctionalHealth":
             header=["day","sensorfreqn","actdensity"];
        else:
            #header=["day","TotalDuration","MinimumDuration","MaximumDuration","MedianDuration","AverageDuration","freqn","count","starttime"];
            header=["day","MedianDuration","freqn","count","starttime"];
        s=",".join(header);
        return s;
    
    def _getTotalDuration(self,actStream): #for bed toilet transition  margin = 1 hr 
            totDuration=0;
            count = 0;        
            durlist = []; 
            for i in range(0,len(actStream)-2):
                #print actStream[i]
                firstLine = actStream[i].split(" ");
                secondLine =actStream[i+1].split(" ");
                #get a date from here
                d1= self._get_datetime(firstLine[0],firstLine[1]);
                d2=self._get_datetime(secondLine[0],secondLine[1]);
                td= d2-d1;
                duration =td.total_seconds();
                #print td, duration 
                #durlist.append(duration)
                margin = self._calculateMargin(d1,d2);
                if duration > 60*margin:              
                    #check to see if there were other activities, 
                    count=count+1;
                    continue;
                durlist.append(duration)
                totDuration=duration+totDuration;   
            try:
                #print round(min(durlist)/3600, 2), round(max(durlist)/3600, 2), round(totDuration/3600,2), round(sum(durlist)/3600, 2)
                #return (round(totDuration/60,5), count, round(numpy.min(durlist)/60, 5), round(numpy.max(durlist)/60, 5), round(numpy.median(durlist)/60,5), round(numpy.average(durlist)/60, 5));
                return (round(numpy.median(durlist)/60,5), count);
            except ValueError:
                #return (round(totDuration/60,5),count, 0, 0, 0, 0);
                return (round(numpy.median(durlist)/60,5), count);
    
    def _getTotalSensorEvents(self,senStream,FH=False): 
            #count number of ON events              
            if FH:
                #bound_regex="M\d+\sON";   #This regex ignores Motion Area Sensors bound_regex="M\d+\sON";   
                bound_regex="\sON";
            else:
                #bound_regex="M\d+\sON"; 
                bound_regex="\sON"; 
                    
            sensorFreq= len(re.findall(bound_regex,str(senStream)));           
            return sensorFreq;     
    
    #see if the activities are recognized under wide time interval 
    #if so do not include them while calculating the duration
    def _calculateMargin(self,d1,d2):      
        margin=2;
        if(self.currentActivityName=="Bed_Toilet_Transition" or self.currentActivityName=="Work"):
            margin = 1;
        elif (self.currentActivityName =="Sleep"): #instead hard coding, there must be some automated way of doing 
                d1clock8Pm=datetime.datetime(d2.year,d2.month,d2.day,20,00,00);  d1clock8Am=datetime.datetime(d2.year,d2.month,d2.day,7,00,00);
                d2clock8pm=datetime.datetime(d2.year,d2.month,d2.day,20,00,00);  d2clock8Am=datetime.datetime(d2.year,d2.month,d2.day,7,00,00);
                #ignore 6 hrs of interruptions in sleep after 9 else 1 hour  
                #criteria for greate than 9 0 clock 
                #after 12 PM
                if(d1.time()>datetime.time(12,00,00)):
                    if d1 < d1clock8Pm and d2 < d1clock8Pm:                                   
                        margin = 1; 
                    elif d1> d1clock8Pm and d2  >d1clock8Pm:                  
                        margin=6;                
                    else :
                        margin = 1;                        
                       # print " Less than 00 Calculating Margin for sleep. This block execute when sleep instances are at wide time interval";
                       
                #Before 12 P
                else:
                    if d1 < d1clock8Am and d2 < d1clock8Am:                                         
                        margin = 6;#print margin; 
                    elif d1> d1clock8Am and d2  >d1clock8Am:                         
                        margin=1;             #if there exists two sleep activities 1 hours apart during afternon , do not include them while calculating durations      
                    else :
                        margin = 1;                        
                        #print " Greater than 00 Calculating Margin for sleep.This block execute when sleep instances are at wide time interval";
        else:
            margin=0.5; 
        return margin;
    
    def _extractActFeatures(self,ts_segment):         
        duration = [];  
        if  (len(ts_segment[1])>1): 
            list_ts= ts_segment[1].split("\n")                  
            duration = self._getTotalDuration(list_ts);
            freqn = self._getTotalSensorEvents(ts_segment[1]);
            startTime = self._getStartTime(list_ts[0]);    
            features=str(ts_segment[0])+","+str(duration[0])+"," +str(freqn)+","+str(duration[1])+","+str(startTime);
            #features=str(ts_segment[0])+","+str(duration[0])+"," +str(duration[2])+","+str(duration[3])+","+str(duration[4])+","+str(duration[5])+"," +str(freqn)+","+str(duration[1])+","+str(startTime);
        else:
            #features=str(ts_segment[0])+","+str("NA")+"," +str("NA")+","+str("NA")+","+str("NA")+str("NA")+"," +str("NA")+","+str("NA")+","+str("NA");        
            features=str(ts_segment[0])+","+str("NA")+"," +str("NA")+","+str("NA")+","+str("NA");  
        return features;
                
    #functional health = mobility in the paper
    def _extractFunctionalHealthFeatures(self,ts_segment,hhid):
        if  len(ts_segment[1])>1:  
             #extract activity density, motion sensor count  
            freqn = self._getTotalSensorEvents(ts_segment[1],FH=True);
            #print "HAPPY TESTING line 149 Activities.py HAPPY TESTING HAPPY TESTING"
            #print ts_segment[1] 
           
            # read the hh.pos files
            self.actDensity._readSensorLocations(hhid);
            actDenFeat = self.actDensity._getHHActDensity(ts_segment[1]);    
            
            features=str(ts_segment[0])+","+str(freqn)+"," +str(actDenFeat) ;            
        else :             
            features=str(ts_segment[0])+","+str("NA")+"," +str("NA") ; 
            
        return features;
            

def extractFeatures( read_path,writePath, hhid,beginTime="00:00:00.000000",debug=False,activityName="Toilet",margin=4,):
         # Steps
         #1. Isolate an acitivity from original annotations 
         #2. Save it
         #3. segment the activity isolated file and calculate the features 
         plotlist = []
         features = []
         tempfeaturesDraw = []
         tempfeaturesDrawMedian = []
         
         print "Start....";          
         print("Extracting features for the activity " + activityName +" for " + read_path)
         actFeatures = ActivityFeatures(); 
         actFeatures.currentActivityName=activityName;        
         seg_file = FileSegmentation(); 
         seg_file._openfile(read_path);
         
         #Step1 : Parse the specific activity in the sensor data, store it in the separate file
         if activityName == "FunctionalHealth":
            act_instances=seg_file._readAll();   
            act_data = "".join(act_instances);                 
         else:
             act_instances=seg_file._segment_activity(name=activityName); 
             act_data = "\n".join(act_instances);   
             if(activityName =="Sleep"):
                  beginTime=  "12:00:00.000000";
             
                   
         if len(act_instances)==0 :
            print "Instances of activity: " + activityName + " not found."
            return;
         
          
         hh_id = '%s_%s'%(hhid,activityName)
         temp = tempfile.NamedTemporaryFile(prefix=hh_id, suffix='.txt', dir='/tmp', delete=False)
 
         temp_path = temp.name
         

         temp_file = open(temp_path,"w");
         temp_file.write(act_data);
         temp_file.close();
         
         seg_file.s_file=act_data;
         seg_str=[0,0,1,0,0,0,0] # sec min hr day week month year         
         seg_file.set_seg_Structure(seg_str); 
     
         
         seg_file._openfile(temp_path);
         ts_segment=seg_file._segment_file_time(beginTime); 
         #os.remove(temp_path); #do some exception handling
        
         writeFile = open(writePath,'w');
         writeFile.write(actFeatures._generateHeader(activityName));
         writeFile.write("\n");
         day =0; 
         
         
         #Step2 : Load the data stored in step1 to extract features 
         if activityName == "FunctionalHealth":  
                print "****************Make sure that you have specified the location of *.pos file**********"
         while ts_segment is not None:  
            #print "happy testing line 217 Activities.py"               
            if activityName == "FunctionalHealth":              
                #features.append(actFeatures._extractFunctionalHealthFeatures(ts_segment,hhid));
                features= actFeatures._extractFunctionalHealthFeatures(ts_segment,hhid);
            else:
                features = actFeatures._extractActFeatures(ts_segment); 
                #features.append(actFeatures._extractActFeatures(ts_segment)); 
                
                #tempfeatures = features.split(",")
                #print features
                tempfeatures = re.split(r",", features);
                #print features, tempfeatures
                #if (tempfeatures[4] in  ("NA", "NANA")):
                #    tempfeatures[4] = 0 #MedianDuration: 4; AverageDuration: 5
                if (tempfeatures[0] in  ("NA", "NANA")):
                    tempfeatures[0] = 0 #MedianDuration: 4; AverageDuration: 5
                if (tempfeatures[1] in  ("NA", "NANA")):
                    tempfeatures[1] = 0 #MedianDuration: 4; AverageDuration: 5
                #if (tempfeatures[5] in  ("NA", "NANA")):
                #    tempfeatures[5] = 0 #MedianDuration: 4; AverageDuration: 5
                #tempfeaturesDraw.append(float(tempfeatures[1]));
                #tempfeaturesDayTest = re.split(r" ", tempfeatures[0])
                
                #tempfeaturesDrawMedian.append(float(tempfeatures[4]))
                #tempfeaturesDraw.append(float(tempfeatures[5]));

                #if (tempfeaturesDayTest[0] in ("2015-08-06", "2015-08-07", "2015-08-08", "2015-08-09", "2015-08-10", "2015-08-11", "2015-08-12", "2015-08-12", "2015-08-13", "2015-08-14", "2015-08-15", "2015-08-16", "2015-08-17", "2015-08-18", "2015-08-19")):
                #    tempfeaturesDraw.append(float(tempfeatures[1]));
                #    tempfeaturesDrawMedian.append(float(tempfeatures[4]))
            #print tempfeatures[0]    
            #["day","TotalDuration","MinimumDuration","MaximumDuration","MedianDuration","AverageDuration","freqn","count","starttime"];
            #['2015-07-13 12:00:00', '23.99', '0.0', '0.81', '0.00056', '0.01115', '0', '0', '2015-07-12  12:00:23.991862']
            #print tempfeatures[4] 
            #print "happy testing line 230 Activities.py"
            
            writeFile.write(str(features));
            writeFile.write("\n"); 
            ts_segment=seg_file._segment_file_time(beginTime);
         #print tempfeaturesDrawMedian
         #print tempfeaturesDraw
         
         #print features[0], tempfeatures[0]
         #n, bins, patches = plt.hist(tempfeaturesDraw, 5, normed = 1, facecolor = 'g', alpha = 0.5)
         #plt.plot(tempfeatures[4])
         #plt.show()
         #print features 


def getID( FILE_PATH):    
    #regex = re.compile("hh\d+")
    regex = re.compile("atmo\d+")
    #print FILE_PATH
    #PID= regex.findall(FILE_PATH)[0];
    PID= regex.findall(FILE_PATH)[0];
    print PID
    return PID;         

#deprecated 
'''def main123():
    
    read_path="/home/pdawadi/Documents/new_actfeatures/file/hh101.txt";
    writepath = "/home/pdawadi/Documents/new_actfeatures/"
     
     
    actList = ["","Sleep",
                  "Bed_Toilet_Transition",
                  "Relax",
                  "Leave_Home",
                  "Cook",
                  "Eat",
                  "Personal_Hygiene",
                  "FunctionalHealth"];
                   
    print "Calculating Activity features"
  
   
    
    print "Reading sensor files from " +read_path;
    
    if os.path.isdir(read_path):
            paths = [os.path.join(read_path,fn) for fn in next(os.walk(read_path))[2]];
    else:
        paths =[read_path];
            
    print paths;
    for p in paths:            
        hh_id = getID(p);
        print "processing..",hh_id
        for i in np.arange(1,9):            
            actname = actList[i];            
            write_path=writepath+"/" +hh_id +"."+ actname;                        
            _extractFeatures(p, write_path,hhid = hh_id, activityName=actname);
   
    print "Output is written at "+write_path;'''
     
   
 
   
         


    
 
   
        

 
