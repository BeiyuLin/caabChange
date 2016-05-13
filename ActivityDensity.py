
#Calculates the activity density of an individual for a day

#Algorithm to calculate Total Activity Density in Smart Home
 

#This is python port for Dr.Cooks activity density calculation with minor changes 
from numpy.core.numeric import seterr
import numpy as np;
import math;
import re;
import sys;
import optparse;
import os;  
import datetime; 
import random; 

class ActDensity :
    id;
    msXY=dict(); #Motion sensor X, Y position   
    RAND_MAX = 1000;
    RADIUS=10;
    #locPath = "/Users/cook/home/projects/students/prafulla/caab/ProjectCorr/positions/"
    locPath="/Volumes/Seagate Backup Plus Drive/IAQ/caabChange/ProjectCorr/postions/"
    def __init__(self):
        random.seed(100);
              
    '''2012-05-25 15:22:52.506142 Kitchen Stove M013 ON Other_Activity
2012-05-25 15:22:52.907977 DiningRoom DiningRoom M014 ON Other_Activity
2012-05-25 15:22:55.885346 DiningRoom DiningRoom M014 OFF Other_Activity
2012-05-25 15:22:56.12333 Kitchen Stove M013 OFF Cooks
2012-05-25 15:22:58.840788 Kitchen Kitchen MA002 OFF Cook
2012-05-25 15:23:04.36729 Ignore Ignore LS012 38 Cook
2012-05-25 15:23:04.426799 LivingRoom LivingRoom MA012 OFF Other_Activity
2012-05-25 15:23:05.133619 LivingRoom LivingRoom MA012 ON Other_Activity
2012-05-25 15:23:10.22671 LivingRoom LivingRoom MA012 OFF Other_Activity
2012-05-25 15:23:13.203269 LivingRoom LivingRoom MA012 ON Other_Activity

    ''' 

        
                
    def _getHHActDensity(self,eventList):
        #print eventList;
        #sensor_idx=4;
        #sensor_status_idx=5;
        sensor_idx=2;
        sensor_status_idx=3;
        senSep=None ;prevDate= None; prevLoc=None;FIRST=True; totdist=0;prevSen=None;
        eventList = eventList.split("\n");
        evLen =len(eventList);
        #print(eventList)
        for senEv in eventList:      
            if len(senEv)>0:
                senEv = senEv.split(" ");       # TO DO instead of splitting,  use regex, giving me trouble due to different formats of file 
                
                #if ("OFF" in senEv[sensor_status_idx]) or ("MA" in senEv[sensor_idx]): #ignore area sensors and events with OFF 
                #print "Happy Testing line 58 ActivityDensity"
                #print senEv
                #print "Happy Testing line 58 ActivityDensity" 
                #Happy Testing line 58 ActivityDensity
                #['2015-08-01', '23:24:34.449898', 'KitchenADiningChair', 'OFF', 'Cook']
                if ("OFF" in senEv[sensor_status_idx]) or ("Area" in senEv[sensor_idx]): #ignore area sensors and events with OFF  
                    #print " Okay Okay Okay Okay Okay Happy Testing line 61 ActivityDensity Okay Okay Okay Okay Okay" 
                    continue;    
                
                #if("M" not in senEv[sensor_idx]): #ignore all other sensors
                    #continue;
                if(("OFF" not in senEv[sensor_status_idx]) and ("ON" not in senEv[sensor_status_idx])): #ignore all other sensors
                    continue;
                
                if FIRST:
                    prevLoc=self._getRandomResLoc(senEv[sensor_idx]);  
                    prevSen = senEv[sensor_idx];
                    FIRST=False;             
                else:
                    resLoc = self._getRandomResLoc(senEv[sensor_idx]);
                    if resLoc is None:
                        continue;
                    #print "Distance between" + prevSen+"   "+ senEv[1]+ " is " +curdist;
                    curdist= self._calculateDistance(resLoc,prevLoc);
                    #print "Distance between " + prevSen+"   "+ senEv[1]+ " is " + str(curdist);
                    prevSen = senEv[sensor_idx];
                    #   print "Distance between" + resloc+"   "+ prevLoc+ " is " 
                    totdist=totdist+curdist;
                    prevLoc=resLoc;
                    #print("-----------------------------------------------------------------------");
        #print(totdist);
        return totdist;
    
    
    def _getActDensity(self,eventList): 
       
        totdist = self._getHHActDensity(eventList);
            
        return totdist;
    
    def _readSensorLocations(self,hhid):
        
           
        #This id is obtained based on the id from the name of sensor data file
        locFileLocal = self.locPath +hhid + ".pos";      
        locFile = open(locFileLocal,"r");
        locLine="";locSep=" ";
        locLine = locFile.readline();
        while(len(locLine)>2):      
            coord = locLine.split(locSep,3);   
            #print "Happy Testing line 97 ActivityDensity"
            #print coord, coord[0];    
            #print "Happy Testing line 97 ActivityDensity"
            if "Area" in coord[0]:     #ignore area sensors 
            #if "MA" in coord[0]:     #ignore area sensors 
                locLine = locFile.readline();
                continue; 
            self.msXY[coord[0]] = (float(coord[1]),float(coord[2]));             
            locLine = locFile.readline();
        locFile.close(); 
        #print self.msXY;                                            
    
    
    def _calculateDistance(self,cord1,cord2):
        if cord1 is None:
           return 0;
        if cord2 is None :
             return 0;       
        #cord1 = self.msXY[ms1];
        #cord2= self.msXY[ms2];
        return math.sqrt((cord2[0] - cord1[0])*(cord2[0] - cord1[0])  + 
                    (cord2[1]-cord1[1])*(cord2[1]-cord1[1]) );
    
    def _getRandomResLoc(self,ms1):
         if self.msXY.has_key(ms1):
             xy = self.msXY[ms1];
         else :
            # this means that we have encountered unknown motion sensor  while calculating activity density, might happen
            #when hhpos file does not contain the motion sensor id
            return None; #the sensor coordinate is not available
           
         #print xy;
         cx = xy[0];cy=xy[1];         
         #print(cx,cy);         
         r =   random.random()/self.RAND_MAX  ;
         tx = (cx - self.RADIUS) + (r * 2.0 * self.RADIUS);
         r = random.random()/self.RAND_MAX  ;
         ty = (cy - self.RADIUS) + (r * 2.0 * self.RADIUS);
         # Look for point inside circle
         while ((((tx - cx) * (tx - cx)) + ((ty - cy) * (ty - cy))) >=
            (self.RADIUS * self.RADIUS)):
                r = random.random()#/self.RAND_MAX  ;
                tx = (cx - self.RADIUS) + (r * 2.0 * self.RADIUS);
                r = random.random()  ;
                ty = (cy - self.RADIUS) + (r * 2.0 * self.RADIUS);    
                #print "rand"                    

         #print(cx,cy);
         #print(tx,ty);
         return (tx,ty);
        
         
def main():
    print "Reading path.."
    #path = "/home/pdawadi/Documents/HorizonHouse/sample_ohsu/OHSU-926.txt"; 
    #path=""    ????????????????
    
    actDens = ActDensity();
    #actDens._readSensorLocations("h101",True);
    actDens._readSensorLocations("atmo1",True);
    #read the file 
    s_file=open(path,"r");      
    txt= s_file.readlines();
    #s_file.close();
    txt= "".join(txt);
    dist = actDens._getActDensity(txt,True);
    print(txt);
    print(dist);
    
    
if __name__=="__main__":  
    main();
