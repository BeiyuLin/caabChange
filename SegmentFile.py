#!/usr/bin/env python
import datetime;
import re;
from datetime import timedelta;
import time;
from sets import Set;
import os;
import sys;
from numpy.core.numeric import seterr

__author__ = "Prafulla Dawadi"
__copyright__ = "Copyright 2011, CASAS Lab@WSU"
__license__ = "GNU"
__version__ = "0.0.1"
__maintainer__ = "Prafulla Dawadi "
__email__ = "pdawadi at eecs dot wsu dot edu"
__status__ = "Development"
__comment__ ="Segment dataset by annotated activity/ by time(e.g. 24 hr segment)";


class FileSegmentation :
    s_file=None;
    filePos=0;
    seg_type="time";
    #for segmenation prupose 
    last_date=datetime.MINYEAR;
    
    
    sec=0;minute=0;hour=0;day=0;week=0;month =0;year=0;
    
     
    def _openfile(self, path):
        self.s_file=open(path,"r");      
        
    def _readAll(self):         
            return self.s_file.readlines();
    def _closeFile(self):
        self.s_file.close();
        
    def _segment_file_time(self,startTime):
        seg_line="";
        #ignore first few lines           
        line =self.s_file.readline();      
        if len(line)<1 : 
            return None;        
        #time span logic
        sp_line=line.split(" ");
        #print "happy testing SegmentFile.py Line 49 self.last_date"
        #print self.last_date
        ##### happy testing SegmentFile.py Line 49 self.last_date
        ##### 2015-08-09 12:00:00

        if self.last_date==datetime.MINYEAR:             
            d_time = self._get_datetime(sp_line[0],startTime);
            next_date = d_time;
            #print "happy testing SegmentFile.py line 56 d_time, sp_line[0], startTime, sp_line[1]"
            #print d_time, "happy testing", sp_line[0], "happy testing", startTime, "happy testing", sp_line[1]
            #print "sp_line", sp_line
            #2015-06-10 12:00:00 happy testing 2015-06-10 happy testing 12:00:00.000000 happy testing 17:32:27.46826
            #sp_line ['2015-06-10', '17:32:27.46826', 'BedroomABed', 'OFF', 'Sleep\n']
            
            self.last_date = next_date;  ############ need change to next_hour 
            #print "next_date", next_date
            #next_date 2015-06-10 12:00:00
        else:
            # create a time span 
            #t_delta =datetime.timedelta(days=self.day,seconds=self.sec,hours=self.hour,minutes=self.minute,weeks=self.week)
            #### happy testing SegmentFile.py line 68 t_delta, next_date, self.last_date
            #### 1 day, 0:00:00 happy testing 2015-07-20 12:00:00 happy testing 2015-07-20 12:00:00
            #### happy testing SegmentFile.py line 68 t_delta, next_date, self.last_date
            #### 1 day, 0:00:00 happy testing 2015-07-21 12:00:00 happy testing 2015-07-21 12:00:00
            t_delta =datetime.timedelta(hours=1)
            #### happy testing SegmentFile.py line 68 t_delta, next_date, self.last_date
            #### 1:00:00 happy testing 2015-08-08 23:00:00 happy testing 2015-08-08 23:00:00
            #### happy testing SegmentFile.py line 68 t_delta, next_date, self.last_date
            #### 1:00:00 happy testing 2015-08-09 00:00:00 happy testing 2015-08-09 00:00:00
            #### happy testing SegmentFile.py line 68 t_delta, next_date, self.last_date
            #### 1:00:00 happy testing 2015-08-09 01:00:00 happy testing 2015-08-09 01:00:00
            
            #generate a time segment till this period
            next_date=self.last_date + t_delta;   
            #print "testing self.last_date before update", self.last_date
            self.last_date = next_date;  
            #print "happy testing SegmentFile.py line 68 t_delta, next_date, self.last_date"
            #print t_delta, "happy testing", next_date

        #segmentation logic     
        while line is not None:
            if len(line)<1:
                break;                 
            sp_line= line.split(" ");
            if len(sp_line)>1:
                #print line;
                new_date=self._get_datetime(sp_line[0],sp_line[1]);
                if new_date is None :
                    line =self.s_file.readline();
                    continue;
                if new_date<next_date:#only for day                  
                    seg_line+= line; 
                else:                  
                    #correction for one extra line read     
                    self.s_file.seek(self.filePos);             
                    return (next_date,seg_line);
                
            self.filePos=self.s_file.tell();
            line =self.s_file.readline();
             
        return (next_date,seg_line);
   
           
    def _get_datetime(self,str_date,str_time):

        str_d=str_date + " " + str_time;
        if "." in str_d :
            parts = str_d.split('.')[0];    
            if (len(re.findall("\D\D\d\d\d",parts))>0): #ignore lines for which splits are improper
                return None;
        elif len(re.findall("\D\D\d\d\d",str_d))>0: #the the split is improper(has sensor id), ignore it 
            print "Ignoring line "+ str_d
            dt=None;
            return dt;
        else:
            parts=str_d;
        try: 
            dt = datetime.datetime.strptime(parts, "%Y-%m-%d %H:%M:%S")
        except ValueError as e:
            print(str_date);print(str_time)
            print("happy testing line 101 101 101")
            seterr("Cannot parse the date .Quit!");
        return dt;         
    
    
    def _segment_activity(self,name=""):
        #pre-parse all the activities and store it in the list
       
        txt=self.s_file.read();
        ##print txt;
        act_list=[];
           
             
        bound_regex="^.*\\s"+name;
        match =re.finditer(bound_regex,txt,re.MULTILINE);                 
        for m in match :                
                act_list.extend([m.group()]);        
        return act_list;
    
  
    
    def set_segmentype(self,seg_type):
        self.seg_type=seg_type;
        #self._segment_activity();
    
    def set_seg_Structure(self, struct):
        self.sec= struct[0];
        self.minute =struct[1];
        self.hour =struct[2];
        self.day= struct[3];
        self.week =struct[4];
        self.month =struct[5];
        self.year=struct[6];
        
import unittest;

class SegmentFileTest(unittest.TestCase):   
   
    readpathAruba="/net/files/home/pdawadi/Dropbox/Studies/Fall 2011/Machine Learning/Project/data/dataAruba.txt"   
 
    #readpathAruba="/net/files/home/pdawadi/Dropbox/Studies/Fall 2011/Machine Learning/Project/data/dataAruba.txt"

    #test for usual 24 hr period  
    def test_segment_file_time(self):        
        fs = FileSegmentation(self.readpathAruba);
        #seg_str=[0,0,0,1,0,0,0] # sec min hr day week month year         
        seg_str=[0,0,1,0,0,0,0]
        fs.set_seg_Structure(seg_str);     
        writepathTest= "/net/files/home/pdawadi/Dropbox/eclipse/workspace/study/MachineLearningProject/test/seg_test1.csv";
        myFile2 = open(writepathTest,"wb");        
       
        startTime="12:00:00.000000"; #provide begining time 
        seg=fs._segment_file_time(startTime);
        l=[1,2,3,4,5,6,7,8,9];
        a=0;
        for  p in l:            
            #print seg;  
            myFile2.write(seg[1])   
            myFile2.write("\n");
            myFile2.write(str(seg[0])+"  ---------------------segment-----------------------");
            myFile2.write("\n");
            seg=fs._segment_file_time(startTime);
            a=a+1;
        
    
    #test for usual all hr period  
    def  test_segment_file_Arbtime(self):
        fs = FileSegmentation(self.readpathAruba);
        seg_str=[0,0,1,0,0,0,0] # sec min hr day week month year         
        fs.set_seg_Structure(seg_str);        
        writepathTest= "/net/files/home/pdawadi/Dropbox/eclipse/workspace/study/MachineLearningProject/test/seg_test2.csv";
        myFile2 = open(writepathTest,"wb");
        
        
        ''' startTime="12:00:00.000000";
        seg=fs._segment_file_time(startTime)
        l=[1,2,3,4,5,6,7,8]
        for a in l:                          
            #print seg;  
            myFile2.write(seg[1])   
            myFile2.write("\n");
            myFile2.write(str(seg[0])+"  ---------------------segment-----------------------");
            myFile2.write("\n");
            seg=fs._segment_file_time(startTime);'''

    
 
