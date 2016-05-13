'''
Created : 15 March 2013
Converts longtd SH HH data to long format
'''

'''                        Name                                                        ID
      _________________________________________________________________________|_______________                        
       1) monthly activity density (number of sensor events per day)           |     actDen
       2) variance of activity density over the month                          |     varActDen
       3) monthly motion amount (distance covered in the apartment per day)    |     mtn  
       4) variance of motion amount                                            |     varMtn,
       5) monthly time spent sleeping                                          |     slpAmt                
       6) variance in time spent sleeping                                      |     varSlpAmt
       7) monthly time spent in bed toilet transitions                         |     b2toilet
       8) variance in bed toilet transition time                               |     varb2toilet  
       9) monthly time spent eating at home                                    |     eat
      10) variance in time spent eating at home                                |     varEat
      11) monthly time spent relaxing at home                                  |     relax 
      12) variance in time spent relaxing at home                              |     varRelax                 
      13) monthly time spent outside the home                                  |     tmOutHome
      14) variance in time spent outside the home                              |     vartmOutHome'''
 


import re
import numpy as np;
import matplotlib.pyplot as plt;
from pdb import set_trace as brake; 

 

readfile = "/net/files/home/pdawadi/Dropbox/Research/Longitudinal Data Analysis/HH smart home data/monthstats.csv";
writefile="/net/files/home/pdawadi/Dropbox/Research/Longitudinal Data Analysis/HH smart home data/shHHLongFormat.csv"; 



with open(readfile) as f:
    content = str(f.readlines());

#find list of HH apartments 
findObj = re.findall('[a-z]+[0-9]+',content);
cnt = np.arange(0,len(findObj));
d = dict(zip(findObj,cnt)); 
revd = dict(zip(cnt,findObj));

#stores lgtd data in long format , each row( represents an apartment) stores the data(matrix) in object format
lghhdata = np.empty((len(d)),dtype="object");

headers = ["actDen","varActDen","mtn","varMtn","slpAmt","varSlpAmt","b2toilet","varb2toilet","eat","varEat","relax","varRelax","tmOutHome","vartmOutHome"]
headers=[ "id","time","density", "motion", "densityvariance", "motionvariance", "sleep", "bedtoilet", "eat", "relax", "out", "sleepvar", "bedtoiletvar", "eatvar", "relaxvar", "outvar"]


with open(readfile) as f:
    content = f.readline();
    aptindex = 0;      
    while content  !="":    
        content = content.strip();        
        if "#" in content:
            aptname = content.strip("#");
            #print aptname;
            if aptname:                     
                    aptind = d[aptname];                              
        else:            
            entries = content.strip().split(",");
            #print str(entries);
            if len(entries) < 2:
                content=f.readline();
                continue;            
            newrow = np.array(entries,dtype ='f')
            totentries = np.size(newrow);#.reshape(1,14);
            ndays = totentries/14;
            #14 variables are consecutively stored
            #reshape the matrix such that each row represents a day
            newmat = newrow.reshape(ndays,14);               
            if lghhdata[aptind] is not  None:
                lghhdata[aptind]= np.concatenate([lghhdata[aptind], newmat]);              
            else:               
                lghhdata[aptind]=newmat 
        content=f.readline();

#get the shape of the matrix 
print "Total apartments " , lghhdata.shape;
print "Apartment names " , d.keys();
nid = lghhdata.shape[0];
with open(writefile, 'w') as f:
    print >>f, ",".join(headers);
    for id in np.arange(0,nid):     
        nrows = lghhdata[id].shape[0];        
        for time in np.arange(0,nrows):
            datacol=""; 
            for ncol in np.arange(0,14):
           # brake();
                datacol = datacol+ str(lghhdata[id][time][ncol])+",";
            writestr = revd[id]+","+str(time+1)+ "," +datacol.rstrip(",");        
            print>>f,writestr; 

print "Transformation Complete."
print "File saved at ", writefile;
            


 