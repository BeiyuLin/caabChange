"""
Randomly annotate each line of sensor data with random activities. 
Random activity labels replaces existing activity labels. 
"""
import numpy as np;
import sys;
import os;
import re;
from os import listdir
from os.path import isfile, join
import optparse;
import random;

ACT_LIST = ["Sleep", "Bed_Toilet_Transition", "Cook", "Eat", "Personal_Hygiene", "Relax", "Work","Other_Activity"] 
 
 

def chooseRandActivity():
     """
     Implements  a simple random sampling algorithm.
     Generate a random number and multiply by N. 
     Choose index using integer represented by this result.    
     """
    
     randn = random.random();
     n = np.size(ACT_LIST);
     idx = np.int(randn * n);  #????????
     return ACT_LIST[idx];

def isAnnotated(read_path):
    """    
    Read the first 10 lines and decide if any activity labels are present.
    """
    
    search_reg = re.compile("("+ "|".join(ACT_LIST) + ")",re.MULTILINE|re.IGNORECASE);
    with open(read_path) as myfile:
        
        #read only first 10 lines
        head=[myfile.next() for x in xrange(10)]
        str= "".join(head);
        #check if head contains any activity labels.
        matches = search_reg.findall(str);
        
    if len(matches)>0:
            return True;
    return False; 
            
         
    
def randAnnotate(read_path, writepath): 
    """
    Annotate a file with random activity label obtained from chooseRandActivity()
    """
    
    print("Reading " + read_path);
    
    is_ann = isAnnotated(read_path);
    read_file = open(read_path);
   
    with open(writepath,"w+") as write_file:   
        for line in read_file:    
              act =chooseRandActivity(); 
              #remove the original activity label 
              if is_ann:
                  regex_remove_label ="\s[A-z]+$"; #activity label is always at the end of the line. 
                  line = re.sub(regex_remove_label,"",line);
              line = line.strip() + " " + act+ "\n";
              write_file.write(line);
               
    read_file.close();
 
    
         
def main(f_loc):   
    """
    If f_loc is a folder location that contains data, save the resulting random annotated data in a folder randAnnotate.
    If f_loc is a file, save the resulting random random annotated data as file.rand.
    """
   
    random.seed(100);
    print("hello: " + f_loc);
         
    if os.path.isdir(f_loc):
        write_path = f_loc+"newRandAnnotate/";
        
        if not os.path.exists(write_path):
            os.makedirs(write_path)
        onlyfiles = [f for f in listdir(f_loc) if isfile(join(f_loc,f)) ]
        #for each file call read annotate
        for file_name in onlyfiles:
            randAnnotate(f_loc+file_name, write_path+file_name+".rand");
        
    elif os.path.isfile(f_loc):
            #elif os.path.isfile(read_path):
        write_path = f_loc+".rand";
        randAnnotate(f_loc,write_path );
        
    else:
        print("I am doing nothing.");
    
    print("Done! Output(s) written at "+ write_path);
    
   

if __name__ == '__main__':
  
    p=optparse.OptionParser(description="Randomly annotate Motion sensor(excludes area sensors) with ON events with one of the 8 Activities with equal probabilities",
                            prog="RandomAr",
                            version="0.01",
                            usage = "%prog -r data_path");
    p.add_option("--readpath" , "-r", 
                 default = "/net/files/home/pdawadi/Documents/HorizonHouse/hhdataAug162013/annotated/", 
                 help = "File/Directory Location of the raw sensor data");
    p.add_option("--actname", 
                 help="1. Sleep, 2. Bed_To_Toilet, 3.Cook, 4.Eat, 5.Personal_Hygeiene, 6.Relax, 7.Work, 8.OTHER_ACTIVITY" );
                 
    opt,args = p.parse_args();   #??????????????
    read_path = opt.readpath;
    if read_path is None:
        seterr("File/Directory location is required.");
    #print read_path;           
    print("           I am annotating  randomly with these activities with equal probabilities");
    print("-----------"+",".join(ACT_LIST)+"-----------")
    print(" ");
    
    main(read_path);
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    