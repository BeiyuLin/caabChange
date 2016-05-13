import sys;
import optparse;
from Activities import  *;

if __name__=="__main__": 
    #read_path= "/home/pdawadi/Documents/new_actfeatures/file/hh101.txt";
    #write_path = "/home/pdawadi/Documents/new_actfeatures/act_features1"
   # read_path="/Users/BeiyuLin/Desktop/caab/ProjectCorr/data/hh101.txt";
    read_path="/Volumes/Seagate Backup Plus Drive/IAQ/caab/ProjectCorr/IndoorAirQuality/nodupatmo1.txt";

    write_path="/Volumes/Seagate Backup Plus Drive/IAQ/caab/ProjectCorr/FeatureExtraction";
#    read_path="Users/BeiyuLin/Desktop/caab/ProjectCorr/IndoorAirQuality/atmo1.txt"
#    write_path="/Users/BeiyuLin/Desktop/caab/ProjectCorr/act_newfeaturesindoorair"
    #List of the activities to process
    #Add more entries to this list if you want to add more activities. Make sure that the names in the activity annotated sensor files and this list are identical. 
    actList = ["","Sleep",
                  "Bed_Toilet_Transition",
                  "Relax",
                  "Leave_Home",
                  "Cook",
                  "Eat",
                  "Personal_Hygiene",
                  "FunctionalHealth"]; #this is mobility
 
 #actList=["", " MainDoor"]
    p=optparse.OptionParser(description="Extracts activity features from activity annotated (Horizon House) sensor data",
                            prog="ActFeatures",
                            version="0.01",
                            usage = """%prog -r data_path -w write_path -a activity_index -p [0/1]   
                            -r read (directory) location of the activity annotated sensor data \n 
                            -w directory location to write extracted features in term of csv files      \n
                            -a activity_index         
                            -p process all activities at the same time \n                          
                            """);
                             
    p.add_option("--readpath" , "-r",
                 default = read_path, 
                 help = """Location of the activity annotated sensor data. The sensor file should be in the following format \n
                 2011-06-13 12:29:09.62626 WorkArea Office M012 ON Work \n
                 """);
                 
    p.add_option("--writepath", "-w",help="""Directory location to write extracted features in term of csv files.
     The naming convention of the csv file is some thing like hh101.Sleep, hh101.Cook.""", default = write_path)
    
    p.add_option("--actIdx"  , "-a", default="1", 
                 help="Which activity do you want me to process? \n 1.Sleep, 2.Bed_Toilet_Transition, 3.Relax, 4.Leave_Home, 5.Cook, 6.Eat, 7.Personal_Hygiene, 8.Functional Health (Mobility)");
                 
    p.add_option("--process_all_act"  , "-p", default="0", 
                 help="Do you want me to process all eight activities? 1: yes 0:No, if No I am going to process Sleep activity if actIdx is not specified");
                 
    
    #parse arguements
    opt,args = p.parse_args();
    read_path = opt.readpath;
    write_path = opt.writepath
    process_all = opt.process_all_act   
    try:
        idx = int(opt.actIdx);
        if idx==0: 
            idx=1;
        elif idx>len(actList)-1:
          print("Provide an index for the the activity name");
          raise TypeError();     
        
    except :
        seterr("Activity name must be integer from 1 to 8 based on this list 1.Sleep, 2. Bed_Toilet_Transition, 3. Relax, 4. Work, 5. Cook, 6.Eat 7. Personal Hygiene 8.Functional Health (mobility)");
        #seterr(" Activity name must be integer from 1 to 12 based on this list 1.BathroomAWindowA, 2.BathroomBWindowA, 3.BedroomADoor, 4.BedroomAWindowA, 5.BedroomAWindowB,6.DoorB,7.DoorC, 8.DoorD, 9.DoorE, 10.DoorF, 11.KitchenAWindowA, 12.OfficeAWindowA");
    
    
    print "Reading sensor data from " + read_path;
    
    if os.path.isdir(read_path):
            paths = [os.path.join(read_path,fn) for fn in next(os.walk(read_path))[2]];
    else:
        paths =[read_path];
    
    print "I found following sensor data"
    for p in paths:
        print "testPPPPPPP"
        print p   
        print "testPPPPPPP"
    
    #-------------start processing-------------------------
    if process_all == "1":        
         
        for p in paths:        
            print;print;
            hh_id = getID(p);
            print "Processing.....................",hh_id         
            for i in np.arange(1,9):            
                actname = actList[i]; 
                write_path_features=write_path+ "/" +hh_id +"."+ actname;             
                extractFeatures(p, write_path_features,hhid = hh_id, activityName=actname);
    else:
        print p
        for p in paths:        
            print "" ;print;
            hh_id = getID(p);
            print "Processing.....................",hh_id       
            actname = actList[idx]; 
            write_path_features=write_path+ "/" +hh_id +"."+ actname;            
            extractFeatures(p, write_path_features,hhid = hh_id, activityName=actname);    
            print "You can find activity features at "+write_path_features;
            print;
    
    
    print;  
    print "Yayy Done!"
           
            
            
            
