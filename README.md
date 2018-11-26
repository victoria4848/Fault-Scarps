# Fault-Scarps
Find offset along fault scarps from high-resolution DEMs

Inputs: 
      1. shp file of fault trace
      2. tif file of DEM
      
# 1.  Run 'Run_first_once-SETUP.R' a few times to install all the packages correctly:
     - may have to run multiple times to install the packages correctly, until you get a printout message 

# 2.  Add your folder containing fault shp file to Faults folder

# 3.  Add your folder containing DEM(s) tif files to the DEMs folder

# 4.  Open 'FaultNames.xlsx' and alter to your fault(s):
     - FaultNames column should contain the name of your fault as it is in your fault shp folder name
     - DEM_folder_name column should contain the name of DEM folder name
     - downtoside column should contain which way the fault dips when looking from the lowest latitude end to the highest            latitude end (i.e. north to south)

# 5.  Open 'Create_Profiles.R':
      - alter profile_spacing: this is the spacing along the fault in metres
      - alter profile_point_spacing: this is the spacing of points along the elevation profiles in metres. The minimum spacing         should be the resolution of your DEM. The algorithm uses a nearest neighbours technique to interpolate the elevations         along the profiles. 
      - run 
      
# 6.  Open 'App_Manual_Profiles.R':
      -  Follow app instructions

# 7.  Open 'Find_Offsets.R':
      - Enter the spacing that you chose in finding manual profiles
      - Change range and step for slope, curvature, and steep in a row 
            -- slope sets the minimum slope above which a location in the elevation profile is considered 'steep' and could be                a scarp
            -- curve sets the minimum curvature that could be a crest or base in the elevation profile
            -- steep in a row sets the number of 'steep' points in a row that need to exist before the section of steepness                  could be a scarp
      - run
      
