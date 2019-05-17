# SHERPA-city

This readme file guides you throught the process of setting up SHERPA-city simulations 
for a whole as many cities and scenarios as you want.

## Preparative steps
Create a directory for you simulation runs (sc_root). This will be the root directory for all scripts.
Make a sub folder for the code (e.g. sherpa_city_code) and clone the code.

Create a file city_list.txt. The format must be the same as the example given on github. When an extent of the domain is not provided (NA) a default domain of 20 by 20 km around the centre is chosen.

The script 'NO2_atlas_workflow.R' coordinates the whole process. It sets the directory where it is located as working directory.



To do/ideas
- default area of 20x20km: put the default in the config file. 
