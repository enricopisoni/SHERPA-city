# SHERPA-city

This readme file guides you throught the process of setting up SHERPA-city simulations 
for a whole as many cities and scenarios as you want.

## Preparative steps
Create a directory for you simulation runs (sc_root). This will be the root directory for all scripts. Download the code (don't forget the point, otherwise the code is in a subdirectory)

```git clone https://github.com/bd77/SHERPA-city.git .```

Adapt the file city_list.txt. Add a line for every city you want to model. The format must be the same as the example given on github. When an extent of the domain is not provided (NA) a default domain of 20 by 20 km around the centre is chosen.

Adapt the paths in the config file "NO2_atlas_config.R":
- The path to city_list.txt
- The path to the folder with the OpenTransportMap data

### OpenTransportMap data


The script 'NO2_atlas_workflow.R' coordinates the whole process. It sets the directory where it is located as working directory.



To do/ideas
- default area of 20x20km: put the default in the config file. 
- add an example of OTM data to the github
