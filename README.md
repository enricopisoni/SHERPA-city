# --------------------------------#
# SHERPA-city (Madrid Case study) #
# --------------------------------#

This readme file guides you through the process of setting up SHERPA-city simulations 
for a whole as many cities and scenarios as you want. This specific branch uses data for Madrid
provided by Panos (JRC Seville)

## Preparative steps
Create a directory for you simulation runs (sc_root). This will be the root directory for all scripts. Download the code by executing the code below in the terminal. (Don't forget the point, otherwise the code is in a subdirectory.)

```git clone https://github.com/bd77/SHERPA-city.git .```

If the directory is not empty - there might be already some input data - do the following:

```git init```

Link the local repository to the one online.

```git remote add -f -t master -m master origin https://github.com/bd77/SHERPA-city.git```

Copy the code from the online repository to the local folder.

``` git pull```

Eventually change to another branch.

``` git checkout <branch name> ```

Save the file "city_list_example.txt" from the github repository as "city_list.txt". Adapt the file city_list.txt. Add a line for every city you want to model. The format must be the same as the example given on github. When an extent of the domain is not provided (NA) a default domain of 20 by 20 km around the centre is chosen.

### NO2_atlas_config.R ###
# ------------------------
Save the file "NO2_atlas_config_example.R" from the github repository as "NO2_atlas_config.R".
Adapt the paths in the config file "NO2_atlas_config.R". The working directory (root directory) is the one where the 'NO2_atlas_workflow.R' script is. This is the directory where you cloned the repository. All paths have to be defined relative to this folder or as absolute paths. The following paths have to be defined:
- The path to "city_list.txt"

- The path to the fleet data from EMISIA, 2 files: "fleets.csv" and "fleets_data.csv". These files were produced from the EMISA data with the script "SHERPAcity\EMISIA data\updated data 20190329\create_SHERPAcity_emission_factors.R"

- The path to the fleet configurations folder and overview file. A fleet configuration template is also created by "SHERPAcity\EMISIA data\updated data 20190329\create_SHERPAcity_emission_factors.R"

- Path to the scenario template file.

- The path to the folder with the OpenTransportMap data (otm.path). This is a gigantic dataset, not available on github. The files were downloaded from http://www.opentransportmap.info/download/. This can be automated. The AADT (annual average daily traffic) and capacity data in the original shp files were scaled to match the GAINS national totals. The 'trafficvol' field underestimates national totals while the 'capacity' data overestimated the GAINS national totals.

- The path to the boundary boxes of the OTM files (nuts3.bbox.file). This file makes it easier to know which OTM files overlap with the domain of interest.

- The source kernels location. This is also a big data set with a source kernel (concentration around a 1 kg/hour source, 4 by 4 km and a resolution of 20 meters). The kernels were calculated with IFDM.

- The path to the city network folder (cities.network.folder). This can be a path to a previous project to avoid making the network again from OTM data.

- The path th the zones folder (cities.zones.folder). This can be a path to a previous project to avoid copying the shape file.

- Define an output folder for all the city results (cities.output.folder).

### NO2 Atlas workflow ###
#-------------------------
The script 'NO2_atlas_workflow.R' coordinates the whole process. First it sets the directory where it is located as working directory. Then the following steps are done for each city in the city_list.txt file:
1) Looking up which OTM shape files are needed to construct a shape file of the domain and producue the shape file. By default the city centre (defined in city.df) is taken as the centre of a 20x20 km square. If the lon.min, lon.max, lat.min, and lat.max variables are avaliable in city.df these are taken as domain boundaries. All results of a city are in a subfolder <cityname>/
2) Add a column with the zone to the network shp. A zone shape file has to be availalble in a sub-folder <cityname>/zones_<city_name>. The format of this file is strictly defined:
- projection WGS84, 3 fields, id (int), name (string), descr (string)
- non overlapping polygons named bigLEZ and smallLEZ (for the Atlas)
3) Grid the road network (currently in Python)
4) Calculate fleet emission factors for all scenarios and zones
5) Calculate gridded emissions for each scenario. Scenarios are under <cityname>/results/<scenario_name>
6) Apply the dispersion kernels on the emissions

### To do/ideas
- Add a path to a folder with the zones for each city to the config file instead of putting them in the city folder.
- Add more error handling and messages. E.g. the parallel routine doesn't return errors. It's hard to know where it went wrong without directily calling sherpa_city_par.R for a specific case.
- default area of 20x20km: put the default in the config file. 
- add an example of OTM data to the github
- parallelize making UTM shape files for each domain and adding zones.
- Add the possibility to define paths to the network file, zoned network file and the zones shape files. This would avoid copying a lot of data when the same network and zones are used with different background, different code,...
