# SHERPA-city methodology

Details on the SHERPA-city methodology can be found at: https://doi.org/10.1016/j.envsoft.2020.104904

B. Degraeuwe, E. Pisoni, P. Christidis, A. Christodoulou, P. Thunis,
SHERPA-city: A web application to assess the impact of traffic measures on NO2 pollution in cities,
Environmental Modelling & Software,
Volume 135, 2021, 104904.

# SHERPA-city offline version

This readme file guides you throught the process of setting up SHERPA-city simulations 
for a whole as many cities and scenarios as you want.

## Preparative steps
Create a directory for you simulation runs (sc_root). This will be the root directory for all scripts. Download the code by executing the code below in the terminal. (Don't forget the point, otherwise the code is in a subdirectory.)

```git clone https://github.com/bd77/SHERPA-city.git .```

If the directory is not empty - there might be already some input data - do the following:

```git init```

Link the local repository to the online one.

```git remote add origin https://github.com/bd77/SHERPA-city.git```

 Pull a specific branch (SNAP7_zero_background) to the local folder.

``` git pull https://github.com/bd77/SHERPA-city.git SNAP7_zero_background```

``` git checkout SNAP7_zero_background ```

Adapt the file city_list.txt. Add a line for every city you want to model. The format must be the same as the example given on github. When an extent of the domain is not provided (NA) a default domain of 20 by 20 km around the centre is chosen.

Adapt the paths in the config file "NO2_atlas_config.R":
- The path to city_list.txt
- The path to the folder with the OpenTransportMap data. This is a gigantic dataset, not available on github. The files were downloaded from http://www.opentransportmap.info/download/. This can be automated. The AADT (anual average daily traffic) data in the original shp files were scaled to mach the GAINS national totals. The trafficvol field underestimates national totals.
- The source kernels location. This is also a big data set with a source kernel (concentration around a 1 kg/hour source, 4 by 4 km and a resolution of 20 meters). The kernels were calculated with IFDM.

### NO2 Atlas workflow
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
- Add more error handling and messages. E.g. the parallel routine doesn't return errors. It's hard to know where it went wrong without directily calling sherpa_city_par.R for a specific case.
- default area of 20x20km: put the default in the config file. 
- add an example of OTM data to the github
- parallelize making UTM shape files for each domain and adding zones.
- parallelize emission and concentration raster between different cities to optimize the number of nodes used.
- Add warning when zones names in grid_table do not correspond to zone names in the scenario definition. Otherwise emissions disapear.
