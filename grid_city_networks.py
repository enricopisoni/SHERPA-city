# -*- coding: utf-8 -*-
"""
Created on Wed Oct 24 15:19:11 2018

@author: degraba
"""
from time import time
import os.path
from fast_gridding import grid_shape 

# re-run all, also if the gridded table already exists
rerun = False

# read the list of cities
city_list_txt = "city_list.txt"
cities_zones_folder = "AllMadrid"
fc = open(city_list_txt, 'r')
line = fc.readline()
city_dict = {}

while True:
    line = fc.readline().rstrip()
    if len(line) == 0:
        fc.close()
        break
    data = line.split(';')
    city_dict[data[0]] = {'lon': data[1], 'lat': data[2]}
    
# loop over all cities
for cityname in city_dict.keys(): #:
    # check if the shp utm is produced
    city_shp_utm = "%s/%s/traffic_roadlinks_zones_%s.shp" % (cities_zones_folder, cityname, cityname)
    if os.path.isfile(city_shp_utm):
        print(city_shp_utm + " exists")
        city_grid_table = "%s/%s/%s_grid_tbl.txt" % (cities_zones_folder, cityname, cityname)
        if (not(os.path.isfile(city_grid_table)) or rerun):
            start = time()
            grid_shape(city_shp_utm, city_grid_table)
            end = time()
            print(city_grid_table + " created in " + str(round(end - start)) + "s")

        else:
            print(city_grid_table + " already exists.")
    else:
        print(city_shp_utm + " does not exist")    
    # path exists 
