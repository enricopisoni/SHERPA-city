# -*- coding: utf-8 -*-
"""
Created on Fri Aug 10 10:21:18 2018

This function converts a road network (shape file) into a table with:
    - attributes of the original road segment (inspireid)
    - cell centres in UTM, cell size 20 by 20 meters
    - road length
    - road class (main, 1st,... 4th)
    - vkm_volcor: corrected AADT

@author: degraba
"""

from math import floor, ceil
from shapely.geometry import LineString
from shapely.geometry import box
import fiona


# function to grid a road represented as a LineString
def grid_LineString(road):
    # input: road is a LineString object, not a MultiLineString!
    # output: list of cell dictionaries with cell_centre_x, cell_centre_y 
    # and road_length of the non empty cells.
    
    # output list in which cell dictionaries will be stored
    road_cell_list = []
    
    # resolution in meters (input has to be in UTM or Lambert or some
    # orthogonal equidistant grid)
    dx = 20
    dy = 20
    
    # calculate the boundary box of the road segment and round it
    # to the grid resolution
    road_bounds = road.bounds
    x_low = floor(road_bounds[0] / dx) * dx
    y_low = floor(road_bounds[1] / dy) * dy
    x_high = ceil(road_bounds[2] / dx) * dx
    y_high = ceil(road_bounds[3] / dy) * dy
    
    # when the point is on a grid line the lower and upper 
    # bounds of the extent could be the same
    if x_low == x_high:
        x_high += dx
        print('hix')
    if y_low == y_high:
        y_high += dy
        print('hiy')
    
    # determine the size fo the extent i cells
    nx = int((x_high - x_low) / dx)
    ny = int((y_high - y_low) / dy)
    
    # loop over all the cells in the bounding box. Check if there is
    # a road segemng inside. If so calculate the length and write
    # a line in the output table
    for ix in range(nx):
        for iy in range(ny):
            # define the cell as a box
            x_box_low = x_low + ix * dx
            x_box_high = x_box_low + dx
            y_box_low = y_low + iy * dy
            y_box_high = y_box_low + dy
            grid_cell = box(x_box_low, y_box_low, x_box_high, y_box_high)
            
            # cell centre
            x_centre = (x_box_low + x_box_high) / 2
            y_centre = (y_box_low + y_box_high) / 2
    
            # calculate the part inside and outside the grid cell
            road_in_box = road.intersection(grid_cell)
            length_in_cell = road_in_box.length
            
            # if the cell is not empty add it to the output dictionary
            if length_in_cell > 0:
                road_cell_dict = {'x': x_centre, 'y': y_centre, 'length_in_cell': length_in_cell}
                road_cell_list.append(road_cell_dict)
                
    return(road_cell_list)

def grid_shape(shp_file, output_table):
        
    print("Gridding '" + shp_file + "'")
    
    # open the shape file
    shape = fiona.open(shp_file)
    n_roads = len(shape)
    
    # open a file for the output table
    fo = open(output_table, 'w')
    fo.write((8 * "%s;" + "%s\n") % \
             ( 'x_centre', 'y_centre', 'inspireid', 'functional', 'zone_name',\
              'trafficvol', 'capcor', 'aadtmad', 'length_in_cell'))
    
    # loop over all roads in the shape file
    counter = 0
    last_printed_progress = 0
    for road_dict in shape:


        # extract some properies
        inspireid = str(road_dict['properties']['inspireid'])
        functional = road_dict['properties']['functional']
        zone_name = road_dict['properties']['zone_name']
        trafficvol = road_dict['properties']['trafficvol']
        capcor = road_dict['properties']['capcor']
        aadtmad = road_dict['properties']['aadtmad']

        if road_dict['geometry']['type'] == 'LineString':
            # update the counter
            counter += 1
            # convert the road in a linestring object
            road = LineString(road_dict['geometry']['coordinates'])
            # convert the Linestring in a list of cell dictionaries
            road_cell_list = grid_LineString(road)
            
        elif road_dict['geometry']['type'] == 'MultiLineString':
            # update the counter
            counter += 1
            # loop over the individual LineStrings of the MultiLineString
            n_ls = len(road_dict['geometry']['coordinates'])
            road_cell_list = []
            for i_ls in range(n_ls):
                # convert the sub road in a linestring object
                road = LineString(road_dict['geometry']['coordinates'][i_ls])
                # convert the Linestring in a list of cell dictionaries
                sub_road_cell_list = grid_LineString(road)
                road_cell_list = road_cell_list + sub_road_cell_list
                
        # write the results to a file
        for cell in road_cell_list:
            # print(cell)
            fo.write("%d;%d;%s;%s;%s;%f;%f;%f;%f\n" % \
                     (cell['x'], cell['y'], inspireid, functional, zone_name,\
                      trafficvol, capcor, aadtmad, cell['length_in_cell']))
        
        # print the progress
        progress = floor(counter / n_roads * 10)
        if progress > last_printed_progress:
            print("Progress: " + str(progress * 10) + "%")
            last_printed_progress = progress
            
    # close the output table        
    fo.close()
    print("Gridding table: '" + output_table + "'")
    # close the connection to the shape file
    shape.close()

# test the function
# if __name__ == "__main__":
    
#    shp_file = 'traffic_roadlinks_ITC4C_UTM32/traffic_roadlinks_ITC4C_UTM32.shp'
#    output_file = 'ITC4C_UTM32_table.txt'
#    grid_shape(shp_file, output_file)
    

#print('linestrings: %d' % (counter_linestrings))
#print('multilinestrings: %d' % (counter_mulitlinestrings))