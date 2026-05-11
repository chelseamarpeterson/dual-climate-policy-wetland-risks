# -*- coding: utf-8 -*-
"""
Created on Mon Mar 30 13:06:37 2026

@author: Chels
"""

import geopandas as gpd
import pandas as pd
from rasterstats import zonal_stats
from rasterstats import point_query
import rasterio as rio
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

# path to combined above and belowground carbon raster
soil_rasters = np.array(["F:/Databases/Carbon/Soil_Rasters/Georgiou_total_MOC_t_ha_ZeroFill.tif",
                         "F:/Databases/Carbon/Soil_Rasters/SoilGrids2_0_IL_Clip_ZeroFill.tif"])
soil_raster_labels = np.array(["MOC","SOC"])
n_r = len(soil_rasters)

# load wetland polygons
polygons = np.array(["F:/Wetland_Climate_Impacts/Flood_Volume_Estimation/IL_WS_Step19_WaterRegime_Filter.shp",
                     "F:/Wetland_Climate_Impacts/Flood_Volume_Estimation/IL_WS_Step20_AreaThreshold_Filter.shp"])
polygon_names = np.array(["Step20_WaterRegime","Step21_AreaThreshold"])
n_p = len(polygons)

for i in range(n_p):
    # read polygons
    zone_gdf = gpd.read_file(polygons[i])
    polygon_name = polygon_names[i]
    
    for j in range(n_r):
        # get raster label
        raster_label = soil_raster_labels[j]
        
        # calculate zonal statistics    
        with rio.open(soil_rasters[j]) as raster:
            zone_gdf = zone_gdf.to_crs(raster.crs) # The key fix
            stats_df = zonal_stats(zone_gdf, 
                                   raster.read(1), 
                                   affine=raster.transform, 
                                   stats=['mean'],
                                   all_touched=True)
            
        # make dataframe
        stats_df = pd.DataFrame(stats_df)
        stock_column_name = f'{raster_label}_Stock'
        total_column_name = f'{raster_label}_Total'
        stats_df.rename(columns={'mean': stock_column_name}, inplace=True)
        stats_df['Area'] = zone_gdf['Polygon_Ar']
        stats_df['Type'] = zone_gdf['WETLAND_TY']
        stats_df[total_column_name] = stats_df[stock_column_name] * stats_df['Area']

        # add biomass C stock estimates to wetland polygon dataframe and then export attribute table as csv
        zone_gdf[total_column_name] = stats_df[total_column_name]
    
    zone_gdf.to_csv(f"F:/Wetland_Climate_Impacts/Carbon_Storage_Estimation/IL_WS_{polygon_name}_Filter_SOC_MOC_Database_Stocks.csv", index=False)

