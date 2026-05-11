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

# paths to soil rasters
tif_path = "F:/Databases/Carbon/WCMC_carbon_tonnes_per_ha/AGB_BGB_carbon_mosaic3_v1b_IL_BoxClip_NullZero.tif"

# load wetland polygons
polygons = np.array(["F:/Wetland_Climate_Impacts/Flood_Volume_Estimation/IL_WS_Step19_WaterRegime_Filter.shp",
                     "F:/Wetland_Climate_Impacts/Flood_Volume_Estimation/IL_WS_Step20_AreaThreshold_Filter.shp"])
polygon_input_labels = np.array(["Step19_WaterRegime","Step20_AreaThreshold"])
polygon_output_labels = np.array(["Step20_WaterRegime","Step21_AreaThreshold"])
n_p = len(polygons)

for i in range(n_p):
    zone_gdf = gpd.read_file(polygons[i])
    polygon_input_label = polygon_input_labels[i]
    polygon_output_label = polygon_input_labels[i]
    
    # calculate zonal statistics    
    with rio.open(tif_path) as raster:
        zone_gdf = zone_gdf.to_crs(raster.crs) # The key fix
        stats_df = zonal_stats(zone_gdf, 
                               raster.read(1), 
                               affine=raster.transform, 
                               stats=['mean'],
                               all_touched=True)
    # make dataframe
    stats_df = pd.DataFrame(stats_df)
    stats_df.rename(columns={'mean': 'Biomass_Cstock'}, inplace=True)
    stats_df['Area'] = zone_gdf['Polygon_Ar']
    stats_df['Type'] = zone_gdf['WETLAND_TY']
    stats_df.loc[stats_df['Type'] == 'Freshwater Pond','Biomass_Cstock'] = 0
    stats_df['Biomass_Ctotal_Zonal'] = stats_df['Biomass_Cstock'] * stats_df['Area']
    
    # get carbon values for each point
    
    wetland_points = f"F:/Wetland_Climate_Impacts/Carbon_Storage_Estimation/IL_WS_{polygon_input_label}_Filter_InsidePoints.shp"
    pts = gpd.read_file(wetland_points)
    pts = pts[['Lon','Lat','Polygon_Ar','geometry','WETLAND_TY']]
    pts.rename(columns={'Lon':'Lon', 
                        'Lat':'Lat', 
                        'Polygon_Ar':'Area',
                        'geometry': 'geometry',
                        'WETLAND_TY': 'Type'}, inplace=True)
    pts.index = range(len(pts))
    coords = [(x,y) for x, y in zip(pts.Lon, pts.Lat)]
    
    # Open the raster and store metadata
    src = rio.open(tif_path)
    
    # Sample the raster at every point location and store values in DataFrame
    pts['Biomass_Cstock'] = [x[0] for x in src.sample(coords)]
    
    # set C stocks to zero for ponds
    pts.loc[pts['Type'] == 'Freshwater Pond','Biomass_Cstock'] = 0
    
    # estimate stocks from point data
    pts['Biomass_Ctotal_Point'] = pts['Biomass_Cstock'] * pts['Area']
    
    # compare results
    #plt.scatter(x=stats_df['Biomass_Ctotal'], y=pts.Biomass_Ctotal)
    #plt.scatter(x=np.log(stats_df['Biomass_Ctotal']), y=np.log(pts.Biomass_Ctotal))
    
    # Plot using Seaborn
    #sns.histplot(np.log(stats_df['Biomass_Ctotal']), color="skyblue", label="Data 1", kde=True)
    #sns.histplot(np.log(pts.Biomass_Ctotal), color="red", label="Data 2", kde=True)
    #plt.legend()
    #plt.show()
    
    # add biomass C stock estimates to wetland polygon dataframe and then export attribute table as csv
    zone_gdf['Biomass_Ctotal_Point'] = pts['Biomass_Ctotal_Point']
    zone_gdf['Biomass_Ctotal_Zonal'] = stats_df['Biomass_Ctotal_Zonal']
    zone_gdf.to_csv(f"F:/Wetland_Climate_Impacts/Carbon_Storage_Estimation/IL_WS_{polygon_output_label}_Filter_BiomassC_stocks.csv", index=False)

