# -*- coding: utf-8 -*-
"""
Created on Tue Dec 23 15:56:58 2025

@author: Chels
"""

import geopandas as gpd
import pandas as pd
import numpy as np
from rasterstats import zonal_stats
import rasterio as rio
#import rioxarray
import xarray as xr
from rasterio.transform import Affine

# categories
categories = np.array(["pr","temp"])
n_c = len(categories)

# future climate scenarios
scenarios = np.array(["ssp245","ssp370","ssp585"])

# time ranges
intervals = np.array(["2041_2070","2071_2100"])
n_t = len(intervals)

# zones vector file
county_shapefile = "F:/Databases/Census/tl_2023_us_county/tl_2023_il_county_clipped.shp"

# variables
meta_data = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Dual-Risk-Repo/Climate_Extremes/climate_threshold_extreme_variables.csv"
meta_data_df = pd.read_csv(meta_data)
temp_variables = meta_data_df["Temperature"].to_numpy(dtype=str)
temp_variables = temp_variables[temp_variables != 'nan']
temp_variables

precip_variables = meta_data_df["Precipitation"].to_numpy(dtype=str)
precip_variables

# test zonal averaging for specific case before making for loop

category = "pr"
scenario = "ssp245"
interval = "2041_2070"
variable = "pr_annual"

# make file path
category_path = f"F:/Wetland_Climate_Impacts/Climate_Data/USGS_Thresholds_Weighted/Grid/{category}/bma_weighted_differences/{scenario}/{interval}"
file_name = f"NCA5_BMA_Weighted_Ensemble_Difference_{variable}_{scenario}_{interval}.nc"
file_path = category_path + "/" + file_name
file_path

# open raster
raster_da = xr.open_dataset(file_path, engine="netcdf4")

# assign coordinate system
raster_da.rio.write_crs("EPSG:4326", inplace=True)
raster_da[variable].plot()

# read in zone file
polygons = gpd.read_file(county_shapefile)
polygons = polygons.to_crs(raster_da.rio.crs)
polygons.plot()

# convert raster to .tif format
var_array = raster_da[variable]
output_path = "F:/Wetland_Climate_Impacts/Climate_Data/USGS_Thresholds_Weighted/Grid/temp_output_raster.tif"
fixed_path = "F:/Wetland_Climate_Impacts/Climate_Data/USGS_Thresholds_Weighted/Grid/fixed_output_raster.tif"
var_array = var_array.rename({'lat': 'y','lon': 'x'})
var_array.rio.to_raster(output_path, driver="GTiff", compress="lzw")

# fix affine transformation in raster
with rio.open(output_path) as src:
    
    # Get the existing transform and data
    transform = src.transform
    data = src.read()
    meta = src.meta.copy()
    
    # Check if y cell size (e) is positive and fix it
    if transform.e > 0:
        new_transform = Affine(transform.a, transform.b, transform.c,
                               transform.d, -transform.e, transform.f) # make e negative
        meta['transform'] = new_transform
     
    with rio.open(fixed_path, 'w', **meta) as dst:
        dst.write(data)
      
# retry zonal statistics
stats = zonal_stats(polygons, fixed_path, stats="mean", all_touched=True)
    