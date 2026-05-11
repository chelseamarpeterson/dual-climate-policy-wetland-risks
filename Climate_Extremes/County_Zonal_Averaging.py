# -*- coding: utf-8 -*-
"""
Created on Tue Dec 23 15:56:58 2025

@author: Chels
"""

import numpy as np
import pandas as pd
import geopandas as gpd
from rasterstats import zonal_stats
import xarray as xr

# climate extreme categories
categories = np.array(["pr","temp"])
n_c = len(categories)

# future climate scenarios
scenarios = np.array(["ssp245","ssp370","ssp585"])
n_s = len(scenarios)

# future 
intervals = np.array(["2041_2070","2071_2100"])
n_y = len(intervals)

# zones vector file
county_shapefile = "F:/Databases/Census/tl_2023_us_county/tl_2023_il_county_clipped_project.shp"
polygons = gpd.read_file(county_shapefile)

# read in metadata 
meta_data = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Dual-Risk-Repo/Climate_Extremes/climate_threshold_extreme_variables.csv"
meta_data_df = pd.read_csv(meta_data)

# temperature variables
temp_variables = meta_data_df["Temperature"].to_numpy(dtype=str)
temp_variables = temp_variables[temp_variables != 'nan']

# precipitation variables
precip_variables = meta_data_df["Precipitation"].to_numpy(dtype=str)
precip_abbreviations = np.array(["dry_days","dry_spells",
                                 "pr_anz90","pr_anz95","pr_anz99",
                                 "pr_annual",
                                 "pr_danz90","pr_danz95","pr_danz99",
                                 "pr_dge1in","pr_dge2in","pr_dge3in",
                                 "pr_dge4in","pr_dge5in","prmax1day",
                                 "prmax5day","prmax10day",
                                "wet_spell","wet_days"])
temp_abbreviations = temp_variables
day_unit_vars = np.array(["FD","ID",
                          "SU","TR",
                          "TX90pDAYS",
                          "TX95pDAYS",
                          "TXge90F",
                          "TXge95F",
                          "TXge100F",
                          "TXge105F",
                          "TXge110F"])

# put all variables into a dictionary
variable_names = dict(pr = precip_variables, temp = temp_variables)
variable_abbreviations = dict(pr = precip_abbreviations, temp = temp_abbreviations)

# average rasters over county polygons
nanoseconds_per_day = 24 * 60 * 60 * 1e9
for c in categories:                 
    for s in scenarios:
        for i in intervals:
            polygons_c_v_s = polygons
            for j in range(len(variable_names[c])):
                v1 = variable_names[c][j]
                v2 = variable_abbreviations[c][j]
                input_path = f"F:/Wetland_Climate_Impacts/Climate_Data/USGS_Thresholds_Weighted/Raster/{c}/bma_weighted_differences/{s}/{i}/NCA5_BMA_Weighted_Ensemble_Difference_{v1}_{s}_{i}.tif"                   
                stats = zonal_stats(polygons, input_path, stats="mean")
                stats_df = pd.DataFrame(stats)
                if v1 in day_unit_vars:
                    stats_df["mean"] = stats_df["mean"] / nanoseconds_per_day              
                stats_df.columns = np.array([v2])
                polygons_c_v_s = pd.concat([polygons_c_v_s, stats_df], axis=1)
            output_path = f"F:/Wetland_Climate_Impacts/Climate_Data/USGS_Thresholds_Weighted/County/{c}/bma_weighted_differences/{s}/{i}/NCA5_BMA_Weighted_Ensemble_Difference_County_Means_{s}_{i}.shp"                   
            polygons_c_v_s.to_file(output_path)
