# -*- coding: utf-8 -*-
"""
Created on Tue Dec 23 15:56:58 2025

@author: Chels
"""

import geopandas as gpd
import pandas as pd
from rasterstats import zonal_stats
import rasterio as rio
import rioxarray
import xarray as xr
import numpy as np
import os

###############################################################################
# metadata

# categories
cats = np.array(["pr","temp"])
n_c = len(cats)

# future climate scenarios
ssps = np.array(["ssp245","ssp370","ssp585"])

# time ranges
start_times = np.array(["2041","2071","2041"])
end_times = np.array(["2070","2100","2100"])
n_t = len(start_times)

# change working directly
os.chdir("F:/Wetland_Climate_Impacts/Climate_Data/USGS_Thresholds_WeightedMMM")


# variable abbreviations
var_abbrv = np.array(['prmax1day','wetdays','dry_days','dryspmxlen','wetspmxlen',
                      'pr_dabnz90','pr_dabnz95','pr_dabnz99','pr_abnz90','pr_abnz95',
                      'pr_abnz99','prmax5day','prmax10day','pr_d_ge1in','pr_d_ge2in',
                      'pr_d_ge3in','pr_d_ge4in','pr_d_ge5in','pr_annual'])

# zones
zones = np.array(["Tract","County","HUC8","HUC12"])
n_z = len(zones)
zone_files = np.array(["F:/Databases/Census/tl_2025_17_tract/tl_2025_17_tract.shp",
                       "F:/Databases/Census/tl_2025_us_county/tl_2025_il_county.shp",
                       ])
zone_gdf = gpd.read_file()


###############################################################################
# calculate zonal statistics for each scenario and time intervals

for c in cats:
    for s in ssps:
        for t in range(3):
            # load raster
            raster_ds = xr.open_dataset("Grid/{}/interval_mean_differences/CMIP6_LOCA2_Thresholds_WMMM_{}_{}_{}_Future_Mean_Difference_Grid.nc".format(c,s,start_times[t],end_times[t]),
                                        engine="rasterio")

            # fix CRS of raster
            if not raster_ds.rio.crs:
                raster_ds.rio.write_crs("EPSG:4326", inplace=True)   
            try:
                current_crs = raster_ds.rio.crs
                print(f"Current CRS: {current_crs}")
            except AttributeError as e:
                print(f"Error accessing CRS via .rio accessor: {e}")
                
            # loop through all variables
            variable_names = list(raster_ds.data_vars.keys())
            n_v = len(variable_names)
            for i in range(n_v):
                v_long = variable_names[i]
                var_array = raster_ds[v_long]
                output_path = 'Tract/new_output_raster.tif'
                var_array.rio.to_raster(output_path, driver="GTiff", compress="lzw")
            
                # calculate zonal statistics    
                with rio.open(output_path) as raster:
                    zone_gdf = zone_gdf.to_crs(raster.crs) # The key fix
                    stats_df = zonal_stats(zone_gdf, 
                                        raster.read(1), 
                                        affine=raster.transform, 
                                        stats=['mean'],
                                        all_touched=True)
                # make dataframe
                stats_df = pd.DataFrame(stats_df)
                
                # change column name
                if c == "temp":
                    stats_df = stats_df.rename(columns={'mean': v_long})
                else:
                    v_short = var_abbrv[i]
                    stats_df = stats_df.rename(columns={'mean': v_short})
                    
                # upend statistics to shapefile
                if v_long == variable_names[0]:
                    result_gdf = gpd.GeoDataFrame(pd.concat([zone_gdf, stats_df], axis=1))
                else:
                    result_gdf = gpd.GeoDataFrame(pd.concat([result_gdf, stats_df], axis=1))
                    
            # save zonal stastics#
            output_shp = "Tract/{}/interval_mean_differences/tract_extreme_stats_{}_{}_{}.shp".format(c,s,start_times[t],end_times[t])
            result_gdf.to_file(output_shp, driver='ESRI Shapefile')
