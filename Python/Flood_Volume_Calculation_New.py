# -*- coding: utf-8 -*-
"""
Created on Fri Feb  6 08:52:51 2026

@author: Chels
"""

import geopandas as gpd
import rasterio
from rasterio.mask import mask
import numpy as np
import rioxarray
import matplotlib

# set the flood level field and pixel area
flood_level_field = "Z_Min"
pixel_area = 8

# input paths
polygon_path = "F:/Wetland_Climate_Impacts/Flood_Volume_Estimation/IL_WS_Step17_Height_GAP_Cnty_Int_Project.shp"
dem_path = "F:/WIM_Rasters/IL_DEM_3m.tif"

# output paths
shapefile_path = "F:/Wetland_Climate_Impacts/Flood_Volume_Estimation/IL_WS_Step18_AreaHeight_GAP_Cnty_FloodVol.shp"
csv_path = "F:/Wetland_Climate_Impacts/Flood_Volume_Estimation/IL_WS_Step18_AreaHeight_GAP_Cnty_FloodVol.csv"

# load polygons
polygons = gpd.read_file(polygon_path)
polygons['Z_Min'].isna().sum()
len(polygons)
#polygon = polygons.iloc[210600]


# list to store the calculated volumes
volumes = np.zeros(len(polygons))
polygons['flood_volume_m3'] = volumes

# Open the DEM lazily
dem = rioxarray.open_rasterio(dem_path, chunks={'x': 1000000, 'y': 1000000})
    
with rasterio.open(dem_path) as dem:  
    # iterate over each polygon to perform calculations
    for index, polygon in polygons.iterrows():
        try:
            # clip the DEM to the current polygon's geometry
            # mask() returns a numpy array of the data and the affine transform
            out_image, out_transform = mask(dataset=dem, shapes=[polygon.geometry], crop=True, nodata=dem.nodata)
            
            # Squeeze the array to remove single-dimensional entries (e.g., from (1, H, W) to (H, W))
            dem_clipped = np.squeeze(out_image)

            # Set nodata values to NaN for correct calculations
            dem_clipped[dem_clipped == dem.nodata] = np.nan

            # Get the specific flood level for this polygon
            flood_level = polygon[flood_level_field]

            # calculate water depth (difference raster)
            # Water depth is the difference between the flood level and the ground elevation
            # Only consider areas where the ground is below the flood level
            depth_raster = np.maximum(0, flood_level - dem_clipped)

            # calculate total volume within the clipped area
            # volume of each pixel = depth * pixel_area
            # sum all pixel volumes (ignoring NaNs)
            total_volume = np.nansum(depth_raster * pixel_area)
            
            polygons.loc[index,'flood_volume_m3'] = total_volume
            print(index)

        except ValueError:
            # Handles cases where polygon is outside the DEM extent
            print(f"Polygon {index} is outside DEM extent or invalid geometry, assigning 0 volume.")
            polygons.loc[index,'flood_volume_m3'] = 0
            
# Write the GeoDataFrame to a shapefile
shape_path = str(shapefile_path)
polygons.to_file(shape_path)

# wirte the attribute table to a csv file
out_csv = str(csv_path)
polygons = polygons.drop(columns=['geometry'])
polygons.to_csv(out_csv, index=False)
