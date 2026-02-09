# -*- coding: utf-8 -*-
"""
Created on Thu Jan  8 18:35:54 2026

@author: Chels
"""

import geopandas as gpd
import rasterio
from rasterio.mask import mask
import numpy as np

# set the flood level field
flood_level_field = "Z_Min"

# input paths
polygon_paths = np.array(["F:/Wetland_Climate_Impacts/Flood_Volume_Estimation/IL_WS_Step17_AreaHeight_GAP_Cnty_Intersect.shp"])
n_p = len(polygon_paths)

# output paths
shapefile_paths = np.array(["F:/Wetland_Climate_Impacts/Flood_Volume_Estimation/IL_WS_Step18_AreaHeight_GAP_Cnty_FloodVol.shp"])
csv_paths = np.array(["F:/Wetland_Climate_Impacts/Flood_Volume_Estimation/IL_WS_Step18_AreaHeight_GAP_Cnty_FloodVol.csv"])

for i in range(n_p):
    # get polygon path
    polygons = gpd.read_file(polygon_paths[i])
    
    # ensure the polygons layer has the necessary flood level field
    if flood_level_field not in polygons.columns:
        raise ValueError(f"Field '{flood_level_field}' not found in the polygon layer attributes.")
    
    # list to store the calculated volumes
    volumes = []
    
    # open the DEM (using rasterio to get metadata easily within the loop)
    dem_path = "F:/WIM_Rasters/IL_DEM_3m.tif"
    with rasterio.open(dem_path) as dem:
        
        # ensure consistent CRS (optional but recommended)
        if polygons.crs != dem.crs:
            polygons = polygons.to_crs(dem.crs)
            print("Updated CRSs")
            
        # get pixel area (assuming a projected CRS with meters as units)
        # the resolution is typically in x and y (meters for projected CRS)
        pixel_area = abs(dem.res[0] * dem.res[1]) # Area in square meters
        print(pixel_area)
        
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
                
                volumes.append(total_volume)
    
            except ValueError:
                # Handles cases where polygon is outside the DEM extent
                print(f"Polygon {index} is outside DEM extent or invalid geometry, assigning 0 volume.")
                volumes.append(0)
                
# add the volumes back to the GeoDataFrame
polygons['flood_volume_m3'] = volumes
    
# Write the GeoDataFrame to a shapefile
shape_path = str(shapefile_paths[i])
polygons.to_file(shape_path)
    
# write the attribute table to a csv file
out_csv = str(csv_paths[i])
polygons = polygons.drop(columns=['geometry'])
polygons.to_csv(out_csv, index=False)