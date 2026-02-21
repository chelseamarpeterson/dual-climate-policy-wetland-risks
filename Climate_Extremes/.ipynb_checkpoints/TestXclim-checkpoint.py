# -*- coding: utf-8 -*-
"""
Created on Thu Dec  4 12:37:20 2025

@author: petej
"""

import xarray as xr
import xclim
import os

os.chdir(r'D:\Climate_Data\USGS_Water_Balance\deficit')
ds = xr.open_dataset("File1.nc", engine="netcdf4")
ds.deficit.values[1:10,1:10,1:10]
#ds["deficit"] = ds["deficit"].astype('float64')
