# -*- coding: utf-8 -*-
"""
Created on Wed Dec 31 13:16:22 2025

@author: Chels
"""

import os
import xarray as xr

# update directory
os.chdir("F:/Wetland_Climate_Impacts/Climate_Data/USGS_Water_Balance")

# function to change days to d
def format_cfunits_and_attrs(da):
    if('units' in da.attrs):
        if(da.attrs['units'] == 'days'):
            da.attrs['units'] = 'd'
    return da

# load file and decode time unit
#in_file = "stor/historical/stor_MWBM_LOCA2.ACCESS-CM2.historical.r1i1p1f1_1950-2014.nc"  
#in_file = "aet/historical/aet_MWBM_LOCA2.ACCESS-CM2.historical.r1i1p1f1_1950-2014.nc"
#in_file = "deficit/historical/deficit_MWBM_LOCA2.ACCESS-CM2.historical.r1i1p1f1_1950-2014.nc"
#in_file = "pet/historical/pet_MWBM_LOCA2.ACCESS-CM2.historical.r1i1p1f1_1950-2014.nc"
#in_file = "runoff/historical/runoff_MWBM_LOCA2.ACCESS-CM2.historical.r1i1p1f1_1950-2014.nc"
#in_file = "snow/historical/snow_MWBM_LOCA2.ACCESS-CM2.historical.r1i1p1f1_1950-2014.nc"
ds_init = xr.open_dataset(in_file, mask_and_scale=False, chunks={}, decode_times=False)
ds = xr.decode_cf(ds_init.map(format_cfunits_and_attrs), mask_and_scale=True, decode_times=True)

# try plotting
ds.snow.isel(time=700).plot()
