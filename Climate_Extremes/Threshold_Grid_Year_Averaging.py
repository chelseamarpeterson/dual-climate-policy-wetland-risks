# -*- coding: utf-8 -*-
"""
Created on Wed Dec  3 22:03:29 2025

@author: Chelsea M. Peterson 
"""

import os
import xarray as xr
import numpy as np
import pandas as pd
#import xclim
#import matplotlib as plt
#from pathlib import Path
#from xclim import ensembles
#from netCDF4 import Dataset

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

# directory
os.chdir("F:/Wetland_Climate_Impacts/Climate_Data/USGS_Thresholds_WeightedMMM/Grid")

# function to change days to d
def format_cfunits_and_attrs(da):
    if('units' in da.attrs):
        if(da.attrs['units'] == 'days'):
            da.attrs['units'] = 'd'
    return da

###############################################################################
# calculate mean differences for each climate scenario and time interval

for c in cats:
    for s in ssps:
        # climate file name
        if c == "temp":
            in_file = "{}/all_years/CMIP6-LOCA2_Thresholds_WeightedMultiModelMean.{}_1950-2100_annual_16thdeg_grid.nc".format(c,s)   
            ds_init = xr.open_dataset(in_file, mask_and_scale=False, chunks={}, decode_times=False)
            ds = xr.decode_cf(ds_init.map(format_cfunits_and_attrs), mask_and_scale=True, decode_times=True)              
        else:
            in_file = "{}/all_years/CRIS-Ensemble.{}.r1i1p1f1.2015-2100.LOCA_16thdeg_pr_metrics.nc".format(c,s)
            hist_file = "{}/all_years/CRIS-Ensemble.historical.r1i1p1f1.1950-2014.LOCA_16thdeg_pr_metrics.nc".format(c)
            ds_init = xr.open_dataset(in_file, mask_and_scale=False, chunks={}, decode_times=False)
            ds = xr.decode_cf(ds_init.map(format_cfunits_and_attrs), mask_and_scale=True, decode_times=True)      

        # plot
        #ds.GDD5.isel(time=0).plot()
        #ds.prmax5day.isel(time=0).plot()
        
        # historical timeframe (1981-2010)
        if c == "temp":
            ds_historical_slice = ds.sel(time=slice("1981","2010"))
            ds_historical_ave = ds_historical_slice.mean(dim="time")
            #ds_historical_ave.GDD5.plot()
        else:
            ds_historical_all = xr.open_dataset(hist_file)
            ds_historical_slice = ds_historical_all.sel(time=slice("1981","2010"))
            ds_historical_ave = ds_historical_slice.mean(dim=["time","ensemble"])
            #ds_historical_ave.prmax5day.plot()
            
        for t in range(3):
            # select future time frames
            ds_future = ds.sel(time=slice(start_times[t],end_times[t]))
            #ds_future.GDD5.isel(time=0).plot()
            #ds_future.prmax5day.isel(time=0).plot()
            
            # calculate mean over timeframe
            if c == "temp":
                ds_future_ave = ds_future.mean(dim="time")
            else:
                ds_future_ave = ds_future.mean(dim=["time","ensemble"])
                
            #ds_future_ave.GDD5.plot()
            #ds_future_ave.prmax5day.plot()
            
            # calculate differences
            ds_ave_diff = ds_future_ave 
            #ds_ave_diff.prmax5day.plot()
            variable_names = list(ds_future_ave.data_vars.keys())
            n_v = len(variable_names)
            for v in variable_names[1:n_v]:
                ds_ave_diff[v] = ds_future_ave[v] - ds_historical_ave[v]
            #ds_ave_diff.GDD5.plot()
            #ds_ave_diff.prmax1day.plot()
            
            # write differences
            ds_ave_diff.to_netcdf("{}/interval_mean_differences/CMIP6_LOCA2_Thresholds_WMMM_{}_{}_{}_Future_Mean_Difference_Grid.nc".format(c,s,start_times[t],end_times[t]))
        







