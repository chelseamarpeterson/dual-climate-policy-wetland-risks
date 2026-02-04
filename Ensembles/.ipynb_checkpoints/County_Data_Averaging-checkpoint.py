# -*- coding: utf-8 -*-
"""
Created on Wed Dec  3 22:03:29 2025

@author: Chelsea M. Peterson 
"""

import os
import xarray as xr
import xclim
import matplotlib as plt
from pathlib import Path
from xclim import ensembles
import numpy as np

###############################################################################
# metadata

# future climate scenarios
ssps = np.array(["ssp245","ssp370","ssp585"])

# time ranges
start_times = np.array(["2041","2071","2041"])
end_times = np.array(["2070","2100","2100"])
n_t = len(start_times)

# scales
bl = np.array(["county","huc8"])
bu = np.array(["County","HUC8"])
n_b = 2

for b in range(1):
    # change wd
    os.chdir("F:/Climate_Data/USGS_Thresholds_WeightedMMM/{}".format(bu[b]))

    for s in ssps:
        ds = xr.open_dataset("CMIP6-LOCA2_Thresholds_WeightedMultiModelMean.{}_1950-2100_annual_timeseries_by_{}.nc".format(s,bl[b]),decode_timedelta=True) 
    
        # historical timeframe (1981-2010)
        ds_historical = ds.sel(time=slice("1981","2010"))
        ds_historical_ave = ds_historical.mean(dim="time")
           
        for t in range(n_t):
            # select future time frames
            ds_future = ds.sel(time=slice(start_times[t],end_times[t]))
            
            # calculate mean over timeframe
            ds_future_ave = ds_future.mean(dim="time", keep_attrs=True)
    
            # calculate differences
            ds_ave_diff = ds_future_ave
            variable_names = list(ds_future_ave.data_vars.keys())
            n_v = len(variable_names)
            for v in variable_names[3:n_v]:
                ds_ave_diff[v] = ds_future_ave[v] - ds_historical_ave[v]
            
            # create text file with county summaries
            df_ave_diff = ds_ave_diff.to_dataframe().reset_index(level='GEOID')
            df_ave_diff['GEOID'] = df_ave_diff['GEOID'].astype(int)
            df_ave_diff['NAME'] = df_ave_diff['NAME'].astype(str)
            df_ave_diff['STATEFP'] = df_ave_diff['STATEFP'].astype(int)
            seconds_in_day = np.timedelta64(1,'D').astype('timedelta64[s]').astype(float)
            for v in variable_names[3:n_v]:
                if df_ave_diff[v].dtype == '<m8[ns]':
                    df_ave_diff[v] = df_ave_diff[v].dt.total_seconds().astype(float)/seconds_in_day
                else:
                    df_ave_diff[v].astype(float)
                
            # isolate illinois counties
            df_ave_diff_il = df_ave_diff[df_ave_diff['STATEFP'] == 17]

            # write data
            df_ave_diff_il.to_csv("Thresholds_WMMM_{}_{}_{}_Hist_Mean_Diff_{}.csv".format(s,start_times[t],end_times[t],bu[b]), index=False)
    
