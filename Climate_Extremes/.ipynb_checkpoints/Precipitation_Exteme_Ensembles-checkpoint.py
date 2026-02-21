# -*- coding: utf-8 -*-
"""
Created on Thu Feb 12 11:30:09 2026

@author: Chels
"""

# In[1]:

import os
import matplotlib
import pandas as pd
import xarray as xr
from itertools import product
from dask.distributed import Client, LocalCluster
import platform
from glob import glob
import numpy as np
import multiprocessing

# In[2]:

# Define paths
data_path = 'F:/Wetland_Climate_Impacts/Climate_Data/USGS_Thresholds_Weighted/Grid/pr/all_metrics'
meta_data_file = 'F:/Wetland_Climate_Impacts/Climate_Data/USGS_Water_Balance/NCCV2_LOCA2_model_extended_meta.csv'

# Specify weight type
weight_type = 'NCA5_BMA_weight' # or 'LOCA2_BMA_weight'

# GHG emission scenarios
scenarios = np.array(['ssp245','ssp370','ssp585'])
n_s = len(scenarios)

# Future year intervals
start_dates = np.array(['2041-01-01','2071-01-01'])
end_dates = np.array(['2070-12-31','2100-12-31'])
n_y = len(start_dates)

# In[4]:
    
def create_ensemble(scenario):      
    # Load the metadata file, filter only for our selected SSP, and create a unqiue ensemble ID (model.scenario.ripf).
    df = pd.read_csv(meta_data_file, encoding="ISO-8859-1")
    df = df[df['EXP'] == scenario]
    df['ensemble'] = df['MODEL']+"."+df['EXP']+"."+df['RIPF']

    # Drop most of the DataFrame and only keep the weights we want and the ensemble ID (also making it an index)
    df_weight = df.set_index('ensemble')[weight_type]

    # Since the ensemble ID is an index, we can easily convert this Pandas DataFrame to a xarray DataArray.
    weight_da = df_weight.to_xarray()
    
    # Use Python expansions to build our list of files
    hist_files = [
        f'{data_path}/historical/{ensemble_id.replace(scenario, 'historical')}.1950-2014.LOCA_16thdeg_pr_metrics.nc'
        for ensemble_id in list(weight_da.ensemble.values)
    ]

    ssp_files = [
        f'{data_path}/{scenario}/{ensemble_id}.2015-2100.LOCA_16thdeg_pr_metrics.nc'
        for ensemble_id in list(weight_da.ensemble.values)
    ]

    # Using the preprocessor, we can now load and combine all historical files.
    ds_hist = xr.open_mfdataset(hist_files, 
                                chunks={}, 
                                combine_attrs="drop_conflicts")

    # We do the same for the selected SSP files
    ds_ssp = xr.open_mfdataset(ssp_files, 
                               chunks={}, 
                               combine_attrs="drop_conflicts")

    # Rename the historical ensemble ID from model.historical.ripf to model.ssp.ripf so they can be concatenated. 
    # We need the ensemble ID labels to match between historical and SSP, so we force them to use the SSP IDs.
    ds_hist['ensemble'] = ds_hist.ensemble.str.replace('historical', scenario)

    # Combine historical + ssp for one single time series, for all models, all variables. 
    # Add weights to dataset as well. Notice the weights xr.DataArray aligns -by ID- across the xr.DataSet. 
    # This makes sure we don't jumble up the order of the model data vs the model weights.
    # 
    # At this point the xr.Dataset will be rather large (75 Gb per variable). 
    # Dask will load chunks of data on the fly as it needs them. 
    # However, if your computer is running out of memory or struggling, you can reduce the chunk size (read the xarray docs). 
    # The default with these data are ~50 Mb / chunk, which is pretty reasonable.
    ds = xr.concat([ds_hist, ds_ssp], dim="time")
    ds['weights'] = weight_da

    # ---- rechunk as needed here; it is currently [1, 360, 158, 236] ~ 50 Mb/chunk. 
    # ---- You could go higher or lower depending on the performance or amount of memory you have.

    # We can now create a weighted xr.Dataset for each year in the dataset. 
    # The weights var can be dropped because it is no longer needed.
    #weighted_ensemble_ds = ds.weighted(ds.weights).mean(dim="ensemble", keep_attrs=True).drop_vars("weights")

    # However, we need to be a little careful since the order of operation often matters. 
    # For example, if you want to create the weighted ensemble climatology, 
    # it is likely best to calculate the climatology first for each model, then apply the multimodel weights.
    ds_clim_historical = ds.sel(time=slice('1981-01-01','2010-12-31')).mean(dim="time")
    ds_clim_future = ds.sel(time=slice(start_dates[0], end_dates[0])).mean(dim="time")
    weighted_ds_clim_historical = ds_clim_historical.weighted(ds_clim_historical.weights).mean(dim="ensemble", keep_attrs=True).drop_vars("weights")
    weighted_ds_clim_future = ds_clim_future.weighted(ds_clim_future.weights).mean(dim="ensemble", keep_attrs=True).drop_vars("weights")
    weighted_ds_clim_change = weighted_ds_clim_future - weighted_ds_clim_historical
    
    # plot example
    #weighted_ds_clim_change.prmax1day.plot()
    #weighted_ds_clim_change.pr_above_nonzero_99th.plot()

    # You are pretty much done at this point. You could save variables out to a new NetCDF or just do plotting. 
    # As I showed above, I would favor doing whatever averaging or summary you need to do first, 
    # and then apply the ensemble multimodel weights as the last step.
    # 
    # You are probably fine to spatially average grids -> HUCs using the weighted ensemble, 
    # but I would do temporal averaging before the model weighting just in case the order of operation matters.
    start_year = start_dates[0].split('-')[0]
    end_year = end_dates[0].split('-')[0]
    for v in list(weighted_ds_clim_change.keys())[1:len(weighted_ds_clim_change.keys())]:
        nc_filename = "F:/Wetland_Climate_Impacts/Climate_Data/USGS_Thresholds_Weighted/Grid/pr/bma_weighted_differences/{}/NCA5_BMA_Weighted_Ensemble_Difference_{}_{}_{}_{}.nc".format(scenario, v, scenario, start_year, end_year)
        weighted_ds_clim_change[v].to_netcdf(nc_filename)

# In[5]:

# Due to the size of the data, we will use Dask to calculate the weighted multimodel mean.
#hostname = platform.uname()[1]
#cluster = LocalCluster() 
#client = Client(cluster)
#print(f"Dask Dashboard is available at: {client.dashboard_link.replace("127.0.0.1", hostname)}")

if __name__ == '__main__':
    multiprocessing.freeze_support()
    for s in scenarios:
        p = multiprocessing.Process(target=create_ensemble, args=s)
        p.start()
        p.join() 
        print("Main process finished")
    
# Close the Dask dashboard.
#client.close()
