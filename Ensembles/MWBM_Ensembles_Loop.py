# coding: utf-8

# Sample code for applying model weights to the USGS NCCV hydrology files found at: 
# https://www.sciencebase.gov/catalog/item/651c8100d34e44db0e2ce2d9
# 
# This code requires the NCCV2_LOCA2_model_extended_meta.csv file to load either 
# LOCA2-specific model weights or the weights found in the NCA5 assessment.

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

# Define your paths and which variables you'd like to include. Keep in mind, 
# xarray uses lazy loading, so variables you don't use won't be loaded anyway.

data_path = 'F:/Wetland_Climate_Impacts/Climate_Data/USGS_Water_Balance'
meta_data_file = 'F:/Wetland_Climate_Impacts/Climate_Data/USGS_Water_Balance/NCCV2_LOCA2_model_extended_meta.csv'
variables = ['aet','deficit','pet','runoff','snow','stor']
weight_type = 'NCA5_BMA_weight' # or 'LOCA2_BMA_weight'

# In[3]:

# When using the LOCA2-specific weights, the number of models vary based on SSP (ssp245=24, ssp370=23, ssp585=25 models). 
# Since the number of models vary, the weighted ensemble mean will be slightly different because of the inclusion 
# or exclusion of one or two models. For the sake of the example, I am only processing one SSP at a time.
# 
# If you want to use the NCA5_BMA_weight, there are 16 GCMs regardless of SSP, 
# so you could use a single weighted historical for all SSP245, 370, and 585. 
# You could do the same with the LOCA2-specific weights if you manually made sure only to use the common models, 
# forcing n=23 for all scenarios.
# 
# Just be careful you aren't mixing different ensemble means that were made with a different balance of weights.

scenarios = np.array(['ssp245','ssp370','ssp585'])
n_s = len(scenarios)

start_dates = np.array(['2041-01-01','2071-01-01'])
end_dates = np.array(['2070-12-31','2100-12-31'])
n_y = len(start_dates)

mwbm_vars = np.array(['stor','runoff','deficit','aet','pet','snow'])

# In[4]:

# Using xarray's open_mfdataset function, we can easily open and combine all the files at once. 
# However, they need something to align on, so we use a preprocessor function (read the docs) 
# to find the CMIP metadata in each file and construct a ensemble ID (as we did the the metadata DataFrame). 
# We use this ensemble ID to create a new 4th dimension for each model. 
# This allows us to load all models at once.
#
# Some of the variables were loading as float64, which takes more memory and is not needed, 
# so I cast them all to float32 datatype.
    
def get_ensemble_id_from_netcdf_metadata(ds):
    ensemble = f"{ds.attrs['model']}.{ds.attrs['experiment']}.{ds.attrs['ripf']}"
    return ds.expand_dims('ensemble').assign_coords(ensemble=[ensemble]).astype('float32')

# In[5]:
    
def create_ensemble(scenario, start_date, end_date):      
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
        f'{data_path}/{var}/historical/{var}_MWBM_LOCA2.{ensemble_id.replace(scenario, 'historical')}_1950-2014.nc'
        for ensemble_id,var in product(list(weight_da.ensemble.values), variables)
    ]

    ssp_files = [
        f'{data_path}/{var}/{scenario}/{var}_MWBM_LOCA2.{ensemble_id}_2015-2100.nc'
        for ensemble_id,var in product(list(weight_da.ensemble.values), variables)
    ]

    # Using the preprocessor, we can now load and combine all historical files.
    ds_hist = xr.open_mfdataset(hist_files, 
                                chunks={}, 
                                preprocess=get_ensemble_id_from_netcdf_metadata, 
                                combine_attrs="drop_conflicts")

    # We do the same for the selected SSP files
    ds_ssp = xr.open_mfdataset(ssp_files, 
                               chunks={}, 
                               preprocess=get_ensemble_id_from_netcdf_metadata, 
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
    ds_clim_future = ds.sel(time=slice(start_date, end_date)).mean(dim="time")
    weighted_ds_clim_historical = ds_clim_historical.weighted(ds_clim_historical.weights).mean(dim="ensemble", keep_attrs=True).drop_vars("weights")
    weighted_ds_clim_future = ds_clim_future.weighted(ds_clim_future.weights).mean(dim="ensemble", keep_attrs=True).drop_vars("weights")
    weighted_ds_clim_change = weighted_ds_clim_future - weighted_ds_clim_historical
    
    # Let's plot snow just as an example. This is the point where xarray and Dask will start loading and processing data. 
    #This step can take a couple minutes. If you have the Dask dashboard open in your browser,
    # you should now see a lot of activity as it loads and processes the data.
    #weighted_ds_clim_change.aet.plot()
    #weighted_ds_clim_change.snow.plot()
    #weighted_ds_clim_change.deficit.plot()
    #weighted_ds_clim_change.pet.plot()
    #weighted_ds_clim_change.runoff.plot()
    #weighted_ds_clim_change['stor'].plot()

    # You are pretty much done at this point. You could save variables out to a new NetCDF or just do plotting. 
    # As I showed above, I would favor doing whatever averaging or summary you need to do first, 
    # and then apply the ensemble multimodel weights as the last step.
    # 
    # You are probably fine to spatially average grids -> HUCs using the weighted ensemble, 
    # but I would do temporal averaging before the model weighting just in case the order of operation matters.
    for v in mwbm_vars:
        print(v)
        nc_filename = "F:/Wetland_Climate_Impacts/Climate_Data/USGS_Water_Balance/weighted_differences/NCA5_BMA_Weighted_Ensemble_Difference_{}_{}_{}_{}.nc".format(v, scenario, start_date.split('-')[0], end_date.split('-')[0])
        weighted_ds_clim_change[v].to_netcdf(nc_filename)
        
# In[118]:

# Due to the size of the data, we will use Dask to calculate the weighted multimodel mean.
hostname = platform.uname()[1]
cluster = LocalCluster() 
client = Client(cluster)
print(f"Dask Dashboard is available at: {client.dashboard_link.replace("127.0.0.1", hostname)}")

if __name__ == '__main__':
    #create_ensemble(scenarios[0],start_dates[0],end_dates[0])
    #create_ensemble(scenarios[0],start_dates[1],end_dates[1])
    #create_ensemble(scenarios[1],start_dates[0],end_dates[0])
    #create_ensemble(scenarios[1],start_dates[1],end_dates[1])
    #create_ensemble(scenarios[2],start_dates[0],end_dates[0])
    create_ensemble(scenarios[2],start_dates[1],end_dates[1])

# Close the Dask dashboard.
#client.close()

