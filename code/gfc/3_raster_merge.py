# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.5.0
#   kernelspec:
#     display_name: GIS
#     language: python
#     name: gds
# ---

# %% Collapsed="false"
import os
import numpy as np
import pandas as pd

import matplotlib.pyplot as plt
import seaborn as sns
import geopandas as gpd

from pathlib import Path

from rasterstats import zonal_stats
import rasterio
from rasterio.merge import merge
from rasterio.plot import show
import glob, os
# run for jupyter notebook
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

# %% [markdown] Collapsed="false"
# ### Folder Structure

# %% Collapsed="false"
#%% Read in data
rice_root = '/home/apoorval/Research/GeoSpatial/India_Forests/'
dbox_root = '/home/alal/res/India_Forests/'
root = Path(dbox_root)
code = root / 'Code'
data = root / 'Data'
spatial = data/'Spatial'

# %% Collapsed="false"
# %%time
vil_treat = gpd.read_file(data/"Spatial/Processed/Village_stack_treatment.gpkg")
vil_treat.head()

# %% Collapsed="false"
deforestation = root / 'Data/Spatial/Rasters/Hansen_GFC-2017-v1.5_lossyear_30N_080E.tif'
ex_ante       = root / 'Data/Spatial/Rasters/Hansen_GFC-2017-v1.5_treecover2000_30N_080E.tif'

with rasterio.open(deforestation) as src:
    print(src.profile)

with rasterio.open(ex_ante) as src:
    print(src.profile)

# %% [markdown] Collapsed="false"
# # Dry Run

# %% [markdown] Collapsed="false"
# ## Raster Merge 

# %% [markdown] Collapsed="false"
# ### Dry Run 

# %% [markdown] Collapsed="true"
# ### 1 district 

# %% Collapsed="false"
sim_def =  gpd.GeoDataFrame.from_features(zonal_stats(simdega, 
                    deforestation, prefix = 'deforest_',
                    stats='count', nodata=-1, categorical=True, geojson_out = True))

# %% Collapsed="false"
sim_def.head()

# %% Collapsed="false"
sim_def['deforest_4'].value_counts()

# %% [markdown] Collapsed="false"
# ## Run Merge 

# %% [markdown] Collapsed="true"
# ### 1 State 

# %% Collapsed="false"
j_def =  gpd.GeoDataFrame.from_features(zonal_stats(jharkhand, deforestation, 
                    prefix = 'deforest_',
                    stats='count', nodata=-1, categorical=True, geojson_out = True))

# %% Collapsed="false"
j_def.to_file(data + '/Spatial/Processed/jharkhand_village_deforestation.shp')

# %% [markdown] Collapsed="false"
# # Merge All 

# %% Collapsed="false"
rasters = root / 'Data/Spatial/Rasters/'
# %ls {rasters}

# %% Collapsed="false"
def_master = rasters / '_Hansen_GFC_lossyear_all.tif'

# %% Collapsed="false"
with rasterio.open(def_master) as src:
    print(src.profile)

# %% Collapsed="false"
# %%time
all_def = gpd.GeoDataFrame.from_features(zonal_stats(vil_treat, def_master,   
            prefix = 'deforest_', stats='count', nodata=-1, 
            categorical=True, geojson_out=True))
all_def.crs = vil_treat.crs

# %% Collapsed="false"
all_def.head()
all_def.shape

# %% Collapsed="false"
# %%time
all_def.to_file(spatial/'Processed/village_stack_deforestation.gpkg', driver = "GPKG")

# %% [markdown] Collapsed="false"
# # Ex-Ante Forest Levels 

# %% Collapsed="false"
rasters = spatial/ 'Rasters/'
# %ls {rasters}

# %% [markdown] Collapsed="false"
# ## Raster Merge 

# %% Collapsed="false"
ea_master = rasters / '_Hansen_GFC_treecover2000_all.tif'

# %% Collapsed="false"
with rasterio.open(ea_master) as src:
    print(src.profile)

# %% Collapsed="false"
# %%time
all_def = gpd.GeoDataFrame.from_features(zonal_stats(vil_treat, ea_master,   
            prefix = 'preF_', nodata=-1, geojson_out = True))
all_def.crs = vil_treat.crs

# %% Collapsed="false"
all_def.head()
all_def.shape

# %% Collapsed="false"
# %%time
all_def.to_file(spatial/'Processed/village_stack_ex_ante.gpkg', driver = "GPKG")
