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

# %% Collapsed="false"
#%% Read in data
root = Path('/home/alal/res/India_Forests/')
code = root / 'Code'
data = root / 'Data'
spatial = data/'Spatial'

# %% [markdown] Collapsed="false"
# ## Ingest Buffers around Treated Blocks

# %% Collapsed="false"
block_buf = gpd.read_file(spatial/"Processed/BLOCKS_sch_coded.geojson")
states    = gpd.read_file(spatial/"Vectors/IND_adm1.shp")

# %% Collapsed="false"
states['bound'] = states.boundary
states.geometry = states['bound']
# subset
blocks = block_buf.loc[:, ['sch', "named", "nameb", 'geometry']] 

# %% Collapsed="false"
w, s, e, n = blocks.total_bounds

# %% Collapsed="false"
f, ax = plt.subplots(1, figsize=(12,12))
blocks.plot(column='sch', categorical=True, cmap = 'viridis', legend=True,
            edgecolor='r',linewidth=0.3,ax=ax)
states.plot(edgecolor='g',linewidth=0.6,ax=ax)
ax.set_xlim(w, e)
ax.set_ylim(s, n)
ax.set_title('Reserved and Unreserved Blocks under PESA \n 5th Schedule States')
ax.set_axis_off()

# %% [markdown] Collapsed="false"
# # Raster Merge 

# %% Collapsed="false"
rasters = root / 'Data/Spatial/Rasters/'
# %ls {rasters}

# %% [markdown]
# ## Mosaic

# %%
rasterlist = [
    'Hansen_GFC-2017-v1.5_treecover2000_20N_070E.tif',
    'Hansen_GFC-2017-v1.5_treecover2000_20N_080E.tif',
    'Hansen_GFC-2017-v1.5_treecover2000_30N_070E.tif',
    'Hansen_GFC-2017-v1.5_treecover2000_30N_080E.tif',
    'Hansen_GFC-2017-v1.5_treecover2000_40N_070E.tif'
]

# %%
src_files_to_mosaic = []
for fp in rasterlist:
    src = rasterio.open(rasters/fp)
    src_files_to_mosaic.append(src)

# %%
 src_files_to_mosaic

# %%
mosaic, out_trans = merge(src_files_to_mosaic)

# %%
out_meta = src.meta.copy()

out_meta.update({"driver": "GTiff",
    "height": mosaic.shape[1],
    "width": mosaic.shape[2],
    "transform": out_trans,
    "crs": "EPSG:4326"})

# %%
# %%time
out_fp = "/home/alal/tmp/mosaic_treecover.tif"
with rasterio.open(out_fp, "w", **out_meta) as dest:
    dest.write(mosaic)

# %% [markdown]
# ## Plot

# %% [markdown]
# This crashes the kernel

# %%
# %%time
f, ax = plt.subplots(1, 2, figsize=(18,12))
blocks.plot(column='sch', categorical=True, cmap = 'viridis', legend=True,
            edgecolor='r',linewidth=0.3,ax=ax[0])
states.plot(edgecolor='g',linewidth=0.6,ax=ax[0])
ax[0].set_xlim(w, e)
ax[0].set_ylim(s, n)
ax[0].set_title('Reserved and Unreserved Blocks under PESA \n 5th Schedule States')
ax[0].set_axis_off()

show(src, 
     title="Forest Cover in 2000", 
     ax=ax[1])
states.plot(edgecolor='g',linewidth=0.6,ax=ax[1])
ax[1].set_xlim(w, e)
ax[1].set_ylim(s, n)
ax[0].set_axis_off()

# %%
