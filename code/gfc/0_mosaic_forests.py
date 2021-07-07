# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.5.0
#   kernelspec:
#     display_name: gis
#     language: python
#     name: gds
# ---

# %% Collapsed="false"
import rasterio
from rasterio.merge import merge
from rasterio.plot import show
import glob, os
from pathlib import Path
# run for jupyter notebook
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

# %% Collapsed="false"
# %pwd

# %% Collapsed="false"
#%% Read in data
rice_root = '/home/apoorval/Research/GeoSpatial/India_Forests/'
dbox_root = '/home/alal/Dropbox/1_Research/India_Forests/'
root = Path(dbox_root)
code = root / 'Code'
data = root / 'Data'

# %% Collapsed="false"
# %cd $root
# %cd Data/Spatial/Rasters/

# %% [markdown] Collapsed="false"
# # Lossyear 

# %% Collapsed="false"
lossyear = [
    'Hansen_GFC-2017-v1.5_lossyear_20N_070E.tif',
    'Hansen_GFC-2017-v1.5_lossyear_20N_080E.tif',
    'Hansen_GFC-2017-v1.5_lossyear_30N_070E.tif',
    'Hansen_GFC-2017-v1.5_lossyear_30N_080E.tif',
    'Hansen_GFC-2017-v1.5_lossyear_40N_070E.tif'
]

lossyear_maker = True

# %% Collapsed="false"
# %%time

src_files_to_mosaic = []
for fp in lossyear:
    src = rasterio.open(fp)
    src_files_to_mosaic.append(src)

mosaic, out_trans = merge(src_files_to_mosaic)

out_meta = src.meta.copy()

# %% Collapsed="false"
# Update the metadata
out_meta.update({"driver": "GTiff", "height": mosaic.shape[1], 
                 "width": mosaic.shape[2], 
                 "transform": out_trans} 
)
out_meta.update(compress = 'lzw')

out_fp = '_Hansen_GFC_lossyear_all.tif'

with rasterio.open(out_fp, "w", **out_meta) as dest:
    dest.write(mosaic)

# %% [markdown] Collapsed="false"
# # Initial Tree cover 

# %% Collapsed="false"
treecover = [
    'Hansen_GFC-2017-v1.5_treecover2000_20N_070E.tif',
    'Hansen_GFC-2017-v1.5_treecover2000_20N_080E.tif',
    'Hansen_GFC-2017-v1.5_treecover2000_30N_070E.tif',
    'Hansen_GFC-2017-v1.5_treecover2000_30N_080E.tif',
    'Hansen_GFC-2017-v1.5_treecover2000_40N_070E.tif'
]


# %% Collapsed="false"
# %%time
src_files_to_mosaic = []
for fp in treecover:
    src = rasterio.open(fp)
    src_files_to_mosaic.append(src)
mosaic, out_trans = merge(src_files_to_mosaic)
out_meta = src.meta.copy()
# Update the metadata
out_meta.update({"driver": "GTiff", "height": mosaic.shape[1], 
                 "width": mosaic.shape[2],
    "transform": out_trans} 
)
out_meta.update(compress = 'lzw')

out_fp = '_Hansen_GFC_treecover2000_all.tif'
with rasterio.open(out_fp, "w", **out_meta) as dest:
    dest.write(mosaic)

# %% Collapsed="false"
