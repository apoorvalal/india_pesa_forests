# %%
import rasterio
from rasterio.merge import merge
from rasterio.plot import show
import glob
import os
from pathlib import Path
# run for jupyter notebook
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'


# %% Collapsed="false"
# %% Read in data
dbox_root = '/home/alal/Dropbox/1_Research/india_pesa_forests/'
root = Path(dbox_root)
data = root / 'inp/GFC_Rasters'
%cd $data
%ls

# %%
lossyear = [
    'Hansen_GFC-2019-v1.7_lossyear_20N_070E.tif',
    'Hansen_GFC-2019-v1.7_lossyear_20N_080E.tif',
    'Hansen_GFC-2019-v1.7_lossyear_30N_070E.tif',
    'Hansen_GFC-2019-v1.7_lossyear_30N_080E.tif',
    'Hansen_GFC-2019-v1.7_lossyear_40N_070E.tif'
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
out_meta.update(compress='lzw')

out_fp = '_Hansen_GFC_lossyear_all.tif'

with rasterio.open(out_fp, "w", **out_meta) as dest:
    dest.write(mosaic)

# %% [markdown] Collapsed="false"
# # Initial Tree cover

# %% Collapsed="false"
treecover = [
    'Hansen_GFC-2019-v1.7_treecover2000_20N_070E.tif',
    'Hansen_GFC-2019-v1.7_treecover2000_20N_080E.tif',
    'Hansen_GFC-2019-v1.7_treecover2000_30N_070E.tif',
    'Hansen_GFC-2019-v1.7_treecover2000_30N_080E.tif',
    'Hansen_GFC-2019-v1.7_treecover2000_40N_070E.tif'
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
out_meta.update(compress='lzw')

out_fp = '_Hansen_GFC_treecover2000_all.tif'
with rasterio.open(out_fp, "w", **out_meta) as dest:
    dest.write(mosaic)

# %% Collapsed="false"
