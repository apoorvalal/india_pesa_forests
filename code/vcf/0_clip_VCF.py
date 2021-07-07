# %% # system
import os, sys, glob, re, itertools, collections, multiprocessing, requests, pprint, pickle
from pathlib import Path
from joblib import delayed, Parallel

# pyscience imports
import numpy as np
import pandas as pd
# viz
import matplotlib
import matplotlib.pyplot as plt
import seaborn as sns
from plotnine import *
sns.set(style="ticks", context="talk")
font = {'family' : 'IBM Plex Sans', 'weight' : 'normal', 'size'   : 10}
plt.rc('font', **font)
plt.rcParams['figure.figsize'] = [10, 10]
matplotlib.style.use(['seaborn-talk', 'seaborn-ticks', 'seaborn-whitegrid'])
%matplotlib inline
%config InlineBackend.figure_format = 'retina'


# %%
# geodata packages
import geopandas as gpd
import rasterio
import rasterio.mask
import georasters as gr

# show all output
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

# %%
rice_root = '/home/apoorval/Research/GeoSpatial/India_Forests/'
dbox_root = '/home/alal/res/india_pesa_forests/'
root = Path(dbox_root)
data = root / 'inp'
spatial = data/'spatial'

 ######  ##       #### ########
##    ## ##        ##  ##     ##
##       ##        ##  ##     ##
##       ##        ##  ########
##       ##        ##  ##
##    ## ##        ##  ##
 ######  ######## #### ##


# %% india outline for clipping
indmap = gpd.read_file(spatial/"IND_adm0.shp")
geom = indmap.geometry
ind_bounds = indmap.bounds
# %%
rast_inp_dir = data/"VCF_Rasters/"
rasters = glob.glob(str(rast_inp_dir/"*.tif"))
# rast_path = str(rast_inp_dir/"VCF5KYR_2001001_001_2018224205557.tif")
# %%
def clip_and_slice(rast_path):
        yname = rast_path.split("/")[-1][8:12]
        rast = rasterio.open(str(rast_path))
        out_image, out_transform = rasterio.mask.mask(rast, shapes = geom,
                                                crop = True)
        out_meta = rast.meta
        tree_cover = out_image[0, :, :]
        out_meta.update({"count": 1,
                        "driver": "GTiff",
                        "height": out_image.shape[1],
                        "width": out_image.shape[2],
                        "transform": out_transform})
        with rasterio.open(str(root/f"tmp/clipped_rasters/treecover_{yname}.tif"), "w", **out_meta) as dest:
                dest.write(tree_cover, 1)
        return tree_cover

# %%
%%time
rasters_clipped = {r: clip_and_slice(r) for r in rasters}
# %%
########     ###     ######  ########  ######   ########  #### ########
##     ##   ## ##   ##    ##    ##    ##    ##  ##     ##  ##  ##     ##
##     ##  ##   ##  ##          ##    ##        ##     ##  ##  ##     ##
########  ##     ##  ######     ##    ##   #### ########   ##  ##     ##
##   ##   #########       ##    ##    ##    ##  ##   ##    ##  ##     ##
##    ##  ##     ## ##    ##    ##    ##    ##  ##    ##   ##  ##     ##
##     ## ##     ##  ######     ##     ######   ##     ## #### ########

# %%
clipped_rasts = glob.glob(str(root/"tmp/clipped_rasters/*.tif"))
def rast_to_df(p):
        y = p.split("/")[-1].split(".")[0][-4:]
        rast   = gr.from_file(p)
        #  centroid of each cell
        df2 = rast.to_pandas()
        df2['x'] = df2.x + rast.x_cell_size/2
        df2['y'] = df2.y + rast.y_cell_size/2
        df2['year']   = int(y)
        return df2
# %%
%%time
raster_points = [rast_to_df(p) for p in clipped_rasts]
# %%
cell_values_stack = pd.concat(raster_points, axis = 0)
cell_values_stack.head()

# %%
# convert cell-id to string
cell_values_stack = cell_values_stack[['row', 'col', 'year', 'value', 'x', 'y']]
cell_values_stack.to_csv(root/'tmp/forest_cover_panel_cell_level.csv.gz',
                        compression="gzip")
# %%
cell_values_stack.info()
# %%
cell_values_stack.value.plot(kind='kde', xlim=(0,100))
# %%
