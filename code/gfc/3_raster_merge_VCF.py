# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.5.0
#   kernelspec:
#     display_name: 'Env: Geo'
#     language: python
#     name: gds
# ---

# %%
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

# %%
from joblib import Parallel, delayed
import functools

# %% [markdown]
# ### Folder Structure

# %%
#%% Read in data
rice_root = '/home/apoorval/Research/GeoSpatial/India_Forests/'
dbox_root = '/home/alal/res/india_pesa_forests/'
root = Path(dbox_root)
data = root / 'inp'
spatial = data/'spatial'

# %% [markdown]
# # Polygon prep

# %%
# %%time
vil_treat = gpd.read_parquet(spatial/"Village_stack_treatment.spq")
vil_treat.head()

# %%
# %%time
vil_mcols = vil_treat[['STATE_UT', 'CODE_2011', 'geometry']].dissolve(by="CODE_2011")

# %%
vil_mcols.groupby(['CODE_2011']).size().value_counts()
vil_mcols.set_index("CODE_2011", inplace = True)
vil_mcols.to_parquet(root/"tmp/vil_to_merge.spq")

# %% [markdown]
# # Dry Run

# %%
vil_mcols = gpd.read_parquet(root/"tmp/vil_to_merge.spq")

# %%
vcf_rasters = glob.glob(str(root/"inp/**/*.tif"), recursive = True)
vcf_rasters[:5]

# %%
rast = rasterio.open(vcf_rasters[0])

# %%
show((rast, 1), cmap = "viridis")
show((rast, 2), cmap = "viridis")
show((rast, 3), cmap = "viridis")

# %% [markdown]
# ## reproject village data before zonal stats

# %%
# %%time
vil_reprojected = vil_mcols.to_crs(rast.crs.data)
vil_reprojected.crs


# %% [markdown]
# ## zonal stats func

# %%
def vil_zonal_stats(fp, polygons = vil_reprojected,
                    stat = ['min', 'max', 'mean', 'median', 'majority']):
    name = fp.split("/")[-1].split(".")[0][8:12]
    rast = rasterio.open(fp)
    array = rast.read(1)
    # Get the affine
    affine = rast.transform
    gdf1 = gpd.GeoDataFrame.from_features(
                zonal_stats(polygons, array, affine=affine,
                geojson_out = True, nodata = -1, stats = stat)
            )
    gdf1.index = vil_mcols.index
    remaps = dict(zip(stat, [s + "_" + name for s in stat]))
    gdf1.rename(remaps, axis = 1, inplace = True)
    gdf1.drop("geometry", axis = 1, inplace = True)
    return gdf1
# %%


# %%
# %%time
df = vil_zonal_stats(vcf_rasters[0])
# df.head()

# %%
df.mean_1982.describe()
df.mean_1982.isna().value_counts()

# %% [markdown]
# ## Raster Merge 

# %%
# %%time
list_cov_gdf = Parallel(n_jobs=3)(delayed(vil_zonal_stats)(f) for f in vcf_rasters)

# %%
# %%time
forest_cols = functools.reduce(lambda x, y: pd.merge(x, y,
        left_index = True, right_index = True),
    list_cov_gdf)

# %%
forest_cols.info(max_cols = 200)

# %%
forest_cols.drop(["STATE_UT_x", "STATE_UT_y"], axis = 1, inplace = True)

# %%
forest_cols.to_csv(root/"tmp/forest_long_time_series.csv")

# %% [markdown]
# ## plot

# %%
import georasters as gr
from shapely.geometry import box

# %%
vil_w_forest = vil_reprojected.merge(forest_cols, left_index = True, right_index = True)
vil_w_forest.info()

# %%
# %%time
vil_w_forest = vil_w_forest.to_crs("epsg:4326")

# %%
vil_w_forest['mean_2001'] = vil_w_forest['mean_2001'].fillna(0)
jh = vil_w_forest.query("STATE_UT == 'Jharkhand'")

# %%
inpath = root/'inp/VCF_Rasters/VCF5KYR_2001001_001_2018224205557.tif'
rast = rasterio.open(str(inpath))

# %%
# %%time
outline = jh.dissolve(by = "STATE_UT")

# %%
bbox = box(outline.bounds)

# %%
show((rast, 1), cmap='viridis')

# %%

f, ax = plt.subplots(1, 2, figsize = (10, 12), dpi = 120)
# polygon outline
# categorical polygon
jh.plot(column = 'mean_2001', 
                cmap = 'viridis', edgecolor = None, ax = ax[0]
             )
# title
ax[0].set_title("Forest Cover (VCF)\n Jharkhand, 2001")
# set axes off
ax[0].set_axis_off()



f.savefig(root/'tmp/jhar_map.pdf')

# %%
