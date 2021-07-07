# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.5.0
#   kernelspec:
#     display_name: Python 2
#     language: python
#     name: python2
# ---

# %% [markdown]
# ## Libraries 

# %%
import geopandas as gpd
import rasterio
import rasterio.plot
import pyproj
import numpy as np
import matplotlib
import matplotlib.pyplot as plt

# %%
from rasterstats import zonal_stats

# %%
import matplotlib.pyplot as plt
import seaborn as sns
plt.style.use("seaborn-darkgrid")

# %matplotlib inline
# run for jupyter notebook
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

# %% [markdown]
# ## Folders 

# %%
# %pwd

# %%
#%% Read in data
rice_root = '/home/apoorval/Research/GeoSpatial/India_Forests/'
dbox_root = '/home/alal/Dropbox/1_Research/India_Forests/'
root = rice_root
code = root + 'Code'
data = root + 'Data'

# %%
# root = '/home/apoorval/Research/India_Forests/'
# root = '/home/alal/Dropbox/1_Research/India_Forests/'
# %cd $root
# %ls Data/Spatial/Rasters/

# %%
# %ls Data/Spatial/Vectors/
# %ls Data/Spatial/Vectors/2001/

# %%
blocks = gpd.read_file('Data/Spatial/Vectors/2001/BLOCKMAP.shp')

# %%
blocks.shape

# %%
statenames = [s for s in list(blocks.STATE_UT.unique()) if s is not None]
statenames

# %%
state_list = [
    'Orissa', 'Maharashtra', 'Rajasthan', 'Jharkhand', 'Chhattisgarh',
    'Madhya Pradesh', 'Gujarat', 'Himachal Pradesh', 'Andhra Pradesh'
]

# %% [markdown]
# ### Subset block map to 9 states 

# %%
blocks_state = blocks[blocks.STATE_UT.isin(state_list)]
blocks_state.shape

# %%
del blocks

# %%
len(blocks_state.STATE_UT.unique())

# %%
blocks_state.columns

# %%
keepcols = [
    'BLOCK_ID', 'NAME', 'DISTRICT', 'STATE_UT', 'C_CODE01',
    'TOT_NM_HH', 'TOT_POP', 'geometry'
]

blocks_state = blocks_state[keepcols]

# %%
blocks_state.info()

# %% [markdown]
# #### Jharkhand 

# %%
jharkhand = blocks_state.loc[blocks_state.STATE_UT == 'Jharkhand', :]
jharkhand.shape

# %%
jharkhand.plot(edgecolor='k')

# %%
blocks_state.plot(edgecolor='k')

# %% [markdown]
# ## Ingest forest data into Matrix

# %% [markdown]
# [data source](http://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.5.html)

# %%
deforestation = 'Data/Spatial/Rasters/Hansen_GFC-2017-v1.5_lossyear_30N_080E.tif'
ex_ante       = 'Data/Spatial/Rasters/Hansen_GFC-2017-v1.5_treecover2000_30N_080E.tif'

# %%
with rasterio.open(deforestation) as src:
    print(src.profile)

# %%
with rasterio.open(ex_ante) as src:
    print(src.profile)

# %% [markdown]
# ### Raster merge 

# %% [markdown]
# ### Ex-Ante Forests 

# %%
jharkhand_base = gpd.GeoDataFrame.from_features(
    zonal_stats(jharkhand, ex_ante, prefix='ex_ante_',
                                      nodata=-1,
                                      geojson_out=True))

# %%
jharkhand_base.head()

# %%
sns.distplot(jharkhand_base.ex_ante_mean,kde=True,rug=True)

# %%
sns.distplot(jharkhand_base.ex_ante_max,kde=True,rug=True)

# %%
f, ax = plt.subplots(1, figsize=(12, 12))
jharkhand_base.plot(column='ex_ante_max',
                             scheme='QUANTILES', alpha=1, k=5, cmap=plt.cm.YlGn, 
                             ax = ax, legend=True)
leg = ax.get_legend()
leg.set_bbox_to_anchor((0., 0., 0.2, 0.2))
plt.axis('off');

# %% [markdown]
# ### Deforestation 

# %%
jharkhand_deforestation = gpd.GeoDataFrame.from_features(zonal_stats(jharkhand, 
                                      deforestation, prefix='def_',
                                      nodata=-99,stats='count', categorical=True,  
                                      geojson_out=True))

# %%
jharkhand_deforestation.head()

# %%
f, ax = plt.subplots(1, figsize=(12, 12))
jharkhand_deforestation.plot(column='def_mean',
                             scheme='fisher_jenks', alpha=1, k=4, cmap=plt.cm.RdPu_r, 
                             ax = ax, legend=True)
leg = ax.get_legend()
leg.set_bbox_to_anchor((0., 0., 0.2, 0.2))
plt.axis('off');

# %% [markdown]
#

# %%
