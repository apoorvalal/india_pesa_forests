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
from numpy import *
import pandas as pd

from pathlib import Path

import matplotlib.pyplot as plt
import seaborn as sns
import geopandas as gpd
# import mplleaflet 
# import folium
plt.style.use("seaborn-darkgrid")
# plt.style.use("dark_background") # dark bg plots
sns.set(style="ticks", context="talk")
# # %matplotlib inline
# run for jupyter notebook
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

# %% Collapsed="false"
# disable nonsensical pandas warning 
pd.options.mode.chained_assignment = None

# %% [markdown] Collapsed="false"
# ### Folder Structure

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

# %% Collapsed="false"
f.savefig(root / 'Output/treatment_map.pdf')

# %% [markdown] Collapsed="false" toc-hr-collapsed=false
# # Spatial Merge Villages from all (treatment/proximate) states with block treatments
# Bigger buffer; includes Western and Northern States 

# %% Collapsed="false"
villages = Path("/home/alal/Dropbox/_Data/India_GIS/CENSUS/2011/2011_census/village map")
# %cd $villages

# %% Collapsed="false"
# %%time
andhra      = gpd.read_file('VILLAGE MAP A - H/ANDHRA PRADESH.shp')
chhat       = gpd.read_file('VILLAGE MAP A - H/CHHATTISGARH.shp')
gujarat     = gpd.read_file('VILLAGE MAP A - H/GUJARAT.shp')
himan       = gpd.read_file('VILLAGE MAP A - H/HIMACHAL PRADESH.shp')
jharkhand   = gpd.read_file('VILLAGE MAP J - P/JHARKHAND.shp')
maharashtra = gpd.read_file('VILLAGE MAP J - P/MAHARASHTRA.shp') 
madhya      = gpd.read_file('VILLAGE MAP J - P/MADHYA PRADESH.shp')
orissa      = gpd.read_file('VILLAGE MAP J - P/ODISHA.shp') 
rajasthan   = gpd.read_file('VILLAGE MAP R - W/RAJASTHAN.shp')

# %% Collapsed="false"
# %%time
datalist = [jharkhand, orissa, chhat,  andhra,  gujarat, himan, madhya, 
            maharashtra, rajasthan]
village_stack = pd.concat(datalist, sort=False)

# %% Collapsed="false"
del datalist

# %% [markdown] Collapsed="false"
# ## Temporarily set geometry to centroid

# %% Collapsed="false"
villages = village_stack.copy()
villages.crs = blocks.crs
villages.head()

# %% Collapsed="false"
villages['centr'] = villages.centroid
villages = villages.set_geometry('centr')
villages.shape
villages.head()

# %% Collapsed="false"
# %%time
vil_treat = gpd.sjoin(villages, blocks, how='inner', op='within')

# %% Collapsed="false"
villages.shape
vil_treat.shape

# %% [markdown] Collapsed="false"
# ## Switch back

# %% Collapsed="false"
vv = vil_treat.drop(['centr'], axis = 1).set_geometry('geometry')
vv.geometry.head()

# %% Collapsed="false"
# %%time
f, ax = plt.subplots(1, figsize=(12,12))
vv.plot(column='sch', categorical=True, edgecolor='k',linewidth=0.3,ax=ax)
ax.set_axis_off()

# %% [markdown] Collapsed="false"
# # Process and Write 

# %% Collapsed="false"
n_blocks_vill = vv.groupby('CODE_2011').nameb.nunique().sort_values(ascending = False)
n_blocks_vill.head()

# %% Collapsed="false"
# %%time
vv.to_file(spatial/'Processed/Village_stack_treatment.gpkg', driver = 'GPKG')
