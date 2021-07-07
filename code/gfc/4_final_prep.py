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
from numpy import *
import pandas as pd

from pathlib import Path

import matplotlib.pyplot as plt
# import seaborn as sns
import geopandas as gpd

# from rasterstats import zonal_stats
import rasterio
from rasterio.merge import merge
from rasterio.plot import show
import glob, os
# run for jupyter notebook
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

# %%
import rtree
from shapely.geometry import shape, mapping, Point, LinearRing

# %% [markdown]
# ### Folder Structure

# %%
#%% Read in data
rice_root = '/home/apoorval/Research/GeoSpatial/India_Forests/'
dbox_root = '/home/alal/res/India_Forests/'
# saad_root = '~/Dropbox/india_forests/'
# root = dbox_root 
root = Path(dbox_root)
code = root / 'Code'
data = root / 'Data'
spatial = data / 'Spatial'

# %%
# %cd $spatial

# %% [markdown]
# # Read Village level files 

# %%
vil_1 = gpd.read_file("Processed/village_stack_deforestation.gpkg")
vil_2 = gpd.read_file("Processed/village_stack_ex_ante.gpkg")

# %%
vil_1.shape
vil_2.shape

# %%
[x for x in vil_1.columns]

# %%
[x for x in vil_1.columns if x not in vil_2.columns]
ea_cols = [x for x in vil_2.columns if x not in vil_1.columns]

# %%
ea_cols

# %% [markdown]
# ### Regular merge on index  

# %%
vil = pd.merge(vil_1, vil_2.loc[:, ea_cols], right_index = True, left_index = True)

# %%
vil.columns

# %% [markdown]
# # Create villages dataset with centroids and distance to relevant border

# %% [markdown]
# Steps to construct border
#
# + read in `BLOCKS_sch_coded.geojson`
# + filter by `sch == 1`
# + Vector Tools > Dissolve
# + Temporary Layer > Vector Tools > Polygons to line

# %% jupyter={"outputs_hidden": true}
border= gpd.read_file('Processed/boundary.geojson')

# %% [markdown]
# ## Centroids 

# %% jupyter={"outputs_hidden": true}
vil['centroid'] = vil.centroid
vil["x"] = vil.centroid.map(lambda p: p.x)
vil["y"] = vil.centroid.map(lambda p: p.y)
vil = vil.set_geometry('centroid')
vil.drop(['geometry'], inplace = True, axis=1)

# %% jupyter={"outputs_hidden": true}
vil.info()

# %% [markdown]
# ## Nearest segment of border 

# %% jupyter={"outputs_hidden": true}
index = rtree.index.Index()
for ind, row in border.iterrows():
    geom1 = shape(row['geometry'])
    index.insert(ind, geom1.bounds)

# %% jupyter={"outputs_hidden": true}
nearest_segment     = lambda x: list(index.nearest(x.bounds, 1))[0]
distance_to_nearest = lambda x: border.distance(x).min()

# %% jupyter={"outputs_hidden": true}
# %%time
vil['min_dist_to_border'] = vil.geometry.apply(distance_to_nearest)
vil['nearest_segment'] = vil.geometry.apply(nearest_segment)

# %% jupyter={"outputs_hidden": true}
#%% plot minimum distance
f, ax = plt.subplots(1,dpi=200)
vil.plot(column='min_dist_to_border', cmap='viridis',
    markersize=0.2,ax=ax)
border.plot(edgecolor='red', ax=ax)
ax.set_axis_off()

# %% jupyter={"outputs_hidden": true}
vil.rename(columns = {'x': 'lon', 'y': 'lat'}, inplace = True)

# %% jupyter={"outputs_hidden": true}
vil.shape

# %% jupyter={"outputs_hidden": true}
# %%time 
vil.to_file(spatial/'Processed/all_villages_data.gpkg', driver='GPKG')

# %% [markdown]
# ## write csv for prelim analysis 

# %% jupyter={"outputs_hidden": true}
vil.columns

# %% jupyter={"outputs_hidden": true}
df = pd.DataFrame(vil.drop(columns=['centroid']))

# %% jupyter={"outputs_hidden": true}
df.to_csv(data / 'Intermediate/villages_points_all2.csv')
