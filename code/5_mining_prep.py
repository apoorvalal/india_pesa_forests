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
import os, sys, glob
import re

from pathlib import Path
import numpy as np
import pandas as pd
import seaborn as sns

import matplotlib.pyplot as plt
font = {'family' : 'IBM Plex Sans',
        'weight' : 'normal',
        'size'   : 10}
plt.rc('font', **font)


from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

# parallel
from multiprocessing import Pool
from joblib import Parallel, delayed

# geo
import geopandas as gpd
import rtree
from shapely.geometry import shape, mapping, Point, LinearRing

# %%
home_root = '/home/alal/Dropbox/1_Research/India_Forests'
yen_root  = '/home/users/apoorval/Research/India_Forests'
root = Path(home_root)

# %%
code = root / 'Code'
data = root / 'Data'
dataout = '/home/alal/Dropbox/1_Research/india_pesa_forests/inp/'
spatial = data / 'Spatial'

# %% [markdown]
# # Mining Atlas 

# %%
# %%time 
vil = pd.read_csv(data / 'Intermediate/villages_points_all2.csv')
vil_g = gpd.GeoDataFrame(vil, geometry=gpd.points_from_xy(vil.lon, vil.lat))

# %%
df = pd.read_stata(root/'Data/Admin/india-mines/atlas_clean.dta')
df.info()

# %%
df.head()

# %%
(df[['metal_class', 'locality']].groupby('locality')
    .count().sort_values(by = 'metal_class', ascending = False)
)

# %%
dropminerals = ['limestone', 'fire clay', 'clay', 'quartz glass/silica sand',
                'mica', 'ochre', 'talc', 'gypsum', 'salt']
df2 = df.loc[~df.mineral.isin(dropminerals)]

# %%
df.shape
df2.shape

# %%
df2.metal_class.value_counts()
df2.mineral.value_counts()

# %% [markdown]
# ## Convert to Geodata 

# %%
mines = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.longitude, df.latitude))
mines.mineral.nunique()

# %%
top20 = mines.mineral.value_counts().nlargest(20).index

# %% [markdown]
# # Maps

# %%
states = gpd.read_file("/home/alal/Dropbox/_Data/India_spatial_pipeline/data/ADM/IND_adm1.shp")

# %%
f, ax = plt.subplots(1,figsize = (10, 12), dpi = 200)
states.plot(facecolor = 'None',linewidth = 0.5, edgecolor = 'k', ax = ax)
mines.loc[mines.mineral.isin(top20)].plot(column = 'mineral', categorical = True, 
            markersize = 0.8 , ax = ax,
            legend = True, cmap = 'tab20',
            legend_kwds = {
                'loc': 'lower right', 'markerscale': 0.3, 'ncol': 2,
                'prop': {'size': 10}})
ax.set_axis_off()
ax.set_title("Locations of Mines in India \n 20 most common minerals \n State borders overlaid")
f.savefig(root/'out/mine_map.pdf')

# %% [markdown]
# ### Iron

# %%
iron = mines.query('metal_class == "Iron and Ferro-Alloy Metals"')
iron.shape

# %%
f, ax = plt.subplots(1,figsize = (10, 12), dpi = 200)
states.plot(facecolor = 'None',linewidth = 0.5, edgecolor = 'k', ax = ax)
iron.plot(column = 'mineral', categorical = True, 
            markersize = 0.8 , ax = ax,
            legend = True, cmap = 'tab20',
            legend_kwds = {
                'loc': 'lower right', 'markerscale': 0.3, 'ncol': 2,
                'prop': {'size': 10}})
ax.set_axis_off()
ax.set_title("Locations of Iron Mines in India \n State borders overlaid")
f.savefig(root/'out/iron_mine_map.pdf')

# %% [markdown]
# # Distance to Mines computation 

# %% [markdown]
# ## Distance Computation - overall

# %%
index = rtree.index.Index()
for ind, row in mines.iterrows():
    geom1 = shape(row['geometry'])
    index.insert(ind, geom1.bounds)

# %% jupyter={"outputs_hidden": true}
nearest_segment     = lambda x: list(index.nearest(x.bounds, 1))[0]
distance_to_nearest = lambda x: mines.distance(x).min()

# %% jupyter={"outputs_hidden": true}
# %%time
vil_g['min_dist_to_mine'] = vil_g.geometry.apply(distance_to_nearest)
vil_g['nearest_mine_id']  = vil_g.geometry.apply(nearest_segment)

# %%
Client().send_message("Mine Distance Computation done")

# %%
sns.distplot(vil_g.min_dist_to_mine)

# %% [markdown]
# ### By mineral 

# %%
minerals = mines.mineral.unique()


# %%
def distance_compute(m):
    mines_subset = mines.loc[mines.mineral == m]
    distance_to_mine = lambda x: mines_subset.distance(x).min()
    d = vil_g.geometry.apply(distance_to_mine)


# %%
# %%time
distances = Parallel(n_jobs = 10, verbose = 50)(delayed(
    distance_compute)(m) for m in minerals
    )
Client().send_message("Mineral-wise distance Computation done")

# %% [markdown]
# ### collate and write 

# %%
distances_df = pd.concat(distances, axis = 1)
distances_df.columns = ['min_dist_to_' + re.sub("( |/)", "", m) for m in minerals]

# %%
vil2 = pd.concat([vil_g, distances_df], axis = 1)

# %% [markdown]
# ### Write 

# %%
vil2 = pd.DataFrame(vil2.drop('geometry', axis = 1))

# %% jupyter={"outputs_hidden": true}
vil2.to_csv(data/"Intermediate/village_points_all_mines.csv")

# %% [markdown]
# # Intensive margin - count number of mines in buffer

# %% [markdown]
# ## 5 km

# %%
sindex = mines.sindex
len(sindex.leaves())

# %%
# %%time 
results_list = []
# iterate over the points
for index, row in vil_g.iterrows():
    buffer = row['geometry'].buffer(0.05)  # buffer
    # find approximate matches with r-tree, then precise matches from those approximate ones
    possible_matches_index = list(sindex.intersection(buffer.bounds))
    possible_matches = mines.iloc[possible_matches_index]
    precise_matches = possible_matches[possible_matches.intersects(buffer)]
    results_list.append(len(precise_matches))

# %%
results_list[:5]

# %%
vil_g['mines_in_5k'] = pd.Series(results_list)

# %%
sns.distplot(vil_g.mines_in_5k)

# %%
vil_g.mines_in_5k.describe()

# %% [markdown]
# ## 10 km

# %%
# %%time 
results_list = []
# iterate over the points
for index, row in vil_g.iterrows():
    buffer = row['geometry'].buffer(0.1)  # buffer
    # find approximate matches with r-tree, then precise matches from those approximate ones
    possible_matches_index = list(sindex.intersection(buffer.bounds))
    possible_matches = mines.iloc[possible_matches_index]
    precise_matches = possible_matches[possible_matches.intersects(buffer)]
    results_list.append(len(precise_matches))

# %%
results_list[:5]

# %%
vil_g['mines_in_10k'] = pd.Series(results_list)

# %%
sns.distplot(vil_g.mines_in_10k)

# %%
vil_g.mines_in_10k.describe()

# %% [markdown]
# ## 50 km

# %%
# %%time 
results_list = []
# iterate over the points
for index, row in vil_g.iterrows():
    buffer = row['geometry'].buffer(0.5)  # buffer
    # find approximate matches with r-tree, then precise matches from those approximate ones
    possible_matches_index = list(sindex.intersection(buffer.bounds))
    possible_matches = mines.iloc[possible_matches_index]
    precise_matches = possible_matches[possible_matches.intersects(buffer)]
    results_list.append(len(precise_matches))

# %%
results_list[:5]

# %%
vil_g['mines_in_50k'] = pd.Series(results_list)

# %%
sns.distplot(vil_g.mines_in_50k)

# %%
vil_g.mines_in_50k.describe()

# %%

# %%
# %%time 
results_list = []
# iterate over the points
for index, row in vil_g.iterrows():
    buffer = row['geometry'].buffer(0.1)  # buffer
    # find approximate matches with r-tree, then precise matches from those approximate ones
    possible_matches_index = list(sindex.intersection(buffer.bounds))
    possible_matches = mines.iloc[possible_matches_index]
    precise_matches = possible_matches[possible_matches.intersects(buffer)]
    results_list.append(len(precise_matches))

# %%
results_list[:5]

# %%
vil_g['mines_in_10k'] = pd.Series(results_list)

# %%
sns.distplot(vil_g.mines_in_10k)

# %%
vil_g.mines_in_10k.describe()

# %%
# %%time
pd.DataFrame(vil_g.drop('geometry', axis = 1)).to_csv(data/"Intermediate/village_points_mine_counts.csv")

# %% [markdown]
# # Map v2 with village points

# %%
states = gpd.read_file("/home/alal/Dropbox/_Data/India_spatial_pipeline/data/ADM/IND_adm1.shp")

# %%
f, ax = plt.subplots(1,figsize = (10, 12), dpi = 200)
states.plot(facecolor = 'None',linewidth = 0.5, edgecolor = 'k', ax = ax)
vil_g.plot(column = 'mines_in_10k',  markersize = 0.5 , ax = ax, cmap = 'viridis')
mines.loc[mines.mineral.isin(top20)].plot(column = 'mineral', categorical = True, 
            markersize = 0.8 , ax = ax,
            cmap = 'tab20')
ax.set_axis_off()
ax.set_title("Villages with Mine Proximity \n Number of mines within 10 km radius \n State borders overlaid")

# %%
f.savefig(root/'out/mine_density_map.pdf')

f.savefig(root/'out/mine_density_map.png')
