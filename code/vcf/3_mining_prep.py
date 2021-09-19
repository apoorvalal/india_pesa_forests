# %%
import os, sys, glob
import re

from pathlib import Path
import numpy as np
import pandas as pd
import swifter
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
rice_root = '/home/apoorval/Research/GeoSpatial/India_Forests/'
dbox_root = '/home/alal/res/india_pesa_forests/'
root = Path(dbox_root)

# %% # # Mining Atlas
df = pd.read_stata(root/'inp/india-mines/atlas_clean.dta')
df.info()
df.head()

# %%
(df[['metal_class', 'locality']].groupby('locality')
    .count().sort_values(by = 'metal_class', ascending = False)
)
dropminerals = ['limestone', 'fire clay', 'clay', 'quartz glass/silica sand',
                'mica', 'ochre', 'talc', 'gypsum', 'salt']
df2 = df.loc[~df.mineral.isin(dropminerals)]

# %%
df.shape
df2.shape

# %%
df2.metal_class.value_counts()
df2.mineral.value_counts()

# %%
mines = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.longitude, df.latitude))
mines.crs = "EPSG:4326"
mines.mineral.nunique()
# %%
mines.to_file(root/'tmp/mine_points.geojson', driver = "GeoJSON")
# %%
top20 = mines.mineral.value_counts().nlargest(20).index

# %%
states = gpd.read_file("/home/alal/Dropbox/_Data/India_spatial_pipeline/data/ADM/IND_adm1.shp")
block2 = gpd.read_parquet(root/"tmp/BLOCKS_sch_coded.spq")
block2.head()
##     ##    ###    ########   ######
###   ###   ## ##   ##     ## ##    ##
#### ####  ##   ##  ##     ## ##
## ### ## ##     ## ########   ######
##     ## ######### ##              ##
##     ## ##     ## ##        ##    ##
##     ## ##     ## ##         ######

# %%
f, ax = plt.subplots(1,figsize = (10, 12), dpi = 200)
states.plot(facecolor = 'None',linewidth = 0.5, edgecolor = 'k', ax = ax)
block2.plot(column = 'sch',linewidth = 0.5, edgecolor = 'k', alpha = 0.3, ax = ax)
mines.loc[mines.mineral.isin(top20)].plot(column = 'mineral', categorical = True,
            markersize = 0.8 , ax = ax,
            legend = True, cmap = 'tab20',
            legend_kwds = {
                'loc': 'lower right', 'markerscale': 0.3, 'ncol': 2,
                'prop': {'size': 10}})
ax.set_axis_off()
ax.set_title("Locations of Mines in India \n 20 most common minerals \n State borders overlaid")
# %%
f.savefig(root/'out/mine_map.pdf')

mines.head()

# %%
pd.set_option("display.max_rows", None, "display.max_columns", None)

block3 = gpd.sjoin(block2, mines[['metal_class', 'mineral', 'geometry']],
    how = 'left', op = 'intersects')
block3.head()
state_grouped = (block3.groupby(['STATE_UT', 'sch', 'mineral'])
                    .size())
tabulation = state_grouped.groupby(['STATE_UT', 'sch']).apply(lambda x: x.sort_values(ascending = False))

tabulation

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


# %% ingest grid fishnet
%%time
fishnet = gpd.read_file(root/"tmp/vcf_pixel_cells.geojson")
fishnet.info()
fishnet.crs = "EPSG:4326"
# %%
fishnet.drop(['value', 'x', 'y'], axis = 1, inplace = True)
fishnet['centr'] = fishnet.centroid
fishnet = fishnet.set_geometry('centr')
# %% distance computation
distance_to_nearest = lambda x: mines.distance(x).min()
# %%
%%time
fishnet['min_dist_to_mine'] = fishnet['centr'].swifter.apply(distance_to_nearest)
fishnet.head()
# %%
sns.distplot(fishnet.min_dist_to_mine)

# %%
fishnet_w_mine_distances = fishnet.drop(['geometry', 'centr'], axis = 1)
fishnet_w_mine_distances.to_csv(root/"tmp/fishnet_w_mine_distances.csv")

# %%
