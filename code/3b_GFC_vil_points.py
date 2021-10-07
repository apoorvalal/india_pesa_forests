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

import rtree
from shapely.geometry import shape, mapping, Point, LinearRing

#%% Read in data
dbox_root = '/home/alal/res/India_Forests/'
# saad_root = '~/Dropbox/india_forests/'
# root = dbox_root
root = Path(dbox_root)
# %%
vil_1 = gpd.read_file(root / "inp/spatial/village_stack_deforestation.gpkg")
vil_2 = gpd.read_file(root / "inp/spatial/village_stack_ex_ante.gpkg")
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
vil['centroid'] = vil.centroid
vil["x"] = vil.centroid.map(lambda p: p.x)
vil["y"] = vil.centroid.map(lambda p: p.y)
vil = vil.set_geometry('centroid')
vil.drop(['geometry'], inplace = True, axis=1)

# %% jupyter={"outputs_hidden": true}
vil.info()


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

vil.to_file(spatial/'Processed/all_villages_data.gpkg', driver='GPKG')

# %% [markdown]
# ## write csv for prelim analysis

# %% jupyter={"outputs_hidden": true}
vil.columns

# %% jupyter={"outputs_hidden": true}
df = pd.DataFrame(vil.drop(columns=['centroid']))

# %% jupyter={"outputs_hidden": true}
df.to_csv(root/ 'inp/villages_points_all2.csv')
# %%
