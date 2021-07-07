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
import os, sys, glob
from pathlib import Path
import numpy as np
import pandas as pd
import seaborn as sns

import matplotlib.pyplot as plt

import geopandas as gpd

from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

# %% Collapsed="false"
root = Path('/home/alal/Dropbox/1_Research/India_Forests/')
#%% Read in data
code = root/'Code'
data = root/'Data'

# %% Collapsed="false"
mining_atlas = data/'Admin/india-mines/atlas_clean.dta'
atlas = pd.read_stata(mining_atlas)
atlas.head()

# %% Collapsed="false"
atlas.mineral.value_counts()

# %% [markdown] Collapsed="false"
# ## Village data

# %% Collapsed="false"
vil = gpd.read_file(data/'Spatial/Processed/vil_points.shp')

# %% Collapsed="false"
vil.shape
mines.shape

# %% [markdown] Collapsed="false"
# # Spatial

# %% Collapsed="false"
mines = gpd.GeoDataFrame(atlas, geometry = gpd.points_from_xy(atlas.longitude, atlas.latitude))

# %% Collapsed="false"
f, ax = plt.subplots(1, dpi = 300)
vil.plot(markersize = .5, facecolor = 'b', ax = ax)
mines.plot(markersize = .5, facecolor = 'r', ax = ax)
ax.set_axis_off()

# %% Collapsed="false"
