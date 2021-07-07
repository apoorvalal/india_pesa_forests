# %% library loads
import os, sys, glob, re, itertools, collections, requests
import multiprocessing # parallelise list comprehensions
from pathlib import Path
# filler to import personal library
# sys.path.append('/home/alal/Desktop/code/py_libraries/')
# pyscience imports
import numpy as np
import pandas as pd
# viz
import matplotlib
import matplotlib.pyplot as plt
import seaborn as sns
from plotnine import *
font = {'family' : 'IBM Plex Sans',
				'weight' : 'normal',
				'size'   : 10}
plt.rc('font', **font)
plt.rcParams['figure.figsize'] = (10, 10)
matplotlib.style.use(['seaborn-talk', 'seaborn-ticks', 'seaborn-whitegrid'])
%matplotlib inline
%config InlineBackend.figure_format = 'retina'
# geodata packages
import geopandas as gpd
import georasters as gr
# raster packages
# import rasterio as rio
# from rasterstats import zonal_stats
# show all output
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'
# %%
rice_root = '/home/apoorval/Research/GeoSpatial/India_Forests/'
dbox_root = '/home/alal/res/india_pesa_forests/'
root = Path(dbox_root)
data = root / 'inp'
spatial = data/'spatial'
# %% village shapefile stack with treatment
vil = gpd.read_parquet(data/"vil_treatment.spq")
vil.info()
# %%
# %%
print('aargh')
