# %% library loads
from pathlib import Path
# filler to import personal library
# sys.path.append('/home/alal/Desktop/code/py_libraries/')
# pyscience imports
import pandas as pd
# viz
import matplotlib
import matplotlib.pyplot as plt
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
import contextily as cx
# raster packages
# import rasterio as rio
# from rasterstats import zonal_stats
# show all output
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'
# %% # disable nonsensical pandas warning
pd.options.mode.chained_assignment = None

#%% Read in data
root = Path('/home/alal/Dropbox/1_Research/india_pesa_forests/')
code = root / 'Code'
data = root / 'inp'
%cd $data
# %%
census_2001 = data/"india_spatial_master/data/CENSUS/India_Census_Villages_2001"
adm_2001    = data/"india_spatial_master/data/2001_admin/"
%ls $adm_2001

# %%
block     = gpd.read_file(adm_2001/'BLOCKMAP.shp')
states    = gpd.read_file(adm_2001/'STATE.shp')
block_buf = gpd.read_parquet(root/"tmp/BLOCKS_sch_coded.spq")

# %%
# subset
blocks = block_buf.loc[:, ['sch', "named", "nameb", 'geometry']]
xmin, ymin, xmax, ymax= block_buf.total_bounds
f, ax = plt.subplots(1, figsize=(12,12))
block_buf.plot(column='sch', categorical=True, legend=True, alpha = 0.8,
            cmap = 'Set1', edgecolor='k',linewidth=0.3,ax=ax)
states.plot(facecolor = 'none', categorical=True, legend=True,
            edgecolor='y',linewidth=1,ax=ax)
ax.set_xlim(xmin, xmax)
ax.set_ylim(ymin, ymax)
ax.set_axis_off()
cx.add_basemap(ax, crs = states.crs.to_string(),
    source = cx.providers.Stamen.TonerLite)
ax.set_title('Fifth Schedule Areas')
ax.set_axis_off()
f.savefig(root/'out/treatmap/scheduled_areas_map.pdf')
f.savefig(root/'out/treatmap/scheduled_areas_map.png')



# %%
%ls $census_2001

# %%
%%time
andhra      = gpd.read_file(census_2001/'andhra.geojson')
chhat       = gpd.read_file(census_2001/'chhatisgarh.geojson')
gujarat     = gpd.read_file(census_2001/'gujarat.geojson')
himan       = gpd.read_file(census_2001/'himachal.geojson')
jharkhand   = gpd.read_file(census_2001/'jharkhand.geojson')
maharashtra = gpd.read_file(census_2001/'maharashtra.geojson')
madhya      = gpd.read_file(census_2001/'madhya_pradesh.geojson')
orissa      = gpd.read_file(census_2001/'orissa.geojson')
rajasthan   = gpd.read_file(census_2001/'rajasthan.geojson')

# %%
datalist = [jharkhand, orissa, chhat, andhra,  gujarat, himan, madhya,
            maharashtra, rajasthan]
village_stack = pd.concat(datalist, sort=False)

# %%
del datalist

# %% Temporarily set geometry to centroid
villages = village_stack.copy()
villages.crs = blocks.crs
villages['centr'] = villages.centroid
villages = villages.set_geometry('centr')

_# %%
%%time
vil_treat = gpd.sjoin(villages, blocks, how='inner', op='within')

# %%
villages.shape
vil_treat.shape
# %% switch geometry back
vv = vil_treat.drop(['centr'], axis = 1).set_geometry('geometry')
vv.geometry.head()


# %%
vv.info(max_cols = 200)
vv.c_code01.nunique()

# %% Process and Write
n_blocks_vill = vv.groupby('c_code01').nameb.nunique().sort_values(ascending = False)
n_blocks_vill.head()
# %%time
type(vv)
# %%
vv.to_parquet(root/"inp/spatial/villages_sch_coded.spq")
# %%
