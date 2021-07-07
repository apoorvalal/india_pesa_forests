# %% library loads
import glob
from pathlib import Path
from numpy.core.fromnumeric import compress
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

# %% block shapefile stack with treatment
block_buf = gpd.read_parquet(root/"tmp/BLOCKS_sch_coded.spq").infer_objects()
block_buf.info(max_cols = 200)

# %% plot block census
f, ax = plt.subplots(figsize = (10, 12), dpi = 150)
block_buf.plot(column = 'TOT_POP', edgecolor = 'k',
	cmap = 'viridis', ax = ax)
# fishnet.plot(facecolor = "None", edgecolor = 'r', ax = ax)
ax.set_axis_off()

# %% raster grid fishnet
%%time
clipped_rasts = glob.glob(str(root/"tmp/clipped_rasters/*.tif"))
p = clipped_rasts[0]
rast   = gr.from_file(p)
fishnet = rast.to_geopandas()
# %% keep just geometry and cellid
fishnet.drop(['value', 'x', 'y'], axis = 1, inplace = True)
fishnet = fishnet.to_crs(block_buf.crs)
fishnet.info()
# %% set geom to centroid
fishnet['centr'] = fishnet.centroid
fishnet = fishnet.set_geometry('centr')
# %%
grid_treat = gpd.sjoin(fishnet, block_buf, how='inner', op='within')
grid_merged = grid_treat.drop(['centr'], axis = 1).set_geometry('geometry')
# %% back up grid geometries
%%time
grid_merged.to_parquet(root/"tmp/raster_fishnet.spq")

# %% viz map for jharkhand
f, ax = plt.subplots(1, 2, figsize = (15, 10), dpi = 150)
block_buf.query("STATE_UT == 'Jharkhand'").plot(column = 'sch',
	cmap = "Set1", edgecolor = 'k',
	ax = ax[0])
ax[0].set_axis_off()
grid_merged.query("STATE_UT == 'Jharkhand'").plot(column = 'sch',
	cmap = "Set1", edgecolor = 'k',
	ax = ax[1])
ax[1].set_axis_off()
plt.tight_layout()
f.suptitle("0.05 X 0.05 degree cell merge \n Jharkhand", fontsize=14)
f.savefig(root/'tmp/jh_sp_merge.pdf')
# %%
grid_merged.info(max_cols = 200)
# %%
grid_treat_tags = pd.DataFrame(grid_merged.drop('geometry', axis = 1))
grid_treat_tags.head()

grid_treat_tags.to_csv(root/"tmp/fishnet_treat_merged.csv.gz", compression="gzip")

# %%
 ######  ######## ##       ##
##    ## ##       ##       ##
##       ##       ##       ##
##       ######   ##       ##
##       ##       ##       ##
##    ## ##       ##       ##
 ######  ######## ######## ########

 ########     ###    ##    ## ######## ##
##     ##   ## ##   ###   ## ##       ##
##     ##  ##   ##  ####  ## ##       ##
########  ##     ## ## ## ## ######   ##
##        ######### ##  #### ##       ##
##        ##     ## ##   ### ##       ##
##        ##     ## ##    ## ######## ########
# create pandas df by dropping geo
# %%


# %%
cell_panel = (pd.read_csv(root/"tmp/forest_cover_panel_cell_level.csv.gz")
		.infer_objects())
cell_panel.head()
# %% merge with cell panel
cell_pan = pd.merge(grid_treat_tags, cell_panel,
	left_on=["row", "col"], right_on = ["row", "col"])
cell_pan.info()


# %% long panel write
%%time
cell_pan.to_csv(root/"tmp/cell_panel_with_treatment_and_outcome.csv.gz",
	compression="gzip")
# %%
